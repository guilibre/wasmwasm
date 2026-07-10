#include "binaryen_emit.hpp"

#include "binaryen-c.h"
#include "instance_layout.hpp"
#include "routing/routing.hpp"
#include <array>
#include <functional>
#include <string>
#include <unordered_map>
#include <unordered_set>
#include <vector>

namespace {

auto pfx(const IRModule &ir, const std::string &s) -> std::string {
    return ir.name + "$" + s;
}

auto dense_addr(BinaryenModuleRef mod, const InstanceLayout &layout,
                BinaryenExpressionRef idx) -> BinaryenExpressionRef {
    return BinaryenBinary(
        mod, BinaryenAddInt32(),
        BinaryenConst(
            mod, BinaryenLiteralInt32(static_cast<int32_t>(layout.dense_base))),
        BinaryenBinary(mod, BinaryenMulInt32(), idx,
                       BinaryenConst(mod, BinaryenLiteralInt32(4))));
}

auto active_count_load(BinaryenModuleRef mod, const InstanceLayout &layout)
    -> BinaryenExpressionRef {
    return BinaryenLoad(
        mod, 4, false, 0, 4, BinaryenTypeInt32(),
        BinaryenConst(mod, BinaryenLiteralInt32(
                               static_cast<int32_t>(layout.active_count_addr))),
        "0");
}

auto instance_base_of_slot(BinaryenModuleRef mod, const InstanceLayout &layout,
                           BinaryenExpressionRef slot_index)
    -> BinaryenExpressionRef {
    return BinaryenBinary(
        mod, BinaryenAddInt32(),
        BinaryenConst(mod, BinaryenLiteralInt32(
                               static_cast<int32_t>(layout.module_base))),
        BinaryenBinary(
            mod, BinaryenMulInt32(), slot_index,
            BinaryenConst(mod, BinaryenLiteralInt32(
                                   static_cast<int32_t>(layout.slot_stride)))));
}

using OutLocalMap = std::unordered_map<std::string, BinaryenIndex>;

auto resolve_source(BinaryenModuleRef mod, const RoutingGraph &graph,
                    const OutLocalMap &out_locals, const std::string &src,
                    BinaryenIndex in_base, BinaryenIndex sample,
                    BinaryenIndex num_samples) -> BinaryenExpressionRef {
    if (auto it = graph.external_input_channels.find(src);
        it != graph.external_input_channels.end()) {
        auto *ch_base = BinaryenBinary(
            mod, BinaryenAddInt32(),
            BinaryenLocalGet(mod, in_base, BinaryenTypeInt32()),
            BinaryenBinary(
                mod, BinaryenMulInt32(),
                BinaryenBinary(
                    mod, BinaryenMulInt32(),
                    BinaryenLocalGet(mod, num_samples, BinaryenTypeInt32()),
                    BinaryenConst(mod, BinaryenLiteralInt32(4))),
                BinaryenConst(mod, BinaryenLiteralInt32(it->second))));
        auto *addr = BinaryenBinary(
            mod, BinaryenAddInt32(), ch_base,
            BinaryenBinary(mod, BinaryenMulInt32(),
                           BinaryenLocalGet(mod, sample, BinaryenTypeInt32()),
                           BinaryenConst(mod, BinaryenLiteralInt32(4))));
        return BinaryenUnary(mod, BinaryenPromoteFloat32(),
                             BinaryenLoad(mod, 4, false, 0, 4,
                                          BinaryenTypeFloat32(), addr, "0"));
    }
    if (auto it = out_locals.find(src); it != out_locals.end())
        return BinaryenLocalGet(mod, it->second, BinaryenTypeFloat64());
    return BinaryenConst(mod, BinaryenLiteralFloat64(0.0));
}

auto resolve_feedback_source(
    BinaryenModuleRef mod, const std::string &src,
    const std::unordered_map<std::string, InstanceLayout> &layouts,
    const std::function<BinaryenExpressionRef()> &slot_get)
    -> BinaryenExpressionRef {
    const auto under = src.rfind("_out_");
    const auto mod_name = src.substr(0, under);
    const auto port_idx = std::stoul(src.substr(under + 5));
    const auto &layout = layouts.at(mod_name);
    auto *instance_base = instance_base_of_slot(mod, layout, slot_get());
    auto *addr = BinaryenBinary(
        mod, BinaryenAddInt32(), instance_base,
        BinaryenConst(mod, BinaryenLiteralInt32(static_cast<int32_t>(
                               layout.out_ports_offset + (port_idx * 8)))));
    return BinaryenLoad(mod, 8, false, 0, 8, BinaryenTypeFloat64(), addr, "0");
}

} // namespace

void emit_main_loop(
    const RoutingGraph &graph, BinaryenModuleRef mod,
    const std::unordered_map<std::string, InstanceLayout> &layouts) {
    const std::vector<std::string> channels =
        !graph.out_sources.empty()
            ? graph.out_sources
            : std::vector<std::string>{graph.dac_l_source, graph.dac_r_source};

    BinaryenModuleSetFeatures(mod, BinaryenFeatureAll());
    if (!BinaryenHasMemory(mod))
        BinaryenAddMemoryImport(mod, "0", "env", "memory", 0);

    std::vector<BinaryenType> param_types = {
        BinaryenTypeInt32(), BinaryenTypeInt32(), BinaryenTypeInt32()};

    constexpr BinaryenIndex NUM_SAMPLES = 0;
    constexpr BinaryenIndex IN_BASE = 1;
    constexpr BinaryenIndex OUT_BASE = 2;

    const auto SAMPLE = static_cast<BinaryenIndex>(param_types.size());
    const auto SLOT = SAMPLE + 1;
    const auto CUR_SLOT = SLOT + 1;
    const auto ACTIVE_COUNT_LOCAL = CUR_SLOT + 1;
    const auto KILL_IDX = ACTIVE_COUNT_LOCAL + 1;

    std::vector<BinaryenType> var_types = {
        BinaryenTypeInt32(), BinaryenTypeInt32(), BinaryenTypeInt32(),
        BinaryenTypeInt32(), BinaryenTypeInt32()};

    OutLocalMap out_locals;
    OutLocalMap cur_locals;
    for (const auto &route : graph.modules) {
        for (size_t o = 0; o < route.ir.num_outputs; ++o) {
            const auto key = route.ir.name + "_out_" + std::to_string(o);
            out_locals[key] = static_cast<BinaryenIndex>(param_types.size() +
                                                         var_types.size());
            var_types.push_back(BinaryenTypeFloat64());
            cur_locals[key] = static_cast<BinaryenIndex>(param_types.size() +
                                                         var_types.size());
            var_types.push_back(BinaryenTypeFloat64());
        }
    }

    std::unordered_map<std::string, const ModuleRoute *> route_by_name;
    for (const auto &route : graph.modules)
        route_by_name[route.ir.name] = &route;

    std::unordered_set<std::string> grouped_module_names;
    for (const auto &group : graph.instruments)
        for (const auto &name : group.module_names)
            grouped_module_names.insert(name);

    auto emit_block_body =
        [&](const ModuleRoute &route, const InstanceLayout &layout,
            const std::function<BinaryenExpressionRef()> &slot_get,
            const OutLocalMap &input_locals,
            std::vector<BinaryenExpressionRef> &out_stmts) -> void {
        for (size_t i = 0; i < route.inputs.size(); ++i) {
            auto *val =
                (i < route.feedback_inputs.size() && route.feedback_inputs[i])
                    ? resolve_feedback_source(mod, route.inputs[i], layouts,
                                              slot_get)
                    : resolve_source(mod, graph, input_locals, route.inputs[i],
                                     IN_BASE, SAMPLE, NUM_SAMPLES);
            auto *instance_base =
                instance_base_of_slot(mod, layout, slot_get());
            auto *in_field_addr = BinaryenBinary(
                mod, BinaryenAddInt32(), instance_base,
                BinaryenConst(mod, BinaryenLiteralInt32(static_cast<int32_t>(
                                       layout.in_ports_offset + (i * 8)))));
            out_stmts.push_back(BinaryenStore(mod, 8, 0, 8, in_field_addr, val,
                                              BinaryenTypeFloat64(), "0"));
        }

        {
            auto *instance_base =
                instance_base_of_slot(mod, layout, slot_get());
            const auto fn_name = pfx(route.ir, route.ir.main_fn);
            std::array<BinaryenExpressionRef, 1> call_args = {instance_base};
            out_stmts.push_back(BinaryenCall(
                mod, fn_name.c_str(), call_args.data(), 1, BinaryenTypeNone()));
        }

        for (size_t o = 0; o < route.ir.num_outputs; ++o) {
            auto *instance_base =
                instance_base_of_slot(mod, layout, slot_get());
            auto *out_field_addr = BinaryenBinary(
                mod, BinaryenAddInt32(), instance_base,
                BinaryenConst(mod, BinaryenLiteralInt32(static_cast<int32_t>(
                                       layout.out_ports_offset + (o * 8)))));
            auto *out_val =
                BinaryenLoad(mod, 8, false, 0, 8, BinaryenTypeFloat64(),
                             out_field_addr, "0");
            const auto key = route.ir.name + "_out_" + std::to_string(o);
            const auto cur_local = cur_locals.at(key);
            out_stmts.push_back(BinaryenLocalSet(mod, cur_local, out_val));

            const auto out_local = out_locals.at(key);
            out_stmts.push_back(BinaryenLocalSet(
                mod, out_local,
                BinaryenBinary(
                    mod, BinaryenAddFloat64(),
                    BinaryenLocalGet(mod, out_local, BinaryenTypeFloat64()),
                    BinaryenLocalGet(mod, cur_local, BinaryenTypeFloat64()))));
        }
    };

    auto reset_outputs =
        [&](const IRModule &ir, OutLocalMap &target,
            std::vector<BinaryenExpressionRef> &out_stmts) -> void {
        for (size_t o = 0; o < ir.num_outputs; ++o) {
            const auto key = ir.name + "_out_" + std::to_string(o);
            out_stmts.push_back(BinaryenLocalSet(
                mod, target.at(key),
                BinaryenConst(mod, BinaryenLiteralFloat64(0.0))));
        }
    };

    auto build_per_sample = [&]() -> std::vector<BinaryenExpressionRef> {
        std::vector<BinaryenExpressionRef> stmts;

        for (const auto &group : graph.instruments) {
            if (group.module_names.empty()) continue;
            const auto &shared_layout = layouts.at(group.module_names.front());

            for (const auto &name : group.module_names)
                reset_outputs(route_by_name.at(name)->ir, out_locals, stmts);

            stmts.push_back(BinaryenLocalSet(
                mod, SLOT, BinaryenConst(mod, BinaryenLiteralInt32(0))));

            std::vector<BinaryenExpressionRef> loop_stmts;
            auto slot_get = [&]() -> BinaryenExpressionRef {
                return BinaryenLocalGet(mod, CUR_SLOT, BinaryenTypeInt32());
            };

            loop_stmts.push_back(BinaryenLocalSet(
                mod, CUR_SLOT,
                BinaryenLoad(mod, 4, false, 0, 4, BinaryenTypeInt32(),
                             dense_addr(mod, shared_layout,
                                        BinaryenLocalGet(mod, SLOT,
                                                         BinaryenTypeInt32())),
                             "0")));

            std::vector<BinaryenExpressionRef> live_body;
            for (const auto &name : group.module_names)
                reset_outputs(route_by_name.at(name)->ir, cur_locals,
                              live_body);
            for (const auto &name : group.module_names) {
                const auto &route = *route_by_name.at(name);
                const auto &layout = layouts.at(name);
                emit_block_body(route, layout, slot_get, cur_locals, live_body);
            }

            for (auto *live_stmt : live_body) loop_stmts.push_back(live_stmt);

            loop_stmts.push_back(BinaryenLocalSet(
                mod, SLOT,
                BinaryenBinary(mod, BinaryenAddInt32(),
                               BinaryenLocalGet(mod, SLOT, BinaryenTypeInt32()),
                               BinaryenConst(mod, BinaryenLiteralInt32(1)))));

            const auto loop_label = group.id + "$main$instance_loop";
            loop_stmts.push_back(BinaryenBreak(
                mod, loop_label.c_str(),
                BinaryenBinary(mod, BinaryenLtUInt32(),
                               BinaryenLocalGet(mod, SLOT, BinaryenTypeInt32()),
                               active_count_load(mod, shared_layout)),
                nullptr));

            auto *loop_body =
                BinaryenBlock(mod, nullptr, loop_stmts.data(),
                              static_cast<BinaryenIndex>(loop_stmts.size()),
                              BinaryenTypeNone());
            stmts.push_back(BinaryenIf(
                mod,
                BinaryenBinary(mod, BinaryenLtUInt32(),
                               BinaryenLocalGet(mod, SLOT, BinaryenTypeInt32()),
                               active_count_load(mod, shared_layout)),
                BinaryenLoop(mod, loop_label.c_str(), loop_body), nullptr));

            {
                std::vector<BinaryenExpressionRef> kill_loop_stmts;
                auto kill_idx_get = [&]() -> BinaryenExpressionRef {
                    return BinaryenLocalGet(mod, KILL_IDX, BinaryenTypeInt32());
                };
                auto *pending_slot = BinaryenLoad(
                    mod, 4, false, 0, 4, BinaryenTypeInt32(),
                    BinaryenBinary(
                        mod, BinaryenAddInt32(),
                        BinaryenConst(mod,
                                      BinaryenLiteralInt32(static_cast<int32_t>(
                                          shared_layout.pending_kills_base))),
                        BinaryenBinary(
                            mod, BinaryenMulInt32(), kill_idx_get(),
                            BinaryenConst(mod, BinaryenLiteralInt32(4)))),
                    "0");
                std::array<BinaryenExpressionRef, 1> free_args = {pending_slot};
                const auto free_fn_name = group.id + "$free_slot";
                kill_loop_stmts.push_back(
                    BinaryenCall(mod, free_fn_name.c_str(), free_args.data(), 1,
                                 BinaryenTypeNone()));

                kill_loop_stmts.push_back(BinaryenLocalSet(
                    mod, KILL_IDX,
                    BinaryenBinary(
                        mod, BinaryenAddInt32(), kill_idx_get(),
                        BinaryenConst(mod, BinaryenLiteralInt32(1)))));

                auto *pending_count = BinaryenLoad(
                    mod, 4, false, 0, 4, BinaryenTypeInt32(),
                    BinaryenConst(mod,
                                  BinaryenLiteralInt32(static_cast<int32_t>(
                                      shared_layout.pending_kill_count_addr))),
                    "0");
                const auto kill_loop_label = group.id + "$main$kill_loop";
                kill_loop_stmts.push_back(
                    BinaryenBreak(mod, kill_loop_label.c_str(),
                                  BinaryenBinary(mod, BinaryenLtUInt32(),
                                                 kill_idx_get(), pending_count),
                                  nullptr));

                auto *kill_loop_body = BinaryenBlock(
                    mod, nullptr, kill_loop_stmts.data(),
                    static_cast<BinaryenIndex>(kill_loop_stmts.size()),
                    BinaryenTypeNone());

                stmts.push_back(BinaryenLocalSet(
                    mod, KILL_IDX,
                    BinaryenConst(mod, BinaryenLiteralInt32(0))));
                auto *pending_count_check = BinaryenLoad(
                    mod, 4, false, 0, 4, BinaryenTypeInt32(),
                    BinaryenConst(mod,
                                  BinaryenLiteralInt32(static_cast<int32_t>(
                                      shared_layout.pending_kill_count_addr))),
                    "0");
                stmts.push_back(BinaryenIf(
                    mod,
                    BinaryenBinary(mod, BinaryenLtUInt32(), kill_idx_get(),
                                   pending_count_check),
                    BinaryenLoop(mod, kill_loop_label.c_str(), kill_loop_body),
                    nullptr));

                stmts.push_back(BinaryenStore(
                    mod, 4, 0, 4,
                    BinaryenConst(mod,
                                  BinaryenLiteralInt32(static_cast<int32_t>(
                                      shared_layout.pending_kill_count_addr))),
                    BinaryenConst(mod, BinaryenLiteralInt32(0)),
                    BinaryenTypeInt32(), "0"));
            }
        }

        for (const auto &route : graph.modules) {
            if (grouped_module_names.contains(route.ir.name)) continue;
            const auto &layout = layouts.at(route.ir.name);

            reset_outputs(route.ir, out_locals, stmts);

            auto slot_zero = [&]() -> BinaryenExpressionRef {
                return BinaryenConst(mod, BinaryenLiteralInt32(0));
            };
            emit_block_body(route, layout, slot_zero, out_locals, stmts);
        }

        auto dac_f32 = [&](const std::string &src) -> BinaryenExpressionRef {
            return BinaryenUnary(mod, BinaryenDemoteFloat64(),
                                 resolve_source(mod, graph, out_locals, src,
                                                IN_BASE, SAMPLE, NUM_SAMPLES));
        };

        auto out_addr =
            [&](int channel,
                BinaryenIndex sample_local) -> BinaryenExpressionRef {
            auto *ch_base = BinaryenBinary(
                mod, BinaryenAddInt32(),
                BinaryenLocalGet(mod, OUT_BASE, BinaryenTypeInt32()),
                BinaryenBinary(
                    mod, BinaryenMulInt32(),
                    BinaryenBinary(
                        mod, BinaryenMulInt32(),
                        BinaryenLocalGet(mod, NUM_SAMPLES, BinaryenTypeInt32()),
                        BinaryenConst(mod, BinaryenLiteralInt32(4))),
                    BinaryenConst(mod, BinaryenLiteralInt32(channel))));
            return BinaryenBinary(
                mod, BinaryenAddInt32(), ch_base,
                BinaryenBinary(
                    mod, BinaryenMulInt32(),
                    BinaryenLocalGet(mod, sample_local, BinaryenTypeInt32()),
                    BinaryenConst(mod, BinaryenLiteralInt32(4))));
        };
        for (size_t ch = 0; ch < channels.size(); ++ch) {
            stmts.push_back(BinaryenStore(
                mod, 4, 0, 4, out_addr(static_cast<int>(ch), SAMPLE),
                dac_f32(channels[ch]), BinaryenTypeFloat32(), "0"));
        }

        stmts.push_back(BinaryenLocalSet(
            mod, SAMPLE,
            BinaryenBinary(mod, BinaryenAddInt32(),
                           BinaryenLocalGet(mod, SAMPLE, BinaryenTypeInt32()),
                           BinaryenConst(mod, BinaryenLiteralInt32(1)))));

        return stmts;
    };

    auto stmts = build_per_sample();

    stmts.push_back(BinaryenBreak(
        mod, "outer_loop",
        BinaryenBinary(mod, BinaryenLtUInt32(),
                       BinaryenLocalGet(mod, SAMPLE, BinaryenTypeInt32()),
                       BinaryenLocalGet(mod, NUM_SAMPLES, BinaryenTypeInt32())),
        nullptr));

    auto *outer_block = BinaryenBlock(mod, "outer_block", stmts.data(),
                                      static_cast<BinaryenIndex>(stmts.size()),
                                      BinaryenTypeNone());

    std::array<BinaryenExpressionRef, 2> body_stmts = {
        BinaryenLocalSet(mod, SAMPLE,
                         BinaryenConst(mod, BinaryenLiteralInt32(0))),
        BinaryenLoop(mod, "outer_loop", outer_block),
    };
    BinaryenExpressionRef body = BinaryenBlock(
        mod, nullptr, body_stmts.data(), body_stmts.size(), BinaryenTypeNone());

    BinaryenAddFunction(
        mod, "main",
        BinaryenTypeCreate(param_types.data(),
                           static_cast<BinaryenIndex>(param_types.size())),
        BinaryenTypeNone(), var_types.data(),
        static_cast<BinaryenIndex>(var_types.size()), body);
    BinaryenAddFunctionExport(mod, "main", "main");
}
