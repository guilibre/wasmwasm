#include "emit.hpp"

#include "binaryen-c.h"
#include "routing/routing.hpp"
#include <algorithm>
#include <array>
#include <cstdint>
#include <string>
#include <vector>

namespace {

auto pfx(const IRModule &ir, const std::string &s) -> std::string {
    return ir.name + "$" + s;
}

auto resolve_source(BinaryenModuleRef mod, const RoutingGraph &graph,
                    const std::string &src, BinaryenIndex in_base,
                    BinaryenIndex sample, BinaryenIndex num_samples)
    -> BinaryenExpressionRef {
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
                                          BinaryenTypeFloat32(), addr,
                                          "memory"));
    }
    const auto under = src.rfind('_');
    if (under != std::string::npos) {
        const auto mod_part = src.substr(0, under - 4);
        const auto idx_part = src.substr(under + 1);
        const auto gname = mod_part + "$OUT$" + idx_part;
        return BinaryenGlobalGet(mod, gname.c_str(), BinaryenTypeFloat64());
    }
    return BinaryenConst(mod, BinaryenLiteralFloat64(0.0));
}

auto resolve_source_vec_lane(BinaryenModuleRef mod, const RoutingGraph &graph,
                             const std::string &src, int lane,
                             BinaryenIndex in_base, BinaryenIndex sample,
                             BinaryenIndex num_samples)
    -> BinaryenExpressionRef {
    if (auto it = graph.external_input_channels.find(src);
        it != graph.external_input_channels.end()) {
        auto *sample_n =
            BinaryenBinary(mod, BinaryenAddInt32(),
                           BinaryenLocalGet(mod, sample, BinaryenTypeInt32()),
                           BinaryenConst(mod, BinaryenLiteralInt32(lane)));
        auto *sample_offset =
            BinaryenBinary(mod, BinaryenMulInt32(), sample_n,
                           BinaryenConst(mod, BinaryenLiteralInt32(4)));
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
        auto *addr =
            BinaryenBinary(mod, BinaryenAddInt32(), ch_base, sample_offset);
        return BinaryenUnary(mod, BinaryenPromoteFloat32(),
                             BinaryenLoad(mod, 4, false, 0, 4,
                                          BinaryenTypeFloat32(), addr,
                                          "memory"));
    }
    const auto under = src.rfind('_');
    if (under != std::string::npos) {
        const auto mod_part = src.substr(0, under - 4);
        const auto idx_part = src.substr(under + 1);
        const auto gname = mod_part + "$OUT$" + idx_part + "_vec";
        auto *vec = BinaryenGlobalGet(mod, gname.c_str(), BinaryenTypeVec128());
        return BinaryenSIMDExtract(mod, BinaryenExtractLaneVecF64x2(), vec,
                                   static_cast<uint8_t>(lane));
    }
    return BinaryenConst(mod, BinaryenLiteralFloat64(0.0));
}

} // namespace

void emit_main_loop(const RoutingGraph &graph, BinaryenModuleRef mod) {
    const bool simd_eligible = std::ranges::all_of(
        graph.modules, [](const ModuleRoute &route) -> bool {
            return std::ranges::all_of(route.ir.delays,
                                       [](const IRDelayDecl &b) -> bool {
                                           return b.size_elements >= 2;
                                       });
        });

    const std::vector<std::string> channels =
        !graph.out_sources.empty()
            ? graph.out_sources
            : std::vector<std::string>{graph.dac_l_source, graph.dac_r_source};

    const bool vec_available =
        simd_eligible && !graph.modules.empty() && channels.size() == 2 &&
        std::ranges::all_of(
            graph.modules, [&](const ModuleRoute &route) -> bool {
                const auto fn_name = pfx(route.ir, route.ir.main_fn + "_vec");
                return BinaryenGetFunction(mod, fn_name.c_str()) != nullptr;
            });

    BinaryenModuleSetFeatures(mod, BinaryenFeatureAll());
    if (!BinaryenHasMemory(mod))
        BinaryenAddMemoryImport(mod, "memory", "env", "memory", 0);

    const bool has_ext = graph.external_input_count > 0;

    std::vector<BinaryenType> param_types = {
        BinaryenTypeInt32(), BinaryenTypeInt32(), BinaryenTypeInt32()};
    if (has_ext)
        param_types.insert(param_types.begin() + 1, BinaryenTypeInt32());

    const BinaryenIndex OUT_BASE = 0;
    BinaryenIndex IN_BASE = 0;
    BinaryenIndex NUM_SAMPLES = 0;
    if (has_ext) {
        IN_BASE = 1;
        NUM_SAMPLES = 2;
    } else {
        NUM_SAMPLES = 1;
    }

    std::array<BinaryenType, 1> var_types_arr = {BinaryenTypeInt32()};
    const auto SAMPLE = static_cast<BinaryenIndex>(param_types.size());

    auto build_per_sample = [&]() -> std::vector<BinaryenExpressionRef> {
        std::vector<BinaryenExpressionRef> stmts;

        for (const auto &route : graph.modules) {
            for (size_t i = 0; i < route.inputs.size(); ++i) {
                const auto gname = pfx(route.ir, "IN$" + std::to_string(i));
                auto *val = resolve_source(mod, graph, route.inputs[i], IN_BASE,
                                           SAMPLE, NUM_SAMPLES);
                stmts.push_back(BinaryenGlobalSet(mod, gname.c_str(), val));
            }
            const auto fn_name = pfx(route.ir, route.ir.main_fn);
            stmts.push_back(BinaryenCall(mod, fn_name.c_str(), nullptr, 0,
                                         BinaryenTypeNone()));
        }

        auto dac_f32 = [&](const std::string &src) -> BinaryenExpressionRef {
            return BinaryenUnary(
                mod, BinaryenDemoteFloat64(),
                resolve_source(mod, graph, src, IN_BASE, SAMPLE, NUM_SAMPLES));
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
                dac_f32(channels[ch]), BinaryenTypeFloat32(), "memory"));
        }

        stmts.push_back(BinaryenLocalSet(
            mod, SAMPLE,
            BinaryenBinary(mod, BinaryenAddInt32(),
                           BinaryenLocalGet(mod, SAMPLE, BinaryenTypeInt32()),
                           BinaryenConst(mod, BinaryenLiteralInt32(1)))));

        return stmts;
    };

    auto build_per_sample_vec = [&]() -> std::vector<BinaryenExpressionRef> {
        std::vector<BinaryenExpressionRef> stmts;

        for (const auto &route : graph.modules) {
            const auto fn_name = pfx(route.ir, route.ir.main_fn + "_vec");
            stmts.push_back(BinaryenCall(mod, fn_name.c_str(), nullptr, 0,
                                         BinaryenTypeNone()));
        }

        auto lane_f32 = [&](const std::string &src,
                            int lane) -> BinaryenExpressionRef {
            return BinaryenUnary(mod, BinaryenDemoteFloat64(),
                                 resolve_source_vec_lane(mod, graph, src, lane,
                                                         IN_BASE, SAMPLE,
                                                         NUM_SAMPLES));
        };

        auto out_addr_vec_l = [&](int lane) -> BinaryenExpressionRef {
            auto *sample_n =
                lane == 0
                    ? BinaryenLocalGet(mod, SAMPLE, BinaryenTypeInt32())
                    : BinaryenBinary(
                          mod, BinaryenAddInt32(),
                          BinaryenLocalGet(mod, SAMPLE, BinaryenTypeInt32()),
                          BinaryenConst(mod, BinaryenLiteralInt32(lane)));
            return BinaryenBinary(
                mod, BinaryenAddInt32(),
                BinaryenLocalGet(mod, OUT_BASE, BinaryenTypeInt32()),
                BinaryenBinary(mod, BinaryenMulInt32(), sample_n,
                               BinaryenConst(mod, BinaryenLiteralInt32(4))));
        };
        auto out_addr_vec_r = [&](int lane) -> BinaryenExpressionRef {
            auto *sample_n =
                lane == 0
                    ? BinaryenLocalGet(mod, SAMPLE, BinaryenTypeInt32())
                    : BinaryenBinary(
                          mod, BinaryenAddInt32(),
                          BinaryenLocalGet(mod, SAMPLE, BinaryenTypeInt32()),
                          BinaryenConst(mod, BinaryenLiteralInt32(lane)));
            auto *r_base = BinaryenBinary(
                mod, BinaryenAddInt32(),
                BinaryenLocalGet(mod, OUT_BASE, BinaryenTypeInt32()),
                BinaryenBinary(
                    mod, BinaryenMulInt32(),
                    BinaryenLocalGet(mod, NUM_SAMPLES, BinaryenTypeInt32()),
                    BinaryenConst(mod, BinaryenLiteralInt32(4))));
            return BinaryenBinary(
                mod, BinaryenAddInt32(), r_base,
                BinaryenBinary(mod, BinaryenMulInt32(), sample_n,
                               BinaryenConst(mod, BinaryenLiteralInt32(4))));
        };
        stmts.push_back(BinaryenStore(mod, 4, 0, 4, out_addr_vec_l(0),
                                      lane_f32(channels[0], 0),
                                      BinaryenTypeFloat32(), "memory"));
        stmts.push_back(BinaryenStore(mod, 4, 0, 4, out_addr_vec_r(0),
                                      lane_f32(channels[1], 0),
                                      BinaryenTypeFloat32(), "memory"));
        stmts.push_back(BinaryenStore(mod, 4, 0, 4, out_addr_vec_l(1),
                                      lane_f32(channels[0], 1),
                                      BinaryenTypeFloat32(), "memory"));
        stmts.push_back(BinaryenStore(mod, 4, 0, 4, out_addr_vec_r(1),
                                      lane_f32(channels[1], 1),
                                      BinaryenTypeFloat32(), "memory"));

        stmts.push_back(BinaryenLocalSet(
            mod, SAMPLE,
            BinaryenBinary(mod, BinaryenAddInt32(),
                           BinaryenLocalGet(mod, SAMPLE, BinaryenTypeInt32()),
                           BinaryenConst(mod, BinaryenLiteralInt32(2)))));

        return stmts;
    };

    BinaryenExpressionRef body = nullptr;

    if (vec_available) {
        auto vec_stmts = build_per_sample_vec();

        auto *wide_break = BinaryenBreak(
            mod, "wide_loop",
            BinaryenBinary(
                mod, BinaryenLtUInt32(),
                BinaryenBinary(
                    mod, BinaryenAddInt32(),
                    BinaryenLocalGet(mod, SAMPLE, BinaryenTypeInt32()),
                    BinaryenConst(mod, BinaryenLiteralInt32(1))),
                BinaryenLocalGet(mod, NUM_SAMPLES, BinaryenTypeInt32())),
            nullptr);

        vec_stmts.push_back(wide_break);

        auto *wide_block = BinaryenBlock(
            mod, "wide_block", vec_stmts.data(),
            static_cast<BinaryenIndex>(vec_stmts.size()), BinaryenTypeNone());

        auto *guard_break = BinaryenBreak(
            mod, "wide_guard",
            BinaryenBinary(
                mod, BinaryenLtUInt32(),
                BinaryenLocalGet(mod, NUM_SAMPLES, BinaryenTypeInt32()),
                BinaryenConst(mod, BinaryenLiteralInt32(2))),
            nullptr);

        std::array<BinaryenExpressionRef, 2> guard_stmts = {
            guard_break,
            BinaryenLoop(mod, "wide_loop", wide_block),
        };
        auto *guarded_loop = BinaryenBlock(
            mod, "wide_guard", guard_stmts.data(), 2, BinaryenTypeNone());

        auto tail_stmts = build_per_sample();
        auto *tail_guard_break = BinaryenBreak(
            mod, "tail_guard",
            BinaryenBinary(
                mod, BinaryenGeUInt32(),
                BinaryenLocalGet(mod, SAMPLE, BinaryenTypeInt32()),
                BinaryenLocalGet(mod, NUM_SAMPLES, BinaryenTypeInt32())),
            nullptr);

        std::vector<BinaryenExpressionRef> tail_block_stmts;
        tail_block_stmts.push_back(tail_guard_break);
        tail_block_stmts.insert(tail_block_stmts.end(), tail_stmts.begin(),
                                tail_stmts.end());
        auto *tail_block =
            BinaryenBlock(mod, "tail_guard", tail_block_stmts.data(),
                          static_cast<BinaryenIndex>(tail_block_stmts.size()),
                          BinaryenTypeNone());

        std::array<BinaryenExpressionRef, 3> body_stmts = {
            BinaryenLocalSet(mod, SAMPLE,
                             BinaryenConst(mod, BinaryenLiteralInt32(0))),
            guarded_loop,
            tail_block,
        };
        body = BinaryenBlock(mod, nullptr, body_stmts.data(), body_stmts.size(),
                             BinaryenTypeNone());
    } else {
        auto stmts = build_per_sample();

        stmts.push_back(BinaryenBreak(
            mod, "outer_loop",
            BinaryenBinary(
                mod, BinaryenLtUInt32(),
                BinaryenLocalGet(mod, SAMPLE, BinaryenTypeInt32()),
                BinaryenLocalGet(mod, NUM_SAMPLES, BinaryenTypeInt32())),
            nullptr));

        auto *outer_block = BinaryenBlock(
            mod, "outer_block", stmts.data(),
            static_cast<BinaryenIndex>(stmts.size()), BinaryenTypeNone());

        std::array<BinaryenExpressionRef, 2> body_stmts = {
            BinaryenLocalSet(mod, SAMPLE,
                             BinaryenConst(mod, BinaryenLiteralInt32(0))),
            BinaryenLoop(mod, "outer_loop", outer_block),
        };
        body = BinaryenBlock(mod, nullptr, body_stmts.data(), body_stmts.size(),
                             BinaryenTypeNone());
    }

    BinaryenAddFunction(
        mod, "main",
        BinaryenTypeCreate(param_types.data(),
                           static_cast<BinaryenIndex>(param_types.size())),
        BinaryenTypeNone(), var_types_arr.data(), 1, body);
    BinaryenAddFunctionExport(mod, "main", "main");

    std::vector<BinaryenExpressionRef> init_calls;
    for (const auto &route : graph.modules) {
        if (!route.ir.static_init_fn.empty()) {
            const auto sname = pfx(route.ir, route.ir.static_init_fn);
            init_calls.push_back(BinaryenCall(mod, sname.c_str(), nullptr, 0,
                                              BinaryenTypeNone()));
        }
        const auto init_name = pfx(route.ir, route.ir.init_fn);
        init_calls.push_back(BinaryenCall(mod, init_name.c_str(), nullptr, 0,
                                          BinaryenTypeNone()));
    }
    auto *init_body =
        init_calls.empty()
            ? BinaryenNop(mod)
            : BinaryenBlock(mod, nullptr, init_calls.data(),
                            static_cast<BinaryenIndex>(init_calls.size()),
                            BinaryenTypeNone());
    BinaryenAddFunction(mod, "init", BinaryenTypeCreate(nullptr, 0),
                        BinaryenTypeNone(), nullptr, 0, init_body);
    BinaryenAddFunctionExport(mod, "init", "init");
}
