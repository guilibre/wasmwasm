#include "binaryen_emit.hpp"

#include "binaryen-c.h"
#include "instance_layout.hpp"
#include "ir/ir.hpp"
#include <array>
#include <functional>
#include <string>
#include <vector>

namespace {

auto pfx(const IRModule &ir, const std::string &s) -> std::string {
    return ir.name + "$" + s;
}

auto used_addr(BinaryenModuleRef mod, const InstanceLayout &layout,
               BinaryenExpressionRef slot_index) -> BinaryenExpressionRef {
    return BinaryenBinary(
        mod, BinaryenAddInt32(),
        BinaryenConst(mod, BinaryenLiteralInt32(
                               static_cast<int32_t>(layout.slots_table_base))),
        BinaryenBinary(mod, BinaryenMulInt32(), slot_index,
                       BinaryenConst(mod, BinaryenLiteralInt32(4))));
}

auto id_addr(BinaryenModuleRef mod, const InstanceLayout &layout,
             BinaryenExpressionRef slot_index) -> BinaryenExpressionRef {
    return BinaryenBinary(
        mod, BinaryenAddInt32(),
        BinaryenConst(mod, BinaryenLiteralInt32(
                               static_cast<int32_t>(layout.slots_ids_base))),
        BinaryenBinary(mod, BinaryenMulInt32(), slot_index,
                       BinaryenConst(mod, BinaryenLiteralInt32(4))));
}

auto used_at(BinaryenModuleRef mod, const InstanceLayout &layout,
             BinaryenExpressionRef slot_index) -> BinaryenExpressionRef {
    return BinaryenLoad(mod, 4, false, 0, 4, BinaryenTypeInt32(),
                        used_addr(mod, layout, slot_index), "0");
}

auto id_at(BinaryenModuleRef mod, const InstanceLayout &layout,
           BinaryenExpressionRef slot_index) -> BinaryenExpressionRef {
    return BinaryenLoad(mod, 4, false, 0, 4, BinaryenTypeInt32(),
                        id_addr(mod, layout, slot_index), "0");
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

using StopCond = std::function<BinaryenExpressionRef(BinaryenModuleRef,
                                                     BinaryenExpressionRef)>;

auto emit_find_slot(BinaryenModuleRef mod,
                    std::vector<BinaryenExpressionRef> &stmts,
                    const std::string &label_base, BinaryenIndex slot_local,
                    const StopCond &stop_when) -> void {
    const auto loop_label = label_base + "$loop";
    const auto block_label = label_base + "$block";

    auto slot_get = [&]() -> BinaryenExpressionRef {
        return BinaryenLocalGet(mod, slot_local, BinaryenTypeInt32());
    };

    auto *found = stop_when(mod, slot_get());
    auto *br_exit_found =
        BinaryenBreak(mod, block_label.c_str(), found, nullptr);

    auto *inc = BinaryenLocalSet(
        mod, slot_local,
        BinaryenBinary(mod, BinaryenAddInt32(), slot_get(),
                       BinaryenConst(mod, BinaryenLiteralInt32(1))));
    auto *keep_going = BinaryenBinary(
        mod, BinaryenLtUInt32(), slot_get(),
        BinaryenConst(mod, BinaryenLiteralInt32(static_cast<int32_t>(
                               max_instances_per_module))));

    std::array<BinaryenExpressionRef, 3> loop_body_stmts = {
        br_exit_found, inc,
        BinaryenBreak(mod, loop_label.c_str(), keep_going, nullptr)};
    auto *loop_body = BinaryenBlock(mod, nullptr, loop_body_stmts.data(),
                                    loop_body_stmts.size(), BinaryenTypeNone());
    auto *loop = BinaryenLoop(mod, loop_label.c_str(), loop_body);

    std::array<BinaryenExpressionRef, 1> block_stmts = {loop};
    auto *block = BinaryenBlock(mod, block_label.c_str(), block_stmts.data(),
                                block_stmts.size(), BinaryenTypeNone());

    stmts.push_back(BinaryenLocalSet(
        mod, slot_local, BinaryenConst(mod, BinaryenLiteralInt32(0))));
    stmts.push_back(block);
}

auto slot_not_found(BinaryenModuleRef mod, BinaryenIndex slot_local)
    -> BinaryenExpressionRef {
    return BinaryenBinary(
        mod, BinaryenGeUInt32(),
        BinaryenLocalGet(mod, slot_local, BinaryenTypeInt32()),
        BinaryenConst(mod, BinaryenLiteralInt32(static_cast<int32_t>(
                               max_instances_per_module))));
}

auto matches_id(BinaryenModuleRef mod, const InstanceLayout &layout,
                BinaryenIndex slot_local, BinaryenIndex id_param)
    -> BinaryenExpressionRef {
    return BinaryenBinary(
        mod, BinaryenAndInt32(),
        used_at(mod, layout,
                BinaryenLocalGet(mod, slot_local, BinaryenTypeInt32())),
        BinaryenBinary(
            mod, BinaryenEqInt32(),
            id_at(mod, layout,
                  BinaryenLocalGet(mod, slot_local, BinaryenTypeInt32())),
            BinaryenLocalGet(mod, id_param, BinaryenTypeInt32())));
}

} // namespace

void emit_instance_api_group(
    const std::string &instrument_id,
    const std::vector<const IRModule *> &members,
    const std::vector<std::string> &param_names, BinaryenModuleRef mod,
    const std::unordered_map<std::string, InstanceLayout> &layouts) {
    const auto &shared_layout = layouts.at(members.front()->name);

    {
        constexpr BinaryenIndex id_param = 0;
        constexpr BinaryenIndex slot_local = 1;
        std::vector<BinaryenExpressionRef> stmts;

        emit_find_slot(
            mod, stmts, instrument_id + "$instantiate$existing", slot_local,
            [&](BinaryenModuleRef m,
                BinaryenExpressionRef) -> BinaryenExpressionRef {
                return matches_id(m, shared_layout, slot_local, id_param);
            });
        stmts.push_back(BinaryenIf(
            mod,
            BinaryenUnary(mod, BinaryenEqZInt32(),
                          slot_not_found(mod, slot_local)),
            BinaryenReturn(mod, BinaryenConst(mod, BinaryenLiteralInt32(-1))),
            nullptr));

        emit_find_slot(
            mod, stmts, instrument_id + "$instantiate$free", slot_local,
            [&](BinaryenModuleRef m,
                BinaryenExpressionRef slot) -> BinaryenExpressionRef {
                return BinaryenUnary(m, BinaryenEqZInt32(),
                                     used_at(m, shared_layout, slot));
            });
        stmts.push_back(BinaryenIf(
            mod, slot_not_found(mod, slot_local),
            BinaryenReturn(mod, BinaryenConst(mod, BinaryenLiteralInt32(-2))),
            nullptr));

        stmts.push_back(BinaryenStore(
            mod, 4, 0, 4,
            used_addr(mod, shared_layout,
                      BinaryenLocalGet(mod, slot_local, BinaryenTypeInt32())),
            BinaryenConst(mod, BinaryenLiteralInt32(1)), BinaryenTypeInt32(),
            "0"));
        stmts.push_back(BinaryenStore(
            mod, 4, 0, 4,
            id_addr(mod, shared_layout,
                    BinaryenLocalGet(mod, slot_local, BinaryenTypeInt32())),
            BinaryenLocalGet(mod, id_param, BinaryenTypeInt32()),
            BinaryenTypeInt32(), "0"));

        constexpr BinaryenIndex instance_base_local = 2;
        for (const auto *ir : members) {
            const auto &layout = layouts.at(ir->name);
            auto *instance_base = instance_base_of_slot(
                mod, layout,
                BinaryenLocalGet(mod, slot_local, BinaryenTypeInt32()));
            stmts.push_back(
                BinaryenLocalSet(mod, instance_base_local, instance_base));
            std::array<BinaryenExpressionRef, 1> init_args = {
                BinaryenLocalGet(mod, instance_base_local, BinaryenTypeInt32()),
            };
            if (!ir->static_init_fn.empty()) {
                std::array<BinaryenExpressionRef, 1> static_init_args = {
                    BinaryenLocalGet(mod, instance_base_local,
                                     BinaryenTypeInt32())};
                const auto sname = pfx(*ir, ir->static_init_fn);
                stmts.push_back(BinaryenCall(mod, sname.c_str(),
                                             static_init_args.data(), 1,
                                             BinaryenTypeNone()));
            }
            const auto init_name = pfx(*ir, ir->init_fn);
            stmts.push_back(BinaryenCall(mod, init_name.c_str(),
                                         init_args.data(), 1,
                                         BinaryenTypeNone()));
        }

        stmts.push_back(
            BinaryenReturn(mod, BinaryenConst(mod, BinaryenLiteralInt32(0))));

        auto *body = BinaryenBlock(mod, nullptr, stmts.data(),
                                   static_cast<BinaryenIndex>(stmts.size()),
                                   BinaryenTypeInt32());
        std::array<BinaryenType, 1> param_types = {BinaryenTypeInt32()};
        std::array<BinaryenType, 2> var_types = {BinaryenTypeInt32(),
                                                 BinaryenTypeInt32()};
        const auto fn_name = instrument_id + "$instantiate";
        BinaryenAddFunction(
            mod, fn_name.c_str(),
            BinaryenTypeCreate(param_types.data(),
                               static_cast<BinaryenIndex>(param_types.size())),
            BinaryenTypeInt32(), var_types.data(), var_types.size(), body);
        BinaryenAddFunctionExport(mod, fn_name.c_str(), fn_name.c_str());
    }

    {
        constexpr BinaryenIndex id_param = 0;
        constexpr BinaryenIndex slot_local = 1;
        std::vector<BinaryenExpressionRef> stmts;

        emit_find_slot(mod, stmts, instrument_id + "$destroy", slot_local,
                       [&](BinaryenModuleRef m,
                           BinaryenExpressionRef) -> BinaryenExpressionRef {
                           return matches_id(m, shared_layout, slot_local,
                                             id_param);
                       });
        stmts.push_back(BinaryenIf(
            mod, slot_not_found(mod, slot_local),
            BinaryenReturn(mod, BinaryenConst(mod, BinaryenLiteralInt32(-1))),
            nullptr));

        stmts.push_back(BinaryenStore(
            mod, 4, 0, 4,
            used_addr(mod, shared_layout,
                      BinaryenLocalGet(mod, slot_local, BinaryenTypeInt32())),
            BinaryenConst(mod, BinaryenLiteralInt32(0)), BinaryenTypeInt32(),
            "0"));

        stmts.push_back(
            BinaryenReturn(mod, BinaryenConst(mod, BinaryenLiteralInt32(0))));

        auto *body = BinaryenBlock(mod, nullptr, stmts.data(),
                                   static_cast<BinaryenIndex>(stmts.size()),
                                   BinaryenTypeInt32());
        std::array<BinaryenType, 1> param_types = {BinaryenTypeInt32()};
        std::array<BinaryenType, 1> var_types = {BinaryenTypeInt32()};
        const auto fn_name = instrument_id + "$destroy";
        BinaryenAddFunction(
            mod, fn_name.c_str(),
            BinaryenTypeCreate(param_types.data(),
                               static_cast<BinaryenIndex>(param_types.size())),
            BinaryenTypeInt32(), var_types.data(), var_types.size(), body);
        BinaryenAddFunctionExport(mod, fn_name.c_str(), fn_name.c_str());
    }

    {
        std::unordered_map<std::string, std::vector<const IRModule *>>
            modules_by_param;
        for (const auto *ir : members)
            for (const auto &[name, _] : ir->params)
                modules_by_param[name].push_back(ir);

        constexpr BinaryenIndex id_param = 0;
        constexpr BinaryenIndex param_index_param = 1;
        constexpr BinaryenIndex value_param = 2;
        constexpr BinaryenIndex slot_local = 3;

        std::vector<BinaryenExpressionRef> stmts;

        emit_find_slot(mod, stmts, instrument_id + "$set_param", slot_local,
                       [&](BinaryenModuleRef m,
                           BinaryenExpressionRef) -> BinaryenExpressionRef {
                           return matches_id(m, shared_layout, slot_local,
                                             id_param);
                       });
        stmts.push_back(BinaryenIf(
            mod, slot_not_found(mod, slot_local),
            BinaryenReturn(mod, BinaryenConst(mod, BinaryenLiteralInt32(-1))),
            nullptr));

        for (size_t i = 0; i < param_names.size(); ++i) {
            const auto &name = param_names[i];
            std::vector<BinaryenExpressionRef> branch_stmts;
            for (const auto *ir : modules_by_param.at(name)) {
                const auto &layout = layouts.at(ir->name);
                auto *instance_base = instance_base_of_slot(
                    mod, layout,
                    BinaryenLocalGet(mod, slot_local, BinaryenTypeInt32()));
                auto *addr = BinaryenBinary(
                    mod, BinaryenAddInt32(), instance_base,
                    BinaryenConst(mod,
                                  BinaryenLiteralInt32(static_cast<int32_t>(
                                      layout.param_offset.at(name)))));
                branch_stmts.push_back(BinaryenStore(
                    mod, 8, 0, 8, addr,
                    BinaryenLocalGet(mod, value_param, BinaryenTypeFloat64()),
                    BinaryenTypeFloat64(), "0"));
            }

            auto *matches_idx = BinaryenBinary(
                mod, BinaryenEqInt32(),
                BinaryenLocalGet(mod, param_index_param, BinaryenTypeInt32()),
                BinaryenConst(mod,
                              BinaryenLiteralInt32(static_cast<int32_t>(i))));
            auto *branch_block =
                BinaryenBlock(mod, nullptr, branch_stmts.data(),
                              branch_stmts.size(), BinaryenTypeNone());
            stmts.push_back(
                BinaryenIf(mod, matches_idx, branch_block, nullptr));
        }
        stmts.push_back(
            BinaryenReturn(mod, BinaryenConst(mod, BinaryenLiteralInt32(0))));

        auto *body = BinaryenBlock(mod, nullptr, stmts.data(),
                                   static_cast<BinaryenIndex>(stmts.size()),
                                   BinaryenTypeInt32());
        std::array<BinaryenType, 3> param_types = {
            BinaryenTypeInt32(), BinaryenTypeInt32(), BinaryenTypeFloat64()};
        std::array<BinaryenType, 1> var_types = {BinaryenTypeInt32()};
        const auto fn_name = instrument_id + "$set_param";
        BinaryenAddFunction(
            mod, fn_name.c_str(),
            BinaryenTypeCreate(param_types.data(),
                               static_cast<BinaryenIndex>(param_types.size())),
            BinaryenTypeInt32(), var_types.data(), var_types.size(), body);
        BinaryenAddFunctionExport(mod, fn_name.c_str(), fn_name.c_str());
    }
}

void emit_global_set_param(
    const std::vector<const IRModule *> &members,
    const std::vector<std::string> &param_names, BinaryenModuleRef mod,
    const std::unordered_map<std::string, InstanceLayout> &layouts) {
    if (members.empty()) return;

    std::unordered_map<std::string, std::vector<const IRModule *>>
        modules_by_param;
    for (const auto *ir : members) {
        for (const auto &[name, _] : ir->params)
            modules_by_param[name].push_back(ir);
    }

    constexpr BinaryenIndex param_index_param = 0;
    constexpr BinaryenIndex value_param = 1;

    std::vector<BinaryenExpressionRef> stmts;

    for (size_t i = 0; i < param_names.size(); ++i) {
        const auto &name = param_names[i];
        std::vector<BinaryenExpressionRef> branch_stmts;
        for (const auto *ir : modules_by_param.at(name)) {
            const auto &layout = layouts.at(ir->name);
            const auto local_offset = layout.param_offset.at(name);
            auto *addr =
                BinaryenConst(mod, BinaryenLiteralInt32(static_cast<int32_t>(
                                       layout.module_base + local_offset)));
            stmts.push_back(BinaryenStore(
                mod, 8, 0, 8, addr,
                BinaryenLocalGet(mod, value_param, BinaryenTypeFloat64()),
                BinaryenTypeFloat64(), "0"));
        }

        auto *matches_idx = BinaryenBinary(
            mod, BinaryenEqInt32(),
            BinaryenLocalGet(mod, param_index_param, BinaryenTypeInt32()),
            BinaryenConst(mod, BinaryenLiteralInt32(static_cast<int32_t>(i))));
        auto *branch_block =
            BinaryenBlock(mod, nullptr, branch_stmts.data(),
                          branch_stmts.size(), BinaryenTypeNone());
        stmts.push_back(BinaryenIf(mod, matches_idx, branch_block, nullptr));
    }
    stmts.push_back(
        BinaryenReturn(mod, BinaryenConst(mod, BinaryenLiteralInt32(0))));

    auto *body = BinaryenBlock(mod, nullptr, stmts.data(),
                               static_cast<BinaryenIndex>(stmts.size()),
                               BinaryenTypeInt32());
    std::array<BinaryenType, 2> param_types = {BinaryenTypeInt32(),
                                               BinaryenTypeFloat64()};
    constexpr auto fn_name = "global$set_param";
    BinaryenAddFunction(
        mod, fn_name,
        BinaryenTypeCreate(param_types.data(),
                           static_cast<BinaryenIndex>(param_types.size())),
        BinaryenTypeInt32(), nullptr, 0, body);
    BinaryenAddFunctionExport(mod, fn_name, fn_name);
}
