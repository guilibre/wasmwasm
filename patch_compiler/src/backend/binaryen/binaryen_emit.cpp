#include "binaryen_emit.hpp"

#include "ast/ast.hpp"
#include "binaryen-c.h"
#include "instance_layout.hpp"
#include "ir/ir.hpp"
#include <algorithm>
#include <array>
#include <cstdint>
#include <stdexcept>
#include <string>
#include <type_traits>
#include <unordered_map>
#include <utility>
#include <variant>
#include <vector>

namespace {

auto pfx(const IRModule &ir, const std::string &s) -> std::string {
    return ir.name + "$" + s;
}

constexpr BinaryenIndex instance_base_local = 0;

auto instance_base_get(BinaryenModuleRef mod) -> BinaryenExpressionRef {
    return BinaryenLocalGet(mod, instance_base_local, BinaryenTypeInt32());
}

auto field_addr(BinaryenModuleRef mod, uint32_t field_offset)
    -> BinaryenExpressionRef {
    return BinaryenBinary(
        mod, BinaryenAddInt32(), instance_base_get(mod),
        BinaryenConst(
            mod, BinaryenLiteralInt32(static_cast<int32_t>(field_offset))));
}

auto to_btype(IRType t) -> BinaryenType {
    switch (t) {
    case IRType::Float:
        return BinaryenTypeFloat64();
    case IRType::Int:
        return BinaryenTypeInt32();
    case IRType::Void:
        return BinaryenTypeNone();
    default:
        std::unreachable();
    }
}

auto to_bop(Operation op) -> BinaryenOp {
    switch (op) {
    case Operation::Add:
        return BinaryenAddFloat64();
    case Operation::Sub:
        return BinaryenSubFloat64();
    case Operation::Mul:
        return BinaryenMulFloat64();
    case Operation::Div:
        return BinaryenDivFloat64();
    default:
        std::unreachable();
    }
}

auto to_btype_multi(const std::vector<IRType> &types) -> BinaryenType {
    if (types.empty()) return BinaryenTypeNone();
    if (types.size() == 1) return to_btype(types[0]);
    std::vector<BinaryenType> btypes;
    btypes.reserve(types.size());
    for (const auto t : types) btypes.push_back(to_btype(t));
    return BinaryenTypeCreate(btypes.data(),
                              static_cast<BinaryenIndex>(btypes.size()));
}

struct FnCtx {
    BinaryenModuleRef mod{};
    double sample_rate{};
    const IRModule *ir{};
    const InstanceLayout *layout{};

    std::unordered_map<std::string, BinaryenIndex> idx;
    std::vector<BinaryenType> param_types;
    std::vector<BinaryenType> var_types;

    [[nodiscard]] auto resolve_mem(const std::string &buffer_name) const
        -> BinaryenExpressionRef {
        if (auto it = layout->delay_buffer_offset.find(buffer_name);
            it != layout->delay_buffer_offset.end())
            return field_addr(mod, it->second);
        return field_addr(mod, layout->static_array_offset.at(buffer_name));
    }

    void add_param(const std::string &name, IRType type) {
        idx[name] = static_cast<BinaryenIndex>(idx.size());
        param_types.push_back(to_btype(type));
    }

    void ensure_var(const std::string &name, IRType type) {
        if (!idx.contains(name)) {
            idx[name] = static_cast<BinaryenIndex>(param_types.size() +
                                                   var_types.size());
            var_types.push_back(to_btype(type));
        }
    }

    void ensure_raw_var(const std::string &name, BinaryenType btype) {
        if (!idx.contains(name)) {
            idx[name] = static_cast<BinaryenIndex>(param_types.size() +
                                                   var_types.size());
            var_types.push_back(btype);
        }
    }

    auto local_type(const std::string &name) -> BinaryenType {
        if (!idx.contains(name))
            throw std::runtime_error("internal error: local '" + name +
                                     "' not defined");
        const auto i = idx.at(name);
        if (i < param_types.size()) return param_types[i];
        return var_types[i - param_types.size()];
    }

    auto get(const IRValue &v) -> BinaryenExpressionRef {
        return std::visit(
            [&](const auto &x) -> BinaryenExpressionRef {
                using T = std::decay_t<decltype(x)>;
                if constexpr (std::is_same_v<T, IRLiteral>)
                    return BinaryenConst(mod, BinaryenLiteralFloat64(x.value));
                else {
                    if (!idx.contains(x.name))
                        throw std::runtime_error("internal error: local '" +
                                                 x.name + "' not defined");
                    return BinaryenLocalGet(mod, idx.at(x.name),
                                            local_type(x.name));
                }
            },
            v);
    }

    auto set(const std::string &name, BinaryenExpressionRef val)
        -> BinaryenExpressionRef {
        if (!idx.contains(name))
            throw std::runtime_error("internal error: local '" + name +
                                     "' not defined");
        return BinaryenLocalSet(mod, idx.at(name), val);
    }
};

void prescan(FnCtx &ctx, const std::vector<IRInstr> &body) {
    for (const auto &instr : body) {
        std::visit(
            [&](const auto &i) -> auto {
                using T = std::decay_t<decltype(i)>;
                if constexpr (std::is_same_v<T, IRBinOp>)
                    ctx.ensure_var(i.result, IRType::Float);
                if constexpr (std::is_same_v<T, IRUnaryNeg>)
                    ctx.ensure_var(i.result, IRType::Float);
                if constexpr (std::is_same_v<T, IRAssign>)
                    ctx.ensure_var(i.result, i.type);
                if constexpr (std::is_same_v<T, IRCall>) {
                    for (size_t k = 0; k < i.result.size(); ++k)
                        ctx.ensure_var(i.result[k], i.result_type[k]);
                    if (i.result.size() > 1)
                        ctx.ensure_raw_var("$tuple$" + i.result[0],
                                           to_btype_multi(i.result_type));
                }
                if constexpr (std::is_same_v<T, IRDelayRead>)
                    ctx.ensure_var(i.result, IRType::Float);
                if constexpr (std::is_same_v<T, IRDelayReadDelayed>)
                    ctx.ensure_var(i.result, IRType::Float);
                if constexpr (std::is_same_v<T, IRGlobalRead>)
                    ctx.ensure_var(i.result, i.type);
                if constexpr (std::is_same_v<T, IRInputRead>)
                    ctx.ensure_var(i.result, IRType::Float);
                if constexpr (std::is_same_v<T, IRIf>) {
                    prescan(ctx, i.body->then_body);
                    prescan(ctx, i.body->else_body);
                }
                if constexpr (std::is_same_v<T, IRStaticRead>)
                    ctx.ensure_var(i.result, IRType::Float);
                if constexpr (std::is_same_v<T, IRParamRead>)
                    ctx.ensure_var(i.result, IRType::Float);
                if constexpr (std::is_same_v<T, IRMemRead>)
                    ctx.ensure_var(i.result, IRType::Float);
            },
            instr);
    }
}

auto emit_stmts(FnCtx &ctx, const std::vector<IRInstr> &body)
    -> std::vector<BinaryenExpressionRef>;

auto emit_body(FnCtx &ctx, const std::vector<IRInstr> &body,
               const std::vector<IRType> &ret_type) -> BinaryenExpressionRef {
    auto stmts = emit_stmts(ctx, body);
    if (stmts.empty()) return BinaryenNop(ctx.mod);
    return BinaryenBlock(ctx.mod, nullptr, stmts.data(),
                         static_cast<BinaryenIndex>(stmts.size()),
                         to_btype_multi(ret_type));
}

auto emit_stmts(FnCtx &ctx, const std::vector<IRInstr> &body)
    -> std::vector<BinaryenExpressionRef> {
    std::vector<BinaryenExpressionRef> stmts;

    for (const auto &instr : body) {
        std::visit(
            [&](const auto &i) -> auto {
                using T = std::decay_t<decltype(i)>;

                if constexpr (std::is_same_v<T, IRBinOp>) {
                    if (i.op == Operation::Lt || i.op == Operation::Gt ||
                        i.op == Operation::Le || i.op == Operation::Ge ||
                        i.op == Operation::Eq || i.op == Operation::Ne) {
                        const auto cmp_op = [&] -> BinaryenOp {
                            if (i.op == Operation::Lt)
                                return BinaryenLtFloat64();
                            if (i.op == Operation::Gt)
                                return BinaryenGtFloat64();
                            if (i.op == Operation::Le)
                                return BinaryenLeFloat64();
                            if (i.op == Operation::Ge)
                                return BinaryenGeFloat64();
                            if (i.op == Operation::Eq)
                                return BinaryenEqFloat64();
                            return BinaryenNeFloat64();
                        }();
                        auto *cmp = BinaryenBinary(
                            ctx.mod, cmp_op, ctx.get(i.left), ctx.get(i.right));
                        auto *as_f64 = BinaryenUnary(
                            ctx.mod, BinaryenConvertUInt32ToFloat64(), cmp);
                        stmts.push_back(ctx.set(i.result, as_f64));
                    } else {
                        stmts.push_back(ctx.set(
                            i.result,
                            BinaryenBinary(ctx.mod, to_bop(i.op),
                                           ctx.get(i.left), ctx.get(i.right))));
                    }
                }
                if constexpr (std::is_same_v<T, IRUnaryNeg>) {
                    stmts.push_back(ctx.set(
                        i.result, BinaryenUnary(ctx.mod, BinaryenNegFloat64(),
                                                ctx.get(i.operand))));
                }
                if constexpr (std::is_same_v<T, IRAssign>) {
                    stmts.push_back(ctx.set(i.result, ctx.get(i.value)));
                }
                if constexpr (std::is_same_v<T, IRCall>) {
                    const auto is_math = i.callee.starts_with("wasmwasm_");
                    std::vector<BinaryenExpressionRef> args;
                    args.reserve(i.args.size() + 1);
                    if (!is_math) args.push_back(instance_base_get(ctx.mod));
                    for (const auto &a : i.args) args.push_back(ctx.get(a));
                    const auto callee_name =
                        is_math ? i.callee : pfx(*ctx.ir, i.callee);
                    if (i.result.empty()) {
                        auto *call = BinaryenCall(
                            ctx.mod, callee_name.c_str(), args.data(),
                            static_cast<BinaryenIndex>(args.size()),
                            to_btype_multi(i.result_type));
                        stmts.push_back(call);
                    } else if (i.result.size() == 1) {
                        auto *call = BinaryenCall(
                            ctx.mod, callee_name.c_str(), args.data(),
                            static_cast<BinaryenIndex>(args.size()),
                            to_btype_multi(i.result_type));
                        stmts.push_back(ctx.set(i.result[0], call));
                    } else {
                        auto *call = BinaryenCall(
                            ctx.mod, callee_name.c_str(), args.data(),
                            static_cast<BinaryenIndex>(args.size()),
                            to_btype_multi(i.result_type));
                        const auto tuple_name = "$tuple$" + i.result[0];
                        stmts.push_back(BinaryenLocalSet(
                            ctx.mod, ctx.idx.at(tuple_name), call));
                        for (size_t k = 0; k < i.result.size(); ++k) {
                            auto *tuple_get = BinaryenLocalGet(
                                ctx.mod, ctx.idx.at(tuple_name),
                                to_btype_multi(i.result_type));
                            stmts.push_back(
                                ctx.set(i.result[k],
                                        BinaryenTupleExtract(
                                            ctx.mod, tuple_get,
                                            static_cast<BinaryenIndex>(k))));
                        }
                    }
                }
                if constexpr (std::is_same_v<T, IRInputRead>) {
                    const auto off =
                        ctx.layout->in_ports_offset + (i.index * 8);
                    stmts.push_back(ctx.set(
                        i.result,
                        BinaryenLoad(
                            ctx.mod, 8, false, 0, 8, BinaryenTypeFloat64(),
                            field_addr(ctx.mod, static_cast<uint32_t>(off)),
                            "0")));
                }
                if constexpr (std::is_same_v<T, IROutputWrite>) {
                    const auto off =
                        ctx.layout->out_ports_offset + (i.index * 8);
                    stmts.push_back(BinaryenStore(
                        ctx.mod, 8, 0, 8,
                        field_addr(ctx.mod, static_cast<uint32_t>(off)),
                        ctx.get(i.value), BinaryenTypeFloat64(), "0"));
                }
                if constexpr (std::is_same_v<T, IRDelayRead>) {
                    const auto base = ctx.resolve_mem(i.delay);
                    auto *ptr = BinaryenLoad(
                        ctx.mod, 4, false, 0, 4, BinaryenTypeInt32(),
                        field_addr(ctx.mod,
                                   ctx.layout->delay_ptr_offset.at(i.delay)),
                        "0");
                    stmts.push_back(ctx.set(
                        i.result,
                        BinaryenLoad(ctx.mod, 8, false, 0, 8,
                                     BinaryenTypeFloat64(),
                                     BinaryenBinary(ctx.mod, BinaryenAddInt32(),
                                                    base, ptr),
                                     "0")));
                }
                if constexpr (std::is_same_v<T, IRDelayReadDelayed>) {
                    const auto delay_ptr_off =
                        ctx.layout->delay_ptr_offset.at(i.delay);
                    const auto &delay = [&]() -> const IRDelayDecl & {
                        for (const auto &b : ctx.ir->delays)
                            if (b.name == i.delay) return b;
                        throw std::runtime_error("Unknown delay: " + i.delay);
                    }();
                    const auto size_bytes =
                        static_cast<int32_t>(delay.size_elements * 8);
                    auto *mod = ctx.mod;

                    auto delay_f64 = [&]() -> BinaryenExpressionRef {
                        return BinaryenLocalGet(mod, ctx.idx.at(i.delay_ref),
                                                BinaryenTypeFloat64());
                    };

                    auto read_sample =
                        [&](int32_t k_offset) -> BinaryenExpressionRef {
                        auto *i_int = BinaryenUnary(
                            mod, BinaryenTruncSFloat64ToInt32(),
                            BinaryenUnary(mod, BinaryenFloorFloat64(),
                                          delay_f64()));
                        auto *k = BinaryenBinary(
                            mod, BinaryenAddInt32(), i_int,
                            BinaryenConst(mod, BinaryenLiteralInt32(k_offset)));
                        auto *k_bytes = BinaryenBinary(
                            mod, BinaryenMulInt32(), k,
                            BinaryenConst(mod, BinaryenLiteralInt32(8)));
                        auto *mod_k = BinaryenBinary(
                            mod, BinaryenRemSInt32(), k_bytes,
                            BinaryenConst(mod,
                                          BinaryenLiteralInt32(size_bytes)));
                        auto *ptr_val = BinaryenLoad(
                            mod, 4, false, 0, 4, BinaryenTypeInt32(),
                            field_addr(mod, delay_ptr_off), "0");
                        auto *sub = BinaryenBinary(mod, BinaryenAddInt32(),
                                                   ptr_val, mod_k);
                        auto *wrapped = BinaryenBinary(
                            mod, BinaryenRemUInt32(),
                            BinaryenBinary(
                                mod, BinaryenAddInt32(), sub,
                                BinaryenConst(
                                    mod, BinaryenLiteralInt32(size_bytes))),
                            BinaryenConst(mod,
                                          BinaryenLiteralInt32(size_bytes)));
                        return BinaryenLoad(
                            mod, 8, false, 0, 8, BinaryenTypeFloat64(),
                            BinaryenBinary(mod, BinaryenAddInt32(),
                                           ctx.resolve_mem(i.delay), wrapped),
                            "0");
                    };

                    const auto p0_name = "$p0$" + i.result;
                    const auto p1_name = "$p1$" + i.result;
                    const auto p2_name = "$p2$" + i.result;
                    const auto p3_name = "$p3$" + i.result;
                    const auto fr_name = "$fr$" + i.result;
                    ctx.ensure_var(p0_name, IRType::Float);
                    ctx.ensure_var(p1_name, IRType::Float);
                    ctx.ensure_var(p2_name, IRType::Float);
                    ctx.ensure_var(p3_name, IRType::Float);
                    ctx.ensure_var(fr_name, IRType::Float);

                    stmts.push_back(BinaryenLocalSet(mod, ctx.idx.at(p0_name),
                                                     read_sample(-1)));
                    stmts.push_back(BinaryenLocalSet(mod, ctx.idx.at(p1_name),
                                                     read_sample(0)));
                    stmts.push_back(BinaryenLocalSet(mod, ctx.idx.at(p2_name),
                                                     read_sample(1)));
                    stmts.push_back(BinaryenLocalSet(mod, ctx.idx.at(p3_name),
                                                     read_sample(2)));
                    stmts.push_back(BinaryenLocalSet(
                        mod, ctx.idx.at(fr_name),
                        BinaryenBinary(mod, BinaryenSubFloat64(), delay_f64(),
                                       BinaryenUnary(mod,
                                                     BinaryenFloorFloat64(),
                                                     delay_f64()))));

                    auto p0 = [&]() -> BinaryenExpressionRef {
                        return BinaryenLocalGet(mod, ctx.idx.at(p0_name),
                                                BinaryenTypeFloat64());
                    };
                    auto p1 = [&]() -> BinaryenExpressionRef {
                        return BinaryenLocalGet(mod, ctx.idx.at(p1_name),
                                                BinaryenTypeFloat64());
                    };
                    auto p2 = [&]() -> BinaryenExpressionRef {
                        return BinaryenLocalGet(mod, ctx.idx.at(p2_name),
                                                BinaryenTypeFloat64());
                    };
                    auto p3 = [&]() -> BinaryenExpressionRef {
                        return BinaryenLocalGet(mod, ctx.idx.at(p3_name),
                                                BinaryenTypeFloat64());
                    };
                    auto frac = [&]() -> BinaryenExpressionRef {
                        return BinaryenLocalGet(mod, ctx.idx.at(fr_name),
                                                BinaryenTypeFloat64());
                    };

                    auto f64 = [&](double v) -> BinaryenExpressionRef {
                        return BinaryenConst(mod, BinaryenLiteralFloat64(v));
                    };
                    auto add = [&](auto *a, auto *b) -> BinaryenExpressionRef {
                        return BinaryenBinary(mod, BinaryenAddFloat64(), a, b);
                    };
                    auto mul = [&](auto *a, auto *b) -> BinaryenExpressionRef {
                        return BinaryenBinary(mod, BinaryenMulFloat64(), a, b);
                    };
                    auto neg = [&](auto *a) -> BinaryenExpressionRef {
                        return BinaryenUnary(mod, BinaryenNegFloat64(), a);
                    };

                    auto *coef_a = add(add(neg(p0()), mul(f64(3.0), p1())),
                                       add(mul(f64(-3.0), p2()), p3()));
                    auto *coef_b =
                        add(add(mul(f64(2.0), p0()), mul(f64(-5.0), p1())),
                            add(mul(f64(4.0), p2()), neg(p3())));
                    auto *coef_c = add(neg(p0()), p2());
                    stmts.push_back(ctx.set(
                        i.result,
                        mul(f64(0.5),
                            add(mul(f64(2.0), p1()),
                                mul(frac(),
                                    add(coef_c,
                                        mul(frac(),
                                            add(coef_b,
                                                mul(frac(), coef_a)))))))));
                }
                if constexpr (std::is_same_v<T, IRDelayWrite>) {
                    const auto delay_ptr_off =
                        ctx.layout->delay_ptr_offset.at(i.delay);
                    const auto &delay = *std::ranges::find_if(
                        ctx.ir->delays, [&](const IRDelayDecl &b) -> bool {
                            return b.name == i.delay;
                        });
                    const auto size_bytes =
                        static_cast<int32_t>(delay.size_elements * 8);
                    auto *mod = ctx.mod;
                    const auto ptr_var = "$dptr$" + i.delay;
                    ctx.ensure_raw_var(ptr_var, BinaryenTypeInt32());
                    stmts.push_back(BinaryenLocalSet(
                        mod, ctx.idx.at(ptr_var),
                        BinaryenLoad(mod, 4, false, 0, 4, BinaryenTypeInt32(),
                                     field_addr(mod, delay_ptr_off), "0")));
                    auto *ptr = [&]() -> BinaryenExpressionRef {
                        return BinaryenLocalGet(mod, ctx.idx.at(ptr_var),
                                                BinaryenTypeInt32());
                    }();
                    stmts.push_back(BinaryenStore(
                        mod, 8, 0, 8,
                        BinaryenBinary(mod, BinaryenAddInt32(),
                                       ctx.resolve_mem(i.delay), ptr),
                        ctx.get(i.value), BinaryenTypeFloat64(), "0"));
                    auto *tmp = BinaryenBinary(
                        mod, BinaryenAddInt32(), ptr,
                        BinaryenConst(mod, BinaryenLiteralInt32(8)));
                    auto *cond = BinaryenBinary(
                        mod, BinaryenGeUInt32(), tmp,
                        BinaryenConst(mod, BinaryenLiteralInt32(size_bytes)));
                    auto *new_ptr = BinaryenSelect(
                        mod, cond, BinaryenConst(mod, BinaryenLiteralInt32(0)),
                        tmp);
                    stmts.push_back(BinaryenStore(
                        mod, 4, 0, 4, field_addr(mod, delay_ptr_off), new_ptr,
                        BinaryenTypeInt32(), "0"));
                }
                if constexpr (std::is_same_v<T, IRDelayWriteQuiet>) {
                    const auto delay_ptr_off =
                        ctx.layout->delay_ptr_offset.at(i.delay);
                    const auto &delay = *std::ranges::find_if(
                        ctx.ir->delays, [&](const IRDelayDecl &b) -> bool {
                            return b.name == i.delay;
                        });
                    const auto size_bytes =
                        static_cast<int32_t>(delay.size_elements * 8);
                    auto *mod = ctx.mod;
                    auto *ptr =
                        BinaryenLoad(mod, 4, false, 0, 4, BinaryenTypeInt32(),
                                     field_addr(mod, delay_ptr_off), "0");
                    BinaryenExpressionRef write_addr = nullptr;
                    if (i.delay_ref) {
                        auto *delay_f64 =
                            BinaryenLocalGet(mod, ctx.idx.at(*i.delay_ref),
                                             BinaryenTypeFloat64());
                        auto *n_int = BinaryenUnary(
                            mod, BinaryenTruncSFloat64ToInt32(),
                            BinaryenUnary(mod, BinaryenFloorFloat64(),
                                          delay_f64));
                        auto *n_bytes = BinaryenBinary(
                            mod, BinaryenMulInt32(), n_int,
                            BinaryenConst(mod, BinaryenLiteralInt32(8)));
                        auto *mod_k = BinaryenBinary(
                            mod, BinaryenRemSInt32(), n_bytes,
                            BinaryenConst(mod,
                                          BinaryenLiteralInt32(size_bytes)));
                        auto *raw =
                            BinaryenBinary(mod, BinaryenAddInt32(), ptr, mod_k);
                        write_addr = BinaryenBinary(
                            mod, BinaryenRemUInt32(),
                            BinaryenBinary(
                                mod, BinaryenAddInt32(), raw,
                                BinaryenConst(
                                    mod, BinaryenLiteralInt32(size_bytes))),
                            BinaryenConst(mod,
                                          BinaryenLiteralInt32(size_bytes)));
                    } else {
                        write_addr = ptr;
                    }
                    stmts.push_back(BinaryenStore(
                        mod, 8, 0, 8,
                        BinaryenBinary(mod, BinaryenAddInt32(),
                                       ctx.resolve_mem(i.delay), write_addr),
                        ctx.get(i.value), BinaryenTypeFloat64(), "0"));
                }
                if constexpr (std::is_same_v<T, IRGlobalRead>) {
                    auto *g = BinaryenGlobalGet(ctx.mod, i.name.c_str(),
                                                to_btype(i.type));
                    BinaryenExpressionRef expr = g;
                    if (i.name == "SAMPLE_RATE")
                        expr = BinaryenConst(
                            ctx.mod, BinaryenLiteralFloat64(ctx.sample_rate));

                    stmts.push_back(ctx.set(i.result, expr));
                }
                if constexpr (std::is_same_v<T, IRIf>) {
                    auto make_then = [&]() -> BinaryenExpressionRef {
                        auto then_s = emit_stmts(ctx, i.body->then_body);
                        return BinaryenBlock(
                            ctx.mod, nullptr, then_s.data(),
                            static_cast<BinaryenIndex>(then_s.size()),
                            BinaryenTypeNone());
                    };
                    auto make_else = [&]() -> BinaryenExpressionRef {
                        if (i.body->else_body.empty())
                            return BinaryenNop(ctx.mod);
                        auto else_s = emit_stmts(ctx, i.body->else_body);
                        return BinaryenBlock(
                            ctx.mod, nullptr, else_s.data(),
                            static_cast<BinaryenIndex>(else_s.size()),
                            BinaryenTypeNone());
                    };

                    std::array<BinaryenExpressionRef, 2> args = {
                        BinaryenConst(ctx.mod, BinaryenLiteralFloat64(0.0)),
                        BinaryenConst(ctx.mod, BinaryenLiteralFloat64(1.0)),
                    };

                    auto *is_geq_one = BinaryenBinary(
                        ctx.mod, BinaryenGeFloat64(), ctx.get(i.condition),
                        BinaryenConst(ctx.mod, BinaryenLiteralFloat64(1.0)));
                    auto *is_leq_zero = BinaryenBinary(
                        ctx.mod, BinaryenLeFloat64(), ctx.get(i.condition),
                        BinaryenConst(ctx.mod, BinaryenLiteralFloat64(0.0)));
                    auto *taken = BinaryenBinary(
                        ctx.mod, BinaryenLtFloat64(),
                        BinaryenCall(ctx.mod, "wasmwasm_uniform", args.data(),
                                     2, BinaryenTypeFloat64()),
                        ctx.get(i.condition));

                    stmts.push_back(BinaryenIf(
                        ctx.mod, is_geq_one, make_then(),
                        BinaryenIf(ctx.mod, is_leq_zero, make_else(),
                                   BinaryenIf(ctx.mod, taken, make_then(),
                                              make_else()))));
                }
                if constexpr (std::is_same_v<T, IRStaticRead>) {
                    const auto off = ctx.layout->static_var_offset.at(i.name);
                    stmts.push_back(ctx.set(
                        i.result, BinaryenLoad(ctx.mod, 8, false, 0, 8,
                                               BinaryenTypeFloat64(),
                                               field_addr(ctx.mod, off), "0")));
                }
                if constexpr (std::is_same_v<T, IRStaticWrite>) {
                    const auto off = ctx.layout->static_var_offset.at(i.name);
                    stmts.push_back(BinaryenStore(
                        ctx.mod, 8, 0, 8, field_addr(ctx.mod, off),
                        ctx.get(i.value), BinaryenTypeFloat64(), "0"));
                }
                if constexpr (std::is_same_v<T, IRParamRead>) {
                    const auto off = ctx.layout->param_offset.at(i.name);
                    stmts.push_back(ctx.set(
                        i.result, BinaryenLoad(ctx.mod, 8, false, 0, 8,
                                               BinaryenTypeFloat64(),
                                               field_addr(ctx.mod, off), "0")));
                }
                if constexpr (std::is_same_v<T, IRParamWrite>) {
                    const auto off = ctx.layout->param_offset.at(i.name);
                    stmts.push_back(BinaryenStore(
                        ctx.mod, 8, 0, 8, field_addr(ctx.mod, off),
                        ctx.get(i.value), BinaryenTypeFloat64(), "0"));
                }
                if constexpr (std::is_same_v<T, IRReturn>) {
                    if (i.value) {
                        if (i.value->size() == 1) {
                            stmts.push_back(ctx.get((*i.value)[0]));
                        } else {
                            std::vector<BinaryenExpressionRef> elems;
                            elems.reserve(i.value->size());
                            for (const auto &v : *i.value)
                                elems.push_back(ctx.get(v));
                            stmts.push_back(BinaryenTupleMake(
                                ctx.mod, elems.data(),
                                static_cast<BinaryenIndex>(elems.size())));
                        }
                    }
                }
                if constexpr (std::is_same_v<T, IRMemRead>) {
                    stmts.push_back(ctx.set(
                        i.result,
                        BinaryenLoad(ctx.mod, 8, false, i.ref.byte_offset, 8,
                                     BinaryenTypeFloat64(),
                                     ctx.resolve_mem(i.ref.buffer), "0")));
                }
                if constexpr (std::is_same_v<T, IRMemWrite>) {
                    stmts.push_back(BinaryenStore(
                        ctx.mod, 8, i.ref.byte_offset, 8,
                        ctx.resolve_mem(i.ref.buffer), ctx.get(i.value),
                        BinaryenTypeFloat64(), "0"));
                }
                if constexpr (std::is_same_v<T, IRDie>) {
                    auto slot_index_of = [&]() -> BinaryenExpressionRef {
                        return BinaryenBinary(
                            ctx.mod, BinaryenDivUInt32(),
                            BinaryenBinary(
                                ctx.mod, BinaryenSubInt32(),
                                instance_base_get(ctx.mod),
                                BinaryenConst(
                                    ctx.mod,
                                    BinaryenLiteralInt32(static_cast<int32_t>(
                                        ctx.layout->module_base)))),
                            BinaryenConst(
                                ctx.mod,
                                BinaryenLiteralInt32(static_cast<int32_t>(
                                    ctx.layout->slot_stride))));
                    };

                    const std::string count_var = "$die_pending_count";
                    ctx.ensure_raw_var(count_var, BinaryenTypeInt32());
                    const auto count_idx = ctx.idx.at(count_var);

                    stmts.push_back(BinaryenLocalSet(
                        ctx.mod, count_idx,
                        BinaryenLoad(
                            ctx.mod, 4, false, 0, 4, BinaryenTypeInt32(),
                            BinaryenConst(
                                ctx.mod,
                                BinaryenLiteralInt32(static_cast<int32_t>(
                                    ctx.layout->pending_kill_count_addr))),
                            "0")));

                    auto *pending_addr = BinaryenBinary(
                        ctx.mod, BinaryenAddInt32(),
                        BinaryenConst(ctx.mod,
                                      BinaryenLiteralInt32(static_cast<int32_t>(
                                          ctx.layout->pending_kills_base))),
                        BinaryenBinary(
                            ctx.mod, BinaryenMulInt32(),
                            BinaryenLocalGet(ctx.mod, count_idx,
                                             BinaryenTypeInt32()),
                            BinaryenConst(ctx.mod, BinaryenLiteralInt32(4))));
                    stmts.push_back(BinaryenStore(ctx.mod, 4, 0, 4,
                                                  pending_addr, slot_index_of(),
                                                  BinaryenTypeInt32(), "0"));

                    stmts.push_back(BinaryenStore(
                        ctx.mod, 4, 0, 4,
                        BinaryenConst(
                            ctx.mod, BinaryenLiteralInt32(static_cast<int32_t>(
                                         ctx.layout->pending_kill_count_addr))),
                        BinaryenBinary(
                            ctx.mod, BinaryenAddInt32(),
                            BinaryenLocalGet(ctx.mod, count_idx,
                                             BinaryenTypeInt32()),
                            BinaryenConst(ctx.mod, BinaryenLiteralInt32(1))),
                        BinaryenTypeInt32(), "0"));
                }
            },
            instr);
    }

    return stmts;
}

void emit_function(const IRFunction &fn, BinaryenModuleRef mod,
                   double sample_rate, const IRModule &ir,
                   const InstanceLayout &layout) {
    FnCtx ctx{
        .mod = mod,
        .sample_rate = sample_rate,
        .ir = &ir,
        .layout = &layout,
        .idx = {},
        .param_types = {},
        .var_types = {},
    };
    ctx.add_param("$instance_base", IRType::Int);
    for (const auto &p : fn.params) ctx.add_param(p.name, p.type);
    prescan(ctx, fn.body);

    BinaryenExpressionRef body = nullptr;
    try {
        body = emit_body(ctx, fn.body, fn.return_type);
    } catch (const std::exception &ex) {
        throw std::runtime_error(std::string(ex.what()) + " (in function '" +
                                 fn.name + "')");
    }

    const auto wasm_name = pfx(ir, fn.name);
    BinaryenAddFunction(
        mod, wasm_name.c_str(),
        BinaryenTypeCreate(ctx.param_types.data(),
                           static_cast<BinaryenIndex>(ctx.param_types.size())),
        to_btype_multi(fn.return_type), ctx.var_types.data(),
        static_cast<BinaryenIndex>(ctx.var_types.size()), body);
}

void emit_init_delays(const IRModule &ir, BinaryenModuleRef mod,
                      const InstanceLayout &layout) {
    std::vector<BinaryenExpressionRef> all_loops;
    std::array<BinaryenType, 1> var_types = {BinaryenTypeInt32()};
    constexpr BinaryenIndex counter_local = 1;

    for (const auto &delay : ir.delays) {
        const auto n = static_cast<int32_t>(delay.size_elements);

        const auto idx_local = [&]() -> BinaryenExpressionRef {
            return BinaryenLocalGet(mod, counter_local, BinaryenTypeInt32());
        };
        auto *i_as_f64 =
            BinaryenUnary(mod, BinaryenConvertUInt32ToFloat64(), idx_local());

        std::array<BinaryenExpressionRef, 2> call_args = {
            instance_base_get(mod), i_as_f64};
        const auto init_fn_name = pfx(ir, delay.init_fn);
        auto *init_val =
            BinaryenCall(mod, init_fn_name.c_str(), call_args.data(), 2,
                         BinaryenTypeFloat64());
        auto *byte_offset =
            BinaryenBinary(mod, BinaryenMulInt32(), idx_local(),
                           BinaryenConst(mod, BinaryenLiteralInt32(8)));

        auto *store = BinaryenStore(
            mod, 8, 0, 8,
            BinaryenBinary(
                mod, BinaryenAddInt32(),
                field_addr(mod, layout.delay_buffer_offset.at(delay.name)),
                byte_offset),
            init_val, BinaryenTypeFloat64(), "0");

        auto *inc = BinaryenLocalSet(
            mod, counter_local,
            BinaryenBinary(mod, BinaryenAddInt32(), idx_local(),
                           BinaryenConst(mod, BinaryenLiteralInt32(1))));

        const auto loop_label = pfx(ir, delay.name + "$init_loop");
        auto *br = BinaryenBreak(
            mod, loop_label.c_str(),
            BinaryenBinary(mod, BinaryenLtUInt32(), idx_local(),
                           BinaryenConst(mod, BinaryenLiteralInt32(n))),
            nullptr);

        std::array<BinaryenExpressionRef, 3> loop_body = {store, inc, br};
        auto *loop_block = BinaryenBlock(mod, nullptr, loop_body.data(),
                                         loop_body.size(), BinaryenTypeNone());

        std::array<BinaryenExpressionRef, 2> buf_init = {
            BinaryenLocalSet(mod, counter_local,
                             BinaryenConst(mod, BinaryenLiteralInt32(0))),
            BinaryenLoop(mod, loop_label.c_str(), loop_block),
        };
        all_loops.push_back(BinaryenBlock(mod, nullptr, buf_init.data(),
                                          buf_init.size(), BinaryenTypeNone()));

        all_loops.push_back(BinaryenStore(
            mod, 4, 0, 4,
            field_addr(mod, layout.delay_ptr_offset.at(delay.name)),
            BinaryenConst(mod, BinaryenLiteralInt32(0)), BinaryenTypeInt32(),
            "0"));
    }

    for (const auto &[pname, pdefault] : ir.params)
        all_loops.push_back(BinaryenStore(
            mod, 8, 0, 8, field_addr(mod, layout.param_offset.at(pname)),
            BinaryenConst(mod, BinaryenLiteralFloat64(pdefault)),
            BinaryenTypeFloat64(), "0"));

    auto *body =
        all_loops.empty()
            ? BinaryenNop(mod)
            : BinaryenBlock(mod, nullptr, all_loops.data(),
                            static_cast<BinaryenIndex>(all_loops.size()),
                            BinaryenTypeNone());

    std::array<BinaryenType, 1> param_types = {BinaryenTypeInt32()};
    const auto init_name = pfx(ir, ir.init_fn);
    BinaryenAddFunction(
        mod, init_name.c_str(),
        BinaryenTypeCreate(param_types.data(),
                           static_cast<BinaryenIndex>(param_types.size())),
        BinaryenTypeNone(), var_types.data(), var_types.size(), body);
}

void import_math(BinaryenModuleRef main_mod, BinaryenModuleRef math_mod) {
    std::unordered_map<std::string, std::string> exported;
    const auto num_exports = BinaryenGetNumExports(math_mod);
    for (BinaryenIndex i = 0; i < num_exports; i++) {
        auto *ex = BinaryenGetExportByIndex(math_mod, i);
        if (BinaryenExportGetKind(ex) != BinaryenExternalFunction()) continue;
        exported[BinaryenExportGetValue(ex)] = BinaryenExportGetName(ex);
    }

    const auto num_globals = BinaryenGetNumGlobals(math_mod);
    for (BinaryenIndex i = 0; i < num_globals; i++) {
        auto *g = BinaryenGetGlobalByIndex(math_mod, i);
        auto *init =
            BinaryenExpressionCopy(BinaryenGlobalGetInitExpr(g), main_mod);
        BinaryenAddGlobal(main_mod, BinaryenGlobalGetName(g),
                          BinaryenGlobalGetType(g), BinaryenGlobalIsMutable(g),
                          init);
    }

    const auto num_fns = BinaryenGetNumFunctions(math_mod);
    for (BinaryenIndex i = 0; i < num_fns; i++) {
        auto *fn = BinaryenGetFunctionByIndex(math_mod, i);

        const std::string int_name = BinaryenFunctionGetName(fn);
        const auto is_exp = exported.contains(int_name);
        const auto &ext_name = is_exp ? exported.at(int_name) : int_name;

        auto *fn_body = BinaryenFunctionGetBody(fn);
        if (fn_body == nullptr) continue;

        const auto num_vars = BinaryenFunctionGetNumVars(fn);
        std::vector<BinaryenType> var_types(num_vars);
        for (BinaryenIndex j = 0; j < num_vars; j++)
            var_types[j] = BinaryenFunctionGetVar(fn, j);

        auto *body = BinaryenExpressionCopy(fn_body, main_mod);
        BinaryenAddFunction(
            main_mod, ext_name.c_str(), BinaryenFunctionGetParams(fn),
            BinaryenFunctionGetResults(fn), var_types.data(), num_vars, body);
    }
}

} // namespace

void emit_ir(const IRModule &ir, BinaryenModuleRef mod,
             BinaryenModuleRef math_module, double sample_rate,
             const InstanceLayout &layout) {
    BinaryenModuleSetFeatures(mod, BinaryenFeatureAll());
    if (!BinaryenHasMemory(mod))
        BinaryenAddMemoryImport(mod, "0", "env", "memory", 0);

    if (BinaryenGetFunction(mod, "wasmwasm_sin") == nullptr)
        import_math(mod, math_module);

    for (const auto &fn : ir.functions)
        emit_function(fn, mod, sample_rate, ir, layout);

    emit_init_delays(ir, mod, layout);
}
