#include "emit.hpp"

#include "ast/ast.hpp"
#include "binaryen-c.h"
#include "ir.hpp"
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

auto to_bop_vec(Operation op) -> BinaryenOp {
    switch (op) {
    case Operation::Add:
        return BinaryenAddVecF64x2();
    case Operation::Sub:
        return BinaryenSubVecF64x2();
    case Operation::Mul:
        return BinaryenMulVecF64x2();
    case Operation::Div:
        return BinaryenDivVecF64x2();
    default:
        std::unreachable();
    }
}

auto splat_f64(BinaryenModuleRef mod, double v) -> BinaryenExpressionRef {
    return BinaryenUnary(mod, BinaryenSplatVecF64x2(),
                         BinaryenConst(mod, BinaryenLiteralFloat64(v)));
}

struct FnCtx {
    BinaryenModuleRef mod{};
    double sample_rate{};
    const IRModule *ir{};

    std::unordered_map<std::string, BinaryenIndex> idx;
    std::vector<BinaryenType> param_types;
    std::vector<BinaryenType> var_types;

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

    auto local_type(const std::string &name) -> BinaryenType {
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
                else
                    return BinaryenLocalGet(mod, idx.at(x.name),
                                            local_type(x.name));
            },
            v);
    }

    auto set(const std::string &name, BinaryenExpressionRef val)
        -> BinaryenExpressionRef {
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
                if constexpr (std::is_same_v<T, IRCall>)
                    if (!i.result.empty())
                        ctx.ensure_var(i.result, i.result_type);
                if constexpr (std::is_same_v<T, IRBufferRead>)
                    ctx.ensure_var(i.result, IRType::Float);
                if constexpr (std::is_same_v<T, IRBufferReadDelayed>)
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
            },
            instr);
    }
}

auto emit_stmts(FnCtx &ctx, const std::vector<IRInstr> &body)
    -> std::vector<BinaryenExpressionRef>;

auto emit_body(FnCtx &ctx, const std::vector<IRInstr> &body, IRType ret_type)
    -> BinaryenExpressionRef {
    auto stmts = emit_stmts(ctx, body);
    if (stmts.empty()) return BinaryenNop(ctx.mod);
    return BinaryenBlock(ctx.mod, nullptr, stmts.data(),
                         static_cast<BinaryenIndex>(stmts.size()),
                         to_btype(ret_type));
}

auto emit_stmts(FnCtx &ctx, const std::vector<IRInstr> &body)
    -> std::vector<BinaryenExpressionRef> {
    std::vector<BinaryenExpressionRef> stmts;

    for (const auto &instr : body) {
        std::visit(
            [&](const auto &i) -> auto {
                using T = std::decay_t<decltype(i)>;

                if constexpr (std::is_same_v<T, IRBinOp>) {
                    if (i.op == Operation::Lt || i.op == Operation::Gt) {
                        const auto cmp_op = i.op == Operation::Lt
                                                ? BinaryenLtFloat64()
                                                : BinaryenGtFloat64();
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
                    std::vector<BinaryenExpressionRef> args;
                    args.reserve(i.args.size());
                    for (const auto &a : i.args) args.push_back(ctx.get(a));
                    const bool is_math = i.callee.starts_with("wasmwasm_");
                    const auto callee_name =
                        is_math ? i.callee : pfx(*ctx.ir, i.callee);
                    auto *call =
                        BinaryenCall(ctx.mod, callee_name.c_str(), args.data(),
                                     static_cast<BinaryenIndex>(args.size()),
                                     to_btype(i.result_type));
                    if (i.result.empty())
                        stmts.push_back(call);
                    else
                        stmts.push_back(ctx.set(i.result, call));
                }
                if constexpr (std::is_same_v<T, IRInputRead>) {
                    const auto gname =
                        pfx(*ctx.ir, "IN$" + std::to_string(i.index));
                    stmts.push_back(ctx.set(
                        i.result, BinaryenGlobalGet(ctx.mod, gname.c_str(),
                                                    BinaryenTypeFloat64())));
                }
                if constexpr (std::is_same_v<T, IROutputWrite>) {
                    const auto gname =
                        pfx(*ctx.ir, "OUT$" + std::to_string(i.index));
                    stmts.push_back(BinaryenGlobalSet(ctx.mod, gname.c_str(),
                                                      ctx.get(i.value)));
                }
                if constexpr (std::is_same_v<T, IRBufferRead>) {
                    const auto base = ctx.ir->buffer_base(i.buffer);
                    const auto pname = pfx(*ctx.ir, i.buffer);
                    auto *ptr = BinaryenGlobalGet(ctx.mod, pname.c_str(),
                                                  BinaryenTypeInt32());
                    stmts.push_back(
                        ctx.set(i.result, BinaryenLoad(ctx.mod, 8, false, base,
                                                       8, BinaryenTypeFloat64(),
                                                       ptr, "memory")));
                }
                if constexpr (std::is_same_v<T, IRBufferReadDelayed>) {
                    const auto base = ctx.ir->buffer_base(i.buffer);
                    const auto pname = pfx(*ctx.ir, i.buffer);
                    const auto &buf = [&]() -> const IRBufferDecl & {
                        for (const auto &b : ctx.ir->buffers)
                            if (b.name == i.buffer) return b;
                        throw std::runtime_error("Unknown buffer: " + i.buffer);
                    }();
                    const auto size_bytes =
                        static_cast<int32_t>(buf.size_elements * 8);
                    auto *mod = ctx.mod;
                    const auto didx = ctx.idx.at(i.delay_ref);

                    auto delay_f64 = [&]() -> BinaryenExpressionRef {
                        return BinaryenLocalGet(mod, didx,
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
                        auto *sub = BinaryenBinary(
                            mod, BinaryenAddInt32(),
                            BinaryenGlobalGet(mod, pname.c_str(),
                                              BinaryenTypeInt32()),
                            mod_k);
                        auto *wrapped = BinaryenBinary(
                            mod, BinaryenRemUInt32(),
                            BinaryenBinary(
                                mod, BinaryenAddInt32(), sub,
                                BinaryenConst(
                                    mod, BinaryenLiteralInt32(size_bytes))),
                            BinaryenConst(mod,
                                          BinaryenLiteralInt32(size_bytes)));
                        return BinaryenLoad(mod, 8, false, base, 8,
                                            BinaryenTypeFloat64(), wrapped,
                                            "memory");
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
                if constexpr (std::is_same_v<T, IRBufferWrite>) {
                    const auto base = ctx.ir->buffer_base(i.buffer);
                    const auto &buf = *std::ranges::find_if(
                        ctx.ir->buffers, [&](const IRBufferDecl &b) -> bool {
                            return b.name == i.buffer;
                        });
                    const auto size_bytes =
                        static_cast<int32_t>(buf.size_elements * 8);
                    const auto pname = pfx(*ctx.ir, i.buffer);
                    auto *ptr = BinaryenGlobalGet(ctx.mod, pname.c_str(),
                                                  BinaryenTypeInt32());
                    stmts.push_back(BinaryenStore(
                        ctx.mod, 8, base, 8, ptr, ctx.get(i.value),
                        BinaryenTypeFloat64(), "memory"));
                    auto *tmp = BinaryenBinary(
                        ctx.mod, BinaryenAddInt32(), ptr,
                        BinaryenConst(ctx.mod, BinaryenLiteralInt32(8)));
                    auto *cond = BinaryenBinary(
                        ctx.mod, BinaryenGeUInt32(), tmp,
                        BinaryenConst(ctx.mod,
                                      BinaryenLiteralInt32(size_bytes)));
                    auto *new_ptr = BinaryenSelect(
                        ctx.mod, cond,
                        BinaryenConst(ctx.mod, BinaryenLiteralInt32(0)), tmp);
                    stmts.push_back(
                        BinaryenGlobalSet(ctx.mod, pname.c_str(), new_ptr));
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
                    std::array<BinaryenExpressionRef, 2> args = {
                        BinaryenConst(ctx.mod, BinaryenLiteralFloat64(0.0)),
                        BinaryenConst(ctx.mod, BinaryenLiteralFloat64(1.0)),
                    };
                    auto *rng =
                        BinaryenCall(ctx.mod, "wasmwasm_uniform", args.data(),
                                     2, BinaryenTypeFloat64());
                    auto *taken = BinaryenBinary(ctx.mod, BinaryenLtFloat64(),
                                                 rng, ctx.get(i.condition));
                    auto then_s = emit_stmts(ctx, i.body->then_body);
                    auto *then_block =
                        BinaryenBlock(ctx.mod, nullptr, then_s.data(),
                                      static_cast<BinaryenIndex>(then_s.size()),
                                      BinaryenTypeNone());
                    BinaryenExpressionRef else_block = nullptr;
                    if (!i.body->else_body.empty()) {
                        auto else_s = emit_stmts(ctx, i.body->else_body);
                        else_block = BinaryenBlock(
                            ctx.mod, nullptr, else_s.data(),
                            static_cast<BinaryenIndex>(else_s.size()),
                            BinaryenTypeNone());
                    }
                    stmts.push_back(
                        BinaryenIf(ctx.mod, taken, then_block, else_block));
                }
                if constexpr (std::is_same_v<T, IRStaticRead>) {
                    const auto gname = pfx(*ctx.ir, i.name);
                    stmts.push_back(ctx.set(
                        i.result, BinaryenGlobalGet(ctx.mod, gname.c_str(),
                                                    BinaryenTypeFloat64())));
                }
                if constexpr (std::is_same_v<T, IRStaticWrite>) {
                    const auto gname = pfx(*ctx.ir, i.name);
                    stmts.push_back(BinaryenGlobalSet(ctx.mod, gname.c_str(),
                                                      ctx.get(i.value)));
                }
                if constexpr (std::is_same_v<T, IRReturn>) {
                    if (i.value) stmts.push_back(ctx.get(*i.value));
                }
            },
            instr);
    }

    return stmts;
}

void emit_function(const IRFunction &fn, BinaryenModuleRef mod,
                   double sample_rate, const IRModule &ir) {
    FnCtx ctx{
        .mod = mod,
        .sample_rate = sample_rate,
        .ir = &ir,
        .idx = {},
        .param_types = {},
        .var_types = {},
    };
    for (const auto &p : fn.params) ctx.add_param(p.name, p.type);
    prescan(ctx, fn.body);

    auto *body = emit_body(ctx, fn.body, fn.return_type);

    const auto wasm_name = pfx(ir, fn.name);
    BinaryenAddFunction(
        mod, wasm_name.c_str(),
        BinaryenTypeCreate(ctx.param_types.data(),
                           static_cast<BinaryenIndex>(ctx.param_types.size())),
        to_btype(fn.return_type), ctx.var_types.data(),
        static_cast<BinaryenIndex>(ctx.var_types.size()), body);
}

void emit_init_buffers(const IRModule &ir, BinaryenModuleRef mod) {
    std::vector<BinaryenExpressionRef> all_loops;
    std::array<BinaryenType, 1> idx_type = {BinaryenTypeInt32()};

    for (const auto &buf : ir.buffers) {
        const auto base = ir.buffer_base(buf.name);
        const auto n = static_cast<int32_t>(buf.size_elements);

        const auto idx_local = [&]() -> BinaryenExpressionRef {
            return BinaryenLocalGet(mod, 0, BinaryenTypeInt32());
        };
        auto *i_as_f64 =
            BinaryenUnary(mod, BinaryenConvertUInt32ToFloat64(), idx_local());

        std::array<BinaryenExpressionRef, 1> call_args = {i_as_f64};
        const auto init_fn_name = pfx(ir, buf.init_fn);
        auto *init_val =
            BinaryenCall(mod, init_fn_name.c_str(), call_args.data(), 1,
                         BinaryenTypeFloat64());
        auto *byte_offset =
            BinaryenBinary(mod, BinaryenMulInt32(), idx_local(),
                           BinaryenConst(mod, BinaryenLiteralInt32(8)));

        auto *store = BinaryenStore(mod, 8, base, 8, byte_offset, init_val,
                                    BinaryenTypeFloat64(), "memory");

        auto *inc = BinaryenLocalSet(
            mod, 0,
            BinaryenBinary(mod, BinaryenAddInt32(),
                           BinaryenLocalGet(mod, 0, BinaryenTypeInt32()),
                           BinaryenConst(mod, BinaryenLiteralInt32(1))));

        const auto loop_label = pfx(ir, buf.name + "$init_loop");
        auto *br = BinaryenBreak(
            mod, loop_label.c_str(),
            BinaryenBinary(mod, BinaryenLtUInt32(),
                           BinaryenLocalGet(mod, 0, BinaryenTypeInt32()),
                           BinaryenConst(mod, BinaryenLiteralInt32(n))),
            nullptr);

        std::array<BinaryenExpressionRef, 3> loop_body = {store, inc, br};
        auto *loop_block = BinaryenBlock(mod, nullptr, loop_body.data(),
                                         loop_body.size(), BinaryenTypeNone());

        std::array<BinaryenExpressionRef, 2> buf_init = {
            BinaryenLocalSet(mod, 0,
                             BinaryenConst(mod, BinaryenLiteralInt32(0))),
            BinaryenLoop(mod, loop_label.c_str(), loop_block),
        };
        all_loops.push_back(BinaryenBlock(mod, nullptr, buf_init.data(),
                                          buf_init.size(), BinaryenTypeNone()));
    }

    auto *body =
        all_loops.empty()
            ? BinaryenNop(mod)
            : BinaryenBlock(mod, nullptr, all_loops.data(),
                            static_cast<BinaryenIndex>(all_loops.size()),
                            BinaryenTypeNone());

    const auto init_name = pfx(ir, ir.init_fn);
    BinaryenAddFunction(mod, init_name.c_str(), BinaryenTypeCreate(nullptr, 0),
                        BinaryenTypeNone(), idx_type.data(), 1, body);
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

struct FnCtxVec {
    BinaryenModuleRef mod{};
    double sample_rate{};
    const IRModule *ir{};
    std::unordered_map<std::string, BinaryenIndex> idx;
    std::vector<BinaryenType> var_types;

    void ensure_var(const std::string &name) {
        if (!idx.contains(name)) {
            idx[name] = static_cast<BinaryenIndex>(var_types.size());
            var_types.push_back(BinaryenTypeVec128());
        }
    }

    auto get(const IRValue &v) -> BinaryenExpressionRef {
        return std::visit(
            [&](const auto &x) -> BinaryenExpressionRef {
                using T = std::decay_t<decltype(x)>;
                if constexpr (std::is_same_v<T, IRLiteral>)
                    return splat_f64(mod, x.value);
                else
                    return BinaryenLocalGet(mod, idx.at(x.name),
                                            BinaryenTypeVec128());
            },
            v);
    }

    auto set(const std::string &name, BinaryenExpressionRef val)
        -> BinaryenExpressionRef {
        return BinaryenLocalSet(mod, idx.at(name), val);
    }
};

void prescan_vec(FnCtxVec &ctx, const std::vector<IRInstr> &body) {
    for (const auto &instr : body) {
        std::visit(
            [&](const auto &i) -> auto {
                using T = std::decay_t<decltype(i)>;
                if constexpr (std::is_same_v<T, IRBinOp>)
                    ctx.ensure_var(i.result);
                if constexpr (std::is_same_v<T, IRUnaryNeg>)
                    ctx.ensure_var(i.result);
                if constexpr (std::is_same_v<T, IRAssign>)
                    ctx.ensure_var(i.result);
                if constexpr (std::is_same_v<T, IRCall>)
                    if (!i.result.empty()) ctx.ensure_var(i.result);
                if constexpr (std::is_same_v<T, IRBufferRead>)
                    ctx.ensure_var(i.result);
                if constexpr (std::is_same_v<T, IRGlobalRead>)
                    ctx.ensure_var(i.result);
            },
            instr);
    }
}

auto emit_body_vec(FnCtxVec &ctx, const std::vector<IRInstr> &body)
    -> BinaryenExpressionRef {
    std::vector<BinaryenExpressionRef> stmts;

    for (const auto &instr : body) {
        std::visit(
            [&](const auto &i) -> auto {
                using T = std::decay_t<decltype(i)>;

                if constexpr (std::is_same_v<T, IRBinOp>) {
                    if (i.op == Operation::Lt || i.op == Operation::Gt) {
                        const auto vec_op = i.op == Operation::Lt
                                                ? BinaryenLtVecF64x2()
                                                : BinaryenGtVecF64x2();
                        auto *mask = BinaryenBinary(
                            ctx.mod, vec_op, ctx.get(i.left), ctx.get(i.right));
                        auto *v = BinaryenSIMDTernary(
                            ctx.mod, BinaryenBitselectVec128(),
                            splat_f64(ctx.mod, 1.0), splat_f64(ctx.mod, 0.0),
                            mask);
                        stmts.push_back(ctx.set(i.result, v));
                    } else {
                        stmts.push_back(ctx.set(
                            i.result,
                            BinaryenBinary(ctx.mod, to_bop_vec(i.op),
                                           ctx.get(i.left), ctx.get(i.right))));
                    }
                }
                if constexpr (std::is_same_v<T, IRUnaryNeg>) {
                    stmts.push_back(ctx.set(
                        i.result, BinaryenUnary(ctx.mod, BinaryenNegVecF64x2(),
                                                ctx.get(i.operand))));
                }
                if constexpr (std::is_same_v<T, IRAssign>) {
                    stmts.push_back(ctx.set(i.result, ctx.get(i.value)));
                }
                if constexpr (std::is_same_v<T, IRCall>) {
                    const auto vec_callee = i.callee + "_f64x2";
                    std::vector<BinaryenExpressionRef> args;
                    args.reserve(i.args.size());
                    for (const auto &a : i.args) args.push_back(ctx.get(a));
                    const BinaryenType ret_type = i.result.empty()
                                                      ? BinaryenTypeNone()
                                                      : BinaryenTypeVec128();
                    auto *call = BinaryenCall(
                        ctx.mod, vec_callee.c_str(), args.data(),
                        static_cast<BinaryenIndex>(args.size()), ret_type);
                    if (i.result.empty())
                        stmts.push_back(call);
                    else
                        stmts.push_back(ctx.set(i.result, call));
                }
                if constexpr (std::is_same_v<T, IRBufferRead>) {
                    const auto base = ctx.ir->buffer_base(i.buffer);
                    const auto pname = pfx(*ctx.ir, i.buffer);
                    auto *ptr = BinaryenGlobalGet(ctx.mod, pname.c_str(),
                                                  BinaryenTypeInt32());
                    stmts.push_back(
                        ctx.set(i.result, BinaryenLoad(ctx.mod, 16, false, base,
                                                       8, BinaryenTypeVec128(),
                                                       ptr, "memory")));
                }
                if constexpr (std::is_same_v<T, IROutputWrite>) {
                    const auto gname =
                        pfx(*ctx.ir, "OUT$" + std::to_string(i.index) + "_vec");
                    stmts.push_back(BinaryenGlobalSet(ctx.mod, gname.c_str(),
                                                      ctx.get(i.value)));
                }
                if constexpr (std::is_same_v<T, IRBufferWrite>) {
                    const auto base = ctx.ir->buffer_base(i.buffer);
                    const auto &buf = *std::ranges::find_if(
                        ctx.ir->buffers, [&](const IRBufferDecl &b) -> bool {
                            return b.name == i.buffer;
                        });
                    const auto size_bytes =
                        static_cast<int32_t>(buf.size_elements * 8);
                    const auto pname = pfx(*ctx.ir, i.buffer);
                    auto *ptr = BinaryenGlobalGet(ctx.mod, pname.c_str(),
                                                  BinaryenTypeInt32());
                    stmts.push_back(BinaryenStore(
                        ctx.mod, 16, base, 8, ptr, ctx.get(i.value),
                        BinaryenTypeVec128(), "memory"));
                    auto *tmp = BinaryenBinary(
                        ctx.mod, BinaryenAddInt32(), ptr,
                        BinaryenConst(ctx.mod, BinaryenLiteralInt32(16)));
                    auto *cond = BinaryenBinary(
                        ctx.mod, BinaryenGeUInt32(), tmp,
                        BinaryenConst(ctx.mod,
                                      BinaryenLiteralInt32(size_bytes)));
                    auto *new_ptr = BinaryenSelect(
                        ctx.mod, cond,
                        BinaryenConst(ctx.mod, BinaryenLiteralInt32(0)), tmp);
                    stmts.push_back(
                        BinaryenGlobalSet(ctx.mod, pname.c_str(), new_ptr));
                }
                if constexpr (std::is_same_v<T, IRGlobalRead>) {
                    BinaryenExpressionRef expr = nullptr;
                    if (i.name == "SAMPLE_RATE") {
                        expr = splat_f64(ctx.mod, ctx.sample_rate);
                    } else {
                        auto *g = BinaryenGlobalGet(ctx.mod, i.name.c_str(),
                                                    to_btype(i.type));
                        expr =
                            BinaryenUnary(ctx.mod, BinaryenSplatVecF64x2(), g);
                    }
                    stmts.push_back(ctx.set(i.result, expr));
                }
            },
            instr);
    }

    if (stmts.empty()) return BinaryenNop(ctx.mod);
    return BinaryenBlock(ctx.mod, nullptr, stmts.data(),
                         static_cast<BinaryenIndex>(stmts.size()),
                         BinaryenTypeNone());
}

void emit_function_vec(const IRFunction &fn, BinaryenModuleRef mod,
                       double sample_rate, const IRModule &ir) {
    FnCtxVec ctx{
        .mod = mod,
        .sample_rate = sample_rate,
        .ir = &ir,
        .idx = {},
        .var_types = {},
    };
    prescan_vec(ctx, fn.body);
    auto *body = emit_body_vec(ctx, fn.body);
    const auto wasm_name = pfx(ir, fn.name + "_vec");
    BinaryenAddFunction(mod, wasm_name.c_str(), BinaryenTypeCreate(nullptr, 0),
                        BinaryenTypeNone(), ctx.var_types.data(),
                        static_cast<BinaryenIndex>(ctx.var_types.size()), body);
}

auto is_main_vec_eligible(const IRModule &ir) -> bool {
    if (!ir.static_vars.empty()) return false;
    if (!std::ranges::all_of(ir.buffers, [](const IRBufferDecl &b) -> bool {
            return b.size_elements >= 2;
        }))
        return false;
    for (const auto &fn : ir.functions) {
        if (fn.name != ir.main_fn) continue;
        for (const auto &instr : fn.body) {
            if (std::holds_alternative<IRBufferReadDelayed>(instr))
                return false;
            if (std::holds_alternative<IRInputRead>(instr)) return false;
            if (std::holds_alternative<IRIf>(instr)) return false;
            if (const auto *call = std::get_if<IRCall>(&instr))
                if (!call->callee.starts_with("wasmwasm_")) return false;
        }
        return true;
    }
    return false;
}

} // namespace

auto IRModule::buffer_base(const std::string &buf_name) const -> uint32_t {
    auto addr = memory_base;
    for (const auto &b : buffers) {
        if (b.name == buf_name) return addr;
        addr += static_cast<uint32_t>(b.size_elements * 8);
    }
    throw std::runtime_error("Unknown buffer: " + buf_name);
}

auto IRModule::total_buffer_bytes() const -> uint32_t {
    uint32_t total = 0;
    for (const auto &b : buffers)
        total += static_cast<uint32_t>(b.size_elements * 8);
    return total;
}

void emit_ir(const IRModule &ir, BinaryenModuleRef mod,
             BinaryenModuleRef math_module, double sample_rate) {
    BinaryenModuleSetFeatures(mod, BinaryenFeatureAll());
    if (!BinaryenHasMemory(mod))
        BinaryenAddMemoryImport(mod, "memory", "env", "memory", 0);

    if (BinaryenGetFunction(mod, "wasmwasm_sin") == nullptr) {
        BinaryenAddMemoryImport(mod, "0", "env", "memory", 0);
        import_math(mod, math_module);
    }

    for (const auto &fn : ir.functions) emit_function(fn, mod, sample_rate, ir);

    for (const auto &buf : ir.buffers) {
        const auto bname = pfx(ir, buf.name);
        BinaryenAddGlobal(mod, bname.c_str(), BinaryenTypeInt32(), true,
                          BinaryenConst(mod, BinaryenLiteralInt32(0)));
    }

    for (const auto &sv : ir.static_vars) {
        const auto gname = pfx(ir, sv.name);
        BinaryenAddGlobal(mod, gname.c_str(), to_btype(sv.type), true,
                          BinaryenConst(mod, BinaryenLiteralFloat64(0.0)));
    }

    for (size_t i = 0; i < ir.num_inputs; ++i) {
        const auto iname = pfx(ir, "IN$" + std::to_string(i));
        BinaryenAddGlobal(mod, iname.c_str(), BinaryenTypeFloat64(), true,
                          BinaryenConst(mod, BinaryenLiteralFloat64(0.0)));
    }
    for (size_t i = 0; i < ir.num_outputs; ++i) {
        const auto oname = pfx(ir, "OUT$" + std::to_string(i));
        BinaryenAddGlobal(mod, oname.c_str(), BinaryenTypeFloat64(), true,
                          BinaryenConst(mod, BinaryenLiteralFloat64(0.0)));
    }

    if (is_main_vec_eligible(ir)) {
        std::array<uint8_t, 16> zeros = {};
        for (size_t i = 0; i < ir.num_outputs; ++i) {
            const auto oname = pfx(ir, "OUT$" + std::to_string(i) + "_vec");
            BinaryenAddGlobal(
                mod, oname.c_str(), BinaryenTypeVec128(), true,
                BinaryenConst(mod, BinaryenLiteralVec128(zeros.data())));
        }
        for (const auto &fn : ir.functions) {
            if (fn.name == ir.main_fn) {
                emit_function_vec(fn, mod, sample_rate, ir);
                break;
            }
        }
    }

    emit_init_buffers(ir, mod);
}
