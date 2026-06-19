#include "emit.hpp"

#include "ast/ast.hpp"
#include "binaryen-c.h"
#include "ir.hpp"
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

auto to_btype(IRType t) -> BinaryenType {
    switch (t) {
    case IRType::Float:
        return BinaryenTypeFloat64();
    case IRType::Int:
        return BinaryenTypeInt32();
    case IRType::Void:
        return BinaryenTypeNone();
    }
    std::unreachable();
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
    }
    std::unreachable();
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
            },
            instr);
    }
}

auto emit_body(FnCtx &ctx, const std::vector<IRInstr> &body, IRType ret_type)
    -> BinaryenExpressionRef {
    std::vector<BinaryenExpressionRef> stmts;

    for (const auto &instr : body) {
        std::visit(
            [&](const auto &i) -> auto {
                using T = std::decay_t<decltype(i)>;

                if constexpr (std::is_same_v<T, IRBinOp>) {
                    stmts.push_back(
                        ctx.set(i.result, BinaryenBinary(ctx.mod, to_bop(i.op),
                                                         ctx.get(i.left),
                                                         ctx.get(i.right))));
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
                    auto *call =
                        BinaryenCall(ctx.mod, i.callee.c_str(), args.data(),
                                     static_cast<BinaryenIndex>(args.size()),
                                     to_btype(i.result_type));
                    if (i.result.empty())
                        stmts.push_back(call);
                    else
                        stmts.push_back(ctx.set(i.result, call));
                }
                if constexpr (std::is_same_v<T, IRBufferRead>) {
                    const auto base = ctx.ir->buffer_base(i.buffer);
                    auto *ptr = BinaryenGlobalGet(ctx.mod, i.buffer.c_str(),
                                                  BinaryenTypeInt32());
                    stmts.push_back(
                        ctx.set(i.result, BinaryenLoad(ctx.mod, 8, false, base,
                                                       8, BinaryenTypeFloat64(),
                                                       ptr, "memory")));
                }
                if constexpr (std::is_same_v<T, IRBufferReadDelayed>) {
                    const auto base = ctx.ir->buffer_base(i.buffer);
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
                            BinaryenGlobalGet(mod, i.buffer.c_str(),
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
                    stmts.push_back(BinaryenGlobalSet(
                        ctx.mod, (i.buffer + "$future").c_str(),
                        ctx.get(i.value)));
                }
                if constexpr (std::is_same_v<T, IRGlobalRead>) {
                    auto *g = BinaryenGlobalGet(ctx.mod, i.name.c_str(),
                                                to_btype(i.type));
                    BinaryenExpressionRef expr = g;
                    if (i.name == "TIME")
                        expr = BinaryenBinary(
                            ctx.mod, BinaryenDivFloat64(), g,
                            BinaryenConst(ctx.mod, BinaryenLiteralFloat64(
                                                       ctx.sample_rate)));
                    else if (i.name == "SAMPLE_RATE")
                        expr = BinaryenConst(
                            ctx.mod, BinaryenLiteralFloat64(ctx.sample_rate));

                    stmts.push_back(ctx.set(i.result, expr));
                }
                if constexpr (std::is_same_v<T, IRReturn>) {
                    if (i.value) stmts.push_back(ctx.get(*i.value));
                }
            },
            instr);
    }

    if (stmts.empty()) return BinaryenNop(ctx.mod);
    return BinaryenBlock(ctx.mod, nullptr, stmts.data(),
                         static_cast<BinaryenIndex>(stmts.size()),
                         to_btype(ret_type));
}

void emit_function(const IRFunction &fn, BinaryenModuleRef mod,
                   double sample_rate, const IRModule &ir) {
    FnCtx ctx{.mod = mod, .sample_rate = sample_rate, .ir = &ir};
    for (const auto &p : fn.params) ctx.add_param(p.name, p.type);
    prescan(ctx, fn.body);

    auto *body = emit_body(ctx, fn.body, fn.return_type);

    BinaryenAddFunction(
        mod, fn.name.c_str(),
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
        auto *init_val =
            BinaryenCall(mod, buf.init_fn.c_str(), call_args.data(), 1,
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

        const auto loop_label = buf.name + "$init_loop";
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

    BinaryenAddFunction(mod, ir.init_fn.c_str(), BinaryenTypeCreate(nullptr, 0),
                        BinaryenTypeNone(), idx_type.data(), 1, body);
    BinaryenAddFunctionExport(mod, ir.init_fn.c_str(), ir.init_fn.c_str());
}

auto make_channel_loop(BinaryenModuleRef mod, BinaryenIndex base_ptr,
                       BinaryenIndex num_samples, BinaryenIndex num_channels,
                       BinaryenIndex sample, BinaryenIndex channel,
                       BinaryenIndex out_val) -> BinaryenExpressionRef {
    auto *out_addr = BinaryenBinary(
        mod, BinaryenAddInt32(),
        BinaryenLocalGet(mod, base_ptr, BinaryenTypeInt32()),
        BinaryenBinary(
            mod, BinaryenMulInt32(),
            BinaryenConst(mod, BinaryenLiteralInt32(4)),
            BinaryenBinary(
                mod, BinaryenAddInt32(),
                BinaryenLocalGet(mod, sample, BinaryenTypeInt32()),
                BinaryenBinary(
                    mod, BinaryenMulInt32(),
                    BinaryenLocalGet(mod, channel, BinaryenTypeInt32()),
                    BinaryenLocalGet(mod, num_samples, BinaryenTypeInt32())))));

    auto *store_out = BinaryenStore(
        mod, 4, 0, 4, out_addr,
        BinaryenUnary(mod, BinaryenDemoteFloat64(),
                      BinaryenLocalGet(mod, out_val, BinaryenTypeFloat64())),
        BinaryenTypeFloat32(), "memory");

    auto *inc_channel = BinaryenLocalSet(
        mod, channel,
        BinaryenBinary(mod, BinaryenAddInt32(),
                       BinaryenLocalGet(mod, channel, BinaryenTypeInt32()),
                       BinaryenConst(mod, BinaryenLiteralInt32(1))));

    auto *br_inner = BinaryenBreak(
        mod, "inner_loop",
        BinaryenBinary(
            mod, BinaryenLtUInt32(),
            BinaryenLocalGet(mod, channel, BinaryenTypeInt32()),
            BinaryenLocalGet(mod, num_channels, BinaryenTypeInt32())),
        nullptr);

    std::array<BinaryenExpressionRef, 3> inner_body = {store_out, inc_channel,
                                                       br_inner};
    auto *inner_block = BinaryenBlock(mod, "inner_block", inner_body.data(),
                                      inner_body.size(), BinaryenTypeNone());
    return BinaryenLoop(mod, "inner_loop", inner_block);
}

auto make_buffer_updates(BinaryenModuleRef mod, const IRModule &ir)
    -> BinaryenExpressionRef {
    std::vector<BinaryenExpressionRef> buf_updates;
    for (const auto &buf : ir.buffers) {
        const auto base = ir.buffer_base(buf.name);
        const auto size_bytes = static_cast<int32_t>(buf.size_elements * 8);

        auto *cur_ptr =
            BinaryenGlobalGet(mod, buf.name.c_str(), BinaryenTypeInt32());
        auto *future_val = BinaryenGlobalGet(
            mod, (buf.name + "$future").c_str(), BinaryenTypeFloat64());

        buf_updates.push_back(BinaryenStore(mod, 8, base, 8, cur_ptr,
                                            future_val, BinaryenTypeFloat64(),
                                            "memory"));

        buf_updates.push_back(BinaryenGlobalSet(
            mod, buf.name.c_str(),
            BinaryenBinary(
                mod, BinaryenRemUInt32(),
                BinaryenBinary(mod, BinaryenAddInt32(),
                               BinaryenGlobalGet(mod, buf.name.c_str(),
                                                 BinaryenTypeInt32()),
                               BinaryenConst(mod, BinaryenLiteralInt32(8))),
                BinaryenConst(mod, BinaryenLiteralInt32(size_bytes)))));
    }

    if (buf_updates.empty()) return BinaryenNop(mod);
    return BinaryenBlock(mod, nullptr, buf_updates.data(),
                         static_cast<BinaryenIndex>(buf_updates.size()),
                         BinaryenTypeNone());
}

void emit_main_loop(const IRModule &ir, BinaryenModuleRef mod) {
    std::array<BinaryenType, 3> params = {
        BinaryenTypeInt32(),
        BinaryenTypeInt32(),
        BinaryenTypeInt32(),
    };
    std::array<BinaryenType, 3> vars = {
        BinaryenTypeInt32(),
        BinaryenTypeInt32(),
        BinaryenTypeFloat64(),
    };
    const BinaryenIndex BASE_PTR = 0;
    const BinaryenIndex NUM_SAMPLES = 1;
    const BinaryenIndex NUM_CHANNELS = 2;
    const BinaryenIndex CHANNEL = 3;
    const BinaryenIndex SAMPLE = 4;
    const BinaryenIndex OUT_VAL = 5;

    auto *advance_time = BinaryenGlobalSet(
        mod, "TIME",
        BinaryenBinary(mod, BinaryenAddFloat64(),
                       BinaryenGlobalGet(mod, "TIME", BinaryenTypeFloat64()),
                       BinaryenConst(mod, BinaryenLiteralFloat64(1.0))));

    auto *init_channel = BinaryenLocalSet(
        mod, CHANNEL, BinaryenConst(mod, BinaryenLiteralInt32(0)));

    auto *inc_sample = BinaryenLocalSet(
        mod, SAMPLE,
        BinaryenBinary(mod, BinaryenAddInt32(),
                       BinaryenLocalGet(mod, SAMPLE, BinaryenTypeInt32()),
                       BinaryenConst(mod, BinaryenLiteralInt32(1))));

    auto *br_outer = BinaryenBreak(
        mod, "outer_loop",
        BinaryenBinary(mod, BinaryenLtUInt32(),
                       BinaryenLocalGet(mod, SAMPLE, BinaryenTypeInt32()),
                       BinaryenLocalGet(mod, NUM_SAMPLES, BinaryenTypeInt32())),
        nullptr);

    auto *compute_out =
        BinaryenLocalSet(mod, OUT_VAL,
                         BinaryenCall(mod, ir.main_fn.c_str(), nullptr, 0,
                                      BinaryenTypeFloat64()));

    std::array<BinaryenExpressionRef, 7> outer_body = {
        init_channel,
        compute_out,
        make_channel_loop(mod, BASE_PTR, NUM_SAMPLES, NUM_CHANNELS, SAMPLE,
                          CHANNEL, OUT_VAL),
        advance_time,
        make_buffer_updates(mod, ir),
        inc_sample,
        br_outer,
    };
    auto *outer_block = BinaryenBlock(mod, "outer_block", outer_body.data(),
                                      outer_body.size(), BinaryenTypeNone());

    auto *init_sample = BinaryenLocalSet(
        mod, SAMPLE, BinaryenConst(mod, BinaryenLiteralInt32(0)));

    std::array<BinaryenExpressionRef, 2> body_stmts = {
        init_sample,
        BinaryenLoop(mod, "outer_loop", outer_block),
    };
    auto *body = BinaryenBlock(mod, nullptr, body_stmts.data(),
                               body_stmts.size(), BinaryenTypeNone());

    BinaryenAddFunction(mod, "main",
                        BinaryenTypeCreate(params.data(), params.size()),
                        BinaryenTypeNone(), vars.data(), vars.size(), body);
    BinaryenAddFunctionExport(mod, "main", "main");
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
        const std::string ext_name = is_exp ? exported.at(int_name) : int_name;

        const auto num_vars = BinaryenFunctionGetNumVars(fn);
        std::vector<BinaryenType> var_types(num_vars);
        for (BinaryenIndex j = 0; j < num_vars; j++)
            var_types[j] = BinaryenFunctionGetVar(fn, j);

        auto *body =
            BinaryenExpressionCopy(BinaryenFunctionGetBody(fn), main_mod);
        BinaryenAddFunction(
            main_mod, int_name.c_str(), BinaryenFunctionGetParams(fn),
            BinaryenFunctionGetResults(fn), var_types.data(), num_vars, body);

        if (is_exp && ext_name.starts_with("wasmwasm_")) {
            auto *body2 =
                BinaryenExpressionCopy(BinaryenFunctionGetBody(fn), main_mod);
            BinaryenAddFunction(main_mod, ext_name.c_str(),
                                BinaryenFunctionGetParams(fn),
                                BinaryenFunctionGetResults(fn),
                                var_types.data(), num_vars, body2);
        }
    }
}

} // namespace

auto IRModule::buffer_base(const std::string &name) const -> uint32_t {
    auto addr = buffer_memory_start;
    for (const auto &b : buffers) {
        if (b.name == name) return addr;
        addr += static_cast<uint32_t>(b.size_elements * 8);
    }
    throw std::runtime_error("Unknown buffer: " + name);
}

void emit_ir(const IRModule &ir, BinaryenModuleRef mod,
             BinaryenModuleRef math_module, double sample_rate) {
    BinaryenModuleSetFeatures(mod, BinaryenFeatureAll());
    BinaryenAddMemoryImport(mod, "memory", "env", "memory", 0);

    import_math(mod, math_module);

    BinaryenAddGlobal(mod, "TIME", BinaryenTypeFloat64(), true,
                      BinaryenConst(mod, BinaryenLiteralFloat64(0.0)));

    for (const auto &fn : ir.functions) emit_function(fn, mod, sample_rate, ir);

    for (const auto &buf : ir.buffers) {
        BinaryenAddGlobal(mod, buf.name.c_str(), BinaryenTypeInt32(), true,
                          BinaryenConst(mod, BinaryenLiteralInt32(0)));
        BinaryenAddGlobal(mod, (buf.name + "$future").c_str(),
                          BinaryenTypeFloat64(), true,
                          BinaryenConst(mod, BinaryenLiteralFloat64(0.0)));
    }

    emit_init_buffers(ir, mod);
    emit_main_loop(ir, mod);
}
