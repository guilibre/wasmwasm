#include "emit.hpp"

#include "../ast/ast.hpp"
#include "binaryen-c.h"
#include "ir.hpp"
#include <algorithm>
#include <array>
#include <cstdint>
#include <stdexcept>
#include <string>
#include <type_traits>
#include <unordered_map>
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
    return BinaryenTypeNone();
}

auto to_bop(Operation op) -> BinaryenOp {
    switch (op) {
    case Add:
        return BinaryenAddFloat64();
    case Sub:
        return BinaryenSubFloat64();
    case Mul:
        return BinaryenMulFloat64();
    case Div:
        return BinaryenDivFloat64();
    }
    return BinaryenAddFloat64();
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
        auto i = idx.at(name);
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
            [&](const auto &i) {
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
            [&](const auto &i) {
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
                    for (const auto &a : i.args)
                        args.push_back(ctx.get(a));
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
                    auto base = ctx.ir->buffer_base(i.buffer);
                    auto *ptr = BinaryenGlobalGet(ctx.mod, i.buffer.c_str(),
                                                  BinaryenTypeInt32());
                    stmts.push_back(
                        ctx.set(i.result, BinaryenLoad(ctx.mod, 8, false, base,
                                                       8, BinaryenTypeFloat64(),
                                                       ptr, "memory")));
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
    for (const auto &p : fn.params)
        ctx.add_param(p.name, p.type);
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
        auto base = ir.buffer_base(buf.name);
        auto n = static_cast<int32_t>(buf.size_elements);

        auto idx_local = [&]() -> BinaryenExpressionRef {
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

        auto loop_label = buf.name + "$init_loop";
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

void emit_main_loop(const IRModule &ir, BinaryenModuleRef mod,
                    double sample_rate) {
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

    auto *compute_out =
        BinaryenLocalSet(mod, OUT_VAL,
                         BinaryenCall(mod, ir.main_fn.c_str(), nullptr, 0,
                                      BinaryenTypeFloat64()));

    auto *out_addr = BinaryenBinary(
        mod, BinaryenAddInt32(),
        BinaryenLocalGet(mod, BASE_PTR, BinaryenTypeInt32()),
        BinaryenBinary(
            mod, BinaryenMulInt32(),
            BinaryenConst(mod, BinaryenLiteralInt32(4)),
            BinaryenBinary(
                mod, BinaryenAddInt32(),
                BinaryenLocalGet(mod, SAMPLE, BinaryenTypeInt32()),
                BinaryenBinary(
                    mod, BinaryenMulInt32(),
                    BinaryenLocalGet(mod, CHANNEL, BinaryenTypeInt32()),
                    BinaryenLocalGet(mod, NUM_SAMPLES, BinaryenTypeInt32())))));

    auto *store_out = BinaryenStore(
        mod, 4, 0, 4, out_addr,
        BinaryenUnary(mod, BinaryenDemoteFloat64(),
                      BinaryenLocalGet(mod, OUT_VAL, BinaryenTypeFloat64())),
        BinaryenTypeFloat32(), "memory");

    auto *inc_channel = BinaryenLocalSet(
        mod, CHANNEL,
        BinaryenBinary(mod, BinaryenAddInt32(),
                       BinaryenLocalGet(mod, CHANNEL, BinaryenTypeInt32()),
                       BinaryenConst(mod, BinaryenLiteralInt32(1))));

    auto *br_inner = BinaryenBreak(
        mod, "inner_loop",
        BinaryenBinary(
            mod, BinaryenLtUInt32(),
            BinaryenLocalGet(mod, CHANNEL, BinaryenTypeInt32()),
            BinaryenLocalGet(mod, NUM_CHANNELS, BinaryenTypeInt32())),
        nullptr);

    std::array<BinaryenExpressionRef, 3> inner_body = {store_out, inc_channel,
                                                       br_inner};
    auto *inner_block = BinaryenBlock(mod, "inner_block", inner_body.data(),
                                      inner_body.size(), BinaryenTypeNone());

    std::vector<BinaryenExpressionRef> buf_updates;
    for (const auto &buf : ir.buffers) {
        auto base = ir.buffer_base(buf.name);
        auto size_bytes = static_cast<int32_t>(buf.size_elements * 8);

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
    auto *buf_block =
        buf_updates.empty()
            ? BinaryenNop(mod)
            : BinaryenBlock(mod, nullptr, buf_updates.data(),
                            static_cast<BinaryenIndex>(buf_updates.size()),
                            BinaryenTypeNone());

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

    std::array<BinaryenExpressionRef, 7> outer_body = {
        init_channel, compute_out, BinaryenLoop(mod, "inner_loop", inner_block),
        advance_time, buf_block,   inc_sample,
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
    auto num_exports = BinaryenGetNumExports(math_mod);
    for (BinaryenIndex i = 0; i < num_exports; i++) {
        auto *ex = BinaryenGetExportByIndex(math_mod, i);
        if (BinaryenExportGetKind(ex) != BinaryenExternalFunction()) continue;
        exported[BinaryenExportGetValue(ex)] = BinaryenExportGetName(ex);
    }

    auto num_globals = BinaryenGetNumGlobals(math_mod);
    for (BinaryenIndex i = 0; i < num_globals; i++) {
        auto *g = BinaryenGetGlobalByIndex(math_mod, i);
        auto *init =
            BinaryenExpressionCopy(BinaryenGlobalGetInitExpr(g), main_mod);
        BinaryenAddGlobal(main_mod, BinaryenGlobalGetName(g),
                          BinaryenGlobalGetType(g), BinaryenGlobalIsMutable(g),
                          init);
    }

    auto is_numeric = [](const std::string &s) -> bool {
        return !s.empty() &&
               std::ranges::all_of(
                   s, [](unsigned char c) -> bool { return std::isdigit(c); });
    };

    auto num_fns = BinaryenGetNumFunctions(math_mod);
    for (BinaryenIndex i = 0; i < num_fns; i++) {
        auto *fn = BinaryenGetFunctionByIndex(math_mod, i);
        std::string int_name = BinaryenFunctionGetName(fn);
        bool is_exp = exported.contains(int_name);
        if (!is_exp && !is_numeric(int_name)) continue;
        std::string ext_name = is_exp ? exported.at(int_name) : int_name;
        const char *out_name = ext_name.c_str();

        auto num_vars = BinaryenFunctionGetNumVars(fn);
        std::vector<BinaryenType> var_types(num_vars);
        for (BinaryenIndex j = 0; j < num_vars; j++)
            var_types[j] = BinaryenFunctionGetVar(fn, j);

        auto *body =
            BinaryenExpressionCopy(BinaryenFunctionGetBody(fn), main_mod);

        BinaryenAddFunction(main_mod, out_name, BinaryenFunctionGetParams(fn),
                            BinaryenFunctionGetResults(fn), var_types.data(),
                            num_vars, body);
    }
}

} // namespace

auto IRModule::buffer_base(const std::string &name) const -> uint32_t {
    uint32_t addr = 4096;
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

    for (const auto &buf : ir.buffers) {
        BinaryenAddGlobal(mod, buf.name.c_str(), BinaryenTypeInt32(), true,
                          BinaryenConst(mod, BinaryenLiteralInt32(0)));
        BinaryenAddGlobal(mod, (buf.name + "$future").c_str(),
                          BinaryenTypeFloat64(), true,
                          BinaryenConst(mod, BinaryenLiteralFloat64(0.0)));
    }

    for (const auto &fn : ir.functions)
        emit_function(fn, mod, sample_rate, ir);

    emit_init_buffers(ir, mod);
    emit_main_loop(ir, mod, sample_rate);
}
