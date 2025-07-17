#include "main_module_builder.hpp"

#include "binaryen-c.h"
#include "code_gen_context.hpp"
#include "expression_emitter.hpp"
#include "wasm.h"

#include <algorithm>
#include <cmath>
#include <cstdint>
#include <memory>
#include <string>
#include <unordered_map>
#include <utility>

namespace {

auto get_types(
    const std::unordered_map<std::string, BinaryenVariable> &variables)
    -> std::vector<BinaryenType> {
    std::vector<std::pair<std::string, BinaryenVariable>> sorted_variables(
        variables.begin(), variables.end());

    std::ranges::sort(sorted_variables, [](const auto &a, const auto &b) {
        return a.second.local < b.second.local;
    });

    std::vector<BinaryenType> var_types;
    var_types.reserve(sorted_variables.size());
    std::ranges::transform(sorted_variables, std::back_inserter(var_types),
                           [](const auto &p) { return p.second.type; });

    return var_types;
}

} // namespace

MainModuleBuilder::MainModuleBuilder(BinaryenModuleRef math_module,
                                     double sample_rate)
    : ctx(std::make_shared<CodeGenContext>(BinaryenModuleCreate())),
      math_module(math_module) {
    if (ctx->module == nullptr)
        throw std::runtime_error("Module not initialized");

    expression_emitter = std::make_shared<ExpressionEmitter>(ctx);
}

auto MainModuleBuilder::build(const std::vector<ExprPtr> &exprs)
    -> BinaryenModuleRef {
    BinaryenModuleSetFeatures(ctx->module, BinaryenFeatureAll());

    BinaryenAddMemoryImport(ctx->module, "memory", "env", "memory", 0);
    BinaryenAddFunctionImport(ctx->module, "print_f64", "env", "print_f64",
                              BinaryenTypeFloat64(), BinaryenTypeNone());

    auto move_module_items = [&](auto &from, auto add_fn) {
        while (!from.empty()) {
            auto item = std::move(from.back());
            from.pop_back();
            add_fn(std::move(item));
        }
    };

    move_module_items(math_module->globals, [&](auto &&g) {
        ctx->module->addGlobal(std::forward<decltype(g)>(g));
    });
    move_module_items(math_module->functions, [&](auto f) {
        for (const auto &ex : math_module->exports) {
            if (ex->getInternalName()->str == f->name.str) {
                f->setExplicitName(ex->name);
                break;
            }
        }
        ctx->module->addFunction(std::move(f));
    });

    auto make_const_i32 = [&](int32_t val) {
        return BinaryenConst(ctx->module, BinaryenLiteralInt32(val));
    };
    auto make_const_f64 = [&](double val) {
        return BinaryenConst(ctx->module, BinaryenLiteralFloat64(val));
    };

    BinaryenAddGlobal(ctx->module, "TIME", BinaryenTypeFloat64(), true,
                      make_const_f64(0.0));

    ctx->constants.emplace("PI", BinaryenLiteralFloat64(M_PI));

    auto add_param = [&](const std::string &name, BinaryenType type) {
        ctx->parameters.back().emplace(name,
                                       BinaryenVariable{
                                           .local = static_cast<BinaryenIndex>(
                                               ctx->parameters.back().size()),
                                           .type = type,
                                       });
    };
    add_param("BASE_PTR", BinaryenTypeInt32());
    add_param("NUM_SAMPLES", BinaryenTypeInt32());
    add_param("NUM_CHANNELS", BinaryenTypeInt32());

    auto add_var = [&](const std::string &name, BinaryenType type) {
        ctx->variables.back().emplace(name,
                                      BinaryenVariable{
                                          .local = static_cast<BinaryenIndex>(
                                              ctx->variables.back().size() +
                                              ctx->parameters.back().size()),
                                          .type = type,
                                      });
    };
    add_var("CHANNEL", BinaryenTypeInt32());
    add_var("SAMPLE", BinaryenTypeInt32());

    auto *sample_cond = BinaryenBinary(
        ctx->module, BinaryenLtUInt32(),
        ctx->variables.back()["SAMPLE"].get_local(ctx->module),
        ctx->parameters.back()["NUM_SAMPLES"].get_local(ctx->module));
    auto *channel_cond = BinaryenBinary(
        ctx->module, BinaryenLtUInt32(),
        ctx->variables.back()["CHANNEL"].get_local(ctx->module),
        ctx->parameters.back()["NUM_CHANNELS"].get_local(ctx->module));

    auto *index_expr = BinaryenBinary(
        ctx->module, BinaryenAddInt32(),
        ctx->variables.back()["CHANNEL"].get_local(ctx->module),
        BinaryenBinary(
            ctx->module, BinaryenMulInt32(),
            ctx->variables.back()["SAMPLE"].get_local(ctx->module),
            ctx->parameters.back()["NUM_CHANNELS"].get_local(ctx->module)));

    auto *address = BinaryenBinary(
        ctx->module, BinaryenAddInt32(),
        ctx->parameters.back()["BASE_PTR"].get_local(ctx->module),
        BinaryenBinary(ctx->module, BinaryenMulInt32(), make_const_i32(4),
                       index_expr));

    define_binary_operator("+", BinaryenAddFloat64());
    define_binary_operator("*", BinaryenMulFloat64());

    ctx->variables.back().emplace(
        "env_ptr$",
        BinaryenVariable{
            .local = static_cast<BinaryenIndex>(ctx->parameters.back().size() +
                                                ctx->variables.back().size()),
            .type = BinaryenTypeInt32(),
        });
    auto &env_ptr = ctx->variables.back()["env_ptr$"];

    ctx->variables.back().emplace(
        "arg$",
        BinaryenVariable{
            .local = static_cast<BinaryenIndex>(ctx->parameters.back().size() +
                                                ctx->variables.back().size()),
            .type = BinaryenTypeInt32(),
        });
    auto &arg = ctx->variables.back()["arg$"];

    std::vector<BinaryenExpressionRef> binaryen_exprs;
    binaryen_exprs.reserve(exprs.size());
    for (const auto &expr : exprs)
        binaryen_exprs.emplace_back(expression_emitter->create(expr));

    if (!ctx->variables.back().contains("OUT"))
        throw std::runtime_error("Output variable OUT not defined");

    auto *get_out = ctx->variables.back()["OUT"].get_local(ctx->module);

    auto *assign_out = BinaryenStore(
        ctx->module, 4, 0, 4, address,
        BinaryenUnary(ctx->module, BinaryenDemoteFloat64(), get_out),
        BinaryenTypeFloat32(), "memory");

    auto *output_block =
        BinaryenBlock(ctx->module, "output_block", binaryen_exprs.data(),
                      binaryen_exprs.size(), BinaryenTypeNone());

    std::vector<BinaryenExpressionRef> inner_block_children = {
        output_block, assign_out,
        BinaryenCall(
            ctx->module, "print_f64",
            std::array{ctx->variables.back()["OUT"].get_local(ctx->module)}
                .data(),
            1, BinaryenTypeNone()),
        ctx->variables.back()["CHANNEL"].set_local(
            ctx->module,
            BinaryenBinary(
                ctx->module, BinaryenAddInt32(),
                ctx->variables.back()["CHANNEL"].get_local(ctx->module),
                make_const_i32(1))),
        BinaryenBreak(ctx->module, "inner_loop", channel_cond, nullptr)};
    auto *inner_loop = BinaryenLoop(
        ctx->module, "inner_loop",
        BinaryenBlock(ctx->module, "inner_block", inner_block_children.data(),
                      inner_block_children.size(), BinaryenTypeNone()));

    auto *pass_time = BinaryenGlobalSet(
        ctx->module, "TIME",
        BinaryenBinary(
            ctx->module, BinaryenAddFloat64(),
            BinaryenGlobalGet(ctx->module, "TIME", BinaryenTypeFloat64()),
            make_const_f64(1.0)));

    std::vector<BinaryenExpressionRef> outer_block_children = {
        ctx->variables.back()["CHANNEL"].set_local(ctx->module,
                                                   make_const_i32(0)),
        inner_loop, pass_time,
        ctx->variables.back()["SAMPLE"].set_local(
            ctx->module,
            BinaryenBinary(
                ctx->module, BinaryenAddInt32(),
                ctx->variables.back()["SAMPLE"].get_local(ctx->module),
                make_const_i32(1))),
        BinaryenBreak(ctx->module, "outer_loop", sample_cond, nullptr)};
    auto *outer_loop = BinaryenLoop(
        ctx->module, "outer_loop",
        BinaryenBlock(ctx->module, "outer_block", outer_block_children.data(),
                      outer_block_children.size(), BinaryenTypeNone()));

    std::vector<BinaryenExpressionRef> body_block = {
        env_ptr.set_local(
            ctx->module,
            BinaryenConst(ctx->module, BinaryenLiteralInt32(1024))),
        arg.set_local(ctx->module,
                      BinaryenConst(ctx->module, BinaryenLiteralInt32(0))),
        ctx->variables.back()["SAMPLE"].set_local(ctx->module,
                                                  make_const_i32(0)),
        outer_loop};

    auto *body = BinaryenBlock(ctx->module, "body", body_block.data(),
                               body_block.size(), BinaryenTypeNone());

    BinaryenAddFunction(
        ctx->module, "main",
        BinaryenTypeCreate(get_types(ctx->parameters.back()).data(),
                           ctx->parameters.back().size()),
        BinaryenTypeNone(), get_types(ctx->variables.back()).data(),
        ctx->variables.back().size(), body);
    BinaryenAddFunctionExport(ctx->module, "main", "main");

    if (!ctx->fun_indices.empty()) {
        std::vector<std::pair<std::string, BinaryenVariable>> sorted_funs(
            ctx->fun_indices.begin(), ctx->fun_indices.end());

        std::ranges::sort(sorted_funs, [](const auto &a, const auto &b) {
            return a.second.local < b.second.local;
        });

        BinaryenAddTable(ctx->module, "fun_table", sorted_funs.size(),
                         sorted_funs.size(), BinaryenTypeFuncref());
        std::vector<std::string> names;
        std::vector<const char *> elems;
        elems.reserve(sorted_funs.size());
        names.reserve(sorted_funs.size());
        for (const auto &[name, pair] : sorted_funs) {
            names.emplace_back(name);
            elems.emplace_back(names.back().c_str());
        }

        BinaryenAddActiveElementSegment(
            ctx->module, "fun_table", "fun_table_seg", elems.data(),
            elems.size(), BinaryenConst(ctx->module, BinaryenLiteralInt32(0)));
    }

    return ctx->module;
}

void MainModuleBuilder::define_binary_operator(const std::string &symbol,
                                               BinaryenOp op) {
    ctx->fun_indices.emplace(
        symbol + "$inner",
        BinaryenVariable{
            .local = static_cast<BinaryenIndex>(ctx->fun_indices.size()),
            .type = BinaryenTypeInt32(),
        });
    auto &inner_fun_var = ctx->fun_indices[symbol + "$inner"];

    ctx->fun_indices.emplace(symbol, BinaryenVariable{
                                         .local = static_cast<BinaryenIndex>(
                                             ctx->fun_indices.size()),
                                         .type = BinaryenTypeInt32(),
                                     });

    ctx->parameters.emplace_back();

    ctx->parameters.back().emplace(
        "env_ptr$", BinaryenVariable{.local = 0, .type = BinaryenTypeInt32()});
    auto &env_ptr = ctx->parameters.back()["env_ptr$"];

    ctx->parameters.back().emplace(
        "arg$", BinaryenVariable{.local = 1, .type = BinaryenTypeFloat64()});
    auto &arg = ctx->parameters.back()["arg$"];

    auto outer_fun_body = std::array{
        BinaryenStore(ctx->module, 8, 0, 8, env_ptr.get_local(ctx->module),
                      arg.get_local(ctx->module), BinaryenTypeFloat64(),
                      "memory"),
        BinaryenStore(ctx->module, 4, 8, 4, env_ptr.get_local(ctx->module),
                      BinaryenConst(ctx->module,
                                    BinaryenLiteralInt32(inner_fun_var.local)),
                      BinaryenTypeInt32(), "memory"),
        BinaryenStore(ctx->module, 4, 12, 4, env_ptr.get_local(ctx->module),
                      env_ptr.get_local(ctx->module), BinaryenTypeInt32(),
                      "memory"),
        BinaryenLoad(ctx->module, 4, false, 8, 4, BinaryenTypeInt32(),
                     BinaryenLoad(ctx->module, 4, false, 12, 4,
                                  BinaryenTypeInt32(),
                                  env_ptr.get_local(ctx->module), "memory"),
                     "memory"),
    };

    ctx->parameters.emplace_back();
    ctx->parameters.back().emplace(
        "env_ptr$", BinaryenVariable{.local = 0, .type = BinaryenTypeInt32()});
    ctx->parameters.back().emplace(
        "arg$", BinaryenVariable{.local = 1, .type = BinaryenTypeFloat64()});

    env_ptr = ctx->parameters.back()["env_ptr$"];
    arg = ctx->parameters.back()["arg$"];

    BinaryenAddFunction(
        ctx->module, (symbol + "$inner").c_str(),
        BinaryenTypeCreate(
            std::array{BinaryenTypeInt32(), BinaryenTypeFloat64()}.data(), 2),
        BinaryenTypeFloat64(), nullptr, 0,
        BinaryenBinary(ctx->module, op, arg.get_local(ctx->module),
                       BinaryenLoad(ctx->module, 8, false, 0, 8,
                                    BinaryenTypeFloat64(),
                                    env_ptr.get_local(ctx->module), "memory")));

    ctx->parameters.pop_back();

    BinaryenAddFunction(
        ctx->module, symbol.c_str(),
        BinaryenTypeCreate(
            std::array{BinaryenTypeInt32(), BinaryenTypeFloat64()}.data(), 2),
        BinaryenTypeInt32(), nullptr, 0,
        BinaryenBlock(ctx->module, nullptr, outer_fun_body.data(),
                      outer_fun_body.size(), BinaryenTypeInt32()));

    ctx->parameters.pop_back();
}