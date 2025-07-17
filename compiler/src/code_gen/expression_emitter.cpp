#include "expression_emitter.hpp"

#include "binaryen-c.h"
#include "code_gen_context.hpp"
#include "free_var_analyzer.hpp"

#include <algorithm>
#include <array>
#include <iostream>
#include <memory>
#include <stdexcept>
#include <string>
#include <utility>
#include <variant>

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

ExpressionEmitter::ExpressionEmitter(std::shared_ptr<CodeGenContext> ctx)
    : ctx(std::move(ctx)) {}

auto ExpressionEmitter::create(const ExprPtr &expr) -> BinaryenExpressionRef {
    return std::visit(
        [&](const auto &node) -> BinaryenExpressionRef {
            using T = std::decay_t<decltype(node)>;
            if constexpr (std::is_same_v<T, Expr::Assignment>) {
                ctx->variables.back().emplace(
                    node.name.lexeme,
                    BinaryenVariable{
                        .local = static_cast<BinaryenIndex>(
                            ctx->variables.back().size() +
                            ctx->parameters.back().size()),
                        .type = node.value->type->to_binaryen_type(),
                    });

                return ctx->variables.back()[node.name.lexeme].set_local(
                    ctx->module, create(node.value));
            }

            if constexpr (std::is_same_v<T, Expr::Block>) {
                std::vector<BinaryenExpressionRef> children;
                children.reserve(node.expressions.size());
                for (const auto &e : node.expressions)
                    children.emplace_back(create(e));

                return BinaryenBlock(
                    ctx->module, nullptr, children.data(), children.size(),
                    node.expressions.back()->type->to_binaryen_type());
            }

            if constexpr (std::is_same_v<T, Expr::Call>) {
                auto &env_ptr = ctx->parameters.back().contains("env_ptr$")
                                    ? ctx->parameters.back()["env_ptr$"]
                                    : ctx->variables.back()["env_ptr$"];
                auto &arg = ctx->parameters.back().contains("arg$")
                                ? ctx->parameters.back()["arg$"]
                                : ctx->variables.back()["arg$"];

                auto callee_expr = create(node.callee);
                auto arg_expr = create(node.argument);

                return BinaryenCallIndirect(
                    ctx->module, "fun_table", callee_expr,
                    std::array{
                        BinaryenBinary(ctx->module, BinaryenAddInt32(),
                                       env_ptr.get_local(ctx->module),
                                       BinaryenConst(ctx->module,
                                                     BinaryenLiteralInt32(8))),
                        arg_expr}
                        .data(),
                    2,
                    BinaryenTypeCreate(
                        std::array{BinaryenTypeInt32(), BinaryenTypeFloat64()}
                            .data(),
                        2),
                    std::get<TypeFun>(node.callee->type->node)
                        .result->to_binaryen_type());
            }

            if constexpr (std::is_same_v<T, Expr::Lambda>) {
                auto fun_name =
                    "lambda$" + std::to_string(ctx->fun_indices.size());
                ctx->fun_indices.emplace(
                    fun_name, BinaryenVariable{
                                  .local = static_cast<BinaryenIndex>(
                                      ctx->fun_indices.size()),
                                  .type = expr->type->to_binaryen_type(),
                              });
                auto &fun_var = ctx->fun_indices[fun_name];

                ctx->variables.emplace_back();
                ctx->parameters.emplace_back();

                ctx->parameters.back().emplace("env_ptr$",
                                               BinaryenVariable{
                                                   .local = 0,
                                                   .type = BinaryenTypeInt32(),
                                               });
                auto &env_ptr = ctx->parameters.back()["env_ptr$"];

                ctx->parameters.back().emplace(
                    node.parameter.lexeme,
                    BinaryenVariable{
                        .local = 1,
                        .type = std::get<TypeFun>(expr->type->node)
                                    .param->to_binaryen_type(),
                    });
                ctx->parameters.back().emplace(
                    "arg$", BinaryenVariable{
                                .local = 1,
                                .type = std::get<TypeFun>(expr->type->node)
                                            .param->to_binaryen_type(),
                            });
                auto &arg = ctx->parameters.back()[node.parameter.lexeme];

                std::vector<BinaryenExpressionRef> result;
                result.reserve(4);
                auto is_eight_bit = arg.type == BinaryenTypeFloat64() ? 8 : 4;

                BinaryenIndex offset = 0;
                result.emplace_back(BinaryenStore(
                    ctx->module, is_eight_bit, offset, is_eight_bit,
                    env_ptr.get_local(ctx->module), arg.get_local(ctx->module),
                    arg.type, "memory"));
                offset += is_eight_bit;

                auto free_vars = FreeVarAnalyzer::analyze(expr, {}, ctx);

                for (const auto &var : free_vars) {
                    auto it = ctx->variables.back().find(var);
                    if (it == ctx->variables.back().end())
                        it = ctx->parameters.back().find(var);
                    if (it != ctx->parameters.back().end()) {
                        is_eight_bit =
                            it->second.type == BinaryenTypeFloat64() ? 8 : 4;
                        result.emplace_back(BinaryenStore(
                            ctx->module, 4, offset, 4,
                            it->second.get_local(ctx->module),
                            create(node.body), BinaryenTypeInt32(), "memory"));
                        offset += is_eight_bit;
                    } else {
                        for (int i = ctx->variables.size() - 2; i >= 0; --i) {
                            it = ctx->variables[i].find(var);
                            if (it != ctx->variables[i].end()) break;
                            it = ctx->parameters[i].find(var);
                            if (it != ctx->parameters[i].end()) break;
                        }
                        if (it == ctx->parameters.front().end())
                            throw std::runtime_error("Unbound variable: " +
                                                     var);
                        is_eight_bit =
                            it->second.type == BinaryenTypeFloat64() ? 8 : 4;
                        result.emplace_back(BinaryenStore(
                            ctx->module, 4, offset, 4,
                            BinaryenLoad(ctx->module, is_eight_bit, false,
                                         offset, is_eight_bit, it->second.type,
                                         env_ptr.get_local(ctx->module),
                                         "memory"),
                            create(node.body), BinaryenTypeInt32(), "memory"));
                        offset += is_eight_bit;
                    }

                    std::cout << var << ' ';
                }
                std::cout << '\n';
                ASTPrinter ast_printer;
                ast_printer(expr);
                std::cout << '\n';

                BinaryenExpressionRef body = nullptr;
                if (std::holds_alternative<TypeFun>(
                        std::get<TypeFun>(expr->type->node).result->node)) {
                    result.emplace_back(BinaryenStore(
                        ctx->module, 4, offset, 4,
                        env_ptr.get_local(ctx->module), create(node.body),
                        BinaryenTypeInt32(), "memory"));
                    offset += 4;

                    result.emplace_back(
                        BinaryenStore(ctx->module, 4, offset, 4,
                                      env_ptr.get_local(ctx->module),
                                      env_ptr.get_local(ctx->module),
                                      BinaryenTypeInt32(), "memory"));
                    offset += 4;

                    auto result_type =
                        std::get<TypeFun>(expr->type->node).result;
                    auto is_eight_bit =
                        std::holds_alternative<TypeBase>(result_type->node) &&
                                std::get<TypeBase>(result_type->node).kind ==
                                    BaseTypeKind::Float
                            ? 8
                            : 4;

                    result.emplace_back(BinaryenLoad(
                        ctx->module, is_eight_bit, false, offset - 8,
                        is_eight_bit, result_type->to_binaryen_type(),
                        BinaryenLoad(ctx->module, 4, false, offset - 4, 4,
                                     BinaryenTypeInt32(),
                                     env_ptr.get_local(ctx->module), "memory"),
                        "memory"));
                    body = BinaryenBlock(ctx->module, nullptr, result.data(),
                                         result.size(),
                                         std::get<TypeFun>(expr->type->node)
                                             .result->to_binaryen_type());
                } else {
                    body = create(node.body);
                }

                ctx->parameters.back().erase(node.parameter.lexeme);

                auto param_types = get_types(ctx->parameters.back());

                BinaryenAddFunction(
                    ctx->module, fun_name.c_str(),
                    BinaryenTypeCreate(param_types.data(), param_types.size()),
                    std::get<TypeFun>(expr->type->node)
                        .result->to_binaryen_type(),
                    get_types(ctx->variables.back()).data(),
                    ctx->variables.back().size(), body);

                ctx->variables.pop_back();
                ctx->parameters.pop_back();
                return BinaryenConst(ctx->module,
                                     BinaryenLiteralInt32(fun_var.local));
            }

            if constexpr (std::is_same_v<T, Expr::Literal>) {
                double value = std::stod(std::string(node.value.lexeme));
                return BinaryenConst(ctx->module,
                                     BinaryenLiteralFloat64(value));
            }

            if constexpr (std::is_same_v<T, Expr::Variable>) {
                if (auto it = ctx->constants.find(node.name.lexeme);
                    it != ctx->constants.end())
                    return BinaryenConst(ctx->module, it->second);

                if (auto it = ctx->fun_indices.find(node.name.lexeme);
                    it != ctx->fun_indices.end())
                    return BinaryenConst(
                        ctx->module, BinaryenLiteralInt32(it->second.local));

                auto var_it = ctx->variables.back().find(node.name.lexeme);
                if (var_it != ctx->variables.back().end())
                    return ctx->variables.back()[node.name.lexeme].get_local(
                        ctx->module);

                auto param_it = ctx->parameters.back().find(node.name.lexeme);
                if (param_it != ctx->parameters.back().end())
                    return ctx->parameters.back()[node.name.lexeme].get_local(
                        ctx->module);

                if (ctx->parameters.size() > 1) {
                    param_it = ctx->parameters[ctx->parameters.size() - 2].find(
                        node.name.lexeme);
                    if (param_it != ctx->parameters.back().end()) {
                        auto is_eight_bit =
                            param_it->second.type == BinaryenTypeFloat64();
                        return BinaryenLoad(
                            ctx->module, is_eight_bit ? 8 : 4, false, 0,
                            is_eight_bit ? 8 : 4, param_it->second.type,
                            BinaryenLocalGet(ctx->module, 0,
                                             BinaryenTypeInt32()),
                            "memory");
                    }
                }

                throw std::runtime_error("Unknown variable: " +
                                         node.name.lexeme);
            }

            throw std::runtime_error("Unknown expression type in create");
        },
        expr->node);
}