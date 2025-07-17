#include "expression_emitter.hpp"

#include "binaryen-c.h"
#include "code_gen_context.hpp"

#include <algorithm>
#include <array>
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
                std::vector<BinaryenExpressionRef> result;
                result.reserve(2);

                auto env =
                    (ctx->variables.back().contains("env$") ? ctx->variables
                                                            : ctx->parameters)
                        .back()
                        .at("env$");

                auto closure_name =
                    "closure$" + std::to_string(ctx->parameters.back().size() +
                                                ctx->variables.back().size());
                ctx->variables.back().emplace(
                    closure_name, BinaryenVariable{
                                      .local = static_cast<BinaryenIndex>(
                                          ctx->parameters.back().size() +
                                          ctx->variables.back().size()),
                                      .type = BinaryenTypeInt32(),
                                  });
                auto closure = ctx->variables.back().at(closure_name);

                auto callee_expr = create(node.callee);
                auto arg_expr = create(node.argument);

                result.emplace_back(
                    closure.set_local(ctx->module, callee_expr));

                result.emplace_back(BinaryenCallIndirect(
                    ctx->module, "fun_table",
                    BinaryenLoad(ctx->module, 4, false, 4, 4,
                                 BinaryenTypeInt32(),
                                 closure.get_local(ctx->module), "memory"),
                    std::array{closure.get_local(ctx->module), arg_expr}.data(),
                    2,
                    BinaryenTypeCreate(
                        std::array{BinaryenTypeInt32(),
                                   node.argument->type->to_binaryen_type()}
                            .data(),
                        2),
                    expr->type->to_binaryen_type()));

                return BinaryenBlock(ctx->module, nullptr, result.data(),
                                     result.size(),
                                     expr->type->to_binaryen_type());
            }

            if constexpr (std::is_same_v<T, Expr::Lambda>) {
                ctx->parameters.emplace_back();
                ctx->variables.emplace_back();
                ctx->offsets.emplace_back(8);
                auto previous_offset = ctx->offsets.back();

                auto fun_name =
                    "lambda$" + std::to_string(ctx->fun_indices.size());

                ctx->fun_indices.emplace(
                    fun_name, BinaryenVariable{
                                  .local = static_cast<BinaryenIndex>(
                                      ctx->fun_indices.size()),
                                  .type = BinaryenTypeInt32(),
                              });
                auto &fun_var = ctx->fun_indices.at(fun_name);

                auto param_type = std::get<TypeFun>(expr->type->node)
                                      .param->to_binaryen_type();
                ctx->parameters.back().emplace(
                    node.parameter.lexeme, BinaryenVariable{
                                               .local = 1,
                                               .type = param_type,
                                               .offset = ctx->offsets.back(),
                                           });
                auto &param = ctx->parameters.back().at(node.parameter.lexeme);
                auto is_eight_bit = param_type == BinaryenTypeFloat64() ? 8 : 4;
                ctx->offsets.back() += is_eight_bit;

                ctx->parameters.back().emplace(
                    "env$", BinaryenVariable{
                                .local = 0,
                                .type = BinaryenTypeInt32(),
                                .offset = ctx->offsets.back(),
                            });
                auto &env = ctx->parameters.back().at("env$");
                ctx->offsets.back() += 4;

                fun_var.offset = ctx->offsets.back();
                ctx->offsets.back() += 4;

                std::vector<BinaryenExpressionRef> body_result;

                if (std::holds_alternative<TypeBase>(
                        std::get<TypeFun>(expr->type->node).result->node)) {
                    body_result.emplace_back(create(node.body));
                } else {
                    body_result.emplace_back(BinaryenStore(
                        ctx->module, is_eight_bit, param.offset, is_eight_bit,
                        env.get_local(ctx->module),
                        param.get_local(ctx->module), param_type, "memory"));

                    body_result.emplace_back(BinaryenStore(
                        ctx->module, 4, env.offset, 4,
                        env.get_local(ctx->module), env.get_local(ctx->module),
                        BinaryenTypeInt32(), "memory"));

                    body_result.emplace_back(
                        BinaryenStore(ctx->module, 4, fun_var.offset, 4,
                                      env.get_local(ctx->module),
                                      BinaryenLoad(ctx->module, 4, false, 4, 4,
                                                   BinaryenTypeInt32(),
                                                   create(node.body), "memory"),
                                      BinaryenTypeInt32(), "memory"));

                    body_result.emplace_back(BinaryenBinary(
                        ctx->module, BinaryenAddInt32(),
                        env.get_local(ctx->module),
                        BinaryenConst(ctx->module,
                                      BinaryenLiteralInt32(env.offset))));
                }

                BinaryenAddFunction(
                    ctx->module, fun_name.c_str(),
                    BinaryenTypeCreate(
                        std::array{BinaryenTypeInt32(), param_type}.data(), 2),
                    node.body->type->to_binaryen_type(),
                    get_types(ctx->variables.back()).data(),
                    ctx->variables.back().size(),
                    BinaryenBlock(ctx->module, nullptr, body_result.data(),
                                  body_result.size(),
                                  node.body->type->to_binaryen_type()));

                auto offset_diff = ctx->offsets.back() - previous_offset;
                ctx->variables.pop_back();
                ctx->parameters.pop_back();
                ctx->offsets.pop_back();

                env = (ctx->parameters.back().contains("env$") ? ctx->parameters
                                                               : ctx->variables)
                          .back()
                          .at("env$");

                std::vector<BinaryenExpressionRef> result;

                result.emplace_back(BinaryenStore(
                    ctx->module, 4, ctx->offsets.back(), 4,
                    env.get_local(ctx->module),
                    BinaryenBinary(
                        ctx->module, BinaryenAddInt32(),
                        env.get_local(ctx->module),
                        BinaryenConst(ctx->module,
                                      BinaryenLiteralInt32(env.offset))),
                    BinaryenTypeInt32(), "memory"));
                ctx->offsets.back() += 4;

                result.emplace_back(BinaryenStore(
                    ctx->module, 4, ctx->offsets.back(), 4,
                    env.get_local(ctx->module),
                    BinaryenConst(ctx->module,
                                  BinaryenLiteralInt32(fun_var.local)),
                    BinaryenTypeInt32(), "memory"));
                ctx->offsets.back() += 4;

                result.emplace_back(BinaryenBinary(
                    ctx->module, BinaryenAddInt32(), env.get_local(ctx->module),
                    BinaryenConst(ctx->module, BinaryenLiteralInt32(
                                                   ctx->offsets.back() - 8))));

                ctx->offsets.back() += offset_diff;

                return BinaryenBlock(ctx->module, nullptr, result.data(),
                                     result.size(), BinaryenTypeInt32());
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
                    it != ctx->fun_indices.end()) {
                    std::vector<BinaryenExpressionRef> result;
                    auto &env = (ctx->variables.back().contains("env$")
                                     ? ctx->variables
                                     : ctx->parameters)
                                    .back()
                                    .at("env$");

                    BinaryenIndex env_offset = ctx->offsets.back();
                    result.emplace_back(BinaryenStore(
                        ctx->module, 4, ctx->offsets.back(), 4,
                        env.get_local(ctx->module), env.get_local(ctx->module),
                        BinaryenTypeInt32(), "memory"));
                    ctx->offsets.back() += 4;

                    result.emplace_back(BinaryenStore(
                        ctx->module, 4, ctx->offsets.back(), 4,
                        env.get_local(ctx->module),
                        BinaryenConst(ctx->module,
                                      BinaryenLiteralInt32(it->second.local)),
                        BinaryenTypeInt32(), "memory"));
                    ctx->offsets.back() += 4;

                    result.emplace_back(BinaryenBinary(
                        ctx->module, BinaryenAddInt32(),
                        env.get_local(ctx->module),
                        BinaryenConst(ctx->module,
                                      BinaryenLiteralInt32(env_offset))));

                    return BinaryenBlock(ctx->module, nullptr, result.data(),
                                         result.size(), BinaryenTypeInt32());
                }

                auto it = ctx->variables.back().find(node.name.lexeme);
                if (it != ctx->variables.back().end())
                    return ctx->variables.back()[node.name.lexeme].get_local(
                        ctx->module);

                it = ctx->parameters.back().find(node.name.lexeme);
                if (it != ctx->parameters.back().end())
                    return ctx->parameters.back()[node.name.lexeme].get_local(
                        ctx->module);

                int depth = 1;
                it = ctx->parameters.front().end();
                for (int i = ctx->variables.size() - 2; i >= 0; --i) {
                    it = ctx->variables[i].find(node.name.lexeme);
                    if (it != ctx->variables[i].end()) break;
                    it = ctx->parameters[i].find(node.name.lexeme);
                    if (it != ctx->parameters[i].end()) break;
                    depth++;
                }
                if (it == ctx->parameters.front().end() ||
                    it->second.offset == static_cast<BinaryenIndex>(-1))
                    throw std::runtime_error("Unknown variable: " +
                                             node.name.lexeme);

                auto *acc =
                    BinaryenLocalGet(ctx->module, 0, BinaryenTypeInt32());

                for (int i = 0; i < depth; ++i)
                    acc = BinaryenLoad(ctx->module, 4, false, 0, 4,
                                       BinaryenTypeInt32(), acc, "memory");
                auto is_eight_bit =
                    it->second.type == BinaryenTypeFloat64() ? 8 : 4;
                return BinaryenLoad(ctx->module, is_eight_bit, false,
                                    it->second.offset, is_eight_bit,
                                    it->second.type, acc, "memory");
            }

            throw std::runtime_error("Unknown expression type in create");
        },
        expr->node);
}