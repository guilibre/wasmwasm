#include "expression_emitter.hpp"

#include "binaryen-c.h"
#include "code_gen_context.hpp"

#include <array>
#include <memory>
#include <stdexcept>
#include <string>
#include <type_traits>
#include <utility>
#include <variant>
#include <vector>

ExpressionEmitter::ExpressionEmitter(std::shared_ptr<CodeGenContext> ctx,
                                     double sample_rate)
    : ctx(std::move(ctx)), sample_rate(sample_rate) {}

auto ExpressionEmitter::create(const ExprPtr &expr) -> BinaryenExpressionRef {
    return std::visit(
        [&](const auto &node) -> BinaryenExpressionRef {
            using T = std::decay_t<decltype(node)>;
            if constexpr (std::is_same_v<T, Assignment>) {
                auto value_expr = create(node.value);

                if (ctx->has_buffer(node.name.lexeme))
                    return BinaryenGlobalSet(
                        ctx->module(), (node.name.lexeme + "$future").c_str(),
                        value_expr);

                if (ctx->has_variable_or_parameter(node.name.lexeme)) {
                    auto [var, depth] =
                        ctx->variable_or_parameter(node.name.lexeme);
                    if (depth == 0)
                        return var.set_local(ctx->module(), value_expr);

                    auto *acc = BinaryenLocalGet(
                        ctx->module(), ctx->env().local, BinaryenTypeInt32());

                    for (int i = 0; i < depth; ++i)
                        acc = BinaryenLoad(ctx->module(), 4, false, 0, 4,
                                           BinaryenTypeInt32(), acc, "memory");

                    auto var_size = var.type == BinaryenTypeFloat64() ? 8 : 4;
                    return BinaryenStore(ctx->module(), var_size, var.offset,
                                         var_size, acc, value_expr, var.type,
                                         "memory");
                }

                auto &var = ctx->add_variable(
                    node.name.lexeme, node.value->type->to_binaryen_type());
                return var.set_local(ctx->module(), value_expr);
            }

            if constexpr (std::is_same_v<T, BinaryOp>) {
                BinaryenOp binaryen_op = 0;
                switch (node.op) {
                case Operation::Add:
                    binaryen_op = BinaryenAddFloat64();
                    break;
                case Operation::Sub:
                    binaryen_op = BinaryenSubFloat64();
                    break;
                case Operation::Mul:
                    binaryen_op = BinaryenMulFloat64();
                    break;
                case Operation::Div:
                    binaryen_op = BinaryenDivFloat64();
                    break;
                default:
                    throw std::runtime_error("Unsuported operator");
                }

                return BinaryenBinary(ctx->module(), binaryen_op,
                                      create(node.left), create(node.right));
            }

            if constexpr (std::is_same_v<T, Block>) {
                if (node.expressions.size() == 0)
                    return BinaryenNop(ctx->module());
                std::vector<BinaryenExpressionRef> children;
                children.reserve(node.expressions.size());
                for (const auto &e : node.expressions)
                    children.emplace_back(create(e));

                return BinaryenBlock(
                    ctx->module(), nullptr, children.data(), children.size(),
                    node.expressions.back()->type->to_binaryen_type());
            }

            if constexpr (std::is_same_v<T, Buffer>) {
                ctx->push_context();
                auto &lambda =
                    std::get<Lambda>(node.init_buffer_function->node);
                ctx->add_parameter(lambda.parameter.lexeme,
                                   BinaryenTypeFloat64());
                auto &env = ctx->add_variable("env$", BinaryenTypeInt32());
                ctx->add_buffer(node.name, node.size, create(lambda.body));
                ctx->pop_context();

                auto &idx = ctx->add_variable("idx$", BinaryenTypeInt32());

                auto loop_body = std::array{
                    BinaryenStore(
                        ctx->module(), 8, ctx->buffers.at(node.name), 8,
                        idx.get_local(ctx->module()),
                        BinaryenCall(
                            ctx->module(), (node.name + "$init").c_str(),
                            std::array{
                                BinaryenBinary(
                                    ctx->module(), BinaryenDivFloat64(),
                                    BinaryenUnary(
                                        ctx->module(),
                                        BinaryenConvertSInt32ToFloat64(),
                                        idx.get_local(ctx->module())),
                                    BinaryenConst(ctx->module(),
                                                  BinaryenLiteralFloat64(8.0))),
                            }
                                .data(),
                            1, BinaryenTypeFloat64()),
                        BinaryenTypeFloat64(), "memory"),

                    idx.set_local(
                        ctx->module(),
                        BinaryenBinary(ctx->module(), BinaryenAddInt32(),
                                       idx.get_local(ctx->module()),
                                       BinaryenConst(ctx->module(),
                                                     BinaryenLiteralInt32(8)))),

                    BinaryenBreak(
                        ctx->module(), (node.name + "$loop").c_str(),
                        BinaryenBinary(
                            ctx->module(), BinaryenGeSInt32(),
                            BinaryenConst(ctx->module(),
                                          BinaryenLiteralInt32(node.size)),
                            idx.get_local(ctx->module())),
                        nullptr),
                };

                auto main_body = std::array{
                    idx.set_local(
                        ctx->module(),
                        BinaryenConst(ctx->module(), BinaryenLiteralInt32(0))),

                    BinaryenLoop(
                        ctx->module(), (node.name + "$loop").c_str(),
                        BinaryenBlock(ctx->module(), nullptr, loop_body.data(),
                                      loop_body.size(), BinaryenTypeNone())),
                };

                return BinaryenBlock(ctx->module(), nullptr, main_body.data(),
                                     main_body.size(), BinaryenTypeNone());
            }

            if constexpr (std::is_same_v<T, Call>) {
                auto &temp = ctx->add_variable(BinaryenTypeInt32());

                auto arg_expr = create(node.argument);
                auto callee_expr = create(node.callee);

                std::vector<BinaryenExpressionRef> result;

                for (const auto &var : ctx->variables.back()) {
                    auto var_size =
                        var.second.type == BinaryenTypeFloat64() ? 8 : 4;
                    result.emplace_back(BinaryenStore(
                        ctx->module(), var_size, var.second.offset, var_size,
                        ctx->env().get_local(ctx->module()),
                        var.second.get_local(ctx->module()), var.second.type,
                        "memory"));
                }

                for (const auto &var : ctx->parameters.back()) {
                    auto var_size =
                        var.second.type == BinaryenTypeFloat64() ? 8 : 4;
                    result.emplace_back(BinaryenStore(
                        ctx->module(), var_size, var.second.offset, var_size,
                        ctx->env().get_local(ctx->module()),
                        var.second.get_local(ctx->module()), var.second.type,
                        "memory"));
                }

                result.emplace_back(temp.set_local(ctx->module(), callee_expr));

                if (expr->type->to_binaryen_type() == BinaryenTypeNone()) {
                    result.emplace_back(BinaryenCallIndirect(
                        ctx->module(), "fun_table",
                        BinaryenLoad(ctx->module(), 4, false, 4, 4,
                                     BinaryenTypeInt32(),
                                     temp.get_local(ctx->module()), "memory"),
                        std::array{arg_expr, temp.get_local(ctx->module())}
                            .data(),
                        2,
                        BinaryenTypeCreate(
                            std::array{
                                node.argument->type->to_binaryen_type(),
                                BinaryenTypeInt32(),
                            }
                                .data(),
                            2),
                        expr->type->to_binaryen_type()));

                    for (const auto &var : ctx->variables.back()) {
                        auto var_size =
                            var.second.type == BinaryenTypeFloat64() ? 8 : 4;
                        result.emplace_back(var.second.set_local(
                            ctx->module(),
                            BinaryenLoad(ctx->module(), var_size, false,
                                         var.second.offset, var_size,
                                         var.second.type,
                                         ctx->env().get_local(ctx->module()),
                                         "memory")));
                    }

                    for (const auto &var : ctx->parameters.back()) {
                        auto var_size =
                            var.second.type == BinaryenTypeFloat64() ? 8 : 4;
                        result.emplace_back(var.second.set_local(
                            ctx->module(),
                            BinaryenLoad(ctx->module(), var_size, false,
                                         var.second.offset, var_size,
                                         var.second.type,
                                         ctx->env().get_local(ctx->module()),
                                         "memory")));
                    }
                } else {
                    auto &temp2 =
                        ctx->add_variable(expr->type->to_binaryen_type());
                    auto var_size =
                        expr->type->to_binaryen_type() == BinaryenTypeFloat64()
                            ? 8
                            : 4;
                    result.emplace_back(BinaryenStore(
                        ctx->module(), var_size, temp2.offset, var_size,
                        ctx->env().get_local(ctx->module()),
                        BinaryenCallIndirect(
                            ctx->module(), "fun_table",
                            BinaryenLoad(ctx->module(), 4, false, 4, 4,
                                         BinaryenTypeInt32(),
                                         temp.get_local(ctx->module()),
                                         "memory"),
                            std::array{arg_expr, temp.get_local(ctx->module())}
                                .data(),
                            2,
                            BinaryenTypeCreate(
                                std::array{
                                    node.argument->type->to_binaryen_type(),
                                    BinaryenTypeInt32(),
                                }
                                    .data(),
                                2),
                            expr->type->to_binaryen_type()),
                        expr->type->to_binaryen_type(), "memory"));

                    for (const auto &var : ctx->variables.back()) {
                        auto var_size =
                            var.second.type == BinaryenTypeFloat64() ? 8 : 4;
                        result.emplace_back(var.second.set_local(
                            ctx->module(),
                            BinaryenLoad(ctx->module(), var_size, false,
                                         var.second.offset, var_size,
                                         var.second.type,
                                         ctx->env().get_local(ctx->module()),
                                         "memory")));
                    }

                    for (const auto &var : ctx->parameters.back()) {
                        auto var_size =
                            var.second.type == BinaryenTypeFloat64() ? 8 : 4;
                        result.emplace_back(var.second.set_local(
                            ctx->module(),
                            BinaryenLoad(ctx->module(), var_size, false,
                                         var.second.offset, var_size,
                                         var.second.type,
                                         ctx->env().get_local(ctx->module()),
                                         "memory")));
                    }

                    result.emplace_back(temp2.get_local(ctx->module()));
                }

                return BinaryenBlock(ctx->module(), nullptr, result.data(),
                                     result.size(),
                                     expr->type->to_binaryen_type());
            }

            if constexpr (std::is_same_v<T, Lambda>) {
                ctx->push_context();

                auto &param = ctx->add_parameter(
                    node.parameter.lexeme, std::get<TypeFun>(expr->type->node)
                                               .param->to_binaryen_type());
                ctx->add_env();

                BinaryenIndex param_size =
                    param.type == BinaryenTypeFloat64() ? 8 : 4;

                auto previous_offset = ctx->offset();

                auto body_result = std::array{
                    BinaryenStore(
                        ctx->module(), param_size, param.offset, param_size,
                        ctx->env().get_local(ctx->module()),
                        param.get_local(ctx->module()), param.type, "memory"),
                    create(node.body),
                };
                auto &lambda = ctx->add_function(
                    BinaryenBlock(ctx->module(), nullptr, body_result.data(),
                                  body_result.size(),
                                  node.body->type->to_binaryen_type()),
                    node.body->type->to_binaryen_type(),
                    ctx->offset() - previous_offset);

                ctx->pop_context();

                return ctx->make_closure(lambda);
            }

            if constexpr (std::is_same_v<T, Literal>) {
                double value = std::stod(std::string(node.value.lexeme));
                return BinaryenConst(ctx->module(),
                                     BinaryenLiteralFloat64(value));
            }

            if constexpr (std::is_same_v<T, UnaryOp>)
                return BinaryenUnary(ctx->module(), BinaryenNegFloat64(),
                                     create(node.expr));

            if constexpr (std::is_same_v<T, Variable>) {
                if (node.name.lexeme == "TIME")
                    return BinaryenBinary(
                        ctx->module(), BinaryenDivFloat64(),
                        BinaryenGlobalGet(ctx->module(),
                                          node.name.lexeme.c_str(),
                                          BinaryenTypeFloat64()),
                        BinaryenConst(ctx->module(),
                                      BinaryenLiteralFloat64(sample_rate)));

                if (ctx->has_constant(node.name.lexeme))
                    return ctx->constant(node.name.lexeme);

                if (ctx->has_function(node.name.lexeme)) {
                    auto &fun = ctx->function(node.name.lexeme);
                    return ctx->make_closure(fun);
                }

                if (!ctx->has_variable_or_parameter(node.name.lexeme)) {
                    if (ctx->has_buffer(node.name.lexeme))
                        return BinaryenLoad(
                            ctx->module(), 8, false,
                            ctx->buffers.at(node.name.lexeme), 8,
                            BinaryenTypeFloat64(),
                            BinaryenGlobalGet(ctx->module(),
                                              node.name.lexeme.c_str(),
                                              BinaryenTypeInt32()),
                            "memory");

                    throw std::runtime_error("Unknown variable: " +
                                             node.name.lexeme);
                }

                auto [var, depth] =
                    ctx->variable_or_parameter(node.name.lexeme);

                if (depth == 0) return var.get_local(ctx->module());

                BinaryenExpressionRef acc = BinaryenLocalGet(
                    ctx->module(), ctx->env().local, BinaryenTypeInt32());

                for (int i = 0; i < depth; ++i)
                    acc = BinaryenLoad(ctx->module(), 4, false, 0, 4,
                                       BinaryenTypeInt32(), acc, "memory");

                auto var_size = var.type == BinaryenTypeFloat64() ? 8 : 4;
                return BinaryenLoad(ctx->module(), var_size, false, var.offset,
                                    var_size, var.type, acc, "memory");
            }
        },
        expr->node);
}