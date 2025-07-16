#include "expression_emitter.hpp"

#include "binaryen-c.h"
#include "closure_builder.hpp"
#include "code_gen_context.hpp"
#include "free_var_analyzer.hpp"

#include <memory>
#include <string_view>

namespace {

auto map_to_binaryen_type(const TypePtr &type) -> BinaryenType {
    if (auto *base = std::get_if<TypeBase>(&type->node)) {
        switch (base->kind) {
        case BaseTypeKind::Float:
            return BinaryenTypeFloat64();
        case BaseTypeKind::Int:
        case BaseTypeKind::Bool:
            return BinaryenTypeInt32();
        case BaseTypeKind::Void:
            return BinaryenTypeNone();
        }
    }

    if (std::holds_alternative<TypeFun>(type->node))
        return BinaryenTypeFuncref();

    return BinaryenTypeInt32();
}

auto get_function_param_types(const TypePtr &type)
    -> std::vector<BinaryenType> {
    std::vector<BinaryenType> params;
    params.push_back(BinaryenTypeInt32());

    TypePtr current = type;
    while (auto *fun = std::get_if<TypeFun>(&current->node)) {
        params.push_back(map_to_binaryen_type(fun->param));
        current = fun->result;
    }

    return params;
}

} // namespace

ExpressionEmitter::ExpressionEmitter(const std::shared_ptr<CodeGenContext> &ctx)
    : ctx(ctx), closure_builder(ctx->module, 1024) {}

auto ExpressionEmitter::create(const ExprPtr &expr) -> BinaryenExpressionRef {
    return std::visit(
        [&](const auto &node) -> BinaryenExpressionRef {
            using T = std::decay_t<decltype(node)>;

            if constexpr (std::is_same_v<T, Expr::Assignment>) {
                std::string_view name(node.name.lexeme);
                if (ctx->variables.contains(name))
                    throw std::runtime_error("Variable already assigned: " +
                                             std::string(name));

                auto *val = create(node.value);

                BinaryenIndex idx =
                    ctx->parameters.size() + ctx->variables.size();
                ctx->variables[name] = BinaryenVariable{
                    .local = idx,
                    .binaryen_type = map_to_binaryen_type(node.value->type),
                    .type = node.value->type,
                };

                return BinaryenLocalSet(ctx->module, idx, val);
            }

            if constexpr (std::is_same_v<T, Expr::Block>) {
                std::vector<BinaryenExpressionRef> children;
                children.reserve(node.expressions.size());
                for (const auto &e : node.expressions)
                    children.emplace_back(create(e));

                return BinaryenBlock(
                    ctx->module, nullptr, children.data(), children.size(),
                    map_to_binaryen_type(node.expressions.back()->type));
            }

            if constexpr (std::is_same_v<T, Expr::Call>) {
                const auto &call = node;

                auto callee_expr = create(call.callee);
                auto arg_expr = create(call.argument);

                if (auto *var =
                        std::get_if<Expr::Variable>(&call.callee->node)) {
                    std::string_view name = var->name.lexeme;

                    if (name == "sin")
                        return BinaryenCall(ctx->module, "wasmwasm_sin",
                                            &arg_expr, 1,
                                            BinaryenTypeFloat64());

                    if (ctx->function_indices.contains(name))
                        return BinaryenCall(
                            ctx->module, std::string(name).c_str(), &arg_expr,
                            1, BinaryenTypeFloat64());
                }

                auto func_index =
                    BinaryenLoad(ctx->module, 4, false, 0, 4,
                                 BinaryenTypeInt32(), callee_expr, "memory");
                auto env_ptr =
                    BinaryenLoad(ctx->module, 4, false, 4, 4,
                                 BinaryenTypeInt32(), callee_expr, "memory");

                std::array<BinaryenExpressionRef, 2> call_args = {env_ptr,
                                                                  arg_expr};
                auto param_types = get_function_param_types(expr->type);
                auto result_type = map_to_binaryen_type(call.argument->type);

                return BinaryenCallIndirect(
                    ctx->module, "func_table", func_index, call_args.data(),
                    call_args.size(),
                    BinaryenTypeCreate(param_types.data(), param_types.size()),
                    result_type);
            }

            if constexpr (std::is_same_v<T, Expr::Lambda>) {
                auto free_vars = FreeVarAnalyzer::analyze(
                    expr, {std::string_view(node.parameter.lexeme)});

                std::unordered_map<std::string_view, BinaryenIndex> env_offsets;
                BinaryenIndex offset = 0;
                for (const auto &v : free_vars)
                    env_offsets[v] = offset++;

                auto func_name =
                    "lambda$" + std::to_string(ctx->function_indices.size());
                BinaryenIndex func_index = ctx->function_indices.size();
                ctx->function_indices[func_name] =
                    BinaryenVariable{.local = func_index};

                auto body_expr = with_fresh_scope([&] {
                    ctx->variables[node.parameter.lexeme] = BinaryenVariable{
                        .local = 1,
                        .binaryen_type = map_to_binaryen_type(expr->type),
                        .type = std::get<TypeFun>(expr->type->node).param,
                    };

                    for (const auto &[var, idx] : env_offsets) {
                        ctx->variables[var] = BinaryenVariable{
                            .local = idx * 8,
                            .binaryen_type = BinaryenTypeFloat64(),
                            .type = Type::make<TypeBase>(BaseTypeKind::Float),
                        };
                        ctx->mem_loaded_variables.insert(var);
                    }

                    return create(node.body);
                });

                std::array<BinaryenType, 2> param_types = {
                    BinaryenTypeInt32(), BinaryenTypeFloat64()};
                auto param_type =
                    BinaryenTypeCreate(param_types.data(), param_types.size());

                BinaryenFunctionRef func = BinaryenAddFunction(
                    ctx->module, func_name.c_str(), param_type,
                    BinaryenTypeFloat64(), nullptr, 0, body_expr);

                ctx->function_indices[func_name] = BinaryenVariable{
                    .local = func_index, .binaryen_type = BinaryenTypeInt32()};

                return closure_builder.build(
                    BinaryenConst(ctx->module,
                                  BinaryenLiteralInt32(func_index)),
                    build_captures(free_vars));
            }

            if constexpr (std::is_same_v<T, Expr::Literal>) {
                double value = std::stod(std::string(node.value.lexeme));
                return BinaryenConst(ctx->module,
                                     BinaryenLiteralFloat64(value));
            }

            if constexpr (std::is_same_v<T, Expr::Variable>) {
                std::string_view name(node.name.lexeme);

                if (name == "PI")
                    return BinaryenConst(
                        ctx->module, BinaryenLiteralFloat64(std::numbers::pi));

                if (name == "TIME")
                    return BinaryenGlobalGet(ctx->module,
                                             std::string(name).c_str(),
                                             BinaryenTypeFloat64());

                if (auto it = ctx->variables.find(name);
                    it != ctx->variables.end()) {
                    return BinaryenLocalGet(ctx->module, it->second.local,
                                            map_to_binaryen_type(expr->type));
                }
                throw std::runtime_error("Unknown variable: " +
                                         std::string(name));
            }

            throw std::runtime_error("Unknown expression type in create");
        },
        expr->node);
}

auto ExpressionEmitter::build_captures(
    const std::unordered_set<std::string_view> &free_vars)
    -> std::vector<BinaryenExpressionRef> {

    std::vector<BinaryenExpressionRef> captured_values;
    captured_values.reserve(free_vars.size());

    for (const auto &v : free_vars) {
        auto it = ctx->variables.find(v);
        if (it == ctx->variables.end()) {
            throw std::runtime_error("Free var not found in context: " +
                                     std::string(v));
        }

        // Build typed Expr::Variable
        auto var_expr =
            Expr::make<Expr::Variable>(Token{.lexeme = std::string(v)});
        var_expr->type = ctx->variables[v].type;
        captured_values.push_back(create(var_expr));
    }

    return captured_values;
}
