#include "type_inference.hpp"

#include "type.hpp"

#include <cstddef>
#include <optional>
#include <stdexcept>
#include <string>
#include <type_traits>
#include <unordered_map>
#include <variant>
#include <vector>

namespace {

auto lookup_env(
    const std::string &name,
    const std::vector<std::unordered_map<std::string, TypePtr>> &env)
    -> std::optional<TypePtr> {
    for (int i = static_cast<int>(env.size()) - 1; i >= 0; --i) {
        auto it = env[static_cast<size_t>(i)].find(name);
        if (it != env[static_cast<size_t>(i)].end()) return it->second;
    }
    return std::nullopt;
}

} // namespace

auto occurs_in(size_t var_id, const TypePtr &type) -> bool {
    return std::visit(
        [&](const auto &node) -> bool {
            using T = std::decay_t<decltype(node)>;
            if constexpr (std::is_same_v<T, TypeVar>) return node.id == var_id;
            if constexpr (std::is_same_v<T, TypeFun>)
                return occurs_in(var_id, node.param) ||
                       occurs_in(var_id, node.result);
            return false;
        },
        type->node);
}

auto apply_subst(const Substitution &subst, const TypePtr &type) -> TypePtr {
    return std::visit(
        [&](const auto &node) -> TypePtr {
            using T = std::decay_t<decltype(node)>;
            if constexpr (std::is_same_v<T, TypeVar>) {
                auto it = subst.find(node.id);
                return it != subst.end() ? apply_subst(subst, it->second)
                                         : type;
            }
            if constexpr (std::is_same_v<T, TypeFun>) {
                auto new_param = apply_subst(subst, node.param);
                auto new_result = apply_subst(subst, node.result);
                return Type::make<TypeFun>(new_param, new_result);
            }
            return type;
        },
        type->node);
}

void unify(const TypePtr &a, const TypePtr &b, Substitution &subst) {
    const TypePtr ta = apply_subst(subst, a);
    const TypePtr tb = apply_subst(subst, b);
    if (ta == tb) return;

    auto error = [](const std::string &msg) -> void {
        throw std::runtime_error("Type error: " + msg);
    };

    std::visit(
        [&](const auto &na) -> void {
            using A = std::decay_t<decltype(na)>;
            if constexpr (std::is_same_v<A, TypeBase>) {
                std::visit(
                    [&](const auto &nb) -> void {
                        using B = std::decay_t<decltype(nb)>;
                        if constexpr (std::is_same_v<B, TypeBase>) {
                            if (na.kind != nb.kind) error("Base type mismatch");
                        } else if constexpr (std::is_same_v<B, TypeVar>) {
                            if (occurs_in(nb.id, ta))
                                error("Occurs check failed (right)");
                            subst.emplace(nb.id, ta);
                        } else {
                            error("Cannot unify base type with non-base");
                        }
                    },
                    tb->node);
            } else if constexpr (std::is_same_v<A, TypeFun>) {
                std::visit(
                    [&](const auto &nb) -> void {
                        using B = std::decay_t<decltype(nb)>;
                        if constexpr (std::is_same_v<B, TypeFun>) {
                            unify(na.param, nb.param, subst);
                            unify(na.result, nb.result, subst);
                        } else if constexpr (std::is_same_v<B, TypeVar>) {
                            if (occurs_in(nb.id, ta))
                                error("Occurs check failed (right)");
                            subst.emplace(nb.id, ta);
                        } else {
                            error(
                                "Cannot unify function type with non-function");
                        }
                    },
                    tb->node);
            } else if constexpr (std::is_same_v<A, TypeVar>) {
                if (occurs_in(na.id, tb)) error("Occurs check failed (left)");
                subst.emplace(na.id, tb);
            } else if constexpr (std::is_same_v<A, TypeArray>) {
                std::visit(
                    [&](const auto &nb) -> void {
                        using B = std::decay_t<decltype(nb)>;
                        if constexpr (std::is_same_v<B, TypeArray>) {
                            // compatible
                        } else if constexpr (std::is_same_v<B, TypeVar>) {
                            subst.emplace(nb.id, ta);
                        } else {
                            error("Cannot unify array type with non-array");
                        }
                    },
                    tb->node);
            }
        },
        ta->node);
}

void infer_expr(const ExprPtr &expr,
                std::vector<std::unordered_map<std::string, TypePtr>> &env,
                Substitution &subst, TypeGenerator &gen) {
    expr->type = std::visit(
        [&](const auto &node) -> TypePtr {
            using T = std::decay_t<decltype(node)>;

            if constexpr (std::is_same_v<T, Bind>) {
                auto existing = lookup_env(node.name.lexeme, env);
                auto rec_type = gen.fresh_type_var();
                if (!existing) env.back().emplace(node.name.lexeme, rec_type);
                infer_expr(node.value, env, subst, gen);
                if (!existing) {
                    unify(rec_type, node.value->type, subst);
                    env.back()[node.name.lexeme] =
                        apply_subst(subst, node.value->type);
                } else {
                    unify(*existing, node.value->type, subst);
                }
                return Type::make<TypeBase>(BaseTypeKind::Void);
            }

            if constexpr (std::is_same_v<T, BinaryOp>) {
                infer_expr(node.left, env, subst, gen);
                infer_expr(node.right, env, subst, gen);
                unify(node.left->type, node.right->type, subst);
                return apply_subst(subst, node.left->type);
            }

            if constexpr (std::is_same_v<T, CodeBlock>) {
                if (node.expressions.empty())
                    return Type::make<TypeBase>(BaseTypeKind::Void);
                for (auto &child : node.expressions)
                    infer_expr(child, env, subst, gen);
                return apply_subst(subst, node.expressions.back()->type);
            }

            if constexpr (std::is_same_v<T, DelayCtor>) {
                infer_expr(node.init_fn, env, subst, gen);
                return Type::make<TypeBase>(BaseTypeKind::Float);
            }

            if constexpr (std::is_same_v<T, DelayRead>) {
                if (node.delay) infer_expr(*node.delay, env, subst, gen);
                auto type = lookup_env(node.name.lexeme, env);
                if (!type)
                    throw std::runtime_error("Unbound delay: @" +
                                             node.name.lexeme);
                return apply_subst(subst, *type);
            }

            if constexpr (std::is_same_v<T, DelayWrite>) {
                infer_expr(node.value, env, subst, gen);
                return Type::make<TypeBase>(BaseTypeKind::Void);
            }

            if constexpr (std::is_same_v<T, DelayWriteQuiet>) {
                if (node.delay) infer_expr(*node.delay, env, subst, gen);
                infer_expr(node.value, env, subst, gen);
                return Type::make<TypeBase>(BaseTypeKind::Void);
            }

            if constexpr (std::is_same_v<T, Call>) {
                infer_expr(node.callee, env, subst, gen);
                infer_expr(node.argument, env, subst, gen);
                auto result_type = gen.fresh_type_var();
                auto fun_type =
                    Type::make<TypeFun>(node.argument->type, result_type);
                unify(node.callee->type, fun_type, subst);
                node.callee->type = apply_subst(subst, node.callee->type);
                return apply_subst(subst, result_type);
            }

            if constexpr (std::is_same_v<T, Lambda>) {
                auto param_type = gen.fresh_type_var();
                if (node.parameter.has_value()) {
                    env.emplace_back(std::unordered_map<std::string, TypePtr>(
                        {{node.parameter->lexeme, param_type}}));
                    infer_expr(node.body, env, subst, gen);
                    env.pop_back();
                } else {
                    infer_expr(node.body, env, subst, gen);
                }
                return Type::make<TypeFun>(apply_subst(subst, param_type),
                                           apply_subst(subst, node.body->type));
            }

            if constexpr (std::is_same_v<T, InputRead>) {
                return Type::make<TypeBase>(BaseTypeKind::Float);
            }

            if constexpr (std::is_same_v<T, OutputWrite>) {
                infer_expr(node.value, env, subst, gen);
                unify(node.value->type,
                      Type::make<TypeBase>(BaseTypeKind::Float), subst);
                return Type::make<TypeBase>(BaseTypeKind::Void);
            }

            if constexpr (std::is_same_v<T, StaticBind>) {
                infer_expr(node.init, env, subst, gen);
                auto existing = lookup_env(node.name.lexeme, env);
                if (!existing)
                    env.back().emplace(node.name.lexeme, node.init->type);
                else
                    unify(*existing, node.init->type, subst);
                return Type::make<TypeBase>(BaseTypeKind::Void);
            }

            if constexpr (std::is_same_v<T, ParamBind>) {
                auto float_type = Type::make<TypeBase>(BaseTypeKind::Float);
                env.back().emplace(node.name.lexeme, float_type);
                return Type::make<TypeBase>(BaseTypeKind::Void);
            }

            if constexpr (std::is_same_v<T, Conditional>) {
                infer_expr(node.condition, env, subst, gen);
                infer_expr(node.then_branch, env, subst, gen);
                if (node.else_branch) {
                    infer_expr(*node.else_branch, env, subst, gen);
                    unify(node.then_branch->type, (*node.else_branch)->type,
                          subst);
                }
                return apply_subst(subst, node.then_branch->type);
            }

            if constexpr (std::is_same_v<T, ArrayLiteral>) {
                if (node.elements.empty())
                    throw std::runtime_error(
                        "Array literal must have at least one element");
                const auto float_type =
                    Type::make<TypeBase>(BaseTypeKind::Float);
                for (const auto &elem : node.elements) {
                    infer_expr(elem, env, subst, gen);
                    unify(elem->type, float_type, subst);
                }
                return Type::make<TypeArray>();
            }

            if constexpr (std::is_same_v<T, ArrayCtor>) {
                const auto float_type =
                    Type::make<TypeBase>(BaseTypeKind::Float);
                const auto float_to_float =
                    Type::make<TypeFun>(float_type, float_type);
                infer_expr(node.init_fn, env, subst, gen);
                unify(node.init_fn->type, float_to_float, subst);
                return Type::make<TypeArray>();
            }

            if constexpr (std::is_same_v<T, Literal>) {
                return Type::make<TypeBase>(BaseTypeKind::Float);
            }

            if constexpr (std::is_same_v<T, UnaryOp>) {
                infer_expr(node.expr, env, subst, gen);
                return apply_subst(subst, node.expr->type);
            }

            if constexpr (std::is_same_v<T, Variable>) {
                auto type = lookup_env(node.name.lexeme, env);
                if (!type)
                    throw std::runtime_error("Unbound variable: " +
                                             node.name.lexeme);
                return apply_subst(subst, *type);
            }

            throw std::runtime_error("Unknown expression type");
        },
        expr->node);
}
