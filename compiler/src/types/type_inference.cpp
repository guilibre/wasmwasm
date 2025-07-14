#include "type_inference.hpp"

#include "type.hpp"

#include <cstddef>
#include <stdexcept>
#include <type_traits>
#include <variant>

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
    const auto ta = apply_subst(subst, a);
    const auto tb = apply_subst(subst, b);

    if (ta == tb) return;

    auto &ta_node = ta->node;

    std::visit(
        [&](const auto &na) {
            using TA = std::decay_t<decltype(na)>;
            auto &tb_node = tb->node;
            std::visit(
                [&](const auto &nb) {
                    using TB = std::decay_t<decltype(nb)>;

                    if constexpr (std::is_same_v<TA, TypeVar>) {
                        if (occurs_in(na.id, tb))
                            throw std::runtime_error("Occurs check failed");
                        subst[na.id] = tb;
                    } else if constexpr (std::is_same_v<TB, TypeVar>) {
                        if (occurs_in(nb.id, ta))
                            throw std::runtime_error("Occurs check failed");
                        subst[nb.id] = ta;
                    } else if constexpr (std::is_same_v<TA, TypeFun>) {
                        if constexpr (std::is_same_v<TB, TypeFun>) {
                            unify(na.param, nb.param, subst);
                            unify(na.result, nb.result, subst);
                        } else if constexpr (std::is_same_v<TA, TypeBase>) {
                            if constexpr (std::is_same_v<TB, TypeBase>) {
                                if (na.kind != nb.kind)
                                    throw std::runtime_error(
                                        "Base types don't match");
                            } else
                                throw std::runtime_error("Type mismatch");
                        } else
                            throw std::runtime_error("Type mismatch");
                    } else if constexpr (std::is_same_v<TA, TypeBase> &&
                                         std::is_same_v<TB, TypeBase>) {
                        if (na.kind != nb.kind)
                            throw std::runtime_error("Base types don't match");
                    } else
                        throw std::runtime_error("Type mismatch");
                },
                tb_node);
        },
        ta_node);
}

auto infer_expr(const ExprPtr &expr,
                std::unordered_map<std::string_view, TypePtr> &env,
                Substitution &subst, TypeGenerator &gen) -> TypePtr {
    return std::visit(
        [&](const auto &node) -> TypePtr {
            using T = std::decay_t<decltype(node)>;
            if constexpr (std::is_same_v<T, Expr::Assignment>) {
                node.value->type = infer_expr(node.value, env, subst, gen);
                env[node.name.lexeme] = node.value->type;
                return node.value->type;
            }

            if constexpr (std::is_same_v<T, Expr::Binary>) {
                node.lhs->type = infer_expr(node.lhs, env, subst, gen);
                node.rhs->type = infer_expr(node.rhs, env, subst, gen);

                unify(node.lhs->type, Type::make<TypeBase>(BaseTypeKind::Float),
                      subst);
                unify(node.rhs->type, Type::make<TypeBase>(BaseTypeKind::Float),
                      subst);

                node.lhs->type = apply_subst(subst, node.lhs->type);
                node.rhs->type = apply_subst(subst, node.rhs->type);

                return Type::make<TypeBase>(BaseTypeKind::Float);
            }

            if constexpr (std::is_same_v<T, Expr::Block>) {
                for (auto &child : node.expressions)
                    child->type = infer_expr(child, env, subst, gen);
                return node.expressions.back()->type;
            }

            if constexpr (std::is_same_v<T, Expr::Call>) {
                node.callee->type = infer_expr(node.callee, env, subst, gen);
                node.argument->type =
                    infer_expr(node.argument, env, subst, gen);
                auto result_type = gen.fresh_type_var();
                unify(node.callee->type,
                      Type::make<TypeFun>(node.argument->type, result_type),
                      subst);
                return result_type;
            }

            if constexpr (std::is_same_v<T, Expr::Lambda>) {
                auto param_type = gen.fresh_type_var();
                env[node.parameter.lexeme] = param_type;
                node.body->type = infer_expr(node.body, env, subst, gen);
                env.erase(node.parameter.lexeme);
                return Type::make<TypeFun>(apply_subst(subst, param_type),
                                           apply_subst(subst, node.body->type));
            }

            if constexpr (std::is_same_v<T, Expr::Literal>)
                return Type::make<TypeBase>(BaseTypeKind::Float);

            if constexpr (std::is_same_v<T, Expr::Variable>) {
                auto it = env.find(node.name.lexeme);
                if (it == env.end())
                    throw std::runtime_error("Unbound variable: " +
                                             std::string(node.name.lexeme));
                return apply_subst(subst, it->second);
            }

            throw std::runtime_error("Unknown expression type");
        },
        expr->node);
}