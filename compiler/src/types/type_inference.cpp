#include "type_inference.hpp"

#include "type.hpp"

#include <cstddef>
#include <stdexcept>
#include <type_traits>
#include <variant>

class TypeGenerator {
    size_t current_type = 0;

  public:
    auto fresh_type_var() -> TypePtr {
        return Type::make<TypeVar>(current_type++);
    }
};

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
                Substitution &subst) -> TypePtr {
    auto gen = TypeGenerator();
    return std::visit(
        [&](const auto &node) -> TypePtr {
            using T = std::decay_t<decltype(node)>;

            if constexpr (std::is_same_v<T, Literal>)
                return Type::make<TypeBase>(BaseTypeKind::Float);
            if constexpr (std::is_same_v<T, Variable>) {
                auto it = env.find(node.name.lexeme);
                if (it == env.end())
                    throw std::runtime_error("Unbound variable: " +
                                             std::string(node.name.lexeme));
                return apply_subst(subst, it->second);
            }
            if constexpr (std::is_same_v<T, Assignment>) {
                auto rhs_type = infer_expr(node.value, env, subst);
                env[node.name.lexeme] = rhs_type;
                return rhs_type;
            }

            if constexpr (std::is_same_v<T, Call>) {
                auto fn_type = infer_expr(node.callee, env, subst);
                auto arg_type = infer_expr(node.argument, env, subst);
                auto result_type = gen.fresh_type_var();
                unify(fn_type, Type::make<TypeFun>(arg_type, result_type),
                      subst);
                return result_type;
            }

            if constexpr (std::is_same_v<T, Binary>) {
                auto left_type = infer_expr(node.lhs, env, subst);
                auto right_type = infer_expr(node.rhs, env, subst);

                // For now, assume binary ops require Float
                unify(left_type, Type::make<TypeBase>(BaseTypeKind::Float),
                      subst);
                unify(right_type, Type::make<TypeBase>(BaseTypeKind::Float),
                      subst);
                return Type::make<TypeBase>(BaseTypeKind::Float);
            }

            throw std::runtime_error("Unknown expression type");
        },
        expr->node);
}