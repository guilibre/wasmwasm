#include "type_inference.hpp"

#include "type.hpp"

#include <cstddef>
#include <stdexcept>
#include <type_traits>
#include <variant>

namespace {

template <typename... Ts> struct overloaded : Ts... {
    using Ts::operator()...;
};
template <typename... Ts> overloaded(Ts...) -> overloaded<Ts...>;

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

    auto error = [](const std::string &msg) {
        throw std::runtime_error("Type error: " + msg);
    };

    std::visit(
        overloaded{
            [&](const TypeVar &va) {
                if (occurs_in(va.id, tb)) error("Occurs check failed (left)");
                subst.emplace(va.id, tb);
            },
            [&](const TypeFun &fa) {
                std::visit(
                    overloaded{
                        [&](const TypeFun &fb) {
                            unify(fa.param, fb.param, subst);
                            unify(fa.result, fb.result, subst);
                        },
                        [&](const TypeVar &vb) {
                            if (occurs_in(vb.id, ta))
                                error("Occurs check failed (right)");
                            subst.emplace(vb.id, ta);
                        },
                        [&](const auto &) {
                            error(
                                "Cannot unify function type with non-function");
                        }},
                    tb->node);
            },
            [&](const TypeBase &ba) {
                std::visit(
                    overloaded{[&](const TypeBase &bb) {
                                   if (ba.kind != bb.kind)
                                       error("Base type mismatch");
                               },
                               [&](const TypeVar &vb) {
                                   if (occurs_in(vb.id, ta))
                                       error("Occurs check failed (right)");
                                   subst.emplace(vb.id, ta);
                               },
                               [&](const auto &) {
                                   error(
                                       "Cannot unify base type with non-base");
                               }},
                    tb->node);
            }},
        ta->node);
}

void infer_expr(const ExprPtr &expr,
                std::unordered_map<std::string, TypePtr> &env,
                Substitution &subst, TypeGenerator &gen) {
    expr->type = std::visit(
        [&](const auto &node) -> TypePtr {
            using T = std::decay_t<decltype(node)>;

            if constexpr (std::is_same_v<T, Expr::Assignment>) {
                infer_expr(node.value, env, subst, gen);
                env.emplace(node.name.lexeme, node.value->type);
                return apply_subst(subst,
                                   Type::make<TypeBase>(BaseTypeKind::Void));
            }

            if constexpr (std::is_same_v<T, Expr::Block>) {
                for (auto &child : node.expressions)
                    infer_expr(child, env, subst, gen);
                return apply_subst(subst, node.expressions.back()->type);
            }

            if constexpr (std::is_same_v<T, Expr::Call>) {
                infer_expr(node.callee, env, subst, gen);
                infer_expr(node.argument, env, subst, gen);
                auto result_type = gen.fresh_type_var();
                auto fun_type =
                    Type::make<TypeFun>(node.argument->type, result_type);
                unify(node.callee->type, fun_type, subst);
                node.callee->type = apply_subst(subst, node.callee->type);
                infer_expr(node.callee, env, subst, gen);
                infer_expr(node.argument, env, subst, gen);
                return apply_subst(subst, result_type);
            }

            if constexpr (std::is_same_v<T, Expr::Lambda>) {
                auto param_type = gen.fresh_type_var();
                env.emplace(node.parameter.lexeme, param_type);
                infer_expr(node.body, env, subst, gen);
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
                                             node.name.lexeme);
                return apply_subst(subst, it->second);
            }

            throw std::runtime_error("Unknown expression type");
        },
        expr->node);
}