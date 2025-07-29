#include "type_inference.hpp"

#include "type.hpp"

#include <cstddef>
#include <stdexcept>
#include <string>
#include <type_traits>
#include <unordered_map>
#include <variant>
#include <vector>

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
            [&](const TypeVar &va) {
                if (occurs_in(va.id, tb)) error("Occurs check failed (left)");
                subst.emplace(va.id, tb);
            },
        },
        ta->node);
}

void infer_expr(const ExprPtr &expr,
                std::vector<std::unordered_map<std::string, TypePtr>> &env,
                Substitution &subst, TypeGenerator &gen) {
    try {
        expr->type = std::visit(
            [&](const auto &node) -> TypePtr {
                using T = std::decay_t<decltype(node)>;

                if constexpr (std::is_same_v<T, Assignment>) {
                    infer_expr(node.value, env, subst, gen);
                    auto it = env.front().end();
                    for (int i = env.size() - 1; i >= 0; --i) {
                        it = env[i].find(node.name.lexeme);
                        if (it != env[i].end()) break;
                    }
                    if (it == env.front().end())
                        env.back().emplace(node.name.lexeme, node.value->type);
                    else
                        unify(it->second, node.value->type, subst);
                    return Type::make<TypeBase>(BaseTypeKind::Void);
                }

                if constexpr (std::is_same_v<T, BinaryOp>) {
                    infer_expr(node.left, env, subst, gen);
                    infer_expr(node.right, env, subst, gen);
                    unify(node.left->type, node.right->type, subst);
                    return node.left->type;
                }

                if constexpr (std::is_same_v<T, Block>) {
                    if (node.expressions.size() == 0)
                        return Type::make<TypeBase>(BaseTypeKind::Void);
                    for (auto &child : node.expressions)
                        infer_expr(child, env, subst, gen);
                    return apply_subst(subst, node.expressions.back()->type);
                }

                if constexpr (std::is_same_v<T, Buffer>) {
                    infer_expr(node.init_buffer_function, env, subst, gen);
                    env.back().emplace(
                        node.name, Type::make<TypeBase>(BaseTypeKind::Float));
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
                    env.emplace_back(std::unordered_map<std::string, TypePtr>(
                        {{node.parameter.lexeme, param_type}}));
                    infer_expr(node.body, env, subst, gen);
                    env.pop_back();
                    return Type::make<TypeFun>(
                        apply_subst(subst, param_type),
                        apply_subst(subst, node.body->type));
                }

                if constexpr (std::is_same_v<T, Literal>) {
                    return Type::make<TypeBase>(BaseTypeKind::Float);
                }

                if constexpr (std::is_same_v<T, UnaryOp>) {
                    infer_expr(node.expr, env, subst, gen);
                    return node.expr->type;
                }

                if constexpr (std::is_same_v<T, Variable>) {
                    auto it = env.front().end();
                    for (int i = env.size() - 1; i >= 0; --i) {
                        it = env[i].find(node.name.lexeme);
                        if (it != env[i].end()) break;
                    }
                    if (it == env.front().end())
                        throw std::runtime_error("Unbound variable: " +
                                                 node.name.lexeme);
                    return apply_subst(subst, it->second);
                }

                throw std::runtime_error("Unknown expression type");
            },
            expr->node);
    } catch (const std::runtime_error &ex) {
        throw ex;
    }
}