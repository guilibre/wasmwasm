#include "type_inference.hpp"

#include "type.hpp"
#include <algorithm>
#include <cstddef>
#include <optional>
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

using NameEnv = std::vector<std::unordered_map<std::string, std::string>>;

auto lookup_name(const std::string &name, const NameEnv &names)
    -> std::optional<std::string> {
    for (int i = static_cast<int>(names.size()) - 1; i >= 0; --i) {
        auto it = names[static_cast<size_t>(i)].find(name);
        if (it != names[static_cast<size_t>(i)].end()) return it->second;
    }
    return std::nullopt;
}

auto make_unique_name(const std::string &name, size_t &shadow_counter)
    -> std::string {
    return name + "$" + std::to_string(shadow_counter++);
}

auto find_tail_array_arity(const ExprPtr &e) -> std::optional<size_t> {
    if (const auto *lit = std::get_if<ArrayLiteral>(&e->node))
        return lit->elements.size();
    if (const auto *block = std::get_if<CodeBlock>(&e->node)) {
        if (block->expressions.empty()) return std::nullopt;
        return find_tail_array_arity(block->expressions.back());
    }
    if (const auto *cond = std::get_if<Conditional>(&e->node)) {
        const auto then_arity = find_tail_array_arity(cond->then_branch);
        if (!cond->else_branch) return std::nullopt;
        const auto else_arity = find_tail_array_arity(*cond->else_branch);
        if (then_arity && else_arity && *then_arity == *else_arity)
            return then_arity;
        return std::nullopt;
    }
    return std::nullopt;
}

auto make_array_of_floats(size_t n) -> TypePtr {
    const auto float_type = Type::make<TypeBase>(BaseTypeKind::Float);
    return Type::make<TypeArray>(std::vector<TypePtr>(n, float_type));
}

} // namespace

void pre_register_toplevel(
    const ExprPtr &program,
    std::vector<std::unordered_map<std::string, TypePtr>> &env) {
    const auto *block = std::get_if<CodeBlock>(&program->node);
    if (block == nullptr) return;
    for (const auto &expr : block->expressions) {
        const auto *bind = std::get_if<Bind>(&expr->node);
        if (bind == nullptr) continue;
        if (!std::holds_alternative<Lambda>(bind->value->node)) continue;

        std::vector<Token> params;
        const ExprPtr *ptr = &bind->value;
        while (std::holds_alternative<Lambda>((*ptr)->node)) {
            const auto &lam = std::get<Lambda>((*ptr)->node);
            if (lam.parameter.has_value()) params.push_back(*lam.parameter);
            ptr = &lam.body;
        }
        const auto arity = find_tail_array_arity(*ptr);
        if (!arity) continue;

        TypePtr ret_type = make_array_of_floats(*arity);
        for (auto it = params.rbegin(); it != params.rend(); ++it)
            ret_type = Type::make<TypeFun>(
                Type::make<TypeBase>(BaseTypeKind::Float), ret_type);
        env.back().emplace(bind->name.lexeme, ret_type);
    }
}

auto scalar_kind_of(const TypePtr &t) -> BaseTypeKind {
    if (const auto *base = std::get_if<TypeBase>(&t->node)) return base->kind;
    return BaseTypeKind::Float;
}

auto scalar_kinds_of(const TypePtr &t) -> std::vector<BaseTypeKind> {
    if (const auto *arr = std::get_if<TypeArray>(&t->node)) {
        std::vector<BaseTypeKind> out;
        out.reserve(arr->elements.size());
        for (const auto &elem : arr->elements)
            out.push_back(scalar_kind_of(elem));
        return out;
    }
    const auto scalar = scalar_kind_of(t);
    if (scalar == BaseTypeKind::Void) return {};
    return {scalar};
}

auto arity_of(const TypePtr &t) -> size_t {
    size_t arity = 0;
    const TypePtr *cur = &t;
    while (const auto *fn = std::get_if<TypeFun>(&(*cur)->node)) {
        arity++;
        cur = &fn->result;
    }
    return arity;
}

auto occurs_in(size_t var_id, const TypePtr &type) -> bool {
    return std::visit(
        [&](const auto &node) -> bool {
            using T = std::decay_t<decltype(node)>;
            if constexpr (std::is_same_v<T, TypeVar>) return node.id == var_id;
            if constexpr (std::is_same_v<T, TypeFun>)
                return occurs_in(var_id, node.param) ||
                       occurs_in(var_id, node.result);
            if constexpr (std::is_same_v<T, TypeArray>)
                return std::ranges::any_of(node.elements,
                                           [&](const TypePtr &elem) -> bool {
                                               return occurs_in(var_id, elem);
                                           });
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
            if constexpr (std::is_same_v<T, TypeArray>) {
                if (node.elements.empty()) return type;
                std::vector<TypePtr> new_elements;
                new_elements.reserve(node.elements.size());
                for (const auto &elem : node.elements)
                    new_elements.push_back(apply_subst(subst, elem));
                return Type::make<TypeArray>(std::move(new_elements));
            }
            return type;
        },
        type->node);
}

void unify(const TypePtr &a, const TypePtr &b, Substitution &subst,
           SourcePos pos) {
    const TypePtr ta = apply_subst(subst, a);
    const TypePtr tb = apply_subst(subst, b);
    if (ta == tb) return;

    auto error = [pos](const std::string &msg) -> void {
        throw TypeError(msg, pos);
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
                        }
                    },
                    tb->node);
            } else if constexpr (std::is_same_v<A, TypeFun>) {
                std::visit(
                    [&](const auto &nb) -> void {
                        using B = std::decay_t<decltype(nb)>;
                        if constexpr (std::is_same_v<B, TypeFun>) {
                            unify(na.param, nb.param, subst, pos);
                            unify(na.result, nb.result, subst, pos);
                        } else if constexpr (std::is_same_v<B, TypeVar>) {
                            if (occurs_in(nb.id, ta))
                                error("Occurs check failed (right)");
                            subst.emplace(nb.id, ta);
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
                            if (na.elements.empty() || nb.elements.empty())
                                return;
                            if (na.elements.size() != nb.elements.size())
                                error("Array/tuple arity mismatch: " +
                                      std::to_string(na.elements.size()) +
                                      " vs " +
                                      std::to_string(nb.elements.size()));
                            for (size_t i = 0; i < na.elements.size(); ++i)
                                unify(na.elements[i], nb.elements[i], subst,
                                      pos);
                        } else if constexpr (std::is_same_v<B, TypeVar>) {
                            if (occurs_in(nb.id, ta))
                                error("Occurs check failed (right)");
                            subst.emplace(nb.id, ta);
                        }
                    },
                    tb->node);
            }
        },
        ta->node);
}

namespace {

void infer_expr_impl(const ExprPtr &expr,
                     std::vector<std::unordered_map<std::string, TypePtr>> &env,
                     NameEnv &names, size_t &shadow_counter,
                     Substitution &subst, TypeGenerator &gen);

} // namespace

void infer_expr(const ExprPtr &expr,
                std::vector<std::unordered_map<std::string, TypePtr>> &env,
                Substitution &subst, TypeGenerator &gen) {
    NameEnv names(env.size());
    size_t shadow_counter = 0;
    if (auto *block = std::get_if<CodeBlock>(&expr->node)) {
        for (auto &child : block->expressions)
            infer_expr_impl(child, env, names, shadow_counter, subst, gen);
        expr->type = block->expressions.empty()
                         ? Type::make<TypeBase>(BaseTypeKind::Void)
                         : apply_subst(subst, block->expressions.back()->type);
        return;
    }
    infer_expr_impl(expr, env, names, shadow_counter, subst, gen);
}

namespace {

void infer_expr_impl(const ExprPtr &expr,
                     std::vector<std::unordered_map<std::string, TypePtr>> &env,
                     NameEnv &names, size_t &shadow_counter,
                     Substitution &subst, TypeGenerator &gen) {
    expr->type = std::visit(
        [&](auto &node) -> TypePtr {
            using T = std::decay_t<decltype(node)>;

            if constexpr (std::is_same_v<T, Bind>) {
                auto existing = lookup_env(node.name.lexeme, env);
                const bool is_new_binding = !existing;
                const auto resolved_name =
                    is_new_binding
                        ? make_unique_name(node.name.lexeme, shadow_counter)
                        : lookup_name(node.name.lexeme, names)
                              .value_or(node.name.lexeme);
                node.resolved_name = resolved_name;

                auto rec_type = gen.fresh_type_var();
                if (is_new_binding) {
                    env.back().emplace(node.name.lexeme, rec_type);
                    names.back().insert_or_assign(node.name.lexeme,
                                                  resolved_name);
                }
                infer_expr_impl(node.value, env, names, shadow_counter, subst,
                                gen);
                if (is_new_binding) {
                    unify(rec_type, node.value->type, subst, expr->pos);
                    env.back()[node.name.lexeme] =
                        apply_subst(subst, node.value->type);
                } else {
                    unify(*existing, node.value->type, subst, expr->pos);
                }
                return Type::make<TypeBase>(BaseTypeKind::Void);
            }

            if constexpr (std::is_same_v<T, BinaryOp>) {
                infer_expr_impl(node.left, env, names, shadow_counter, subst,
                                gen);
                infer_expr_impl(node.right, env, names, shadow_counter, subst,
                                gen);
                unify(node.left->type, node.right->type, subst, expr->pos);
                return apply_subst(subst, node.left->type);
            }

            if constexpr (std::is_same_v<T, CodeBlock>) {
                if (node.expressions.empty())
                    return Type::make<TypeBase>(BaseTypeKind::Void);
                env.emplace_back();
                names.emplace_back();
                for (auto &child : node.expressions)
                    infer_expr_impl(child, env, names, shadow_counter, subst,
                                    gen);
                env.pop_back();
                names.pop_back();
                return apply_subst(subst, node.expressions.back()->type);
            }

            if constexpr (std::is_same_v<T, DelayCtor>) {
                infer_expr_impl(node.init_fn, env, names, shadow_counter, subst,
                                gen);
                return Type::make<TypeBase>(BaseTypeKind::Float);
            }

            if constexpr (std::is_same_v<T, DelayRead>) {
                auto type = lookup_env(node.name.lexeme, env);
                if (!type)
                    throw TypeError("Unbound delay: @" + node.name.lexeme,
                                    expr->pos);
                auto resolved = apply_subst(subst, *type);
                if (std::holds_alternative<TypeArray>(resolved->node))
                    throw TypeError(
                        "'@" + node.name.lexeme + "' is an array; use " +
                            node.name.lexeme + "[i] to access an element",
                        expr->pos);
                if (node.delay)
                    infer_expr_impl(*node.delay, env, names, shadow_counter,
                                    subst, gen);
                return resolved;
            }

            if constexpr (std::is_same_v<T, ArrayIndex>) {
                auto type = lookup_env(node.name.lexeme, env);
                if (!type)
                    throw TypeError("Unbound variable: " + node.name.lexeme,
                                    expr->pos);
                auto resolved = apply_subst(subst, *type);
                const auto *idx_lit = std::get_if<Literal>(&node.index->node);
                if (!idx_lit)
                    throw TypeError(
                        "Array index in " + node.name.lexeme +
                            "[i] must be a compile-time integer literal",
                        expr->pos);
                const size_t index =
                    std::stoul(std::string(idx_lit->value.lexeme));

                auto resolved_name = lookup_name(node.name.lexeme, names);
                node.resolved_name =
                    resolved_name ? *resolved_name : node.name.lexeme;

                const auto *arr = std::get_if<TypeArray>(&resolved->node);
                if (!arr) return Type::make<TypeBase>(BaseTypeKind::Float);
                if (!arr->elements.empty() && index >= arr->elements.size())
                    throw TypeError("Array index " + std::to_string(index) +
                                        " out of bounds for '" +
                                        node.name.lexeme + "' (size " +
                                        std::to_string(arr->elements.size()) +
                                        ")",
                                    expr->pos);
                return arr->elements.empty()
                           ? Type::make<TypeBase>(BaseTypeKind::Float)
                           : arr->elements[index];
            }

            if constexpr (std::is_same_v<T, ExprIndex>) {
                infer_expr_impl(node.base, env, names, shadow_counter, subst,
                                gen);
                auto resolved = apply_subst(subst, node.base->type);
                const auto *idx_lit = std::get_if<Literal>(&node.index->node);
                if (!idx_lit)
                    throw TypeError(
                        "Array index must be a compile-time integer literal",
                        expr->pos);
                const size_t index =
                    std::stoul(std::string(idx_lit->value.lexeme));

                const auto *arr = std::get_if<TypeArray>(&resolved->node);
                if (!arr) return Type::make<TypeBase>(BaseTypeKind::Float);
                if (!arr->elements.empty() && index >= arr->elements.size())
                    throw TypeError("Array index " + std::to_string(index) +
                                        " out of bounds (size " +
                                        std::to_string(arr->elements.size()) +
                                        ")",
                                    expr->pos);
                return arr->elements.empty()
                           ? Type::make<TypeBase>(BaseTypeKind::Float)
                           : arr->elements[index];
            }

            if constexpr (std::is_same_v<T, DelayWrite>) {
                infer_expr_impl(node.value, env, names, shadow_counter, subst,
                                gen);
                return Type::make<TypeBase>(BaseTypeKind::Void);
            }

            if constexpr (std::is_same_v<T, DelayWriteQuiet>) {
                if (node.delay)
                    infer_expr_impl(*node.delay, env, names, shadow_counter,
                                    subst, gen);
                infer_expr_impl(node.value, env, names, shadow_counter, subst,
                                gen);
                return Type::make<TypeBase>(BaseTypeKind::Void);
            }

            if constexpr (std::is_same_v<T, Call>) {
                infer_expr_impl(node.callee, env, names, shadow_counter, subst,
                                gen);
                infer_expr_impl(node.argument, env, names, shadow_counter,
                                subst, gen);
                auto result_type = gen.fresh_type_var();
                auto fun_type =
                    Type::make<TypeFun>(node.argument->type, result_type);
                unify(node.callee->type, fun_type, subst, expr->pos);
                node.callee->type = apply_subst(subst, node.callee->type);
                return apply_subst(subst, result_type);
            }

            if constexpr (std::is_same_v<T, Lambda>) {
                auto param_type = gen.fresh_type_var();
                if (node.parameter.has_value()) {
                    env.emplace_back(std::unordered_map<std::string, TypePtr>(
                        {{node.parameter->lexeme, param_type}}));
                    names.emplace_back(
                        std::unordered_map<std::string, std::string>(
                            {{node.parameter->lexeme,
                              node.parameter->lexeme}}));
                    infer_expr_impl(node.body, env, names, shadow_counter,
                                    subst, gen);
                    env.pop_back();
                    names.pop_back();
                } else {
                    infer_expr_impl(node.body, env, names, shadow_counter,
                                    subst, gen);
                }
                return Type::make<TypeFun>(apply_subst(subst, param_type),
                                           apply_subst(subst, node.body->type));
            }

            if constexpr (std::is_same_v<T, InputRead>) {
                return Type::make<TypeBase>(BaseTypeKind::Float);
            }

            if constexpr (std::is_same_v<T, OutputWrite>) {
                infer_expr_impl(node.value, env, names, shadow_counter, subst,
                                gen);
                unify(node.value->type,
                      Type::make<TypeBase>(BaseTypeKind::Float), subst,
                      expr->pos);
                return Type::make<TypeBase>(BaseTypeKind::Void);
            }

            if constexpr (std::is_same_v<T, StaticBind>) {
                infer_expr_impl(node.init, env, names, shadow_counter, subst,
                                gen);
                auto existing = lookup_env(node.name.lexeme, env);
                if (!existing) {
                    env.back().emplace(node.name.lexeme, node.init->type);
                    names.back().insert_or_assign(node.name.lexeme,
                                                  node.name.lexeme);
                } else {
                    unify(*existing, node.init->type, subst, expr->pos);
                }
                return Type::make<TypeBase>(BaseTypeKind::Void);
            }

            if constexpr (std::is_same_v<T, ParamBind>) {
                auto float_type = Type::make<TypeBase>(BaseTypeKind::Float);
                env.back().emplace(node.name.lexeme, float_type);
                names.back().insert_or_assign(node.name.lexeme,
                                              node.name.lexeme);
                return Type::make<TypeBase>(BaseTypeKind::Void);
            }

            if constexpr (std::is_same_v<T, Conditional>) {
                infer_expr_impl(node.condition, env, names, shadow_counter,
                                subst, gen);
                infer_expr_impl(node.then_branch, env, names, shadow_counter,
                                subst, gen);
                if (node.else_branch) {
                    infer_expr_impl(*node.else_branch, env, names,
                                    shadow_counter, subst, gen);
                    unify(node.then_branch->type, (*node.else_branch)->type,
                          subst, expr->pos);
                }
                return apply_subst(subst, node.then_branch->type);
            }

            if constexpr (std::is_same_v<T, ArrayLiteral>) {
                if (node.elements.empty())
                    throw TypeError(
                        "Array literal must have at least one element",
                        expr->pos);
                const auto float_type =
                    Type::make<TypeBase>(BaseTypeKind::Float);
                std::vector<TypePtr> elem_types;
                elem_types.reserve(node.elements.size());
                for (const auto &elem : node.elements) {
                    infer_expr_impl(elem, env, names, shadow_counter, subst,
                                    gen);
                    unify(elem->type, float_type, subst, elem->pos);
                    elem_types.push_back(apply_subst(subst, elem->type));
                }
                return Type::make<TypeArray>(std::move(elem_types));
            }

            if constexpr (std::is_same_v<T, ArrayCtor>) {
                const auto float_type =
                    Type::make<TypeBase>(BaseTypeKind::Float);
                const auto float_to_float =
                    Type::make<TypeFun>(float_type, float_type);
                infer_expr_impl(node.init_fn, env, names, shadow_counter, subst,
                                gen);
                unify(node.init_fn->type, float_to_float, subst, expr->pos);
                return Type::make<TypeArray>(
                    std::vector<TypePtr>(node.size, float_type));
            }

            if constexpr (std::is_same_v<T, Literal>) {
                return Type::make<TypeBase>(BaseTypeKind::Float);
            }

            if constexpr (std::is_same_v<T, UnaryOp>) {
                infer_expr_impl(node.expr, env, names, shadow_counter, subst,
                                gen);
                return apply_subst(subst, node.expr->type);
            }

            if constexpr (std::is_same_v<T, Variable>) {
                auto type = lookup_env(node.name.lexeme, env);
                if (!type)
                    throw TypeError("Unbound variable: " + node.name.lexeme,
                                    expr->pos);
                auto resolved_name = lookup_name(node.name.lexeme, names);
                node.resolved_name =
                    resolved_name ? *resolved_name : node.name.lexeme;
                return apply_subst(subst, *type);
            }

            throw TypeError("Unknown expression type", expr->pos);
        },
        expr->node);
}

} // namespace

auto resolve_type(const Substitution &subst, const TypePtr &type) -> TypePtr {
    auto resolved = apply_subst(subst, type);
    return std::visit(
        [&](const auto &node) -> TypePtr {
            using T = std::decay_t<decltype(node)>;
            if constexpr (std::is_same_v<T, TypeVar>)
                return Type::make<TypeBase>(BaseTypeKind::Float);
            if constexpr (std::is_same_v<T, TypeFun>)
                return Type::make<TypeFun>(resolve_type(subst, node.param),
                                           resolve_type(subst, node.result));
            if constexpr (std::is_same_v<T, TypeArray>) {
                if (node.elements.empty()) return resolved;
                std::vector<TypePtr> new_elements;
                new_elements.reserve(node.elements.size());
                for (const auto &elem : node.elements)
                    new_elements.push_back(resolve_type(subst, elem));
                return Type::make<TypeArray>(std::move(new_elements));
            }
            return resolved;
        },
        resolved->node);
}

void finalize_types(const ExprPtr &expr, const Substitution &subst) {
    if (expr->type) expr->type = resolve_type(subst, expr->type);
    std::visit(
        [&](const auto &node) -> void {
            using T = std::decay_t<decltype(node)>;
            if constexpr (std::is_same_v<T, Bind>)
                finalize_types(node.value, subst);
            if constexpr (std::is_same_v<T, StaticBind>)
                finalize_types(node.init, subst);
            if constexpr (std::is_same_v<T, ParamBind>)
                finalize_types(node.default_val, subst);
            if constexpr (std::is_same_v<T, DelayWrite>)
                finalize_types(node.value, subst);
            if constexpr (std::is_same_v<T, DelayWriteQuiet>) {
                if (node.delay) finalize_types(*node.delay, subst);
                finalize_types(node.value, subst);
            }
            if constexpr (std::is_same_v<T, DelayCtor>)
                finalize_types(node.init_fn, subst);
            if constexpr (std::is_same_v<T, DelayRead>) {
                if (node.delay) finalize_types(*node.delay, subst);
            }
            if constexpr (std::is_same_v<T, ArrayIndex>)
                finalize_types(node.index, subst);
            if constexpr (std::is_same_v<T, ExprIndex>) {
                finalize_types(node.base, subst);
                finalize_types(node.index, subst);
            }
            if constexpr (std::is_same_v<T, OutputWrite>)
                finalize_types(node.value, subst);
            if constexpr (std::is_same_v<T, BinaryOp>) {
                finalize_types(node.left, subst);
                finalize_types(node.right, subst);
            }
            if constexpr (std::is_same_v<T, UnaryOp>)
                finalize_types(node.expr, subst);
            if constexpr (std::is_same_v<T, Conditional>) {
                finalize_types(node.condition, subst);
                finalize_types(node.then_branch, subst);
                if (node.else_branch) finalize_types(*node.else_branch, subst);
            }
            if constexpr (std::is_same_v<T, ArrayLiteral>) {
                for (const auto &elem : node.elements)
                    finalize_types(elem, subst);
            }
            if constexpr (std::is_same_v<T, ArrayCtor>)
                finalize_types(node.init_fn, subst);
            if constexpr (std::is_same_v<T, CodeBlock>) {
                for (const auto &child : node.expressions)
                    finalize_types(child, subst);
            }
            if constexpr (std::is_same_v<T, Call>) {
                finalize_types(node.callee, subst);
                finalize_types(node.argument, subst);
            }
            if constexpr (std::is_same_v<T, Lambda>)
                finalize_types(node.body, subst);
        },
        expr->node);
}
