#include "lower_internal.hpp"

#include "ast/ast.hpp"
#include "ir.hpp"
#include <algorithm>
#include <type_traits>
#include <unordered_set>

namespace lower_detail {

auto Lowerer::free_vars_of(const ExprPtr &e,
                           std::unordered_set<std::string> bound)
    -> std::unordered_set<std::string> {
    return std::visit(
        [&](const auto &node) -> std::unordered_set<std::string> {
            using T = std::decay_t<decltype(node)>;

            if constexpr (std::is_same_v<T, Variable>) {
                const auto &name = node.resolved_name.empty()
                                       ? node.name.lexeme
                                       : node.resolved_name;
                if (!bound.contains(name) && !is_special(node.name.lexeme) &&
                    locals.contains(name))
                    return {name};
                return {};
            }
            if constexpr (std::is_same_v<T, BinaryOp>) {
                auto l = free_vars_of(node.left, bound);
                const auto r = free_vars_of(node.right, bound);
                l.insert(r.begin(), r.end());
                return l;
            }
            if constexpr (std::is_same_v<T, UnaryOp>)
                return free_vars_of(node.expr, bound);
            if constexpr (std::is_same_v<T, Call>) {
                auto l = free_vars_of(node.callee, bound);
                const auto r = free_vars_of(node.argument, bound);
                l.insert(r.begin(), r.end());
                return l;
            }
            if constexpr (std::is_same_v<T, Lambda>) {
                auto b2 = bound;
                if (node.parameter.has_value())
                    b2.insert(node.parameter->lexeme);
                return free_vars_of(node.body, b2);
            }
            if constexpr (std::is_same_v<T, CodeBlock>) {
                std::unordered_set<std::string> result;
                auto inner = bound;
                for (const auto &expr : node.expressions) {
                    auto fv = free_vars_of(expr, inner);
                    result.insert(fv.begin(), fv.end());
                    if (const auto *b = std::get_if<Bind>(&expr->node))
                        inner.insert(b->resolved_name.empty()
                                         ? b->name.lexeme
                                         : b->resolved_name);
                }
                return result;
            }
            if constexpr (std::is_same_v<T, Bind>)
                return free_vars_of(node.value, bound);
            if constexpr (std::is_same_v<T, Conditional>) {
                auto fv = free_vars_of(node.condition, bound);
                const auto ft = free_vars_of(node.then_branch, bound);
                fv.insert(ft.begin(), ft.end());
                if (node.else_branch) {
                    const auto fe = free_vars_of(*node.else_branch, bound);
                    fv.insert(fe.begin(), fe.end());
                }
                return fv;
            }
            if constexpr (std::is_same_v<T, DelayRead>) return {};
            if constexpr (std::is_same_v<T, DelayWrite>)
                return free_vars_of(node.value, bound);
            if constexpr (std::is_same_v<T, InputRead>) return {};
            if constexpr (std::is_same_v<T, OutputWrite>)
                return free_vars_of(node.value, bound);
            if constexpr (std::is_same_v<T, StaticBind>) return {};
            if constexpr (std::is_same_v<T, ParamBind>) return {};
            if constexpr (std::is_same_v<T, ArrayLiteral>) {
                std::unordered_set<std::string> result;
                for (const auto &elem : node.elements) {
                    const auto fv = free_vars_of(elem, bound);
                    result.insert(fv.begin(), fv.end());
                }
                return result;
            }
            if constexpr (std::is_same_v<T, ArrayCtor>)
                return free_vars_of(node.init_fn, bound);

            return {};
        },
        e->node);
}

void Lowerer::lift(const std::string &name, const ExprPtr &e) {
    std::vector<std::string> params;
    const ExprPtr *body_ptr = &e;
    while (std::holds_alternative<Lambda>((*body_ptr)->node)) {
        const auto &lam = std::get<Lambda>((*body_ptr)->node);
        if (lam.parameter.has_value())
            params.emplace_back(lam.parameter->lexeme);
        body_ptr = &lam.body;
    }
    const ExprPtr &body = *body_ptr;

    const std::unordered_set<std::string> bound(params.begin(), params.end());
    const auto fv_set = free_vars_of(body, bound);
    std::vector<std::string> fv_vec(fv_set.begin(), fv_set.end());
    std::ranges::sort(fv_vec);

    const auto ret_types = ir_types_of(body->type);

    fns[name] = FnInfo{
        .free_vars = fv_vec,
        .return_type = ret_types,
        .arity = params.size(),
    };
    fn_indices.emplace(name, fn_indices.size());

    IRFunction fn;
    fn.name = name;
    fn.return_type = ret_types;
    for (const auto &p : params) fn.params.emplace_back(p, IRType::Float);
    for (const auto &fv : fv_vec)
        fn.params.emplace_back(fv, locals.contains(fv) ? locals.at(fv)
                                                       : IRType::Float);

    std::vector<std::pair<std::string, IRType>> body_params;
    body_params.reserve(params.size());
    for (const auto &p : params) body_params.emplace_back(p, IRType::Float);
    lower_fn_body(fn, body_params, body);

    mod.functions.emplace_back(std::move(fn));
}

} // namespace lower_detail
