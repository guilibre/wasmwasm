#include "lower_internal.hpp"

#include "ast/ast.hpp"
#include "ir.hpp"
#include <algorithm>
#include <type_traits>
#include <unordered_set>

namespace lower_detail {

namespace {

auto max_array_index_of(const ExprPtr &e, const std::string &param_name)
    -> std::optional<size_t> {
    std::optional<size_t> best;
    auto consider = [&](size_t idx) -> void {
        if (!best || idx > *best) best = idx;
    };
    std::visit(
        [&](const auto &node) -> auto {
            using T = std::decay_t<decltype(node)>;
            if constexpr (std::is_same_v<T, ArrayIndex>) {
                if (node.name.lexeme == param_name) {
                    if (const auto *lit =
                            std::get_if<Literal>(&node.index->node))
                        consider(std::stoul(std::string(lit->value.lexeme)));
                }
                if (auto sub = max_array_index_of(node.index, param_name))
                    consider(*sub);
            } else if constexpr (std::is_same_v<T, BinaryOp>) {
                if (auto sub = max_array_index_of(node.left, param_name))
                    consider(*sub);
                if (auto sub = max_array_index_of(node.right, param_name))
                    consider(*sub);
            } else if constexpr (std::is_same_v<T, UnaryOp>) {
                if (auto sub = max_array_index_of(node.expr, param_name))
                    consider(*sub);
            } else if constexpr (std::is_same_v<T, Call>) {
                if (auto sub = max_array_index_of(node.callee, param_name))
                    consider(*sub);
                if (auto sub = max_array_index_of(node.argument, param_name))
                    consider(*sub);
            } else if constexpr (std::is_same_v<T, CodeBlock>) {
                for (const auto &child : node.expressions)
                    if (auto sub = max_array_index_of(child, param_name))
                        consider(*sub);
            } else if constexpr (std::is_same_v<T, Bind>) {
                if (auto sub = max_array_index_of(node.value, param_name))
                    consider(*sub);
            } else if constexpr (std::is_same_v<T, Conditional>) {
                if (auto sub = max_array_index_of(node.condition, param_name))
                    consider(*sub);
                if (auto sub = max_array_index_of(node.then_branch, param_name))
                    consider(*sub);
                if (node.else_branch)
                    if (auto sub =
                            max_array_index_of(*node.else_branch, param_name))
                        consider(*sub);
            } else if constexpr (std::is_same_v<T, ArrayLiteral>) {
                for (const auto &elem : node.elements)
                    if (auto sub = max_array_index_of(elem, param_name))
                        consider(*sub);
            } else if constexpr (std::is_same_v<T, ExprIndex>) {
                if (auto sub = max_array_index_of(node.base, param_name))
                    consider(*sub);
                if (auto sub = max_array_index_of(node.index, param_name))
                    consider(*sub);
            }
        },
        e->node);
    return best;
}

auto find_tail_return_size(
    const ExprPtr &e,
    const std::unordered_map<std::string, ParamShape> &param_shapes_by_name,
    const std::unordered_map<std::string, FnInfo> &fns)
    -> std::optional<size_t> {
    if (const auto *lit = std::get_if<ArrayLiteral>(&e->node))
        return lit->elements.size();
    if (const auto *block = std::get_if<CodeBlock>(&e->node)) {
        if (block->expressions.empty()) return std::nullopt;
        return find_tail_return_size(block->expressions.back(),
                                     param_shapes_by_name, fns);
    }
    if (const auto *cond = std::get_if<Conditional>(&e->node)) {
        const auto then_size =
            find_tail_return_size(cond->then_branch, param_shapes_by_name, fns);
        if (!cond->else_branch) return then_size;
        const auto else_size = find_tail_return_size(*cond->else_branch,
                                                     param_shapes_by_name, fns);
        if (then_size) return then_size;
        return else_size;
    }
    if (const auto *var = std::get_if<Variable>(&e->node)) {
        auto it = param_shapes_by_name.find(var->name.lexeme);
        if (it != param_shapes_by_name.end() &&
            it->second.kind == ParamKind::Array)
            return it->second.size;
        return std::nullopt;
    }
    if (std::holds_alternative<Call>(e->node)) {
        std::vector<const ExprPtr *> args;
        const ExprPtr *node = &e;
        while (const auto *call = std::get_if<Call>(&(*node)->node)) {
            args.emplace_back(&call->argument);
            node = &call->callee;
        }
        if (const auto *cv = std::get_if<Variable>(&(*node)->node)) {
            auto fit = fns.find(cv->name.lexeme);
            if (fit != fns.end() && fit->second.arity == args.size() &&
                fit->second.return_type.size() > 1)
                return fit->second.return_type.size();
        }
        return std::nullopt;
    }
    return std::nullopt;
}

} // namespace

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
            if constexpr (std::is_same_v<T, ArrayIndex>) return {};
            if constexpr (std::is_same_v<T, ExprIndex>)
                return free_vars_of(node.base, bound);
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

void Lowerer::register_fn_signature(const std::string &name, const ExprPtr &e) {
    if (fns.contains(name)) return;
    std::vector<std::string> params;
    std::vector<ParamShape> param_shapes;
    const auto call_site_it = call_site_shapes.find(name);
    const auto *observed_shapes = call_site_it != call_site_shapes.end()
                                      ? &call_site_it->second
                                      : nullptr;
    const ExprPtr *body_ptr = &e;
    while (std::holds_alternative<Lambda>((*body_ptr)->node)) {
        const auto &lam = std::get<Lambda>((*body_ptr)->node);
        if (lam.parameter.has_value()) {
            const size_t param_idx = params.size();
            params.emplace_back(lam.parameter->lexeme);
            const auto *fun_type =
                std::get_if<TypeFun>(&(*body_ptr)->type->node);
            const auto *param_type =
                fun_type != nullptr ? &fun_type->param->node : nullptr;
            if (param_type != nullptr &&
                std::holds_alternative<TypeFun>(*param_type)) {
                param_shapes.push_back(
                    {.kind = ParamKind::Closure, .size = closure_max_captures});
            } else if (param_type != nullptr &&
                       std::holds_alternative<TypeArray>(*param_type) &&
                       !std::get<TypeArray>(*param_type).elements.empty()) {
                const auto &arr = std::get<TypeArray>(*param_type);
                param_shapes.push_back(
                    {.kind = ParamKind::Array, .size = arr.elements.size()});
            } else if (observed_shapes != nullptr &&
                       param_idx < observed_shapes->size() &&
                       (*observed_shapes)[param_idx].kind !=
                           ParamKind::Scalar) {
                param_shapes.push_back((*observed_shapes)[param_idx]);
            } else {
                param_shapes.push_back({.kind = ParamKind::Scalar, .size = 1});
            }
        }
        body_ptr = &lam.body;
    }
    const ExprPtr &body = *body_ptr;

    for (size_t i = 0; i < params.size(); ++i) {
        if (param_shapes[i].kind != ParamKind::Scalar) continue;
        if (auto max_idx = max_array_index_of(body, params[i]))
            param_shapes[i] = {.kind = ParamKind::Array, .size = *max_idx + 1};
    }

    const std::unordered_set<std::string> bound(params.begin(), params.end());
    const auto fv_set = free_vars_of(body, bound);
    std::vector<std::string> fv_vec(fv_set.begin(), fv_set.end());
    std::ranges::sort(fv_vec);

    auto ret_types = ir_types_of(body->type);
    if (ret_types.size() <= 1) {
        std::unordered_map<std::string, ParamShape> param_shapes_by_name;
        for (size_t i = 0; i < params.size(); ++i)
            param_shapes_by_name[params[i]] = param_shapes[i];
        if (auto tail_size =
                find_tail_return_size(body, param_shapes_by_name, fns);
            tail_size && *tail_size > 1)
            ret_types.assign(*tail_size, IRType::Float);
    }

    fns[name] = FnInfo{
        .free_vars = fv_vec,
        .return_type = ret_types,
        .arity = params.size(),
        .param_shapes = param_shapes,
    };
    fn_indices.emplace(name, fn_indices.size());
}

void Lowerer::lift(const std::string &name, const ExprPtr &e) {
    register_fn_signature(name, e);
    if (lowered_fns.contains(name)) return;
    lowered_fns.insert(name);

    const auto &info = fns.at(name);
    const auto &param_shapes = info.param_shapes;

    std::vector<std::string> params;
    const ExprPtr *body_ptr = &e;
    while (std::holds_alternative<Lambda>((*body_ptr)->node)) {
        const auto &lam = std::get<Lambda>((*body_ptr)->node);
        if (lam.parameter.has_value())
            params.emplace_back(lam.parameter->lexeme);
        body_ptr = &lam.body;
    }
    const ExprPtr &body = *body_ptr;

    IRFunction fn;
    fn.name = name;
    fn.return_type = info.return_type;
    for (size_t i = 0; i < params.size(); ++i) {
        if (param_shapes[i].kind == ParamKind::Closure) {
            for (const auto &slot : closure_slot_names(params[i]))
                fn.params.emplace_back(slot, IRType::Float);
        } else if (param_shapes[i].kind == ParamKind::Array) {
            for (size_t k = 0; k < param_shapes[i].size; ++k)
                fn.params.emplace_back(params[i] + "__" + std::to_string(k),
                                       IRType::Float);
        } else {
            fn.params.emplace_back(params[i], IRType::Float);
        }
    }
    for (const auto &fv : info.free_vars)
        fn.params.emplace_back(fv, locals.contains(fv) ? locals.at(fv)
                                                       : IRType::Float);

    std::vector<std::pair<std::string, IRType>> body_params;
    body_params.reserve(params.size());
    for (size_t i = 0; i < params.size(); ++i) {
        if (param_shapes[i].kind == ParamKind::Closure) {
            closure_params.insert(params[i]);
            for (const auto &slot : closure_slot_names(params[i]))
                body_params.emplace_back(slot, IRType::Float);
        } else if (param_shapes[i].kind == ParamKind::Array) {
            std::vector<std::string> elem_names;
            elem_names.reserve(param_shapes[i].size);
            for (size_t k = 0; k < param_shapes[i].size; ++k)
                elem_names.push_back(params[i] + "__" + std::to_string(k));
            for (const auto &elem : elem_names)
                body_params.emplace_back(elem, IRType::Float);
            array_env[params[i]] = std::move(elem_names);
        } else {
            body_params.emplace_back(params[i], IRType::Float);
        }
    }
    const auto saved_fn_name = cur_fn_name;
    cur_fn_name = name;
    lower_fn_body(fn, body_params, body);
    cur_fn_name = saved_fn_name;

    mod.functions.emplace_back(std::move(fn));
}

} // namespace lower_detail
