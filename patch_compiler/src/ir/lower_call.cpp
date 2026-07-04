#include "lower_internal.hpp"

#include "ast/ast.hpp"
#include "builtins.hpp"
#include "ir.hpp"
#include <algorithm>
#include <stdexcept>

namespace lower_detail {

auto Lowerer::flatten_calls(const ExprPtr &e)
    -> std::pair<const ExprPtr *, std::vector<const ExprPtr *>> {
    std::vector<const ExprPtr *> args;
    const ExprPtr *node = &e;
    while (const auto *call = std::get_if<Call>(&(*node)->node)) {
        args.emplace_back(&call->argument);
        node = &call->callee;
    }
    std::ranges::reverse(args);
    return {node, args};
}

auto Lowerer::inline_lambda_body(const ExprPtr &lam_expr,
                                 const std::vector<std::string> &arg_locals)
    -> std::optional<IRValue> {
    const ExprPtr *ptr = &lam_expr;
    std::vector<std::string> params;
    while (std::holds_alternative<Lambda>((*ptr)->node)) {
        const auto &lam = std::get<Lambda>((*ptr)->node);
        if (lam.parameter.has_value())
            params.emplace_back(lam.parameter->lexeme);
        ptr = &lam.body;
    }
    for (size_t k = 0; k < params.size() && k < arg_locals.size(); ++k)
        inline_alias[params[k]] = arg_locals[k];
    auto result = lower_expr(*ptr);
    for (const auto &p : params) inline_alias.erase(p);
    return result;
}

auto Lowerer::lower_closure_arg(const ExprPtr &ap) -> std::vector<IRValue> {
    const auto lam_it = lambda_arg_names.find(ap.get());
    const auto fn_name = lam_it != lambda_arg_names.end()
                             ? lam_it->second
                             : "$lam$" + std::to_string(tmp_n++);
    if (!lowered_fns.contains(fn_name)) lift(fn_name, ap);
    if (!fns.contains(fn_name))
        throw std::runtime_error("internal error: '" + fn_name +
                                 "' not registered after lift");
    const auto &info = fns.at(fn_name);
    if (info.free_vars.size() > closure_max_captures)
        throw std::runtime_error(
            "lambda captures too many free variables (max " +
            std::to_string(closure_max_captures) + ")");

    std::vector<IRValue> slots;
    slots.reserve(closure_max_captures + 1);
    auto idx_t = tmp();
    define(idx_t, IRType::Float);
    emit(IRAssign{.result = idx_t,
                  .value = IRLiteral{(double)fn_indices.at(fn_name)},
                  .type = IRType::Float});
    slots.emplace_back(IRLocalRef{idx_t});
    for (size_t i = 0; i < closure_max_captures; ++i) {
        auto cap_t = tmp();
        define(cap_t, IRType::Float);
        const auto cap_val = i < info.free_vars.size()
                                 ? IRValue{IRLocalRef{info.free_vars[i]}}
                                 : IRValue{IRLiteral{0.0}};
        emit(
            IRAssign{.result = cap_t, .value = cap_val, .type = IRType::Float});
        slots.emplace_back(IRLocalRef{cap_t});
    }
    return slots;
}

auto Lowerer::lower_call(const ExprPtr &e) -> std::optional<IRValue> {
    std::optional<IRValue> unrolled;
    if (try_unroll_bounded_recursion(e, unrolled)) return unrolled;

    const auto [callee_node, arg_ptrs] = flatten_calls(e);

    if (const auto *cv = std::get_if<Variable>(&(*callee_node)->node)) {
        if (cv->name.lexeme == "foldr") return lower_foldr(arg_ptrs);
    }

    const FnInfo *callee_info = nullptr;
    if (const auto *cv = std::get_if<Variable>(&(*callee_node)->node);
        cv != nullptr && fns.contains(cv->name.lexeme))
        callee_info = &fns.at(cv->name.lexeme);

    auto param_expects_closure = [&](size_t i) -> bool {
        return callee_info != nullptr && i < callee_info->param_shapes.size() &&
               callee_info->param_shapes[i].kind == ParamKind::Closure;
    };

    std::vector<std::vector<IRValue>> arg_slots;
    arg_slots.reserve(arg_ptrs.size());
    for (size_t arg_idx = 0; arg_idx < arg_ptrs.size(); ++arg_idx) {
        const auto *ap = arg_ptrs[arg_idx];
        while (const auto *block = std::get_if<CodeBlock>(&(*ap)->node)) {
            if (block->expressions.empty()) break;
            ap = &block->expressions.back();
        }
        if (std::holds_alternative<Lambda>((*ap)->node)) {
            arg_slots.emplace_back(lower_closure_arg(*ap));
            continue;
        }
        if (const auto *v = std::get_if<Variable>(&(*ap)->node); v != nullptr) {
            if (closure_params.contains(v->name.lexeme) &&
                !fns.contains(v->name.lexeme)) {
                std::vector<IRValue> slots;
                for (const auto &slot : closure_slot_names(v->name.lexeme))
                    slots.emplace_back(IRLocalRef{slot});
                arg_slots.emplace_back(std::move(slots));
                continue;
            }
            if (array_env.contains(v->name.lexeme) &&
                !fns.contains(v->name.lexeme)) {
                arg_slots.emplace_back(lower_tail_as_array(
                    *ap, array_env.at(v->name.lexeme).size()));
                continue;
            }
            if (fns.contains(v->name.lexeme) &&
                (fns.at(v->name.lexeme).arity > 0 ||
                 param_expects_closure(arg_idx))) {
                const auto &info = fns.at(v->name.lexeme);
                if (!fn_indices.contains(v->name.lexeme))
                    throw std::runtime_error(
                        "internal error: '" + v->name.lexeme +
                        "' registered in fns but not fn_indices");
                std::vector<IRValue> slots;
                slots.reserve(closure_max_captures + 1);
                slots.emplace_back(
                    IRLiteral{(double)fn_indices.at(v->name.lexeme)});
                for (size_t i = 0; i < closure_max_captures; ++i)
                    slots.emplace_back(
                        i < info.free_vars.size()
                            ? IRValue{IRLocalRef{info.free_vars[i]}}
                            : IRValue{IRLiteral{0.0}});
                arg_slots.emplace_back(std::move(slots));
                continue;
            }
        }
        if (const auto elem_types = ir_types_of((*ap)->type);
            elem_types.size() > 1) {
            arg_slots.emplace_back(lower_tail_as_array(*ap, elem_types.size()));
            continue;
        }
        if (std::holds_alternative<Call>((*ap)->node)) {
            const auto [inner_callee, inner_args] = flatten_calls(*ap);
            if (std::holds_alternative<Variable>((*inner_callee)->node)) {
                std::vector<size_t> candidate_arities;
                for (const auto &[fn_name, fn_info] : fns) {
                    if (fn_info.arity != inner_args.size()) continue;
                    if (fn_info.return_type.size() <= 1) continue;
                    if (!std::ranges::contains(candidate_arities,
                                               fn_info.return_type.size()))
                        candidate_arities.push_back(fn_info.return_type.size());
                }
                if (candidate_arities.size() == 1) {
                    arg_slots.emplace_back(
                        lower_tail_as_array(*ap, candidate_arities[0]));
                    continue;
                }
            }
        }
        auto v = lower_expr(*ap);
        if (!v) throw std::runtime_error("void argument in call");
        arg_slots.emplace_back(std::vector<IRValue>{*v});
    }

    auto flatten_args = [](const std::vector<std::vector<IRValue>> &slots)
        -> std::vector<IRValue> {
        std::vector<IRValue> out;
        for (const auto &s : slots) out.insert(out.end(), s.begin(), s.end());
        return out;
    };

    if (const auto *var = std::get_if<Variable>(&(*callee_node)->node)) {
        const auto &name = var->name.lexeme;

        if (std::ranges::contains(math_builtins, name)) {
            auto r = tmp();
            emit(make_scalar_call(r, "wasmwasm_" + name,
                                  flatten_args(arg_slots), IRType::Float));
            return IRLocalRef{r};
        }

        if (fns.contains(name)) {
            const auto &info = fns.at(name);
            auto arg_vals = flatten_args(arg_slots);
            for (const auto &fv : info.free_vars)
                arg_vals.emplace_back(IRLocalRef{fv});

            if (info.return_type.size() > 1) {
                std::vector<std::string> results;
                results.reserve(info.return_type.size());
                for (auto rt : info.return_type) {
                    auto t = tmp();
                    define(t, rt);
                    results.push_back(t);
                }
                emit(IRCall{
                    .result = results,
                    .callee = name,
                    .args = arg_vals,
                    .result_type = info.return_type,
                });
                const auto tmp_name = "$ret$" + std::to_string(tmp_n++);
                array_env[tmp_name] = std::move(results);
                return IRLocalRef{tmp_name};
            }

            if (info.return_type.empty()) {
                emit(IRCall{
                    .result = {},
                    .callee = name,
                    .args = arg_vals,
                    .result_type = {},
                });
                return std::nullopt;
            }
            const auto r = tmp();
            emit(make_scalar_call(r, name, arg_vals, info.return_type[0]));
            return IRLocalRef{r};
        }

        if (locals.contains(name) || closure_params.contains(name)) {
            std::vector<size_t> multi_result_arities;
            for (const auto &[fn_name, fn_info] : fns) {
                if (fn_info.arity != arg_slots.size()) continue;
                if (fn_info.return_type.size() <= 1) continue;
                if (!std::ranges::contains(multi_result_arities,
                                           fn_info.return_type.size()))
                    multi_result_arities.push_back(fn_info.return_type.size());
            }
            const auto expected_types = ir_types_of(e->type);
            const size_t expected_arity =
                multi_result_arities.size() == 1
                    ? multi_result_arities[0]
                    : (expected_types.empty() ? 1 : expected_types.size());

            std::vector<std::string> results;
            results.reserve(expected_arity);
            for (size_t i = 0; i < expected_arity; ++i) {
                auto t = tmp();
                define(t, IRType::Float);
                emit(IRAssign{.result = t,
                              .value = IRLiteral{0.0},
                              .type = IRType::Float});
                results.push_back(t);
            }

            for (const auto &[fn_name, fn_info] : fns) {
                if (fn_info.arity != arg_slots.size()) continue;
                const auto candidate_arity = fn_info.return_type.empty()
                                                 ? 1
                                                 : fn_info.return_type.size();
                if (candidate_arity != expected_arity) continue;
                if (!fn_indices.contains(fn_name))
                    throw std::runtime_error(
                        "internal error: '" + fn_name +
                        "' registered in fns but not fn_indices");

                auto cond = tmp();
                emit(IRBinOp{
                    .result = cond,
                    .op = Operation::Eq,
                    .left = IRLocalRef{name + "$idx"},
                    .right = IRLiteral{(double)fn_indices.at(fn_name)},
                });
                std::vector<IRValue> call_args;
                for (const auto &slots : arg_slots)
                    call_args.insert(call_args.end(), slots.begin(),
                                     slots.end());
                for (const auto &fv : fn_info.free_vars)
                    call_args.emplace_back(IRLocalRef{fv});

                std::vector<IRInstr> then_instrs;
                auto *saved = cur;
                cur = &then_instrs;
                const auto callee =
                    fn_info.is_math ? "wasmwasm_" + fn_name : fn_name;
                if (fn_info.return_type.size() > 1) {
                    std::vector<std::string> call_results;
                    call_results.reserve(fn_info.return_type.size());
                    for (auto rt : fn_info.return_type) {
                        auto t = tmp();
                        define(t, rt);
                        call_results.push_back(t);
                    }
                    emit(IRCall{
                        .result = call_results,
                        .callee = callee,
                        .args = call_args,
                        .result_type = fn_info.return_type,
                    });
                    for (size_t i = 0; i < results.size(); ++i)
                        then_instrs.emplace_back(IRAssign{
                            .result = results[i],
                            .value = IRLocalRef{call_results[i]},
                            .type = IRType::Float,
                        });
                } else {
                    auto call_r = tmp();
                    define(call_r, IRType::Float);
                    then_instrs.emplace_back(make_scalar_call(
                        call_r, callee, call_args, fn_info.return_type[0]));
                    then_instrs.emplace_back(IRAssign{
                        .result = results[0],
                        .value = IRLocalRef{call_r},
                        .type = IRType::Float,
                    });
                }
                cur = saved;
                emit(IRIf{
                    .condition = IRLocalRef{cond},
                    .body = std::make_shared<IRIfBody>(std::move(then_instrs),
                                                       std::vector<IRInstr>{}),
                });
            }

            if (expected_arity > 1) {
                const auto tmp_name = "$ret$" + std::to_string(tmp_n++);
                array_env[tmp_name] = results;
                return IRLocalRef{tmp_name};
            }
            return IRLocalRef{results[0]};
        }
        throw std::runtime_error("Unknown function: " + name);
    }

    if (std::holds_alternative<Lambda>((*callee_node)->node)) {
        std::vector<std::string> arg_locals;
        arg_locals.reserve(arg_slots.size());
        for (const auto &slots : arg_slots) {
            const auto &av = slots.front();
            if (const auto *lr = std::get_if<IRLocalRef>(&av)) {
                arg_locals.emplace_back(lr->name);
            } else {
                auto t = tmp();
                define(t, IRType::Float);
                emit(IRAssign{
                    .result = t,
                    .value = av,
                    .type = IRType::Float,
                });
                arg_locals.emplace_back(t);
            }
        }
        return inline_lambda_body(*callee_node, arg_locals);
    }

    throw std::runtime_error("Dynamic dispatch not supported");
}

} // namespace lower_detail
