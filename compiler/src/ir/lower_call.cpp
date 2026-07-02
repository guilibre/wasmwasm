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

auto Lowerer::lower_call(const ExprPtr &e) -> std::optional<IRValue> {
    const auto [callee_node, arg_ptrs] = flatten_calls(e);

    if (const auto *cv = std::get_if<Variable>(&(*callee_node)->node)) {
        if (cv->name.lexeme == "foldr") return lower_foldr(arg_ptrs);
    }

    std::vector<IRValue> arg_vals;
    arg_vals.reserve(arg_ptrs.size());
    for (const auto *ap : arg_ptrs) {
        auto v = lower_expr(*ap);
        if (!v) throw std::runtime_error("void argument in call");
        arg_vals.emplace_back(*v);
    }

    if (const auto *var = std::get_if<Variable>(&(*callee_node)->node)) {
        const auto &name = var->name.lexeme;

        if (std::ranges::contains(math_builtins, name)) {
            auto r = tmp();
            emit(make_scalar_call(r, "wasmwasm_" + name, arg_vals,
                                  IRType::Float));
            return IRLocalRef{r};
        }

        if (fns.contains(name)) {
            const auto &info = fns.at(name);
            if (info.return_type.size() > 1)
                throw std::runtime_error("'" + name +
                                         "' returns an array; call it via a "
                                         "bind (x = " +
                                         name +
                                         " ...) before indexing with @[i]");
            for (const auto &fv : info.free_vars)
                arg_vals.emplace_back(IRLocalRef{fv});

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

        if (locals.contains(name)) {
            auto res = tmp();
            emit(IRAssign{
                .result = res,
                .value = IRLiteral{0.0},
                .type = IRType::Float,
            });
            for (const auto &[fn_name, fn_info] : fns) {
                if (fn_info.arity != arg_vals.size()) continue;
                if (fn_info.return_type.size() != 1) continue;
                auto cond = tmp();
                emit(IRBinOp{
                    .result = cond,
                    .op = Operation::Eq,
                    .left = IRLocalRef{name},
                    .right = IRLiteral{(double)fn_indices.at(fn_name)},
                });
                auto call_r = tmp();
                auto call_args = arg_vals;
                for (const auto &fv : fn_info.free_vars)
                    call_args.emplace_back(IRLocalRef{fv});
                std::vector<IRInstr> then_instrs;
                const auto callee =
                    fn_info.is_math ? "wasmwasm_" + fn_name : fn_name;
                then_instrs.emplace_back(make_scalar_call(
                    call_r, callee, call_args, fn_info.return_type[0]));
                then_instrs.emplace_back(IRAssign{
                    .result = res,
                    .value = IRLocalRef{call_r},
                    .type = IRType::Float,
                });
                emit(IRIf{
                    .condition = IRLocalRef{cond},
                    .body = std::make_shared<IRIfBody>(std::move(then_instrs),
                                                       std::vector<IRInstr>{}),
                });
            }
            return IRLocalRef{res};
        }
        throw std::runtime_error("Unknown function: " + name);
    }

    if (std::holds_alternative<Lambda>((*callee_node)->node)) {
        std::vector<std::string> arg_locals;
        arg_locals.reserve(arg_vals.size());
        for (const auto &av : arg_vals) {
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
