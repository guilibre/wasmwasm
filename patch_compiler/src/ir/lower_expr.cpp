#include "lower_internal.hpp"

#include "ast/ast.hpp"
#include "ir.hpp"
#include <numbers>
#include <stdexcept>
#include <type_traits>

namespace lower_detail {

void Lowerer::lower_fn_body(
    IRFunction &fn, const std::vector<std::pair<std::string, IRType>> &params,
    const ExprPtr &body) {
    auto *saved_cur = cur;
    const auto saved_locals = locals;
    cur = &fn.body;
    for (const auto &[name, type] : params) locals[name] = type;
    if (fn.return_type.size() > 1) {
        auto values = lower_tail_as_array(body, fn.return_type.size());
        emit(IRReturn{std::move(values)});
    } else {
        auto result = lower_expr(body);
        if (result && !fn.return_type.empty())
            emit(IRReturn{std::vector<IRValue>{*result}});
    }
    cur = saved_cur;
    locals = saved_locals;
}

auto Lowerer::lower_expr(const ExprPtr &e) -> std::optional<IRValue> {
    return std::visit(
        [&](const auto &node) -> std::optional<IRValue> {
            using T = std::decay_t<decltype(node)>;

            if constexpr (std::is_same_v<T, Literal>)
                return IRLiteral{std::stod(std::string(node.value.lexeme))};

            if constexpr (std::is_same_v<T, Variable>) {
                const auto &name = node.name.lexeme;

                if (name == "PI") return IRLiteral{std::numbers::pi};

                if (name == "SAMPLE_RATE")
                    return emit_global_read(name, IRType::Float);

                if (name == "die") {
                    emit(IRDie{});
                    return std::nullopt;
                }

                if (inline_alias.contains(name))
                    return IRLocalRef{inline_alias.at(name)};

                if (param_names.contains(name) && !locals.contains(name)) {
                    auto r = tmp();
                    emit(IRParamRead{.result = r, .name = name});
                    return IRLocalRef{r};
                }

                if (statics.contains(name) && !locals.contains(name)) {
                    auto r = tmp();
                    emit(IRStaticRead{r, name});
                    return IRLocalRef{r};
                }

                if (array_env.contains(name))
                    throw std::runtime_error(
                        "'" + name +
                        "' is an array; use foldr to operate on it");

                if (fns.contains(name)) {
                    const auto &info = fns.at(name);
                    if (info.arity == 0) {
                        if (info.return_type.size() > 1)
                            throw std::runtime_error(
                                "'" + name +
                                "' returns an array; call it via a "
                                "bind (x = " +
                                name + ") before indexing with x[i]");
                        std::vector<IRValue> call_args;
                        call_args.reserve(info.free_vars.size());
                        for (const auto &fv : info.free_vars)
                            call_args.emplace_back(IRLocalRef{fv});
                        if (info.return_type.empty()) {
                            emit(IRCall{.result = {},
                                        .callee = name,
                                        .args = call_args,
                                        .result_type = {}});
                            return std::nullopt;
                        }
                        auto r = tmp();
                        emit(IRCall{.result = {r},
                                    .callee = name,
                                    .args = call_args,
                                    .result_type = info.return_type});
                        return IRLocalRef{r};
                    }
                    if (!fn_indices.contains(name))
                        throw std::runtime_error(
                            "internal error: '" + name +
                            "' registered in fns but not fn_indices");
                    return IRLiteral{(double)fn_indices.at(name)};
                }

                return IRLocalRef{
                    node.resolved_name.empty() ? name : node.resolved_name};
            }

            if constexpr (std::is_same_v<T, DelayRead>) {
                const auto &name = node.name.lexeme;
                if (array_env.contains(name))
                    throw std::runtime_error("'" + name +
                                             "' is an array; use " + name +
                                             "[i] to access an element");
                if (!bufs.contains(name))
                    throw std::runtime_error("@" + name + " is not a delay");
                auto r = tmp();
                if (node.delay) {
                    auto d = lower_expr(*node.delay);
                    if (!d)
                        throw std::runtime_error("delay expression is void");
                    auto dr = tmp();
                    emit(IRAssign{
                        .result = dr, .value = *d, .type = IRType::Float});
                    emit(IRDelayReadDelayed{
                        .result = r, .delay = name, .delay_ref = dr});
                } else {
                    emit(IRDelayRead{.result = r, .delay = name});
                }
                return IRLocalRef{r};
            }

            if constexpr (std::is_same_v<T, ArrayIndex>) {
                const auto &name = node.name.lexeme;
                if (!array_env.contains(name))
                    throw std::runtime_error("'" + node.name.lexeme +
                                             "' is not an array");
                const auto *idx_lit = std::get_if<Literal>(&node.index->node);
                if (!idx_lit)
                    throw std::runtime_error(
                        "Array index in " + node.name.lexeme +
                        "[i] must be a compile-time integer literal");
                const size_t index =
                    std::stoul(std::string(idx_lit->value.lexeme));
                return read_array_elem(name, index);
            }

            if constexpr (std::is_same_v<T, ExprIndex>) {
                const auto *idx_lit = std::get_if<Literal>(&node.index->node);
                if (!idx_lit)
                    throw std::runtime_error(
                        "Array index must be a compile-time integer literal");
                const size_t index =
                    std::stoul(std::string(idx_lit->value.lexeme));

                const ExprPtr *base_ptr = &node.base;
                while (const auto *block =
                           std::get_if<CodeBlock>(&(*base_ptr)->node)) {
                    if (block->expressions.empty()) break;
                    base_ptr = &block->expressions.back();
                }

                size_t n = 0;
                if (std::holds_alternative<Call>((*base_ptr)->node)) {
                    const auto [callee_node, arg_ptrs] =
                        flatten_calls(*base_ptr);
                    if (const auto *cv =
                            std::get_if<Variable>(&(*callee_node)->node);
                        cv != nullptr && fns.contains(cv->name.lexeme)) {
                        n = fns.at(cv->name.lexeme).return_type.size();
                    }
                }
                if (n == 0) n = ir_types_of(node.base->type).size();

                auto values = lower_tail_as_array(node.base, n);
                if (index >= values.size())
                    throw std::runtime_error("Array index out of bounds");
                return values[index];
            }

            if constexpr (std::is_same_v<T, DelayWrite>) {
                const auto &target = node.target.lexeme;
                const auto val = lower_expr(node.value);
                if (!val) throw std::runtime_error("assigning void to delay");
                if (!bufs.contains(target))
                    throw std::runtime_error(target + " is not a delay");
                emit(IRDelayWrite{.delay = target, .value = *val});
                return std::nullopt;
            }

            if constexpr (std::is_same_v<T, DelayWriteQuiet>) {
                const auto &target = node.target.lexeme;
                const auto val = lower_expr(node.value);
                if (!val) throw std::runtime_error("assigning void to delay");
                if (!bufs.contains(target))
                    throw std::runtime_error(target + " is not a delay");
                std::optional<std::string> dr;
                if (node.delay) {
                    auto d = lower_expr(*node.delay);
                    if (!d)
                        throw std::runtime_error("delay expression is void");
                    auto drtmp = tmp();
                    emit(IRAssign{
                        .result = drtmp, .value = *d, .type = IRType::Float});
                    dr = drtmp;
                }
                emit(IRDelayWriteQuiet{
                    .delay = target, .value = *val, .delay_ref = dr});
                return std::nullopt;
            }

            if constexpr (std::is_same_v<T, InputRead>) {
                auto r = tmp();
                emit(IRInputRead{.result = r, .index = node.index});
                return IRLocalRef{r};
            }

            if constexpr (std::is_same_v<T, OutputWrite>) {
                const auto val = lower_expr(node.value);
                if (!val) throw std::runtime_error("assigning void to OUT[n]");
                emit(IROutputWrite{.index = node.index, .value = *val});
                return std::nullopt;
            }

            if constexpr (std::is_same_v<T, UnaryOp>) {
                const auto val = lower_expr(node.expr);
                if (!val) throw std::runtime_error("unary op on void");
                if (node.op == Operation::Not) {
                    auto ca = tmp();
                    emit(make_scalar_call(ca, "wasmwasm_clip", {*val},
                                          IRType::Float));
                    auto r = tmp();
                    emit(IRBinOp{
                        .result = r,
                        .op = Operation::Sub,
                        .left = IRLiteral{1.0},
                        .right = IRLocalRef{ca},
                    });
                    return IRLocalRef{r};
                }
                auto r = tmp();
                emit(IRUnaryNeg{r, *val});
                return IRLocalRef{r};
            }

            if constexpr (std::is_same_v<T, BinaryOp>) {
                const auto l = lower_expr(node.left);
                const auto r = lower_expr(node.right);
                if (!l || !r) throw std::runtime_error("binary op on void");
                if (node.op == Operation::And) {
                    auto ca = tmp();
                    emit(make_scalar_call(ca, "wasmwasm_clip", {*l},
                                          IRType::Float));
                    auto cb = tmp();
                    emit(make_scalar_call(cb, "wasmwasm_clip", {*r},
                                          IRType::Float));
                    auto res = tmp();
                    emit(IRBinOp{
                        .result = res,
                        .op = Operation::Mul,
                        .left = IRLocalRef{ca},
                        .right = IRLocalRef{cb},
                    });
                    return IRLocalRef{res};
                }
                if (node.op == Operation::Or) {
                    auto ca = tmp();
                    emit(make_scalar_call(ca, "wasmwasm_clip", {*l},
                                          IRType::Float));
                    auto cb = tmp();
                    emit(make_scalar_call(cb, "wasmwasm_clip", {*r},
                                          IRType::Float));
                    auto one_minus_cb = tmp();
                    emit(IRBinOp{
                        .result = one_minus_cb,
                        .op = Operation::Sub,
                        .left = IRLiteral{1.0},
                        .right = IRLocalRef{cb},
                    });
                    auto prod = tmp();
                    emit(IRBinOp{
                        .result = prod,
                        .op = Operation::Mul,
                        .left = IRLocalRef{ca},
                        .right = IRLocalRef{one_minus_cb},
                    });
                    auto res = tmp();
                    emit(IRBinOp{
                        .result = res,
                        .op = Operation::Add,
                        .left = IRLocalRef{prod},
                        .right = IRLocalRef{cb},
                    });
                    return IRLocalRef{res};
                }
                if (node.op == Operation::Pow) {
                    auto res = tmp();
                    emit(make_scalar_call(res, "wasmwasm_pow", {*l, *r},
                                          IRType::Float));
                    return IRLocalRef{res};
                }
                auto res = tmp();
                emit(IRBinOp{res, node.op, *l, *r});
                return IRLocalRef{res};
            }

            if constexpr (std::is_same_v<T, Bind>) return lower_bind(node);

            if constexpr (std::is_same_v<T, StaticBind>)
                return lower_static_bind(node);

            if constexpr (std::is_same_v<T, ParamBind>)
                return lower_param_bind(node);

            if constexpr (std::is_same_v<T, CodeBlock>) {
                std::optional<IRValue> last;
                for (const auto &expr : node.expressions)
                    last = lower_expr(expr);
                return last;
            }

            if constexpr (std::is_same_v<T, Call>) return lower_call(e);

            if constexpr (std::is_same_v<T, Conditional>) {
                const auto cond = lower_expr(node.condition);
                if (!cond)
                    throw std::runtime_error("conditional condition is void");
                std::vector<IRInstr> then_instrs;
                std::vector<IRInstr> else_instrs;
                auto *saved = cur;
                cur = &then_instrs;
                auto then_val = lower_expr(node.then_branch);
                cur = &else_instrs;
                std::optional<IRValue> else_val;
                if (node.else_branch) else_val = lower_expr(*node.else_branch);
                cur = saved;
                if (then_val || else_val) {
                    auto res = tmp();
                    define(res, IRType::Float);
                    if (then_val)
                        then_instrs.emplace_back(IRAssign{
                            res,
                            *then_val,
                            IRType::Float,
                        });
                    if (else_val)
                        else_instrs.emplace_back(IRAssign{
                            .result = res,
                            .value = *else_val,
                            .type = IRType::Float,
                        });
                    emit(IRIf{*cond, std::make_shared<IRIfBody>(
                                         std::move(then_instrs),
                                         std::move(else_instrs))});
                    return IRLocalRef{res};
                }
                emit(IRIf{*cond,
                          std::make_shared<IRIfBody>(std::move(then_instrs),
                                                     std::move(else_instrs))});
                return std::nullopt;
            }

            if constexpr (std::is_same_v<T, DelayCtor>)
                throw std::runtime_error(
                    "delay(...) must appear as the rhs of a bind (name = "
                    "delay N f)");

            return std::nullopt;
        },
        e->node);
}

} // namespace lower_detail
