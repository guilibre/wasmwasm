#include "lower_internal.hpp"

#include "ast/ast.hpp"
#include "ir.hpp"
#include <algorithm>
#include <functional>
#include <stdexcept>
#include <type_traits>
#include <unordered_set>

namespace lower_detail {

namespace {

auto tail_ptr_of(const ExprPtr *e) -> const ExprPtr * {
    const ExprPtr *cur = e;
    while (const auto *block = std::get_if<CodeBlock>(&(*cur)->node)) {
        if (block->expressions.empty()) break;
        cur = &block->expressions.back();
    }
    return cur;
}

void collect_lead_statements(const ExprPtr *branch,
                             std::vector<const ExprPtr *> &out) {
    const auto *block = std::get_if<CodeBlock>(&(*branch)->node);
    if (block == nullptr || block->expressions.empty()) return;
    for (size_t i = 0; i + 1 < block->expressions.size(); ++i)
        out.push_back(&block->expressions[i]);
    collect_lead_statements(&block->expressions.back(), out);
}

auto contains_self_call_excluding(const ExprPtr &e, const std::string &fn_name,
                                  size_t arity, const ExprPtr *excluded)
    -> bool {
    bool found = false;
    std::function<void(const ExprPtr &)> walk =
        [&](const ExprPtr &node) -> void {
        if (found) return;
        std::visit(
            [&](const auto &n) -> void {
                using T = std::decay_t<decltype(n)>;
                if constexpr (std::is_same_v<T, Call>) {
                    const auto [callee, args] = Lowerer::flatten_calls(node);
                    if (&node != excluded) {
                        if (const auto *cv =
                                std::get_if<Variable>(&(*callee)->node);
                            cv != nullptr && cv->name.lexeme == fn_name &&
                            args.size() == arity) {
                            found = true;
                            return;
                        }
                    }
                    walk(*callee);
                    for (const auto *a : args) walk(*a);
                } else if constexpr (std::is_same_v<T, Bind> ||
                                     std::is_same_v<T, DelayWrite> ||
                                     std::is_same_v<T, OutputWrite>) {
                    walk(n.value);
                } else if constexpr (std::is_same_v<T, CodeBlock>) {
                    for (const auto &s : n.expressions) walk(s);
                } else if constexpr (std::is_same_v<T, BinaryOp>) {
                    walk(n.left);
                    walk(n.right);
                } else if constexpr (std::is_same_v<T, UnaryOp>) {
                    walk(n.expr);
                } else if constexpr (std::is_same_v<T, Conditional>) {
                    walk(n.condition);
                    walk(n.then_branch);
                    if (n.else_branch) walk(*n.else_branch);
                } else if constexpr (std::is_same_v<T, Lambda>) {
                    walk(n.body);
                } else if constexpr (std::is_same_v<T, DelayWriteQuiet>) {
                    walk(n.value);
                    if (n.delay) walk(*n.delay);
                } else if constexpr (std::is_same_v<T, DelayRead>) {
                    if (n.delay) walk(*n.delay);
                } else if constexpr (std::is_same_v<T, DelayCtor> ||
                                     std::is_same_v<T, ArrayCtor>) {
                    walk(n.init_fn);
                } else if constexpr (std::is_same_v<T, ParamBind>) {
                    walk(n.default_val);
                } else if constexpr (std::is_same_v<T, StaticBind>) {
                    walk(n.init);
                } else if constexpr (std::is_same_v<T, ArrayLiteral>) {
                    for (const auto &el : n.elements) walk(el);
                } else if constexpr (std::is_same_v<T, ArrayIndex>) {
                    walk(n.index);
                } else if constexpr (std::is_same_v<T, ExprIndex>) {
                    walk(n.base);
                    walk(n.index);
                }
            },
            node->node);
    };
    walk(e);
    return found;
}

auto match_decrement(const ExprPtr &arg, const std::string &counter_name)
    -> std::optional<double> {
    const auto *tail = tail_ptr_of(&arg);
    const auto *bin = std::get_if<BinaryOp>(&(*tail)->node);
    if (bin == nullptr) return std::nullopt;
    const auto *var = std::get_if<Variable>(&bin->left->node);
    if (var == nullptr || var->name.lexeme != counter_name) return std::nullopt;

    if (bin->op == Operation::Sub) {
        if (const auto *lit = std::get_if<Literal>(&bin->right->node))
            return std::stod(std::string(lit->value.lexeme));
        return std::nullopt;
    }
    if (bin->op == Operation::Add) {
        if (const auto *neg = std::get_if<UnaryOp>(&bin->right->node);
            neg != nullptr && neg->op == Operation::Sub) {
            if (const auto *lit = std::get_if<Literal>(&neg->expr->node))
                return std::stod(std::string(lit->value.lexeme));
        }
        return std::nullopt;
    }
    return std::nullopt;
}

auto mirror_op(Operation op) -> Operation {
    switch (op) {
    case Operation::Lt:
        return Operation::Gt;
    case Operation::Gt:
        return Operation::Lt;
    case Operation::Le:
        return Operation::Ge;
    case Operation::Ge:
        return Operation::Le;
    default:
        return op;
    }
}

auto satisfies(Operation op, double lhs, double rhs) -> bool {
    switch (op) {
    case Operation::Lt:
        return lhs < rhs;
    case Operation::Le:
        return lhs <= rhs;
    case Operation::Gt:
        return lhs > rhs;
    case Operation::Ge:
        return lhs >= rhs;
    case Operation::Eq:
        return lhs == rhs;
    case Operation::Ne:
        return lhs != rhs;
    default:
        return false;
    }
}

} // namespace

auto Lowerer::classify_unrollable(const std::string &fn_name,
                                  const ExprPtr *root_lambda)
    -> std::optional<UnrollShape> {
    std::vector<std::string> params;
    const ExprPtr *body_ptr = root_lambda;
    while (std::holds_alternative<Lambda>((*body_ptr)->node)) {
        const auto &lam = std::get<Lambda>((*body_ptr)->node);
        if (lam.parameter.has_value()) params.push_back(lam.parameter->lexeme);
        body_ptr = &lam.body;
    }

    if (const auto *block = std::get_if<CodeBlock>(&(*body_ptr)->node)) {
        if (block->expressions.size() != 1) return std::nullopt;
        body_ptr = &block->expressions.front();
    }

    const auto *cond = std::get_if<Conditional>(&(*body_ptr)->node);
    if (cond == nullptr) return std::nullopt;

    const auto *cond_tail = tail_ptr_of(&cond->condition);
    const auto *bin = std::get_if<BinaryOp>(&(*cond_tail)->node);
    if (bin == nullptr) return std::nullopt;

    static const std::unordered_set<Operation> cmp_ops = {
        Operation::Lt, Operation::Le, Operation::Gt,
        Operation::Ge, Operation::Eq, Operation::Ne};
    if (!cmp_ops.contains(bin->op)) return std::nullopt;

    const auto *left_var = std::get_if<Variable>(&bin->left->node);
    const auto *right_lit = std::get_if<Literal>(&bin->right->node);
    const auto *right_var = std::get_if<Variable>(&bin->right->node);
    const auto *left_lit = std::get_if<Literal>(&bin->left->node);

    size_t counter_idx = 0;
    double threshold = 0.0;
    Operation cmp_op = bin->op;
    bool matched = false;
    if (left_var != nullptr && right_lit != nullptr) {
        const auto it = std::ranges::find(params, left_var->name.lexeme);
        if (it != params.end()) {
            counter_idx = static_cast<size_t>(it - params.begin());
            threshold = std::stod(std::string(right_lit->value.lexeme));
            matched = true;
        }
    } else if (right_var != nullptr && left_lit != nullptr) {
        const auto it = std::ranges::find(params, right_var->name.lexeme);
        if (it != params.end()) {
            counter_idx = static_cast<size_t>(it - params.begin());
            threshold = std::stod(std::string(left_lit->value.lexeme));
            cmp_op = mirror_op(bin->op);
            matched = true;
        }
    }
    if (!matched) return std::nullopt;

    const ExprPtr *then_branch = &cond->then_branch;
    const ExprPtr *else_branch =
        cond->else_branch ? &*cond->else_branch : nullptr;
    const size_t arity = params.size();

    auto tail_self_call_args = [&](const ExprPtr *branch)
        -> std::optional<std::vector<const ExprPtr *>> {
        const auto *tail = tail_ptr_of(branch);
        if (!std::holds_alternative<Call>((*tail)->node)) return std::nullopt;
        const auto [callee, args] = Lowerer::flatten_calls(*tail);
        const auto *cv = std::get_if<Variable>(&(*callee)->node);
        if (cv == nullptr || cv->name.lexeme != fn_name || args.size() != arity)
            return std::nullopt;
        return args;
    };

    const auto then_args = tail_self_call_args(then_branch);
    const auto else_args = else_branch != nullptr
                               ? tail_self_call_args(else_branch)
                               : std::nullopt;

    bool recursive_on_true = false;
    const ExprPtr *recursive_branch = nullptr;
    const ExprPtr *base_branch = nullptr;
    std::vector<const ExprPtr *> recursive_call_args;

    if (else_branch == nullptr) {
        if (!then_args.has_value()) return std::nullopt;
        recursive_on_true = true;
        recursive_branch = then_branch;
        base_branch = nullptr;
        recursive_call_args = *then_args;
    } else {
        if (then_args.has_value() == else_args.has_value()) return std::nullopt;
        recursive_on_true = then_args.has_value();
        recursive_branch = recursive_on_true ? then_branch : else_branch;
        base_branch = recursive_on_true ? else_branch : then_branch;
        recursive_call_args = recursive_on_true ? *then_args : *else_args;
    }

    if (base_branch != nullptr &&
        contains_self_call_excluding(*base_branch, fn_name, arity, nullptr))
        return std::nullopt;
    const auto *recursive_tail = tail_ptr_of(recursive_branch);
    if (contains_self_call_excluding(*recursive_branch, fn_name, arity,
                                     recursive_tail))
        return std::nullopt;

    if (counter_idx >= recursive_call_args.size()) return std::nullopt;
    const auto decrement =
        match_decrement(*recursive_call_args[counter_idx], params[counter_idx]);
    if (!decrement || *decrement <= 0.0) return std::nullopt;

    UnrollShape shape;
    shape.counter_param_idx = counter_idx;
    shape.decrement = *decrement;
    shape.cmp_op = cmp_op;
    shape.threshold = threshold;
    shape.recursive_on_true = recursive_on_true;
    shape.base_branch = base_branch;
    shape.recursive_branch = recursive_branch;
    shape.recursive_call_args = std::move(recursive_call_args);
    return shape;
}

auto Lowerer::try_unroll_bounded_recursion(const ExprPtr &call_expr,
                                           std::optional<IRValue> &out)
    -> bool {
    const auto [callee_node, arg_ptrs] = flatten_calls(call_expr);
    const auto *cv = std::get_if<Variable>(&(*callee_node)->node);
    if (cv == nullptr) return false;
    const auto &fn_name = cv->name.lexeme;

    const auto cache_it = unroll_shape_cache.find(fn_name);
    const UnrollShape *shape = nullptr;
    if (cache_it != unroll_shape_cache.end()) {
        if (!cache_it->second) return false;
        shape = &*cache_it->second;
    } else {
        const auto ast_it = fn_ast.find(fn_name);
        if (ast_it == fn_ast.end()) return false;
        auto classified = classify_unrollable(fn_name, ast_it->second);
        const auto it =
            unroll_shape_cache.emplace(fn_name, std::move(classified)).first;
        if (!it->second) return false;
        shape = &*it->second;
    }

    const auto fn_it = fns.find(fn_name);
    if (fn_it == fns.end() || arg_ptrs.size() != fn_it->second.arity)
        return false;
    if (shape->counter_param_idx >= arg_ptrs.size()) return false;

    double counter_value = 0.0;
    try {
        counter_value = eval_const_expr(*arg_ptrs[shape->counter_param_idx]);
    } catch (const std::exception &) { return false; }
    double sim = counter_value;
    bool terminates = false;
    for (size_t steps = 0; steps <= kMaxUnrollIterations; ++steps) {
        const bool cond_true = satisfies(shape->cmp_op, sim, shape->threshold);
        const bool is_base = shape->recursive_on_true ? !cond_true : cond_true;
        if (is_base) {
            terminates = true;
            break;
        }
        sim -= shape->decrement;
    }
    if (!terminates) return false;

    std::vector<std::string> arg_locals;
    arg_locals.reserve(arg_ptrs.size());
    for (const auto *ap : arg_ptrs) {
        auto v = lower_expr(*ap);
        if (!v) throw std::runtime_error("void argument in call");
        auto t = tmp();
        define(t, IRType::Float);
        emit(IRAssign{.result = t, .value = *v, .type = IRType::Float});
        arg_locals.push_back(t);
    }

    out = unroll_call(fn_name, *shape, counter_value, arg_locals);
    return true;
}

auto Lowerer::unroll_call(const std::string &fn_name, const UnrollShape &shape,
                          double counter_value,
                          const std::vector<std::string> &arg_locals)
    -> std::optional<IRValue> {
    const ExprPtr *root = fn_ast.at(fn_name);
    std::vector<std::string> params;
    const ExprPtr *body_ptr = root;
    while (std::holds_alternative<Lambda>((*body_ptr)->node)) {
        const auto &lam = std::get<Lambda>((*body_ptr)->node);
        if (lam.parameter.has_value()) params.push_back(lam.parameter->lexeme);
        body_ptr = &lam.body;
    }

    struct SavedAlias {
        std::string name;
        std::optional<std::string> prior;
    };
    std::vector<SavedAlias> saved;
    saved.reserve(params.size());
    for (size_t i = 0; i < params.size(); ++i) {
        const auto it = inline_alias.find(params[i]);
        saved.push_back({
            .name = params[i],
            .prior = it != inline_alias.end()
                         ? std::optional<std::string>(it->second)
                         : std::nullopt,
        });
        inline_alias[params[i]] = arg_locals[i];
    }

    const auto &info = fns.at(fn_name);
    std::optional<IRValue> result;
    double sim = counter_value;
    bool hit_base = false;

    for (size_t iter = 0; iter <= kMaxUnrollIterations; ++iter) {
        const bool cond_true = satisfies(shape.cmp_op, sim, shape.threshold);
        const bool is_base = shape.recursive_on_true ? !cond_true : cond_true;

        if (is_base) {
            if (shape.base_branch == nullptr) {
                result = std::nullopt;
            } else if (info.return_type.size() > 1) {
                auto values = lower_tail_as_array(*shape.base_branch,
                                                  info.return_type.size());
                std::vector<std::string> results;
                results.reserve(values.size());
                for (size_t i = 0; i < values.size(); ++i) {
                    auto t = tmp();
                    define(t, info.return_type[i]);
                    emit(IRAssign{.result = t,
                                  .value = values[i],
                                  .type = info.return_type[i]});
                    results.push_back(t);
                }
                const auto tmp_name = "$ret$" + std::to_string(tmp_n++);
                array_env[tmp_name] = std::move(results);
                result = IRLocalRef{tmp_name};
            } else {
                result = lower_expr(*shape.base_branch);
            }
            hit_base = true;
            break;
        }

        std::vector<const ExprPtr *> lead;
        collect_lead_statements(shape.recursive_branch, lead);
        for (const auto *stmt : lead) lower_expr(*stmt);

        std::vector<std::string> next_locals;
        next_locals.reserve(shape.recursive_call_args.size());
        for (const auto *arg_expr : shape.recursive_call_args) {
            auto v = lower_expr(*arg_expr);
            if (!v)
                throw std::runtime_error(
                    "void argument in unrolled recursive call");
            auto t = tmp();
            define(t, IRType::Float);
            emit(IRAssign{.result = t, .value = *v, .type = IRType::Float});
            next_locals.push_back(t);
        }
        for (size_t i = 0; i < params.size(); ++i)
            inline_alias[params[i]] = next_locals[i];

        sim -= shape.decrement;
    }

    for (auto &s : saved) {
        if (s.prior)
            inline_alias[s.name] = *s.prior;
        else
            inline_alias.erase(s.name);
    }

    if (!hit_base)
        throw std::runtime_error(
            "internal error: unroll_call exceeded the iteration cap without "
            "reaching the base case (should have been caught by the "
            "termination simulation)");

    return result;
}

} // namespace lower_detail
