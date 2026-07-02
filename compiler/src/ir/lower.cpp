#include "lower.hpp"

#include "ast/ast.hpp"
#include "builtins.hpp"
#include "ir.hpp"
#include "types/type.hpp"
#include <algorithm>
#include <cmath>
#include <numbers>
#include <optional>
#include <stdexcept>
#include <string>
#include <type_traits>
#include <unordered_map>
#include <unordered_set>
#include <utility>
#include <variant>
#include <vector>

namespace {

auto ir_type_of(const TypePtr &t) -> IRType {
    if (const auto *base = std::get_if<TypeBase>(&t->node)) {
        switch (base->kind) {
        case BaseTypeKind::Float:
            return IRType::Float;
        case BaseTypeKind::Int:
            return IRType::Int;
        case BaseTypeKind::Void:
            return IRType::Void;
        default:
            return IRType::Float;
        }
    }
    return IRType::Float;
}

auto ir_types_of(const TypePtr &t) -> std::vector<IRType> {
    if (const auto *arr = std::get_if<TypeArray>(&t->node)) {
        std::vector<IRType> out;
        out.reserve(arr->elements.size());
        for (const auto &elem : arr->elements) out.push_back(ir_type_of(elem));
        return out;
    }
    const auto scalar = ir_type_of(t);
    if (scalar == IRType::Void) return {};
    return {scalar};
}

auto eval_const_expr(const ExprPtr &e) -> double {
    if (const auto *lit = std::get_if<Literal>(&e->node))
        return std::stod(std::string(lit->value.lexeme));

    if (const auto *un = std::get_if<UnaryOp>(&e->node)) {
        const double v = eval_const_expr(un->expr);
        switch (un->op) {
        case Operation::Sub:
            return -v;
        case Operation::Not:
            return v == 0.0 ? 1.0 : 0.0;
        default:
            throw std::runtime_error(
                "param default must be a constant expression");
        }
    }

    if (const auto *bin = std::get_if<BinaryOp>(&e->node)) {
        const double l = eval_const_expr(bin->left);
        const double r = eval_const_expr(bin->right);
        switch (bin->op) {
        case Operation::Add:
            return l + r;
        case Operation::Sub:
            return l - r;
        case Operation::Mul:
            return l * r;
        case Operation::Div:
            return l / r;
        case Operation::Pow:
            return std::pow(l, r);
        default:
            throw std::runtime_error(
                "param default must be a constant expression");
        }
    }

    throw std::runtime_error("param default must be a constant expression");
}

struct FnInfo {
    std::vector<std::string> free_vars;
    std::vector<IRType> return_type;
    size_t arity{0};
    bool is_math{false};
};

struct Lowerer {
    IRModule mod;
    std::vector<IRInstr> *cur = nullptr;

    std::unordered_map<std::string, FnInfo> fns;
    std::unordered_map<std::string, size_t> fn_indices;
    std::unordered_set<std::string> bufs;
    std::unordered_set<std::string> statics;
    std::unordered_set<std::string> param_names;
    std::unordered_map<std::string, IRType> locals;
    std::vector<IRInstr> static_init_body;
    std::unordered_map<std::string, std::vector<std::string>> array_env;
    std::unordered_set<std::string> static_arrays;
    std::unordered_set<std::string> memory_arrays;
    std::unordered_map<std::string, std::string> inline_alias;

    size_t tmp_n = 0;

    auto tmp() -> std::string { return "$t" + std::to_string(tmp_n++); }
    void emit(IRInstr i) const { cur->emplace_back(std::move(i)); }
    void define(const std::string &name, IRType type) { locals[name] = type; }

    [[nodiscard]] auto is_special(const std::string &name) const -> bool {
        return std::ranges::contains(language_globals, name) ||
               std::ranges::contains(math_builtins, name) ||
               fns.contains(name) || bufs.contains(name) ||
               param_names.contains(name) || array_env.contains(name) ||
               name == "foldr" || name == "zip" || name == "map";
    }

    auto read_array_elem(const std::string &arr_name, size_t i) -> IRValue {
        if (memory_arrays.contains(arr_name)) {
            auto r = tmp();
            define(r, IRType::Float);
            const auto addr =
                mod.static_array_base(arr_name) + static_cast<uint32_t>(i * 8);
            emit(IRMemRead{.result = r, .addr = addr});
            return IRLocalRef{r};
        }
        const auto &elem = array_env.at(arr_name).at(i);
        if (static_arrays.contains(arr_name)) {
            auto r = tmp();
            define(r, IRType::Float);
            emit(IRStaticRead{.result = r, .name = elem});
            return IRLocalRef{r};
        }
        return IRLocalRef{elem};
    }

    auto emit_global_read(const std::string &name, IRType type) -> IRValue {
        auto r = tmp();
        emit(IRGlobalRead{.result = r, .name = name, .type = type});
        return IRLocalRef{r};
    }

    auto lower_tail_as_array(const ExprPtr &e, size_t n)
        -> std::vector<IRValue> {
        if (const auto *lit = std::get_if<ArrayLiteral>(&e->node)) {
            if (lit->elements.size() != n)
                throw std::runtime_error(
                    "internal error: array tail arity mismatch");
            std::vector<IRValue> out;
            out.reserve(n);
            for (const auto &elem : lit->elements) {
                auto v = lower_expr(elem);
                if (!v)
                    throw std::runtime_error(
                        "array element expression is void");
                out.push_back(*v);
            }
            return out;
        }

        if (const auto *block = std::get_if<CodeBlock>(&e->node)) {
            if (block->expressions.empty())
                throw std::runtime_error(
                    "internal error: empty array-tail block");
            for (size_t i = 0; i + 1 < block->expressions.size(); ++i)
                lower_expr(block->expressions[i]);
            return lower_tail_as_array(block->expressions.back(), n);
        }

        if (const auto *cond = std::get_if<Conditional>(&e->node)) {
            if (!cond->else_branch)
                throw std::runtime_error(
                    "array-returning conditional needs an else branch");
            const auto c = lower_expr(cond->condition);
            if (!c) throw std::runtime_error("conditional condition is void");

            std::vector<IRInstr> then_instrs;
            std::vector<IRInstr> else_instrs;
            auto *saved = cur;

            cur = &then_instrs;
            auto then_vals = lower_tail_as_array(cond->then_branch, n);
            cur = &else_instrs;
            auto else_vals = lower_tail_as_array(*cond->else_branch, n);
            cur = saved;

            std::vector<std::string> results;
            results.reserve(n);
            for (size_t i = 0; i < n; ++i) {
                auto r = tmp();
                define(r, IRType::Float);
                then_instrs.emplace_back(IRAssign{
                    .result = r, .value = then_vals[i], .type = IRType::Float});
                else_instrs.emplace_back(IRAssign{
                    .result = r, .value = else_vals[i], .type = IRType::Float});
                results.push_back(r);
            }
            emit(IRIf{
                .condition = *c,
                .body = std::make_shared<IRIfBody>(std::move(then_instrs),
                                                   std::move(else_instrs)),
            });
            std::vector<IRValue> out;
            out.reserve(n);
            for (const auto &r : results) out.emplace_back(IRLocalRef{r});
            return out;
        }

        if (std::holds_alternative<Call>(e->node)) {
            const auto [callee_node, arg_ptrs] = flatten_calls(e);
            const auto *var = std::get_if<Variable>(&callee_node->node);
            if (var != nullptr && fns.contains(var->name.lexeme)) {
                const auto &info = fns.at(var->name.lexeme);
                if (info.return_type.size() == n) {
                    std::vector<IRValue> call_args;
                    call_args.reserve(arg_ptrs.size() + info.free_vars.size());
                    for (const auto *arg_ptr : arg_ptrs) {
                        auto av = lower_expr(*arg_ptr);
                        if (!av)
                            throw std::runtime_error(
                                "void argument in call to " + var->name.lexeme);
                        call_args.push_back(*av);
                    }
                    for (const auto &fv : info.free_vars)
                        call_args.emplace_back(IRLocalRef{fv});
                    std::vector<std::string> results;
                    results.reserve(n);
                    for (size_t i = 0; i < n; ++i) {
                        auto t = tmp();
                        define(t, info.return_type[i]);
                        results.push_back(t);
                    }
                    emit(IRCall{
                        .result = results,
                        .callee = var->name.lexeme,
                        .args = std::move(call_args),
                        .result_type = info.return_type,
                    });
                    std::vector<IRValue> out;
                    out.reserve(n);
                    for (const auto &r : results)
                        out.emplace_back(IRLocalRef{r});
                    return out;
                }
            }
        }

        throw std::runtime_error(
            "array-returning function body must end in an array literal "
            "(optionally through a block or if/else)");
    }

    void
    lower_fn_body(IRFunction &fn,
                  const std::vector<std::pair<std::string, IRType>> &params,
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

    auto free_vars_of(const ExprPtr &e, std::unordered_set<std::string> bound)
        -> std::unordered_set<std::string> {
        return std::visit(
            [&](const auto &node) -> std::unordered_set<std::string> {
                using T = std::decay_t<decltype(node)>;

                if constexpr (std::is_same_v<T, Variable>) {
                    const auto &name = node.resolved_name.empty()
                                           ? node.name.lexeme
                                           : node.resolved_name;
                    if (!bound.contains(name) &&
                        !is_special(node.name.lexeme) && locals.contains(name))
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

    void lift(const std::string &name, const ExprPtr &e) {
        std::vector<std::string> params;
        const ExprPtr *body_ptr = &e;
        while (std::holds_alternative<Lambda>((*body_ptr)->node)) {
            const auto &lam = std::get<Lambda>((*body_ptr)->node);
            if (lam.parameter.has_value())
                params.emplace_back(lam.parameter->lexeme);
            body_ptr = &lam.body;
        }
        const ExprPtr &body = *body_ptr;

        const std::unordered_set<std::string> bound(params.begin(),
                                                    params.end());
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

    auto inline_lambda_body(const ExprPtr &lam_expr,
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

    static auto flatten_calls(const ExprPtr &e)
        -> std::pair<const Expr *, std::vector<const ExprPtr *>> {
        std::vector<const ExprPtr *> args;
        const Expr *node = e.get();
        while (const auto *call = std::get_if<Call>(&node->node)) {
            args.emplace_back(&call->argument);
            node = call->callee.get();
        }
        std::ranges::reverse(args);
        return {node, args};
    }

    void lower_bind_map(const std::string &dest_name, const ExprPtr &value) {
        const auto [callee_node, arg_ptrs] = flatten_calls(value);
        if (arg_ptrs.size() != 2)
            throw std::runtime_error("map expects 2 arguments");

        const bool is_inline_lambda =
            std::holds_alternative<Lambda>((*arg_ptrs[0])->node);
        std::string fn_name;
        if (const auto *fn_var = std::get_if<Variable>(&(*arg_ptrs[0])->node)) {
            fn_name = fn_var->name.lexeme;
            if (!fns.contains(fn_name))
                throw std::runtime_error("map: '" + fn_name +
                                         "' is not a function");
        } else if (is_inline_lambda) {
            fn_name = "";
        } else {
            throw std::runtime_error(
                "map: first argument must be a function or lambda");
        }

        const auto *arr_var = std::get_if<Variable>(&(*arg_ptrs[1])->node);
        if (arr_var == nullptr)
            throw std::runtime_error("map: array argument must be a variable");
        const auto &arr = arr_var->name.lexeme;
        if (!array_env.contains(arr))
            throw std::runtime_error("map: '" + arr + "' is not an array");

        const auto n = array_env.at(arr).size();

        std::vector<IRValue> results;
        results.reserve(n);
        for (size_t i = 0; i < n; ++i) {
            auto v = read_array_elem(arr, i);
            auto r = tmp();
            define(r, IRType::Float);
            if (is_inline_lambda) {
                const auto &elem_local = std::get<IRLocalRef>(v).name;
                auto body_val = inline_lambda_body(*arg_ptrs[0], {elem_local});
                if (!body_val)
                    throw std::runtime_error("map: lambda body is void");
                emit(IRAssign{
                    .result = r, .value = *body_val, .type = IRType::Float});
            } else {
                const auto &fn_info = fns.at(fn_name);
                std::vector<IRValue> call_args = {v};
                for (const auto &fv : fn_info.free_vars)
                    call_args.emplace_back(IRLocalRef{fv});
                emit(make_scalar_call(r, fn_name, std::move(call_args),
                                      IRType::Float));
            }
            results.emplace_back(IRLocalRef{r});
        }

        if (memory_arrays.contains(dest_name)) {
            const auto base = mod.static_array_base(dest_name);
            for (size_t i = 0; i < n; ++i)
                emit(IRMemWrite{.addr = base + static_cast<uint32_t>(i * 8),
                                .value = results[i]});
        } else if (static_arrays.contains(dest_name)) {
            const auto &elem_names = array_env.at(dest_name);
            for (size_t i = 0; i < n; ++i)
                emit(IRStaticWrite{.name = elem_names[i], .value = results[i]});
        } else {
            std::vector<std::string> elem_names;
            elem_names.reserve(n);
            for (size_t i = 0; i < n; ++i) {
                const auto &ref = std::get<IRLocalRef>(results[i]);
                elem_names.push_back(ref.name);
            }
            array_env[dest_name] = std::move(elem_names);
        }
    }

    void lower_bind_zip(const std::string &dest_name, const ExprPtr &value) {
        const auto [callee_node, arg_ptrs] = flatten_calls(value);
        if (arg_ptrs.size() != 3)
            throw std::runtime_error("zip expects 3 arguments");

        const bool is_inline_lambda =
            std::holds_alternative<Lambda>((*arg_ptrs[0])->node);
        std::string fn_name;
        if (const auto *fn_var = std::get_if<Variable>(&(*arg_ptrs[0])->node)) {
            fn_name = fn_var->name.lexeme;
            if (!fns.contains(fn_name))
                throw std::runtime_error("zip: '" + fn_name +
                                         "' is not a function");
        } else if (is_inline_lambda) {
            fn_name = "";
        } else {
            throw std::runtime_error(
                "zip: first argument must be a function or lambda");
        }

        const auto *arr1_var = std::get_if<Variable>(&(*arg_ptrs[1])->node);
        const auto *arr2_var = std::get_if<Variable>(&(*arg_ptrs[2])->node);
        if ((arr1_var == nullptr) || (arr2_var == nullptr))
            throw std::runtime_error("zip: array arguments must be variables");
        const auto &arr1 = arr1_var->name.lexeme;
        const auto &arr2 = arr2_var->name.lexeme;
        if (!array_env.contains(arr1))
            throw std::runtime_error("zip: '" + arr1 + "' is not an array");
        if (!array_env.contains(arr2))
            throw std::runtime_error("zip: '" + arr2 + "' is not an array");
        const auto n = array_env.at(arr1).size();
        if (n != array_env.at(arr2).size())
            throw std::runtime_error("zip: arrays must have the same size");

        std::vector<IRValue> results;
        results.reserve(n);
        for (size_t i = 0; i < n; ++i) {
            auto v1 = read_array_elem(arr1, i);
            auto v2 = read_array_elem(arr2, i);
            auto r = tmp();
            define(r, IRType::Float);
            if (is_inline_lambda) {
                const auto &l1 = std::get<IRLocalRef>(v1).name;
                const auto &l2 = std::get<IRLocalRef>(v2).name;
                auto body_val = inline_lambda_body(*arg_ptrs[0], {l1, l2});
                if (!body_val)
                    throw std::runtime_error("zip: lambda body is void");
                emit(IRAssign{
                    .result = r, .value = *body_val, .type = IRType::Float});
            } else {
                const auto &fn_info = fns.at(fn_name);
                std::vector<IRValue> call_args = {v1, v2};
                for (const auto &fv : fn_info.free_vars)
                    call_args.emplace_back(IRLocalRef{fv});
                emit(make_scalar_call(r, fn_name, std::move(call_args),
                                      IRType::Float));
            }
            results.emplace_back(IRLocalRef{r});
        }

        if (memory_arrays.contains(dest_name)) {
            const auto base = mod.static_array_base(dest_name);
            for (size_t i = 0; i < n; ++i)
                emit(IRMemWrite{.addr = base + static_cast<uint32_t>(i * 8),
                                .value = results[i]});
        } else if (static_arrays.contains(dest_name)) {
            const auto &elem_names = array_env.at(dest_name);
            for (size_t i = 0; i < n; ++i)
                emit(IRStaticWrite{.name = elem_names[i], .value = results[i]});
        } else {
            std::vector<std::string> elem_names;
            elem_names.reserve(n);
            for (size_t i = 0; i < n; ++i) {
                const auto &ref = std::get<IRLocalRef>(results[i]);
                elem_names.push_back(ref.name);
            }
            array_env[dest_name] = std::move(elem_names);
        }
    }

    auto lower_foldr(const std::vector<const ExprPtr *> &arg_ptrs)
        -> std::optional<IRValue> {
        if (arg_ptrs.size() != 3)
            throw std::runtime_error("foldr expects 3 arguments");

        auto init_v = lower_expr(*arg_ptrs[0]);
        if (!init_v) throw std::runtime_error("foldr: void init value");

        const bool is_inline_lambda =
            std::holds_alternative<Lambda>((*arg_ptrs[1])->node);
        std::string fn_name;
        if (const auto *fn_var = std::get_if<Variable>(&(*arg_ptrs[1])->node)) {
            fn_name = fn_var->name.lexeme;
            if (!fns.contains(fn_name))
                throw std::runtime_error("foldr: '" + fn_name +
                                         "' is not a function");
        } else if (is_inline_lambda) {
            fn_name = "";
        } else {
            throw std::runtime_error(
                "foldr: second argument must be a function or lambda");
        }

        const auto *arr_var = std::get_if<Variable>(&(*arg_ptrs[2])->node);
        if (arr_var == nullptr)
            throw std::runtime_error("foldr: array must be a variable");
        const auto &arr_name = arr_var->name.lexeme;
        if (!array_env.contains(arr_name))
            throw std::runtime_error("foldr: '" + arr_name +
                                     "' is not an array");

        const auto n = array_env.at(arr_name).size();

        auto acc = tmp();
        define(acc, IRType::Float);
        emit(IRAssign{
            .result = acc,
            .value = *init_v,
            .type = IRType::Float,
        });

        for (int i = static_cast<int>(n) - 1; i >= 0; --i) {
            auto elem_v = read_array_elem(arr_name, static_cast<size_t>(i));
            auto r = tmp();
            define(r, IRType::Float);
            if (is_inline_lambda) {
                const auto &elem_local = std::get<IRLocalRef>(elem_v).name;
                auto body_val =
                    inline_lambda_body(*arg_ptrs[1], {elem_local, acc});
                if (!body_val)
                    throw std::runtime_error("foldr: lambda body is void");
                emit(IRAssign{
                    .result = r, .value = *body_val, .type = IRType::Float});
            } else {
                const auto &fn_info = fns.at(fn_name);
                std::vector<IRValue> call_args = {elem_v, IRLocalRef{acc}};
                for (const auto &fv : fn_info.free_vars)
                    call_args.emplace_back(IRLocalRef{fv});
                emit(make_scalar_call(r, fn_name, std::move(call_args),
                                      IRType::Float));
            }
            emit(IRAssign{
                .result = acc,
                .value = IRLocalRef{r},
                .type = IRType::Float,
            });
        }

        return IRLocalRef{acc};
    }

    auto lower_expr(const ExprPtr &e) -> std::optional<IRValue> {
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
                                    name + ") before indexing with @[i]");
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
                        return IRLiteral{(double)fn_indices.at(name)};
                    }

                    return IRLocalRef{
                        node.resolved_name.empty() ? name : node.resolved_name};
                }

                if constexpr (std::is_same_v<T, DelayRead>) {
                    const auto &name = node.name.lexeme;
                    if (array_env.contains(name)) {
                        if (!node.delay)
                            throw std::runtime_error(
                                "'" + name + "' is an array; use @[i]" + name +
                                " to access an element");
                        const auto *idx_lit =
                            std::get_if<Literal>(&(*node.delay)->node);
                        if (!idx_lit)
                            throw std::runtime_error(
                                "Array index in @[i]" + name +
                                " must be a compile-time integer literal");
                        const size_t index =
                            std::stoul(std::string(idx_lit->value.lexeme));
                        return read_array_elem(name, index);
                    }
                    if (!bufs.contains(name))
                        throw std::runtime_error("@" + name +
                                                 " is not a delay");
                    auto r = tmp();
                    if (node.delay) {
                        auto d = lower_expr(*node.delay);
                        if (!d)
                            throw std::runtime_error(
                                "delay expression is void");
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

                if constexpr (std::is_same_v<T, DelayWrite>) {
                    const auto &target = node.target.lexeme;
                    const auto val = lower_expr(node.value);
                    if (!val)
                        throw std::runtime_error("assigning void to delay");
                    if (!bufs.contains(target))
                        throw std::runtime_error(target + " is not a delay");
                    emit(IRDelayWrite{.delay = target, .value = *val});
                    return std::nullopt;
                }

                if constexpr (std::is_same_v<T, DelayWriteQuiet>) {
                    const auto &target = node.target.lexeme;
                    const auto val = lower_expr(node.value);
                    if (!val)
                        throw std::runtime_error("assigning void to delay");
                    if (!bufs.contains(target))
                        throw std::runtime_error(target + " is not a delay");
                    std::optional<std::string> dr;
                    if (node.delay) {
                        auto d = lower_expr(*node.delay);
                        if (!d)
                            throw std::runtime_error(
                                "delay expression is void");
                        auto drtmp = tmp();
                        emit(IRAssign{.result = drtmp,
                                      .value = *d,
                                      .type = IRType::Float});
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
                    if (!val)
                        throw std::runtime_error("assigning void to OUT[n]");
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

                if constexpr (std::is_same_v<T, Bind>) {
                    const auto &name = node.name.lexeme;
                    const auto &scalar_name = node.resolved_name.empty()
                                                  ? node.name.lexeme
                                                  : node.resolved_name;

                    if (std::holds_alternative<ArrayLiteral>(
                            node.value->node)) {
                        const auto &arr =
                            std::get<ArrayLiteral>(node.value->node);
                        std::vector<std::string> elems;
                        elems.reserve(arr.elements.size());
                        for (const auto &elem_expr : arr.elements) {
                            auto v = lower_expr(elem_expr);
                            if (!v)
                                throw std::runtime_error(
                                    "void element in array literal");
                            auto t = tmp();
                            define(t, IRType::Float);
                            emit(IRAssign{
                                t,
                                *v,
                                IRType::Float,
                            });
                            elems.push_back(t);
                        }
                        array_env[name] = std::move(elems);
                        return std::nullopt;
                    }

                    if (std::holds_alternative<ArrayCtor>(node.value->node)) {
                        const auto &ctor =
                            std::get<ArrayCtor>(node.value->node);
                        const std::string fn_name = name + "$array_init";
                        lift(fn_name, ctor.init_fn);
                        const auto &fn_info = fns.at(fn_name);
                        std::vector<std::string> elems;
                        elems.reserve(ctor.size);
                        for (size_t i = 0; i < ctor.size; ++i) {
                            auto t = tmp();
                            define(t, IRType::Float);
                            std::vector<IRValue> call_args = {
                                IRLiteral{static_cast<double>(i)}};
                            for (const auto &fv : fn_info.free_vars)
                                call_args.emplace_back(IRLocalRef{fv});
                            emit(make_scalar_call(t, fn_name,
                                                  std::move(call_args),
                                                  IRType::Float));
                            elems.push_back(t);
                        }
                        array_env[name] = std::move(elems);
                        return std::nullopt;
                    }

                    {
                        const auto [cn, ap] = flatten_calls(node.value);
                        if (const auto *cv = std::get_if<Variable>(&cn->node)) {
                            if (cv->name.lexeme == "zip") {
                                lower_bind_zip(name, node.value);
                                return std::nullopt;
                            }
                            if (cv->name.lexeme == "map") {
                                lower_bind_map(name, node.value);
                                return std::nullopt;
                            }
                        }
                    }

                    if (std::holds_alternative<Lambda>(node.value->node)) {
                        lift(name, node.value);
                        return std::nullopt;
                    }

                    if (std::holds_alternative<DelayCtor>(node.value->node)) {
                        const auto &ctor =
                            std::get<DelayCtor>(node.value->node);
                        if (!std::holds_alternative<Lambda>(ctor.init_fn->node))
                            throw std::runtime_error(
                                "Delay init must be a lambda");
                        const auto &lam = std::get<Lambda>(ctor.init_fn->node);
                        const std::string init_name = name + "$init";
                        const std::string param_name =
                            lam.parameter.has_value() ? lam.parameter->lexeme
                                                      : "$_";

                        IRFunction init_fn;
                        init_fn.name = init_name;
                        init_fn.return_type = {IRType::Float};
                        init_fn.params.emplace_back(param_name, IRType::Float);

                        lower_fn_body(init_fn,
                                      {{
                                          param_name,
                                          IRType::Float,
                                      }},
                                      lam.body);

                        mod.functions.emplace_back(std::move(init_fn));
                        bufs.insert(name);
                        mod.alloc_delay(name, ctor.size);
                        mod.delays.push_back({
                            .name = name,
                            .size_elements = ctor.size,
                            .init_fn = init_name,
                        });
                        return std::nullopt;
                    }

                    const bool shadows_locally = scalar_name != name;

                    if (param_names.contains(name) && !shadows_locally) {
                        const auto val = lower_expr(node.value);
                        if (val) emit(IRParamWrite{name, *val});
                        return std::nullopt;
                    }

                    if (statics.contains(name) && !shadows_locally) {
                        const auto val = lower_expr(node.value);
                        if (val) emit(IRStaticWrite{name, *val});
                        return std::nullopt;
                    }

                    {
                        const auto [cn, ap] = flatten_calls(node.value);
                        if (const auto *cv = std::get_if<Variable>(&cn->node);
                            cv != nullptr && fns.contains(cv->name.lexeme)) {
                            const auto &info = fns.at(cv->name.lexeme);
                            if (info.return_type.size() > 1) {
                                std::vector<IRValue> call_args;
                                call_args.reserve(ap.size());
                                for (const auto *arg_ptr : ap) {
                                    auto av = lower_expr(*arg_ptr);
                                    if (!av)
                                        throw std::runtime_error(
                                            "void argument in call to " +
                                            cv->name.lexeme);
                                    call_args.push_back(*av);
                                }
                                for (const auto &fv : info.free_vars)
                                    call_args.emplace_back(IRLocalRef{fv});
                                std::vector<std::string> results;
                                results.reserve(info.return_type.size());
                                for (size_t i = 0; i < info.return_type.size();
                                     ++i) {
                                    auto t = tmp();
                                    define(t, info.return_type[i]);
                                    results.push_back(t);
                                }
                                emit(IRCall{
                                    .result = results,
                                    .callee = cv->name.lexeme,
                                    .args = std::move(call_args),
                                    .result_type = info.return_type,
                                });
                                array_env[name] = std::move(results);
                                return std::nullopt;
                            }
                        }
                    }

                    const auto val = lower_expr(node.value);
                    if (!val) return std::nullopt;
                    const auto type = ir_type_of(node.value->type);
                    define(scalar_name, type);
                    emit(IRAssign{
                        .result = scalar_name,
                        .value = *val,
                        .type = type,
                    });
                    return std::nullopt;
                }

                if constexpr (std::is_same_v<T, StaticBind>) {
                    const auto &name = node.name.lexeme;

                    if (std::holds_alternative<ArrayCtor>(node.init->node)) {
                        const auto &ctor = std::get<ArrayCtor>(node.init->node);
                        const std::string fn_name = name + "$array_init";
                        lift(fn_name, ctor.init_fn);
                        const auto &fn_info = fns.at(fn_name);
                        const auto base =
                            mod.alloc_static_array(name, ctor.size);
                        std::vector<std::string> elem_names;
                        elem_names.reserve(ctor.size);
                        for (size_t i = 0; i < ctor.size; ++i) {
                            elem_names.push_back(name + "__" +
                                                 std::to_string(i));
                            std::vector<IRValue> call_args = {
                                IRLiteral{static_cast<double>(i)}};
                            for (const auto &fv : fn_info.free_vars)
                                call_args.emplace_back(IRLocalRef{fv});
                            auto r = "$sinit$" + std::to_string(tmp_n++);
                            static_init_body.emplace_back(make_scalar_call(
                                r, fn_name, std::move(call_args),
                                IRType::Float));
                            static_init_body.emplace_back(IRMemWrite{
                                .addr = base + static_cast<uint32_t>(i * 8),
                                .value = IRLocalRef{r},
                            });
                        }
                        static_arrays.insert(name);
                        memory_arrays.insert(name);
                        array_env[name] = std::move(elem_names);
                        return std::nullopt;
                    }

                    if (std::holds_alternative<ArrayLiteral>(node.init->node)) {
                        const auto &arr =
                            std::get<ArrayLiteral>(node.init->node);
                        const auto n = arr.elements.size();
                        const auto base = mod.alloc_static_array(name, n);
                        std::vector<std::string> elem_names;
                        elem_names.reserve(n);
                        auto *saved = cur;
                        cur = &static_init_body;
                        for (size_t i = 0; i < n; ++i) {
                            elem_names.push_back(name + "__" +
                                                 std::to_string(i));
                            const auto v = lower_expr(arr.elements[i]);
                            if (v)
                                static_init_body.emplace_back(IRMemWrite{
                                    .addr = base + static_cast<uint32_t>(i * 8),
                                    .value = *v,
                                });
                        }
                        cur = saved;
                        static_arrays.insert(name);
                        memory_arrays.insert(name);
                        array_env[name] = std::move(elem_names);
                        return std::nullopt;
                    }

                    statics.insert(name);
                    mod.static_vars.push_back({
                        name,
                        IRType::Float,
                    });
                    auto *saved = cur;
                    cur = &static_init_body;
                    const auto val = lower_expr(node.init);
                    cur = saved;
                    if (val)
                        static_init_body.emplace_back(
                            IRStaticWrite{name, *val});
                    return std::nullopt;
                }

                if constexpr (std::is_same_v<T, ParamBind>) {
                    const auto &name = node.name.lexeme;
                    const double default_val =
                        eval_const_expr(node.default_val);
                    param_names.insert(name);
                    mod.params.emplace_back(name, default_val);
                    return std::nullopt;
                }

                if constexpr (std::is_same_v<T, CodeBlock>) {
                    std::optional<IRValue> last;
                    for (const auto &expr : node.expressions)
                        last = lower_expr(expr);
                    return last;
                }

                if constexpr (std::is_same_v<T, Call>) {
                    const auto [callee_node, arg_ptrs] = flatten_calls(e);

                    if (const auto *cv =
                            std::get_if<Variable>(&callee_node->node)) {
                        if (cv->name.lexeme == "foldr")
                            return lower_foldr(arg_ptrs);
                    }

                    std::vector<IRValue> arg_vals;
                    arg_vals.reserve(arg_ptrs.size());
                    for (const auto *ap : arg_ptrs) {
                        auto v = lower_expr(*ap);
                        if (!v)
                            throw std::runtime_error("void argument in call");
                        arg_vals.emplace_back(*v);
                    }

                    if (const auto *var =
                            std::get_if<Variable>(&callee_node->node)) {
                        const auto &name = var->name.lexeme;

                        if (std::ranges::contains(math_builtins, name)) {
                            auto r = tmp();
                            emit(make_scalar_call(r, "wasmwasm_" + name,
                                                  arg_vals, IRType::Float));
                            return IRLocalRef{r};
                        }

                        if (fns.contains(name)) {
                            const auto &info = fns.at(name);
                            if (info.return_type.size() > 1)
                                throw std::runtime_error(
                                    "'" + name +
                                    "' returns an array; call it via a "
                                    "bind (x = " +
                                    name + " ...) before indexing with @[i]");
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
                            emit(make_scalar_call(r, name, arg_vals,
                                                  info.return_type[0]));
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
                                    .right = IRLiteral{(double)fn_indices.at(
                                        fn_name)},
                                });
                                auto call_r = tmp();
                                auto call_args = arg_vals;
                                for (const auto &fv : fn_info.free_vars)
                                    call_args.emplace_back(IRLocalRef{fv});
                                std::vector<IRInstr> then_instrs;
                                const auto callee = fn_info.is_math
                                                        ? "wasmwasm_" + fn_name
                                                        : fn_name;
                                then_instrs.emplace_back(
                                    make_scalar_call(call_r, callee, call_args,
                                                     fn_info.return_type[0]));
                                then_instrs.emplace_back(IRAssign{
                                    .result = res,
                                    .value = IRLocalRef{call_r},
                                    .type = IRType::Float,
                                });
                                emit(IRIf{
                                    .condition = IRLocalRef{cond},
                                    .body = std::make_shared<IRIfBody>(
                                        std::move(then_instrs),
                                        std::vector<IRInstr>{}),
                                });
                            }
                            return IRLocalRef{res};
                        }
                        throw std::runtime_error("Unknown function: " + name);
                    }

                    throw std::runtime_error("Dynamic dispatch not supported");
                }

                if constexpr (std::is_same_v<T, Conditional>) {
                    const auto cond = lower_expr(node.condition);
                    if (!cond)
                        throw std::runtime_error(
                            "conditional condition is void");
                    std::vector<IRInstr> then_instrs;
                    std::vector<IRInstr> else_instrs;
                    auto *saved = cur;
                    cur = &then_instrs;
                    auto then_val = lower_expr(node.then_branch);
                    cur = &else_instrs;
                    std::optional<IRValue> else_val;
                    if (node.else_branch)
                        else_val = lower_expr(*node.else_branch);
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
                    emit(IRIf{*cond, std::make_shared<IRIfBody>(
                                         std::move(then_instrs),
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

    void lower_main(const ExprPtr &main_ast) {
        IRFunction fn;
        fn.name = "main$body";
        fn.return_type = {};

        auto *saved_cur = cur;
        cur = &fn.body;

        lower_expr(main_ast);

        cur = saved_cur;
        mod.functions.emplace_back(std::move(fn));
        mod.main_fn = "main$body";

        if (!static_init_body.empty()) {
            IRFunction sfn;
            sfn.name = "static_init";
            sfn.return_type = {};
            sfn.body = std::move(static_init_body);
            mod.functions.push_back(std::move(sfn));
            mod.static_init_fn = "static_init";
        }
    }

    void scan_arity(const std::vector<IRInstr> &body) {
        for (const auto &instr : body) {
            if (const auto *ir = std::get_if<IRInputRead>(&instr))
                mod.num_inputs = std::max(mod.num_inputs, ir->index + 1);
            if (const auto *ow = std::get_if<IROutputWrite>(&instr))
                mod.num_outputs = std::max(mod.num_outputs, ow->index + 1);
            if (const auto *iif = std::get_if<IRIf>(&instr)) {
                scan_arity(iif->body->then_body);
                scan_arity(iif->body->else_body);
            }
        }
    }

    void pre_register_math_builtins() {
        const auto env = make_builtin_env();
        const auto &map = env[0];
        for (const auto &sv : math_builtins) {
            const std::string name(sv);
            const auto it = map.find(name);
            if (it == map.end()) continue;
            size_t arity = 0;
            const TypePtr *t = &it->second;
            while (const auto *fn = std::get_if<TypeFun>(&(*t)->node)) {
                arity++;
                t = &fn->result;
            }
            fns.emplace(name, FnInfo{.free_vars = {},
                                     .return_type = {IRType::Float},
                                     .arity = arity,
                                     .is_math = true});
            fn_indices.emplace(name, fn_indices.size());
        }
    }

    void pre_register_fns(const ExprPtr &program) {
        const auto *block = std::get_if<CodeBlock>(&program->node);
        if (block == nullptr) return;
        for (const auto &expr : block->expressions) {
            const auto *bind = std::get_if<Bind>(&expr->node);
            if (bind == nullptr) continue;
            const auto &name = bind->name.lexeme;
            if (!std::holds_alternative<Lambda>(bind->value->node)) continue;
            size_t arity = 0;
            const ExprPtr *ptr = &bind->value;
            while (std::holds_alternative<Lambda>((*ptr)->node)) {
                if (std::get<Lambda>((*ptr)->node).parameter.has_value())
                    arity++;
                ptr = &std::get<Lambda>((*ptr)->node).body;
            }
            const auto ret_types = ir_types_of((*ptr)->type);
            fns.emplace(name, FnInfo{.free_vars = {},
                                     .return_type = ret_types,
                                     .arity = arity});
            fn_indices.emplace(name, fn_indices.size());
        }
    }

    void compute_arity() {
        for (const auto &fn : mod.functions) scan_arity(fn.body);
    }
};

} // namespace

auto lower(const ExprPtr &program, const std::string &module_name,
           uint32_t memory_base) -> IRModule {
    Lowerer l;
    l.mod.name = module_name;
    l.mod.memory_base = memory_base;
    l.mod.init_fn = "init";
    l.pre_register_math_builtins();
    l.pre_register_fns(program);
    l.lower_main(program);
    l.compute_arity();
    return std::move(l.mod);
}
