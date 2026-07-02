#include "lower_internal.hpp"

#include "ast/ast.hpp"
#include "ir.hpp"
#include <stdexcept>
#include <string>
#include <vector>

namespace lower_detail {

auto Lowerer::read_array_elem(const std::string &arr_name, size_t i)
    -> IRValue {
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

auto Lowerer::lower_tail_as_array(const ExprPtr &e, size_t n)
    -> std::vector<IRValue> {
    if (const auto *var = std::get_if<Variable>(&e->node)) {
        const auto &raw_name = var->name.lexeme;
        const auto name = inline_alias.contains(raw_name)
                              ? inline_alias.at(raw_name)
                              : raw_name;
        if (!array_env.contains(name))
            throw std::runtime_error("'" + name + "' is not an array");
        if (array_env.at(name).size() != n)
            throw std::runtime_error(
                "internal error: array tail arity mismatch");
        std::vector<IRValue> out;
        out.reserve(n);
        for (size_t i = 0; i < n; ++i) out.push_back(read_array_elem(name, i));
        return out;
    }

    if (const auto *lit = std::get_if<ArrayLiteral>(&e->node)) {
        if (lit->elements.size() != n)
            throw std::runtime_error(
                "internal error: array tail arity mismatch");
        std::vector<IRValue> out;
        out.reserve(n);
        for (const auto &elem : lit->elements) {
            auto v = lower_expr(elem);
            if (!v)
                throw std::runtime_error("array element expression is void");
            out.push_back(*v);
        }
        return out;
    }

    if (const auto *block = std::get_if<CodeBlock>(&e->node)) {
        if (block->expressions.empty())
            throw std::runtime_error("internal error: empty array-tail block");
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
        const auto *var = std::get_if<Variable>(&(*callee_node)->node);
        if (var != nullptr && fns.contains(var->name.lexeme)) {
            const auto &info = fns.at(var->name.lexeme);
            if (info.return_type.size() == n) {
                std::vector<IRValue> call_args;
                call_args.reserve(arg_ptrs.size() + info.free_vars.size());
                for (const auto *arg_ptr : arg_ptrs) {
                    auto av = lower_expr(*arg_ptr);
                    if (!av)
                        throw std::runtime_error("void argument in call to " +
                                                 var->name.lexeme);
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
                for (const auto &r : results) out.emplace_back(IRLocalRef{r});
                return out;
            }
        }
    }

    throw std::runtime_error(
        "array-returning function body must end in an array literal "
        "(optionally through a block or if/else)");
}

void Lowerer::lower_bind_map(const std::string &dest_name,
                             const ExprPtr &value) {
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
            if (!body_val) throw std::runtime_error("map: lambda body is void");
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

void Lowerer::lower_bind_zip(const std::string &dest_name,
                             const ExprPtr &value) {
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
            if (!body_val) throw std::runtime_error("zip: lambda body is void");
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

auto Lowerer::lower_foldr(const std::vector<const ExprPtr *> &arg_ptrs)
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
        throw std::runtime_error("foldr: '" + arr_name + "' is not an array");

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
            auto body_val = inline_lambda_body(*arg_ptrs[1], {elem_local, acc});
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

} // namespace lower_detail
