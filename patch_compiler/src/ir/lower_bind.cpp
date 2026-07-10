#include "lower_internal.hpp"

#include "ast/ast.hpp"
#include "ir.hpp"
#include <stdexcept>

namespace lower_detail {

auto Lowerer::lower_bind(const Bind &node) -> std::optional<IRValue> {
    const auto &name = node.name.lexeme;
    const auto &scalar_name =
        node.resolved_name.empty() ? node.name.lexeme : node.resolved_name;

    if (std::holds_alternative<ArrayLiteral>(node.value->node)) {
        const auto &arr = std::get<ArrayLiteral>(node.value->node);
        std::vector<std::string> elems;
        elems.reserve(arr.elements.size());
        for (const auto &elem_expr : arr.elements) {
            auto v = lower_expr(elem_expr);
            if (!v) throw std::runtime_error("void element in array literal");
            auto t = tmp();
            define(t, IRType::Float);
            emit(IRAssign{
                .result = t,
                .value = *v,
                .type = IRType::Float,
            });
            elems.push_back(t);
        }
        array_env[name] = std::move(elems);
        return std::nullopt;
    }

    if (std::holds_alternative<ArrayCtor>(node.value->node)) {
        const auto &ctor = std::get<ArrayCtor>(node.value->node);
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
            emit(make_scalar_call(t, fn_name, std::move(call_args),
                                  IRType::Float));
            elems.push_back(t);
        }
        array_env[name] = std::move(elems);
        return std::nullopt;
    }

    {
        const auto [cn, ap] = flatten_calls(node.value);
        if (const auto *cv = std::get_if<Variable>(&(*cn)->node)) {
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
        const auto &ctor = std::get<DelayCtor>(node.value->node);
        if (!std::holds_alternative<Lambda>(ctor.init_fn->node))
            throw std::runtime_error("Delay init must be a lambda");
        const auto &lam = std::get<Lambda>(ctor.init_fn->node);
        const std::string init_name = name + "$init";
        const std::string param_name =
            lam.parameter.has_value() ? lam.parameter->lexeme : "$_";

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
        mod.alloc_order.push_back(name);
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
        if (val) emit(IRParamWrite{.name = name, .value = *val});
        return std::nullopt;
    }

    if (statics.contains(name) && !shadows_locally) {
        const auto val = lower_expr(node.value);
        if (val) emit(IRStaticWrite{.name = name, .value = *val});
        return std::nullopt;
    }

    {
        const auto [cn, ap] = flatten_calls(node.value);
        if (const auto *cv = std::get_if<Variable>(&(*cn)->node);
            cv != nullptr && fns.contains(cv->name.lexeme)) {
            const auto &info = fns.at(cv->name.lexeme);
            if (info.return_type.size() > 1) {
                auto result = lower_call(node.value);
                if (!result)
                    throw std::runtime_error(
                        "void call in array-returning bind");
                const auto *ref = std::get_if<IRLocalRef>(&*result);
                if (ref == nullptr || !array_env.contains(ref->name))
                    throw std::runtime_error(
                        "call did not produce an array-returning bind");
                array_env[name] = array_env.at(ref->name);
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

auto Lowerer::lower_static_bind(const StaticBind &node)
    -> std::optional<IRValue> {
    const auto &name = node.name.lexeme;

    if (std::holds_alternative<ArrayCtor>(node.init->node)) {
        const auto &ctor = std::get<ArrayCtor>(node.init->node);
        const std::string fn_name = name + "$array_init";
        lift(fn_name, ctor.init_fn);
        const auto &fn_info = fns.at(fn_name);
        mod.static_arrays_decl.push_back(
            {.name = name, .size_elements = ctor.size});
        mod.alloc_order.push_back(name);
        std::vector<std::string> elem_names;
        elem_names.reserve(ctor.size);
        for (int i = 0; i < static_cast<int>(ctor.size); ++i) {
            elem_names.push_back(name + "__" + std::to_string(i));
            std::vector<IRValue> call_args = {
                IRLiteral{static_cast<double>(i)}};
            for (const auto &fv : fn_info.free_vars)
                call_args.emplace_back(IRLocalRef{fv});
            auto r = "$sinit$" + std::to_string(tmp_n++);
            static_init_body.emplace_back(make_scalar_call(
                r, fn_name, std::move(call_args), IRType::Float));
            static_init_body.emplace_back(IRMemWrite{
                .ref =
                    IRMemRef{
                        .buffer = name,
                        .byte_offset = i * 8,
                    },
                .value = IRLocalRef{r},
            });
        }
        static_arrays.insert(name);
        memory_arrays.insert(name);
        array_env[name] = std::move(elem_names);
        return std::nullopt;
    }

    if (std::holds_alternative<ArrayLiteral>(node.init->node)) {
        const auto &arr = std::get<ArrayLiteral>(node.init->node);
        const auto n = arr.elements.size();
        mod.static_arrays_decl.push_back({.name = name, .size_elements = n});
        mod.alloc_order.push_back(name);
        std::vector<std::string> elem_names;
        elem_names.reserve(n);
        auto *saved = cur;
        cur = &static_init_body;
        for (int i = 0; i < static_cast<int>(n); ++i) {
            elem_names.push_back(name + "__" + std::to_string(i));
            const auto v = lower_expr(arr.elements[static_cast<size_t>(i)]);
            if (v)
                static_init_body.emplace_back(IRMemWrite{
                    .ref =
                        IRMemRef{
                            .buffer = name,
                            .byte_offset = i * 8,
                        },
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
        .name = name,
        .type = IRType::Float,
    });
    auto *saved = cur;
    cur = &static_init_body;
    const auto val = lower_expr(node.init);
    cur = saved;
    if (val)
        static_init_body.emplace_back(
            IRStaticWrite{.name = name, .value = *val});
    return std::nullopt;
}

auto Lowerer::lower_param_bind(const ParamBind &node)
    -> std::optional<IRValue> {
    const auto &name = node.name.lexeme;
    const double default_val = eval_const_expr(node.default_val);
    param_names.insert(name);
    mod.params.emplace_back(name, default_val);
    return std::nullopt;
}

} // namespace lower_detail
