#include "lower.hpp"

#include "ast/ast.hpp"
#include "builtins.hpp"
#include "ir.hpp"
#include "lower_internal.hpp"
#include "types/type.hpp"
#include "types/type_inference.hpp"
#include <algorithm>
#include <cmath>
#include <stdexcept>
#include <string>
#include <type_traits>
#include <variant>

namespace lower_detail {

namespace {
auto to_ir_type(BaseTypeKind kind) -> IRType {
    switch (kind) {
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
} // namespace

auto ir_type_of(const TypePtr &t) -> IRType {
    return to_ir_type(scalar_kind_of(t));
}

auto ir_types_of(const TypePtr &t) -> std::vector<IRType> {
    const auto kinds = scalar_kinds_of(t);
    std::vector<IRType> out;
    out.reserve(kinds.size());
    for (const auto k : kinds) out.push_back(to_ir_type(k));
    return out;
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

auto Lowerer::tmp() -> std::string { return "$t" + std::to_string(tmp_n++); }
void Lowerer::emit(IRInstr i) const { cur->emplace_back(std::move(i)); }
void Lowerer::define(const std::string &name, IRType type) {
    locals[name] = type;
}

auto Lowerer::is_special(const std::string &name) const -> bool {
    return std::ranges::contains(language_globals, name) ||
           std::ranges::contains(math_builtins, name) || fns.contains(name) ||
           bufs.contains(name) || param_names.contains(name) ||
           array_env.contains(name);
}

auto Lowerer::emit_global_read(const std::string &name, IRType type)
    -> IRValue {
    auto r = tmp();
    emit(IRGlobalRead{.result = r, .name = name, .type = type});
    return IRLocalRef{r};
}

void Lowerer::lower_main(const ExprPtr &main_ast) {
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

void Lowerer::scan_arity(const std::vector<IRInstr> &body) {
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

void Lowerer::pre_register_math_builtins() {
    const auto env = make_builtin_env();
    const auto &map = env[0];
    for (const auto &sv : math_builtins) {
        const std::string name(sv);
        const auto it = map.find(name);
        if (it == map.end()) continue;
        const size_t arity = arity_of(it->second);
        fns.emplace(name, FnInfo{
                              .free_vars = {},
                              .return_type = {IRType::Float},
                              .arity = arity,
                              .is_math = true,
                              .param_shapes = {},
                          });
        fn_indices.emplace(name, fn_indices.size());
    }
}

void Lowerer::pre_register_fns(const ExprPtr &program) {
    const auto *block = std::get_if<CodeBlock>(&program->node);
    if (block == nullptr) return;
    for (const auto &expr : block->expressions) {
        const auto *bind = std::get_if<Bind>(&expr->node);
        if (bind == nullptr) continue;
        if (!std::holds_alternative<Lambda>(bind->value->node)) continue;
        register_fn_signature(bind->name.lexeme, bind->value);
    }
}

void Lowerer::pre_register_arities(const ExprPtr &program) {
    const auto *block = std::get_if<CodeBlock>(&program->node);
    if (block == nullptr) return;
    for (const auto &expr : block->expressions) {
        const auto *bind = std::get_if<Bind>(&expr->node);
        if (bind == nullptr) continue;
        if (!std::holds_alternative<Lambda>(bind->value->node)) continue;
        size_t arity = 0;
        const ExprPtr *ptr = &bind->value;
        while (std::holds_alternative<Lambda>((*ptr)->node)) {
            if (std::get<Lambda>((*ptr)->node).parameter.has_value()) arity++;
            ptr = &std::get<Lambda>((*ptr)->node).body;
        }
        known_arities[bind->name.lexeme] = arity;
    }
}

void Lowerer::compute_arity() {
    for (const auto &fn : mod.functions) scan_arity(fn.body);
}

auto Lowerer::shape_of_call_arg(const ExprPtr &arg) const -> ParamShape {
    if (std::holds_alternative<Lambda>(arg->node))
        return {.kind = ParamKind::Closure, .size = closure_max_captures};
    if (const auto *lit = std::get_if<ArrayLiteral>(&arg->node))
        return {.kind = ParamKind::Array, .size = lit->elements.size()};
    if (const auto *var = std::get_if<Variable>(&arg->node)) {
        auto it = known_arities.find(var->name.lexeme);
        if (it != known_arities.end() && it->second > 0)
            return {.kind = ParamKind::Closure, .size = closure_max_captures};
    }
    return {.kind = ParamKind::Scalar, .size = 1};
}

void Lowerer::scan_call_site_shapes(const ExprPtr &e) {
    if (!e) return;
    std::visit(
        [&](const auto &node) -> auto {
            using T = std::decay_t<decltype(node)>;
            if constexpr (std::is_same_v<T, Call>) {
                const auto [callee_node, arg_ptrs] = flatten_calls(e);
                if (const auto *var =
                        std::get_if<Variable>(&(*callee_node)->node)) {
                    auto &shapes = call_site_shapes[var->name.lexeme];
                    if (shapes.size() < arg_ptrs.size())
                        shapes.resize(arg_ptrs.size());
                    for (size_t i = 0; i < arg_ptrs.size(); ++i) {
                        const auto shape = shape_of_call_arg(*arg_ptrs[i]);
                        if (shape.kind != ParamKind::Scalar) shapes[i] = shape;
                    }
                }
                for (const auto *ap : arg_ptrs) scan_call_site_shapes(*ap);
                scan_call_site_shapes(*callee_node);
                return;
            }
            if constexpr (std::is_same_v<T, Bind>)
                scan_call_site_shapes(node.value);
            if constexpr (std::is_same_v<T, StaticBind>)
                scan_call_site_shapes(node.init);
            if constexpr (std::is_same_v<T, ParamBind>)
                scan_call_site_shapes(node.default_val);
            if constexpr (std::is_same_v<T, CodeBlock>)
                for (const auto &child : node.expressions)
                    scan_call_site_shapes(child);
            if constexpr (std::is_same_v<T, Lambda>)
                scan_call_site_shapes(node.body);
            if constexpr (std::is_same_v<T, Conditional>) {
                scan_call_site_shapes(node.condition);
                scan_call_site_shapes(node.then_branch);
                if (node.else_branch) scan_call_site_shapes(*node.else_branch);
            }
            if constexpr (std::is_same_v<T, BinaryOp>) {
                scan_call_site_shapes(node.left);
                scan_call_site_shapes(node.right);
            }
            if constexpr (std::is_same_v<T, UnaryOp>)
                scan_call_site_shapes(node.expr);
            if constexpr (std::is_same_v<T, OutputWrite>)
                scan_call_site_shapes(node.value);
            if constexpr (std::is_same_v<T, DelayWrite>)
                scan_call_site_shapes(node.value);
            if constexpr (std::is_same_v<T, DelayWriteQuiet>)
                scan_call_site_shapes(node.value);
            if constexpr (std::is_same_v<T, ArrayLiteral>)
                for (const auto &elem : node.elements)
                    scan_call_site_shapes(elem);
            if constexpr (std::is_same_v<T, ArrayIndex>)
                scan_call_site_shapes(node.index);
            if constexpr (std::is_same_v<T, ExprIndex>) {
                scan_call_site_shapes(node.base);
                scan_call_site_shapes(node.index);
            }
        },
        e->node);
}

void Lowerer::pre_register_lambda_args(const ExprPtr &e) {
    if (!e) return;
    std::visit(
        [&](const auto &node) -> auto {
            using T = std::decay_t<decltype(node)>;
            if constexpr (std::is_same_v<T, Call>) {
                const auto [callee_node, arg_ptrs] = flatten_calls(e);
                for (const auto *ap : arg_ptrs) {
                    if (std::holds_alternative<Lambda>((*ap)->node) &&
                        !lambda_arg_names.contains(ap->get())) {
                        const auto fn_name = "$lam$" + std::to_string(tmp_n++);
                        lambda_arg_names[ap->get()] = fn_name;
                        register_fn_signature(fn_name, *ap);
                    }
                    pre_register_lambda_args(*ap);
                }
                pre_register_lambda_args(*callee_node);
                return;
            }
            if constexpr (std::is_same_v<T, Bind>)
                pre_register_lambda_args(node.value);
            if constexpr (std::is_same_v<T, StaticBind>)
                pre_register_lambda_args(node.init);
            if constexpr (std::is_same_v<T, ParamBind>)
                pre_register_lambda_args(node.default_val);
            if constexpr (std::is_same_v<T, CodeBlock>)
                for (const auto &child : node.expressions)
                    pre_register_lambda_args(child);
            if constexpr (std::is_same_v<T, Lambda>)
                pre_register_lambda_args(node.body);
            if constexpr (std::is_same_v<T, Conditional>) {
                pre_register_lambda_args(node.condition);
                pre_register_lambda_args(node.then_branch);
                if (node.else_branch)
                    pre_register_lambda_args(*node.else_branch);
            }
            if constexpr (std::is_same_v<T, BinaryOp>) {
                pre_register_lambda_args(node.left);
                pre_register_lambda_args(node.right);
            }
            if constexpr (std::is_same_v<T, UnaryOp>)
                pre_register_lambda_args(node.expr);
            if constexpr (std::is_same_v<T, OutputWrite>)
                pre_register_lambda_args(node.value);
            if constexpr (std::is_same_v<T, DelayWrite>)
                pre_register_lambda_args(node.value);
            if constexpr (std::is_same_v<T, DelayWriteQuiet>)
                pre_register_lambda_args(node.value);
            if constexpr (std::is_same_v<T, ArrayLiteral>)
                for (const auto &elem : node.elements)
                    pre_register_lambda_args(elem);
            if constexpr (std::is_same_v<T, ArrayIndex>)
                pre_register_lambda_args(node.index);
            if constexpr (std::is_same_v<T, ExprIndex>) {
                pre_register_lambda_args(node.base);
                pre_register_lambda_args(node.index);
            }
        },
        e->node);
}

} // namespace lower_detail

auto lower(const ExprPtr &program, const std::string &module_name) -> IRModule {
    lower_detail::Lowerer l;
    l.mod.name = module_name;
    l.mod.init_fn = "init";
    l.pre_register_math_builtins();
    l.pre_register_arities(program);
    l.scan_call_site_shapes(program);
    l.pre_register_fns(program);
    l.pre_register_lambda_args(program);
    l.lower_main(program);
    l.compute_arity();
    return std::move(l.mod);
}
