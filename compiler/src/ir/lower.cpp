#include "lower.hpp"
#include "lower_internal.hpp"

#include "ast/ast.hpp"
#include "builtins.hpp"
#include "ir.hpp"
#include "types/type.hpp"
#include <algorithm>
#include <cmath>
#include <stdexcept>
#include <string>

namespace lower_detail {

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

auto Lowerer::tmp() -> std::string { return "$t" + std::to_string(tmp_n++); }
void Lowerer::emit(IRInstr i) const { cur->emplace_back(std::move(i)); }
void Lowerer::define(const std::string &name, IRType type) {
    locals[name] = type;
}

auto Lowerer::is_special(const std::string &name) const -> bool {
    return std::ranges::contains(language_globals, name) ||
           std::ranges::contains(math_builtins, name) || fns.contains(name) ||
           bufs.contains(name) || param_names.contains(name) ||
           array_env.contains(name) || name == "foldr" || name == "zip" ||
           name == "map";
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

void Lowerer::pre_register_fns(const ExprPtr &program) {
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
            if (std::get<Lambda>((*ptr)->node).parameter.has_value()) arity++;
            ptr = &std::get<Lambda>((*ptr)->node).body;
        }
        const auto ret_types = ir_types_of((*ptr)->type);
        fns.emplace(
            name,
            FnInfo{.free_vars = {}, .return_type = ret_types, .arity = arity});
        fn_indices.emplace(name, fn_indices.size());
    }
}

void Lowerer::compute_arity() {
    for (const auto &fn : mod.functions) scan_arity(fn.body);
}

} // namespace lower_detail

auto lower(const ExprPtr &program, const std::string &module_name,
           uint32_t memory_base) -> IRModule {
    lower_detail::Lowerer l;
    l.mod.name = module_name;
    l.mod.memory_base = memory_base;
    l.mod.init_fn = "init";
    l.pre_register_math_builtins();
    l.pre_register_fns(program);
    l.lower_main(program);
    l.compute_arity();
    return std::move(l.mod);
}
