#include "lower.hpp"

#include "ast/ast.hpp"
#include "builtins.hpp"
#include "ir.hpp"
#include "types/type.hpp"
#include <algorithm>
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

struct FnInfo {
    std::vector<std::string> free_vars;
    IRType return_type{IRType::Void};
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

    size_t tmp_n = 0;

    auto tmp() -> std::string { return "$t" + std::to_string(tmp_n++); }
    void emit(IRInstr i) const { cur->emplace_back(std::move(i)); }
    void define(const std::string &name, IRType type) { locals[name] = type; }

    [[nodiscard]] auto is_special(const std::string &name) const -> bool {
        return std::ranges::contains(language_globals, name) ||
               std::ranges::contains(math_builtins, name) ||
               fns.contains(name) || bufs.contains(name) ||
               param_names.contains(name);
    }

    auto emit_global_read(const std::string &name, IRType type) -> IRValue {
        auto r = tmp();
        emit(IRGlobalRead{.result = r, .name = name, .type = type});
        return IRLocalRef{r};
    }

    void
    lower_fn_body(IRFunction &fn,
                  const std::vector<std::pair<std::string, IRType>> &params,
                  const ExprPtr &body) {
        auto *saved_cur = cur;
        const auto saved_locals = locals;
        cur = &fn.body;
        for (const auto &[name, type] : params) locals[name] = type;
        auto result = lower_expr(body);
        if (result && fn.return_type != IRType::Void) emit(IRReturn{result});
        cur = saved_cur;
        locals = saved_locals;
    }

    auto free_vars_of(const ExprPtr &e, std::unordered_set<std::string> bound)
        -> std::unordered_set<std::string> {
        return std::visit(
            [&](const auto &node) -> std::unordered_set<std::string> {
                using T = std::decay_t<decltype(node)>;

                if constexpr (std::is_same_v<T, Variable>) {
                    const auto &name = node.name.lexeme;
                    if (!bound.contains(name) && !is_special(name) &&
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
                    b2.insert(node.parameter.lexeme);
                    return free_vars_of(node.body, b2);
                }
                if constexpr (std::is_same_v<T, CodeBlock>) {
                    std::unordered_set<std::string> result;
                    auto inner = bound;
                    for (const auto &expr : node.expressions) {
                        auto fv = free_vars_of(expr, inner);
                        result.insert(fv.begin(), fv.end());
                        if (const auto *b = std::get_if<Bind>(&expr->node))
                            inner.insert(b->name.lexeme);
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

                return {};
            },
            e->node);
    }

    void lift(const std::string &name, const ExprPtr &e) {
        std::vector<std::string> params;
        const ExprPtr *body_ptr = &e;
        while (std::holds_alternative<Lambda>((*body_ptr)->node)) {
            const auto &lam = std::get<Lambda>((*body_ptr)->node);
            params.emplace_back(lam.parameter.lexeme);
            body_ptr = &lam.body;
        }
        const ExprPtr &body = *body_ptr;

        const std::unordered_set<std::string> bound(params.begin(),
                                                    params.end());
        const auto fv_set = free_vars_of(body, bound);
        std::vector<std::string> fv_vec(fv_set.begin(), fv_set.end());
        std::ranges::sort(fv_vec);

        const auto ret_type = ir_type_of(body->type);

        fns[name] = FnInfo{
            .free_vars = fv_vec,
            .return_type = ret_type,
            .arity = params.size(),
        };
        fn_indices.emplace(name, fn_indices.size());

        IRFunction fn;
        fn.name = name;
        fn.return_type = ret_type;
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

                    if (statics.contains(name)) {
                        auto r = tmp();
                        emit(IRStaticRead{r, name});
                        return IRLocalRef{r};
                    }

                    if (param_names.contains(name)) {
                        auto r = tmp();
                        emit(IRParamRead{.result = r, .name = name});
                        return IRLocalRef{r};
                    }

                    if (fns.contains(name))
                        return IRLiteral{(double)fn_indices.at(name)};

                    return IRLocalRef{name};
                }

                if constexpr (std::is_same_v<T, DelayRead>) {
                    const auto &name = node.name.lexeme;
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
                        emit(
                            IRCall{ca, "wasmwasm_clip", {*val}, IRType::Float});
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
                        emit(IRCall{ca, "wasmwasm_clip", {*l}, IRType::Float});
                        auto cb = tmp();
                        emit(IRCall{cb, "wasmwasm_clip", {*r}, IRType::Float});
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
                        emit(IRCall{ca, "wasmwasm_clip", {*l}, IRType::Float});
                        auto cb = tmp();
                        emit(IRCall{cb, "wasmwasm_clip", {*r}, IRType::Float});
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
                        emit(IRCall{
                            res, "wasmwasm_pow", {*l, *r}, IRType::Float});
                        return IRLocalRef{res};
                    }
                    auto res = tmp();
                    emit(IRBinOp{res, node.op, *l, *r});
                    return IRLocalRef{res};
                }

                if constexpr (std::is_same_v<T, Bind>) {
                    const auto &name = node.name.lexeme;

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

                        IRFunction init_fn;
                        init_fn.name = init_name;
                        init_fn.return_type = IRType::Float;
                        init_fn.params.emplace_back(lam.parameter.lexeme,
                                                    IRType::Float);

                        lower_fn_body(init_fn,
                                      {{lam.parameter.lexeme, IRType::Float}},
                                      lam.body);

                        mod.functions.emplace_back(std::move(init_fn));
                        bufs.insert(name);
                        mod.delays.push_back({
                            .name = name,
                            .size_elements = ctor.size,
                            .init_fn = init_name,
                        });
                        return std::nullopt;
                    }

                    if (statics.contains(name)) {
                        const auto val = lower_expr(node.value);
                        if (val) emit(IRStaticWrite{name, *val});
                        return std::nullopt;
                    }

                    if (param_names.contains(name)) {
                        const auto val = lower_expr(node.value);
                        if (val) emit(IRParamWrite{name, *val});
                        return std::nullopt;
                    }

                    const auto val = lower_expr(node.value);
                    if (!val) return std::nullopt;
                    const auto type = ir_type_of(node.value->type);
                    define(name, type);
                    emit(IRAssign{
                        .result = name,
                        .value = *val,
                        .type = type,
                    });
                    return std::nullopt;
                }

                if constexpr (std::is_same_v<T, StaticBind>) {
                    const auto &name = node.name.lexeme;
                    statics.insert(name);
                    mod.static_vars.push_back({name, IRType::Float});
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
                    const auto *lit =
                        std::get_if<Literal>(&node.default_val->node);
                    if (!lit)
                        throw std::runtime_error(
                            "param default must be a numeric literal");
                    const double default_val =
                        std::stod(std::string(lit->value.lexeme));
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
                            emit(IRCall{
                                .result = r,
                                .callee = "wasmwasm_" + name,
                                .args = arg_vals,
                                .result_type = IRType::Float,
                            });
                            return IRLocalRef{r};
                        }

                        if (fns.contains(name)) {
                            const auto &info = fns.at(name);
                            for (const auto &fv : info.free_vars)
                                arg_vals.emplace_back(IRLocalRef{fv});

                            if (info.return_type == IRType::Void) {
                                emit(IRCall{
                                    .result = "",
                                    .callee = name,
                                    .args = arg_vals,
                                    .result_type = IRType::Void,
                                });
                                return std::nullopt;
                            }
                            const auto r = tmp();
                            emit(IRCall{
                                .result = r,
                                .callee = name,
                                .args = arg_vals,
                                .result_type = info.return_type,
                            });
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
                                if (fn_info.return_type == IRType::Void)
                                    continue;
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
                                then_instrs.emplace_back(IRCall{
                                    .result = call_r,
                                    .callee = callee,
                                    .args = call_args,
                                    .result_type = fn_info.return_type,
                                });
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
                            then_instrs.emplace_back(
                                IRAssign{res, *then_val, IRType::Float});
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
        fn.return_type = IRType::Void;

        auto *saved_cur = cur;
        cur = &fn.body;

        lower_expr(main_ast);

        cur = saved_cur;
        mod.functions.emplace_back(std::move(fn));
        mod.main_fn = "main$body";

        if (!static_init_body.empty()) {
            IRFunction sfn;
            sfn.name = "static_init";
            sfn.return_type = IRType::Void;
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
                                     .return_type = IRType::Float,
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
                arity++;
                ptr = &std::get<Lambda>((*ptr)->node).body;
            }
            const auto ret_type = ir_type_of((*ptr)->type);
            fns.emplace(name, FnInfo{.free_vars = {},
                                     .return_type = ret_type,
                                     .arity = arity});
            fn_indices.emplace(name, fn_indices.size());
        }
    }

    void compute_arity() {
        for (const auto &fn : mod.functions) scan_arity(fn.body);
    }
};

} // namespace

auto lower(const ExprPtr &program, const std::string &module_name) -> IRModule {
    Lowerer l;
    l.mod.name = module_name;
    l.mod.init_fn = "init";
    l.pre_register_math_builtins();
    l.pre_register_fns(program);
    l.lower_main(program);
    l.compute_arity();
    return std::move(l.mod);
}
