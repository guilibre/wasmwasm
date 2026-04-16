#include "lower.hpp"

#include "../ast/ast.hpp"
#include "../builtins.hpp"
#include "../types/type.hpp"
#include "ir.hpp"
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
    int arity{0};
};

struct Lowerer {
    IRModule mod;
    std::vector<IRInstr> *cur = nullptr;

    std::unordered_map<std::string, FnInfo> fns;
    std::unordered_set<std::string> bufs;
    std::unordered_map<std::string, IRType> locals;

    int tmp_n = 0;

    auto tmp() -> std::string { return "$t" + std::to_string(tmp_n++); }
    void emit(IRInstr i) const { cur->emplace_back(std::move(i)); }
    void define(const std::string &name, IRType type) { locals[name] = type; }

    auto is_special(const std::string &name) const -> bool {
        return std::ranges::contains(language_globals, name) ||
               std::ranges::contains(math_builtins, name) ||
               fns.contains(name) || bufs.contains(name);
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
                if constexpr (std::is_same_v<T, BufferRead>) return {};
                if constexpr (std::is_same_v<T, BufferWrite>)
                    return free_vars_of(node.value, bound);

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
            .arity = static_cast<int>(params.size()),
        };

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

                    if (name == "TIME" || name == "SAMPLE_RATE")
                        return emit_global_read(name, IRType::Float);

                    if (fns.contains(name))
                        throw std::runtime_error("Function '" + name +
                                                 "' used as value (partial "
                                                 "application not supported)");

                    return IRLocalRef{name};
                }

                if constexpr (std::is_same_v<T, BufferRead>) {
                    const auto &name = node.name.lexeme;
                    if (!bufs.contains(name))
                        throw std::runtime_error("@" + name +
                                                 " is not a buffer");
                    auto r = tmp();
                    emit(IRBufferRead{.result = r, .buffer = name});
                    return IRLocalRef{r};
                }

                if constexpr (std::is_same_v<T, BufferWrite>) {
                    const auto &target = node.target.lexeme;
                    const auto val = lower_expr(node.value);
                    if (!val)
                        throw std::runtime_error("assigning void to buffer");
                    if (target == "OUT") {
                        emit(IRAssign{
                            .result = "OUT",
                            .value = *val,
                            .type = IRType::Float,
                        });
                    } else {
                        if (!bufs.contains(target))
                            throw std::runtime_error(target +
                                                     " is not a buffer");
                        emit(IRBufferWrite{.buffer = target, .value = *val});
                    }
                    return std::nullopt;
                }

                if constexpr (std::is_same_v<T, UnaryOp>) {
                    const auto val = lower_expr(node.expr);
                    if (!val) throw std::runtime_error("unary op on void");
                    auto r = tmp();
                    emit(IRUnaryNeg{r, *val});
                    return IRLocalRef{r};
                }

                if constexpr (std::is_same_v<T, BinaryOp>) {
                    const auto l = lower_expr(node.left);
                    const auto r = lower_expr(node.right);
                    if (!l || !r) throw std::runtime_error("binary op on void");
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

                    if (std::holds_alternative<BufferCtor>(node.value->node)) {
                        const auto &ctor =
                            std::get<BufferCtor>(node.value->node);
                        if (!std::holds_alternative<Lambda>(ctor.init_fn->node))
                            throw std::runtime_error(
                                "Buffer init must be a lambda");
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
                        mod.buffers.push_back({
                            .name = name,
                            .size_elements = ctor.size,
                            .init_fn = init_name,
                        });
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

                        throw std::runtime_error("Unknown function: " + name);
                    }

                    throw std::runtime_error("Dynamic dispatch not supported");
                }

                if constexpr (std::is_same_v<T, BufferCtor>)
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
        fn.return_type = IRType::Float;

        auto *saved_cur = cur;
        cur = &fn.body;

        define("OUT", IRType::Float);
        emit(IRAssign{
            .result = "OUT",
            .value = IRLiteral{0.0},
            .type = IRType::Float,
        });

        lower_expr(main_ast);

        emit(IRReturn{IRLocalRef{"OUT"}});

        cur = saved_cur;
        mod.functions.emplace_back(std::move(fn));
        mod.main_fn = "main$body";
    }
};

} // namespace

auto lower(const ExprPtr &program) -> IRModule {
    Lowerer l;
    l.mod.init_fn = "init";
    l.lower_main(program);
    return std::move(l.mod);
}
