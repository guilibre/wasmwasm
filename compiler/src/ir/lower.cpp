#include "lower.hpp"

#include "../ast/ast.hpp"
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

const std::unordered_set<std::string> MATH_BUILTINS = {
    "sin", "cos", "sign", "fract", "clip", "exp",
};

const std::unordered_set<std::string> LANGUAGE_GLOBALS = {
    "TIME",
    "PI",
    "SAMPLE_RATE",
    "OUT",
};

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
        return LANGUAGE_GLOBALS.contains(name) ||
               MATH_BUILTINS.contains(name) || fns.contains(name) ||
               bufs.contains(name);
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
                    auto r = free_vars_of(node.right, bound);
                    l.insert(r.begin(), r.end());
                    return l;
                }
                if constexpr (std::is_same_v<T, UnaryOp>)
                    return free_vars_of(node.expr, bound);
                if constexpr (std::is_same_v<T, Call>) {
                    auto l = free_vars_of(node.callee, bound);
                    auto r = free_vars_of(node.argument, bound);
                    l.insert(r.begin(), r.end());
                    return l;
                }
                if constexpr (std::is_same_v<T, Lambda>) {
                    auto b2 = bound;
                    b2.insert(node.parameter.lexeme);
                    return free_vars_of(node.body, b2);
                }
                if constexpr (std::is_same_v<T, Block>) {
                    std::unordered_set<std::string> result;
                    auto inner = bound;
                    for (const auto &expr : node.expressions) {
                        auto fv = free_vars_of(expr, inner);
                        result.insert(fv.begin(), fv.end());
                        if (const auto *asg =
                                std::get_if<Assignment>(&expr->node))
                            inner.insert(asg->name.lexeme);
                    }
                    return result;
                }
                if constexpr (std::is_same_v<T, Assignment>)
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
        auto fv_set = free_vars_of(body, bound);
        std::vector<std::string> fv_vec(fv_set.begin(), fv_set.end());
        std::ranges::sort(fv_vec);

        auto ret_type = ir_type_of(body->type);

        fns[name] = FnInfo{
            .free_vars = fv_vec,
            .return_type = ret_type,
            .arity = static_cast<int>(params.size()),
        };

        IRFunction fn;
        fn.name = name;
        fn.return_type = ret_type;
        for (const auto &p : params)
            fn.params.emplace_back(p, IRType::Float);
        for (const auto &fv : fv_vec)
            fn.params.emplace_back(fv, locals.contains(fv) ? locals.at(fv)
                                                           : IRType::Float);

        auto *saved_cur = cur;
        auto saved_locals = locals;
        cur = &fn.body;
        for (const auto &p : params)
            locals[p] = IRType::Float;

        auto result = lower_expr(body);
        if (result && ret_type != IRType::Void) emit(IRReturn{result});

        cur = saved_cur;
        locals = saved_locals;

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

                    if (name == "TIME") {
                        auto r = tmp();
                        emit(IRGlobalRead{
                            .result = r,
                            .name = "TIME",
                            .type = IRType::Float,
                        });
                        return IRLocalRef{r};
                    }

                    if (name == "SAMPLE_RATE") {
                        auto r = tmp();
                        emit(IRGlobalRead{
                            .result = r,
                            .name = "SAMPLE_RATE",
                            .type = IRType::Float,
                        });
                        return IRLocalRef{r};
                    }

                    if (bufs.contains(name)) {
                        auto r = tmp();
                        emit(IRBufferRead{
                            .result = r,
                            .buffer = name,
                        });
                        return IRLocalRef{r};
                    }

                    if (fns.contains(name))
                        throw std::runtime_error("Function '" + name +
                                                 "' used as value (partial "
                                                 "application not supported)");

                    return IRLocalRef{name};
                }

                if constexpr (std::is_same_v<T, UnaryOp>) {
                    auto val = lower_expr(node.expr);
                    if (!val) throw std::runtime_error("unary op on void");
                    auto r = tmp();
                    emit(IRUnaryNeg{r, *val});
                    return IRLocalRef{r};
                }

                if constexpr (std::is_same_v<T, BinaryOp>) {
                    auto l = lower_expr(node.left);
                    auto r = lower_expr(node.right);
                    if (!l || !r) throw std::runtime_error("binary op on void");
                    auto res = tmp();
                    emit(IRBinOp{res, node.op, *l, *r});
                    return IRLocalRef{res};
                }

                if constexpr (std::is_same_v<T, Assignment>) {
                    const auto &name = node.name.lexeme;

                    if (std::holds_alternative<Lambda>(node.value->node)) {
                        lift(name, node.value);
                        return std::nullopt;
                    }

                    if (bufs.contains(name)) {
                        auto val = lower_expr(node.value);
                        if (!val)
                            throw std::runtime_error(
                                "assigning void to buffer");
                        emit(IRBufferWrite{
                            .buffer = name,
                            .value = *val,
                        });
                        return std::nullopt;
                    }

                    auto val = lower_expr(node.value);
                    if (!val) return std::nullopt;
                    auto type = ir_type_of(node.value->type);
                    define(name, type);
                    emit(IRAssign{
                        .result = name,
                        .value = *val,
                        .type = type,
                    });
                    return std::nullopt;
                }

                if constexpr (std::is_same_v<T, Block>) {
                    std::optional<IRValue> last;
                    for (const auto &expr : node.expressions)
                        last = lower_expr(expr);
                    return last;
                }

                if constexpr (std::is_same_v<T, Call>) {
                    auto [callee_node, arg_ptrs] = flatten_calls(e);

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

                        if (MATH_BUILTINS.contains(name)) {
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
                            auto r = tmp();
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

                if constexpr (std::is_same_v<T, Buffer>)
                    throw std::runtime_error(
                        "Buffer declaration in unexpected context");

                return std::nullopt;
            },
            e->node);
    }

    void lower_init(const ExprPtr &init_ast) {
        mod.init_fn = "init";
        if (!std::holds_alternative<Block>(init_ast->node)) return;
        const auto &block = std::get<Block>(init_ast->node);

        for (const auto &expr : block.expressions) {
            if (const auto *buf_node = std::get_if<Buffer>(&expr->node)) {
                const auto &buf = *buf_node;
                if (!std::holds_alternative<Lambda>(
                        buf.init_buffer_function->node))
                    throw std::runtime_error("Buffer init must be a lambda");

                const auto &lam =
                    std::get<Lambda>(buf.init_buffer_function->node);
                std::string init_name = buf.name + "$init";

                IRFunction init_fn;
                init_fn.name = init_name;
                init_fn.return_type = IRType::Float;
                init_fn.params.emplace_back(lam.parameter.lexeme,
                                            IRType::Float);

                auto *saved_cur = cur;
                auto saved_locals = locals;
                cur = &init_fn.body;
                locals[lam.parameter.lexeme] = IRType::Float;

                auto result = lower_expr(lam.body);
                if (result) emit(IRReturn{result});

                cur = saved_cur;
                locals = saved_locals;

                mod.functions.emplace_back(std::move(init_fn));
                bufs.insert(buf.name);
                mod.buffers.push_back({
                    .name = buf.name,
                    .size_elements = buf.size,
                    .init_fn = init_name,
                });
            }
        }
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

auto lower(const ExprPtr &init, const ExprPtr &main) -> IRModule {
    Lowerer l;
    l.lower_init(init);
    l.lower_main(main);
    return std::move(l.mod);
}
