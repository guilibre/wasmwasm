#include "compile.hpp"

#include "binaryen-c.h"
#include "builtins.hpp"
#include "ir/emit.hpp"
#include "ir/lower.hpp"
#include "parser/parser.hpp"
#include "parser/tokenizer.hpp"
#include "routing/routing.hpp"
#include "types/type_inference.hpp"

#include <functional>
#include <memory>
#include <stdexcept>

namespace {

auto make_monomorphize(
    const std::function<TypePtr(const TypePtr &)> &monomorphize_fun_type)
    -> std::function<void(const ExprPtr &)> {
    std::function<void(const ExprPtr &)> monomorphize =
        [&](const auto &expr) -> auto {
        std::visit(
            [&](const auto &node) -> auto {
                using T = std::decay_t<decltype(node)>;
                if constexpr (std::is_same_v<T, Bind>) monomorphize(node.value);
                if constexpr (std::is_same_v<T, StaticBind>)
                    monomorphize(node.init);
                if constexpr (std::is_same_v<T, ParamBind>)
                    monomorphize(node.default_val);
                if constexpr (std::is_same_v<T, DelayWrite>)
                    monomorphize(node.value);
                if constexpr (std::is_same_v<T, DelayWriteQuiet>) {
                    if (node.delay) monomorphize(*node.delay);
                    monomorphize(node.value);
                }
                if constexpr (std::is_same_v<T, DelayCtor>)
                    monomorphize(node.init_fn);
                if constexpr (std::is_same_v<T, OutputWrite>)
                    monomorphize(node.value);
                if constexpr (std::is_same_v<T, CodeBlock>) {
                    for (const auto &e : node.expressions) monomorphize(e);
                    expr->type = monomorphize_fun_type(expr->type);
                }
                if constexpr (std::is_same_v<T, Call>) {
                    monomorphize(node.callee);
                    monomorphize(node.argument);
                    expr->type = monomorphize_fun_type(expr->type);
                }
                if constexpr (std::is_same_v<T, Lambda>) {
                    monomorphize(node.body);
                    expr->type = monomorphize_fun_type(expr->type);
                }
                if constexpr (std::is_same_v<T, Variable>) {
                    if (std::holds_alternative<TypeVar>(expr->type->node))
                        expr->type = Type::make<TypeBase>(BaseTypeKind::Float);
                    if (std::holds_alternative<TypeFun>(expr->type->node))
                        expr->type = monomorphize_fun_type(expr->type);
                }
            },
            expr->node);
    };
    return monomorphize;
}

auto compile_module(const std::string &name, const std::string &src,
                    BinaryenModuleRef math_module, double sample_rate,
                    BinaryenModuleRef main_module, uint32_t memory_base)
    -> IRModule {
    const Tokenizer tok(src);
    Parser parser(tok);
    const auto ast = parser.parse_code();
    if (!ast)
        throw std::runtime_error("[" + name +
                                 "] parse error: " + ast.error().msg);

    auto env = make_builtin_env();
    Substitution subst;
    TypeGenerator gen;
    infer_expr(*ast, env, subst, gen);

    std::function<TypePtr(const TypePtr &)> mono_type =
        [&](const auto &type) -> TypePtr {
        return std::visit(
            [&](const auto &node) -> auto {
                using T = std::decay_t<decltype(node)>;
                if constexpr (std::is_same_v<T, TypeVar>)
                    return Type::make<TypeBase>(BaseTypeKind::Float);
                if constexpr (std::is_same_v<T, TypeFun>)
                    return Type::make<TypeFun>(mono_type(node.param),
                                               mono_type(node.result));
                return type;
            },
            type->node);
    };
    make_monomorphize(mono_type)(*ast);

    auto ir = lower(*ast, name, memory_base);

    emit_ir(ir, main_module, math_module, sample_rate);
    return ir;
}

} // namespace

auto compile_to_binary(float sample_rate, const std::string &patch_json,
                       char *math_bin, size_t math_bin_size)
    -> std::vector<char> {
    auto *math_module = BinaryenModuleReadWithFeatures(math_bin, math_bin_size,
                                                       BinaryenFeatureAll());
    if (math_module == nullptr)
        throw std::runtime_error("failed to parse math module");
    if (!BinaryenModuleValidate(math_module))
        throw std::runtime_error("math module is invalid");

    auto patch = parse_patch_json(patch_json);

    auto *main_module = BinaryenModuleCreate();
    if (main_module == nullptr)
        throw std::runtime_error("unable to create binaryen module");

    std::vector<IRModule> compiled;
    auto next_mem = delay_memory_start;
    for (const auto &[name, src] : patch.module_sources) {
        auto ir = compile_module(name, src, math_module,
                                 static_cast<double>(sample_rate), main_module,
                                 next_mem);
        next_mem += ir.total_bytes();
        compiled.push_back(std::move(ir));
    }

    auto graph = build_routing_graph(patch, std::move(compiled));
    emit_main_loop(graph, main_module);

    if (!BinaryenModuleValidate(main_module))
        throw std::runtime_error("invalid module");

    BinaryenSetOptimizeLevel(3);
    BinaryenSetShrinkLevel(0);
    BinaryenSetFastMath(true);
    BinaryenSetLowMemoryUnused(true);
    BinaryenSetAlwaysInlineMaxSize(100);
    BinaryenSetFlexibleInlineMaxSize(250);
    BinaryenSetOneCallerInlineMaxSize(250);
    BinaryenModuleOptimize(main_module);

    auto delay = BinaryenModuleAllocateAndWrite(main_module, nullptr);
    auto binary_ptr =
        std::unique_ptr<void, decltype(&free)>(delay.binary, free);
    auto *binary_data = static_cast<char *>(delay.binary);
    std::vector<char> result(binary_data, binary_data + delay.binaryBytes);
    BinaryenModuleDispose(main_module);
    BinaryenModuleDispose(math_module);
    return result;
}
