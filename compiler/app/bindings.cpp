#include "binaryen-c.h"
#include "builtins.hpp"
#include "ir/emit.hpp"
#include "ir/lower.hpp"
#include "lsp/lsp.hpp"
#include "parser/parser.hpp"
#include "parser/tokenizer.hpp"
#include "routing/routing.hpp"
#include "types/type_inference.hpp"
#include <emscripten/bind.h>
#include <emscripten/val.h>
#include <functional>
#include <stdexcept>
#include <vector>

namespace {

std::function<void(const ExprPtr &)> make_monomorphize(
    const std::function<TypePtr(const TypePtr &)> &monomorphize_fun_type) {
    std::function<void(const ExprPtr &)> monomorphize =
        [&](const auto &expr) -> auto {
        std::visit(
            [&](const auto &node) -> auto {
                using T = std::decay_t<decltype(node)>;
                if constexpr (std::is_same_v<T, Bind>) monomorphize(node.value);
                if constexpr (std::is_same_v<T, BufferWrite>)
                    monomorphize(node.value);
                if constexpr (std::is_same_v<T, BufferCtor>)
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
    Tokenizer tok(src);
    Parser parser(tok);
    auto ast = parser.parse_code();
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

    auto ir = lower(*ast, name);
    ir.memory_base = memory_base;

    emit_ir(ir, main_module, math_module, sample_rate);
    return ir;
}

std::vector<char> compile_to_binary(float sample_rate,
                                    const std::string &patch_json,
                                    char *math_bin, size_t math_bin_size) {
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
    uint32_t next_mem = buffer_memory_start;
    for (const auto &[name, src] : patch.module_sources) {
        auto ir = compile_module(name, src, math_module,
                                 static_cast<double>(sample_rate), main_module,
                                 next_mem);
        next_mem += ir.total_buffer_bytes();
        compiled.push_back(std::move(ir));
    }

    auto graph = build_routing_graph(std::move(patch), std::move(compiled));
    emit_main_loop(graph, main_module);

    if (!BinaryenModuleValidate(main_module))
        throw std::runtime_error("invalid module");

    BinaryenSetOptimizeLevel(3);
    BinaryenSetShrinkLevel(0);
    BinaryenSetFastMath(true);
    BinaryenSetLowMemoryUnused(true);
    BinaryenModuleOptimize(main_module);

    auto buffer = BinaryenModuleAllocateAndWrite(main_module, nullptr);
    auto binary_ptr =
        std::unique_ptr<void, decltype(&free)>(buffer.binary, free);
    auto *binary_data = static_cast<char *>(buffer.binary);
    std::vector<char> result(binary_data, binary_data + buffer.binaryBytes);
    BinaryenModuleDispose(main_module);
    BinaryenModuleDispose(math_module);
    return result;
}

auto run_compiler_js(float sample_rate, const std::string &patch_json,
                     const emscripten::val &math_bin) -> emscripten::val {
    std::vector<uint8_t> math_data =
        emscripten::convertJSArrayToNumberVector<uint8_t>(math_bin);

    try {
        auto binary = compile_to_binary(
            sample_rate, patch_json, reinterpret_cast<char *>(math_data.data()),
            math_data.size());
        auto view = emscripten::typed_memory_view(binary.size(), binary.data());
        return emscripten::val::global("Uint8Array").new_(view);
    } catch (const std::exception &e) {
        emscripten::val::global("Error")
            .new_(emscripten::val(e.what()))
            .throw_();
        return emscripten::val::undefined();
    }
}

} // namespace

EMSCRIPTEN_BINDINGS(wasmwasm) {
    emscripten::function("run_compiler", &run_compiler_js);
    emscripten::function("lsp_diagnostics", &lsp_diagnostics);
    emscripten::function("lsp_tokens", &lsp_tokens);
    emscripten::function("lsp_completions", &lsp_completions);
    emscripten::function("lsp_hover", &lsp_hover);
}
