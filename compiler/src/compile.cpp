#include "compile.hpp"

#include "binaryen-c.h"
#include "builtins.hpp"
#include "ir/emit.hpp"
#include "ir/lower.hpp"
#include "parser/parser.hpp"
#include "parser/tokenizer.hpp"
#include "routing/routing.hpp"
#include "types/type_inference.hpp"

#include <memory>
#include <stdexcept>

namespace {

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
    pre_register_toplevel(*ast, env);
    infer_expr(*ast, env, subst, gen);
    finalize_types(*ast, subst);

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
