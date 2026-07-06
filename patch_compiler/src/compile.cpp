#include "compile.hpp"

#include "backend/backend.hpp"
#include "backend/binaryen/binaryen_backend.hpp"
#include "builtins.hpp"
#include "ir/lower.hpp"
#include "parser/parser.hpp"
#include "parser/tokenizer.hpp"
#include "routing/routing.hpp"
#include "types/type_inference.hpp"

#include <memory>
#include <stdexcept>

namespace {

auto lower_module(const std::string &name, const std::string &src) -> IRModule {
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

    return lower(*ast, name);
}

} // namespace

auto compile_to_binary(float sample_rate, const std::string &patch_json,
                       char *math_bin, size_t math_bin_size) -> CompileResult {
    auto backend = std::make_unique<BinaryenBackend>(math_bin, math_bin_size);

    auto patch = parse_patch_json(patch_json);

    const BackendOptions opts{.sample_rate = static_cast<double>(sample_rate)};
    auto codegen = backend->create_codegen(opts);

    std::vector<IRModule> compiled;
    for (const auto &instr : patch.instruments) {
        for (const auto &[name, src] : instr.module_sources) {
            auto ir = lower_module(name, src);
            codegen->add_module(ir);
            compiled.push_back(std::move(ir));
        }
    }
    for (const auto &[name, src] : patch.global_module_sources) {
        auto ir = lower_module(name, src);
        codegen->add_module(ir);
        compiled.push_back(std::move(ir));
    }

    auto graph = build_routing_graph(patch, std::move(compiled));
    codegen->finalize(graph);

    auto artifact = codegen->build();
    return CompileResult{.bytes = std::move(artifact.bytes),
                         .memory_bytes = artifact.memory_bytes};
}
