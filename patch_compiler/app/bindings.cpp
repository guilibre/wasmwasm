#include "compile.hpp"
#include "lsp/lsp.hpp"
#include <emscripten/bind.h>
#include <emscripten/val.h>

namespace {

auto run_compiler_js(float sample_rate, const std::string &patch_json,
                     const emscripten::val &math_bin) -> emscripten::val {
    auto math_data =
        emscripten::convertJSArrayToNumberVector<uint8_t>(math_bin);

    try {
        auto artifact = compile_to_binary(
            sample_rate, patch_json, reinterpret_cast<char *>(math_data.data()),
            math_data.size());
        auto view = emscripten::typed_memory_view(artifact.bytes.size(),
                                                  artifact.bytes.data());
        auto result = emscripten::val::object();
        result.set("bytes", emscripten::val::global("Uint8Array").new_(view));
        result.set("memory_bytes", artifact.memory_bytes);
        return result;
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
