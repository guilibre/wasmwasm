#include "compile.hpp"
#include "lsp/lsp.hpp"
#include <emscripten/bind.h>
#include <emscripten/val.h>

namespace {

auto compile_score_js(const std::string &source) -> emscripten::val {
    try {
        return emscripten::val(compile_to_json(source));
    } catch (const std::exception &e) {
        emscripten::val::global("Error")
            .new_(emscripten::val(e.what()))
            .throw_();
        return emscripten::val::undefined();
    }
}

} // namespace

EMSCRIPTEN_BINDINGS(wasmwasm) {
    emscripten::function("compile_score", &compile_score_js);
    emscripten::function("lsp_diagnostics", &lsp_diagnostics);
    emscripten::function("lsp_tokens", &lsp_tokens);
    emscripten::function("lsp_completions", &lsp_completions);
    emscripten::function("lsp_hover", &lsp_hover);
}
