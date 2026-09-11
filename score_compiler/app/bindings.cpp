#include "ast/binop_eval.hpp"
#include "compile.hpp"
#include "diagnostics.hpp"
#include "lsp/lsp.hpp"
#include "parser/parser.hpp"
#include "resolve/resolver.hpp"
#include <emscripten/bind.h>
#include <emscripten/val.h>

namespace {

auto throw_js_error(const std::string &msg, size_t line, size_t col) -> void {
    const auto [norm_line, norm_col] = normalize_position(line, col);
    auto err = emscripten::val::global("Error").new_(emscripten::val(msg));
    err.set("line", static_cast<double>(norm_line));
    err.set("col", static_cast<double>(norm_col));
    err.throw_();
}

auto compile_score_js(const std::string &source) -> emscripten::val {
    try {
        return emscripten::val(compile_to_json(source));
    } catch (const ParseException &e) {
        throw_js_error(e.what(), e.line, e.col);
    } catch (const ResolveException &e) {
        throw_js_error(e.what(), e.line, e.col);
    } catch (const FoldException &e) {
        throw_js_error(e.what(), e.line, e.col);
    } catch (const std::exception &e) {
        throw_js_error(e.what(), 0, 0);
    }
    return emscripten::val::undefined();
}

} // namespace

EMSCRIPTEN_BINDINGS(wasmwasm) {
    emscripten::function("compile_score", &compile_score_js);
    emscripten::function("lsp_diagnostics", &lsp_diagnostics);
    emscripten::function("lsp_tokens", &lsp_tokens);
    emscripten::function("lsp_completions", &lsp_completions);
    emscripten::function("lsp_hover", &lsp_hover);
}
