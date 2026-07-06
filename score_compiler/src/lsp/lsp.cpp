#include "lsp.hpp"

#include "backend/json_writer.hpp"
#include "compile.hpp"
#include "parser/parser.hpp"
#include "parser/tokenizer.hpp"
#include "resolve/resolver.hpp"

auto lsp_tokens(const std::string &src) -> std::string {
    Tokenizer tokenizer(src);
    std::string out = "[";
    bool first = true;
    while (!tokenizer.is_done()) {
        Token tok = tokenizer.next();
        if (!first) out += ",";
        first = false;
        out += "{\"kind\":" + std::to_string(static_cast<int>(tok.kind));
        out += ",\"lexeme\":" + json_string(tok.lexeme);
        out += ",\"line\":" + std::to_string(tok.line);
        out += ",\"column\":" + std::to_string(tok.column) + "}";
    }
    out += "]";
    return out;
}

auto lsp_diagnostics(const std::string &src) -> std::string {
    try {
        (void)compile_to_json(src);
        return "[]";
    } catch (const ParseException &e) {
        return "[{\"line\":" + std::to_string(e.line) +
               ",\"column\":" + std::to_string(e.col) +
               ",\"message\":" + json_string(e.what()) + "}]";
    } catch (const ResolveException &e) {
        return "[{\"line\":" + std::to_string(e.line) +
               ",\"column\":" + std::to_string(e.col) +
               ",\"message\":" + json_string(e.what()) + "}]";
    }
}

auto lsp_completions(const std::string & /*src*/, size_t /*line*/,
                     size_t /*col*/) -> std::string {
    return "[]";
}

auto lsp_hover(const std::string & /*src*/, size_t /*line*/, size_t /*col*/)
    -> std::string {
    return "null";
}
