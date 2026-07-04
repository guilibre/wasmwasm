#include "lsp.hpp"

#include "parser/parser.hpp"
#include "parser/tokenizer.hpp"
#include "resolve/resolver.hpp"

#include <array>
#include <optional>
#include <string>

namespace {

auto json_escape(const std::string &s) -> std::string {
    std::string out;
    out.reserve(s.size());
    for (const char c : s) {
        if (c == '"')
            out += "\\\"";
        else if (c == '\\')
            out += "\\\\";
        else if (c == '\n')
            out += "\\n";
        else if (c == '\r')
            out += "\\r";
        else
            out += c;
    }
    return out;
}

auto token_kind_label(TokenKind kind) -> const char * {
    switch (kind) {
    case TokenKind::Play:
        return "keyword";
    case TokenKind::Note:
        return "note";
    case TokenKind::Rest:
        return "rest";
    case TokenKind::Ident:
        return "identifier";
    case TokenKind::Number:
        return "number";
    case TokenKind::Equals:
    case TokenKind::At:
    case TokenKind::Slash:
    case TokenKind::Pipe:
        return "operator";
    case TokenKind::LParen:
    case TokenKind::RParen:
        return "punctuation";
    case TokenKind::Newline:
    case TokenKind::Eof:
    case TokenKind::Invalid:
        return nullptr;
    }
    return nullptr;
}

auto token_hover_text(const Token &token) -> const char * {
    switch (token.kind) {
    case TokenKind::Play:
        return "Statement keyword: plays a previously-defined motiv.";
    case TokenKind::Note:
        return "Note: pitch class, optional accidental (# or b), optional "
               "octave, e.g. C4, D#4, Bb3, Eb. Octave is optional: if "
               "omitted, it inherits the current octave.";
    case TokenKind::Rest:
        return "Rest: silence. Inherits the current duration unless given "
               "an explicit @duration (which then updates it for later "
               "notes). Never affects the current octave.";
    case TokenKind::Ident:
        return "Motiv reference: refers to a previously-defined `name = "
               "...`. Resolves with its own independent state.";
    case TokenKind::Number:
        return "Duration component, in beats.";
    case TokenKind::Equals:
        return "Defines a named motiv: `name = <sequence>`.";
    case TokenKind::At:
        return "Separates a note (or group) from its duration.";
    case TokenKind::Slash:
        return "Separates a duration's numerator and denominator.";
    case TokenKind::Pipe:
        return "Chord operator: combines notes to start simultaneously.";
    case TokenKind::LParen:
    case TokenKind::RParen:
        return "Groups a sub-sequence or chord; may take a trailing "
               "@duration overriding its overall time advance.";
    case TokenKind::Newline:
    case TokenKind::Eof:
    case TokenKind::Invalid:
        return nullptr;
    }
    return nullptr;
}

auto token_at(const std::string &src, size_t line, size_t col)
    -> std::optional<Token> {
    Tokenizer tokenizer(src);
    while (true) {
        auto token = tokenizer.next();
        if (token.kind == TokenKind::Eof) break;
        if (token.kind != TokenKind::Invalid && token.line == line &&
            col >= token.column && col < token.column + token.lexeme.size())
            return token;
    }
    return std::nullopt;
}

} // namespace

auto lsp_diagnostics(const std::string &src) -> std::string {
    try {
        const Tokenizer tokenizer(src);
        Parser parser(tokenizer);
        auto program = parser.parse();
        resolve_program(program);
    } catch (const ParseException &e) {
        return R"([{"msg":")" + json_escape(e.what()) + R"(","line":)" +
               std::to_string(e.line) + R"(,"col":)" + std::to_string(e.col) +
               R"(,"severity":"error"}])";
    } catch (const ResolveException &e) {
        return R"([{"msg":")" + json_escape(e.what()) + R"(","line":)" +
               std::to_string(e.line) + R"(,"col":)" + std::to_string(e.col) +
               R"(,"severity":"error"}])";
    } catch (const std::exception &e) {
        return R"([{"msg":")" + json_escape(e.what()) +
               R"(","line":0,"col":0,"severity":"error"}])";
    }

    return "[]";
}

auto lsp_tokens(const std::string &src) -> std::string {
    std::string json;
    json.reserve(1024);
    json += '[';
    bool first = true;

    Tokenizer tokenizer(src);
    while (true) {
        auto token = tokenizer.next();
        if (token.kind == TokenKind::Eof) break;

        const char *label = token_kind_label(token.kind);
        if (label == nullptr) continue;

        if (!first) json += ',';
        first = false;
        json += "{\"line\":" + std::to_string(token.line) +
                ",\"col\":" + std::to_string(token.column) +
                ",\"len\":" + std::to_string(token.lexeme.size()) +
                R"(,"type":")" + label + "\"}";
    }

    json += ']';
    return json;
}

auto lsp_completions(const std::string & /*src*/, size_t /*line*/,
                     size_t /*col*/) -> std::string {
    struct Keyword {
        const char *label;
        const char *detail;
    };
    static constexpr std::array<Keyword, 1> keywords = {{
        {.label = "play", .detail = "Plays a previously-defined motiv"},
    }};

    std::string json;
    json.reserve(256);
    json += '[';
    bool first = true;
    for (const auto &kw : keywords) {
        if (!first) json += ',';
        first = false;
        json += R"({"label":")" + std::string(kw.label) + R"(","detail":")" +
                std::string(kw.detail) + R"(","kind":"keyword"})";
    }
    json += ']';
    return json;
}

auto lsp_hover(const std::string &src, size_t line, size_t col) -> std::string {
    auto token = token_at(src, line, col);
    if (!token) return "null";

    const char *text = token_hover_text(*token);
    if (text == nullptr) return "null";

    return R"({"text":")" + json_escape(text) + "\"}";
}
