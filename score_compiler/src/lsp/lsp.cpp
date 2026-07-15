#include "lsp.hpp"

#include "ast/binop_eval.hpp"
#include "backend/json_writer.hpp"
#include "parser/parser.hpp"
#include "parser/tokenizer.hpp"
#include "resolve/resolver.hpp"
#include <unordered_set>

namespace {

auto is_keyword_ident(const std::string &lexeme) -> bool {
    static const std::unordered_set<std::string> keywords = {"true", "false"};
    return keywords.contains(lexeme);
}

} // namespace

auto lsp_tokens(const std::string &src) -> std::string {
    std::string json;
    json.reserve(4096);
    json += '[';
    bool first = true;

    auto emit = [&](size_t line, size_t column, size_t len,
                    const char *type) -> void {
        if (!first) json += ',';
        first = false;
        json += R"({"line":)" + std::to_string(line - 1) + R"(,"col":)" +
                std::to_string(column - 1) + R"(,"len":)" +
                std::to_string(len) + R"(,"type":")" + std::string(type) +
                R"("})";
    };

    Tokenizer tokenizer(src);
    while (!tokenizer.is_done()) {
        const auto tok = tokenizer.next();
        const auto len = tok.lexeme.size();
        switch (tok.kind) {
        case TokenKind::Number:
            emit(tok.line, tok.column, len, "number");
            break;
        case TokenKind::String:
            emit(tok.line, tok.column, len + 2, "string");
            break;
        case TokenKind::KwPlay:
        case TokenKind::KwReverse:
        case TokenKind::KwRepeat:
        case TokenKind::KwChoose:
        case TokenKind::KwEmit:
        case TokenKind::KwListen:
        case TokenKind::KwNull:
        case TokenKind::KwConst:
        case TokenKind::KwSkip:
            emit(tok.line, tok.column, len, "keyword");
            break;
        case TokenKind::Ident:
            if (is_keyword_ident(tok.lexeme))
                emit(tok.line, tok.column, len, "keyword");
            else
                emit(tok.line, tok.column, len, "variable");
            break;
        case TokenKind::Plus:
        case TokenKind::Minus:
        case TokenKind::Star:
        case TokenKind::Slash:
        case TokenKind::Percent:
        case TokenKind::Equals:
        case TokenKind::Colon:
        case TokenKind::Semicolon:
        case TokenKind::Ampersand:
        case TokenKind::At:
        case TokenKind::Tilde:
        case TokenKind::Caret:
        case TokenKind::Pipe:
        case TokenKind::Or:
        case TokenKind::Question:
        case TokenKind::EqEq:
        case TokenKind::NotEq:
        case TokenKind::Less:
        case TokenKind::Greater:
        case TokenKind::LessEq:
        case TokenKind::GreaterEq:
            emit(tok.line, tok.column, len, "operator");
            break;
        default:
            break;
        }
    }

    json += ']';
    return json;
}

auto lsp_diagnostics(const std::string &src) -> std::string {
    try {
        const Tokenizer tokenizer(src);
        Parser parser(tokenizer);
        const auto program = parser.parse();
        (void)expand_program(program);
    } catch (const ParseException &e) {
        return R"([{"msg":)" + json_string(e.what()) + R"(,"line":)" +
               std::to_string(e.line - 1) + R"(,"col":)" +
               std::to_string(e.col - 1) + R"(,"severity":"error"}])";
    } catch (const ResolveException &e) {
        const auto line = e.line > 0 ? e.line - 1 : 0;
        const auto col = e.col > 0 ? e.col - 1 : 0;
        return R"([{"msg":)" + json_string(e.what()) + R"(,"line":)" +
               std::to_string(line) + R"(,"col":)" + std::to_string(col) +
               R"(,"severity":"error"}])";
    } catch (const FoldException &e) {
        const auto line = e.line > 0 ? e.line - 1 : 0;
        const auto col = e.col > 0 ? e.col - 1 : 0;
        return R"([{"msg":)" + json_string(e.what()) + R"(,"line":)" +
               std::to_string(line) + R"(,"col":)" + std::to_string(col) +
               R"(,"severity":"error"}])";
    } catch (const std::exception &e) {
        return R"([{"msg":)" + json_string(e.what()) +
               R"(,"line":0,"col":0,"severity":"error"}])";
    }
    return "[]";
}

auto lsp_completions(const std::string &src, size_t /*line*/, size_t /*col*/)
    -> std::string {
    std::string json;
    json.reserve(1024);
    json += '[';
    bool first = true;

    const auto emit = [&](const std::string &label, const char *detail,
                          const char *kind) -> void {
        if (!first) json += ',';
        first = false;
        json += R"({"label":)" + json_string(label) + R"(,"detail":")" +
                std::string(detail) + R"(","kind":")" + std::string(kind) +
                R"("})";
    };

    emit("play", "", "keyword");
    emit("reverse", "", "keyword");
    emit("repeat", "", "keyword");
    emit("choose", "", "keyword");
    emit("emit", "", "keyword");
    emit("listen", "", "keyword");
    emit("null", "", "keyword");
    emit("true", "", "keyword");
    emit("false", "", "keyword");
    emit("const", "", "keyword");
    emit("skip", "", "keyword");

    const Tokenizer tokenizer(src);
    Parser parser(tokenizer);
    try {
        const auto program = parser.parse();
        for (const auto &decl : program.decls)
            emit(decl.name, "Var", "variable");
    } catch (const ParseException &) { return json + ']'; }

    json += ']';
    return json;
}

auto lsp_hover(const std::string & /*src*/, size_t /*line*/, size_t /*col*/)
    -> std::string {
    return "null";
}
