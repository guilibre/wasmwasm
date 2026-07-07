#include "tokenizer.hpp"

#include <array>
#include <cctype>

namespace {

auto is_alpha(char c) -> bool {
    return std::isalpha(static_cast<unsigned char>(c)) != 0 || c == '_';
}

auto is_digit(char c) -> bool {
    return std::isdigit(static_cast<unsigned char>(c)) != 0;
}

auto is_alnum(char c) -> bool {
    return std::isalnum(static_cast<unsigned char>(c)) != 0 || c == '_';
}

auto make(TokenKind kind, std::string lexeme, size_t line, size_t column)
    -> Token {
    return Token{.kind = kind,
                 .lexeme = std::move(lexeme),
                 .line = line,
                 .column = column};
}

} // namespace

auto Token::to_string() const -> std::string {
    static constexpr std::array<const char *, 18> names = {
        "Ident",  "Number",    "String", "Equals",  "Colon",  "Plus",
        "Minus",  "Star",      "Slash",  "LBrace",  "RBrace", "LParen",
        "RParen", "Ampersand", "KwPlay", "Newline", "Eof",    "Invalid",
    };
    std::string out = names[static_cast<uint8_t>(kind)];
    out += " '" + lexeme + "' @" + std::to_string(line) + ":" +
           std::to_string(column);
    return out;
}

Tokenizer::Tokenizer(std::string_view source) : source(source) {}

auto Tokenizer::peek_current() const -> char {
    if (current >= source.size()) return '\0';
    return source[current];
}

auto Tokenizer::peek_next() const -> char {
    if (current + 1 >= source.size()) return '\0';
    return source[current + 1];
}

auto Tokenizer::advance() -> char {
    char c = source[current++];
    if (c == '\n') {
        line++;
        column = 1;
    } else {
        column++;
    }
    return c;
}

void Tokenizer::skip_whitespace() {
    while (current < source.size()) {
        char c = peek_current();
        if (c == ' ' || c == '\t' || c == '\r') {
            advance();
        } else if (c == '#') {
            while (current < source.size() && peek_current() != '\n') advance();
        } else {
            break;
        }
    }
}

auto Tokenizer::make_token(TokenKind kind) -> Token {
    return make(kind, std::string(source.substr(start, current - start)), line,
                column);
}

auto Tokenizer::error_token(const std::string &msg) const -> Token {
    return make(TokenKind::Invalid, msg, line, column);
}

auto Tokenizer::scan_identifier() -> Token {
    while (current < source.size() && is_alnum(peek_current())) advance();
    std::string lexeme(source.substr(start, current - start));
    if (lexeme == "play") return make(TokenKind::KwPlay, lexeme, line, column);
    return make(TokenKind::Ident, lexeme, line, column);
}

auto Tokenizer::scan_string() -> Token {
    std::string value;
    while (current < source.size() && peek_current() != '"') {
        char c = advance();
        if (c == '\\' && peek_current() == '"')
            value += advance();
        else
            value += c;
    }
    if (current >= source.size())
        return error_token("unterminated string literal");
    advance();
    return make(TokenKind::String, value, line, column);
}

auto Tokenizer::scan_number() -> Token {
    while (current < source.size() && is_digit(peek_current())) advance();
    if (peek_current() == '.' && is_digit(peek_next())) {
        advance();
        while (current < source.size() && is_digit(peek_current())) advance();
    }
    return make_token(TokenKind::Number);
}

auto Tokenizer::is_done() const -> bool { return done; }

auto Tokenizer::next() -> Token {
    skip_whitespace();
    start = current;
    size_t start_line = line;
    size_t start_column = column;

    if (current >= source.size()) {
        done = true;
        return make(TokenKind::Eof, "", start_line, start_column);
    }

    char c = advance();

    if (c == '\n')
        return make(TokenKind::Newline, "\n", start_line, start_column);

    if (is_alpha(c)) {
        Token tok = scan_identifier();
        tok.line = start_line;
        tok.column = start_column;
        return tok;
    }

    if (is_digit(c)) {
        Token tok = scan_number();
        tok.line = start_line;
        tok.column = start_column;
        return tok;
    }

    if (c == '"') {
        Token tok = scan_string();
        tok.line = start_line;
        tok.column = start_column;
        return tok;
    }

    switch (c) {
    case '=':
        return make(TokenKind::Equals, "=", start_line, start_column);
    case ':':
        return make(TokenKind::Colon, ":", start_line, start_column);
    case '+':
        return make(TokenKind::Plus, "+", start_line, start_column);
    case '-':
        return make(TokenKind::Minus, "-", start_line, start_column);
    case '*':
        return make(TokenKind::Star, "*", start_line, start_column);
    case '/':
        return make(TokenKind::Slash, "/", start_line, start_column);
    case '{':
        return make(TokenKind::LBrace, "{", start_line, start_column);
    case '}':
        return make(TokenKind::RBrace, "}", start_line, start_column);
    case '(':
        return make(TokenKind::LParen, "(", start_line, start_column);
    case ')':
        return make(TokenKind::RParen, ")", start_line, start_column);
    case '&':
        return make(TokenKind::Ampersand, "&", start_line, start_column);
    default: {
        Token tok =
            error_token(std::string("unexpected character '") + c + "'");
        tok.line = start_line;
        tok.column = start_column;
        return tok;
    }
    }
}
