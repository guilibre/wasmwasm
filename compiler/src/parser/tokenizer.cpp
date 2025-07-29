#include "tokenizer.hpp"

#include <cctype>
#include <string>

Tokenizer::Tokenizer(std::string_view source) : source(source) {}

[[nodiscard]] auto Tokenizer::is_done() const -> bool {
    return current >= source.size();
}

auto Tokenizer::advance() -> char {
    if (is_done()) return '\0';
    char c = source[current++];
    column++;
    return c;
}

auto Tokenizer::peek_current() const -> char {
    return is_done() ? '\0' : source[current];
}

auto Tokenizer::peek_next() const -> char {
    return current + 1 >= source.size() ? '\0' : source[current + 1];
}

auto Tokenizer::match(char expected) -> bool {
    if (is_done() || source[current] != expected) return false;
    current++;
    column++;
    return true;
}

void Tokenizer::skip_whitespace() {
    while (!is_done()) {
        char c = peek_current();
        switch (c) {
        case ' ':
        case '\r':
        case '\t':
            advance();
            break;
        case '#':
            while (!is_done() && peek_current() != '\n')
                advance();
            break;
        default:
            return;
        }
    }
}

auto Tokenizer::make_token(TokenKind kind) -> Token {
    return Token{
        .kind = kind,
        .lexeme = std::string(source.substr(start, current - start)),
        .line = line,
        .column = column - (current - start),
    };
}

auto Tokenizer::error_token(const std::string &msg) -> Token {
    return Token{
        .kind = TokenKind::Invalid,
        .lexeme = msg,
        .line = line,
        .column = column,
    };
}

auto Tokenizer::scan_identifier() -> Token {
    while ((std::isalnum(peek_current()) != 0) || peek_current() == '_')
        advance();
    return make_token(TokenKind::Identifier);
}

auto Tokenizer::scan_number() -> Token {
    while (std::isdigit(peek_current()) != 0)
        advance();

    if (peek_current() == '.' && std::isdigit(peek_next()) != 0) {
        advance();
        while (std::isdigit(peek_current()) != 0)
            advance();
    }
    return make_token(TokenKind::Number);
}

auto Tokenizer::peek_token() const -> Token {
    auto copy = *this;
    return copy.next();
}

auto Tokenizer::next() -> Token {
    skip_whitespace();
    start = current;

    if (is_done()) return make_token(TokenKind::Eof);

    char c = advance();

    if ((std::isalpha(c) != 0) || c == '_') return scan_identifier();
    if (std::isdigit(c) != 0) return scan_number();

    switch (c) {
    case '+':
    case '-':
        return make_token(TokenKind::Additive);
    case '*':
    case '/':
        return make_token(TokenKind::Multiplicative);
    case '>':
        return make_token(TokenKind::Arrow);
    case ':':
        return make_token(TokenKind::Colon);
    case ',':
        return make_token(TokenKind::Comma);
    case '.':
        return make_token(TokenKind::Period);
    case '{':
        return make_token(TokenKind::LBrace);
    case '}':
        return make_token(TokenKind::RBrace);
    case '(':
        return make_token(TokenKind::LParen);
    case ')':
        return make_token(TokenKind::RParen);
    case '\n':
        line++;
        column = 1;
        return make_token(TokenKind::Eol);
    default:
        return error_token("unexpected character");
    }
}

auto Token::to_string() const -> std::string {
    std::string kind_string;
    switch (kind) {
    case TokenKind::Additive:
    case TokenKind::Multiplicative:
        kind_string = lexeme;
        break;
    case TokenKind::Arrow:
        kind_string = ">";
        break;
    case TokenKind::Colon:
        kind_string = ":";
        break;
    case TokenKind::Comma:
        kind_string = ",";
        break;
        kind_string = "*";
        break;
    case TokenKind::Period:
        kind_string = ".";
        break;
    case TokenKind::LBrace:
        kind_string = "{";
        break;
    case TokenKind::RBrace:
        kind_string = "}";
        break;
    case TokenKind::LParen:
        kind_string = "(";
        break;
    case TokenKind::RParen:
        kind_string = ")";
        break;
    case TokenKind::Eol:
        kind_string = "eol";
        break;
    case TokenKind::Eof:
        kind_string = "eof";
        break;
    case TokenKind::Invalid:
        kind_string = "unknown";
        break;
    default:
        kind_string = lexeme;
        break;
    }
    return "'" + kind_string + "' | (" + std::to_string(line) + "," +
           std::to_string(column) + ")";
}