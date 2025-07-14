#include "tokenizer.hpp"

#include <cctype>
#include <string>

auto Token::to_string() const -> std::string {
    std::string kind_string;
    switch (kind) {
    case TokenKind::Arrow:
        kind_string = "->";
        break;
    case TokenKind::Plus:
        kind_string = "+";
        break;
    case TokenKind::Minus:
        kind_string = "-";
        break;
    case TokenKind::Star:
        kind_string = "*";
        break;
    case TokenKind::Slash:
        kind_string = "/";
        break;
    case TokenKind::LParen:
        kind_string = "(";
        break;
    case TokenKind::RParen:
        kind_string = ")";
        break;
    case TokenKind::LBra:
        kind_string = "{";
        break;
    case TokenKind::RBra:
        kind_string = "}";
        break;
    case TokenKind::Period:
        kind_string = ".";
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
    return kind_string + "|(" + std::to_string(line) + "," +
           std::to_string(column) + ")";
}

Tokenizer::Tokenizer(std::string_view src) : source(src) {}

[[nodiscard]] auto Tokenizer::is_done() const -> bool {
    return current >= source.size();
}

auto Tokenizer::advance() -> char {
    if (is_done()) return '\0';
    char c = source[current++];
    column++;
    return c;
}

auto Tokenizer::peek_char() const -> char {
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
        char c = peek_char();
        switch (c) {
        case ' ':
        case '\r':
        case '\t':
            advance();
            break;
        case '\n':
            advance();
            line++;
            column = 1;
            break;
        case '/':
            if (peek_next() == '/')
                while (!is_done() && peek_char() != '\n')
                    advance();
            else
                return;
            break;
        default:
            return;
        }
    }
}

auto Tokenizer::make_token(TokenKind kind) -> Token {
    return Token{.kind = kind,
                 .lexeme = std::string(source.substr(start, current - start)),
                 .line = line,
                 .column = column - (current - start)};
}

auto Tokenizer::error_token(const std::string &msg) -> Token {
    return Token{.kind = TokenKind::Invalid,
                 .lexeme = msg,
                 .line = line,
                 .column = column};
}

auto Tokenizer::scan_identifier() -> Token {
    while ((std::isalnum(peek_char()) != 0) || peek_char() == '_')
        advance();
    return make_token(TokenKind::Identifier);
}

auto Tokenizer::scan_number() -> Token {
    while (std::isdigit(peek_char()) != 0)
        advance();
    if (peek_char() == '.' && std::isdigit(peek_next()) != 0) {
        advance();
        while (std::isdigit(peek_char()) != 0)
            advance();
    }
    return make_token(TokenKind::Number);
}

auto Tokenizer::peek() const -> Token {
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
        return make_token(TokenKind::Plus);
    case '-':
        if (match('>')) return make_token(TokenKind::Arrow);
        return make_token(TokenKind::Minus);
    case '*':
        return make_token(TokenKind::Star);
    case '/':
        return make_token(TokenKind::Slash);
    case '(':
        return make_token(TokenKind::LParen);
    case ')':
        return make_token(TokenKind::RParen);
    case '{':
        return make_token(TokenKind::LBra);
    case '}':
        return make_token(TokenKind::RBra);
    case '.':
        return make_token(TokenKind::Period);
    default:
        return error_token("unexpected character");
    }
}
