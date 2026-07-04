#include "tokenizer.hpp"

#include <cctype>
#include <string>
#include <string_view>

Tokenizer::Tokenizer(std::string_view source) : source(source) {}

[[nodiscard]] auto Tokenizer::is_done() const -> bool {
    return current >= source.size();
}

auto Tokenizer::advance() -> char {
    if (is_done()) return '\0';
    const auto c = source[current++];
    if (c == '\n') {
        line++;
        column = 0;
    } else {
        column++;
    }
    return c;
}

auto Tokenizer::peek_current() const -> char {
    return is_done() ? '\0' : source[current];
}

void Tokenizer::skip_whitespace() {
    while (!is_done()) {
        const char c = peek_current();
        if (c == ' ' || c == '\t' || c == '\r')
            advance();
        else
            break;
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
    while (true) {
        const char c = peek_current();
        if (std::isalnum(static_cast<unsigned char>(c)) != 0 || c == '_')
            advance();
        else
            break;
    }

    const auto text = source.substr(start, current - start);

    if (text == "R") return make_token(TokenKind::Rest);
    if (text == "play") return make_token(TokenKind::Play);

    const char first = text.front();
    const bool valid_letter =
        (first >= 'A' && first <= 'G') || (first >= 'a' && first <= 'g');
    if (valid_letter) {
        size_t pos = 1;
        if (pos < text.size() && (text[pos] == '#' || text[pos] == 'b')) pos++;
        while (pos < text.size() &&
               std::isdigit(static_cast<unsigned char>(text[pos])) != 0)
            pos++;
        if (pos == text.size()) return make_token(TokenKind::Note);
    }

    return make_token(TokenKind::Ident);
}

auto Tokenizer::scan_number() -> Token {
    while (std::isdigit(static_cast<unsigned char>(peek_current())) != 0)
        advance();
    return make_token(TokenKind::Number);
}

auto Tokenizer::next() -> Token {
    skip_whitespace();
    start = current;

    if (is_done()) return make_token(TokenKind::Eof);

    const auto c = advance();

    if (c == '\n') {
        while (!is_done()) {
            const char n = peek_current();
            if (n == '\n' || n == ' ' || n == '\t' || n == '\r')
                advance();
            else
                break;
        }
        return make_token(TokenKind::Newline);
    }

    if (std::isalpha(static_cast<unsigned char>(c)) != 0)
        return scan_identifier();
    if (std::isdigit(static_cast<unsigned char>(c)) != 0) return scan_number();

    switch (c) {
    case '(':
        return make_token(TokenKind::LParen);
    case ')':
        return make_token(TokenKind::RParen);
    case '|':
        return make_token(TokenKind::Pipe);
    case '=':
        return make_token(TokenKind::Equals);
    case '@':
        return make_token(TokenKind::At);
    case '/':
        return make_token(TokenKind::Slash);
    default:
        return error_token("unexpected character '" + std::string(1, c) + "'");
    }
}

auto Token::to_string() const -> std::string {
    std::string kind_string;
    switch (kind) {
    case TokenKind::Play:
    case TokenKind::Note:
    case TokenKind::Rest:
    case TokenKind::Ident:
    case TokenKind::Number:
        kind_string = lexeme;
        break;
    case TokenKind::LParen:
        kind_string = "(";
        break;
    case TokenKind::RParen:
        kind_string = ")";
        break;
    case TokenKind::Pipe:
        kind_string = "|";
        break;
    case TokenKind::Equals:
        kind_string = "=";
        break;
    case TokenKind::At:
        kind_string = "@";
        break;
    case TokenKind::Slash:
        kind_string = "/";
        break;
    case TokenKind::Newline:
        kind_string = "\\n";
        break;
    case TokenKind::Eof:
        kind_string = "eof";
        break;
    case TokenKind::Invalid:
        kind_string = lexeme;
        break;
    }
    return "'" + kind_string + "' | (" + std::to_string(line) + "," +
           std::to_string(column) + ")";
}
