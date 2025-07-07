// tokenizer.hpp
#pragma once

#include <cstdint>
#include <string>
#include <string_view>

enum class TokenKind : uint8_t {
    Identifier,
    Number,
    Plus,
    Minus,
    Star,
    Slash,
    LParen,
    RParen,
    If,
    Then,
    Else,
    Arrow,
    Eq,
    EqEq,
    Bang,
    BangEq,
    Eof,
    Invalid
};

struct Token {
    TokenKind kind;
    std::string_view lexeme;
    size_t line;
    size_t column;
};

class Tokenizer {
  public:
    explicit Tokenizer(std::string_view src);

    auto next() -> Token;
    [[nodiscard]] auto peek() const -> Token;
    [[nodiscard]] auto is_done() const -> bool;

  private:
    std::string_view source;
    size_t start = 0;
    size_t current = 0;
    size_t line = 1;
    size_t column = 1;

    void skip_whitespace();
    auto advance() -> char;
    [[nodiscard]] auto peek_char() const -> char;
    [[nodiscard]] auto peek_next() const -> char;
    auto match(char expected) -> bool;

    auto make_token(TokenKind kind) -> Token;
    auto error_token(const std::string &msg) -> Token;

    auto scan_identifier() -> Token;
    auto scan_number() -> Token;
};
