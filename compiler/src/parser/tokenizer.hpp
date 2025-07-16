// tokenizer.hpp
#pragma once

#include <cstdint>
#include <string>
#include <string_view>

enum class TokenKind : uint8_t {
    Identifier = 0,
    Number = 1,
    LParen = 6,
    RParen = 7,
    Arrow = 8,
    LBra = 9,
    RBra = 10,
    Period = 11,
    Eof = 12,
    Invalid = 13
};

struct Token {
    TokenKind kind;
    std::string lexeme;
    size_t line;
    size_t column;
    [[nodiscard]] auto to_string() const -> std::string;
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
