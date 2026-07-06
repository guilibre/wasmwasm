#pragma once

#include <cstdint>
#include <string>
#include <string_view>

enum class TokenKind : uint8_t {};

struct Token {
    TokenKind kind;
    std::string lexeme;
    size_t line;
    size_t column;
    [[nodiscard]] auto to_string() const -> std::string;
};

class Tokenizer {
    std::string_view source;
    size_t start = 0;
    size_t current = 0;
    size_t line = 0;
    size_t column = 0;

    void skip_whitespace();
    auto advance() -> char;
    [[nodiscard]] auto peek_current() const -> char;

    auto make_token(TokenKind kind) -> Token;
    auto error_token(const std::string &msg) -> Token;

    auto scan_identifier() -> Token;
    auto scan_number() -> Token;

  public:
    explicit Tokenizer(std::string_view source);

    auto next() -> Token;
    [[nodiscard]] auto is_done() const -> bool;
};
