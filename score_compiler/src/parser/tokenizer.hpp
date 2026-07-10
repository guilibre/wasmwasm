#pragma once

#include <cstdint>
#include <string>
#include <string_view>

enum class TokenKind : uint8_t {
    Ident,
    Number,
    String,
    Equals,
    Colon,
    Semicolon,
    Plus,
    Minus,
    Star,
    Slash,
    LBrace,
    RBrace,
    LParen,
    RParen,
    LBracket,
    RBracket,
    Comma,
    Tick,
    Tilde,
    Caret,
    At,
    Ampersand,
    Pipe,
    Bang,
    Question,
    EqEq,
    NotEq,
    Less,
    Greater,
    LessEq,
    GreaterEq,
    KwPlay,
    KwTransform,
    KwBy,
    KwNull,
    KwReverse,
    KwRepeat,
    Eof,
    Invalid,
};

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
    size_t line = 1;
    size_t column = 1;
    bool done = false;

    void skip_whitespace();
    auto advance() -> char;
    [[nodiscard]] auto peek_current() const -> char;
    [[nodiscard]] auto peek_next() const -> char;

    auto make_token(TokenKind kind) -> Token;
    [[nodiscard]] auto error_token(const std::string &msg) const -> Token;

    auto scan_identifier() -> Token;
    auto scan_number() -> Token;
    auto scan_string() -> Token;

  public:
    explicit Tokenizer(std::string_view source);

    auto next() -> Token;
    [[nodiscard]] auto is_done() const -> bool;
};
