#pragma once

#include "../ast/ast.hpp"
#include "tokenizer.hpp"

#include <expected>

struct ParseError {
    std::string msg;
    size_t line = 0;
    size_t col = 0;
};

using ParseResult = std::expected<ExprPtr, ParseError>;

class Parser {
  public:
    explicit Parser(Tokenizer tokenizer);
    auto parse_code() -> ParseResult;
    auto parse_initialization() -> ParseResult;

  private:
    Tokenizer tokenizer;
    Token current;

    void advance();
    [[nodiscard]] auto match(TokenKind kind) const -> bool;

    auto parse_expression() -> ParseResult;
    auto parse_application() -> ParseResult;
    auto parse_additive() -> ParseResult;
    auto parse_multiplicative() -> ParseResult;
    auto parse_factor() -> ParseResult;
    auto parse_lambda() -> ParseResult;
    auto parse_block() -> ParseResult;
    auto parse_buffer() -> ParseResult;
};
