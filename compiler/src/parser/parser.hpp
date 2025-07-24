#pragma once

#include "../ast/ast.hpp"
#include "tokenizer.hpp"

#include <expected>

using ParseResult = std::expected<ExprPtr, std::string>;

class Parser {
  public:
    explicit Parser(Tokenizer tokenizer);
    auto parse() -> ParseResult;

  private:
    Tokenizer tokenizer;
    Token current;

    void advance();
    [[nodiscard]] auto match(TokenKind kind) const -> bool;

    auto parse_block() -> ParseResult;
    auto parse_expression() -> ParseResult;
    auto parse_application() -> ParseResult;
    auto parse_factor() -> ParseResult;
    auto parse_lambda() -> ParseResult;
};