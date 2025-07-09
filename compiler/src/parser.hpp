#pragma once

#include "ast.hpp"
#include "tokenizer.hpp"

#include <expected>
#include <vector>

using ParseResult = std::expected<ExprPtr, std::string>;

class Parser {
  public:
    explicit Parser(Tokenizer tokenizer);
    auto parse_assignments() -> std::vector<ParseResult>;

  private:
    Tokenizer tokenizer;
    Token current;

    void advance();
    [[nodiscard]] auto match(TokenKind kind) const -> bool;

    auto parse_assignment() -> ParseResult;
    auto parse_infix_expr(Precedence prec) -> ParseResult;
    auto parse_application() -> ParseResult;
    auto parse_factor() -> ParseResult;
};