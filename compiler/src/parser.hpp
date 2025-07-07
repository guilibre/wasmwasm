#pragma once

#include "ast.hpp"
#include "tokenizer.hpp"

class Parser {
  public:
    explicit Parser(Tokenizer tokenizer);
    auto parse_expr() -> ExprPtr;

  private:
    Tokenizer tokenizer;
    Token current;

    void advance();
    [[nodiscard]] auto match(TokenKind kind) const -> bool;

    auto parse_assignment() -> ExprPtr;
    auto parse_infix_expr(Precedence prec) -> ExprPtr;
    auto parse_application() -> ExprPtr;
    auto parse_factor() -> ExprPtr;
};