#pragma once

#include "ast/ast.hpp"
#include "tokenizer.hpp"
#include <stdexcept>
#include <string>

class ParseException : public std::runtime_error {
  public:
    ParseException(const std::string &msg, size_t line, size_t col)
        : std::runtime_error(msg), line(line), col(col) {}

    size_t line;
    size_t col;
};

class Parser {
    Tokenizer tokenizer;
    Token current;

    void advance();
    [[nodiscard]] auto match(TokenKind kind) const -> bool;
    auto expect(TokenKind kind, const std::string &what) -> Token;
    void skip_newlines();
    void end_statement();
    [[nodiscard]] auto starts_term() const -> bool;

  public:
    explicit Parser(Tokenizer tokenizer);
    auto parse() -> Program;
};
