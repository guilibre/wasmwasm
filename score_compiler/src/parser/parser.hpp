#pragma once

#include "ast/ast.hpp"
#include "tokenizer.hpp"

#include <memory>
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

    auto parse_motiv_def() -> MotivDef;
    auto parse_play_stmt() -> PlayStmt;
    auto parse_seq_expr() -> SeqExpr;
    auto parse_term() -> Term;
    auto parse_branch() -> Branch;
    auto parse_group() -> std::unique_ptr<Group>;
    auto parse_voice() -> Voice;
    auto parse_note_atom() -> NoteAtom;
    auto parse_duration() -> Duration;

  public:
    explicit Parser(Tokenizer tokenizer);
    auto parse() -> Program;
};
