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
    [[nodiscard]] auto match(TokenKind kind) -> bool;
    auto expect(TokenKind kind, const std::string &what) -> Token;
    void end_statement();
    [[nodiscard]] auto starts_term() const -> bool;

    [[nodiscard]] auto parse_ternary() -> std::unique_ptr<Expr>;
    [[nodiscard]] auto parse_comparison() -> std::unique_ptr<Expr>;
    [[nodiscard]] auto parse_expr() -> std::unique_ptr<Expr>;
    [[nodiscard]] auto parse_arith_term() -> std::unique_ptr<Expr>;
    [[nodiscard]] auto parse_pow() -> std::unique_ptr<Expr>;
    [[nodiscard]] auto parse_factor() -> std::unique_ptr<Expr>;
    [[nodiscard]] auto parse_array_literal()
        -> std::vector<std::unique_ptr<Expr>>;
    [[nodiscard]] auto parse_block() -> Block;
    void consume_legato_tilde(Term &term);
    void parse_comp_terms(CompExpr &comp);
    [[nodiscard]] auto parse_comp_expr() -> CompExpr;
    [[nodiscard]] auto parse_comp_atom() -> Term;
    [[nodiscard]] auto parse_atomic_join() -> Term;
    [[nodiscard]] auto continue_pipe_term(Term term) -> Term;
    [[nodiscard]] auto parse_pipe_term() -> Term;
    [[nodiscard]] auto continue_fork_term(Term first) -> Term;
    [[nodiscard]] auto parse_fork_term() -> Term;
    [[nodiscard]] auto parse_pipe_suffix(std::unique_ptr<CompExpr> lhs) -> Term;
    [[nodiscard]] auto parse_bang_suffix(std::unique_ptr<CompExpr> lhs) -> Term;
    [[nodiscard]] auto parse_var_decl() -> VarDecl;
    [[nodiscard]] auto parse_play_stmt() -> PlayStmt;

  public:
    explicit Parser(Tokenizer tokenizer);
    auto parse() -> Program;
};
