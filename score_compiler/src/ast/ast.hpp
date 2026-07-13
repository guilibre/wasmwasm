#pragma once

#include <cstdint>
#include <memory>
#include <string>
#include <variant>
#include <vector>

#include "rational.hpp"

using ExprValue = std::variant<Rational, double, std::string>;

enum class BinOp : uint8_t {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Pow,
    Eq,
    NotEq,
    Lt,
    Gt,
    LtEq,
    GtEq,
    And,
    Or
};

struct Expr {
    enum class Kind : uint8_t {
        Number,
        String,
        Binary,
        Array,
        Ident,
        Ternary,
        Null,
        Skip
    } kind = Kind::Number;
    double number = 0;
    Rational number_rational;
    std::string string_value;
    BinOp op = BinOp::Add;
    std::unique_ptr<Expr> lhs;
    std::unique_ptr<Expr> rhs;
    std::vector<std::unique_ptr<Expr>> elements;
    std::string ident_name;
    std::unique_ptr<Expr> ternary_cond;
    std::unique_ptr<Expr> ternary_then;
    std::unique_ptr<Expr> ternary_else;
    size_t line = 0;
    size_t column = 0;
};

struct Param {
    std::string name;
    std::unique_ptr<Expr> value;
    bool is_const = false;
};

struct Block {
    std::vector<Param> params;
    size_t line = 0;
    size_t column = 0;
};

struct CompExpr;

struct Term {
    enum class Kind : uint8_t {
        VarRef,
        Fork,
        AtomicJoin,
        Pipe,
        BlockLit,
        Choose,
        Emit
    } kind = Kind::VarRef;
    enum class PipeOp : uint8_t { Reverse, Repeat, Listen };
    std::string var_name;
    std::vector<std::unique_ptr<CompExpr>> branches;
    std::unique_ptr<CompExpr> lhs_expr;
    std::string rhs_name;
    bool rhs_is_block = false;
    Block rhs_block;
    Block block_lit;
    PipeOp pipe_op = PipeOp::Reverse;
    std::unique_ptr<Expr> pipe_expr;
    bool legato_after = false;
    size_t line = 0;
    size_t column = 0;
};

struct CompExpr {
    std::vector<Term> terms;
};

struct VarDecl {
    std::string name;
    enum class Kind : uint8_t {
        BlockDef,
        CompDef,
        ScaleDef
    } kind = Kind::BlockDef;
    Block block;
    CompExpr comp;
    std::vector<std::unique_ptr<Expr>> scale;
    size_t line = 0;
    size_t column = 0;
};

struct PlayStmt {
    std::vector<CompExpr> machines;
    size_t line = 0;
    size_t column = 0;
};

struct Program {
    std::vector<VarDecl> decls;
    std::vector<PlayStmt> plays;
};

class ASTPrinter {
  public:
    [[nodiscard]] auto print(const Program &program) -> std::string;
};
