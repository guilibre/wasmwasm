#pragma once

#include <cstdint>
#include <memory>
#include <optional>
#include <string>
#include <vector>

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
        Binary,
        Array,
        Ident,
        Ternary,
        Null
    } kind = Kind::Number;
    double number = 0;
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
};

struct Block {
    std::vector<Param> params;
    std::optional<std::string> instrument;
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
        Choose
    } kind = Kind::VarRef;
    enum class PipeOp : uint8_t { Transform, Reverse, Repeat };
    std::string var_name;
    std::vector<std::unique_ptr<CompExpr>> branches;
    std::unique_ptr<CompExpr> lhs_expr;
    std::string rhs_name;
    bool rhs_is_block = false;
    Block rhs_block;
    Block block_lit;
    PipeOp pipe_op = PipeOp::Transform;
    std::string pipe_param_name;
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
