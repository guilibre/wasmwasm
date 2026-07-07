#pragma once

#include <cstdint>
#include <memory>
#include <optional>
#include <string>
#include <vector>

enum class BinOp : uint8_t { Add, Sub, Mul, Div };

struct Expr {
    enum class Kind : uint8_t { Number, Binary } kind = Kind::Number;
    double number = 0;
    BinOp op = BinOp::Add;
    std::unique_ptr<Expr> lhs;
    std::unique_ptr<Expr> rhs;
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
    enum class Kind : uint8_t { VarRef, Fork } kind = Kind::VarRef;
    std::string var_name;
    std::vector<std::unique_ptr<CompExpr>> branches;
    size_t line = 0;
    size_t column = 0;
};

struct CompExpr {
    std::vector<Term> terms;
};

struct VarDecl {
    std::string name;
    enum class Kind : uint8_t { BlockDef, CompDef } kind = Kind::BlockDef;
    Block block;
    CompExpr comp;
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
