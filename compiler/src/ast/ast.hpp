#pragma once

#include "../parser/tokenizer.hpp"
#include "../types/type.hpp"

#include <cstdint>
#include <memory>
#include <string>
#include <variant>
#include <vector>

struct Expr;

using ExprPtr = std::unique_ptr<Expr>;

struct Assignment {
    ExprPtr value;
    Token name;
};

enum Operation : uint8_t {
    Add,
    Sub,
    Mul,
    Div,
};

struct BinaryOp {
    Operation op;
    ExprPtr left;
    ExprPtr right;
};

struct Block {
    std::vector<ExprPtr> expressions;
};

struct Buffer {
    std::string name;
    size_t size;
    ExprPtr init_buffer_function;
};

struct Call {
    ExprPtr callee;
    ExprPtr argument;
};

struct Lambda {
    Token parameter;
    ExprPtr body;
};

struct Literal {
    Token value;
};

struct UnaryOp {
    Operation op;
    ExprPtr expr;
};

struct Variable {
    Token name;
};

struct Expr {

    using ExprNode = std::variant<Assignment, Block, BinaryOp, Buffer, Call,
                                  Lambda, Literal, UnaryOp, Variable>;

    ExprNode node;
    TypePtr type;

    template <typename T, typename... Args>
    static auto make(Args &&...args) -> std::unique_ptr<Expr> {
        return std::make_unique<Expr>(Expr{
            .node = T{std::forward<Args>(args)...},
            .type = nullptr,
        });
    }
};

class ASTPrinter {
    auto print(const ExprPtr &expr, size_t indent = 0) -> std::string;

    auto dispatch(const ExprPtr &expr, Assignment &asg, size_t indent)
        -> std::string;
    auto dispatch(const ExprPtr &expr, Block &block, size_t indent)
        -> std::string;
    auto dispatch(const ExprPtr &expr, BinaryOp &op, size_t indent)
        -> std::string;
    auto dispatch(const ExprPtr &expr, Buffer &buf, size_t indent)
        -> std::string;
    auto dispatch(const ExprPtr &expr, Call &call, size_t indent)
        -> std::string;
    auto dispatch(const ExprPtr &expr, Lambda &lam, size_t indent)
        -> std::string;
    auto dispatch(const ExprPtr &expr, Literal &lit, size_t indent)
        -> std::string;
    auto dispatch(const ExprPtr &expr, UnaryOp &op, size_t indent)
        -> std::string;
    auto dispatch(const ExprPtr &expr, Variable &var, size_t indent)
        -> std::string;

    static auto tokenkind_to_string(TokenKind kind) -> std::string;
    static auto type_to_string(const TypePtr &type) -> std::string;
    [[nodiscard]] auto indent_str(size_t indent) const -> std::string;
    static auto attach_type(const std::string &str, const ExprPtr &expr)
        -> std::string;

  public:
    void operator()(const ExprPtr &expr);
};