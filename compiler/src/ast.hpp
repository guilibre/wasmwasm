#pragma once

#include "tokenizer.hpp"

#include <memory>
#include <utility>
#include <variant>

struct Expr;
using ExprPtr = std::unique_ptr<Expr>;

struct Expr {
    struct Binary;
    struct Literal;
    struct If;
    struct Assignment;
    struct Variable;

    using ExprPtr = std::unique_ptr<Expr>;
    using Variant = std::variant<Binary, Literal, If, Assignment, Variable>;

    std::unique_ptr<Variant> node;

    template <typename T, typename... Args>
    static auto make(Args &&...args) -> std::unique_ptr<Expr> {
        auto expr = std::make_unique<Expr>();
        expr->node = std::make_unique<Variant>(T{std::forward<Args>(args)...});
        return expr;
    }

    Expr() = default;
    explicit Expr(std::unique_ptr<Variant> node) : node(std::move(node)) {}
    Expr(Expr &&) = default;
    ~Expr() = default;

    auto operator=(const Expr &) -> Expr & = delete;
    auto operator=(Expr &&) -> Expr & = delete;
    Expr(const Expr &) = delete;
};

struct Expr::Binary {
    Token op;
    ExprPtr left;
    ExprPtr right;
};

struct Expr::Literal {
    Token value;
};

struct Expr::If {
    ExprPtr condition;
    ExprPtr then_branch;
    ExprPtr else_branch;
};

struct Expr::Assignment {
    ExprPtr value;
    Token name;
};

struct Expr::Variable {
    Token name;
};
