#pragma once

#include "tokenizer.hpp"

#include <cstdint>
#include <memory>
#include <variant>
#include <vector>

struct Expr;

using ExprPtr = std::unique_ptr<Expr>;

struct Expr {
    struct Literal {
        Token value;
    };

    struct Assignment {
        ExprPtr value;
        Token name;
    };

    struct Variable {
        Token name;
    };

    struct Call {
        ExprPtr callee;
        ExprPtr argument;
    };

    struct Binary {
        Token op;
        ExprPtr lhs;
        ExprPtr rhs;
    };

    using Block = std::vector<ExprPtr>;

    struct Lambda {
        std::vector<Variable> parameters;
        ExprPtr body;
    };

    using ExprNode = std::variant<Literal, Assignment, Variable, Call, Binary,
                                  Block, Lambda>;

    ExprNode node;

    template <typename T, typename... Args>
    static auto make(Args &&...args) -> std::unique_ptr<Expr> {
        return std::make_unique<Expr>(Expr{T{std::forward<Args>(args)...}});
    }
};

enum class Precedence : uint8_t { Lowest = 0, AddSub, MulDiv, Call };