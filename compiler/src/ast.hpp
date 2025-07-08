#pragma once

#include "tokenizer.hpp"

#include <cstdint>
#include <memory>
#include <variant>

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

    using ExprNode = std::variant<Literal, Assignment, Variable, Call, Binary>;

    ExprNode node;

    template <typename T, typename... Args>
    static auto make(Args &&...args) -> std::unique_ptr<Expr> {
        ExprNode node = T{std::forward<Args>(args)...};
        return std::make_unique<Expr>(Expr{std::move(node)});
    }
};

enum class Precedence : uint8_t { Lowest = 0, AddSub, MulDiv, Call };