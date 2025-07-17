#pragma once

#include "../parser/tokenizer.hpp"
#include "../types/type.hpp"

#include <memory>
#include <variant>
#include <vector>

struct Expr;

using ExprPtr = std::unique_ptr<Expr>;

struct Expr {
    struct Assignment {
        ExprPtr value;
        Token name;
    };

    struct Block {
        std::vector<ExprPtr> expressions;
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

    struct Variable {
        Token name;
    };

    using ExprNode =
        std::variant<Assignment, Block, Call, Lambda, Literal, Variable>;

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

    auto dispatch(const ExprPtr &expr, Expr::Assignment &asg, size_t indent)
        -> std::string;
    auto dispatch(const ExprPtr &expr, Expr::Block &block, size_t indent)
        -> std::string;
    auto dispatch(const ExprPtr &expr, Expr::Call &call, size_t indent)
        -> std::string;
    auto dispatch(const ExprPtr &expr, Expr::Lambda &lam, size_t indent)
        -> std::string;
    auto dispatch(const ExprPtr &expr, Expr::Literal &lit, size_t indent)
        -> std::string;
    auto dispatch(const ExprPtr &expr, Expr::Variable &var, size_t indent)
        -> std::string;

    static auto tokenkind_to_string(TokenKind kind) -> std::string;
    static auto type_to_string(const TypePtr &type) -> std::string;
    [[nodiscard]] auto indent_str(size_t indent) const -> std::string;
    auto attach_type(const std::string &str, const ExprPtr &expr, size_t indent,
                     bool inline_type = false) -> std::string;

  public:
    void operator()(const ExprPtr &expr);
};