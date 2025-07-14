#pragma once

#include "../parser/tokenizer.hpp"

#include <cstdint>
#include <memory>
#include <unordered_map>
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

class SubstitutionVisitor {
    std::unordered_map<std::string, ExprPtr> replacements;

  public:
    explicit SubstitutionVisitor(
        std::unordered_map<std::string, ExprPtr> replacements);

    auto visit(ExprPtr expr) -> ExprPtr;

    auto operator()(Expr::Assignment &assign) -> ExprPtr;
    auto operator()(Expr::Binary &bin) -> ExprPtr;
    auto operator()(Expr::Block &block) -> ExprPtr;
    auto operator()(Expr::Call &call) -> ExprPtr;
    auto operator()(Expr::Lambda &lambda) -> ExprPtr;
    auto operator()(Expr::Literal &lit) -> ExprPtr;
    auto operator()(Expr::Variable &var) -> ExprPtr;
};

class LambdaInliner {
    void collect_arguments(ExprPtr expr, std::vector<ExprPtr> &args);
    static auto apply_arguments(ExprPtr lambda, std::vector<ExprPtr> args)
        -> ExprPtr;

  public:
    auto visit(ExprPtr expr) -> ExprPtr;

    auto operator()(Expr::Assignment &assign) -> ExprPtr;
    auto operator()(Expr::Binary &bin) -> ExprPtr;
    auto operator()(Expr::Block &block) -> ExprPtr;
    auto operator()(Expr::Call &call) -> ExprPtr;
    auto operator()(Expr::Lambda &lambda) -> ExprPtr;
    auto operator()(Expr::Literal &lit) -> ExprPtr;
    auto operator()(Expr::Variable &var) -> ExprPtr;
};

class ASTPrinter {
  public:
    auto print(const Expr &expr) -> std::string;

    auto operator()(const Expr::Assignment &asg) -> std::string;
    auto operator()(const Expr::Binary &bin) -> std::string;
    auto operator()(const Expr::Block &block) -> std::string;
    auto operator()(const Expr::Call &call) -> std::string;
    auto operator()(const Expr::Lambda &lambda) -> std::string;
    auto operator()(const Expr::Literal &lit) -> std::string;
    auto operator()(const Expr::Variable &var) -> std::string;

  private:
    static auto token_to_string(TokenKind kind) -> std::string;
};