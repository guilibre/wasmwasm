#pragma once

#include <optional>
#include <stdexcept>
#include <string>

#include "ast.hpp"

auto to_double(const ExprValue &value) -> double;
auto apply_binary_op(BinOp op, const ExprValue &lhs, const ExprValue &rhs)
    -> std::optional<ExprValue>;

class FoldException : public std::runtime_error {
  public:
    FoldException(const std::string &msg, size_t line, size_t col)
        : std::runtime_error(msg), line(line), col(col) {}

    size_t line;
    size_t col;
};

[[nodiscard]] auto fold_expr_value(const Expr &expr) -> ExprValue;
[[nodiscard]] auto fold_expr(const Expr &expr) -> double;
