#pragma once

#include <optional>

#include "ast.hpp"

auto to_double(const ExprValue &value) -> double;
auto apply_binary_op(BinOp op, const ExprValue &lhs, const ExprValue &rhs)
    -> std::optional<ExprValue>;
