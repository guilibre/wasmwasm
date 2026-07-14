#include "binop_eval.hpp"

#include <cmath>

auto to_double(const ExprValue &value) -> double {
    if (std::holds_alternative<Rational>(value))
        return std::get<Rational>(value).to_double();
    if (std::holds_alternative<double>(value)) return std::get<double>(value);
    return 0;
}

namespace {

auto apply_rational_or_double(BinOp op, const ExprValue &lhs,
                              const ExprValue &rhs)
    -> std::optional<ExprValue> {
    if (std::holds_alternative<Rational>(lhs) &&
        std::holds_alternative<Rational>(rhs)) {
        const auto &a = std::get<Rational>(lhs);
        const auto &b = std::get<Rational>(rhs);
        switch (op) {
        case BinOp::Add:
            return a + b;
        case BinOp::Sub:
            return a - b;
        case BinOp::Mul:
            return a * b;
        case BinOp::Div:
            if (b.num == 0) return std::nullopt;
            return a / b;
        default:
            break;
        }
    }
    const auto a = to_double(lhs);
    const auto b = to_double(rhs);
    switch (op) {
    case BinOp::Add:
        return a + b;
    case BinOp::Sub:
        return a - b;
    case BinOp::Mul:
        return a * b;
    case BinOp::Div:
        if (b == 0) return std::nullopt;
        return a / b;
    default:
        return std::nullopt;
    }
}

} // namespace

auto apply_binary_op(BinOp op, const ExprValue &lhs, const ExprValue &rhs)
    -> std::optional<ExprValue> {
    if (std::holds_alternative<std::string>(lhs) ||
        std::holds_alternative<std::string>(rhs)) {
        if (!std::holds_alternative<std::string>(lhs) ||
            !std::holds_alternative<std::string>(rhs))
            return std::nullopt;
        const auto &lhs_str = std::get<std::string>(lhs);
        const auto &rhs_str = std::get<std::string>(rhs);
        switch (op) {
        case BinOp::Add:
            return lhs_str + rhs_str;
        case BinOp::Eq:
            return lhs_str == rhs_str ? 1.0 : 0.0;
        case BinOp::NotEq:
            return lhs_str != rhs_str ? 1.0 : 0.0;
        default:
            return std::nullopt;
        }
    }

    switch (op) {
    case BinOp::Add:
    case BinOp::Sub:
    case BinOp::Mul:
    case BinOp::Div:
        return apply_rational_or_double(op, lhs, rhs);
    case BinOp::Mod: {
        const auto a = to_double(lhs);
        const auto b = to_double(rhs);
        if (b == 0) return std::nullopt;
        return std::fmod(a, b);
    }
    case BinOp::Pow:
        return std::pow(to_double(lhs), to_double(rhs));
    case BinOp::Eq:
        return to_double(lhs) == to_double(rhs) ? 1.0 : 0.0;
    case BinOp::NotEq:
        return to_double(lhs) != to_double(rhs) ? 1.0 : 0.0;
    case BinOp::Lt:
        return to_double(lhs) < to_double(rhs) ? 1.0 : 0.0;
    case BinOp::Gt:
        return to_double(lhs) > to_double(rhs) ? 1.0 : 0.0;
    case BinOp::LtEq:
        return to_double(lhs) <= to_double(rhs) ? 1.0 : 0.0;
    case BinOp::GtEq:
        return to_double(lhs) >= to_double(rhs) ? 1.0 : 0.0;
    case BinOp::And:
        return (to_double(lhs) != 0 && to_double(rhs) != 0) ? 1.0 : 0.0;
    case BinOp::Or:
        return (to_double(lhs) != 0 || to_double(rhs) != 0) ? 1.0 : 0.0;
    }
    return std::nullopt;
}

auto fold_expr_value(const Expr &expr) -> ExprValue {
    if (expr.kind == Expr::Kind::Number) return expr.number_rational;
    if (expr.kind == Expr::Kind::String) return expr.string_value;
    if (expr.kind == Expr::Kind::Ident)
        throw FoldException("identifier '" + expr.ident_name +
                                "' is only allowed in a '@{...}' field value",
                            expr.line, expr.column);
    if (expr.kind == Expr::Kind::Null) return std::string{};
    if (expr.kind == Expr::Kind::Skip)
        throw FoldException("'skip' is not a valid field value", expr.line,
                            expr.column);
    if (expr.kind == Expr::Kind::Ternary)
        return to_double(fold_expr_value(*expr.ternary_cond)) != 0
                   ? fold_expr_value(*expr.ternary_then)
                   : fold_expr_value(*expr.ternary_else);
    const auto lhs = fold_expr_value(*expr.lhs);
    const auto rhs = fold_expr_value(*expr.rhs);
    if (std::holds_alternative<std::string>(lhs) ||
        std::holds_alternative<std::string>(rhs)) {
        if (!std::holds_alternative<std::string>(lhs) ||
            !std::holds_alternative<std::string>(rhs))
            throw FoldException(
                "cannot mix a string and a number in an expression", expr.line,
                expr.column);
        if (expr.op != BinOp::Add && expr.op != BinOp::Eq &&
            expr.op != BinOp::NotEq)
            throw FoldException("invalid string operation", expr.line,
                                expr.column);
    } else if (expr.op == BinOp::Div || expr.op == BinOp::Mod) {
        const auto rhs_is_zero = std::holds_alternative<Rational>(rhs)
                                     ? std::get<Rational>(rhs).num == 0
                                     : to_double(rhs) == 0;
        if (rhs_is_zero)
            throw FoldException("division by zero", expr.line, expr.column);
    }
    const auto result = apply_binary_op(expr.op, lhs, rhs);
    if (!result)
        throw FoldException("invalid operation", expr.line, expr.column);
    return *result;
}

auto fold_expr(const Expr &expr) -> double {
    const auto value = fold_expr_value(expr);
    if (std::holds_alternative<std::string>(value))
        throw FoldException(
            "a string value cannot be used in a numeric expression", expr.line,
            expr.column);
    return to_double(value);
}
