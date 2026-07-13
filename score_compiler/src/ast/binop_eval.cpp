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
