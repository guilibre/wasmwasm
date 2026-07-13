#pragma once

#include <cstdint>
#include <string>

struct Rational {
    int64_t num = 0;
    int64_t den = 1;

    Rational() = default;
    explicit Rational(int64_t n, int64_t d = 1) : num(n), den(d) { reduce(); }

    static auto from_decimal_literal(const std::string &lexeme) -> Rational;

    auto reduce() -> void;
    [[nodiscard]] auto to_double() const -> double {
        return static_cast<double>(num) / static_cast<double>(den);
    }
    [[nodiscard]] auto is_integer() const -> bool { return den == 1; }
};

auto operator+(const Rational &a, const Rational &b) -> Rational;
auto operator-(const Rational &a, const Rational &b) -> Rational;
auto operator*(const Rational &a, const Rational &b) -> Rational;
auto operator/(const Rational &a, const Rational &b) -> Rational;
auto operator==(const Rational &a, const Rational &b) -> bool;
auto operator<(const Rational &a, const Rational &b) -> bool;
