#include "rational.hpp"

#include <limits>
#include <numeric>
#include <stdexcept>

namespace {

auto checked_narrow(__int128 value) -> int64_t {
    if (value > std::numeric_limits<int64_t>::max() ||
        value < std::numeric_limits<int64_t>::min())
        throw std::overflow_error("rational overflow");
    return static_cast<int64_t>(value);
}

} // namespace

auto Rational::from_decimal_literal(const std::string &lexeme) -> Rational {
    const auto dot = lexeme.find('.');
    if (dot == std::string::npos) return Rational(std::stoll(lexeme), 1);
    const auto int_part = lexeme.substr(0, dot);
    const auto frac_part = lexeme.substr(dot + 1);
    int64_t den = 1;
    for (size_t i = 0; i < frac_part.size(); ++i) den *= 10;
    const bool negative = !int_part.empty() && int_part[0] == '-';
    const auto abs_int_part = negative ? int_part.substr(1) : int_part;
    const int64_t int_value =
        abs_int_part.empty() ? 0 : std::stoll(abs_int_part);
    const int64_t frac_value = frac_part.empty() ? 0 : std::stoll(frac_part);
    int64_t num = (int_value * den) + frac_value;
    if (negative) num = -num;
    return Rational(num, den);
}

auto Rational::reduce() -> void {
    if (den < 0) {
        num = -num;
        den = -den;
    }
    if (num == 0) {
        den = 1;
        return;
    }
    const auto g = std::gcd(num < 0 ? -num : num, den);
    if (g > 1) {
        num /= g;
        den /= g;
    }
}

auto operator+(const Rational &a, const Rational &b) -> Rational {
    const __int128 num = (static_cast<__int128>(a.num) * b.den) +
                         (static_cast<__int128>(b.num) * a.den);
    const __int128 den = static_cast<__int128>(a.den) * b.den;
    return Rational(checked_narrow(num), checked_narrow(den));
}

auto operator-(const Rational &a, const Rational &b) -> Rational {
    const __int128 num = (static_cast<__int128>(a.num) * b.den) -
                         (static_cast<__int128>(b.num) * a.den);
    const __int128 den = static_cast<__int128>(a.den) * b.den;
    return Rational(checked_narrow(num), checked_narrow(den));
}

auto operator*(const Rational &a, const Rational &b) -> Rational {
    const __int128 num = static_cast<__int128>(a.num) * b.num;
    const __int128 den = static_cast<__int128>(a.den) * b.den;
    return Rational(checked_narrow(num), checked_narrow(den));
}

auto operator/(const Rational &a, const Rational &b) -> Rational {
    if (b.num == 0) throw std::domain_error("division by zero");
    const __int128 num = static_cast<__int128>(a.num) * b.den;
    const __int128 den = static_cast<__int128>(a.den) * b.num;
    return Rational(checked_narrow(num), checked_narrow(den));
}

auto operator==(const Rational &a, const Rational &b) -> bool {
    return a.num == b.num && a.den == b.den;
}

auto operator<(const Rational &a, const Rational &b) -> bool {
    return static_cast<__int128>(a.num) * b.den <
           static_cast<__int128>(b.num) * a.den;
}
