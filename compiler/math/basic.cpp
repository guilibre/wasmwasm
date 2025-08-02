#include <algorithm>
#include <cmath>
#include <cstdint>

constexpr double HALF_PI = 1.57079632679489661923132169163975144;
constexpr double TWO_PI = 6.28318530717958647692528676655900577;
constexpr double INV_TWO_PI = 0.15915494309189533576888376337251436;

constexpr double SIN_C1 = 0.9999999999999999999825;
constexpr double SIN_C3 = -0.16666666666666664625997;
constexpr double SIN_C5 = 0.0083333333333333331646;
constexpr double SIN_C7 = -0.00019841269841269461664;
constexpr double SIN_C9 = 2.7557319223930781118e-6;
constexpr double SIN_C11 = -2.5052108383974487989e-8;

constexpr double COS_C0 = 1.0;
constexpr double COS_C2 = -0.4999999999999999444888;
constexpr double COS_C4 = 0.0416666666666665519596;
constexpr double COS_C6 = -0.0013888888888888530298;
constexpr double COS_C8 = 0.000024801587301571904;
constexpr double COS_C10 = -2.7557319223930781118e-7;
constexpr double COS_C12 = 2.5052108383974487989e-9;

extern "C" {
auto wasmwasm_sin(double x) -> double {
    double q = std::round(x * INV_TWO_PI);
    x -= q * TWO_PI;

    double x_abs = std::abs(x);
    double x_reduced = HALF_PI - std::abs(x_abs - HALF_PI);

    double x2 = x_reduced * x_reduced;
    double poly =
        x_reduced *
        (SIN_C1 +
         x2 * (SIN_C3 +
               x2 * (SIN_C5 + x2 * (SIN_C7 + x2 * (SIN_C9 + x2 * SIN_C11)))));

    int quadrant = static_cast<int>(x_abs / HALF_PI) % 4;
    double sign = 1.0 - (2.0 * static_cast<double>(quadrant >= 2));
    sign *= 1.0 - 2.0 * static_cast<double>(x < 0);

    return sign * poly;
}

auto wasmwasm_cos(double x) -> double {
    double q = std::round(x * INV_TWO_PI);
    x -= q * TWO_PI;

    double x_abs = std::abs(x);
    double x_reduced = HALF_PI - std::abs(x_abs - HALF_PI);

    double x2 = x_reduced * x_reduced;
    double poly =
        COS_C0 +
        (x2 * (COS_C2 +
               x2 * (COS_C4 +
                     x2 * (COS_C6 +
                           x2 * (COS_C8 + x2 * (COS_C10 + x2 * COS_C12))))));

    int quadrant = static_cast<int>(x_abs / HALF_PI) % 4;
    double sign = 1.0 - (2.0 * (static_cast<int>(quadrant == 1) |
                                static_cast<int>(quadrant == 2)));

    return sign * poly;
}

auto wasmwasm_sign(double x) -> double {
    if (x >= 0) return 1.0;
    return -1.0;
}

auto wasmwasm_fract(double x) -> double {
    return x - static_cast<double>((static_cast<int64_t>(x)));
}

auto wasmwasm_clip(double x) -> double {
    return std::min(std::max(x, -1.0), 1.0);
}
}