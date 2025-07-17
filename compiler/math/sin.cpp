#include <cmath>
#include <numbers>

extern "C" {
auto wasmwasm_sin(int /*unused*/, double x) -> double {
    constexpr double PI = std::numbers::pi;
    constexpr double TWO_PI = 2.0 * PI;
    constexpr double HALF_PI = PI / 2.0;

    x = std::fmod(x, TWO_PI);
    x += TWO_PI * static_cast<double>(x < 0.0);

    double folded_pi = PI - std::fabs(PI - x);
    double base_x = HALF_PI - std::fabs(HALF_PI - folded_pi);
    double sign = 1.0 - (2.0 * static_cast<double>(x >= PI));

    double x2 = base_x * base_x;
    double x3 = base_x * x2;
    double x5 = x3 * x2;
    double x7 = x5 * x2;

    double approx = base_x - (x3 / 6.0) + (x5 / 120.0) - (x7 / 5040.0);
    return sign * approx;
}
}