#include <algorithm>
#include <bit>
#include <cmath>
#include <numbers>

using v2df = double __attribute__((vector_size(16)));
using v2di = long long __attribute__((vector_size(16)));

namespace {

[[clang::always_inline]] auto fast_round(double x) -> double {
    return __builtin_trunc(x + (x >= 0.0 ? 0.5 : -0.5));
}

[[clang::always_inline]] auto fast_round_x2(v2df x) -> v2df {
    const v2di neg_mask = x < 0.0;
    const v2df half =
        0.5 - __builtin_convertvector(neg_mask & v2di{1, 1}, v2df);
    return __builtin_elementwise_trunc(x + half);
}

constexpr auto HALF_PI = std::numbers::pi / 2.0;
constexpr auto TWO_PI = std::numbers::pi * 2.0;
constexpr auto INV_TWO_PI = 1.0 / TWO_PI;

constexpr auto SIN_C1 = 0.9999999999999999999825;
constexpr auto SIN_C3 = -0.16666666666666664625997;
constexpr auto SIN_C5 = 0.0083333333333333331646;
constexpr auto SIN_C7 = -0.00019841269841269461664;

constexpr auto COS_C0 = 1.0;
constexpr auto COS_C2 = -0.4999999999999999444888;
constexpr auto COS_C4 = 0.0416666666666665519596;
constexpr auto COS_C6 = -0.0013888888888888530298;
constexpr auto COS_C8 = 0.000024801587301571904;

[[clang::always_inline]] void reduce_x2(v2df x, v2df &x_reduced, v2df &x2,
                                        v2df &x_abs) {
    const auto q = fast_round_x2(x * INV_TWO_PI);
    x -= q * TWO_PI;

    x_abs = __builtin_elementwise_abs(x);
    x_reduced = HALF_PI - __builtin_elementwise_abs(x_abs - HALF_PI);
    x2 = x_reduced * x_reduced;
}

} // namespace

extern "C" {
auto wasmwasm_sin(double x) -> double {
    const auto q = fast_round(x * INV_TWO_PI);
    x -= q * TWO_PI;

    const auto x_abs = std::abs(x);
    const auto x_reduced = HALF_PI - std::abs(x_abs - HALF_PI);

    const auto x2 = x_reduced * x_reduced;
    const auto poly =
        x_reduced *
        (SIN_C1 + (x2 * (SIN_C3 + (x2 * (SIN_C5 + (x2 * SIN_C7))))));

    const auto sign = 1.0 - (2.0 * static_cast<double>(x < 0.0));
    return sign * poly;
}

auto wasmwasm_cos(double x) -> double {
    const auto q = fast_round(x * INV_TWO_PI);
    x -= q * TWO_PI;

    const auto x_abs = std::abs(x);
    const auto x_reduced = HALF_PI - std::abs(x_abs - HALF_PI);

    const auto x2 = x_reduced * x_reduced;
    const auto poly =
        COS_C0 +
        (x2 * (COS_C2 + (x2 * (COS_C4 + (x2 * (COS_C6 + (x2 * COS_C8)))))));

    const auto sign = 1.0 - (2.0 * static_cast<double>(x_abs >= HALF_PI));
    return sign * poly;
}

auto wasmwasm_tan(double x) -> double {
    return wasmwasm_sin(x) / wasmwasm_cos(x);
}

auto wasmwasm_sin_x2(v2df x) -> v2df {
    v2df x_reduced;
    v2df x2;
    v2df x_abs;
    reduce_x2(x, x_reduced, x2, x_abs);

    const v2df poly =
        x_reduced *
        (SIN_C1 + (x2 * (SIN_C3 + (x2 * (SIN_C5 + (x2 * SIN_C7))))));

    const v2di neg_mask = x < 0.0;
    const v2df sign =
        1.0 - (2.0 * __builtin_convertvector(neg_mask & v2di{1, 1}, v2df));
    return sign * poly;
}

auto wasmwasm_cos_x2(v2df x) -> v2df {
    v2df x_reduced;
    v2df x2;
    v2df x_abs;
    reduce_x2(x, x_reduced, x2, x_abs);

    const v2df poly =
        COS_C0 +
        (x2 * (COS_C2 + (x2 * (COS_C4 + (x2 * (COS_C6 + (x2 * COS_C8)))))));

    const v2di ge_mask = x_abs >= HALF_PI;
    const v2df sign =
        1.0 - (2.0 * __builtin_convertvector(ge_mask & v2di{1, 1}, v2df));
    return sign * poly;
}

auto wasmwasm_sincos_x2(v2df x) -> v2df {
    v2df x_reduced;
    v2df x2;
    v2df x_abs;
    reduce_x2(x, x_reduced, x2, x_abs);

    const v2df sin_poly =
        x_reduced *
        (SIN_C1 + (x2 * (SIN_C3 + (x2 * (SIN_C5 + (x2 * SIN_C7))))));
    const v2df cos_poly =
        COS_C0 +
        (x2 * (COS_C2 + (x2 * (COS_C4 + (x2 * (COS_C6 + (x2 * COS_C8)))))));

    const v2di sin_neg_mask = x < 0.0;
    const v2df sin_sign =
        1.0 - (2.0 * __builtin_convertvector(sin_neg_mask & v2di{1, 1}, v2df));
    const v2di cos_ge_mask = x_abs >= HALF_PI;
    const v2df cos_sign =
        1.0 - (2.0 * __builtin_convertvector(cos_ge_mask & v2di{1, 1}, v2df));

    const v2df sin_result = sin_sign * sin_poly;
    const v2df cos_result = cos_sign * cos_poly;
    return v2df{sin_result[0], cos_result[1]};
}

[[clang::always_inline]] auto vec_min(v2df a, v2df b) -> v2df {
    const v2di mask = a < b;
    const v2di bits = (mask & __builtin_bit_cast(v2di, a)) |
                      (~mask & __builtin_bit_cast(v2di, b));
    return __builtin_bit_cast(v2df, bits);
}

[[clang::always_inline]] auto vec_max(v2df a, v2df b) -> v2df {
    const v2di mask = a > b;
    const v2di bits = (mask & __builtin_bit_cast(v2di, a)) |
                      (~mask & __builtin_bit_cast(v2di, b));
    return __builtin_bit_cast(v2df, bits);
}

auto wasmwasm_clip_x2(v2df x) -> v2df {
    return vec_min(vec_max(x, v2df{-1.0, -1.0}), v2df{1.0, 1.0});
}

auto wasmwasm_min_x2(v2df a, v2df b) -> v2df { return vec_min(a, b); }

auto wasmwasm_max_x2(v2df a, v2df b) -> v2df { return vec_max(a, b); }

auto wasmwasm_sign(double x) -> double {
    if (x >= 0) return 1.0;
    return -1.0;
}

auto wasmwasm_fract(double x) -> double { return x - std::trunc(x); }

auto wasmwasm_clip(double x) -> double {
    return std::min(std::max(x, -1.0), 1.0);
}

auto wasmwasm_exp(double x) -> double {
    const auto I =
        static_cast<long long>((static_cast<double>(6497320848556798LL) * x) +
                               static_cast<double>(4607182418800017408LL));
    return std::bit_cast<double>(I);
}

namespace {

[[clang::always_inline]] auto next_u01() -> double {
    static unsigned long long random_state = __builtin_readcyclecounter();
    random_state += 0x9e3779b97f4a7c15ULL;
    unsigned long long s = random_state;
    s ^= s >> 30;
    s *= 0xbf58476d1ce4e5b9ULL;
    s ^= s >> 27;
    s *= 0x94d049bb133111ebULL;
    s ^= s >> 31;
    return std::bit_cast<double>((s >> 12) | 0x3FF0000000000000ULL) - 1.0;
}

[[clang::always_inline]] auto next_u01_x2() -> v2df {
    return v2df{next_u01(), next_u01()};
}

} // namespace

auto wasmwasm_uniform(double lower, double upper) -> double {
    return lower + (next_u01() * (upper - lower));
}

auto wasmwasm_gaussian(double mean, double std) -> double {
    const auto u = next_u01() + next_u01() + next_u01() + next_u01();
    return mean + (std * (u - 2.0) * std::numbers::sqrt3);
}

auto wasmwasm_uniform_x2(v2df lower, v2df upper) -> v2df {
    return lower + (next_u01_x2() * (upper - lower));
}

auto wasmwasm_gaussian_x2(v2df mean, v2df std) -> v2df {
    const v2df u =
        next_u01_x2() + next_u01_x2() + next_u01_x2() + next_u01_x2();
    return mean + (std * (u - 2.0) * std::numbers::sqrt3);
}

auto wasmwasm_floor(double x) -> double { return __builtin_floor(x); }

auto wasmwasm_ceil(double x) -> double { return __builtin_ceil(x); }

auto wasmwasm_sqrt(double x) -> double { return __builtin_sqrt(x); }

auto wasmwasm_round(double x) -> double { return fast_round(x); }

auto wasmwasm_log(double x) -> double {
    const auto bits_in = std::bit_cast<unsigned long long>(x);
    const auto e = static_cast<int>((bits_in >> 52) & 0x7FF) - 1023;
    const auto m = std::bit_cast<double>((bits_in & 0x000FFFFFFFFFFFFFULL) |
                                         (1023ULL << 52));
    const auto t = (m - 1.0) / (m + 1.0);
    const auto t2 = t * t;
    const auto p = 1.0 / 9.0;
    const auto p7 = (1.0 / 7.0) + (t2 * p);
    const auto p5 = (1.0 / 5.0) + (t2 * p7);
    const auto p3 = (1.0 / 3.0) + (t2 * p5);
    const auto log_m = 2.0 * t * (1.0 + (t2 * p3));
    return log_m + (static_cast<double>(e) * std::numbers::ln2);
}

auto wasmwasm_pow(double base, double exp) -> double {
    return wasmwasm_exp(exp * wasmwasm_log(base));
}

auto wasmwasm_abs(double x) -> double { return x < 0.0 ? -x : x; }

auto wasmwasm_tanh(double x) -> double {
    const auto e = wasmwasm_exp(2.0 * x);
    return (e - 1.0) / (e + 1.0);
}

auto wasmwasm_min(double a, double b) -> double { return a < b ? a : b; }

auto wasmwasm_max(double a, double b) -> double { return a > b ? a : b; }
}
