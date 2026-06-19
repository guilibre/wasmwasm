#include <algorithm>
#include <bit>
#include <cmath>
#include <numbers>
#include <wasm_simd128.h>

namespace {
[[clang::always_inline]] auto fast_round(double x) -> double {
    return __builtin_trunc(x + (x >= 0.0 ? 0.5 : -0.5));
}
} // namespace

constexpr double HALF_PI = std::numbers::pi / 2.0;
constexpr double TWO_PI = std::numbers::pi * 2.0;
constexpr double INV_TWO_PI = 1.0 / TWO_PI;

constexpr double SIN_C1 = 0.9999999999999999999825;
constexpr double SIN_C3 = -0.16666666666666664625997;
constexpr double SIN_C5 = 0.0083333333333333331646;
constexpr double SIN_C7 = -0.00019841269841269461664;

constexpr double COS_C0 = 1.0;
constexpr double COS_C2 = -0.4999999999999999444888;
constexpr double COS_C4 = 0.0416666666666665519596;
constexpr double COS_C6 = -0.0013888888888888530298;
constexpr double COS_C8 = 0.000024801587301571904;

extern "C" {
auto wasmwasm_sin(double x) -> double {
    const double q = fast_round(x * INV_TWO_PI);
    x -= q * TWO_PI;

    const double x_abs = std::abs(x);
    const double x_reduced = HALF_PI - std::abs(x_abs - HALF_PI);

    const double x2 = x_reduced * x_reduced;
    const double poly =
        x_reduced *
        (SIN_C1 + (x2 * (SIN_C3 + (x2 * (SIN_C5 + (x2 * SIN_C7))))));

    const double sign = 1.0 - (2.0 * static_cast<double>(x < 0.0));
    return sign * poly;
}

auto wasmwasm_sin_f64x2(v128_t x) -> v128_t {
    const v128_t inv_two_pi = wasm_f64x2_splat(INV_TWO_PI);
    const v128_t two_pi = wasm_f64x2_splat(TWO_PI);
    const v128_t half_pi = wasm_f64x2_splat(HALF_PI);
    const v128_t zero = wasm_f64x2_splat(0.0);
    const v128_t one = wasm_f64x2_splat(1.0);
    const v128_t neg_one = wasm_f64x2_splat(-1.0);

    const v128_t q = wasm_f64x2_nearest(wasm_f64x2_mul(x, inv_two_pi));
    x = wasm_f64x2_sub(x, wasm_f64x2_mul(q, two_pi));

    const v128_t x_abs = wasm_f64x2_abs(x);
    const v128_t x_reduced =
        wasm_f64x2_sub(half_pi, wasm_f64x2_abs(wasm_f64x2_sub(x_abs, half_pi)));

    const v128_t x2 = wasm_f64x2_mul(x_reduced, x_reduced);
    const v128_t poly = wasm_f64x2_mul(
        x_reduced,
        wasm_f64x2_add(
            wasm_f64x2_splat(SIN_C1),
            wasm_f64x2_mul(
                x2, wasm_f64x2_add(
                        wasm_f64x2_splat(SIN_C3),
                        wasm_f64x2_mul(
                            x2, wasm_f64x2_add(
                                    wasm_f64x2_splat(SIN_C5),
                                    wasm_f64x2_mul(
                                        x2, wasm_f64x2_splat(SIN_C7))))))));

    const v128_t sign =
        wasm_v128_bitselect(neg_one, one, wasm_f64x2_lt(x, zero));
    return wasm_f64x2_mul(sign, poly);
}

auto wasmwasm_cos(double x) -> double {
    const double q = fast_round(x * INV_TWO_PI);
    x -= q * TWO_PI;

    const double x_abs = std::abs(x);
    const double x_reduced = HALF_PI - std::abs(x_abs - HALF_PI);

    const double x2 = x_reduced * x_reduced;
    const double poly =
        COS_C0 +
        (x2 * (COS_C2 + (x2 * (COS_C4 + (x2 * (COS_C6 + (x2 * COS_C8)))))));

    const double sign = 1.0 - (2.0 * static_cast<double>(x_abs >= HALF_PI));
    return sign * poly;
}

auto wasmwasm_cos_f64x2(v128_t x) -> v128_t {
    const v128_t inv_two_pi = wasm_f64x2_splat(INV_TWO_PI);
    const v128_t two_pi = wasm_f64x2_splat(TWO_PI);
    const v128_t half_pi = wasm_f64x2_splat(HALF_PI);
    const v128_t one = wasm_f64x2_splat(1.0);
    const v128_t neg_one = wasm_f64x2_splat(-1.0);

    const v128_t q = wasm_f64x2_nearest(wasm_f64x2_mul(x, inv_two_pi));
    x = wasm_f64x2_sub(x, wasm_f64x2_mul(q, two_pi));

    const v128_t x_abs = wasm_f64x2_abs(x);
    const v128_t x_reduced =
        wasm_f64x2_sub(half_pi, wasm_f64x2_abs(wasm_f64x2_sub(x_abs, half_pi)));

    const v128_t x2 = wasm_f64x2_mul(x_reduced, x_reduced);
    const v128_t poly = wasm_f64x2_add(
        wasm_f64x2_splat(COS_C0),
        wasm_f64x2_mul(
            x2,
            wasm_f64x2_add(
                wasm_f64x2_splat(COS_C2),
                wasm_f64x2_mul(
                    x2, wasm_f64x2_add(
                            wasm_f64x2_splat(COS_C4),
                            wasm_f64x2_mul(
                                x2, wasm_f64x2_add(
                                        wasm_f64x2_splat(COS_C6),
                                        wasm_f64x2_mul(x2, wasm_f64x2_splat(
                                                               COS_C8)))))))));

    const v128_t sign =
        wasm_v128_bitselect(neg_one, one, wasm_f64x2_ge(x_abs, half_pi));
    return wasm_f64x2_mul(sign, poly);
}

auto wasmwasm_sign(double x) -> double {
    if (x >= 0) return 1.0;
    return -1.0;
}

auto wasmwasm_sign_f64x2(v128_t x) -> v128_t {
    return wasm_v128_bitselect(wasm_f64x2_splat(1.0), wasm_f64x2_splat(-1.0),
                               wasm_f64x2_ge(x, wasm_f64x2_splat(0.0)));
}

auto wasmwasm_fract(double x) -> double { return x - std::trunc(x); }

auto wasmwasm_fract_f64x2(v128_t x) -> v128_t {
    return wasm_f64x2_sub(x, wasm_f64x2_trunc(x));
}

auto wasmwasm_clip(double x) -> double {
    return std::min(std::max(x, -1.0), 1.0);
}

auto wasmwasm_clip_f64x2(v128_t x) -> v128_t {
    return wasm_f64x2_pmax(wasm_f64x2_splat(-1.0),
                           wasm_f64x2_pmin(x, wasm_f64x2_splat(1.0)));
}

auto wasmwasm_exp(double x) -> double {
    const auto I =
        static_cast<long long>((static_cast<double>(6497320848556798LL) * x) +
                               static_cast<double>(4607182418800017408LL));
    return std::bit_cast<double>(I);
}

auto wasmwasm_exp_f64x2(v128_t x) -> v128_t {
    return wasm_f64x2_make(wasmwasm_exp(wasm_f64x2_extract_lane(x, 0)),
                           wasmwasm_exp(wasm_f64x2_extract_lane(x, 1)));
}
}
