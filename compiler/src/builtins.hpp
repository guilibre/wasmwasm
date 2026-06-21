#pragma once

#include "types/type.hpp"

#include <array>
#include <string>
#include <string_view>
#include <unordered_map>
#include <vector>

inline constexpr std::array<std::string_view, 13> math_builtins = {
    "sin",      "cos",   "sign", "fract", "clip",  "exp", "uniform",
    "gaussian", "floor", "ceil", "sqrt",  "round", "log",
};

inline constexpr std::array<std::string_view, 3> language_globals = {
    "PI",
    "SAMPLE_RATE",
    "OUT",
};

auto make_builtin_env()
    -> std::vector<std::unordered_map<std::string, TypePtr>>;
