#pragma once

#include "types/type.hpp"

#include <array>
#include <string>
#include <string_view>
#include <unordered_map>
#include <vector>

inline constexpr std::array<std::string_view, 17> math_builtins = {
    "sin",     "cos",      "sign",  "fract", "clip", "exp",
    "uniform", "gaussian", "floor", "ceil",  "sqrt", "round",
    "log",     "abs",      "tanh",  "min",   "max",
};

inline constexpr std::array<std::string_view, 3> language_globals = {
    "PI",
    "SAMPLE_RATE",
    "OUT",
};

auto make_builtin_env()
    -> std::vector<std::unordered_map<std::string, TypePtr>>;
