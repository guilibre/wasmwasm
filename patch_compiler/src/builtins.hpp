#pragma once

#include "types/type.hpp"

#include <array>
#include <string>
#include <string_view>
#include <unordered_map>
#include <vector>

inline constexpr std::array<std::string_view, 18> math_builtins = {
    "sin",   "cos",     "tan",      "sign",  "fract", "clip",
    "exp",   "uniform", "gaussian", "floor", "ceil",  "sqrt",
    "round", "log",     "abs",      "tanh",  "min",   "max",
};

inline constexpr std::array<std::string_view, 7> language_globals = {
    "PI", "SAMPLE_RATE", "OUT", "foldr", "zip", "map", "die",
};

auto make_builtin_env()
    -> std::vector<std::unordered_map<std::string, TypePtr>>;
