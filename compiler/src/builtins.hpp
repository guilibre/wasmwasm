#pragma once

#include "types/type.hpp"

#include <array>
#include <string>
#include <string_view>
#include <unordered_map>
#include <vector>

inline constexpr std::array<std::string_view, 8> math_builtins = {
    "sin", "cos", "sign", "fract", "clip", "exp", "uniform", "gaussian",
};

inline constexpr std::array<std::string_view, 4> language_globals = {
    "TIME",
    "PI",
    "SAMPLE_RATE",
    "OUT",
};

auto make_builtin_env()
    -> std::vector<std::unordered_map<std::string, TypePtr>>;
