#pragma once

#include <cstddef>
#include <utility>

inline auto normalize_position(size_t line, size_t col)
    -> std::pair<size_t, size_t> {
    const auto norm_line = line > 0 ? line - 1 : 0;
    const auto norm_col = col > 0 ? col - 1 : 0;
    return {norm_line, norm_col};
}
