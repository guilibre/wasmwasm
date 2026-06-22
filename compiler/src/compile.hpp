#pragma once

#include <cstddef>
#include <string>
#include <vector>

auto compile_to_binary(float sample_rate, const std::string &patch_json,
                       char *math_bin, size_t math_bin_size)
    -> std::vector<char>;
