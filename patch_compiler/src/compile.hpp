#pragma once

#include <cstddef>
#include <cstdint>
#include <string>
#include <vector>

struct CompileResult {
    std::vector<char> bytes;
    uint32_t memory_bytes = 0;
};

auto compile_to_binary(float sample_rate, const std::string &patch_json,
                       char *math_bin, size_t math_bin_size) -> CompileResult;

auto get_param_index(const std::string &patch_json) -> std::string;
