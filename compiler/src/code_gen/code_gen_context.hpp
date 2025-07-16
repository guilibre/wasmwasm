#pragma once

#include "../types/type.hpp"
#include "binaryen-c.h"

#include <string_view>
#include <unordered_map>
#include <unordered_set>

struct BinaryenVariable {
    BinaryenIndex local;
    BinaryenType binaryen_type;
    BinaryenFunctionRef fun_ref;
    std::string_view name;
    TypePtr type;
};

struct CodeGenContext {
    BinaryenModuleRef module = nullptr;
    double sample_freq = 44100;
    int32_t heap_top = 1024;

    std::unordered_map<std::string_view, BinaryenLiteral> constants;
    std::unordered_map<std::string_view, BinaryenVariable> globals;
    std::unordered_map<std::string_view, BinaryenVariable> parameters;
    std::unordered_map<std::string_view, BinaryenVariable> variables;
    std::unordered_map<std::string_view, BinaryenVariable> function_indices;
    std::unordered_set<std::string_view> mem_loaded_variables;
};