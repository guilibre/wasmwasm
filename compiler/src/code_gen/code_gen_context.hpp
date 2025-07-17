#pragma once

#include "binaryen-c.h"

#include <string>
#include <unordered_map>
#include <vector>

struct BinaryenVariable {
    BinaryenIndex local = static_cast<BinaryenIndex>(-1);
    BinaryenType type = static_cast<BinaryenType>(-1);
    BinaryenIndex offset = 0;

    auto get_local(BinaryenModuleRef module) const -> BinaryenExpressionRef;
    auto set_local(BinaryenModuleRef module, BinaryenExpressionRef value) const
        -> BinaryenExpressionRef;
};

using VarMap = std::unordered_map<std::string, BinaryenVariable>;

struct CodeGenContext {
    BinaryenModuleRef module = nullptr;

    std::unordered_map<std::string, BinaryenLiteral> constants;
    VarMap globals;
    std::vector<VarMap> parameters{VarMap{}};
    std::vector<VarMap> variables{VarMap{}};
    VarMap fun_indices;

    CodeGenContext(BinaryenModuleRef module);
};