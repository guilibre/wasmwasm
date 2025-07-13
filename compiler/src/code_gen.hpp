#pragma once

#include "ast.hpp"

#include "binaryen-c.h"

#include <unordered_map>
#include <unordered_set>
#include <vector>

class CodeGen {
    BinaryenModuleRef module;
    BinaryenModuleRef math_module;
    double sample_freq;

    std::unordered_map<std::string, BinaryenLiteral> constants;
    std::unordered_map<std::string, BinaryenType> globals;
    std::unordered_map<std::string, std::pair<BinaryenIndex, BinaryenType>>
        parameters;
    std::unordered_map<std::string, std::pair<BinaryenIndex, BinaryenType>>
        variables;
    std::unordered_map<std::string, BinaryenIndex> function_indices;
    std::vector<std::pair<std::string, BinaryenFunctionRef>> lambda_functions;
    std::vector<std::string> function_table;

    auto create(const ExprPtr &expr) -> BinaryenExpressionRef;
    static auto collect_free_vars(const ExprPtr &expr,
                                  const std::unordered_set<std::string> &bound)
        -> std::unordered_set<std::string>;

  public:
    CodeGen(const CodeGen &) = default;
    CodeGen(CodeGen &&) = delete;
    auto operator=(const CodeGen &) -> CodeGen & = default;
    auto operator=(CodeGen &&) -> CodeGen & = delete;
    CodeGen(BinaryenModuleRef math_module, double sample_freq);
    ~CodeGen();

    auto create_main_module(const std::vector<ExprPtr> &exprs)
        -> BinaryenModuleRef;
};