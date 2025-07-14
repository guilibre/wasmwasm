#pragma once

#include "../ast/ast.hpp"

#include "binaryen-c.h"

#include <unordered_map>
#include <unordered_set>
#include <vector>

class CodeGen {
    int32_t heap_top = 1024;

    BinaryenModuleRef module;
    BinaryenModuleRef math_module;
    double sample_freq;

    std::unordered_map<std::string, BinaryenLiteral> constants;
    std::unordered_map<std::string, BinaryenType> globals;
    std::unordered_map<std::string, std::pair<BinaryenIndex, BinaryenType>>
        parameters;
    std::unordered_map<std::string, std::pair<BinaryenIndex, BinaryenType>>
        variables;
    std::unordered_map<std::string,
                       std::pair<BinaryenIndex, BinaryenFunctionRef>>
        function_indices;
    std::unordered_set<std::string> mem_loaded_variables;
    std::unordered_map<std::string, std::string> lambda_env;

    auto create(const ExprPtr &expr) -> BinaryenExpressionRef;
    static auto collect_free_vars(const ExprPtr &expr,
                                  const std::unordered_set<std::string> &bound)
        -> std::unordered_set<std::string>;
    auto make_closure(BinaryenExpressionRef func_index_expr,
                      const std::vector<BinaryenExpressionRef> &captured_values)
        -> BinaryenExpressionRef;

  public:
    CodeGen(const CodeGen &) = default;
    CodeGen(CodeGen &&) = delete;
    auto operator=(const CodeGen &) -> CodeGen & = default;
    auto operator=(CodeGen &&) -> CodeGen & = delete;
    explicit CodeGen(BinaryenModuleRef math_module, double sample_freq);
    ~CodeGen();

    auto create_main_module(const std::vector<ExprPtr> &exprs)
        -> BinaryenModuleRef;
};