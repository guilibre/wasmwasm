#pragma once

#include "binaryen-c.h"

#include <string>
#include <unordered_map>
#include <utility>
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

class CodeGenContext {
    VarMap fun_indices;
    std::unordered_map<std::string, BinaryenLiteral> constants;
    std::vector<VarMap> parameters{VarMap{}};
    std::vector<VarMap> variables{VarMap{}};
    std::vector<BinaryenIndex> offsets{8};
    BinaryenModuleRef module_ = nullptr;

  public:
    CodeGenContext(BinaryenModuleRef module);

    auto module() -> BinaryenModuleRef;

    auto has_constant(const std::string &name) -> bool;
    auto constant(const std::string &name) -> BinaryenExpressionRef;
    void add_constant(const std::string &name, BinaryenLiteral expr);

    auto has_variable_or_parameter(const std::string &name) -> bool;
    auto variable_or_parameter(const std::string &name)
        -> std::pair<BinaryenVariable &, int>;

    auto add_parameter(const std::string &name, BinaryenType param_type)
        -> BinaryenVariable &;
    auto add_env() -> BinaryenVariable &;

    auto add_variable(BinaryenType var_type) -> BinaryenVariable &;
    auto add_variable(const std::string &name, BinaryenType var_type)
        -> BinaryenVariable &;

    auto has_function(const std::string &name) -> bool;
    auto function(const std::string &name) -> BinaryenVariable &;
    auto add_function(BinaryenExpressionRef body, BinaryenType result_type,
                      BinaryenIndex offset) -> BinaryenVariable &;
    auto add_function(const std::string &name, BinaryenExpressionRef body,
                      BinaryenType result_type, BinaryenIndex offset)
        -> BinaryenVariable &;

    auto offset() -> BinaryenIndex;
    auto env() -> BinaryenVariable &;

    void push_context();
    void pop_context();

    auto make_closure(const BinaryenVariable &fun) -> BinaryenExpressionRef;

    void add_function_table();
};