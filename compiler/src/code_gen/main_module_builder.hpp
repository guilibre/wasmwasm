#pragma once

#include "../ast/ast.hpp"
#include "code_gen_context.hpp"

#include "binaryen-c.h"

#include <vector>

class MainModuleBuilder {
  public:
    MainModuleBuilder(BinaryenModuleRef math_module, double sample_freq);
    auto build(const std::vector<ExprPtr> &exprs) -> BinaryenModuleRef;

  private:
    CodeGenContext ctx;
    BinaryenModuleRef math_module;
    double sample_freq;
};