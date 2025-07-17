#pragma once

#include "../ast/ast.hpp"
#include "binaryen-c.h"
#include "code_gen_context.hpp"
#include "expression_emitter.hpp"

#include <vector>

class MainModuleBuilder {
  public:
    MainModuleBuilder(BinaryenModuleRef math_module, double sample_rate);
    auto build(const std::vector<ExprPtr> &exprs) -> BinaryenModuleRef;

  private:
    std::shared_ptr<CodeGenContext> ctx;
    std::shared_ptr<ExpressionEmitter> expression_emitter;

    BinaryenModuleRef math_module;

    void define_binary_operator(const std::string &symbol, BinaryenOp op);
};