#pragma once

#include "../ast/ast.hpp"
#include "binaryen-c.h"
#include "code_gen_context.hpp"

#include <memory>

class ExpressionEmitter {
  public:
    ExpressionEmitter(std::shared_ptr<CodeGenContext> ctx);
    auto create(const ExprPtr &expr) -> BinaryenExpressionRef;

  private:
    std::shared_ptr<CodeGenContext> ctx;
    size_t heap_top = 1024;

    template <typename F>
    auto with_fresh_scope(F &&fn) -> BinaryenExpressionRef;
};