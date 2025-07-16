#pragma once

#include "binaryen-c.h"

#include <vector>

class ClosureBuilder {
  public:
    ClosureBuilder(BinaryenModuleRef module, size_t heap_top);
    auto build(BinaryenExpressionRef func_index,
               const std::vector<BinaryenExpressionRef> &captures)
        -> BinaryenExpressionRef;

  private:
    BinaryenModuleRef module;
    size_t heap_top;
};
