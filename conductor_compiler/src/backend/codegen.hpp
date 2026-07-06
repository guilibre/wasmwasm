#pragma once

#include "ast/ast.hpp"
#include <ostream>

class CodeGenerator {
  public:
    void generate(const Program &program, std::ostream &out);
};
