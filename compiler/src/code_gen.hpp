#pragma once

#include "ast.hpp"

#include "binaryen-c.h"

#include <vector>

namespace code_gen {

auto insert_expr(float sample_freq, const std::vector<ExprPtr> &exprs,
                 BinaryenModuleRef functions_module) -> int;

} // namespace code_gen