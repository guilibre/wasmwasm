#pragma once

#include "ast.hpp"

#include <vector>

namespace code_gen {

auto insert_expr(float sample_freq, const std::vector<ExprPtr> &expr) -> int;

} // namespace code_gen