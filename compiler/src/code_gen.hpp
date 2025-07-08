#pragma once

#include "ast.hpp"

namespace code_gen {

auto insert_expr(float sample_freq, const ExprPtr &expr) -> int;

} // namespace code_gen