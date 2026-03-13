#pragma once

#include "../ast/ast.hpp"
#include "ir.hpp"

auto lower(const ExprPtr &init, const ExprPtr &main) -> IRModule;
