#pragma once

#include "../ast/ast.hpp"
#include "ir.hpp"

auto lower(const ExprPtr &program) -> IRModule;
