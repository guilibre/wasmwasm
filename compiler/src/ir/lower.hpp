#pragma once

#include "ast/ast.hpp"
#include "ir.hpp"

#include <string>

auto lower(const ExprPtr &program, const std::string &module_name) -> IRModule;
