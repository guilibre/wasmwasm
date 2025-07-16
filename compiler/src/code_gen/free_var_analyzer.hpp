#pragma once

#include "../ast/ast.hpp"

#include <unordered_set>

namespace FreeVarAnalyzer {

auto analyze(const ExprPtr &expr,
             const std::unordered_set<std::string_view> &bound)
    -> std::unordered_set<std::string_view>;

}