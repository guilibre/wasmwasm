#pragma once

#include "../ast/ast.hpp"
#include "code_gen_context.hpp"

#include <unordered_set>

namespace FreeVarAnalyzer {

auto analyze(const ExprPtr &expr, const std::unordered_set<std::string> &bound,
             const std::shared_ptr<CodeGenContext> &ctx)
    -> std::unordered_set<std::string>;

}