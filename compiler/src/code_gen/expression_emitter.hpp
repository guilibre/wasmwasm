#pragma once

#include "../ast/ast.hpp"
#include "closure_builder.hpp"
#include "code_gen_context.hpp"

#include <memory>
#include <string_view>
#include <utility>

class ExpressionEmitter {
  public:
    ExpressionEmitter(const std::shared_ptr<CodeGenContext> &ctx);
    auto create(const ExprPtr &expr) -> BinaryenExpressionRef;

  private:
    std::shared_ptr<CodeGenContext> ctx;
    ClosureBuilder closure_builder;

    template <typename F>
    auto with_fresh_scope(F &&fn) -> BinaryenExpressionRef;
    auto build_captures(const std::unordered_set<std::string_view> &free_vars)
        -> std::vector<BinaryenExpressionRef>;
};

template <typename F>
auto ExpressionEmitter::with_fresh_scope(F &&fn) -> BinaryenExpressionRef {
    auto old_vars =
        std::exchange(ctx->variables,
                      std::unordered_map<std::string_view, BinaryenVariable>{});

    auto old_mem_loaded = std::exchange(ctx->mem_loaded_variables,
                                        std::unordered_set<std::string_view>{});

    BinaryenExpressionRef result = std::forward<F>(fn)();

    ctx->variables = std::move(old_vars);
    ctx->mem_loaded_variables = std::move(old_mem_loaded);

    return result;
}
