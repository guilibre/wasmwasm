#pragma once

#include "../ast/ast.hpp"
#include "code_gen_context.hpp"
#include "expression_emitter.hpp"
#include <memory>

class InitBuffersBuilder {
    std::shared_ptr<CodeGenContext> ctx;
    std::shared_ptr<ExpressionEmitter> expression_emitter;

  public:
    InitBuffersBuilder(
        const std::shared_ptr<CodeGenContext> &ctx,
        const std::shared_ptr<ExpressionEmitter> &expression_emitter);
    void build(const ExprPtr &init);
};