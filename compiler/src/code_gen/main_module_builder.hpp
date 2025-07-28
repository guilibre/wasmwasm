#pragma once

#include "../ast/ast.hpp"
#include "binaryen-c.h"
#include "code_gen_context.hpp"
#include "expression_emitter.hpp"

class MainModuleBuilder {
    std::shared_ptr<CodeGenContext> ctx;
    std::shared_ptr<ExpressionEmitter> expression_emitter;

    BinaryenModuleRef math_module;

    void setup_module();

  public:
    MainModuleBuilder(
        const std::shared_ptr<CodeGenContext> &ctx,
        const std::shared_ptr<ExpressionEmitter> &expression_emitter,
        BinaryenModuleRef math_module, double sample_rate);
    void build(const ExprPtr &main);
};