#include "init_buffers_builder.hpp"

#include "binaryen-c.h"
#include "code_gen_context.hpp"

#include <memory>

InitBuffersBuilder::InitBuffersBuilder(

    const std::shared_ptr<CodeGenContext> &ctx,
    const std::shared_ptr<ExpressionEmitter> &expression_emitter)
    : ctx(ctx), expression_emitter(expression_emitter) {}

void InitBuffersBuilder::build(const ExprPtr &init) {
    ctx->push_context();
    ctx->add_function("init_buffers", expression_emitter->create(init),
                      BinaryenTypeNone(), 0);
    ctx->pop_context();
    BinaryenAddFunctionExport(ctx->module(), "init_buffers", "init_buffers");
}