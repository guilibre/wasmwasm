#include "main_module_builder.hpp"

#include "binaryen-c.h"
#include "code_gen_context.hpp"
#include "expression_emitter.hpp"
#include "wasm.h"

#include <algorithm>
#include <array>
#include <cmath>
#include <memory>
#include <string>
#include <utility>
#include <vector>

MainModuleBuilder::MainModuleBuilder(
    const std::shared_ptr<CodeGenContext> &ctx,
    const std::shared_ptr<ExpressionEmitter> &expression_emitter,
    BinaryenModuleRef math_module, double sample_rate)
    : ctx(ctx), math_module(math_module),
      expression_emitter(expression_emitter) {}

void MainModuleBuilder::setup_module() {
    BinaryenModuleSetFeatures(ctx->module(), BinaryenFeatureAll());
    BinaryenAddMemoryImport(ctx->module(), "memory", "env", "memory", 0);

    auto move_module_items = [&](auto &from, auto add_fn) {
        while (!from.empty()) {
            auto item = std::move(from.back());
            from.pop_back();
            add_fn(std::move(item));
        }
    };

    move_module_items(math_module->globals, [&](auto &&g) {
        ctx->module()->addGlobal(std::forward<decltype(g)>(g));
    });

    move_module_items(math_module->functions, [&](auto f) {
        for (const auto &ex : math_module->exports) {
            if (ex->getInternalName()->str == f->name.str) {
                f->setExplicitName(ex->name);
                break;
            }
        }
        ctx->module()->addFunction(std::move(f));
    });
}

void MainModuleBuilder::build(const ExprPtr &main) {
    setup_module();

    BinaryenAddGlobal(ctx->module(), "TIME", BinaryenTypeFloat64(), true,
                      BinaryenConst(ctx->module(), BinaryenLiteralFloat64(0)));

    auto &base_ptr = ctx->add_parameter("BASE_PTR$", BinaryenTypeInt32());
    auto &num_samples = ctx->add_parameter("NUM_SAMPLES$", BinaryenTypeInt32());
    auto &num_channels =
        ctx->add_parameter("NUM_CHANNELS$", BinaryenTypeInt32());

    auto &channel = ctx->add_variable("CHANNEL$", BinaryenTypeInt32());
    auto &sample = ctx->add_variable("SAMPLE$", BinaryenTypeInt32());

    auto &env = ctx->add_variable("env$", BinaryenTypeInt32());
    auto &out = ctx->add_variable("OUT", BinaryenTypeFloat64());

    auto inner_block_children = std::array{
        expression_emitter->create(main),

        BinaryenStore(
            ctx->module(), 4, 0, 4,
            BinaryenBinary(
                ctx->module(), BinaryenAddInt32(),
                base_ptr.get_local(ctx->module()),
                BinaryenBinary(
                    ctx->module(), BinaryenMulInt32(),
                    BinaryenConst(ctx->module(), BinaryenLiteralInt32(4)),
                    BinaryenBinary(
                        ctx->module(), BinaryenAddInt32(),
                        channel.get_local(ctx->module()),
                        BinaryenBinary(
                            ctx->module(), BinaryenMulInt32(),
                            sample.get_local(ctx->module()),
                            num_channels.get_local(ctx->module()))))),
            BinaryenUnary(ctx->module(), BinaryenDemoteFloat64(),
                          out.get_local(ctx->module())),
            BinaryenTypeFloat32(), "memory"),

        channel.set_local(
            ctx->module(),
            BinaryenBinary(
                ctx->module(), BinaryenAddInt32(),
                channel.get_local(ctx->module()),
                BinaryenConst(ctx->module(), BinaryenLiteralInt32(1)))),

        BinaryenBreak(ctx->module(), "inner_loop",
                      BinaryenBinary(ctx->module(), BinaryenLtUInt32(),
                                     channel.get_local(ctx->module()),
                                     num_channels.get_local(ctx->module())),
                      nullptr),
    };

    std::vector<BinaryenExpressionRef> update_buffers;
    update_buffers.reserve(ctx->buffers.size());
    for (const auto &buffer : ctx->buffers) {
        update_buffers.emplace_back(BinaryenStore(
            ctx->module(), 8, buffer.second, 8,
            BinaryenGlobalGet(ctx->module(), buffer.first.c_str(),
                              BinaryenTypeInt32()),
            BinaryenGlobalGet(ctx->module(), (buffer.first + "$future").c_str(),
                              BinaryenTypeFloat64()),
            BinaryenTypeFloat64(), "memory"));

        update_buffers.emplace_back(BinaryenGlobalSet(
            ctx->module(), buffer.first.c_str(),
            BinaryenBinary(
                ctx->module(), BinaryenRemUInt32(),
                BinaryenBinary(
                    ctx->module(), BinaryenAddInt32(),
                    BinaryenGlobalGet(ctx->module(), buffer.first.c_str(),
                                      BinaryenTypeInt32()),
                    BinaryenConst(ctx->module(), BinaryenLiteralInt32(8))),
                BinaryenGlobalGet(ctx->module(),
                                  (buffer.first + "$size").c_str(),
                                  BinaryenTypeInt32()))));
    }

    auto outer_block_children = std::array{
        channel.set_local(
            ctx->module(),
            BinaryenConst(ctx->module(), BinaryenLiteralInt32(0))),

        BinaryenLoop(ctx->module(), "inner_loop",
                     BinaryenBlock(ctx->module(), "inner_block",
                                   inner_block_children.data(),
                                   inner_block_children.size(),
                                   BinaryenTypeNone())),

        BinaryenGlobalSet(
            ctx->module(), "TIME",
            BinaryenBinary(
                ctx->module(), BinaryenAddFloat64(),
                BinaryenGlobalGet(ctx->module(), "TIME", BinaryenTypeFloat64()),
                BinaryenConst(ctx->module(), BinaryenLiteralFloat64(1)))),

        BinaryenBlock(ctx->module(), nullptr, update_buffers.data(),
                      update_buffers.size(), BinaryenTypeNone()),

        sample.set_local(
            ctx->module(),
            BinaryenBinary(
                ctx->module(), BinaryenAddInt32(),
                sample.get_local(ctx->module()),
                BinaryenConst(ctx->module(), BinaryenLiteralInt32(1)))),

        BinaryenBreak(ctx->module(), "outer_loop",
                      BinaryenBinary(ctx->module(), BinaryenLtUInt32(),
                                     sample.get_local(ctx->module()),
                                     num_samples.get_local(ctx->module())),
                      nullptr),
    };

    auto body_block = std::array{
        env.set_local(ctx->module(),
                      BinaryenConst(ctx->module(), BinaryenLiteralInt32(1024))),

        BinaryenStore(ctx->module(), 4, 0, 4, env.get_local(ctx->module()),
                      BinaryenConst(ctx->module(), BinaryenLiteralInt32(1024)),
                      BinaryenTypeInt32(), "memory"),

        sample.set_local(ctx->module(),
                         BinaryenConst(ctx->module(), BinaryenLiteralInt32(0))),

        BinaryenLoop(ctx->module(), "outer_loop",
                     BinaryenBlock(ctx->module(), "outer_block",
                                   outer_block_children.data(),
                                   outer_block_children.size(),
                                   BinaryenTypeNone())),
    };

    ctx->add_function("main",
                      BinaryenBlock(ctx->module(), "body", body_block.data(),
                                    body_block.size(), BinaryenTypeNone()),
                      BinaryenTypeNone(), 0);
    BinaryenAddFunctionExport(ctx->module(), "main", "main");

    ctx->add_function_table();
}