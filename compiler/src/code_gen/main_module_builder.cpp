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

MainModuleBuilder::MainModuleBuilder(BinaryenModuleRef math_module,
                                     double sample_rate)
    : ctx(std::make_shared<CodeGenContext>(BinaryenModuleCreate())),
      math_module(math_module) {
    if (ctx->module() == nullptr)
        throw std::runtime_error("Module not initialized");

    expression_emitter = std::make_shared<ExpressionEmitter>(ctx, sample_rate);
}

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

auto MainModuleBuilder::build(const ExprPtr &inner_body) -> BinaryenModuleRef {
    setup_module();

    BinaryenAddGlobal(ctx->module(), "TIME", BinaryenTypeFloat64(), true,
                      BinaryenConst(ctx->module(), BinaryenLiteralFloat64(0)));

    ctx->add_constant("PI", BinaryenLiteralFloat64(M_PI));

    auto &base_ptr = ctx->add_parameter("BASE_PTR$", BinaryenTypeInt32());
    auto &num_samples = ctx->add_parameter("NUM_SAMPLES$", BinaryenTypeInt32());
    auto &num_channels =
        ctx->add_parameter("NUM_CHANNELS$", BinaryenTypeInt32());

    auto &channel = ctx->add_variable("CHANNEL$", BinaryenTypeInt32());
    auto &sample = ctx->add_variable("SAMPLE$", BinaryenTypeInt32());

    auto &env = ctx->add_variable("env$", BinaryenTypeInt32());
    auto &out = ctx->add_variable("OUT", BinaryenTypeFloat64());

    define_binary_operator("+", BinaryenAddFloat64());
    define_binary_operator("-", BinaryenMinFloat64());
    define_binary_operator("*", BinaryenMulFloat64());
    define_binary_operator("/", BinaryenDivFloat64());

    ctx->push_context();
    auto &phase = ctx->add_parameter("phase", BinaryenTypeFloat64());
    ctx->add_env();
    ctx->add_function(
        "sin",
        BinaryenCall(ctx->module(), "wasmwasm_sin",
                     std::array{phase.get_local(ctx->module())}.data(), 1,
                     BinaryenTypeFloat64()),
        BinaryenTypeFloat64(), 0);
    ctx->pop_context();

    auto inner_block_children = std::array{
        expression_emitter->create(inner_body),

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

    return ctx->module();
}

void MainModuleBuilder::define_binary_operator(const std::string &symbol,
                                               BinaryenOp op) {
    ctx->push_context();
    auto previous_offset = ctx->offset();

    auto &rhs = ctx->add_parameter("rhs$", BinaryenTypeFloat64());
    auto &outer_env = ctx->add_env();

    ctx->push_context();

    auto &lhs = ctx->add_parameter("lhs$", BinaryenTypeFloat64());
    auto &inner_env = ctx->add_env();

    auto &inner_fun = ctx->add_function(
        symbol + "$inner",
        BinaryenBinary(
            ctx->module(), op,
            BinaryenLoad(
                ctx->module(), 8, false, rhs.offset, 8, BinaryenTypeFloat64(),
                BinaryenLoad(ctx->module(), 4, false, 0, 4, BinaryenTypeInt32(),
                             inner_env.get_local(ctx->module()), "memory"),
                "memory"),
            lhs.get_local(ctx->module())),
        BinaryenTypeFloat64(), 0);

    ctx->pop_context();

    auto outer_fun_body = std::array{
        BinaryenStore(
            ctx->module(), 8, rhs.offset, 8, outer_env.get_local(ctx->module()),
            rhs.get_local(ctx->module()), BinaryenTypeFloat64(), "memory"),
        BinaryenStore(ctx->module(), 4, outer_env.offset, 4,
                      outer_env.get_local(ctx->module()),
                      outer_env.get_local(ctx->module()), BinaryenTypeInt32(),
                      "memory"),
        BinaryenStore(
            ctx->module(), 4, ctx->offset(), 4,
            outer_env.get_local(ctx->module()),
            BinaryenConst(ctx->module(), BinaryenLiteralInt32(inner_fun.local)),
            BinaryenTypeInt32(), "memory"),
        BinaryenBinary(ctx->module(), BinaryenAddInt32(),
                       outer_env.get_local(ctx->module()),
                       BinaryenConst(ctx->module(),
                                     BinaryenLiteralInt32(outer_env.offset)))};

    auto &outer_fun = ctx->add_function(
        symbol,
        BinaryenBlock(ctx->module(), nullptr, outer_fun_body.data(),
                      outer_fun_body.size(), BinaryenTypeInt32()),
        BinaryenTypeInt32(), ctx->offset() - previous_offset + 4);

    ctx->pop_context();
}
