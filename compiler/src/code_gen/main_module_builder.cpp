#include "main_module_builder.hpp"

#include "code_gen_context.hpp"
#include "expression_emitter.hpp"

#include "binaryen-c.h"
#include "wasm.h"

#include <algorithm>
#include <memory>
#include <string_view>
#include <utility>

namespace {

auto get_types(
    const std::unordered_map<std::string_view, BinaryenVariable> &variables)
    -> std::vector<BinaryenType> {
    std::vector<std::pair<std::string, BinaryenVariable>> sorted_variables(
        variables.begin(), variables.end());

    std::ranges::sort(sorted_variables, [](const auto &a, const auto &b) {
        return a.second.local < b.second.local;
    });

    std::vector<BinaryenType> var_types;
    var_types.reserve(sorted_variables.size());
    std::ranges::transform(
        sorted_variables, std::back_inserter(var_types),
        [](const auto &p) { return p.second.binaryen_type; });

    return var_types;
}

} // namespace

MainModuleBuilder::MainModuleBuilder(BinaryenModuleRef math_module,
                                     double sample_freq)
    : math_module(math_module), sample_freq(sample_freq) {}

auto MainModuleBuilder::build(const std::vector<ExprPtr> &exprs)
    -> BinaryenModuleRef {
    ctx.module = BinaryenModuleCreate();

    if (ctx.module == nullptr)
        throw std::runtime_error("Module not initialized");

    BinaryenModuleSetFeatures(ctx.module, BinaryenFeatureAll());

    ExpressionEmitter expression_emitter(std::make_shared<CodeGenContext>(ctx));

    BinaryenAddMemoryImport(ctx.module, "memory", "env", "memory", 0);

    auto move_module_items = [&](auto &from, auto add_fn) {
        while (!from.empty()) {
            auto item = std::move(from.back());
            from.pop_back();
            add_fn(std::move(item));
        }
    };

    move_module_items(math_module->globals, [&](auto &&g) {
        ctx.module->addGlobal(std::forward<decltype(g)>(g));
    });
    move_module_items(math_module->functions, [&](auto f) {
        for (const auto &ex : math_module->exports) {
            if (ex->getInternalName()->str == f->name.str) {
                f->setExplicitName(ex->name);
                break;
            }
        }
        ctx.module->addFunction(std::move(f));
    });

    auto make_const_i32 = [&](int32_t val) {
        return BinaryenConst(ctx.module, BinaryenLiteralInt32(val));
    };
    auto make_const_f64 = [&](double val) {
        return BinaryenConst(ctx.module, BinaryenLiteralFloat64(val));
    };

    ctx.globals["TIME"] =
        BinaryenVariable{.binaryen_type = BinaryenTypeFloat64()};
    BinaryenAddGlobal(ctx.module, "TIME", BinaryenTypeFloat64(), true,
                      make_const_f64(0.0));
    auto time_get = [&]() {
        return BinaryenGlobalGet(ctx.module, "TIME", BinaryenTypeFloat64());
    };

    ctx.constants["PI"] = BinaryenLiteralFloat64(std::numbers::pi_v<double>);

    auto add_param = [&](const std::string &name, BinaryenType type) {
        ctx.parameters[name] = BinaryenVariable{
            .local = static_cast<BinaryenIndex>(ctx.parameters.size()),
            .binaryen_type = type};
    };
    add_param("BASE_PTR", BinaryenTypeInt32());
    add_param("NUM_SAMPLES", BinaryenTypeInt32());
    add_param("NUM_CHANNELS", BinaryenTypeInt32());

    auto get_param = [&](const std::string &name, BinaryenType type) {
        return BinaryenLocalGet(ctx.module, ctx.parameters[name].local, type);
    };

    auto add_var = [&](const std::string &name, BinaryenType binaryen_type,
                       TypePtr type) {
        ctx.variables[name] = BinaryenVariable{
            .local = static_cast<BinaryenIndex>(ctx.variables.size() +
                                                ctx.parameters.size()),
            .binaryen_type = binaryen_type,
            .type = std::move(type),
        };
    };
    add_var("CHANNEL", BinaryenTypeInt32(),
            Type::make<TypeBase>(BaseTypeKind::Int));
    add_var("SAMPLE", BinaryenTypeInt32(),
            Type::make<TypeBase>(BaseTypeKind::Int));

    auto get_var = [&](const std::string &name) {
        return BinaryenLocalGet(ctx.module, ctx.variables[name].local,
                                ctx.variables[name].binaryen_type);
    };
    auto set_var = [&](const std::string &name, BinaryenExpressionRef expr) {
        return BinaryenLocalSet(ctx.module, ctx.variables[name].local, expr);
    };

    auto *sample_cond =
        BinaryenBinary(ctx.module, BinaryenLtUInt32(), get_var("SAMPLE"),
                       get_param("NUM_SAMPLES", BinaryenTypeInt32()));
    auto *channel_cond =
        BinaryenBinary(ctx.module, BinaryenLtUInt32(), get_var("CHANNEL"),
                       get_param("NUM_CHANNELS", BinaryenTypeInt32()));

    auto *index_expr = BinaryenBinary(
        ctx.module, BinaryenAddInt32(), get_var("CHANNEL"),
        BinaryenBinary(ctx.module, BinaryenMulInt32(), get_var("SAMPLE"),
                       get_param("NUM_CHANNELS", BinaryenTypeInt32())));

    auto *address =
        BinaryenBinary(ctx.module, BinaryenAddInt32(),
                       get_param("BASE_PTR", BinaryenTypeInt32()),
                       BinaryenBinary(ctx.module, BinaryenMulInt32(),
                                      make_const_i32(4), index_expr));

    std::vector<BinaryenExpressionRef> binaryen_exprs;
    binaryen_exprs.reserve(exprs.size());
    for (const auto &expr : exprs)
        binaryen_exprs.emplace_back(expression_emitter.create(expr));

    auto *output_block =
        BinaryenBlock(ctx.module, "output_block", binaryen_exprs.data(),
                      binaryen_exprs.size(), BinaryenTypeNone());

    if (!ctx.variables.contains("OUT"))
        throw std::runtime_error("Output variable OUT not defined");

    auto *get_out = BinaryenLocalGet(ctx.module, ctx.variables["OUT"].local,
                                     ctx.variables["OUT"].binaryen_type);

    auto *assign_out = BinaryenStore(
        ctx.module, 4, 0, 4, address,
        BinaryenUnary(ctx.module, BinaryenDemoteFloat64(), get_out),
        BinaryenTypeFloat32(), "memory");

    std::vector<BinaryenExpressionRef> inner_block_children = {
        output_block, assign_out,
        set_var("CHANNEL",
                BinaryenBinary(ctx.module, BinaryenAddInt32(),
                               get_var("CHANNEL"), make_const_i32(1))),
        BinaryenBreak(ctx.module, "inner_loop", channel_cond, nullptr)};
    auto *inner_loop = BinaryenLoop(
        ctx.module, "inner_loop",
        BinaryenBlock(ctx.module, "inner_block", inner_block_children.data(),
                      inner_block_children.size(), BinaryenTypeNone()));

    auto *pass_time =
        BinaryenGlobalSet(ctx.module, "TIME",
                          BinaryenBinary(ctx.module, BinaryenAddFloat64(),
                                         time_get(), make_const_f64(1.0)));

    std::vector<BinaryenExpressionRef> outer_block_children = {
        set_var("CHANNEL", make_const_i32(0)), inner_loop, pass_time,
        set_var("SAMPLE", BinaryenBinary(ctx.module, BinaryenAddInt32(),
                                         get_var("SAMPLE"), make_const_i32(1))),
        BinaryenBreak(ctx.module, "outer_loop", sample_cond, nullptr)};
    auto *outer_loop = BinaryenLoop(
        ctx.module, "outer_loop",
        BinaryenBlock(ctx.module, "outer_block", outer_block_children.data(),
                      outer_block_children.size(), BinaryenTypeNone()));

    std::vector<BinaryenExpressionRef> body_block = {
        set_var("SAMPLE", make_const_i32(0)), outer_loop};

    auto *body = BinaryenBlock(ctx.module, "body", body_block.data(),
                               body_block.size(), BinaryenTypeNone());

    BinaryenAddFunction(ctx.module, "main",
                        BinaryenTypeCreate(get_types(ctx.parameters).data(),
                                           ctx.parameters.size()),
                        BinaryenTypeNone(), get_types(ctx.variables).data(),
                        ctx.variables.size(), body);
    BinaryenAddFunctionExport(ctx.module, "main", "main");

    if (!ctx.function_indices.empty()) {
        BinaryenAddTable(ctx.module, "func_table", ctx.function_indices.size(),
                         ctx.function_indices.size(), BinaryenTypeFuncref());
        std::vector<std::string> names;
        std::vector<const char *> elems;
        elems.reserve(ctx.function_indices.size());
        names.reserve(ctx.function_indices.size());
        for (const auto &[name, pair] : ctx.function_indices) {
            names.emplace_back(name);
            elems.emplace_back(names.back().c_str());
        }

        BinaryenAddActiveElementSegment(
            ctx.module, "func_table", "func_table_seg", elems.data(),
            elems.size(), BinaryenConst(ctx.module, BinaryenLiteralInt32(0)));
    }

    return ctx.module;
}
