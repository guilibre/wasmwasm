#include "code_gen.hpp"

#include "ast.hpp"
#include "tokenizer.hpp"

#include "binaryen-c.h"
#include "wasm.h"

#include <algorithm>
#include <cmath>
#include <iterator>
#include <numbers>
#include <stdexcept>
#include <string>
#include <unordered_map>
#include <utility>
#include <variant>
#include <vector>

namespace {

template <class... Ts> struct overloaded : Ts... {
    using Ts::operator()...;
};
template <class... Ts> overloaded(Ts...) -> overloaded<Ts...>;

auto generate_output(
    BinaryenModuleRef module, BinaryenIndex offset,
    std::unordered_map<std::string, std::pair<BinaryenIndex, BinaryenType>>
        &variables,
    std::unordered_map<std::string, BinaryenType> &globals,
    std::unordered_map<std::string, BinaryenLiteral> &constants,
    const ExprPtr &expr, double sample_freq) -> BinaryenExpressionRef {

    auto get_literal = [&](const Expr::Literal &lit) {
        return BinaryenConst(module, BinaryenLiteralFloat64(std::stod(
                                         std::string(lit.value.lexeme))));
    };

    auto get_variable =
        [&](const Expr::Variable &var) -> BinaryenExpressionRef {
        std::string name = std::string(var.name.lexeme);

        if (auto it = globals.find(name); it != globals.end()) {
            if (name == "TIME") {
                return BinaryenBinary(
                    module, BinaryenMulFloat64(),
                    BinaryenGlobalGet(module, "TIME", BinaryenTypeFloat64()),
                    BinaryenConst(module, BinaryenLiteralFloat64(sample_freq)));
            }
            return BinaryenGlobalGet(module, name.c_str(), it->second);
        }

        if (auto it = constants.find(name); it != constants.end()) {
            return BinaryenConst(module, it->second);
        }

        if (auto it = variables.find(name); it != variables.end()) {
            return BinaryenLocalGet(module, it->second.first,
                                    it->second.second);
        }

        throw std::runtime_error("Unknown variable: " + name);
    };

    auto get_binary = [&](const Expr::Binary &bin) -> BinaryenExpressionRef {
        auto *lhs = generate_output(module, offset, variables, globals,
                                    constants, bin.lhs, sample_freq);
        auto *rhs = generate_output(module, offset, variables, globals,
                                    constants, bin.rhs, sample_freq);

        switch (bin.op.kind) {
        case TokenKind::Plus:
            return BinaryenBinary(module, BinaryenAddFloat64(), lhs, rhs);
        case TokenKind::Minus:
            return BinaryenBinary(module, BinaryenSubFloat64(), lhs, rhs);
        case TokenKind::Star:
            return BinaryenBinary(module, BinaryenMulFloat64(), lhs, rhs);
        case TokenKind::Slash:
            return BinaryenBinary(module, BinaryenDivFloat64(), lhs, rhs);
        default:
            throw std::runtime_error("Unsupported binary operator");
        }
    };

    auto get_assignment =
        [&](const Expr::Assignment &asg) -> BinaryenExpressionRef {
        auto *val = generate_output(module, offset, variables, globals,
                                    constants, asg.value, sample_freq);
        std::string name = std::string(asg.name.lexeme);

        auto it = variables.find(name);
        if (it == variables.end()) {
            BinaryenIndex index = offset + variables.size();
            variables.emplace(name,
                              std::make_pair(index, BinaryenTypeFloat64()));
            return BinaryenLocalSet(module, index, val);
        }
        return BinaryenLocalSet(module, it->second.first, val);
    };

    auto get_call = [&](const Expr::Call &call) -> BinaryenExpressionRef {
        const auto *callee = std::get_if<Expr::Variable>(&call.callee->node);
        if (!callee)
            throw std::runtime_error("Only named function calls supported");
        if (!call.argument)
            throw std::runtime_error("Function call missing argument");

        std::string name = std::string(callee->name.lexeme);
        auto *arg = generate_output(module, offset, variables, globals,
                                    constants, call.argument, sample_freq);

        if (name == "sin") {
            return BinaryenCall(module, "wasmwasm_sin", &arg, 1,
                                BinaryenTypeFloat64());
        }
        throw std::runtime_error("Unknown function: " + name);
    };

    auto visitor = overloaded{
        [&](const Expr::Literal &lit) { return get_literal(lit); },
        [&](const Expr::Variable &var) { return get_variable(var); },
        [&](const Expr::Binary &bin) { return get_binary(bin); },
        [&](const Expr::Assignment &asg) { return get_assignment(asg); },
        [&](const Expr::Call &call) { return get_call(call); }};

    return std::visit(visitor, expr->node);
}

auto get_types(
    std::unordered_map<std::string, std::pair<BinaryenIndex, BinaryenType>>
        &variables) -> std::vector<BinaryenType> {
    std::vector<std::pair<std::string, std::pair<BinaryenIndex, BinaryenType>>>
        sorted_variables(variables.begin(), variables.end());

    std::ranges::sort(sorted_variables, [](const auto &a, const auto &b) {
        return a.second.first < b.second.first;
    });

    std::vector<BinaryenType> var_types;
    var_types.reserve(sorted_variables.size());
    std::ranges::transform(sorted_variables, std::back_inserter(var_types),
                           [](const auto &p) { return p.second.second; });

    return var_types;
}

auto create_main_function(BinaryenModuleRef module, double sample_freq,
                          const std::vector<ExprPtr> &exprs,
                          BinaryenModuleRef math_module) -> void {
    BinaryenAddMemoryImport(module, "memory", "env", "memory", 0);

    auto move_module_items = [&](auto &from, auto add_fn) {
        while (!from.empty()) {
            auto item = std::move(from.back());
            from.pop_back();
            add_fn(std::move(item));
        }
    };

    move_module_items(math_module->globals,
                      [&](auto g) { module->addGlobal(std::move(g)); });
    move_module_items(math_module->memories,
                      [&](auto m) { module->addMemory(std::move(m)); });
    move_module_items(math_module->functions, [&](auto f) {
        for (const auto &ex : math_module->exports) {
            if (ex->getInternalName()->str == f->name.str) {
                f->setExplicitName(ex->name);
                break;
            }
        }
        module->addFunction(std::move(f));
    });

    auto make_const_i32 = [&](int32_t val) {
        return BinaryenConst(module, BinaryenLiteralInt32(val));
    };
    auto make_const_f64 = [&](double val) {
        return BinaryenConst(module, BinaryenLiteralFloat64(val));
    };

    std::unordered_map<std::string, std::pair<BinaryenIndex, BinaryenType>>
        parameters;
    std::unordered_map<std::string, std::pair<BinaryenIndex, BinaryenType>>
        variables;
    std::unordered_map<std::string, BinaryenType> globals;
    std::unordered_map<std::string, BinaryenLiteral> constants;

    auto time_type = BinaryenTypeFloat64();
    BinaryenAddGlobal(module, "TIME", time_type, true, make_const_f64(0.0));
    globals.emplace("TIME", time_type);
    auto time_get = [&]() {
        return BinaryenGlobalGet(module, "TIME", time_type);
    };

    constants.emplace("PI", BinaryenLiteralFloat64(std::numbers::pi_v<double>));

    auto add_param = [&](std::string name, BinaryenType type) {
        parameters.emplace(name, std::make_pair(parameters.size(), type));
    };
    add_param("BASE_PTR", BinaryenTypeInt32());
    add_param("NUM_SAMPLES", BinaryenTypeInt32());
    add_param("NUM_CHANNELS", BinaryenTypeInt32());

    auto get_param = [&](const std::string &name) {
        auto [idx, type] = parameters.at(name);
        return BinaryenLocalGet(module, idx, type);
    };

    auto add_var = [&](std::string name, BinaryenType type) {
        variables.emplace(
            name, std::make_pair(variables.size() + parameters.size(), type));
    };
    add_var("CHANNEL", BinaryenTypeInt32());
    add_var("SAMPLE", BinaryenTypeInt32());

    auto get_var = [&](const std::string &name) {
        auto [idx, type] = variables.at(name);
        return BinaryenLocalGet(module, idx, type);
    };
    auto set_var = [&](const std::string &name, BinaryenExpressionRef expr) {
        auto [idx, _] = variables.at(name);
        return BinaryenLocalSet(module, idx, expr);
    };

    auto *sample_cond =
        BinaryenBinary(module, BinaryenLtUInt32(), get_var("SAMPLE"),
                       get_param("NUM_SAMPLES"));

    auto *channel_cond =
        BinaryenBinary(module, BinaryenLtUInt32(), get_var("CHANNEL"),
                       get_param("NUM_CHANNELS"));

    auto *index_expr = BinaryenBinary(
        module, BinaryenAddInt32(), get_var("CHANNEL"),
        BinaryenBinary(module, BinaryenMulInt32(), get_var("SAMPLE"),
                       get_param("NUM_CHANNELS")));

    auto *address =
        BinaryenBinary(module, BinaryenAddInt32(), get_param("BASE_PTR"),
                       BinaryenBinary(module, BinaryenMulInt32(),
                                      make_const_i32(4), index_expr));

    std::vector<BinaryenExpressionRef> binaryen_exprs;
    binaryen_exprs.reserve(exprs.size());
    std::ranges::transform(
        exprs, std::back_inserter(binaryen_exprs), [&](const auto &expr) {
            return generate_output(module, parameters.size(), variables,
                                   globals, constants, expr, sample_freq);
        });

    auto *output_block =
        BinaryenBlock(module, "output_block", binaryen_exprs.data(),
                      binaryen_exprs.size(), BinaryenTypeNone());

    if (!variables.contains("OUT")) throw std::runtime_error("no OUT");
    auto [idx, type] = variables.at("OUT");
    auto *get_out = BinaryenLocalGet(module, idx, type);

    auto *assign_out =
        BinaryenStore(module, 4, 0, 4, address,
                      BinaryenUnary(module, BinaryenDemoteFloat64(), get_out),
                      BinaryenTypeFloat32(), "memory");

    std::vector<BinaryenExpressionRef> inner_block_children = {
        output_block, assign_out,
        set_var("CHANNEL",
                BinaryenBinary(module, BinaryenAddInt32(), get_var("CHANNEL"),
                               make_const_i32(1))),
        BinaryenBreak(module, "inner_loop", channel_cond, nullptr)};
    auto *inner_loop = BinaryenLoop(
        module, "inner_loop",
        BinaryenBlock(module, "inner_block", inner_block_children.data(),
                      inner_block_children.size(), BinaryenTypeNone()));

    auto *pass_time =
        BinaryenGlobalSet(module, "TIME",
                          BinaryenBinary(module, BinaryenAddFloat64(),
                                         time_get(), make_const_f64(1.0)));

    std::vector<BinaryenExpressionRef> outer_block_children = {
        set_var("CHANNEL", make_const_i32(0)), inner_loop, pass_time,
        set_var("SAMPLE", BinaryenBinary(module, BinaryenAddInt32(),
                                         get_var("SAMPLE"), make_const_i32(1))),
        BinaryenBreak(module, "outer_loop", sample_cond, nullptr)};
    auto *outer_loop = BinaryenLoop(
        module, "outer_loop",
        BinaryenBlock(module, "outer_block", outer_block_children.data(),
                      outer_block_children.size(), BinaryenTypeNone()));

    std::vector<BinaryenExpressionRef> body_block = {
        set_var("SAMPLE", make_const_i32(0)), outer_loop};

    BinaryenExpressionRef body =
        BinaryenBlock(module, "body", body_block.data(), body_block.size(),
                      BinaryenTypeNone());

    BinaryenAddFunction(
        module, "main",
        BinaryenTypeCreate(get_types(parameters).data(), parameters.size()),
        BinaryenTypeNone(), get_types(variables).data(), variables.size(),
        body);
    BinaryenAddFunctionExport(module, "main", "main");
}

} // namespace

namespace code_gen {

auto insert_expr(double sample_freq, const std::vector<ExprPtr> &exprs,
                 BinaryenModuleRef math_module) -> BinaryenModuleRef {
    auto *module = BinaryenModuleCreate();

    create_main_function(module, sample_freq, exprs, math_module);
    module->features = BinaryenFeatureAll();

    return module;
}

} // namespace code_gen