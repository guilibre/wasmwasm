#include "code_gen.hpp"

#include "ast.hpp"
#include "tokenizer.hpp"

#include "binaryen-c.h"
#include "wasm.h"

#include <algorithm>
#include <cmath>
#include <fstream>
#include <iostream>
#include <iterator>
#include <numbers>
#include <stdexcept>
#include <string>
#include <string_view>
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
    const ExprPtr &expr) -> BinaryenExpressionRef {
    auto visitor = overloaded{
        [&](const Expr::Literal &lit) -> BinaryenExpressionRef {
            float value = std::stof(std::string(lit.value.lexeme));
            return BinaryenConst(module, BinaryenLiteralFloat32(value));
        },
        [&](const Expr::Variable &var) -> BinaryenExpressionRef {
            auto it = globals.find(std::string(var.name.lexeme));
            if (it != globals.end())
                return BinaryenGlobalGet(
                    module, std::string(var.name.lexeme).c_str(), it->second);

            auto it2 = constants.find(std::string(var.name.lexeme));
            if (it2 != constants.end())
                return BinaryenConst(module, it2->second);

            auto it3 = variables.find(std::string(var.name.lexeme));
            if (it3 == variables.end())
                throw std::runtime_error("Unknown variable: " +
                                         std::string(var.name.lexeme));
            return BinaryenLocalGet(module, it3->second.first,
                                    it3->second.second);
        },
        [&](const Expr::Binary &bin) -> BinaryenExpressionRef {
            auto *left = generate_output(module, offset, variables, globals,
                                         constants, bin.lhs);
            auto *right = generate_output(module, offset, variables, globals,
                                          constants, bin.rhs);

            switch (bin.op.kind) {
            case TokenKind::Plus:
                return BinaryenBinary(module, BinaryenAddFloat32(), left,
                                      right);
            case TokenKind::Minus:
                return BinaryenBinary(module, BinaryenSubFloat32(), left,
                                      right);
            case TokenKind::Star:
                return BinaryenBinary(module, BinaryenMulFloat32(), left,
                                      right);
            case TokenKind::Slash:
                return BinaryenBinary(module, BinaryenDivFloat32(), left,
                                      right);
            default:
                throw std::runtime_error("unsupported binary operator");
            }
        },
        [&](const Expr::Assignment &asg) -> BinaryenExpressionRef {
            auto *value = generate_output(module, offset, variables, globals,
                                          constants, asg.value);

            auto it = variables.find(std::string(asg.name.lexeme));
            if (it == variables.end()) {
                auto idx = variables.size() + offset;
                variables.emplace(asg.name.lexeme,
                                  std::make_pair(idx, BinaryenTypeFloat32()));
                return BinaryenLocalSet(module, idx, value);
            }
            return BinaryenLocalSet(module, it->second.first, value);
        },
        [&](const Expr::Call &call) -> BinaryenExpressionRef {
            const auto *callee =
                std::get_if<Expr::Variable>(&call.callee->node);
            if (!callee) {
                throw std::runtime_error("Only named function calls supported");
            }
            auto name = callee->name.lexeme;
            if (!call.argument)
                throw std::runtime_error("Function call missing argument");
            auto *arg_expr = generate_output(module, offset, variables, globals,
                                             constants, call.argument);
            if (name == "sin")
                return BinaryenCall(module, "wasmwasm_sin", &arg_expr, 1,
                                    BinaryenTypeFloat32());
            throw std::runtime_error("Unknown function: " + std::string(name));
        }};

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

auto create_main_function(BinaryenModuleRef module, float sample_freq,
                          const std::vector<ExprPtr> &exprs,
                          BinaryenModuleRef functions_module) -> void {
    BinaryenAddMemoryImport(module, "memory", "env", "memory", 0);

    while (!functions_module->globals.empty()) {
        auto gl = std::move(functions_module->globals.back());
        functions_module->globals.pop_back();
        module->addGlobal(std::move(gl));
    }

    while (!functions_module->memories.empty()) {
        auto mem = std::move(functions_module->memories.back());
        functions_module->memories.pop_back();
        module->addMemory(std::move(mem));
    }

    while (!functions_module->functions.empty()) {
        auto fn = std::move(functions_module->functions.back());
        functions_module->functions.pop_back();
        for (const auto &ex : functions_module->exports) {
            if (ex->getInternalName()->str == fn->name.str) {
                fn->setExplicitName(ex->name);
                break;
            }
        }
        module->addFunction(std::move(fn));
    }

    auto const_0_int = [&]() {
        return BinaryenConst(module, BinaryenLiteralInt32(0));
    };
    auto const_1_int = [&]() {
        return BinaryenConst(module, BinaryenLiteralInt32(1));
    };

    std::unordered_map<std::string, std::pair<BinaryenIndex, BinaryenType>>
        parameters;
    std::unordered_map<std::string, std::pair<BinaryenIndex, BinaryenType>>
        variables;
    std::unordered_map<std::string, BinaryenType> globals;
    std::unordered_map<std::string, BinaryenLiteral> constants;

    auto time_type = BinaryenTypeFloat32();
    BinaryenAddGlobal(module, "TIME", time_type, true,
                      BinaryenConst(module, BinaryenLiteralFloat32(10.0)));
    globals.emplace("TIME", time_type);
    auto time_get = [&]() {
        return BinaryenGlobalGet(module, "TIME", time_type);
    };

    constants.emplace("PI", BinaryenLiteralFloat32(std::numbers::pi_v<float>));

    parameters.emplace("BASE_PTR",
                       std::make_pair(parameters.size(), BinaryenTypeInt32()));
    parameters.emplace("NUM_SAMPLES",
                       std::make_pair(parameters.size(), BinaryenTypeInt32()));
    parameters.emplace("NUM_CHANNELS",
                       std::make_pair(parameters.size(), BinaryenTypeInt32()));

    auto base_ptr = [&]() {
        return BinaryenLocalGet(module, parameters.at("BASE_PTR").first,
                                BinaryenTypeInt32());
    };
    auto get_num_samples = [&]() {
        return BinaryenLocalGet(module, parameters.at("NUM_SAMPLES").first,
                                BinaryenTypeInt32());
    };
    auto get_num_channels = [&]() {
        return BinaryenLocalGet(module, parameters.at("NUM_CHANNELS").first,
                                BinaryenTypeInt32());
    };

    variables.emplace("CHANNEL",
                      std::make_pair(static_cast<unsigned int>(
                                         variables.size() + parameters.size()),
                                     BinaryenTypeInt32()));
    variables.emplace("SAMPLE",
                      std::make_pair(static_cast<unsigned int>(
                                         variables.size() + parameters.size()),
                                     BinaryenTypeInt32()));

    auto sample_loc = variables.at("SAMPLE").first;
    auto sample_type = variables.at("SAMPLE").second;
    auto sample_get = [&]() {
        return BinaryenLocalGet(module, sample_loc, sample_type);
    };

    auto sample_init = [&]() {
        return BinaryenLocalSet(module, sample_loc, const_0_int());
    };
    auto sample_inc = [&]() {
        return BinaryenLocalSet(module, sample_loc,
                                BinaryenBinary(module, BinaryenAddInt32(),
                                               sample_get(), const_1_int()));
    };
    auto sample_cond = [&]() {
        return BinaryenBinary(module, BinaryenLtUInt32(), sample_get(),
                              get_num_samples());
    };

    auto channel_loc = variables.at("CHANNEL").first;
    auto channel_type = variables.at("CHANNEL").second;
    auto channel_get = [&]() {
        return BinaryenLocalGet(module, channel_loc, channel_type);
    };

    auto channel_init = [&]() {
        return BinaryenLocalSet(module, channel_loc, const_0_int());
    };
    auto channel_inc = [&]() {
        return BinaryenLocalSet(module, channel_loc,
                                BinaryenBinary(module, BinaryenAddInt32(),
                                               channel_get(), const_1_int()));
    };
    auto channel_cond = [&]() {
        return BinaryenBinary(module, BinaryenLtUInt32(), channel_get(),
                              get_num_channels());
    };

    auto index_expr = [&]() {
        return BinaryenBinary(module, BinaryenAddInt32(), channel_get(),
                              BinaryenBinary(module, BinaryenMulInt32(),
                                             sample_get(), get_num_channels()));
    };
    auto address = [&]() {
        return BinaryenBinary(
            module, BinaryenAddInt32(), base_ptr(),
            BinaryenBinary(module, BinaryenMulInt32(),
                           BinaryenConst(module, BinaryenLiteralInt32(4)),
                           index_expr()));
    };

    std::vector<BinaryenExpressionRef> binaryen_exprs;
    binaryen_exprs.reserve(exprs.size());
    std::ranges::transform(
        exprs, std::back_inserter(binaryen_exprs), [&](const auto &expr) {
            return generate_output(module, parameters.size(), variables,
                                   globals, constants, expr);
        });
    auto generate_out = [&]() {
        return BinaryenBlock(module, "output_block", binaryen_exprs.data(),
                             binaryen_exprs.size(), BinaryenTypeNone());
    };

    auto it = variables.find("OUT");
    if (it == variables.end()) throw std::runtime_error("no OUT");

    auto out_loc = it->second.first;
    auto out_type = it->second.second;
    auto get_out = [&]() {
        return BinaryenLocalGet(module, out_loc, out_type);
    };

    auto assign_out = [&]() {
        return BinaryenStore(module, 4, 0, 4, address(), get_out(),
                             BinaryenTypeFloat32(), "memory");
    };

    std::vector<BinaryenExpressionRef> inner_block_children = {
        generate_out(), assign_out(), channel_inc(),
        BinaryenBreak(module, "inner_loop", channel_cond(), nullptr)};
    auto inner_block = [&]() {
        return BinaryenBlock(module, "inner_block", inner_block_children.data(),
                             inner_block_children.size(), BinaryenTypeNone());
    };
    auto inner_loop = [&]() {
        return BinaryenLoop(module, "inner_loop", inner_block());
    };

    auto pass_time = [&]() {
        return BinaryenGlobalSet(
            module, "TIME",
            BinaryenBinary(
                module, BinaryenAddFloat32(), time_get(),
                BinaryenConst(module, BinaryenLiteralFloat32(sample_freq))));
    };

    std::vector<BinaryenExpressionRef> outer_block_children = {
        channel_init(), inner_loop(), pass_time(), sample_inc(),
        BinaryenBreak(module, "outer_loop", sample_cond(), nullptr)};
    auto outer_block = [&]() {
        return BinaryenBlock(module, "outer_block", outer_block_children.data(),
                             outer_block_children.size(), BinaryenTypeNone());
    };
    auto outer_loop = [&]() {
        return BinaryenLoop(module, "outer_loop", outer_block());
    };

    std::vector<BinaryenExpressionRef> body_block = {sample_init(),
                                                     outer_loop()};

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

auto write_module_to_file(BinaryenModuleRef module) -> int {
    std::ofstream out("/tmp/output.wasm", std::ios::binary);
    if (!out) {
        std::cerr << "failed to open file for writing.\n";
        return 1;
    }

    {
        auto buffer = BinaryenModuleAllocateAndWrite(module, nullptr);
        out.write(static_cast<const char *>(buffer.binary),
                  static_cast<long>(buffer.binaryBytes));
        free(buffer.binary);
    }

    out.close();
    return 0;
}

} // namespace

namespace code_gen {

auto insert_expr(float sample_freq, const std::vector<ExprPtr> &exprs,
                 BinaryenModuleRef functions_module) -> int {
    auto *module = BinaryenModuleCreate();

    create_main_function(module, sample_freq, exprs, functions_module);
    module->features = BinaryenFeatureAll();

    if (!BinaryenModuleValidate(module))
        throw std::runtime_error("error validating module\n");

    BinaryenModuleOptimize(module);
    if (write_module_to_file(module) != 0)
        throw std::runtime_error("error writing module\n");

    BinaryenModuleDispose(module);
    return 0;
}

} // namespace code_gen