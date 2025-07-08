#include "code_gen.hpp"

#include "ast.hpp"
#include "binaryen-c.h"
#include "tokenizer.hpp"
#include "wasm.h"

#include <algorithm>
#include <cmath>
#include <exception>
#include <fstream>
#include <iostream>
#include <stdexcept>
#include <string>
#include <string_view>
#include <unordered_map>
#include <utility>
#include <variant>
#include <vector>

using namespace wasm;

namespace {

template <class... Ts> struct overloaded : Ts... {
    using Ts::operator()...;
};
template <class... Ts> overloaded(Ts...) -> overloaded<Ts...>;

auto generate_output(
    Module *module, BinaryenIndex offset,
    std::unordered_map<std::string_view, std::pair<BinaryenIndex, BinaryenType>>
        &variables,
    const ExprPtr &expr) -> BinaryenExpressionRef {
    auto visitor = overloaded{
        [&](const Expr::Literal &lit) -> BinaryenExpressionRef {
            float value = std::stof(std::string(lit.value.lexeme));
            return BinaryenConst(module, BinaryenLiteralFloat32(value));
        },
        [&](const Expr::Variable &var) -> BinaryenExpressionRef {
            auto it = variables.find(var.name.lexeme);
            if (it == variables.end())
                throw std::runtime_error("Unknown variable: " +
                                         std::string(var.name.lexeme));
            return BinaryenLocalGet(module, it->second.first,
                                    it->second.second);
        },
        [&](const Expr::Binary &bin) -> BinaryenExpressionRef {
            auto *left = generate_output(module, offset, variables, bin.lhs);
            auto *right = generate_output(module, offset, variables, bin.rhs);

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
            auto *value = generate_output(module, offset, variables, asg.value);

            auto it = variables.find(asg.name.lexeme);
            if (it == variables.end()) {
                auto idx = variables.size() + offset;
                variables.emplace(asg.name.lexeme,
                                  std::make_pair(idx, BinaryenTypeFloat32()));
                return BinaryenLocalSet(module, idx, value);
            }
            return BinaryenLocalSet(module, it->second.first, value);
        },
        [&](const Expr::Call &call) -> BinaryenExpressionRef {
            throw std::runtime_error(
                "Function application not implemented in codegen");
        }};

    return std::visit(visitor, expr->node);
}

auto get_types(
    std::unordered_map<std::string_view, std::pair<BinaryenIndex, BinaryenType>>
        &variables) -> std::vector<BinaryenType> {
    std::vector<
        std::pair<std::string_view, std::pair<BinaryenIndex, BinaryenType>>>
        sorted_variables(variables.begin(), variables.end());

    std::sort(sorted_variables.begin(), sorted_variables.end(),
              [](const auto &a, const auto &b) {
                  return a.second.first < b.second.first;
              });

    std::vector<BinaryenType> var_types;
    var_types.reserve(sorted_variables.size());
    std::transform(sorted_variables.begin(), sorted_variables.end(),
                   std::back_inserter(var_types),
                   [](const auto &p) { return p.second.second; });

    return var_types;
}

auto create_main_function(wasm::Module *module, float sample_freq,
                          const ExprPtr &expr) -> void {
    BinaryenAddMemoryImport(module, "memory", "env", "memory", 0);
    BinaryenAddFunctionImport(module, "sin", "Math", "sin",
                              BinaryenTypeFloat32(), BinaryenTypeFloat32());

    auto *const_0_int = BinaryenConst(module, BinaryenLiteralInt32(0));
    auto *const_1_int = BinaryenConst(module, BinaryenLiteralInt32(1));

    auto time_type = BinaryenTypeFloat32();
    BinaryenAddGlobal(module, "TIME", time_type, true,
                      BinaryenConst(module, BinaryenLiteralFloat32(0.0)));
    auto *get_time = BinaryenGlobalGet(module, "TIME", time_type);

    std::unordered_map<std::string_view, std::pair<BinaryenIndex, BinaryenType>>
        parameters;
    std::unordered_map<std::string_view, std::pair<BinaryenIndex, BinaryenType>>
        variables;

    parameters.emplace("BASE_PTR",
                       std::make_pair(parameters.size(), BinaryenTypeInt32()));
    parameters.emplace("NUM_SAMPLES",
                       std::make_pair(parameters.size(), BinaryenTypeInt32()));
    parameters.emplace("NUM_CHANNELS",
                       std::make_pair(parameters.size(), BinaryenTypeInt32()));

    auto *base_ptr = BinaryenLocalGet(module, parameters.at("BASE_PTR").first,
                                      BinaryenTypeInt32());
    auto *get_num_samples = BinaryenLocalGet(
        module, parameters.at("NUM_SAMPLES").first, BinaryenTypeInt32());
    auto *get_num_channels = BinaryenLocalGet(
        module, parameters.at("NUM_CHANNELS").first, BinaryenTypeInt32());

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
    auto *sample_get = BinaryenLocalGet(module, sample_loc, sample_type);

    auto *sample_init = BinaryenLocalSet(module, sample_loc, const_0_int);
    auto *sample_inc = BinaryenLocalSet(
        module, sample_loc,
        BinaryenBinary(module, BinaryenAddInt32(), sample_get, const_1_int));
    auto *sample_cond =
        BinaryenBinary(module, BinaryenLtUInt32(), sample_get, get_num_samples);

    auto channel_loc = variables.at("CHANNEL").first;
    auto channel_type = variables.at("CHANNEL").second;
    auto *channel_get = BinaryenLocalGet(module, channel_loc, channel_type);

    auto *channel_init = BinaryenLocalSet(module, channel_loc, const_0_int);
    auto *channel_inc = BinaryenLocalSet(
        module, channel_loc,
        BinaryenBinary(module, BinaryenAddInt32(), channel_get, const_1_int));
    auto *channel_cond = BinaryenBinary(module, BinaryenLtUInt32(), channel_get,
                                        get_num_channels);

    auto *index_expr =
        BinaryenBinary(module, BinaryenAddInt32(), channel_get,
                       BinaryenBinary(module, BinaryenMulInt32(), sample_get,
                                      get_num_channels));
    auto *address = BinaryenBinary(
        module, BinaryenAddInt32(), base_ptr,
        BinaryenBinary(module, BinaryenMulInt32(),
                       BinaryenConst(module, BinaryenLiteralInt32(4)),
                       index_expr));

    auto *generate_out =
        generate_output(module, parameters.size(), variables, expr);

    auto it = variables.find("OUT");
    if (it == variables.end()) throw std::runtime_error("no OUT");

    auto out_loc = it->second.first;
    auto out_type = it->second.second;
    auto *get_out = BinaryenLocalGet(module, out_loc, out_type);

    auto *assign_out = BinaryenStore(module, 4, 0, 4, address, get_out,
                                     BinaryenTypeFloat32(), "memory");

    std::vector<Expression *> inner_block_children = {
        generate_out, assign_out, channel_inc,
        BinaryenBreak(module, "inner_loop", channel_cond, nullptr)};
    auto *inner_block =
        BinaryenBlock(module, "inner_block", inner_block_children.data(),
                      inner_block_children.size(), BinaryenTypeNone());
    auto *inner_loop = BinaryenLoop(module, "inner_loop", inner_block);

    auto *pass_time = BinaryenGlobalSet(
        module, "TIME",
        BinaryenBinary(
            module, BinaryenAddFloat32(), get_time,
            BinaryenConst(module, BinaryenLiteralFloat32(sample_freq))));

    std::vector<Expression *> outer_block_children = {
        channel_init, inner_loop, pass_time, sample_inc,
        BinaryenBreak(module, "outer_loop", sample_cond, nullptr)};
    auto *outer_block =
        BinaryenBlock(module, "outer_block", outer_block_children.data(),
                      outer_block_children.size(), BinaryenTypeNone());
    auto *outer_loop = BinaryenLoop(module, "outer_loop", outer_block);

    std::vector<Expression *> body_block = {sample_init, outer_loop};

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

auto write_module_to_file(wasm::Module *module) -> int {
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

auto insert_expr(float sample_freq, const ExprPtr &expr) -> int {
    auto *module = BinaryenModuleCreate();
    try {
        create_main_function(module, sample_freq, expr);

        if (!BinaryenModuleValidate(module))
            throw std::runtime_error("error validating module\n");

        if (write_module_to_file(module) != 0)
            throw std::runtime_error("error writing module\n");

        BinaryenModuleDispose(module);
        return 0;
    } catch (std::exception &ex) {
        std::cerr << ex.what() << '\n';
        BinaryenModuleDispose(module);
        return 1;
    } catch (...) {
        std::cerr << "unexpected error\n";
        BinaryenModuleDispose(module);
        return 1;
    }
}

} // namespace code_gen