#include "code_gen.hpp"

#include "binaryen-c.h"
#include "wasm.h"

#include <algorithm>
#include <array>
#include <cmath>
#include <exception>
#include <fstream>
#include <iostream>
#include <map>
#include <stdexcept>
#include <vector>

using namespace wasm;

namespace {

auto do_thing(Module *module,
              const std::map<BinaryenIndex, BinaryenType> &variables,
              BinaryenIndex offset, BinaryenIndex freq)
    -> BinaryenExpressionRef {
    auto *sine_argument = BinaryenBinary(
        module, BinaryenMulFloat32(),
        BinaryenGlobalGet(module, "time", BinaryenTypeFloat32()),
        BinaryenBinary(
            module, BinaryenMulFloat32(),
            BinaryenLocalGet(module, freq, BinaryenTypeFloat32()),
            BinaryenConst(module, BinaryenLiteralFloat32(6.28318530718F))));

    return BinaryenCall(module, "Math_sin", &sine_argument, 1,
                        BinaryenTypeFloat32());
}

template <typename Map> auto map_keys(const Map &m) {
    std::vector<typename Map::mapped_type> out;
    out.reserve(m.size());
    std::transform(m.begin(), m.end(), std::back_inserter(out),
                   [](const auto &p) { return p.first; });
    return out;
}

template <typename Map> auto map_values(const Map &m) {
    std::vector<typename Map::mapped_type> out;
    out.reserve(m.size());
    std::transform(m.begin(), m.end(), std::back_inserter(out),
                   [](const auto &p) { return p.second; });
    return out;
}

auto create_main_function(wasm::Module *module, float sample_freq) -> void {
    BinaryenAddMemoryImport(module, "memory", "env", "memory", 0);
    BinaryenAddFunctionImport(module, "Math_sin", "Math", "sin",
                              BinaryenTypeFloat32(), BinaryenTypeFloat32());
    BinaryenAddGlobal(module, "time", BinaryenTypeFloat32(), true,
                      BinaryenConst(module, BinaryenLiteralFloat32(0.0)));

    std::array<BinaryenType, 4> parameters{};
    std::map<BinaryenIndex, BinaryenType> variables;

    parameters[0] = BinaryenTypeInt32();
    BinaryenIndex base_ptr = 0;
    parameters[1] = BinaryenTypeInt32();
    BinaryenIndex rows = 1;
    parameters[2] = BinaryenTypeInt32();
    BinaryenIndex cols = 2;
    parameters[3] = BinaryenTypeFloat32();
    BinaryenIndex freq = 3;

    BinaryenIndex local_i = variables.size() + parameters.size();
    variables.emplace(local_i, BinaryenTypeInt32());
    BinaryenIndex local_j = variables.size() + parameters.size();
    variables.emplace(local_j, BinaryenTypeInt32());

    auto *init_i = BinaryenLocalSet(
        module, local_i, BinaryenConst(module, BinaryenLiteralInt32(0)));
    auto *cond_i =
        BinaryenBinary(module, BinaryenLtUInt32(),
                       BinaryenLocalGet(module, local_i, BinaryenTypeInt32()),
                       BinaryenLocalGet(module, cols, BinaryenTypeInt32()));
    auto *i_inc = BinaryenLocalSet(
        module, local_i,
        BinaryenBinary(module, BinaryenAddInt32(),
                       BinaryenLocalGet(module, local_i, BinaryenTypeInt32()),
                       BinaryenConst(module, BinaryenLiteralInt32(1))));

    auto *init_j = BinaryenLocalSet(
        module, local_j, BinaryenConst(module, BinaryenLiteralInt32(0)));
    auto *cond_j =
        BinaryenBinary(module, BinaryenLtUInt32(),
                       BinaryenLocalGet(module, local_j, BinaryenTypeInt32()),
                       BinaryenLocalGet(module, rows, BinaryenTypeInt32()));
    auto *j_inc = BinaryenLocalSet(
        module, local_j,
        BinaryenBinary(module, BinaryenAddInt32(),
                       BinaryenLocalGet(module, local_j, BinaryenTypeInt32()),
                       BinaryenConst(module, BinaryenLiteralInt32(1))));

    auto *index_expr = BinaryenBinary(
        module, BinaryenAddInt32(),
        BinaryenLocalGet(module, local_i, BinaryenTypeInt32()),
        BinaryenBinary(module, BinaryenMulInt32(),
                       BinaryenLocalGet(module, local_j, BinaryenTypeInt32()),
                       BinaryenLocalGet(module, cols, BinaryenTypeInt32())));
    auto *address = BinaryenBinary(
        module, BinaryenAddInt32(),
        BinaryenLocalGet(module, 0, BinaryenTypeInt32()),
        BinaryenBinary(module, BinaryenMulInt32(),
                       BinaryenConst(module, BinaryenLiteralInt32(4)),
                       index_expr));
    auto *assign_out =
        BinaryenStore(module, 4, 0, 4, address,
                      do_thing(module, variables, parameters.size(), freq),
                      BinaryenTypeFloat32(), "memory");

    auto *inner_block = BinaryenBlock(
        module, "inner_block",
        std::array<BinaryenExpressionRef, 3>(
            {assign_out, i_inc,
             BinaryenBreak(module, "inner_loop", cond_i, nullptr)})
            .data(),
        3, BinaryenTypeNone());
    auto *inner_loop = BinaryenLoop(module, "inner_loop", inner_block);

    auto *pass_time = BinaryenGlobalSet(
        module, "time",
        BinaryenBinary(
            module, BinaryenAddFloat32(),
            BinaryenGlobalGet(module, "time", BinaryenTypeFloat32()),
            BinaryenConst(module, BinaryenLiteralFloat32(sample_freq))));

    auto *outer_block = BinaryenBlock(
        module, "outer_block",
        std::array<BinaryenExpressionRef, 5>(
            {init_i, inner_loop, pass_time, j_inc,
             BinaryenBreak(module, "outer_loop", cond_j, nullptr)})
            .data(),
        5, BinaryenTypeNone());
    auto *outer_loop = BinaryenLoop(module, "outer_loop", outer_block);

    BinaryenExpressionRef body = BinaryenBlock(
        module, "body",
        std::array<BinaryenExpressionRef, 2>({init_j, outer_loop}).data(), 2,
        BinaryenTypeNone());

    BinaryenAddFunction(module, "main",
                        BinaryenTypeCreate(parameters.data(), 4),
                        BinaryenTypeNone(), map_values(variables).data(),
                        variables.size(), body);
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
        out.write(static_cast<const char *>(buffer.binary), buffer.binaryBytes);
        free(buffer.binary);
    }

    out.close();
    return 0;
}

} // namespace

namespace code_gen {

auto test(float sample_freq) -> int {
    auto *module = BinaryenModuleCreate();
    try {
        create_main_function(module, sample_freq);

        if (!BinaryenModuleValidate(module)) {
            BinaryenModulePrint(module);
            throw std::runtime_error("error validating module\n");
        }
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