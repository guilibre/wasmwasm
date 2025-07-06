#include "code_gen.hpp"

#include "binaryen-c.h"
#include "wasm.h"

#include <algorithm>
#include <array>
#include <exception>
#include <fstream>
#include <iostream>
#include <map>
#include <stdexcept>
#include <vector>

using namespace wasm;

namespace {

auto do_thing(Module *module, BinaryenIndex out,
              std::map<BinaryenIndex, BinaryenType> variables,
              BinaryenIndex offset) -> BinaryenExpressionRef {
    return BinaryenLocalSet(module, out,
                            BinaryenConst(module, BinaryenLiteralFloat32(0.0)));
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

auto create_main_function(wasm::Module *module) -> void {
    BinaryenAddMemoryImport(module, "memory", "env", "memory", 0);

    std::array<BinaryenType, 3> parameters{};

    parameters[0] = BinaryenTypeInt32();
    auto *base_ptr = BinaryenLocalGet(module, 0, parameters[0]);
    parameters[1] = BinaryenTypeInt32();
    auto *rows = BinaryenLocalGet(module, 1, parameters[1]);
    parameters[2] = BinaryenTypeInt32();
    auto *cols = BinaryenLocalGet(module, 2, parameters[2]);

    std::map<BinaryenIndex, BinaryenType> local_variables;
    BinaryenIndex local_i = local_variables.size() + parameters.size();
    local_variables.emplace(local_i, BinaryenTypeInt32());
    BinaryenIndex local_j = local_variables.size() + parameters.size();
    local_variables.emplace(local_j, BinaryenTypeInt32());

    auto *init_i = BinaryenLocalSet(
        module, local_i, BinaryenConst(module, BinaryenLiteralInt32(0)));
    auto *cond_i = BinaryenBinary(
        module, BinaryenLtUInt32(),
        BinaryenLocalGet(module, local_i, BinaryenTypeInt32()), rows);
    auto *i_inc = BinaryenLocalSet(
        module, local_i,
        BinaryenBinary(module, BinaryenAddInt32(),
                       BinaryenLocalGet(module, local_i, BinaryenTypeInt32()),
                       BinaryenConst(module, BinaryenLiteralInt32(1))));

    auto *init_j = BinaryenLocalSet(
        module, local_j, BinaryenConst(module, BinaryenLiteralInt32(0)));
    auto *cond_j = BinaryenBinary(
        module, BinaryenLtUInt32(),
        BinaryenLocalGet(module, local_j, BinaryenTypeInt32()), cols);
    auto *j_inc = BinaryenLocalSet(
        module, local_j,
        BinaryenBinary(module, BinaryenAddInt32(),
                       BinaryenLocalGet(module, local_j, BinaryenTypeInt32()),
                       BinaryenConst(module, BinaryenLiteralInt32(1))));

    BinaryenIndex out = local_variables.size() + parameters.size();
    local_variables.emplace(out, BinaryenTypeFloat32());

    auto *index_expr = BinaryenBinary(
        module, BinaryenAddInt32(),
        BinaryenLocalGet(module, local_j, BinaryenTypeInt32()),
        BinaryenBinary(module, BinaryenMulInt32(),
                       BinaryenLocalGet(module, local_i, BinaryenTypeInt32()),
                       BinaryenLocalGet(module, 2, BinaryenTypeInt32())));
    auto *address = BinaryenBinary(
        module, BinaryenAddInt32(),
        BinaryenLocalGet(module, 0, BinaryenTypeInt32()),
        BinaryenBinary(module, BinaryenMulInt32(),
                       BinaryenConst(module, BinaryenLiteralInt32(4)),
                       index_expr));
    auto *assign_out =
        BinaryenStore(module, 4, 0, 4, address,
                      BinaryenLocalGet(module, out, BinaryenTypeFloat32()),
                      BinaryenTypeFloat32(), "memory");

    auto *inner_block = BinaryenBlock(
        module, "inner_block",
        std::array<BinaryenExpressionRef, 4>(
            {do_thing(module, out, local_variables, parameters.size()),
             assign_out, j_inc,
             BinaryenBreak(module, "inner_loop", cond_j, nullptr)})
            .data(),
        4, BinaryenTypeNone());
    auto *inner_loop = BinaryenLoop(module, "inner_loop", inner_block);

    auto *outer_block = BinaryenBlock(
        module, "outer_block",
        std::array<BinaryenExpressionRef, 4>(
            {init_j, inner_loop, i_inc,
             BinaryenBreak(module, "outer_loop", cond_i, nullptr)})
            .data(),
        4, BinaryenTypeNone());
    auto *outer_loop = BinaryenLoop(module, "outer_loop", outer_block);

    BinaryenExpressionRef body =
        BinaryenBlock(module, "body",
                      std::array<BinaryenExpressionRef, 2>(
                          {init_i, BinaryenBlock(module, "loop", &outer_loop, 1,
                                                 BinaryenTypeNone())})
                          .data(),
                      2, BinaryenTypeNone());

    BinaryenAddFunction(module, "main",
                        BinaryenTypeCreate(parameters.data(), 3),
                        BinaryenTypeNone(), map_values(local_variables).data(),
                        local_variables.size(), body);
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

auto test() -> int {
    auto *module = BinaryenModuleCreate();
    try {
        create_main_function(module);

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