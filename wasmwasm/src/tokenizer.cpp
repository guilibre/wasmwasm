#include "tokenizer.hpp"

#include "binaryen-c.h"
#include <array>
#include <cstddef>
#include <fstream>
#include <iostream>

namespace tokenizer {

using namespace wasm;

auto test() -> int {
    auto *module = BinaryenModuleCreate();

    const BinaryenType paramTypes = {BinaryenTypeFloat32()};
    const auto resultType = BinaryenTypeFloat32();

    BinaryenAddFunctionImport(module, "Math_sin", "Math", "sin", paramTypes,
                              resultType);

    auto *t = BinaryenLocalGet(module, 0, BinaryenTypeFloat32());
    auto *call_sin = BinaryenCall(module, "Math_sin", &t, 1, resultType);

    BinaryenAddFunction(module, "main", paramTypes, resultType, nullptr, 0,
                        call_sin);

    BinaryenAddFunctionExport(module, "main", "main");

    BinaryenModuleValidate(module);
    // BinaryenModulePrint(module);

    std::array<char, 1024> code{};
    size_t len = BinaryenModuleWrite(module, code.data(), 1024);
    std::cout << "---------------------------------------------\n";
    std::cout.write(code.data(), len);
    std::cout << "---------------------------------------------\n";
    std::ofstream out("/tmp/output.wasm", std::ios::binary);
    if (!out) {
        std::cerr << "failed to open file for writing.\n";
        return 1;
    }
    out.write(code.data(), len);
    out.close();

    BinaryenModuleDispose(module);
    return 0;
}

} // namespace tokenizer