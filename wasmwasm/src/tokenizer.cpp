#include "tokenizer.hpp"

#include "literal.h"
#include "wasm-builder.h"
#include "wasm-io.h"
#include "wasm-type.h"
#include "wasm.h"

#include <cstdint>
#include <vector>

namespace tokenizer {

using namespace wasm;

auto test(const std::string &src) -> int {
    std::cout << src << '\n';
    Module wasm;
    Builder builder(wasm);

    Name funcName = "main";
    auto type = HeapType{Signature{Type::none, Type::i32}};
    auto *body = builder.makeConst(Literal(int32_t(1)));

    auto func = Builder::makeFunction(funcName, type, {}, body);
    auto exp = Builder::makeExport(funcName, funcName, ExternalKind::Function);

    wasm.addFunction(func.release());
    wasm.addExport(exp.release());
    wasm.updateFunctionsMap();

    ModuleWriter writer({});
    writer.setBinary(true);
    writer.write(wasm, "output.wasm");

    return 0;
}

} // namespace tokenizer