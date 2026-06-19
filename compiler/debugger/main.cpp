#include "binaryen-c.h"
#include "builtins.hpp"
#include "ir/emit.hpp"
#include "ir/lower.hpp"
#include "parser/parser.hpp"
#include "parser/tokenizer.hpp"
#include "types/type_inference.hpp"
#include <iostream>
#include <string>

auto main() -> int {
    std::string line;
    std::string src;
    while (std::getline(std::cin, line)) src += line + "\n";

    const Tokenizer tok(src);
    Parser parser(tok);
    auto result = parser.parse_code();
    if (!result) {
        std::cerr << "parse error: " << result.error().msg << "\n";
        return 1;
    }

    try {
        auto env = make_builtin_env();
        Substitution subst;
        TypeGenerator gen;
        infer_expr(*result, env, subst, gen);
    } catch (const std::exception &e) {
        std::cerr << "type error: " << e.what() << "\n";
        return 1;
    }

    IRModule ir;
    try {
        ir = lower(*result);
        std::cout << "lower OK\n";
    } catch (const std::exception &e) {
        std::cerr << "lower error: " << e.what() << "\n";
        return 1;
    }

    auto *math_mod = BinaryenModuleCreate();
    auto *main_mod = BinaryenModuleCreate();
    try {
        emit_ir(ir, main_mod, math_mod, 44100.0);
        std::cout << "emit OK\n";
    } catch (const std::exception &e) {
        std::cerr << "emit error: " << e.what() << "\n";
        BinaryenModuleDispose(math_mod);
        BinaryenModuleDispose(main_mod);
        return 1;
    }

    if (!BinaryenModuleValidate(main_mod)) {
        std::cerr << "INVALID MODULE\n";
        BinaryenModulePrint(main_mod);
        BinaryenModuleDispose(math_mod);
        BinaryenModuleDispose(main_mod);
        return 1;
    }
    std::cout << "validate OK\n";

    try {
        BinaryenModuleOptimize(main_mod);
    } catch (const std::exception &e) {
        std::cerr << "emit error: " << e.what() << "\n";
        BinaryenModuleDispose(math_mod);
        BinaryenModuleDispose(main_mod);
        return 1;
    }

    if (!BinaryenModuleValidate(main_mod)) {
        std::cerr << "OPTIMIZATION FAILED\n";
        BinaryenModulePrint(main_mod);
        BinaryenModuleDispose(math_mod);
        BinaryenModuleDispose(main_mod);
        return 1;
    }
    std::cout << "optimization OK\n";

    BinaryenModuleDispose(math_mod);
    BinaryenModuleDispose(main_mod);
    return 0;
}
