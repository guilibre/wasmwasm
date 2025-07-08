#include "code_gen.hpp"
#include "parser.hpp"
#include "tokenizer.hpp"

#include <iostream>
#include <string_view>

extern "C" auto run_compiler(float sample_freq, const char *src) -> int {
    Tokenizer tokenizer(src);
    Parser parser(tokenizer);
    auto expr = parser.parse_expr();
    return code_gen::insert_expr(sample_freq, expr);
}

auto main(int argc, char **argv) -> int {
    if (argc != 2) {
        std::cout << "Use: wasmwasm <source>\n";
        return 0;
    }

    try {
        run_compiler(1.0F / 44100.0F, argv[1]);
    } catch (const std::exception &e) {
        std::cerr << e.what() << "\n";
    }

    return 0;
}
