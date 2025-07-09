#include "ast.hpp"
#include "code_gen.hpp"
#include "parser.hpp"
#include "tokenizer.hpp"

#include "src/binaryen-c.h"

#include <algorithm>
#include <fstream>
#include <ios>
#include <iostream>
#include <iterator>
#include <sstream>
#include <stdexcept>
#include <string_view>
#include <vector>

extern "C" auto run_compiler(float sample_freq, const char *src,
                             char *functions_bin, size_t functions_bin_size)
    -> int {
    Tokenizer tokenizer(src);
    Parser parser(tokenizer);
    auto expression_results = parser.parse_assignments();
    std::vector<ExprPtr> expressions;
    expressions.reserve(expression_results.size());
    std::ranges::transform(expression_results, std::back_inserter(expressions),
                           [](auto &result) -> ExprPtr {
                               if (!result)
                                   throw std::runtime_error(
                                       "Invalid expression");
                               return std::move(*result);
                           });

    auto *functions_module = BinaryenModuleReadWithFeatures(
        functions_bin, functions_bin_size, BinaryenFeatureAll());

    BinaryenModuleOptimize(functions_module);

    if (!BinaryenModuleValidate(functions_module)) {
        std::cerr << "Error loading functions\n";
        return 1;
    }

    return code_gen::insert_expr(sample_freq, expressions, functions_module);
}

// for tests only
auto main(int argc, char **argv) -> int {
    if (argc != 3) {
        std::cout << "Use: wasmwasm <source_path> <functions_binary_path>\n";
        return 0;
    }

    std::string src_path(argv[1]); // NOLINT
    std::ifstream src_file(src_path);
    if (!src_file.is_open()) {
        std::cerr << "Failed to open file: " << src_path << '\n';
        return 1;
    }

    std::string functions_path(argv[2]); // NOLINT
    std::ifstream functions_file(functions_path,
                                 std::ios::binary | std::ios::ate);
    if (!functions_file.is_open()) {
        std::cerr << "Failed to open file: " << functions_path << '\n';
        return 1;
    }

    std::streamsize size = functions_file.tellg();
    functions_file.seekg(0, std::ios::beg);

    std::vector<char> functions_buffer(size);
    if (!functions_file.read(functions_buffer.data(), size)) {
        std::cerr << "Failed to read file.\n";
        return 1;
    }

    std::ostringstream src_buffer;
    src_buffer << src_file.rdbuf();

    return run_compiler(1.0F / 44100.0F, src_buffer.str().c_str(),
                        functions_buffer.data(), functions_buffer.size());
}
