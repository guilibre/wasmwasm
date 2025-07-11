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

namespace {

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

extern "C" auto run_compiler(float sample_freq, const char *src, char *math_bin,
                             size_t math_bin_size) -> int {
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

    BinaryenModuleRef math_module = BinaryenModuleReadWithFeatures(
        math_bin, math_bin_size, BinaryenFeatureAll());

    if (!BinaryenModuleValidate(math_module)) {
        std::cerr << "Error loading math\n";
        return 1;
    }

    BinaryenModuleRef module =
        code_gen::insert_expr(sample_freq, expressions, math_module);

    BinaryenModuleOptimize(module);
    if (!BinaryenModuleValidate(module)) {
        std::cerr << "invalid module";
        return 1;
    }

    return write_module_to_file(module);
}

// for tests only
auto main(int argc, char **argv) -> int {
    if (argc != 3) {
        std::cout << "Use: wasmwasm <source_path> <math_binary_path>\n";
        return 0;
    }

    std::string src_path(argv[1]); // NOLINT
    std::ifstream src_file(src_path);
    if (!src_file.is_open()) {
        std::cerr << "Failed to open file: " << src_path << '\n';
        return 1;
    }

    std::string math_path(argv[2]); // NOLINT
    std::ifstream math_file(math_path, std::ios::binary | std::ios::ate);
    if (!math_file.is_open()) {
        std::cerr << "Failed to open file: " << math_path << '\n';
        return 1;
    }

    std::streamsize size = math_file.tellg();
    math_file.seekg(0, std::ios::beg);

    std::vector<char> math_buffer(size);
    if (!math_file.read(math_buffer.data(), size)) {
        std::cerr << "Failed to read file.\n";
        return 1;
    }

    std::ostringstream src_buffer;
    src_buffer << src_file.rdbuf();

    return run_compiler(1.0F / 44100.0F, src_buffer.str().c_str(),
                        math_buffer.data(), math_buffer.size());
}
