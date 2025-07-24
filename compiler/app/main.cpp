#include "ast/ast.hpp"
#include "binaryen-c.h"
#include "code_gen/main_module_builder.hpp"
#include "parser/parser.hpp"
#include "parser/tokenizer.hpp"
#include "types/type.hpp"
#include "types/type_inference.hpp"

#include <fstream>
#include <functional>
#include <ios>
#include <iostream>
#include <sstream>
#include <type_traits>
#include <variant>
#include <vector>

namespace {

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

extern "C" auto run_compiler(float sample_rate, const char *src, char *math_bin,
                             size_t math_bin_size) -> int {

    auto *math_module = BinaryenModuleReadWithFeatures(math_bin, math_bin_size,
                                                       BinaryenFeatureAll());

    if (!BinaryenModuleValidate(math_module)) {
        std::cerr << "Error loading math\n";
        return 1;
    }

    Tokenizer tokenizer(src);
    Parser parser(tokenizer);
    auto parse_result = parser.parse();
    if (!parse_result) {
        std::cerr << "Parser error: " << parse_result.error() << '\n';
        return 1;
    }

    auto float_type = Type::make<TypeBase>(BaseTypeKind::Float);
    auto float_to_float = Type::make<TypeFun>(float_type, float_type);
    std::unordered_map<std::string, TypePtr> env{
        {"PI", float_type},
        {"TIME", float_type},
        {"sin", float_to_float},
        {"+", Type::make<TypeFun>(float_type, float_to_float)},
        {"-", Type::make<TypeFun>(float_type, float_to_float)},
        {"*", Type::make<TypeFun>(float_type, float_to_float)},
        {"/", Type::make<TypeFun>(float_type, float_to_float)},
    };
    Substitution subst;
    TypeGenerator gen;
    infer_expr(*parse_result, env, subst, gen);

    std::function<TypePtr(const TypePtr &)> monomorphize_fun_type =
        [&](const auto &type) -> TypePtr {
        return std::visit(
            [&](const auto &node) {
                using T = std::decay_t<decltype(node)>;
                if constexpr (std::is_same_v<T, TypeVar>) {
                    return Type::make<TypeBase>(BaseTypeKind::Float);
                }

                if constexpr (std::is_same_v<T, TypeFun>) {
                    return Type::make<TypeFun>(
                        monomorphize_fun_type(node.param),
                        monomorphize_fun_type(node.result));
                }
                return type;
            },
            type->node);
    };

    std::function<void(const ExprPtr &)> monomorphize = [&](const auto &expr) {
        std::visit(
            [&](const auto &node) {
                using T = std::decay_t<decltype(node)>;
                if constexpr (std::is_same_v<T, Expr::Assignment>)
                    monomorphize(node.value);

                if constexpr (std::is_same_v<T, Expr::Block>) {
                    for (const auto &expr : node.expressions) {
                        monomorphize(expr);
                    }
                    expr->type = monomorphize_fun_type(expr->type);
                }

                if constexpr (std::is_same_v<T, Expr::Call>) {
                    monomorphize(node.callee);
                    monomorphize(node.argument);
                    expr->type = monomorphize_fun_type(expr->type);
                }
                if constexpr (std::is_same_v<T, Expr::Lambda>) {
                    monomorphize(node.body);
                    expr->type = monomorphize_fun_type(expr->type);
                }

                if constexpr (std::is_same_v<T, Expr::Variable>) {
                    if (std::holds_alternative<TypeVar>(expr->type->node))
                        expr->type = Type::make<TypeBase>(BaseTypeKind::Float);

                    if (std::holds_alternative<TypeFun>(expr->type->node)) {
                        expr->type = monomorphize_fun_type(expr->type);
                    }
                }
            },
            expr->node);
    };
    monomorphize(*parse_result);

    ASTPrinter ast_printer;
    // ast_printer(*parse_result);

    MainModuleBuilder main_module_builder(math_module, sample_rate);

    auto *main_module = main_module_builder.build(*parse_result);

    // BinaryenModulePrint(main_module);
    if (!BinaryenModuleValidate(main_module)) {
        std::cerr << "invalid module";
        return 1;
    }
    BinaryenSetOptimizeLevel(3);
    BinaryenModuleOptimize(main_module);
    BinaryenModulePrint(main_module);

    return write_module_to_file(main_module);
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

    return run_compiler(44100.0, src_buffer.str().c_str(), math_buffer.data(),
                        math_buffer.size());
}
