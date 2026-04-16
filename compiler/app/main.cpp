#include "ast/ast.hpp"
#include "binaryen-c.h"
#include "ir/emit.hpp"
#include "ir/lower.hpp"
#include "parser/parser.hpp"
#include "parser/tokenizer.hpp"
#include "types/type.hpp"
#include "types/type_inference.hpp"
#include <cstdlib>
#include <fstream>
#include <functional>
#include <ios>
#include <iostream>
#include <sstream>
#include <string>
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
    try {
        auto *math_module = BinaryenModuleReadWithFeatures(
            math_bin, math_bin_size, BinaryenFeatureAll());

        if (math_module == nullptr) {
            std::cerr << "Error: failed to parse math module\n";
            return 1;
        }

        if (!BinaryenModuleValidate(math_module)) {
            std::cerr << "Error: math module is invalid\n";
            return 1;
        }

        Tokenizer main_tokenizer(src);
        Parser main_parser(main_tokenizer);
        auto main_result = main_parser.parse_code();
        if (!main_result) {
            std::cerr << "Parser error: " << main_result.error().msg << '\n';
            return 1;
        }

        auto float_type = Type::make<TypeBase>(BaseTypeKind::Float);
        auto float_to_float = Type::make<TypeFun>(float_type, float_type);
        std::vector<std::unordered_map<std::string, TypePtr>> env{
            {
                {"PI", float_type},
                {"TIME", float_type},
                {"SAMPLE_RATE", float_type},
                {"OUT", float_type},
                {"cos", float_to_float},
                {"sin", float_to_float},
                {"sign", float_to_float},
                {"fract", float_to_float},
                {"clip", float_to_float},
                {"exp", float_to_float},
            },
        };
        Substitution subst;
        TypeGenerator gen;
        infer_expr(*main_result, env, subst, gen);

        std::function<TypePtr(const TypePtr &)> monomorphize_fun_type =
            [&](const auto &type) -> TypePtr {
            return std::visit(
                [&](const auto &node) {
                    using T = std::decay_t<decltype(node)>;
                    if constexpr (std::is_same_v<T, TypeVar>)
                        return Type::make<TypeBase>(BaseTypeKind::Float);
                    if constexpr (std::is_same_v<T, TypeFun>)
                        return Type::make<TypeFun>(
                            monomorphize_fun_type(node.param),
                            monomorphize_fun_type(node.result));
                    return type;
                },
                type->node);
        };

        std::function<void(const ExprPtr &)> monomorphize = [&](const auto
                                                                    &expr) {
            std::visit(
                [&](const auto &node) {
                    using T = std::decay_t<decltype(node)>;
                    if constexpr (std::is_same_v<T, Bind>)
                        monomorphize(node.value);
                    if constexpr (std::is_same_v<T, BufferWrite>)
                        monomorphize(node.value);
                    if constexpr (std::is_same_v<T, BufferCtor>)
                        monomorphize(node.init_fn);
                    if constexpr (std::is_same_v<T, Block>) {
                        for (const auto &e : node.expressions)
                            monomorphize(e);
                        expr->type = monomorphize_fun_type(expr->type);
                    }
                    if constexpr (std::is_same_v<T, Call>) {
                        monomorphize(node.callee);
                        monomorphize(node.argument);
                        expr->type = monomorphize_fun_type(expr->type);
                    }
                    if constexpr (std::is_same_v<T, Lambda>) {
                        monomorphize(node.body);
                        expr->type = monomorphize_fun_type(expr->type);
                    }
                    if constexpr (std::is_same_v<T, Variable>) {
                        if (std::holds_alternative<TypeVar>(expr->type->node))
                            expr->type =
                                Type::make<TypeBase>(BaseTypeKind::Float);
                        if (std::holds_alternative<TypeFun>(expr->type->node))
                            expr->type = monomorphize_fun_type(expr->type);
                    }
                },
                expr->node);
        };
        monomorphize(*main_result);

        auto ir = lower(*main_result);

        auto *main_module = BinaryenModuleCreate();
        if (main_module == nullptr) {
            std::cerr << "unable to create binaryen module.";
            return 1;
        }

        emit_ir(ir, main_module, math_module, static_cast<double>(sample_rate));

        if (!BinaryenModuleValidate(main_module)) {
#ifndef NDEBUG
            BinaryenModulePrint(main_module);
#endif
            std::cerr << "invalid module\n";
            return 1;
        }

        BinaryenSetOptimizeLevel(3);
        BinaryenModuleOptimize(main_module);

#ifndef NDEBUG
        BinaryenModulePrint(main_module);
#endif

        return write_module_to_file(main_module);
    } catch (const std::exception &e) {
        std::cerr << "Error: " << e.what() << '\n';
        return 1;
    }
}

auto main(int argc, char **argv) -> int {
    if (argc != 3) {
        std::cout << "Use: wasmwasm <source_path> <math_binary_path>\n";
        return 0;
    }

    std::string src_path(argv[1]);
    std::ifstream src_file(src_path);
    if (!src_file.is_open()) {
        std::cerr << "Failed to open file: " << src_path << '\n';
        return 1;
    }

    std::string math_path(argv[2]);
    std::ifstream math_file(math_path, std::ios::binary | std::ios::ate);
    if (!math_file.is_open()) {
        std::cerr << "Failed to open file: " << math_path << '\n';
        return 1;
    }

    auto math_size = static_cast<size_t>(math_file.tellg());
    math_file.seekg(0, std::ios::beg);

    std::vector<char> math_buffer(math_size);
    if (!math_file.read(math_buffer.data(),
                        static_cast<std::streamsize>(math_size))) {
        std::cerr << "Failed to read file.\n";
        return 1;
    }

    std::ostringstream src_buffer;
    src_buffer << src_file.rdbuf();

    return run_compiler(48'000.0, src_buffer.str().c_str(), math_buffer.data(),
                        math_size);
}
