#include "ast/ast.hpp"
#include "binaryen-c.h"
#include "code_gen/code_gen_context.hpp"
#include "code_gen/expression_emitter.hpp"
#include "code_gen/init_buffers_builder.hpp"
#include "code_gen/main_module_builder.hpp"
#include "parser/parser.hpp"
#include "parser/tokenizer.hpp"
#include "types/type.hpp"
#include "types/type_inference.hpp"
#include "wasm.h"

#include <fstream>
#include <functional>
#include <ios>
#include <iostream>
#include <memory>
#include <numbers>
#include <sstream>
#include <stdexcept>
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

    Tokenizer main_tokenizer(src);
    Parser main_parser(main_tokenizer);
    auto main_result = main_parser.parse_code();
    if (!main_result) {
        std::cerr << "Parser error: " << main_result.error() << '\n';
        return 1;
    }

    Tokenizer init_tokenizer(src);
    Parser init_parser(init_tokenizer);
    auto init_result = init_parser.parse_initialization();
    if (!init_result) {
        std::cerr << "Initialization error error: " << init_result.error()
                  << '\n';
        return 1;
    }

    // ASTPrinter ast_printer;
    // ast_printer(*init_result);
    // ast_printer(*main_result);
    // return 0;

    auto int_type = Type::make<TypeBase>(BaseTypeKind::Int);
    auto float_type = Type::make<TypeBase>(BaseTypeKind::Float);
    auto void_type = Type::make<TypeBase>(BaseTypeKind::Void);
    auto int_to_float = Type::make<TypeFun>(int_type, float_type);
    auto float_to_float = Type::make<TypeFun>(float_type, float_type);
    std::vector<std::unordered_map<std::string, TypePtr>> env{
        {
            {"PI", float_type},
            {"TIME", float_type},
            {"OUT", float_type},
            {"sin", float_to_float},
            {"sign", float_to_float},
            {"fract", float_to_float},
            {"clip", float_to_float},
            {"buffer",
             Type::make<TypeFun>(
                 float_type,
                 Type::make<TypeFun>(
                     int_type, Type::make<TypeFun>(int_to_float, void_type)))},
        },
    };
    Substitution subst;
    TypeGenerator gen;
    infer_expr(*init_result, env, subst, gen);
    infer_expr(*main_result, env, subst, gen);

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
                if constexpr (std::is_same_v<T, Assignment>)
                    monomorphize(node.value);

                if constexpr (std::is_same_v<T, Block>) {
                    for (const auto &expr : node.expressions) {
                        monomorphize(expr);
                    }
                    expr->type = monomorphize_fun_type(expr->type);
                }

                if constexpr (std::is_same_v<T, Buffer>)
                    monomorphize(node.init_buffer_function);

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
                        expr->type = Type::make<TypeBase>(BaseTypeKind::Float);

                    if (std::holds_alternative<TypeFun>(expr->type->node)) {
                        expr->type = monomorphize_fun_type(expr->type);
                    }
                }
            },
            expr->node);
    };
    monomorphize(*init_result);
    monomorphize(*main_result);

    // ASTPrinter ast_printer;
    // ast_printer(*init_result);
    // ast_printer(*main_result);
    // return 0;

    auto *main_module = BinaryenModuleCreate();
    if (main_module == nullptr) {
        std::cerr << "unable to create binaryen module.";
        return 1;
    }
    auto ctx = std::make_shared<CodeGenContext>(main_module);

    ctx->add_constant("PI", BinaryenLiteralFloat64(std::numbers::pi));

    ctx->push_context();
    auto &arg = ctx->add_parameter("phase", BinaryenTypeFloat64());
    ctx->add_env();
    ctx->add_function(
        "sin",
        BinaryenCall(ctx->module(), "wasmwasm_sin",
                     std::array{arg.get_local(ctx->module())}.data(), 1,
                     BinaryenTypeFloat64()),
        BinaryenTypeFloat64(), 0);
    ctx->pop_context();
    ctx->push_context();
    arg = ctx->add_parameter("arg", BinaryenTypeFloat64());
    ctx->add_env();
    ctx->add_function(
        "sign",
        BinaryenCall(ctx->module(), "wasmwasm_sign",
                     std::array{arg.get_local(ctx->module())}.data(), 1,
                     BinaryenTypeFloat64()),
        BinaryenTypeFloat64(), 0);
    ctx->pop_context();
    ctx->push_context();
    arg = ctx->add_parameter("arg", BinaryenTypeFloat64());
    ctx->add_env();
    ctx->add_function(
        "fract",
        BinaryenCall(ctx->module(), "wasmwasm_fract",
                     std::array{arg.get_local(ctx->module())}.data(), 1,
                     BinaryenTypeFloat64()),
        BinaryenTypeFloat64(), 0);
    ctx->pop_context();
    ctx->push_context();
    arg = ctx->add_parameter("arg", BinaryenTypeFloat64());
    ctx->add_env();
    ctx->add_function(
        "clip",
        BinaryenCall(ctx->module(), "wasmwasm_clip",
                     std::array{arg.get_local(ctx->module())}.data(), 1,
                     BinaryenTypeFloat64()),
        BinaryenTypeFloat64(), 0);
    ctx->pop_context();

    auto expression_emitter =
        std::make_shared<ExpressionEmitter>(ctx, sample_rate);
    InitBuffersBuilder init_buffers_builder(ctx, expression_emitter);
    MainModuleBuilder main_module_builder(ctx, expression_emitter, math_module,
                                          sample_rate);

    try {
        init_buffers_builder.build(*init_result);
        main_module_builder.build(*main_result);
    } catch (std::runtime_error &ex) {
        std::cerr << ex.what();
        return 1;
    }

    // BinaryenModulePrint(main_module);
    if (!BinaryenModuleValidate(main_module)) {
        std::cerr << "invalid module";
        return 1;
    }
    BinaryenSetOptimizeLevel(3);
    BinaryenModuleOptimize(main_module);
    // BinaryenModulePrint(main_module);

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

    auto size = math_file.tellg();
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
