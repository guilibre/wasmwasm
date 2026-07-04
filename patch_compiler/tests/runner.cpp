#include "backend/binaryen/binaryen_backend.hpp"
#include "backend/binaryen/binaryen_codegen.hpp"
#include "binaryen-c.h"
#include "builtins.hpp"
#include "ir/lower.hpp"
#include "parser/parser.hpp"
#include "parser/tokenizer.hpp"
#include "routing/routing.hpp"
#include "types/type_inference.hpp"
#include <fstream>
#include <iostream>
#include <memory>
#include <sstream>
#include <stdexcept>
#include <string>
#include <vector>

namespace {

struct StdinFixture {
    std::string ww_source;
    std::string expect_text;
};

auto read_fixture_from_stdin() -> StdinFixture {
    std::string len_line;
    if (!std::getline(std::cin, len_line))
        throw std::runtime_error("stdin: missing ww-source length header");

    size_t ww_len = 0;
    try {
        ww_len = std::stoul(len_line);
    } catch (const std::exception &) {
        throw std::runtime_error("stdin: invalid ww-source length header: " +
                                 len_line);
    }

    std::string ww_source(ww_len, '\0');
    std::cin.read(ww_source.data(), static_cast<std::streamsize>(ww_len));
    if (static_cast<size_t>(std::cin.gcount()) != ww_len)
        throw std::runtime_error("stdin: truncated ww source");

    std::ostringstream rest;
    rest << std::cin.rdbuf();
    return {.ww_source = std::move(ww_source), .expect_text = rest.str()};
}

auto load_binary_file(const std::string &path) -> std::vector<char> {
    std::ifstream f(path, std::ios::binary | std::ios::ate);
    if (!f) throw std::runtime_error("cannot open: " + path);
    const auto size = static_cast<long>(f.tellg());
    f.seekg(0);
    std::vector<char> buf(static_cast<size_t>(size));
    f.read(buf.data(), size);
    return buf;
}

struct Assertion {
    bool expect_present;
    std::string needle;
    size_t line_no;
};

auto parse_expectations(const std::string &text) -> std::vector<Assertion> {
    std::vector<Assertion> assertions;
    std::istringstream ss(text);
    std::string line;
    size_t line_no = 0;
    while (std::getline(ss, line)) {
        ++line_no;
        if (!line.empty() && line.back() == '\r') line.pop_back();

        const auto first_non_space = line.find_first_not_of(" \t");
        if (first_non_space == std::string::npos) continue;
        if (line[first_non_space] == '#') continue;

        const auto colon = line.find(':');
        if (colon == std::string::npos)
            throw std::runtime_error("malformed expectation line " +
                                     std::to_string(line_no) + ": " + line);

        const auto directive = line.substr(0, colon);
        auto needle = line.substr(colon + 1);
        const auto needle_start = needle.find_first_not_of(" \t");
        needle = needle_start == std::string::npos
                     ? ""
                     : needle.substr(needle_start);

        if (directive == "CONTAINS")
            assertions.push_back({
                .expect_present = true,
                .needle = needle,
                .line_no = line_no,
            });
        else if (directive == "NOT_CONTAINS")
            assertions.push_back({
                .expect_present = false,
                .needle = needle,
                .line_no = line_no,
            });
        else
            throw std::runtime_error("unknown directive at line " +
                                     std::to_string(line_no) + ": " +
                                     directive);
    }
    return assertions;
}

auto compile_module_to_wat(const std::string &source,
                           const std::string &math_wasm_path) -> std::string {
    auto math_bin = load_binary_file(math_wasm_path);
    BinaryenBackend backend(math_bin.data(), math_bin.size());

    const BackendOptions opts{.sample_rate = 44100.0};
    auto codegen = backend.create_codegen(opts);
    auto *codegen_impl = dynamic_cast<BinaryenCodeGen *>(codegen.get());
    if (codegen_impl == nullptr)
        throw std::runtime_error("expected a BinaryenCodeGen");

    const Tokenizer tok(source);
    Parser parser(tok);
    auto ast = parser.parse_code();
    if (!ast) throw std::runtime_error("parse error: " + ast.error().msg);

    auto env = make_builtin_env();
    Substitution subst;
    TypeGenerator gen;
    pre_register_toplevel(*ast, env);
    infer_expr(*ast, env, subst, gen);
    finalize_types(*ast, subst);

    auto ir = lower(*ast, "test_module");
    codegen->add_module(ir);

    ParsedPatch patch;
    patch.module_sources.emplace_back("test_module", source);

    std::vector<IRModule> compiled;
    compiled.push_back(std::move(ir));
    auto graph = build_routing_graph(patch, std::move(compiled));
    codegen->finalize(graph);

    auto *main_module = codegen_impl->raw_module();
    if (!BinaryenModuleValidate(main_module))
        throw std::runtime_error("invalid module");

    BinaryenSetOptimizeLevel(3);
    BinaryenSetShrinkLevel(0);
    BinaryenSetFastMath(true);
    BinaryenSetLowMemoryUnused(true);
    BinaryenSetAlwaysInlineMaxSize(100);
    BinaryenSetFlexibleInlineMaxSize(250);
    BinaryenSetOneCallerInlineMaxSize(250);
    BinaryenModuleOptimize(main_module);

    if (!BinaryenModuleValidate(main_module))
        throw std::runtime_error("invalid module");

    std::unique_ptr<char, decltype(&free)> wat{
        BinaryenModuleAllocateAndWriteText(main_module), free};
    return wat.get();
}

} // namespace

auto main(int argc, char **argv) -> int {
    const std::string math_wasm_path = argc > 1 ? argv[1] : "/math.wasm";

    try {
        const auto fixture = read_fixture_from_stdin();
        const auto assertions = parse_expectations(fixture.expect_text);

        const auto wat =
            compile_module_to_wat(fixture.ww_source, math_wasm_path);

        bool ok = true;
        for (const auto &a : assertions)
            if (wat.contains(a.needle) != a.expect_present) ok = false;

        std::cerr << wat << "\n";

        if (!ok) return 1;

        std::cout << "OK\n";
        return 0;
    } catch (const std::exception &e) {
        std::cerr << "error: " << e.what() << "\n";
        return 1;
    }
}
