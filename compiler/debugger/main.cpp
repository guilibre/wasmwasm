#include "binaryen-c.h"
#include "builtins.hpp"
#include "ir/emit.hpp"
#include "ir/lower.hpp"
#include "parser/parser.hpp"
#include "parser/tokenizer.hpp"
#include "routing/routing.hpp"
#include "types/type_inference.hpp"
#include <fstream>
#include <iostream>
#include <json/json.h>
#include <stdexcept>
#include <string>
#include <vector>

namespace {

auto load_file(const char *path) -> std::vector<char> {
    std::ifstream f(path, std::ios::binary | std::ios::ate);
    if (!f) throw std::runtime_error(std::string("cannot open: ") + path);
    const auto size = static_cast<long>(f.tellg());
    f.seekg(0);
    std::vector<char> buf(static_cast<size_t>(size));
    f.read(buf.data(), size);
    return buf;
}

auto frontend_to_patch_json(const Json::Value &root) -> std::string {
    const auto &nodes = root["nodes"];
    const auto &edges = root["edges"];

    Json::Value modules{Json::objectValue};
    Json::Value patch{Json::objectValue};

    std::unordered_map<std::string, const Json::Value *> node_map;
    for (const auto &node : nodes) {
        node_map[node["id"].asString()] = &node;
        if (node["type"].asString() == "block") {
            const auto &data = node["data"];
            modules[data["name"].asString()] = data["code"].asString();
        }
    }

    for (const auto &edge : edges) {
        if (!edge.isMember("sourceHandle") || !edge.isMember("targetHandle"))
            continue;

        const auto src_id = edge["source"].asString();
        const auto tgt_id = edge["target"].asString();
        const auto src_handle = edge["sourceHandle"].asString();
        const auto tgt_handle = edge["targetHandle"].asString();

        auto it_src = node_map.find(src_id);
        auto it_tgt = node_map.find(tgt_id);
        if (it_src == node_map.end() || it_tgt == node_map.end()) continue;

        const auto &src_node = *it_src->second;
        const auto &tgt_node = *it_tgt->second;

        const std::string src_key =
            src_node["type"].asString() == "capture"
                ? src_handle
                : src_node["data"]["name"].asString() + "_" + src_handle;

        const std::string sink_key =
            tgt_node["type"].asString() == "dac"
                ? tgt_handle
                : tgt_node["data"]["name"].asString() + "_" + tgt_handle;

        patch[sink_key] = src_key;
    }

    Json::Value out{Json::objectValue};
    out["modules"] = modules;
    out["patch"] = patch;

    Json::StreamWriterBuilder wb;
    wb["indentation"] = "";
    return Json::writeString(wb, out);
}

auto compile_single_module(const std::string &src, BinaryenModuleRef math_mod,
                           BinaryenModuleRef main_mod) -> IRModule {
    const Tokenizer tok(src);
    Parser parser(tok);
    auto result = parser.parse_code();
    if (!result) throw std::runtime_error("parse error: " + result.error().msg);

    auto env = make_builtin_env();
    Substitution subst;
    TypeGenerator gen;
    infer_expr(*result, env, subst, gen);

    auto ir = lower(*result, "debug");
    emit_ir(ir, main_mod, math_mod, 44100.0);
    return ir;
}

auto run(const char *math_wasm_path) -> int {
    std::string line;
    std::string src;
    while (std::getline(std::cin, line)) src += line + "\n";

    auto math_bin = load_file(math_wasm_path);
    auto *math_module = BinaryenModuleReadWithFeatures(
        math_bin.data(), math_bin.size(), BinaryenFeatureAll());
    if (math_module == nullptr) {
        std::cout << "failed to load math.wasm\n";
        return 1;
    }
    if (!BinaryenModuleValidate(math_module)) {
        std::cout << "math.wasm is invalid\n";
        BinaryenModuleDispose(math_module);
        return 1;
    }
    std::cout << "math.wasm OK\n";

    auto *main_module = BinaryenModuleCreate();

    try {
        const auto trimmed = src.find_first_not_of(" \t\r\n");
        const bool is_json =
            trimmed != std::string::npos && src[trimmed] == '{';

        if (is_json) {
            Json::Value root;
            Json::Reader reader;
            if (!reader.parse(src, root))
                throw std::runtime_error("invalid JSON: " +
                                         reader.getFormattedErrorMessages());

            std::string patch_json;
            if (root.isMember("nodes")) {
                patch_json = frontend_to_patch_json(root);
                std::cout << "converted frontend JSON to patch JSON\n";
            } else {
                Json::StreamWriterBuilder wb;
                wb["indentation"] = "";
                patch_json = Json::writeString(wb, root);
            }

            auto patch = parse_patch_json(patch_json);
            std::cout << "parse OK\n";

            std::vector<IRModule> compiled;
            uint32_t next_mem = buffer_memory_start;
            for (const auto &[name, code] : patch.module_sources) {
                const Tokenizer tok(code);
                Parser parser(tok);
                auto ast = parser.parse_code();
                if (!ast)
                    throw std::runtime_error(
                        "[" + name + "] parse error: " + ast.error().msg);

                auto env = make_builtin_env();
                Substitution subst;
                TypeGenerator gen;
                infer_expr(*ast, env, subst, gen);

                auto ir = lower(*ast, name);
                ir.memory_base = next_mem;
                emit_ir(ir, main_module, math_module, 44100.0);
                next_mem += ir.total_buffer_bytes();
                compiled.push_back(std::move(ir));
            }
            std::cout << "lower OK\n";
            std::cout << "emit OK\n";

            auto graph = build_routing_graph(patch, std::move(compiled));
            emit_main_loop(graph, main_module);
            std::cout << "routing OK\n";
        } else {
            auto ir = compile_single_module(src, math_module, main_module);
            std::cout << "lower OK\n";
            std::cout << "emit OK\n";
            (void)ir;
        }
    } catch (const std::exception &e) {
        std::cout << "compile error: " << e.what() << "\n";
        BinaryenModuleDispose(math_module);
        BinaryenModuleDispose(main_module);
        return 1;
    }
    std::cout << "compilation OK\n";

    if (!BinaryenModuleValidate(main_module)) {
        std::cout << "INVALID MODULE\n";
        BinaryenModuleDispose(math_module);
        BinaryenModuleDispose(main_module);
        return 1;
    }
    std::cout << "validate OK\n";

    BinaryenSetOptimizeLevel(3);
    BinaryenSetShrinkLevel(0);
    BinaryenSetFastMath(true);
    BinaryenSetLowMemoryUnused(true);
    BinaryenModuleOptimize(main_module);

    if (!BinaryenModuleValidate(main_module)) {
        std::cout << "OPTIMIZATION FAILED\n";
        BinaryenModuleDispose(math_module);
        BinaryenModuleDispose(main_module);
        return 1;
    }
    std::cout << "optimization OK\n";

    BinaryenModuleDispose(math_module);
    BinaryenModuleDispose(main_module);
    return 0;
}

} // namespace

auto main(int argc, char **argv) -> int {
    const char *math_path = argc > 1 ? argv[1] : "/math.wasm";
    try {
        return run(math_path);
    } catch (const std::exception &e) {
        std::cout << "unexpected error: " << e.what() << "\n";
        return 1;
    } catch (...) {
        std::cout << "unknown error\n";
        return 1;
    }
}
