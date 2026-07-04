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
#include <json/json.h>
#include <memory>
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
    std::unordered_map<std::string, const Json::Value *> node_map;

    const Json::Value *nodes_ptr = nullptr;
    const Json::Value *edges_ptr = nullptr;

    if (root.isMember("orchestra")) {
        const auto &instr = root["orchestra"]["instruments"][0];
        nodes_ptr = &instr["nodes"];
        edges_ptr = &instr["edges"];
    } else {
        nodes_ptr = &root["nodes"];
        edges_ptr = &root["edges"];
    }

    const auto &nodes = *nodes_ptr;
    const auto &edges = *edges_ptr;

    Json::Value modules{Json::objectValue};
    Json::Value patch{Json::objectValue};

    for (const auto &node : nodes) {
        node_map[node["id"].asString()] = &node;
        if (node["type"].asString() == "block") {
            modules[node["id"].asString()] = node["data"]["code"].asString();
        }
    }

    for (const auto &edge : edges) {
        if (!edge.isMember("sourceHandle") || !edge.isMember("targetHandle"))
            continue;

        const auto src_id = edge["source"].asString();
        const auto tgt_id = edge["target"].asString();

        auto it_src = node_map.find(src_id);
        auto it_tgt = node_map.find(tgt_id);
        if (it_src == node_map.end() || it_tgt == node_map.end()) continue;

        const auto &src_node = *it_src->second;
        const auto &tgt_node = *it_tgt->second;

        const std::string src_key =
            src_node["type"].asString() == "capture"
                ? edge["sourceHandle"].asString()
                : src_id + "_" + edge["sourceHandle"].asString();

        const auto tgt_type = tgt_node["type"].asString();
        const std::string sink_key =
            tgt_type == "out" || tgt_type == "dac"
                ? edge["targetHandle"].asString()
                : tgt_id + "_" + edge["targetHandle"].asString();

        patch[sink_key] = src_key;
    }

    Json::Value out{Json::objectValue};
    out["modules"] = modules;
    out["patch"] = patch;

    Json::StreamWriterBuilder wb;
    wb["indentation"] = "";
    return Json::writeString(wb, out);
}

auto run(const char *math_wasm_path) -> int {
    auto math_bin = load_file(math_wasm_path);
    BinaryenBackend backend(math_bin.data(), math_bin.size());
    std::cout << "math.wasm OK\n";

    const auto src = []() -> std::string {
        std::string line;
        std::string s;
        while (std::getline(std::cin, line)) s += line + "\n";
        return s;
    }();

    Json::Value root;
    Json::Reader reader;
    if (!reader.parse(src, root))
        throw std::runtime_error("invalid JSON: " +
                                 reader.getFormattedErrorMessages());

    const auto patch_json = frontend_to_patch_json(root);
    std::cout << "patch JSON: " << patch_json << "\n";

    auto patch = parse_patch_json(patch_json);
    std::cout << "parse OK\n";

    const BackendOptions opts{.sample_rate = 44100.0};
    auto codegen = backend.create_codegen(opts);
    auto *codegen_impl = dynamic_cast<BinaryenCodeGen *>(codegen.get());
    if (codegen_impl == nullptr)
        throw std::runtime_error("expected a BinaryenCodeGen");

    std::vector<IRModule> compiled;
    for (const auto &[name, code] : patch.module_sources) {
        const Tokenizer tok(code);
        Parser parser(tok);
        auto ast = parser.parse_code();
        if (!ast)
            throw std::runtime_error("[" + name +
                                     "] parse error: " + ast.error().msg);

        auto env = make_builtin_env();
        Substitution subst;
        TypeGenerator gen;
        pre_register_toplevel(*ast, env);
        infer_expr(*ast, env, subst, gen);
        finalize_types(*ast, subst);

        auto ir = lower(*ast, name);
        std::cout << "[" << name << "] lowered OK\n";
        codegen->add_module(ir);
        compiled.push_back(std::move(ir));
    }
    std::cout << "emit OK\n";

    auto graph = build_routing_graph(patch, std::move(compiled));
    codegen->finalize(graph);
    std::cout << "routing OK\n";

    auto *main_module = codegen_impl->raw_module();

    if (!BinaryenModuleValidate(main_module)) {
        std::cout << "INVALID MODULE\n";
        BinaryenModulePrint(main_module);
        return 1;
    }
    std::cout << "validate OK\n";

    std::cout << "\n=== WAT (pre-optimization) ===\n";
    BinaryenModulePrint(main_module);

    const auto artifact = codegen->build();
    (void)artifact;

    std::cout << "\n=== WAT (post-optimization) ===\n";
    BinaryenModulePrint(main_module);

    return 0;
}

} // namespace

auto main(int argc, char **argv) -> int {
    const char *math_path = argc > 1 ? argv[1] : "/math.wasm";
    try {
        return run(math_path);
    } catch (const std::exception &e) {
        std::cout << "error: " << e.what() << "\n";
        return 1;
    } catch (...) {
        std::cout << "unknown error\n";
        return 1;
    }
}
