#include "routing.hpp"

#include <algorithm>
#include <functional>
#include <json/json.h>
#include <stdexcept>
#include <unordered_map>
#include <unordered_set>

namespace {

void validate_patch(const Json::Value &root) {
    if (!root.isObject())
        throw std::runtime_error("patch JSON must be an object");

    if (!root.isMember("patch") || !root["patch"].isObject())
        throw std::runtime_error(
            "patch JSON must have an object field 'patch'");

    const auto &mods = root.isMember("modules")
                           ? root["modules"]
                           : Json::Value{Json::objectValue};
    if (!mods.isObject())
        throw std::runtime_error("'modules' must be an object");

    std::unordered_set<std::string> mod_names;
    for (const auto &name : mods.getMemberNames()) {
        if (!mods[name].isString())
            throw std::runtime_error("module '" + name +
                                     "': source must be a string");
        mod_names.insert(name);
    }

    const auto &patch = root["patch"];

    auto valid_source = [&](const std::string &src) -> bool {
        if (src == "capture_l" || src == "capture_r") return true;
        const auto under = src.rfind('_');
        if (under == std::string::npos || under < 4) return false;
        const auto mod = src.substr(0, under - 4);
        const auto mid = src.substr(under - 4, 4);
        if (mid != "_out") return false;
        return mod_names.contains(mod);
    };

    auto valid_sink = [&](const std::string &sink) -> bool {
        if (sink == "dac_l" || sink == "dac_r") return true;
        const auto under = sink.rfind('_');
        if (under == std::string::npos || under < 3) return false;
        const auto mod = sink.substr(0, under - 3);
        const auto mid = sink.substr(under - 3, 3);
        if (mid != "_in") return false;
        return mod_names.contains(mod);
    };

    for (const auto &sink : patch.getMemberNames()) {
        if (!patch[sink].isString())
            throw std::runtime_error("patch entry '" + sink +
                                     "': value must be a string");
        const auto src = patch[sink].asString();

        if (!valid_sink(sink))
            throw std::runtime_error("unknown sink '" + sink + "'");
        if (!valid_source(src)) {
            std::string msg = "unknown source '";
            msg += src;
            msg += "' for sink '";
            msg += sink;
            msg += '\'';
            throw std::runtime_error(msg);
        }
    }
}

} // namespace

auto parse_patch_json(const std::string &json_str) -> ParsedPatch {
    Json::Value root;
    Json::Reader reader;
    if (!reader.parse(json_str, root))
        throw std::runtime_error("invalid patch JSON: " +
                                 reader.getFormattedErrorMessages());

    validate_patch(root);

    ParsedPatch result;

    if (root.isMember("modules")) {
        const auto &mods = root["modules"];
        for (const auto &name : mods.getMemberNames())
            result.module_sources.emplace_back(name, mods[name].asString());
    }

    const auto &patch = root["patch"];
    for (const auto &sink : patch.getMemberNames())
        result.connections.emplace_back(sink, patch[sink].asString());

    return result;
}

auto build_routing_graph(const ParsedPatch &patch,
                         std::vector<IRModule> compiled_modules)
    -> RoutingGraph {
    std::unordered_map<std::string, IRModule *> mod_map;
    for (auto &ir : compiled_modules) mod_map[ir.name] = &ir;

    std::unordered_map<std::string, std::vector<std::string>> mod_inputs;
    std::string dac_l;
    std::string dac_r;
    bool has_capture = false;

    for (const auto &[sink, src] : patch.connections) {
        if (src == "capture_l" || src == "capture_r") has_capture = true;

        if (sink == "dac_l") {
            dac_l = src;
            continue;
        }
        if (sink == "dac_r") {
            dac_r = src;
            continue;
        }

        for (const auto &[mod_name, _] : patch.module_sources) {
            const auto in_prefix = mod_name + "_in_";
            if (!sink.starts_with(in_prefix)) continue;
            const size_t idx = std::stoul(sink.substr(in_prefix.size()));
            auto &inputs = mod_inputs[mod_name];
            if (inputs.size() <= idx) inputs.resize(idx + 1);
            inputs[idx] = src;
        }
    }

    std::unordered_map<std::string, std::unordered_set<std::string>> deps;
    for (const auto &[mod_name, _] : patch.module_sources) {
        deps[mod_name] = {};
        for (const auto &src : mod_inputs[mod_name]) {
            for (const auto &[other, _] : patch.module_sources) {
                if (src.starts_with(other + "_out_"))
                    deps[mod_name].insert(other);
            }
        }
    }

    std::vector<std::string> order;
    std::unordered_set<std::string> visited;
    std::function<void(const std::string &)> topo =
        [&](const std::string &name) -> void {
        if (visited.contains(name)) return;
        visited.insert(name);
        for (const auto &dep : deps[name]) topo(dep);
        order.push_back(name);
    };
    for (const auto &[name, _] : patch.module_sources) topo(name);

    RoutingGraph graph;
    graph.dac_l_source = dac_l;
    graph.dac_r_source = dac_r;
    graph.has_capture = has_capture;

    for (const auto &name : order) {
        auto it = std::ranges::find_if(
            compiled_modules,
            [&](const IRModule &m) -> bool { return m.name == name; });
        if (it == compiled_modules.end())
            throw std::runtime_error("Module not found: " + name);
        graph.modules.push_back({
            .ir = std::move(*it),
            .inputs = mod_inputs[name],
        });
    }

    return graph;
}
