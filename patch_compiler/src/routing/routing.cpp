#include "routing.hpp"

#include <algorithm>
#include <cctype>
#include <functional>
#include <json/json.h>
#include <map>
#include <optional>
#include <stdexcept>
#include <unordered_map>
#include <unordered_set>
#include <utility>

namespace {

auto split_trailing_index(const std::string &src)
    -> std::optional<std::pair<std::string, int>> {
    const auto under = src.rfind('_');
    if (under == std::string::npos || under + 1 >= src.size())
        return std::nullopt;
    const auto idx_part = src.substr(under + 1);
    if (!std::ranges::all_of(idx_part, [](unsigned char c) -> bool {
            return std::isdigit(c) != 0;
        }))
        return std::nullopt;
    return std::make_pair(src.substr(0, under), std::stoi(idx_part));
}

void validate_graph(const Json::Value &modules, const Json::Value &patch,
                    const std::unordered_set<std::string> &extra_source_names,
                    const std::string &context) {
    if (!modules.isObject())
        throw std::runtime_error("'" + context + ".modules' must be an object");
    if (!patch.isObject())
        throw std::runtime_error("'" + context + ".patch' must be an object");

    std::unordered_set<std::string> mod_names;
    for (const auto &name : modules.getMemberNames()) {
        if (!modules[name].isString())
            throw std::runtime_error("module '" + name +
                                     "': source must be a string");
        mod_names.insert(name);
    }
    for (const auto &name : extra_source_names) mod_names.insert(name);

    auto valid_source = [&](const std::string &src) -> bool {
        if (src == "capture_l" || src == "capture_r") return true;
        const auto under = src.rfind('_');
        if (under != std::string::npos && under >= 4) {
            const auto mod = src.substr(0, under - 4);
            const auto mid = src.substr(under - 4, 4);
            if (mid == "_out" && mod_names.contains(mod)) return true;
        }
        return split_trailing_index(src).has_value();
    };

    auto valid_sink = [&](const std::string &sink) -> bool {
        if (sink == "dac_l" || sink == "dac_r") return true;
        if (sink.starts_with("out_")) {
            const auto idx_part = sink.substr(4);
            if (!idx_part.empty() &&
                std::ranges::all_of(idx_part, [](unsigned char c) -> bool {
                    return std::isdigit(c) != 0;
                }))
                return true;
        }
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

    if (!root.isObject())
        throw std::runtime_error("patch JSON must be an object");
    if (!root.isMember("instruments") || !root["instruments"].isObject())
        throw std::runtime_error(
            "patch JSON must have an object field 'instruments'");
    if (!root.isMember("global") || !root["global"].isObject())
        throw std::runtime_error(
            "patch JSON must have an object field 'global'");

    ParsedPatch result;

    const auto &instruments = root["instruments"];
    std::unordered_set<std::string> instrument_ids;
    for (const auto &instr_id : instruments.getMemberNames())
        instrument_ids.insert(instr_id);

    for (const auto &instr_id : instruments.getMemberNames()) {
        const auto &instr = instruments[instr_id];
        if (!instr.isObject())
            throw std::runtime_error("instrument '" + instr_id +
                                     "' must be an object");

        const auto &mods = instr.isMember("modules")
                               ? instr["modules"]
                               : Json::Value{Json::objectValue};
        const auto &patch = instr.isMember("patch")
                                ? instr["patch"]
                                : Json::Value{Json::objectValue};
        validate_graph(mods, patch, /*extra_source_names=*/{},
                       "instruments." + instr_id);

        InstrumentSource src;
        src.id = instr_id;
        for (const auto &name : mods.getMemberNames())
            src.module_sources.emplace_back(name, mods[name].asString());
        for (const auto &sink : patch.getMemberNames())
            src.connections.emplace_back(sink, patch[sink].asString());
        result.instruments.push_back(std::move(src));
    }

    const auto &global = root["global"];
    const auto &global_mods = global.isMember("modules")
                                  ? global["modules"]
                                  : Json::Value{Json::objectValue};
    const auto &global_patch = global.isMember("patch")
                                   ? global["patch"]
                                   : Json::Value{Json::objectValue};
    validate_graph(global_mods, global_patch, instrument_ids, "global");

    for (const auto &name : global_mods.getMemberNames())
        result.global_module_sources.emplace_back(name,
                                                  global_mods[name].asString());
    for (const auto &sink : global_patch.getMemberNames())
        result.global_connections.emplace_back(sink,
                                               global_patch[sink].asString());

    return result;
}

namespace {

struct RoutedGraph {
    std::vector<std::string> order;
    std::unordered_map<std::string, std::vector<std::string>> mod_inputs;
    std::string dac_l;
    std::string dac_r;
    std::vector<std::string> out_sources;
    std::unordered_map<std::string, int> external_input_channels;
    int external_input_count = 0;
};

auto route_one_graph(
    const std::vector<std::pair<std::string, std::string>> &module_sources,
    const std::vector<std::pair<std::string, std::string>> &connections,
    const std::unordered_set<std::string> &extra_output_names = {})
    -> RoutedGraph {
    std::unordered_set<std::string> mod_names;
    for (const auto &[mod_name, _] : module_sources) mod_names.insert(mod_name);
    for (const auto &name : extra_output_names) mod_names.insert(name);

    auto is_module_output = [&](const std::string &src) -> bool {
        const auto under = src.rfind('_');
        if (under == std::string::npos || under < 4) return false;
        const auto mod = src.substr(0, under - 4);
        const auto mid = src.substr(under - 4, 4);
        return mid == "_out" && mod_names.contains(mod);
    };

    std::unordered_map<std::string, std::vector<std::string>> mod_inputs;
    std::string dac_l;
    std::string dac_r;
    std::vector<std::string> out_sources;

    std::map<std::string, int> ext_channel_counts;
    std::unordered_map<std::string, std::string> ext_source_group;
    std::unordered_map<std::string, int> ext_source_local_channel;

    for (const auto &[sink, src] : connections) {
        if (src == "capture_l" || src == "capture_r") {
            const auto local = src == "capture_l" ? 0 : 1;
            ext_channel_counts["capture"] =
                std::max(ext_channel_counts["capture"], local + 1);
            ext_source_group[src] = "capture";
            ext_source_local_channel[src] = local;
        } else if (!is_module_output(src)) {
            if (auto split = split_trailing_index(src)) {
                auto &count = ext_channel_counts[split->first];
                count = std::max(count, split->second + 1);
                ext_source_group[src] = split->first;
                ext_source_local_channel[src] = split->second;
            }
        }

        if (sink == "dac_l") {
            dac_l = src;
            continue;
        }
        if (sink == "dac_r") {
            dac_r = src;
            continue;
        }
        if (sink.starts_with("out_")) {
            const auto idx = std::stoul(sink.substr(4));
            if (out_sources.size() <= idx) out_sources.resize(idx + 1);
            out_sources[idx] = src;
            continue;
        }

        for (const auto &[mod_name, _] : module_sources) {
            const auto in_prefix = mod_name + "_in_";
            if (!sink.starts_with(in_prefix)) continue;
            const size_t idx = std::stoul(sink.substr(in_prefix.size()));
            auto &inputs = mod_inputs[mod_name];
            if (inputs.size() <= idx) inputs.resize(idx + 1);
            inputs[idx] = src;
        }
    }

    std::unordered_map<std::string, int> group_base_offset;
    int cumulative_offset = 0;
    for (const auto &[name, channels] : ext_channel_counts) {
        group_base_offset[name] = cumulative_offset;
        cumulative_offset += channels;
    }

    std::unordered_map<std::string, int> external_input_channels;
    for (const auto &[src, group] : ext_source_group)
        external_input_channels[src] =
            group_base_offset[group] + ext_source_local_channel[src];

    std::unordered_map<std::string, std::unordered_set<std::string>> deps;
    for (const auto &[mod_name, _] : module_sources) {
        deps[mod_name] = {};
        for (const auto &src : mod_inputs[mod_name]) {
            for (const auto &[other, _] : module_sources) {
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
    for (const auto &[name, _] : module_sources) topo(name);

    return RoutedGraph{
        .order = std::move(order),
        .mod_inputs = std::move(mod_inputs),
        .dac_l = std::move(dac_l),
        .dac_r = std::move(dac_r),
        .out_sources = std::move(out_sources),
        .external_input_channels = std::move(external_input_channels),
        .external_input_count = cumulative_offset,
    };
}

} // namespace

auto build_routing_graph(const ParsedPatch &patch,
                         std::vector<IRModule> compiled_modules)
    -> RoutingGraph {
    std::unordered_map<std::string, size_t> instr_index;
    for (size_t i = 0; i < patch.instruments.size(); ++i)
        instr_index[patch.instruments[i].id] = i;

    RoutingGraph graph;
    std::vector<RoutedGraph> per_instrument;
    per_instrument.reserve(patch.instruments.size());
    std::unordered_set<std::string> all_instrument_block_names;

    for (const auto &instr : patch.instruments) {
        auto routed = route_one_graph(instr.module_sources, instr.connections);

        InstrumentGroup group;
        group.id = instr.id;
        group.module_names = routed.order;
        graph.instruments.push_back(std::move(group));

        for (const auto &name : routed.order) {
            all_instrument_block_names.insert(name);
            auto it = std::ranges::find_if(
                compiled_modules,
                [&](const IRModule &m) -> bool { return m.name == name; });
            if (it == compiled_modules.end())
                throw std::runtime_error("Module not found: " + name);
            graph.modules.push_back({
                .ir = std::move(*it),
                .inputs = routed.mod_inputs[name],
            });
        }

        per_instrument.push_back(std::move(routed));
    }

    std::vector<std::pair<std::string, std::string>>
        resolved_global_connections;
    resolved_global_connections.reserve(patch.global_connections.size());
    for (const auto &[sink, src] : patch.global_connections) {
        const auto under = src.rfind('_');
        bool is_instr_out = false;
        if (under != std::string::npos && under >= 4) {
            const auto mod = src.substr(0, under - 4);
            const auto mid = src.substr(under - 4, 4);
            is_instr_out = mid == "_out" && instr_index.contains(mod);
        }

        if (is_instr_out) {
            const auto instr_id = src.substr(0, under - 4);
            const auto idx = std::stoul(src.substr(under + 1));
            const auto &out_sources =
                per_instrument[instr_index.at(instr_id)].out_sources;
            if (idx >= out_sources.size())
                throw std::runtime_error("instrument '" + instr_id +
                                         "' has no out_" + std::to_string(idx));
            resolved_global_connections.emplace_back(sink, out_sources[idx]);
        } else {
            resolved_global_connections.emplace_back(sink, src);
        }
    }

    auto global_routed = route_one_graph(patch.global_module_sources,
                                         resolved_global_connections,
                                         all_instrument_block_names);

    for (const auto &name : global_routed.order) {
        auto it = std::ranges::find_if(
            compiled_modules,
            [&](const IRModule &m) -> bool { return m.name == name; });
        if (it == compiled_modules.end())
            throw std::runtime_error("Module not found: " + name);
        graph.modules.push_back({
            .ir = std::move(*it),
            .inputs = global_routed.mod_inputs[name],
        });
    }

    graph.dac_l_source = global_routed.dac_l;
    graph.dac_r_source = global_routed.dac_r;
    graph.out_sources = global_routed.out_sources;
    graph.external_input_channels = global_routed.external_input_channels;
    graph.external_input_count = global_routed.external_input_count;

    return graph;
}
