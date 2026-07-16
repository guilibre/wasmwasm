#include "routing.hpp"

#include <algorithm>
#include <cctype>
#include <functional>
#include <json/json.h>
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
                    const std::string &context,
                    const std::string &own_instrument_id,
                    const std::unordered_set<std::string> &instrument_ids) {
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
        if (src == "adc_l" || src == "adc_r") return true;
        if (!own_instrument_id.empty()) {
            const auto prefix = own_instrument_id + "_in_";
            if (src.starts_with(prefix) &&
                split_trailing_index(src).has_value())
                return true;
        }
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
        constexpr std::string_view in_marker = "_in_";
        const auto marker_pos = sink.find(in_marker);
        if (marker_pos != std::string::npos &&
            instrument_ids.contains(sink.substr(0, marker_pos)))
            return true;
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
                       "instruments." + instr_id, instr_id, instrument_ids);

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
    validate_graph(global_mods, global_patch, instrument_ids, "global", "",
                   instrument_ids);

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
    std::unordered_map<std::string, std::unordered_set<std::string>>
        feedback_producers;
    std::string dac_l;
    std::string dac_r;
    std::vector<std::string> out_sources;
};

auto route_one_graph(
    const std::vector<std::pair<std::string, std::string>> &module_sources,
    const std::vector<std::pair<std::string, std::string>> &connections)
    -> RoutedGraph {
    std::unordered_map<std::string, std::vector<std::string>> mod_inputs;
    std::string dac_l;
    std::string dac_r;
    std::vector<std::string> out_sources;

    for (const auto &[sink, src] : connections) {
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

    std::unordered_map<std::string, std::vector<std::string>> deps;
    for (const auto &[mod_name, _] : module_sources) {
        auto &mod_deps = deps[mod_name];
        std::unordered_set<std::string> seen_deps;
        for (const auto &src : mod_inputs[mod_name]) {
            for (const auto &[other, _] : module_sources) {
                if (other == mod_name) continue;
                if (src.starts_with(other + "_out_") &&
                    !seen_deps.contains(other)) {
                    seen_deps.insert(other);
                    mod_deps.push_back(other);
                }
            }
        }
    }

    std::unordered_map<std::string, std::unordered_set<std::string>>
        feedback_producers;
    for (const auto &[mod_name, _] : module_sources) {
        for (const auto &src : mod_inputs[mod_name]) {
            if (src.starts_with(mod_name + "_out_"))
                feedback_producers[mod_name].insert(mod_name);
        }
    }

    std::vector<std::string> order;
    std::unordered_set<std::string> visited;
    std::unordered_set<std::string> in_stack;
    std::function<void(const std::string &)> topo =
        [&](const std::string &name) -> void {
        if (visited.contains(name)) return;
        visited.insert(name);
        in_stack.insert(name);
        for (const auto &dep : deps[name]) {
            if (in_stack.contains(dep)) {
                feedback_producers[name].insert(dep);
                continue;
            }
            topo(dep);
        }
        in_stack.erase(name);
        order.push_back(name);
    };
    for (const auto &[name, _] : module_sources) topo(name);

    return RoutedGraph{
        .order = std::move(order),
        .mod_inputs = std::move(mod_inputs),
        .feedback_producers = std::move(feedback_producers),
        .dac_l = std::move(dac_l),
        .dac_r = std::move(dac_r),
        .out_sources = std::move(out_sources),
    };
}

auto route_and_emit_instrument(
    const InstrumentSource &instr,
    const std::unordered_map<std::string, IRModule> &compiled_modules_by_name,
    RoutingGraph &graph,
    std::unordered_set<std::string> &all_instrument_block_names)
    -> RoutedGraph {
    auto routed = route_one_graph(instr.module_sources, instr.connections);

    std::vector<std::string> param_names;
    std::unordered_set<std::string> seen_params;
    for (const auto &name : routed.order) {
        const auto it = compiled_modules_by_name.find(name);
        if (it == compiled_modules_by_name.end())
            throw std::runtime_error("Module not found: " + name);
        for (const auto &[param_name, _] : it->second.params) {
            if (!seen_params.contains(param_name)) {
                seen_params.insert(param_name);
                param_names.push_back(param_name);
            }
        }
    }

    InstrumentGroup group;
    group.id = instr.id;
    group.module_names = routed.order;
    group.param_names = std::move(param_names);
    graph.instruments.push_back(std::move(group));

    for (const auto &name : routed.order) {
        all_instrument_block_names.insert(name);
        const auto it = compiled_modules_by_name.find(name);
        if (it == compiled_modules_by_name.end())
            throw std::runtime_error("Module not found: " + name);
        const auto &inputs = routed.mod_inputs[name];
        const auto &producers = routed.feedback_producers[name];
        std::vector<bool> feedback_inputs(inputs.size(), false);
        for (size_t i = 0; i < inputs.size(); ++i) {
            const auto under = inputs[i].rfind("_out_");
            if (under != std::string::npos &&
                producers.contains(inputs[i].substr(0, under)))
                feedback_inputs[i] = true;
        }
        graph.modules.push_back({
            .ir = it->second,
            .inputs = inputs,
            .feedback_inputs = std::move(feedback_inputs),
        });
    }

    return routed;
}

} // namespace

auto build_routing_graph(const ParsedPatch &patch,
                         std::vector<IRModule> compiled_modules)
    -> RoutingGraph {
    std::unordered_map<std::string, size_t> instr_index;
    for (size_t i = 0; i < patch.instruments.size(); ++i)
        instr_index[patch.instruments[i].id] = i;

    std::unordered_map<std::string, IRModule> compiled_modules_by_name;
    for (auto &m : compiled_modules)
        compiled_modules_by_name[m.name] = std::move(m);

    RoutingGraph graph;
    std::vector<RoutedGraph> per_instrument;
    per_instrument.reserve(patch.instruments.size());
    std::unordered_set<std::string> all_instrument_block_names;

    for (const auto &instr : patch.instruments) {
        auto routed = route_and_emit_instrument(
            instr, compiled_modules_by_name, graph, all_instrument_block_names);
        per_instrument.push_back(std::move(routed));
    }

    std::unordered_map<std::string, std::string> instrument_in_source;
    std::unordered_map<std::string, std::string> instrument_in_producer;
    for (const auto &[sink, src] : patch.global_connections) {
        constexpr std::string_view marker = "_in_";
        const auto pos = sink.find(marker);
        if (pos == std::string::npos) continue;
        const auto instr_id = sink.substr(0, pos);
        if (!instr_index.contains(instr_id)) continue;

        const auto under = src.rfind('_');
        bool src_is_instr_out = false;
        if (under != std::string::npos && under >= 4) {
            const auto mod = src.substr(0, under - 4);
            const auto mid = src.substr(under - 4, 4);
            src_is_instr_out = mid == "_out" && instr_index.contains(mod);
        }
        if (src_is_instr_out) {
            const auto src_instr_id = src.substr(0, under - 4);
            const auto idx = std::stoul(src.substr(under + 1));
            const auto &out_sources =
                per_instrument[instr_index.at(src_instr_id)].out_sources;
            if (idx >= out_sources.size())
                throw std::runtime_error("instrument '" + src_instr_id +
                                         "' has no out_" + std::to_string(idx));
            instrument_in_source[sink] = out_sources[idx];
            instrument_in_producer[sink] = src_instr_id;
        } else {
            instrument_in_source[sink] = src;
        }
    }

    std::unordered_set<std::string> instruments_needing_reroute;
    for (const auto &[sink, _] : instrument_in_source)
        instruments_needing_reroute.insert(sink.substr(0, sink.find("_in_")));

    for (const auto &instr_id : instruments_needing_reroute) {
        const auto instr_idx = instr_index.at(instr_id);
        const auto &instr = patch.instruments[instr_idx];
        const auto &old_group = graph.instruments[instr_idx];
        graph.modules.erase(
            std::ranges::remove_if(graph.modules,
                                   [&](const ModuleRoute &m) -> bool {
                                       return std::ranges::find(
                                                  old_group.module_names,
                                                  m.ir.name) !=
                                              old_group.module_names.end();
                                   })
                .begin(),
            graph.modules.end());
        graph.instruments.erase(graph.instruments.begin() +
                                static_cast<std::ptrdiff_t>(instr_idx));

        std::vector<std::pair<std::string, std::string>> rewritten_connections;
        rewritten_connections.reserve(instr.connections.size());
        for (const auto &[sink, src] : instr.connections) {
            if (const auto it = instrument_in_source.find(src);
                it != instrument_in_source.end())
                rewritten_connections.emplace_back(sink, it->second);
            else
                rewritten_connections.emplace_back(sink, src);
        }

        auto routed = route_and_emit_instrument(
            InstrumentSource{.id = instr.id,
                             .module_sources = instr.module_sources,
                             .connections = rewritten_connections},
            compiled_modules_by_name, graph, all_instrument_block_names);
        auto new_group = std::move(graph.instruments.back());
        graph.instruments.pop_back();
        graph.instruments.insert(graph.instruments.begin() +
                                     static_cast<std::ptrdiff_t>(instr_idx),
                                 std::move(new_group));
        per_instrument[instr_idx] = std::move(routed);
    }

    std::unordered_map<std::string, std::unordered_set<std::string>> depends_on;
    for (const auto &[sink, producer_id] : instrument_in_producer) {
        const auto consumer_id = sink.substr(0, sink.find("_in_"));
        if (producer_id != consumer_id)
            depends_on[consumer_id].insert(producer_id);
    }

    {
        std::vector<std::string> order;
        std::unordered_set<std::string> visited;
        std::unordered_set<std::string> in_stack;
        std::function<void(const std::string &)> topo =
            [&](const std::string &id) -> void {
            if (visited.contains(id)) return;
            visited.insert(id);
            in_stack.insert(id);
            for (const auto &dep : depends_on[id]) {
                if (in_stack.contains(dep))
                    throw std::runtime_error(
                        "cyclic instrument input dependency involving '" + id +
                        "' and '" + dep + "'");
                topo(dep);
            }
            in_stack.erase(id);
            order.push_back(id);
        };
        for (const auto &instr : patch.instruments) topo(instr.id);

        std::vector<InstrumentGroup> reordered_instruments;
        reordered_instruments.reserve(graph.instruments.size());
        for (const auto &id : order)
            reordered_instruments.push_back(
                std::move(graph.instruments[instr_index.at(id)]));
        graph.instruments = std::move(reordered_instruments);
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
                                         resolved_global_connections);

    std::vector<std::string> global_param_names;
    std::unordered_set<std::string> seen_global_params;
    for (const auto &name : global_routed.order) {
        const auto it = compiled_modules_by_name.find(name);
        if (it == compiled_modules_by_name.end())
            throw std::runtime_error("Module not found: " + name);
        for (const auto &[param_name, _] : it->second.params) {
            if (!seen_global_params.contains(param_name)) {
                seen_global_params.insert(param_name);
                global_param_names.push_back(param_name);
            }
        }
        const auto &inputs = global_routed.mod_inputs[name];
        const auto &producers = global_routed.feedback_producers[name];
        std::vector<bool> feedback_inputs(inputs.size(), false);
        for (size_t i = 0; i < inputs.size(); ++i) {
            const auto under = inputs[i].rfind("_out_");
            if (under != std::string::npos &&
                producers.contains(inputs[i].substr(0, under)))
                feedback_inputs[i] = true;
        }
        graph.modules.push_back({
            .ir = it->second,
            .inputs = inputs,
            .feedback_inputs = std::move(feedback_inputs),
        });
    }

    graph.global_module_names = global_routed.order;
    graph.global_param_names = std::move(global_param_names);
    graph.dac_l_source = global_routed.dac_l;
    graph.dac_r_source = global_routed.dac_r;
    graph.out_sources = global_routed.out_sources;
    graph.external_input_channels = {{"adc_l", 0}, {"adc_r", 1}};
    graph.external_input_count = 2;

    return graph;
}
