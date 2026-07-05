#pragma once

#include "ir/ir.hpp"
#include <string>
#include <unordered_map>
#include <vector>

struct ModuleRoute {
    IRModule ir;
    std::vector<std::string> inputs;
};

struct InstrumentGroup {
    std::string id;
    std::vector<std::string>
        module_names; // this instrument's blocks, topo-ordered
};

struct RoutingGraph {
    std::vector<ModuleRoute> modules;
    std::vector<InstrumentGroup> instruments;
    std::string dac_l_source;
    std::string dac_r_source;
    std::vector<std::string> out_sources;
    std::unordered_map<std::string, int> external_input_channels;
    int external_input_count = 0;
};

struct InstrumentSource {
    std::string id;
    std::vector<std::pair<std::string, std::string>> module_sources;
    std::vector<std::pair<std::string, std::string>> connections;
};

struct ParsedPatch {
    std::vector<InstrumentSource> instruments;
    std::vector<std::pair<std::string, std::string>> global_module_sources;
    std::vector<std::pair<std::string, std::string>> global_connections;
};

auto parse_patch_json(const std::string &json) -> ParsedPatch;

auto build_routing_graph(const ParsedPatch &patch,
                         std::vector<IRModule> compiled_modules)
    -> RoutingGraph;
