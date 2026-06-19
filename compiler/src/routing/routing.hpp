#pragma once

#include "ir/ir.hpp"
#include <string>
#include <vector>

struct ModuleRoute {
    IRModule ir;
    std::vector<std::string> inputs;
};

struct RoutingGraph {
    std::vector<ModuleRoute> modules;
    std::string dac_l_source;
    std::string dac_r_source;
    bool has_capture = false;
};

struct ParsedPatch {
    std::vector<std::pair<std::string, std::string>> module_sources;
    std::vector<std::pair<std::string, std::string>> connections;
};

auto parse_patch_json(const std::string &json) -> ParsedPatch;

auto build_routing_graph(const ParsedPatch &patch,
                         std::vector<IRModule> compiled_modules)
    -> RoutingGraph;
