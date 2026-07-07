#pragma once

#include "binaryen-c.h"
#include "instance_layout.hpp"
#include "ir/ir.hpp"
#include "routing/routing.hpp"
#include <string>
#include <unordered_map>
#include <vector>

void emit_ir(const IRModule &ir, BinaryenModuleRef mod,
             BinaryenModuleRef math_module, double sample_rate,
             const InstanceLayout &layout);

void emit_instance_api_group(
    const std::string &instrument_id,
    const std::vector<const IRModule *> &members,
    const std::vector<std::string> &param_names, BinaryenModuleRef mod,
    const std::unordered_map<std::string, InstanceLayout> &layouts);

void emit_global_set_param(
    const std::vector<const IRModule *> &members,
    const std::vector<std::string> &param_names, BinaryenModuleRef mod,
    const std::unordered_map<std::string, InstanceLayout> &layouts);

void emit_main_loop(
    const RoutingGraph &graph, BinaryenModuleRef mod,
    const std::unordered_map<std::string, InstanceLayout> &layouts);
