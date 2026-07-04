#pragma once

#include "binaryen-c.h"
#include "ir/ir.hpp"
#include "routing/routing.hpp"
#include <cstdint>
#include <string>
#include <unordered_map>

void emit_ir(const IRModule &ir, BinaryenModuleRef mod,
             BinaryenModuleRef math_module, double sample_rate,
             const std::unordered_map<std::string, uint32_t> &buffer_bases);

void emit_main_loop(const RoutingGraph &graph, BinaryenModuleRef mod);
