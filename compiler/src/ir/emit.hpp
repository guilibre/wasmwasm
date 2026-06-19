#pragma once

#include "binaryen-c.h"
#include "ir.hpp"
#include "routing/routing.hpp"

void emit_ir(const IRModule &ir, BinaryenModuleRef mod,
             BinaryenModuleRef math_module, double sample_rate);

void emit_main_loop(const RoutingGraph &graph, BinaryenModuleRef mod);
