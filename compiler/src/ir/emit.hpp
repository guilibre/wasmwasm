#pragma once

#include "binaryen-c.h"
#include "ir.hpp"

void emit_ir(const IRModule &ir, BinaryenModuleRef mod,
             BinaryenModuleRef math_module, double sample_rate);
