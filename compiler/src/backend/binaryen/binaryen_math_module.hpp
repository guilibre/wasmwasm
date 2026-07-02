#pragma once

#include "binaryen-c.h"
#include <cstddef>

auto load_math_module(char *math_bin, size_t math_bin_size)
    -> BinaryenModuleRef;
