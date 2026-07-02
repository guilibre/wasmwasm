#include "binaryen_math_module.hpp"

#include <stdexcept>

auto load_math_module(char *math_bin, size_t math_bin_size)
    -> BinaryenModuleRef {
    auto *math_module = BinaryenModuleReadWithFeatures(math_bin, math_bin_size,
                                                       BinaryenFeatureAll());
    if (math_module == nullptr)
        throw std::runtime_error("failed to parse math module");
    if (!BinaryenModuleValidate(math_module))
        throw std::runtime_error("math module is invalid");
    return math_module;
}
