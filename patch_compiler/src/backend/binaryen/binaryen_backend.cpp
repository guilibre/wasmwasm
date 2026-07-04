#include "binaryen_backend.hpp"

#include "binaryen_codegen.hpp"
#include "binaryen_math_module.hpp"
#include <memory>

BinaryenBackend::BinaryenBackend(char *math_bin, size_t math_bin_size)
    : math_module_(load_math_module(math_bin, math_bin_size)) {}

BinaryenBackend::~BinaryenBackend() {
    if (math_module_ != nullptr) BinaryenModuleDispose(math_module_);
}

auto BinaryenBackend::name() const -> std::string { return "binaryen"; }

auto BinaryenBackend::create_codegen(const BackendOptions &opts)
    -> std::unique_ptr<CodeGen> {
    return std::make_unique<BinaryenCodeGen>(math_module_, opts.sample_rate);
}
