#include "binaryen_codegen.hpp"

#include "binaryen_emit.hpp"
#include <algorithm>
#include <memory>
#include <stdexcept>

BinaryenCodeGen::BinaryenCodeGen(BinaryenModuleRef math_module,
                                 double sample_rate)
    : mod_(BinaryenModuleCreate()), math_module_(math_module),
      sample_rate_(sample_rate) {
    if (mod_ == nullptr)
        throw std::runtime_error("unable to create binaryen module");
}

BinaryenCodeGen::~BinaryenCodeGen() {
    if (mod_ != nullptr) BinaryenModuleDispose(mod_);
}

void BinaryenCodeGen::add_module(const IRModule &ir) {
    for (const auto &name : ir.alloc_order) {
        size_t n = 0;
        if (auto it = std::ranges::find_if(
                ir.delays,
                [&](const auto &d) -> bool { return d.name == name; });
            it != ir.delays.end()) {
            n = it->size_elements;
        } else if (auto it2 = std::ranges::find_if(
                       ir.static_arrays_decl,
                       [&](const auto &a) -> bool { return a.name == name; });
                   it2 != ir.static_arrays_decl.end()) {
            n = it2->size_elements;
        }
        buffer_bases_[ir.name + "$" + name] = next_offset_;
        next_offset_ += static_cast<uint32_t>(n * 8);
    }
    emit_ir(ir, mod_, math_module_, sample_rate_, buffer_bases_);
}

void BinaryenCodeGen::finalize(const RoutingGraph &graph) {
    emit_main_loop(graph, mod_);
}

auto BinaryenCodeGen::build() -> BackendArtifact {
    if (!BinaryenModuleValidate(mod_))
        throw std::runtime_error("invalid module");

    BinaryenSetOptimizeLevel(3);
    BinaryenSetShrinkLevel(0);
    BinaryenSetFastMath(true);
    BinaryenSetLowMemoryUnused(true);
    BinaryenSetAlwaysInlineMaxSize(100);
    BinaryenSetFlexibleInlineMaxSize(250);
    BinaryenSetOneCallerInlineMaxSize(250);
    BinaryenModuleOptimize(mod_);

    if (!BinaryenModuleValidate(mod_))
        throw std::runtime_error("invalid module");

    auto result = BinaryenModuleAllocateAndWrite(mod_, nullptr);
    auto binary_ptr =
        std::unique_ptr<void, decltype(&free)>(result.binary, free);
    auto *binary_data = static_cast<char *>(result.binary);
    BackendArtifact artifact;
    artifact.bytes.assign(binary_data, binary_data + result.binaryBytes);
    return artifact;
}
