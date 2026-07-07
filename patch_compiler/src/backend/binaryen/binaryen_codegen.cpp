#include "binaryen_codegen.hpp"

#include "binaryen_emit.hpp"
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
    auto layout = compute_instance_layout(ir, next_offset_);
    emit_ir(ir, mod_, math_module_, sample_rate_, layout);
    layouts_[ir.name] = std::move(layout);
    modules_[ir.name] = ir;
}

void BinaryenCodeGen::finalize(const RoutingGraph &graph) {
    for (const auto &group : graph.instruments) {
        const auto shared_table = allocate_shared_slot_table(next_offset_);
        for (const auto &name : group.module_names) {
            auto &layout = layouts_.at(name);
            layout.slots_table_base = shared_table.slots_table_base;
            layout.slots_ids_base = shared_table.slots_ids_base;
        }

        std::vector<const IRModule *> members;
        members.reserve(group.module_names.size());
        for (const auto &name : group.module_names)
            members.push_back(&modules_.at(name));
        emit_instance_api_group(group.id, members, group.param_names, mod_,
                                layouts_);
    }

    std::vector<const IRModule *> global_members;
    global_members.reserve(graph.global_module_names.size());
    for (const auto &name : graph.global_module_names)
        global_members.push_back(&modules_.at(name));
    emit_global_set_param(global_members, graph.global_param_names, mod_,
                          layouts_);

    emit_main_loop(graph, mod_, layouts_);
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
    artifact.memory_bytes = next_offset_;
    return artifact;
}
