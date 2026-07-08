#include "instance_layout.hpp"

auto compute_instance_layout(const IRModule &ir, uint32_t &next_offset)
    -> InstanceLayout {
    InstanceLayout layout;

    uint32_t cursor = 0;

    layout.params_offset = cursor;
    for (const auto &[pname, pdefault] : ir.params) {
        layout.param_offset[pname] = cursor;
        cursor += 8;
    }

    layout.static_vars_offset = cursor;
    for (const auto &sv : ir.static_vars) {
        layout.static_var_offset[sv.name] = cursor;
        cursor += 8;
    }

    layout.in_ports_offset = cursor;
    cursor += static_cast<uint32_t>(ir.num_inputs) * 8;

    layout.out_ports_offset = cursor;
    cursor += static_cast<uint32_t>(ir.num_outputs) * 8;

    for (const auto &delay : ir.delays) {
        layout.delay_ptr_offset[delay.name] = cursor;
        cursor += 4;
    }
    for (const auto &delay : ir.delays) {
        layout.delay_buffer_offset[delay.name] = cursor;
        cursor += static_cast<uint32_t>(delay.size_elements) * 8;
    }
    for (const auto &arr : ir.static_arrays_decl) {
        layout.static_array_offset[arr.name] = cursor;
        cursor += static_cast<uint32_t>(arr.size_elements) * 8;
    }

    layout.slot_stride = cursor;

    layout.module_base = next_offset;
    next_offset += layout.slot_stride * max_instances_per_module;

    return layout;
}

auto allocate_shared_slot_table(uint32_t &next_offset) -> SharedSlotTable {
    SharedSlotTable table;
    table.slots_ids_base = next_offset;
    next_offset += max_instances_per_module * 4;
    table.slots_table_base = next_offset;
    next_offset += max_instances_per_module * 4;
    table.dense_base = next_offset;
    next_offset += max_instances_per_module * 4;
    table.sparse_base = next_offset;
    next_offset += max_instances_per_module * 4;
    table.active_count_addr = next_offset;
    next_offset += 4;
    table.pending_kills_base = next_offset;
    next_offset += max_instances_per_module * 4;
    table.pending_kill_count_addr = next_offset;
    next_offset += 4;
    return table;
}
