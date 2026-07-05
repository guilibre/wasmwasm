#pragma once

#include "ir/ir.hpp"
#include <cstdint>
#include <string>
#include <unordered_map>

inline constexpr uint32_t max_instances_per_module = 128;

struct InstanceLayout {
    uint32_t module_base = 0;
    uint32_t slot_stride = 0;
    uint32_t params_offset = 0;
    uint32_t static_vars_offset = 0;
    uint32_t in_ports_offset = 0;
    uint32_t out_ports_offset = 0;
    std::unordered_map<std::string, uint32_t> param_offset;
    std::unordered_map<std::string, uint32_t> static_var_offset;
    std::unordered_map<std::string, uint32_t> delay_ptr_offset;
    std::unordered_map<std::string, uint32_t> delay_buffer_offset;
    std::unordered_map<std::string, uint32_t> static_array_offset;

    uint32_t slots_table_base = 0;
    uint32_t slots_ids_base = 0;

    [[nodiscard]] auto slot_addr(uint32_t slot_index) const -> uint32_t {
        return module_base + (slot_index * slot_stride);
    }
};

struct SharedSlotTable {
    uint32_t slots_table_base = 0;
    uint32_t slots_ids_base = 0;
};

auto compute_instance_layout(const IRModule &ir, uint32_t &next_offset)
    -> InstanceLayout;

auto allocate_shared_slot_table(uint32_t &next_offset) -> SharedSlotTable;
