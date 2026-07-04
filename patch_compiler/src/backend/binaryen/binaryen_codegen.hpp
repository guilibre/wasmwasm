#pragma once

#include "backend/backend.hpp"
#include "binaryen-c.h"
#include <cstdint>
#include <string>
#include <unordered_map>

inline constexpr uint32_t wasm_reserved_io_bytes = (64 * 1024) + 1024;

class BinaryenCodeGen : public CodeGen {
  public:
    BinaryenCodeGen(BinaryenModuleRef math_module, double sample_rate);
    ~BinaryenCodeGen() override;

    BinaryenCodeGen(const BinaryenCodeGen &) = delete;
    auto operator=(const BinaryenCodeGen &) -> BinaryenCodeGen & = delete;
    BinaryenCodeGen(BinaryenCodeGen &&) = delete;
    auto operator=(BinaryenCodeGen &&) -> BinaryenCodeGen & = delete;

    void add_module(const IRModule &ir) override;
    void finalize(const RoutingGraph &graph) override;
    auto build() -> BackendArtifact override;

    [[nodiscard]] auto raw_module() const -> BinaryenModuleRef { return mod_; }

  private:
    BinaryenModuleRef mod_;
    BinaryenModuleRef math_module_;
    double sample_rate_;
    uint32_t next_offset_ = wasm_reserved_io_bytes;
    std::unordered_map<std::string, uint32_t> buffer_bases_;
};
