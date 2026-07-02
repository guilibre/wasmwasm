#pragma once

#include "backend/backend.hpp"
#include "binaryen-c.h"

class BinaryenBackend : public Backend {
  public:
    BinaryenBackend(char *math_bin, size_t math_bin_size);
    ~BinaryenBackend() override;

    BinaryenBackend(const BinaryenBackend &) = delete;
    auto operator=(const BinaryenBackend &) -> BinaryenBackend & = delete;
    BinaryenBackend(BinaryenBackend &&) = delete;
    auto operator=(BinaryenBackend &&) -> BinaryenBackend & = delete;

    [[nodiscard]] auto name() const -> std::string override;
    auto create_codegen(const BackendOptions &opts)
        -> std::unique_ptr<CodeGen> override;

  private:
    BinaryenModuleRef math_module_;
};
