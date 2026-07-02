#pragma once

#include "ir/ir.hpp"
#include "routing/routing.hpp"
#include <memory>
#include <string>
#include <vector>

struct BackendArtifact {
    std::vector<char> bytes;
};

struct BackendOptions {
    double sample_rate = 44100.0;
};

class CodeGen {
  public:
    CodeGen() = default;
    virtual ~CodeGen() = default;
    CodeGen(const CodeGen &) = delete;
    auto operator=(const CodeGen &) -> CodeGen & = delete;
    CodeGen(CodeGen &&) = delete;
    auto operator=(CodeGen &&) -> CodeGen & = delete;

    virtual void add_module(const IRModule &ir) = 0;
    virtual void finalize(const RoutingGraph &graph) = 0;
    virtual auto build() -> BackendArtifact = 0;
};

class Backend {
  public:
    Backend() = default;
    virtual ~Backend() = default;
    Backend(const Backend &) = delete;
    auto operator=(const Backend &) -> Backend & = delete;
    Backend(Backend &&) = delete;
    auto operator=(Backend &&) -> Backend & = delete;

    [[nodiscard]] virtual auto name() const -> std::string = 0;
    virtual auto create_codegen(const BackendOptions &opts)
        -> std::unique_ptr<CodeGen> = 0;
};
