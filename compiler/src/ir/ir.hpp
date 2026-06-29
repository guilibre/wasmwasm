#pragma once

#include "ast/ast.hpp"
#include <cstdint>
#include <memory>
#include <optional>
#include <string>
#include <unordered_map>
#include <variant>
#include <vector>

enum class IRType : uint8_t { Float, Int, Void, Vec };

struct IRLiteral {
    double value;
};
struct IRLocalRef {
    std::string name;
};

using IRValue = std::variant<IRLiteral, IRLocalRef>;

struct IRBinOp {
    std::string result;
    Operation op;
    IRValue left, right;
};

struct IRUnaryNeg {
    std::string result;
    IRValue operand;
};

struct IRAssign {
    std::string result;
    IRValue value;
    IRType type;
};

struct IRCall {
    std::string result;
    std::string callee;
    std::vector<IRValue> args;
    IRType result_type;
};

struct IRDelayRead {
    std::string result;
    std::string delay;
};

struct IRDelayReadDelayed {
    std::string result;
    std::string delay;
    std::string delay_ref;
};

struct IRDelayWrite {
    std::string delay;
    IRValue value;
};

struct IRDelayWriteQuiet {
    std::string delay;
    IRValue value;
    std::optional<std::string> delay_ref;
};

struct IRGlobalRead {
    std::string result;
    std::string name;
    IRType type;
};

struct IRIfBody;

struct IRIf {
    IRValue condition;
    std::shared_ptr<IRIfBody> body;
};

struct IRStaticRead {
    std::string result;
    std::string name;
};

struct IRStaticWrite {
    std::string name;
    IRValue value;
};

struct IRMemRead {
    std::string result;
    uint32_t addr;
};

struct IRMemWrite {
    uint32_t addr;
    IRValue value;
};

struct IRVecLoad {
    std::string result;
    uint32_t addr;
};

struct IRVecStore {
    uint32_t addr;
    std::string value;
};

struct IRVecBinOp {
    std::string result;
    Operation op;
    std::string lhs;
    std::string rhs;
};

struct IRVecSplat {
    std::string result;
    IRValue scalar;
};

struct IRVecExtractLane {
    std::string result;
    std::string vec;
    uint8_t lane;
};

struct IRReturn {
    std::optional<IRValue> value;
};

struct IRParamRead {
    std::string result;
    std::string name;
};

struct IRParamWrite {
    std::string name;
    IRValue value;
};

struct IRInputRead {
    std::string result;
    size_t index;
};

struct IROutputWrite {
    size_t index{};
    IRValue value;
};

using IRInstr =
    std::variant<IRBinOp, IRUnaryNeg, IRAssign, IRCall, IRDelayRead,
                 IRDelayReadDelayed, IRDelayWrite, IRDelayWriteQuiet,
                 IRGlobalRead, IRIf, IRInputRead, IROutputWrite, IRParamRead,
                 IRParamWrite, IRStaticRead, IRStaticWrite, IRReturn, IRMemRead,
                 IRMemWrite, IRVecLoad, IRVecStore, IRVecBinOp, IRVecSplat,
                 IRVecExtractLane>;

struct IRIfBody {
    std::vector<IRInstr> then_body;
    std::vector<IRInstr> else_body;
};

struct IRParam {
    std::string name;
    IRType type;
};

struct IRFunction {
    std::string name;
    std::vector<IRParam> params;
    IRType return_type{IRType::Void};
    std::vector<IRInstr> body;
};

struct IRDelayDecl {
    std::string name;
    size_t size_elements;
    std::string init_fn;
};

inline constexpr uint32_t delay_memory_start = 4096;

struct IRStaticVar {
    std::string name;
    IRType type;
};

struct IRModule {
    std::string name;
    std::vector<IRFunction> functions;
    std::vector<IRDelayDecl> delays;
    std::vector<IRStaticVar> static_vars;
    std::vector<std::pair<std::string, double>> params;
    std::string init_fn;
    std::string main_fn;
    std::string static_init_fn;
    size_t num_inputs = 0;
    size_t num_outputs = 0;
    uint32_t memory_base = delay_memory_start;
    std::unordered_map<std::string, uint32_t> static_array_bases;
    uint32_t static_array_total_bytes = 0;

    [[nodiscard]] auto delay_base(const std::string &buf_name) const
        -> uint32_t;
    [[nodiscard]] auto total_delay_bytes() const -> uint32_t;
    [[nodiscard]] auto static_array_base(const std::string &array_name) const
        -> uint32_t;
    auto alloc_static_array(const std::string &array_name, size_t n_elements)
        -> uint32_t;
};
