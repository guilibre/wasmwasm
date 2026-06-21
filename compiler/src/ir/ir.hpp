#pragma once

#include "ast/ast.hpp"
#include <cstdint>
#include <memory>
#include <optional>
#include <string>
#include <variant>
#include <vector>

enum class IRType : uint8_t { Float, Int, Void };

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

struct IRBufferRead {
    std::string result;
    std::string buffer;
};

struct IRBufferReadDelayed {
    std::string result;
    std::string buffer;
    std::string delay_ref;
};

struct IRBufferWrite {
    std::string buffer;
    IRValue value;
};

struct IRGlobalRead {
    std::string result;
    std::string name;
    IRType type;
};

struct IRIfBody; // forward — defined after IRInstr to break circular dependency

struct IRIf {
    IRValue condition;
    std::shared_ptr<IRIfBody> body;
};

struct IRReturn {
    std::optional<IRValue> value;
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
    std::variant<IRBinOp, IRUnaryNeg, IRAssign, IRCall, IRBufferRead,
                 IRBufferReadDelayed, IRBufferWrite, IRGlobalRead, IRIf,
                 IRInputRead, IROutputWrite, IRReturn>;

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

struct IRBufferDecl {
    std::string name;
    size_t size_elements;
    std::string init_fn;
};

inline constexpr uint32_t buffer_memory_start = 4096;

struct IRModule {
    std::string name;
    std::vector<IRFunction> functions;
    std::vector<IRBufferDecl> buffers;
    std::string init_fn;
    std::string main_fn;
    size_t num_inputs = 0;
    size_t num_outputs = 0;
    uint32_t memory_base = buffer_memory_start;

    [[nodiscard]] auto buffer_base(const std::string &buf_name) const
        -> uint32_t;
    [[nodiscard]] auto total_buffer_bytes() const -> uint32_t;
};
