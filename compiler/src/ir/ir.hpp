#pragma once

#include "../ast/ast.hpp"
#include <cstdint>
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

struct IRBufferWrite {
    std::string buffer;
    IRValue value;
};

struct IRGlobalRead {
    std::string result;
    std::string name;
    IRType type;
};

struct IRReturn {
    std::optional<IRValue> value;
};

using IRInstr =
    std::variant<IRBinOp, IRUnaryNeg, IRAssign, IRCall, IRBufferRead,
                 IRBufferWrite, IRGlobalRead, IRReturn>;

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

struct IRModule {
    std::vector<IRFunction> functions;
    std::vector<IRBufferDecl> buffers;
    std::string init_fn;
    std::string main_fn;

    [[nodiscard]] auto buffer_base(const std::string &name) const -> uint32_t;
};
