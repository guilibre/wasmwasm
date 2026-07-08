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
    std::vector<std::string> result;
    std::string callee;
    std::vector<IRValue> args;
    std::vector<IRType> result_type;
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

struct IRMemRef {
    std::string buffer;
    uint32_t byte_offset = 0;
};

struct IRMemRead {
    std::string result;
    IRMemRef ref;
};

struct IRMemWrite {
    IRMemRef ref;
    IRValue value;
};

struct IRReturn {
    std::optional<std::vector<IRValue>> value;
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

struct IRDie {};

using IRInstr =
    std::variant<IRBinOp, IRUnaryNeg, IRAssign, IRCall, IRDelayRead,
                 IRDelayReadDelayed, IRDelayWrite, IRDelayWriteQuiet,
                 IRGlobalRead, IRIf, IRInputRead, IROutputWrite, IRParamRead,
                 IRParamWrite, IRStaticRead, IRStaticWrite, IRReturn, IRMemRead,
                 IRMemWrite, IRDie>;

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
    std::vector<IRType> return_type;
    std::vector<IRInstr> body;
};

struct IRDelayDecl {
    std::string name;
    size_t size_elements;
    std::string init_fn;
};

struct IRArrayDecl {
    std::string name;
    size_t size_elements;
};

struct IRStaticVar {
    std::string name;
    IRType type;
};

struct IRModule {
    std::string name;
    std::vector<IRFunction> functions;
    std::vector<IRDelayDecl> delays;
    std::vector<IRArrayDecl> static_arrays_decl;
    std::vector<std::string> alloc_order;
    std::vector<IRStaticVar> static_vars;
    std::vector<std::pair<std::string, double>> params;
    std::string init_fn;
    std::string main_fn;
    std::string static_init_fn;
    size_t num_inputs = 0;
    size_t num_outputs = 0;
};

inline auto make_scalar_call(std::string result, std::string callee,
                             std::vector<IRValue> args, IRType result_type)
    -> IRCall {
    return IRCall{.result = {std::move(result)},
                  .callee = std::move(callee),
                  .args = std::move(args),
                  .result_type = {result_type}};
}