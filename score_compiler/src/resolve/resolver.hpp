#pragma once

#include "ast/ast.hpp"
#include <cstdint>
#include <map>
#include <optional>
#include <stdexcept>
#include <string>
#include <unordered_map>
#include <vector>

enum class NodeKind : uint8_t {
    State,
    Fork,
    Join,
    Passthrough,
    TransformPush,
    TransformPop,
    Branch
};

struct TransformEntry {
    std::string param_name;
    const Expr *expr = nullptr;
};

struct GraphNode {
    size_t id = 0;
    NodeKind kind = NodeKind::State;
    std::map<std::string, double> params;
    std::optional<std::string> instrument;
    size_t join_arity = 0;
    std::vector<size_t> next;
    std::vector<TransformEntry> transforms;
    std::optional<std::string> push_instrument;
    const Expr *branch_cond = nullptr;
};

struct ExpandedGraph {
    std::vector<GraphNode> nodes;
    std::vector<std::vector<size_t>> entries;
    std::vector<std::pair<std::string, std::vector<double>>> scales;
    std::vector<std::unique_ptr<Expr>> owned_exprs;
    std::unordered_map<size_t, size_t> transform_pop_of_push;
};

class ResolveException : public std::runtime_error {
  public:
    ResolveException(const std::string &msg, size_t line, size_t col)
        : std::runtime_error(msg), line(line), col(col) {}

    size_t line;
    size_t col;
};

[[nodiscard]] auto expand_program(const Program &program) -> ExpandedGraph;

auto prune_unreachable(ExpandedGraph &graph) -> void;
