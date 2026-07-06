#include "codegen.hpp"

#include "json_writer.hpp"
#include <cmath>
#include <sstream>

namespace {

auto format_number(double value) -> std::string {
    if (value == std::floor(value) && std::abs(value) < 1e15)
        return std::to_string(static_cast<long long>(value));
    std::ostringstream out;
    out.precision(15);
    out << value;
    return out.str();
}

auto node_kind_name(NodeKind kind) -> std::string {
    switch (kind) {
    case NodeKind::State:
        return "state";
    case NodeKind::Fork:
        return "fork";
    case NodeKind::Join:
        return "join";
    case NodeKind::Passthrough:
        return "passthrough";
    }
    return "state";
}

void write_node(const GraphNode &node, std::string &out) {
    out += "{\"id\":" + std::to_string(node.id);
    out += ",\"kind\":" + json_string(node_kind_name(node.kind));
    if (node.kind == NodeKind::State) {
        out += ",\"params\":{";
        bool first = true;
        for (const auto &[name, value] : node.params) {
            if (!first) out += ",";
            first = false;
            out += json_string(name) + ":" + format_number(value);
        }
        out += "}";
    }
    if (node.kind == NodeKind::Join)
        out += ",\"joinArity\":" + std::to_string(node.join_arity);
    out += ",\"next\":[";
    for (size_t i = 0; i < node.next.size(); ++i) {
        if (i != 0) out += ",";
        out += std::to_string(node.next[i]);
    }
    out += "]}";
}

} // namespace

auto graph_to_json(const ExpandedGraph &graph) -> std::string {
    std::string out = R"({"version":1,"nodes":[)";
    for (size_t i = 0; i < graph.nodes.size(); ++i) {
        if (i != 0) out += ",";
        write_node(graph.nodes[i], out);
    }
    out += "],\"entries\":[";
    for (size_t i = 0; i < graph.entries.size(); ++i) {
        if (i != 0) out += ",";
        out += "[";
        for (size_t j = 0; j < graph.entries[i].size(); ++j) {
            if (j != 0) out += ",";
            out += std::to_string(graph.entries[i][j]);
        }
        out += "]";
    }
    out += "]}";
    return out;
}
