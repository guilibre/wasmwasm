#include "codegen.hpp"

#include "json_writer.hpp"
#include <array>
#include <cmath>
#include <sstream>
#include <stdexcept>
#include <variant>

namespace {

auto format_number(double value) -> std::string {
    if (value == std::floor(value) && std::abs(value) < 1e15)
        return std::to_string(static_cast<long long>(value));
    std::ostringstream out;
    out.precision(15);
    out << value;
    return out.str();
}

auto format_rational(const Rational &value) -> std::string {
    if (value.is_integer()) return std::to_string(value.num);
    return "{\"num\":" + std::to_string(value.num) +
           ",\"den\":" + std::to_string(value.den) + "}";
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
    case NodeKind::TransformPush:
        return "transform_push";
    case NodeKind::TransformPop:
        return "transform_pop";
    case NodeKind::Branch:
        return "branch";
    case NodeKind::SignalEmit:
        return "signal_emit";
    }
    return "state";
}

auto expr_to_json(const Expr &expr) -> std::string {
    switch (expr.kind) {
    case Expr::Kind::Number:
        return R"({"kind":"number","value":)" +
               format_rational(expr.number_rational) + "}";
    case Expr::Kind::String:
        return R"({"kind":"string","value":)" + json_string(expr.string_value) +
               "}";
    case Expr::Kind::Null:
        return R"({"kind":"null"})";
    case Expr::Kind::Skip:
        return R"({"kind":"skip"})";
    case Expr::Kind::Ident:
        return R"({"kind":"ident","name":)" + json_string(expr.ident_name) +
               "}";
    case Expr::Kind::Ternary:
        return R"({"kind":"ternary","cond":)" +
               expr_to_json(*expr.ternary_cond) + R"(,"then":)" +
               expr_to_json(*expr.ternary_then) + R"(,"else":)" +
               expr_to_json(*expr.ternary_else) + "}";
    case Expr::Kind::Binary: {
        static constexpr std::array<const char *, 14> kOpNames = {
            "add", "sub", "mul", "div", "mod", "pow", "eq",
            "neq", "lt",  "gt",  "lte", "gte", "and", "or",
        };
        return R"({"kind":"binary","op":)" +
               json_string(kOpNames[static_cast<uint8_t>(expr.op)]) +
               R"(,"lhs":)" + expr_to_json(*expr.lhs) + R"(,"rhs":)" +
               expr_to_json(*expr.rhs) + "}";
    }
    case Expr::Kind::Array:
        throw std::runtime_error("array not allowed in a transform expression");
    }
    throw std::runtime_error("unknown expression kind");
}

auto param_value_to_json(const ExprValue &value) -> std::string {
    if (std::holds_alternative<std::string>(value))
        return json_string(std::get<std::string>(value));
    if (std::holds_alternative<Rational>(value))
        return format_rational(std::get<Rational>(value));
    return format_number(std::get<double>(value));
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
            out += json_string(name) + ":" + param_value_to_json(value);
        }
        out += "}";
    }
    if (node.kind == NodeKind::Join)
        out += ",\"joinArity\":" + std::to_string(node.join_arity);
    if (node.kind == NodeKind::TransformPush) {
        out += ",\"transforms\":[";
        for (size_t i = 0; i < node.transforms.size(); ++i) {
            if (i != 0) out += ",";
            const auto &entry = node.transforms[i];
            out += R"({"paramName":)" + json_string(entry.param_name) +
                   R"(,"expr":)" + expr_to_json(*entry.expr) + "}";
        }
        out += "]";
        if (node.listen_channel)
            out += ",\"listenChannel\":" + json_string(*node.listen_channel);
    }
    if (node.kind == NodeKind::Branch)
        out += ",\"cond\":" + expr_to_json(*node.branch_cond);
    if (node.kind == NodeKind::SignalEmit) {
        out += ",\"signalId\":" + json_string(*node.signal_id);
        out += ",\"params\":{";
        bool first = true;
        for (const auto &[name, value] : node.params) {
            if (!first) out += ",";
            first = false;
            out += json_string(name) + ":" + param_value_to_json(value);
        }
        out += "}";
    }
    out += ",\"next\":[";
    for (size_t i = 0; i < node.next.size(); ++i) {
        if (i != 0) out += ",";
        out += std::to_string(node.next[i]);
    }
    out += "]}";
}

} // namespace

auto graph_to_json(const ExpandedGraph &graph) -> std::string {
    std::string out = R"({"version":1,"scales":[)";
    for (size_t i = 0; i < graph.scales.size(); ++i) {
        if (i != 0) out += ",";
        const auto &[name, values] = graph.scales[i];
        out += R"({"name":)" + json_string(name) + R"(,"values":[)";
        for (size_t j = 0; j < values.size(); ++j) {
            if (j != 0) out += ",";
            out += format_number(values[j]);
        }
        out += "]}";
    }
    out += R"(],"nodes":[)";
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
