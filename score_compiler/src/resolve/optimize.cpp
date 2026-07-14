#include "optimize.hpp"

#include "ast/binop_eval.hpp"

#include <algorithm>
#include <cmath>
#include <cstddef>
#include <functional>
#include <optional>
#include <string>
#include <unordered_map>
#include <variant>
#include <vector>

namespace {

auto for_each_edge(ExpandedGraph &graph, const auto &visit_edge) -> void {
    for (auto &node : graph.nodes) {
        for (auto &succ : node.next) visit_edge(succ);
        if (node.kind == NodeKind::Reverse) {
            visit_edge(node.reverse_body_entry_id);
            visit_edge(node.reverse_body_exit_id);
        }
    }
    for (auto &machine : graph.entries)
        for (auto &id : machine) visit_edge(id);
}

auto is_transparent(const GraphNode &node) -> bool {
    if (node.next.size() != 1) return false;
    if (node.kind == NodeKind::Passthrough) return true;
    if (node.kind == NodeKind::Fork) return true;
    if (node.kind == NodeKind::Join && node.join_arity == 1) return true;
    if (node.kind == NodeKind::TransformPush && node.transforms.empty() &&
        !node.listen_channel)
        return true;
    return false;
}

auto bypass_transparent_nodes(ExpandedGraph &graph) -> bool {
    std::vector<size_t> final_target(graph.nodes.size());
    for (size_t id = 0; id < graph.nodes.size(); ++id) final_target[id] = id;

    for (size_t id = 0; id < graph.nodes.size(); ++id) {
        if (!is_transparent(graph.nodes[id])) continue;
        std::vector<size_t> visited;
        size_t cur = id;
        while (is_transparent(graph.nodes[cur])) {
            if (std::ranges::find(visited, cur) != visited.end()) break;
            visited.push_back(cur);
            cur = graph.nodes[cur].next[0];
        }
        for (auto v : visited) final_target[v] = cur;
    }

    bool changed = false;
    for_each_edge(graph, [&](size_t &edge) -> void {
        if (final_target[edge] != edge) {
            edge = final_target[edge];
            changed = true;
        }
    });
    return changed;
}

auto merge_empty_transform_frames(ExpandedGraph &graph) -> bool {
    bool changed = false;
    for_each_edge(graph, [&](size_t &edge) -> void {
        while (graph.nodes[edge].kind == NodeKind::TransformPush &&
               graph.nodes[edge].next.size() == 1) {
            const auto pop_id = graph.nodes[edge].next[0];
            if (graph.nodes[pop_id].kind != NodeKind::TransformPop) break;
            if (graph.nodes[pop_id].next.size() != 1) break;
            edge = graph.nodes[pop_id].next[0];
            changed = true;
        }
    });
    return changed;
}

auto fold_deterministic_and(double lhs, double rhs) -> std::optional<double> {
    const auto lhs_clip = std::clamp(lhs, 0.0, 1.0);
    const auto rhs_clip = std::clamp(rhs, 0.0, 1.0);
    const auto result = lhs_clip * rhs_clip;
    if (result == 0.0) return 0.0;
    if (result == 1.0) return 1.0;
    return std::nullopt;
}

auto fold_deterministic_or(double lhs, double rhs) -> std::optional<double> {
    const auto lhs_clip = std::clamp(lhs, 0.0, 1.0);
    const auto rhs_clip = std::clamp(rhs, 0.0, 1.0);
    const auto result = lhs_clip + rhs_clip - (lhs_clip * rhs_clip);
    if (result == 0.0) return 0.0;
    if (result == 1.0) return 1.0;
    return std::nullopt;
}

struct NullExpr {
    auto operator==(const NullExpr &) const -> bool = default;
};

struct SkipExpr {
    auto operator==(const SkipExpr &) const -> bool = default;
};

using FoldValue = std::variant<ExprValue, NullExpr, SkipExpr>;
using FoldResult = std::optional<FoldValue>;

auto fold_binary_values(BinOp op, const ExprValue &lhs, const ExprValue &rhs)
    -> std::optional<ExprValue> {
    if (op == BinOp::And) {
        const auto result =
            fold_deterministic_and(to_double(lhs), to_double(rhs));
        if (!result) return std::nullopt;
        return *result;
    }
    if (op == BinOp::Or) {
        const auto result =
            fold_deterministic_or(to_double(lhs), to_double(rhs));
        if (!result) return std::nullopt;
        return *result;
    }
    return apply_binary_op(op, lhs, rhs);
}

auto fold_result_as_double(const FoldResult &result) -> std::optional<double> {
    if (!result || !std::holds_alternative<ExprValue>(*result))
        return std::nullopt;
    const auto &value = std::get<ExprValue>(*result);
    if (std::holds_alternative<std::string>(value)) return std::nullopt;
    return to_double(value);
}

auto fold_constant_expr(const Expr &expr) -> FoldResult {
    switch (expr.kind) {
    case Expr::Kind::Number:
        return FoldValue{std::in_place_type<ExprValue>, expr.number_rational};
    case Expr::Kind::String:
        return FoldValue{std::in_place_type<ExprValue>, expr.string_value};
    case Expr::Kind::Null:
        return FoldValue{NullExpr{}};
    case Expr::Kind::Skip:
        return FoldValue{SkipExpr{}};
    case Expr::Kind::Binary: {
        const auto lhs = fold_constant_expr(*expr.lhs);
        const auto rhs = fold_constant_expr(*expr.rhs);
        if (!lhs || !rhs || !std::holds_alternative<ExprValue>(*lhs) ||
            !std::holds_alternative<ExprValue>(*rhs))
            return std::nullopt;
        const auto result = fold_binary_values(
            expr.op, std::get<ExprValue>(*lhs), std::get<ExprValue>(*rhs));
        if (!result) return std::nullopt;
        return FoldValue{std::in_place_type<ExprValue>, *result};
    }
    case Expr::Kind::Ternary: {
        const auto cond =
            fold_result_as_double(fold_constant_expr(*expr.ternary_cond));
        if (!cond) return std::nullopt;
        return fold_constant_expr(*cond != 0 ? *expr.ternary_then
                                             : *expr.ternary_else);
    }
    case Expr::Kind::Ident:
    case Expr::Kind::Array:
        return std::nullopt;
    }
    return std::nullopt;
}

auto fold_expr_with_params(const Expr &expr,
                           const std::map<std::string, ExprValue> &params)
    -> FoldResult {
    switch (expr.kind) {
    case Expr::Kind::Number:
        return FoldValue{std::in_place_type<ExprValue>, expr.number_rational};
    case Expr::Kind::String:
        return FoldValue{std::in_place_type<ExprValue>, expr.string_value};
    case Expr::Kind::Null:
        return FoldValue{NullExpr{}};
    case Expr::Kind::Skip:
        return FoldValue{SkipExpr{}};
    case Expr::Kind::Ident: {
        const auto it = params.find(expr.ident_name);
        if (it == params.end()) return std::nullopt;
        return FoldValue{std::in_place_type<ExprValue>, it->second};
    }
    case Expr::Kind::Binary: {
        const auto lhs = fold_expr_with_params(*expr.lhs, params);
        const auto rhs = fold_expr_with_params(*expr.rhs, params);
        if (!lhs || !rhs || !std::holds_alternative<ExprValue>(*lhs) ||
            !std::holds_alternative<ExprValue>(*rhs))
            return std::nullopt;
        const auto result = fold_binary_values(
            expr.op, std::get<ExprValue>(*lhs), std::get<ExprValue>(*rhs));
        if (!result) return std::nullopt;
        return FoldValue{std::in_place_type<ExprValue>, *result};
    }
    case Expr::Kind::Ternary: {
        const auto cond = [&]() -> std::optional<bool> {
            if (expr.ternary_cond->kind == Expr::Kind::Ident)
                return params.contains(expr.ternary_cond->ident_name);
            const auto cond_value = fold_result_as_double(
                fold_expr_with_params(*expr.ternary_cond, params));
            if (!cond_value) return std::nullopt;
            return *cond_value != 0;
        }();
        if (!cond) return std::nullopt;
        return fold_expr_with_params(
            *cond ? *expr.ternary_then : *expr.ternary_else, params);
    }
    case Expr::Kind::Array:
        return std::nullopt;
    }
    return std::nullopt;
}

struct TransformScope {
    std::vector<size_t> states;
    std::vector<size_t> pops;
    bool has_cycle = false;
};

auto shadows(const GraphNode &node, const std::vector<std::string> &param_names)
    -> bool {
    return std::ranges::any_of(node.transforms, [&](const auto &entry) -> bool {
        return std::ranges::find(param_names, entry.param_name) !=
               param_names.end();
    });
}

auto find_scope(const ExpandedGraph &graph, size_t push_id,
                const std::vector<std::string> &param_names)
    -> std::optional<TransformScope> {
    std::optional<size_t> own_pop_id;
    if (const auto it = graph.transform_pop_of_push.find(push_id);
        it != graph.transform_pop_of_push.end())
        own_pop_id = it->second;

    TransformScope scope;
    enum class Color : uint8_t { White, Grey, Black };
    std::vector<Color> color(graph.nodes.size(), Color::White);
    color[push_id] = Color::Grey;
    bool aborted = false;

    std::function<void(size_t)> visit = [&](size_t id) -> void {
        if (aborted) return;
        const auto &node = graph.nodes[id];
        if (node.kind == NodeKind::TransformPop) {
            if (own_pop_id && id == *own_pop_id) {
                scope.pops.push_back(id);
                return;
            }
        } else if (node.kind == NodeKind::TransformPush) {
            if (shadows(node, param_names)) {
                aborted = true;
                return;
            }
        } else if (node.kind == NodeKind::State) {
            scope.states.push_back(id);
        }
        for (auto succ : node.next) {
            if (aborted) return;
            if (color[succ] == Color::Grey) {
                scope.has_cycle = true;
                continue;
            }
            if (color[succ] == Color::Black) continue;
            color[succ] = Color::Grey;
            visit(succ);
            color[succ] = Color::Black;
        }
    };

    for (auto succ : graph.nodes[push_id].next) {
        if (color[succ] == Color::Grey) {
            scope.has_cycle = true;
            continue;
        }
        if (color[succ] == Color::Black) continue;
        color[succ] = Color::Grey;
        visit(succ);
        color[succ] = Color::Black;
    }
    color[push_id] = Color::Black;

    if (aborted) return std::nullopt;
    return scope;
}

auto fold_transforms_into_state(ExpandedGraph &graph) -> bool {
    bool changed = false;
    for (size_t push_id = 0; push_id < graph.nodes.size(); ++push_id) {
        auto &push_node = graph.nodes[push_id];
        if (push_node.kind != NodeKind::TransformPush) continue;
        if (push_node.transforms.empty()) continue;

        std::vector<std::string> param_names;
        param_names.reserve(push_node.transforms.size());
        for (const auto &entry : push_node.transforms)
            param_names.push_back(entry.param_name);
        const auto scope = find_scope(graph, push_id, param_names);
        if (!scope) continue;

        std::vector<TransformEntry> remaining;
        for (const auto &entry : push_node.transforms) {
            if (const auto result = fold_constant_expr(*entry.expr)) {
                for (auto state_id : scope->states) {
                    if (std::holds_alternative<ExprValue>(*result))
                        graph.nodes[state_id].params[entry.param_name] =
                            std::get<ExprValue>(*result);
                    else if (std::holds_alternative<NullExpr>(*result))
                        graph.nodes[state_id].params.erase(entry.param_name);
                }
                changed = true;
                continue;
            }

            if (scope->has_cycle) {
                remaining.push_back(entry);
                continue;
            }

            std::vector<std::pair<size_t, FoldValue>> per_state_values;
            per_state_values.reserve(scope->states.size());
            auto foldable_per_state = true;
            for (auto state_id : scope->states) {
                const auto result = fold_expr_with_params(
                    *entry.expr, graph.nodes[state_id].params);
                if (!result) {
                    foldable_per_state = false;
                    break;
                }
                per_state_values.emplace_back(state_id, *result);
            }
            if (!foldable_per_state) {
                remaining.push_back(entry);
                continue;
            }
            for (const auto &[state_id, result] : per_state_values) {
                if (std::holds_alternative<ExprValue>(result))
                    graph.nodes[state_id].params[entry.param_name] =
                        std::get<ExprValue>(result);
                else if (std::holds_alternative<NullExpr>(result))
                    graph.nodes[state_id].params.erase(entry.param_name);
            }
            changed = true;
        }

        push_node.transforms = std::move(remaining);
        if (push_node.transforms.empty())
            for (auto pop_id : scope->pops)
                graph.nodes[pop_id].kind = NodeKind::Passthrough;
    }
    return changed;
}

struct NodeSignature {
    NodeKind kind;
    std::map<std::string, ExprValue> params;
    size_t join_arity;
    std::vector<std::pair<std::string, ExprValue>> transforms;

    auto operator==(const NodeSignature &) const -> bool = default;
};

auto make_signature(const GraphNode &node) -> NodeSignature {
    NodeSignature sig{
        .kind = node.kind,
        .params = node.params,
        .join_arity = node.join_arity,
        .transforms = {},
    };
    for (const auto &t : node.transforms) {
        if (t.expr != nullptr && t.expr->kind == Expr::Kind::Number)
            sig.transforms.emplace_back(t.param_name, t.expr->number_rational);
        else if (t.expr != nullptr && t.expr->kind == Expr::Kind::String)
            sig.transforms.emplace_back(t.param_name, t.expr->string_value);
        else
            sig.transforms.emplace_back(t.param_name, std::nan(""));
    }
    return sig;
}

auto structural_cse(ExpandedGraph &graph) -> bool {
    const auto n = graph.nodes.size();
    if (n == 0) return false;

    std::vector<size_t> part(n, 0);
    {
        std::unordered_map<size_t, size_t> sig_to_part;
        std::vector<NodeSignature> sigs;
        sigs.reserve(n);
        for (const auto &node : graph.nodes)
            sigs.push_back(make_signature(node));
        for (size_t id = 0; id < n; ++id) {
            auto placed = false;
            for (const auto &[existing_id, existing_part] : sig_to_part) {
                if (sigs[existing_id] == sigs[id]) {
                    part[id] = existing_part;
                    placed = true;
                    break;
                }
            }
            if (!placed) {
                const auto new_part = sig_to_part.size();
                sig_to_part[id] = new_part;
                part[id] = new_part;
            }
        }
    }

    for (size_t round = 0; round < n + 1; ++round) {
        std::vector<std::vector<size_t>> refined_key(n);
        for (size_t id = 0; id < n; ++id) {
            refined_key[id].push_back(part[id]);
            for (auto succ : graph.nodes[id].next)
                refined_key[id].push_back(part[succ]);
        }
        std::unordered_map<std::string, size_t> key_to_part;
        std::vector<size_t> new_part(n);
        auto changed = false;
        for (size_t id = 0; id < n; ++id) {
            std::string key;
            for (auto v : refined_key[id]) key += std::to_string(v) + ",";
            auto [it, inserted] =
                key_to_part.try_emplace(key, key_to_part.size());
            new_part[id] = it->second;
            if (new_part[id] != part[id]) changed = true;
        }
        part = std::move(new_part);
        if (!changed) break;
    }

    for (size_t id = 0; id < n; ++id)
        if (graph.nodes[id].kind == NodeKind::Join ||
            graph.nodes[id].kind == NodeKind::TransformPush ||
            graph.nodes[id].kind == NodeKind::TransformPop ||
            graph.nodes[id].kind == NodeKind::Branch ||
            graph.nodes[id].kind == NodeKind::SignalEmit ||
            graph.nodes[id].kind == NodeKind::Reverse ||
            graph.nodes[id].kind == NodeKind::Legato)
            part[id] = n + id;

    std::unordered_map<size_t, size_t> representative;
    for (size_t id = 0; id < n; ++id) representative.try_emplace(part[id], id);

    auto merged = false;
    std::vector<size_t> remap(n);
    for (size_t id = 0; id < n; ++id) {
        remap[id] = representative[part[id]];
        if (remap[id] != id) merged = true;
    }
    if (!merged) return false;

    for_each_edge(graph, [&](size_t &edge) -> void { edge = remap[edge]; });

    std::unordered_map<size_t, size_t> new_pop_of_push;
    for (const auto &[push_id, pop_id] : graph.transform_pop_of_push)
        new_pop_of_push[remap[push_id]] = remap[pop_id];
    graph.transform_pop_of_push = std::move(new_pop_of_push);

    return true;
}

auto renumber_topological(ExpandedGraph &graph) -> void {
    std::vector<size_t> order;
    std::vector<bool> visited(graph.nodes.size(), false);
    std::vector<size_t> queue;
    for (const auto &machine : graph.entries)
        for (auto id : machine)
            if (!visited[id]) {
                visited[id] = true;
                queue.push_back(id);
            }
    for (size_t head = 0; head < queue.size(); ++head) {
        const auto id = queue[head];
        order.push_back(id);
        for (auto succ : graph.nodes[id].next)
            if (!visited[succ]) {
                visited[succ] = true;
                queue.push_back(succ);
            }
        if (graph.nodes[id].kind == NodeKind::Reverse &&
            !visited[graph.nodes[id].reverse_body_entry_id]) {
            visited[graph.nodes[id].reverse_body_entry_id] = true;
            queue.push_back(graph.nodes[id].reverse_body_entry_id);
        }
    }
    for (size_t id = 0; id < graph.nodes.size(); ++id)
        if (!visited[id]) order.push_back(id);

    std::unordered_map<size_t, size_t> new_id;
    for (size_t i = 0; i < order.size(); ++i) new_id[order[i]] = i;

    std::vector<GraphNode> reordered(graph.nodes.size());
    for (size_t i = 0; i < order.size(); ++i) {
        reordered[i] = std::move(graph.nodes[order[i]]);
        reordered[i].id = i;
    }
    for (auto &node : reordered) {
        for (auto &succ : node.next) succ = new_id[succ];
        if (node.kind == NodeKind::Reverse) {
            node.reverse_body_entry_id = new_id[node.reverse_body_entry_id];
            node.reverse_body_exit_id = new_id[node.reverse_body_exit_id];
        }
    }
    for (auto &machine : graph.entries)
        for (auto &id : machine) id = new_id[id];

    graph.nodes = std::move(reordered);
}

} // namespace

auto optimize_graph(ExpandedGraph &graph) -> void {
    prune_unreachable(graph);

    constexpr size_t kMaxRounds = 32;
    for (size_t round = 0; round < kMaxRounds; ++round) {
        auto changed = false;
        changed |= bypass_transparent_nodes(graph);
        changed |= merge_empty_transform_frames(graph);
        changed |= fold_transforms_into_state(graph);
        if (changed) prune_unreachable(graph);
        const auto cse_changed = structural_cse(graph);
        if (cse_changed) prune_unreachable(graph);
        if (!changed && !cse_changed) break;
    }

    renumber_topological(graph);
}
