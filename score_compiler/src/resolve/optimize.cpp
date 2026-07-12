#include "optimize.hpp"

#include <algorithm>
#include <cmath>
#include <cstddef>
#include <functional>
#include <string>
#include <unordered_map>
#include <vector>

namespace {

auto for_each_edge(ExpandedGraph &graph, const auto &visit_edge) -> void {
    for (auto &node : graph.nodes)
        for (auto &succ : node.next) visit_edge(succ);
    for (auto &machine : graph.entries)
        for (auto &id : machine) visit_edge(id);
}

auto is_transparent(const GraphNode &node) -> bool {
    if (node.next.size() != 1) return false;
    if (node.kind == NodeKind::Passthrough) return true;
    if (node.kind == NodeKind::Fork) return true;
    if (node.kind == NodeKind::Join && node.join_arity == 1) return true;
    if (node.kind == NodeKind::TransformPush && node.transforms.empty() &&
        !node.push_instrument && !node.listen_channel)
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

auto fold_constant_expr(const Expr &expr) -> std::optional<double> {
    switch (expr.kind) {
    case Expr::Kind::Number:
        return expr.number;
    case Expr::Kind::Binary: {
        const auto lhs = fold_constant_expr(*expr.lhs);
        const auto rhs = fold_constant_expr(*expr.rhs);
        if (!lhs || !rhs) return std::nullopt;
        switch (expr.op) {
        case BinOp::Add:
            return *lhs + *rhs;
        case BinOp::Sub:
            return *lhs - *rhs;
        case BinOp::Mul:
            return *lhs * *rhs;
        case BinOp::Div:
            if (*rhs == 0) return std::nullopt;
            return *lhs / *rhs;
        case BinOp::Mod:
            if (*rhs == 0) return std::nullopt;
            return std::fmod(*lhs, *rhs);
        case BinOp::Pow:
            return std::pow(*lhs, *rhs);
        case BinOp::Eq:
            return *lhs == *rhs ? 1.0 : 0.0;
        case BinOp::NotEq:
            return *lhs != *rhs ? 1.0 : 0.0;
        case BinOp::Lt:
            return *lhs < *rhs ? 1.0 : 0.0;
        case BinOp::Gt:
            return *lhs > *rhs ? 1.0 : 0.0;
        case BinOp::LtEq:
            return *lhs <= *rhs ? 1.0 : 0.0;
        case BinOp::GtEq:
            return *lhs >= *rhs ? 1.0 : 0.0;
        case BinOp::And:
            return (*lhs != 0 && *rhs != 0) ? 1.0 : 0.0;
        case BinOp::Or:
            return (*lhs != 0 || *rhs != 0) ? 1.0 : 0.0;
        }
        return std::nullopt;
    }
    case Expr::Kind::Ternary: {
        const auto cond = fold_constant_expr(*expr.ternary_cond);
        if (!cond) return std::nullopt;
        return fold_constant_expr(*cond != 0 ? *expr.ternary_then
                                             : *expr.ternary_else);
    }
    case Expr::Kind::Ident:
    case Expr::Kind::Array:
    case Expr::Kind::Null:
        return std::nullopt;
    }
    return std::nullopt;
}

auto fold_expr_with_params(const Expr &expr,
                           const std::map<std::string, double> &params)
    -> std::optional<double> {
    switch (expr.kind) {
    case Expr::Kind::Number:
        return expr.number;
    case Expr::Kind::Ident: {
        const auto it = params.find(expr.ident_name);
        if (it == params.end()) return std::nullopt;
        return it->second;
    }
    case Expr::Kind::Binary: {
        const auto lhs = fold_expr_with_params(*expr.lhs, params);
        const auto rhs = fold_expr_with_params(*expr.rhs, params);
        if (!lhs || !rhs) return std::nullopt;
        switch (expr.op) {
        case BinOp::Add:
            return *lhs + *rhs;
        case BinOp::Sub:
            return *lhs - *rhs;
        case BinOp::Mul:
            return *lhs * *rhs;
        case BinOp::Div:
            if (*rhs == 0) return std::nullopt;
            return *lhs / *rhs;
        case BinOp::Mod:
            if (*rhs == 0) return std::nullopt;
            return std::fmod(*lhs, *rhs);
        case BinOp::Pow:
            return std::pow(*lhs, *rhs);
        case BinOp::Eq:
            return *lhs == *rhs ? 1.0 : 0.0;
        case BinOp::NotEq:
            return *lhs != *rhs ? 1.0 : 0.0;
        case BinOp::Lt:
            return *lhs < *rhs ? 1.0 : 0.0;
        case BinOp::Gt:
            return *lhs > *rhs ? 1.0 : 0.0;
        case BinOp::LtEq:
            return *lhs <= *rhs ? 1.0 : 0.0;
        case BinOp::GtEq:
            return *lhs >= *rhs ? 1.0 : 0.0;
        case BinOp::And:
            return (*lhs != 0 && *rhs != 0) ? 1.0 : 0.0;
        case BinOp::Or:
            return (*lhs != 0 || *rhs != 0) ? 1.0 : 0.0;
        }
        return std::nullopt;
    }
    case Expr::Kind::Ternary: {
        const auto cond = [&]() -> std::optional<bool> {
            if (expr.ternary_cond->kind == Expr::Kind::Ident)
                return params.contains(expr.ternary_cond->ident_name);
            const auto cond_value =
                fold_expr_with_params(*expr.ternary_cond, params);
            if (!cond_value) return std::nullopt;
            return *cond_value != 0;
        }();
        if (!cond) return std::nullopt;
        return fold_expr_with_params(
            *cond ? *expr.ternary_then : *expr.ternary_else, params);
    }
    case Expr::Kind::Array:
    case Expr::Kind::Null:
        return std::nullopt;
    }
    return std::nullopt;
}

struct TransformScope {
    std::vector<size_t> states;
    std::vector<size_t> pops;
    bool has_cycle = false;
};

auto shadows(const GraphNode &node, const std::vector<std::string> &param_names,
             bool folding_instrument) -> bool {
    if (folding_instrument && node.push_instrument) return true;
    return std::ranges::any_of(node.transforms, [&](const auto &entry) -> bool {
        return std::ranges::find(param_names, entry.param_name) !=
               param_names.end();
    });
}

auto find_scope(const ExpandedGraph &graph, size_t push_id,
                const std::vector<std::string> &param_names,
                bool folding_instrument) -> std::optional<TransformScope> {
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
            if (shadows(node, param_names, folding_instrument)) {
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
        if (push_node.transforms.empty() && !push_node.push_instrument)
            continue;

        std::vector<std::string> param_names;
        param_names.reserve(push_node.transforms.size());
        for (const auto &entry : push_node.transforms)
            param_names.push_back(entry.param_name);
        const auto scope = find_scope(graph, push_id, param_names,
                                      push_node.push_instrument.has_value());
        if (!scope) continue;

        std::vector<TransformEntry> remaining;
        for (const auto &entry : push_node.transforms) {
            if (const auto value = fold_constant_expr(*entry.expr)) {
                for (auto state_id : scope->states)
                    graph.nodes[state_id].params[entry.param_name] = *value;
                changed = true;
                continue;
            }

            if (scope->has_cycle) {
                remaining.push_back(entry);
                continue;
            }

            std::vector<std::pair<size_t, double>> per_state_values;
            per_state_values.reserve(scope->states.size());
            auto foldable_per_state = true;
            for (auto state_id : scope->states) {
                const auto value = fold_expr_with_params(
                    *entry.expr, graph.nodes[state_id].params);
                if (!value) {
                    foldable_per_state = false;
                    break;
                }
                per_state_values.emplace_back(state_id, *value);
            }
            if (!foldable_per_state) {
                remaining.push_back(entry);
                continue;
            }
            for (const auto &[state_id, value] : per_state_values)
                graph.nodes[state_id].params[entry.param_name] = value;
            changed = true;
        }

        if (push_node.push_instrument) {
            for (auto state_id : scope->states)
                graph.nodes[state_id].instrument = push_node.push_instrument;
            changed = true;
        }

        push_node.transforms = std::move(remaining);
        push_node.push_instrument.reset();
        if (push_node.transforms.empty())
            for (auto pop_id : scope->pops)
                graph.nodes[pop_id].kind = NodeKind::Passthrough;
    }
    return changed;
}

struct NodeSignature {
    NodeKind kind;
    std::map<std::string, double> params;
    std::optional<std::string> instrument;
    size_t join_arity;
    std::optional<std::string> push_instrument;
    std::vector<std::pair<std::string, double>> transforms;

    auto operator==(const NodeSignature &) const -> bool = default;
};

auto make_signature(const GraphNode &node) -> NodeSignature {
    NodeSignature sig{
        .kind = node.kind,
        .params = node.params,
        .instrument = node.instrument,
        .join_arity = node.join_arity,
        .push_instrument = node.push_instrument,
        .transforms = {},
    };
    for (const auto &t : node.transforms)
        sig.transforms.emplace_back(t.param_name,
                                    (t.expr != nullptr) &&
                                            t.expr->kind == Expr::Kind::Number
                                        ? t.expr->number
                                        : std::nan(""));
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
            graph.nodes[id].kind == NodeKind::SignalEmit)
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
    for (auto &node : reordered)
        for (auto &succ : node.next) succ = new_id[succ];
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
