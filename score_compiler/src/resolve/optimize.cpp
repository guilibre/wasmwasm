#include "optimize.hpp"

#include <algorithm>
#include <cmath>
#include <cstddef>
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
        if (graph.nodes[id].kind == NodeKind::Join) part[id] = n + id;

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
        if (changed) prune_unreachable(graph);
        const auto cse_changed = structural_cse(graph);
        if (cse_changed) prune_unreachable(graph);
        if (!changed && !cse_changed) break;
    }

    renumber_topological(graph);
}
