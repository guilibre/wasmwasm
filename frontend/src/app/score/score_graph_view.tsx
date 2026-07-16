import { useCallback, useEffect, useState } from 'react';
import {
    ReactFlow,
    ReactFlowProvider,
    Controls,
    MarkerType,
    useReactFlow,
    useNodesState,
} from '@xyflow/react';
import type { Node, Edge } from '@xyflow/react';
import '@xyflow/react/dist/style.css';
import ELK from 'elkjs/lib/elk.bundled.js';
import ScoreWasm from '../../scorewasm/compiler';
import type { ScoreGraph, GraphNode } from '../../audio/conductor';
import { ScorePianoRoll } from './score_piano_roll';
import { ScoreGraphNode, ScoreGraphPlayNode, FloatingEdge } from './score_graph_nodes';
import './score_graph_view.scss';

interface Props {
    source: string;
    bpm: number;
}

const NODE_TYPES = {
    score_node: ScoreGraphNode,
    play_node: ScoreGraphPlayNode,
};

const EDGE_TYPES = {
    floating: FloatingEdge,
};

async function layout_nodes(nodes: Node[], edges: Edge[]): Promise<Node[]> {
    const elk = new ELK();
    const graph = {
        id: 'root',
        layoutOptions: {
            'elk.algorithm': 'layered',
            'elk.direction': 'DOWN',
            'elk.layered.crossingMinimization.strategy': 'LAYER_SWEEP',
            'elk.layered.nodePlacement.strategy': 'BRANDES_KOEPF',
            'elk.spacing.nodeNode': '10',
        },
        children: nodes.map((n) => ({
            id: n.id,
            width: n.measured?.width ?? n.width ?? 140,
            height: n.measured?.height ?? n.height ?? 60,
        })),
        edges: edges.map((e) => ({
            id: e.id,
            sources: [e.source],
            targets: [e.target],
        })),
    };
    const laid_out = await elk.layout(graph);
    return nodes.map((n) => {
        const child = laid_out.children?.find((c) => c.id === n.id);
        return child ? { ...n, position: { x: child.x ?? 0, y: child.y ?? 0 } } : n;
    });
}

function body_entry_of(node: GraphNode): number | undefined {
    if (node.kind === 'reverse') return node.reverseBodyEntryId;
    if (node.kind === 'repeat') return node.repeatBodyEntryId;
    return undefined;
}

function body_exit_of(node: GraphNode): number | undefined {
    if (node.kind === 'reverse') return node.reverseBodyExitId;
    if (node.kind === 'repeat') return node.repeatBodyExitId;
    return undefined;
}

function is_loop_marker(node: GraphNode): boolean {
    return body_entry_of(node) !== undefined;
}

function is_absorbing_marker(node: GraphNode): boolean {
    return is_loop_marker(node) || node.kind === 'skip';
}

function is_transparent_link(node: GraphNode): boolean {
    return (
        (node.kind === 'legato' || node.kind === 'passthrough' || node.kind === 'transform_pop') &&
        node.next.length === 1
    );
}

function is_dead_end_passthrough(node: GraphNode): boolean {
    return node.kind === 'passthrough' && node.next.length === 0;
}

function resolve_visual_target(
    nodes_by_id: Map<number, GraphNode>,
    start_id: number,
): number | undefined {
    let id = start_id;
    const visited = new Set<number>();
    while (!visited.has(id)) {
        visited.add(id);
        const node = nodes_by_id.get(id);
        if (!node) return id;
        if (is_dead_end_passthrough(node)) return undefined;
        if (!is_transparent_link(node)) return id;
        id = node.next[0];
    }
    return id;
}

function resolve_all(nodes_by_id: Map<number, GraphNode>, ids: number[]): number[] {
    return ids
        .map((id) => resolve_visual_target(nodes_by_id, id))
        .filter((id): id is number => id !== undefined);
}

function collect_loop_body(
    nodes_by_id: Map<number, GraphNode>,
    body_entry_id: number,
    exit_id: number,
): number[] {
    const members: number[] = [];
    const visited = new Set<number>();
    const stack = [body_entry_id];
    while (stack.length > 0) {
        const id = stack.pop()!;
        if ((id === exit_id && id !== body_entry_id) || visited.has(id)) continue;
        visited.add(id);
        const node = nodes_by_id.get(id);
        if (!node) continue;
        members.push(id);
        if (id !== body_entry_id && is_loop_marker(node)) continue;
        stack.push(...node.next);
    }
    return members;
}

interface SkipRepeatFrame {
    repeat_id: number;
    remaining: number;
}

function skip_loop_back(
    nodes_by_id: Map<number, GraphNode>,
    repeat_stack: SkipRepeatFrame[],
    node_id: number,
): number[] | undefined {
    const top = repeat_stack[repeat_stack.length - 1];
    if (top === undefined) return undefined;
    const repeat_node = nodes_by_id.get(top.repeat_id);
    if (!repeat_node || body_exit_of(repeat_node) !== node_id) return undefined;
    repeat_stack.pop();
    if (top.remaining > 0) {
        repeat_stack.push({ repeat_id: top.repeat_id, remaining: top.remaining - 1 });
        return [body_entry_of(repeat_node)!];
    }
    return repeat_node.next;
}

function collect_skipped_states(
    nodes_by_id: Map<number, GraphNode>,
    start_id: number,
    skip_count: number,
): { members: number[]; continuation?: number[] } {
    const members: number[] = [];
    let remaining = skip_count;
    let id = start_id;
    const repeat_stack: SkipRepeatFrame[] = [];
    const visited = new Set<number>();
    for (;;) {
        if (visited.has(id) || id === undefined) return { members };
        visited.add(id);
        const node = nodes_by_id.get(id);
        if (!node) return { members };
        members.push(id);

        const entry_id = body_entry_of(node);
        if (entry_id !== undefined && node.repeatCount !== undefined) {
            repeat_stack.push({ repeat_id: node.id, remaining: node.repeatCount - 1 });
            id = entry_id;
            continue;
        }

        if (node.kind === 'state') {
            remaining--;
            if (remaining <= 0) {
                const looped_after = skip_loop_back(nodes_by_id, repeat_stack, node.id);
                if (looped_after !== undefined) return { members, continuation: looped_after };
                return { members, continuation: node.next };
            }
        }

        const looped = skip_loop_back(nodes_by_id, repeat_stack, node.id);
        if (looped !== undefined) {
            if (looped.length !== 1) return { members, continuation: looped };
            id = looped[0];
            continue;
        }

        if (node.next.length !== 1) return { members, continuation: node.next };
        id = node.next[0];
    }
}

function compact_flow_graph(full_graph: ScoreGraph): { nodes: Node[]; edges: Edge[] } {
    const nodes_by_id_raw = new Map(full_graph.nodes.map((n) => [n.id, n]));

    const absorbed_by: Map<number, number[]> = new Map();
    const add_owner = (member_id: number, owner_id: number): void => {
        const owners = absorbed_by.get(member_id) ?? [];
        if (!owners.includes(owner_id)) owners.push(owner_id);
        absorbed_by.set(member_id, owners);
    };
    for (const n of full_graph.nodes) {
        const entry_id = body_entry_of(n);
        const exit_id = body_exit_of(n);
        if (entry_id === undefined || exit_id === undefined) continue;
        for (const member_id of collect_loop_body(nodes_by_id_raw, entry_id, exit_id))
            add_owner(member_id, n.id);
    }

    const skip_continuation: Map<number, number[]> = new Map();
    for (const n of full_graph.nodes) {
        if (n.kind !== 'skip' || n.next.length !== 1 || n.skipCount === undefined) continue;
        const { members, continuation } = collect_skipped_states(
            nodes_by_id_raw,
            n.next[0],
            n.skipCount,
        );
        for (const member_id of members) add_owner(member_id, n.id);
        if (continuation !== undefined) skip_continuation.set(n.id, continuation);
    }

    const graph: ScoreGraph = {
        ...full_graph,
        nodes: full_graph.nodes
            .filter(
                (n) =>
                    !is_transparent_link(n) &&
                    !is_dead_end_passthrough(n) &&
                    !absorbed_by.has(n.id),
            )
            .map((n) => ({
                ...n,
                next:
                    n.kind === 'skip' && skip_continuation.has(n.id)
                        ? resolve_all(nodes_by_id_raw, skip_continuation.get(n.id)!)
                        : resolve_all(nodes_by_id_raw, n.next),
            })),
    };
    const nodes_by_id = new Map(graph.nodes.map((n) => [n.id, n]));
    const in_degree = new Map<number, number>();
    for (const n of graph.nodes)
        for (const target of n.next) in_degree.set(target, (in_degree.get(target) ?? 0) + 1);

    const has_real_transform = (node: GraphNode): boolean =>
        node.kind !== 'state' &&
        node.kind !== 'join' &&
        node.kind !== 'fork' &&
        (node.transforms?.length ?? 0) > 0;

    const fork_has_real_transform = new Map<number, boolean>();
    const fork_contains_real_transform = (fork_id: number): boolean => {
        const cached = fork_has_real_transform.get(fork_id);
        if (cached !== undefined) return cached;
        const fork = nodes_by_id.get(fork_id);
        fork_has_real_transform.set(fork_id, false);
        if (!fork) return false;
        const visited = new Set<number>();
        const stack = [...fork.next];
        let found = false;
        while (stack.length > 0) {
            const id = stack.pop()!;
            if (visited.has(id)) continue;
            visited.add(id);
            const node = nodes_by_id.get(id);
            if (!node || node.kind === 'join') continue;
            if (has_real_transform(node) || is_absorbing_marker(node)) {
                found = true;
                break;
            }
            stack.push(...node.next);
        }
        fork_has_real_transform.set(fork_id, found);
        return found;
    };

    const is_groupable = (node: GraphNode): boolean => {
        if (node.kind === 'state' || node.kind === 'join') return true;
        if (node.kind === 'fork') return !fork_contains_real_transform(node.id);
        return false;
    };

    const chain_prev = new Map<number, number[]>();
    for (const n of graph.nodes) {
        if (!is_groupable(n)) continue;
        for (const next_id of n.next) {
            const target = nodes_by_id.get(next_id);
            if (!target || !is_groupable(target)) continue;
            const preds = chain_prev.get(next_id) ?? [];
            preds.push(n.id);
            chain_prev.set(next_id, preds);
        }
    }

    const chainable = new Set<number>();
    for (const [target_id, preds] of chain_prev.entries())
        if (preds.length === (in_degree.get(target_id) ?? 0)) chainable.add(target_id);

    const node_group = new Map<number, number>();
    const groups = new Map<number, GraphNode[]>();

    const union = (a: number, b: number): void => {
        const root_a = node_group.get(a);
        const root_b = node_group.get(b);
        if (root_a === undefined || root_b === undefined || root_a === root_b) return;
        const group_a = groups.get(root_a)!;
        const group_b = groups.get(root_b)!;
        for (const node of group_b) node_group.set(node.id, root_a);
        groups.set(root_a, [...group_a, ...group_b]);
        groups.delete(root_b);
    };

    for (const n of graph.nodes) {
        node_group.set(n.id, n.id);
        groups.set(n.id, [n]);
    }
    for (const target_id of chainable) {
        for (const pred_id of chain_prev.get(target_id) ?? []) union(pred_id, target_id);
    }
    for (const n of graph.nodes) {
        if (n.kind !== 'join' || n.next.length !== 1) continue;
        const target = nodes_by_id.get(n.next[0]);
        if (!target || target.kind !== 'state') continue;
        if ((in_degree.get(target.id) ?? 0) === 1) union(n.id, target.id);
    }

    const resolve_absorption_roots = (id: number): number[] => {
        const roots = new Set<number>();
        const visit = (current: number, path: Set<number>): void => {
            const owners = absorbed_by.get(current);
            if (!owners || path.has(current)) {
                roots.add(current);
                return;
            }
            const next_path = new Set(path);
            next_path.add(current);
            for (const owner of owners) visit(owner, next_path);
        };
        visit(id, new Set());
        return [...roots];
    };

    for (const [member_id] of absorbed_by.entries()) {
        const member = nodes_by_id_raw.get(member_id);
        if (!member || is_transparent_link(member) || is_dead_end_passthrough(member)) continue;
        const root_ids = resolve_absorption_roots(member_id);
        node_group.set(member_id, root_ids[0]);
        for (const root_id of root_ids) {
            const group = groups.get(root_id) ?? [];
            group.push(member);
            groups.set(root_id, group);
        }
    }

    const representative = new Map<number, number>();
    for (const [root, group] of groups.entries())
        for (const node of group) representative.set(node.id, root);

    const predecessors = new Map<number, number[]>();
    for (const n of graph.nodes)
        for (const target of n.next) {
            const preds = predecessors.get(target) ?? [];
            preds.push(n.id);
            predecessors.set(target, preds);
        }

    const nodes: Node[] = [...groups.entries()].map(([start_id, group]) => {
        const marker = group.find((n) => n.id === start_id && is_absorbing_marker(n));
        if (marker) {
            return {
                id: String(start_id),
                type: 'score_node',
                position: { x: 0, y: 0 },
                data: {
                    nodes: [marker, ...group.filter((n) => n.id !== marker.id)],
                    entry_ids: [],
                    exit_ids: [],
                },
            };
        }
        const group_ids = new Set(group.map((n) => n.id));
        const is_playable = (n: GraphNode): boolean => n.kind === 'state' || n.kind === 'fork';
        const playable = group.filter(is_playable);
        let entry_ids = playable
            .filter((n) => !(predecessors.get(n.id) ?? []).some((p) => group_ids.has(p)))
            .map((n) => n.id);
        let exit_ids = playable
            .filter((n) => !n.next.some((next_id) => group_ids.has(next_id)))
            .map((n) => n.id);
        if (entry_ids.length === 0 && playable.length > 0) entry_ids = [playable[0].id];
        if (exit_ids.length === 0 && playable.length > 0)
            exit_ids = [playable[playable.length - 1].id];
        return {
            id: String(start_id),
            type: 'score_node',
            position: { x: 0, y: 0 },
            data: { nodes: group, entry_ids, exit_ids },
        };
    });

    for (const [index] of full_graph.entries.entries()) {
        nodes.push({
            id: `play-${index}`,
            type: 'play_node',
            position: { x: 0, y: 0 },
            data: { index },
        });
    }

    const edges: Edge[] = [];
    const seen_edges = new Set<string>();
    for (const n of graph.nodes) {
        const source_rep = representative.get(n.id);
        if (source_rep === undefined) continue;
        for (const target of n.next) {
            const target_rep = representative.get(target);
            if (target_rep === undefined || target_rep === source_rep) continue;
            const edge_id = `${source_rep}->${target_rep}`;
            if (seen_edges.has(edge_id)) continue;
            seen_edges.add(edge_id);
            edges.push({
                id: edge_id,
                source: String(source_rep),
                target: String(target_rep),
                type: 'floating',
                animated: false,
                markerEnd: { type: MarkerType.ArrowClosed },
            });
        }
    }
    for (const [index, machine] of full_graph.entries.entries()) {
        const play_id = `play-${index}`;
        for (const entry_id of machine) {
            const target_id = resolve_visual_target(nodes_by_id_raw, entry_id);
            if (target_id === undefined) continue;
            const target_rep = representative.get(target_id);
            if (target_rep === undefined) continue;
            const edge_id = `${play_id}->${target_rep}`;
            if (seen_edges.has(edge_id)) continue;
            seen_edges.add(edge_id);
            edges.push({
                id: edge_id,
                source: play_id,
                target: String(target_rep),
                type: 'floating',
                animated: false,
                markerEnd: { type: MarkerType.ArrowClosed },
            });
        }
    }
    return { nodes, edges };
}

function ScoreGraphViewInner({ source, bpm }: Props) {
    const [nodes, set_nodes, on_nodes_change] = useNodesState<Node>([]);
    const [edges, set_edges] = useState<Edge[]>([]);
    const [error, set_error] = useState<string | null>(null);
    const [measured_once, set_measured_once] = useState(false);
    const [graph, set_graph] = useState<ScoreGraph | null>(null);
    const [piano_roll_target, set_piano_roll_target] = useState<{
        start_id: number;
        stop_id: number;
    } | null>(null);
    const rf = useReactFlow();

    const redraw = useCallback(() => {
        ScoreWasm.compile_score(source)
            .then(async (compiled) => {
                const { nodes: flow_nodes, edges: flow_edges } = compact_flow_graph(compiled);
                const positioned = await layout_nodes(flow_nodes, flow_edges);
                set_measured_once(false);
                set_nodes(positioned);
                set_edges(flow_edges);
                set_graph(compiled);
                set_error(null);
            })
            .catch((e: unknown) => {
                set_error(e instanceof Error ? e.message : String(e));
            });
    }, [source, set_nodes]);

    useEffect(() => {
        if (measured_once || nodes.length === 0) return;
        if (!nodes.every((n) => n.measured?.width && n.measured?.height)) return;
        let cancelled = false;
        layout_nodes(nodes, edges).then((positioned) => {
            if (cancelled) return;
            set_measured_once(true);
            set_nodes(positioned);
            setTimeout(() => rf.fitView({ duration: 200 }), 50);
        });
        return () => {
            cancelled = true;
        };
    }, [nodes, edges, measured_once, rf, set_nodes]);

    return (
        <div className="score-graph-view__inner">
            <button className="score-graph-view__redraw" onClick={redraw}>
                redesenhar
            </button>
            {error ? (
                <div className="score-graph-view__error">{error}</div>
            ) : (
                <ReactFlow
                    nodes={nodes}
                    edges={edges}
                    onNodesChange={on_nodes_change}
                    onNodeDoubleClick={(_event, node) => {
                        const group_nodes = node.data.nodes as GraphNode[] | undefined;
                        const marker = group_nodes?.find((n) => is_loop_marker(n));
                        if (marker) {
                            const start_id = body_entry_of(marker);
                            const stop_id = body_exit_of(marker);
                            if (start_id !== undefined && stop_id !== undefined)
                                set_piano_roll_target({ start_id, stop_id });
                            return;
                        }
                        const entry_ids = node.data.entry_ids as number[];
                        const exit_ids = node.data.exit_ids as number[];
                        if (entry_ids.length === 0 || exit_ids.length === 0) return;
                        set_piano_roll_target({
                            start_id: entry_ids[0],
                            stop_id: exit_ids[exit_ids.length - 1],
                        });
                    }}
                    nodeTypes={NODE_TYPES}
                    edgeTypes={EDGE_TYPES}
                    fitView
                    proOptions={{ hideAttribution: true }}
                >
                    <Controls showZoom={false} showInteractive={false} />
                </ReactFlow>
            )}
            {graph && piano_roll_target && (
                <ScorePianoRoll
                    graph={graph}
                    start_node_id={piano_roll_target.start_id}
                    stop_after_node_id={piano_roll_target.stop_id}
                    bpm={bpm}
                    on_close={() => set_piano_roll_target(null)}
                />
            )}
        </div>
    );
}

export function ScoreGraphView(props: Props) {
    return (
        <div className="score-graph-view">
            <ReactFlowProvider>
                <ScoreGraphViewInner {...props} />
            </ReactFlowProvider>
        </div>
    );
}
