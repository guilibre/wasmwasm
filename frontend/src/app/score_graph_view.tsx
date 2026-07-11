import { useCallback, useEffect, useState } from 'react';
import {
    ReactFlow,
    ReactFlowProvider,
    Controls,
    Handle,
    Position,
    MarkerType,
    useReactFlow,
    useInternalNode,
    useNodesState,
    getStraightPath,
} from '@xyflow/react';
import type { InternalNode, Node, Edge, NodeProps, EdgeProps } from '@xyflow/react';
import '@xyflow/react/dist/style.css';
import ELK from 'elkjs/lib/elk.bundled.js';
import ScoreWasm from '../scorewasm/compiler';
import type { ScoreGraph, GraphNode, ExprNode } from '../audio/conductor';
import { ScorePianoRoll } from './score_piano_roll';
import './score_graph_view.scss';

interface Props {
    source: string;
    bpm: number;
}

function expr_to_string(expr: ExprNode): string {
    switch (expr.kind) {
        case 'number':
            return String(expr.value);
        case 'null':
            return 'null';
        case 'ident':
            return expr.name;
        case 'ternary':
            return `${expr_to_string(expr.cond)} ? ${expr_to_string(expr.then)} : ${expr_to_string(expr.else)}`;
        case 'binary': {
            const symbols: Record<string, string> = {
                add: '+',
                sub: '-',
                mul: '*',
                div: '/',
                pow: '^',
                eq: '==',
                neq: '!=',
                lt: '<',
                gt: '>',
                lte: '<=',
                gte: '>=',
                and: '&',
                or: '|',
            };
            return `${expr_to_string(expr.lhs)} ${symbols[expr.op]} ${expr_to_string(expr.rhs)}`;
        }
    }
}

const KIND_LABEL: Record<GraphNode['kind'], string> = {
    state: 'state',
    fork: 'fork',
    join: 'join',
    passthrough: 'passthrough',
    transform_push: 'push',
    transform_pop: 'pop',
    branch: 'choose',
};

function ScoreGraphNoteBody({ node }: { node: GraphNode }) {
    return (
        <>
            {node.instrument && (
                <div className="score-graph-node__instrument">{node.instrument}</div>
            )}
            {node.params && (
                <div className="score-graph-node__params">
                    {Object.entries(node.params).map(([name, value]) => (
                        <div key={name} className="score-graph-node__param">
                            {name}: {value}
                        </div>
                    ))}
                </div>
            )}
        </>
    );
}

function summarize_sequence(nodes: GraphNode[]): { instrument?: string; total_dur?: number } {
    const instruments = new Set(nodes.map((n) => n.instrument).filter((i): i is string => !!i));
    const durs = nodes.map((n) => n.params?.dur);
    const total_dur = durs.every((d) => d !== undefined)
        ? durs.reduce((a, b) => a + (b ?? 0), 0)
        : undefined;
    const instrument =
        instruments.size === 1 ? [...instruments][0] : instruments.size > 1 ? 'vários' : undefined;
    return { instrument, total_dur };
}

function ScoreGraphSequenceSummary({ nodes }: { nodes: GraphNode[] }) {
    const { instrument, total_dur } = summarize_sequence(nodes);
    return (
        <>
            {instrument && <div className="score-graph-node__instrument">{instrument}</div>}
            <div className="score-graph-node__summary">
                {nodes.length} notas
                {total_dur !== undefined && `, dur ${total_dur}`}
            </div>
        </>
    );
}

function ScoreGraphNode({ data }: NodeProps) {
    const nodes = data.nodes as GraphNode[];
    const node = nodes[0];
    return (
        <div className={`score-graph-node score-graph-node--${node.kind}`}>
            <Handle type="source" position={Position.Top} id="top" />
            <Handle type="target" position={Position.Top} id="top" />
            <Handle type="source" position={Position.Right} id="right" />
            <Handle type="target" position={Position.Right} id="right" />
            <Handle type="source" position={Position.Bottom} id="bottom" />
            <Handle type="target" position={Position.Bottom} id="bottom" />
            <Handle type="source" position={Position.Left} id="left" />
            <Handle type="target" position={Position.Left} id="left" />
            <div className="score-graph-node__kind">
                {KIND_LABEL[node.kind]}
                {nodes.length > 1 && ` ×${nodes.length}`}
            </div>
            {node.kind === 'state' &&
                (nodes.length === 1 ? (
                    <ScoreGraphNoteBody node={node} />
                ) : (
                    <ScoreGraphSequenceSummary nodes={nodes} />
                ))}
            {node.kind === 'transform_push' && node.transforms && node.transforms.length > 0 && (
                <div className="score-graph-node__expr">
                    {node.transforms.map((t, i) => (
                        <div key={i}>
                            {t.paramName} = {expr_to_string(t.expr)}
                        </div>
                    ))}
                </div>
            )}
            {node.kind === 'transform_push' && node.pushInstrument && (
                <div className="score-graph-node__instrument">{node.pushInstrument}</div>
            )}
            {node.kind === 'join' && node.joinArity !== undefined && (
                <div className="score-graph-node__arity">arity: {node.joinArity}</div>
            )}
            {node.kind === 'branch' && node.cond && (
                <div className="score-graph-node__expr">{expr_to_string(node.cond)}</div>
            )}
        </div>
    );
}

const NODE_TYPES = {
    score_node: ScoreGraphNode,
};

function get_node_intersection(intersection_node: InternalNode, target_node: InternalNode) {
    const { width: iw, height: ih } = intersection_node.measured;
    const intersection_pos = intersection_node.internals.positionAbsolute;
    const target_pos = target_node.internals.positionAbsolute;
    const { width: tw, height: th } = target_node.measured;

    const w = (iw ?? 0) / 2;
    const h = (ih ?? 0) / 2;

    const x2 = intersection_pos.x + w;
    const y2 = intersection_pos.y + h;
    const x1 = target_pos.x + (tw ?? 0) / 2;
    const y1 = target_pos.y + (th ?? 0) / 2;

    const xx1 = (x1 - x2) / (2 * w) - (y1 - y2) / (2 * h);
    const yy1 = (x1 - x2) / (2 * w) + (y1 - y2) / (2 * h);
    const a = 1 / (Math.abs(xx1) + Math.abs(yy1) || 1);
    const xx3 = a * xx1;
    const yy3 = a * yy1;
    const x = w * (xx3 + yy3) + x2;
    const y = h * (-xx3 + yy3) + y2;

    return { x, y };
}

function get_edge_params(source: InternalNode, target: InternalNode) {
    const source_intersection = get_node_intersection(source, target);
    const target_intersection = get_node_intersection(target, source);

    return {
        sx: source_intersection.x,
        sy: source_intersection.y,
        tx: target_intersection.x,
        ty: target_intersection.y,
    };
}

function FloatingEdge({ id, source, target, markerEnd, style }: EdgeProps) {
    const source_node = useInternalNode(source);
    const target_node = useInternalNode(target);

    if (!source_node || !target_node) return null;

    const { sx, sy, tx, ty } = get_edge_params(source_node, target_node);
    const [path] = getStraightPath({
        sourceX: sx,
        sourceY: sy,
        targetX: tx,
        targetY: ty,
    });

    return (
        <path
            id={id}
            className="react-flow__edge-path"
            d={path}
            markerEnd={markerEnd}
            style={style}
        />
    );
}

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

// Consecutive `state` nodes with no transforms (or anything else) between them -
// i.e. a straight chain A -> B -> C where each step is the only way in and out -
// are compacted into a single flow node, so a long melody doesn't turn into a
// wall of identical-looking boxes.
function compact_flow_graph(graph: ScoreGraph): { nodes: Node[]; edges: Edge[] } {
    const nodes_by_id = new Map(graph.nodes.map((n) => [n.id, n]));
    const in_degree = new Map<number, number>();
    for (const n of graph.nodes)
        for (const target of n.next) in_degree.set(target, (in_degree.get(target) ?? 0) + 1);

    // chain_prev.get(target) === source means source -> target is a compactable link.
    const chain_prev = new Map<number, number>();
    for (const n of graph.nodes) {
        if (n.kind !== 'state' || n.next.length !== 1) continue;
        const target = nodes_by_id.get(n.next[0]);
        if (target && target.kind === 'state' && (in_degree.get(target.id) ?? 0) === 1)
            chain_prev.set(target.id, n.id);
    }

    const representative = new Map<number, number>(); // node id -> id of the flow node it belongs to
    const groups = new Map<number, GraphNode[]>(); // flow node id -> its chain of original nodes

    for (const n of graph.nodes) {
        if (chain_prev.has(n.id)) continue; // mid-chain node, picked up by its chain's start below
        const group: GraphNode[] = [n];
        representative.set(n.id, n.id);
        let cur = n;
        for (;;) {
            if (cur.kind !== 'state' || cur.next.length !== 1) break;
            const next_id = cur.next[0];
            if (chain_prev.get(next_id) !== cur.id) break;
            const next_node = nodes_by_id.get(next_id);
            if (!next_node) break;
            group.push(next_node);
            representative.set(next_id, n.id);
            cur = next_node;
        }
        groups.set(n.id, group);
    }

    const nodes: Node[] = [...groups.entries()].map(([start_id, group]) => ({
        id: String(start_id),
        type: 'score_node',
        position: { x: 0, y: 0 },
        data: { nodes: group },
    }));

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
    return { nodes, edges };
}

function ScoreGraphViewInner({ source, bpm }: Props) {
    const [nodes, set_nodes, on_nodes_change] = useNodesState<Node>([]);
    const [edges, set_edges] = useState<Edge[]>([]);
    const [error, set_error] = useState<string | null>(null);
    const [measured_once, set_measured_once] = useState(false);
    const [graph, set_graph] = useState<ScoreGraph | null>(null);
    const [piano_roll_node_id, set_piano_roll_node_id] = useState<number | null>(null);
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
                    onNodeDoubleClick={(_event, node) => set_piano_roll_node_id(Number(node.id))}
                    nodeTypes={NODE_TYPES}
                    edgeTypes={EDGE_TYPES}
                    fitView
                    proOptions={{ hideAttribution: true }}
                >
                    <Controls showZoom={false} showInteractive={false} />
                </ReactFlow>
            )}
            {graph && piano_roll_node_id !== null && (
                <ScorePianoRoll
                    graph={graph}
                    start_node_id={piano_roll_node_id}
                    bpm={bpm}
                    on_close={() => set_piano_roll_node_id(null)}
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
