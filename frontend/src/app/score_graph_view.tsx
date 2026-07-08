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
import './score_graph_view.scss';

interface Props {
    source: string;
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
};

function ScoreGraphNode({ data }: NodeProps) {
    const node = data.node as GraphNode;
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
            <div className="score-graph-node__kind">{KIND_LABEL[node.kind]}</div>
            {node.kind === 'state' && (
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
            )}
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

function graph_to_flow(graph: ScoreGraph): { nodes: Node[]; edges: Edge[] } {
    const nodes: Node[] = graph.nodes.map((n) => ({
        id: String(n.id),
        type: 'score_node',
        position: { x: 0, y: 0 },
        data: { node: n },
    }));
    const edges: Edge[] = [];
    for (const n of graph.nodes) {
        for (const target of n.next) {
            edges.push({
                id: `${n.id}->${target}`,
                source: String(n.id),
                target: String(target),
                type: 'floating',
                animated: false,
                markerEnd: { type: MarkerType.ArrowClosed },
            });
        }
    }
    return { nodes, edges };
}

function ScoreGraphViewInner({ source }: Props) {
    const [nodes, set_nodes, on_nodes_change] = useNodesState<Node>([]);
    const [edges, set_edges] = useState<Edge[]>([]);
    const [error, set_error] = useState<string | null>(null);
    const [measured_once, set_measured_once] = useState(false);
    const rf = useReactFlow();

    const redraw = useCallback(() => {
        ScoreWasm.compile_score(source)
            .then(async (graph) => {
                const { nodes: flow_nodes, edges: flow_edges } = graph_to_flow(graph);
                const positioned = await layout_nodes(flow_nodes, flow_edges);
                set_measured_once(false);
                set_nodes(positioned);
                set_edges(flow_edges);
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
                    nodeTypes={NODE_TYPES}
                    edgeTypes={EDGE_TYPES}
                    fitView
                    proOptions={{ hideAttribution: true }}
                >
                    <Controls showZoom={false} showInteractive={false} />
                </ReactFlow>
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
