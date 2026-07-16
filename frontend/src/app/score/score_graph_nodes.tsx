import { Handle, Position } from '@xyflow/react';
import type { InternalNode, NodeProps, EdgeProps } from '@xyflow/react';
import { useInternalNode, getStraightPath } from '@xyflow/react';
import type { GraphNode } from '../../audio/conductor';
import { wire_to_number } from '../../audio/rational';
import { expr_to_string, summarize_sequence } from './score_graph_helpers';

const KIND_LABEL: Record<GraphNode['kind'], string> = {
    state: 'state',
    fork: 'fork',
    join: 'join',
    passthrough: 'passthrough',
    transform_push: 'push',
    transform_pop: 'pop',
    branch: 'choose',
    signal_emit: 'emit',
    reverse: 'reverse',
    legato: 'legato',
    skip: 'skip',
    repeat: 'repeat',
};

function ScoreGraphNoteBody({ node }: { node: GraphNode }) {
    return (
        <>
            {node.params?.instrument && (
                <div className="score-graph-node__instrument">
                    {typeof node.params.instrument === 'object'
                        ? wire_to_number(node.params.instrument)
                        : node.params.instrument}
                </div>
            )}
            {node.params && (
                <div className="score-graph-node__params">
                    {Object.entries(node.params).map(([name, value]) => (
                        <div key={name} className="score-graph-node__param">
                            {name}: {typeof value === 'object' ? wire_to_number(value) : value}
                        </div>
                    ))}
                </div>
            )}
        </>
    );
}

function ScoreGraphSequenceSummary({
    nodes,
    count_nodes,
}: {
    nodes: GraphNode[];
    count_nodes?: GraphNode[];
}) {
    const { instrument, total_dur } = summarize_sequence(nodes);
    return (
        <>
            {instrument && <div className="score-graph-node__instrument">{instrument}</div>}
            <div className="score-graph-node__summary">
                {(count_nodes ?? nodes).length} notas, dur {total_dur}
            </div>
        </>
    );
}

export function ScoreGraphNode({ data }: NodeProps) {
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
            {(node.kind === 'state' || node.kind === 'fork') &&
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
            {node.kind === 'transform_push' && node.listenChannel && (
                <div className="score-graph-node__expr">listen "{node.listenChannel}"</div>
            )}
            {node.kind === 'join' && node.joinArity !== undefined && (
                <div className="score-graph-node__arity">arity: {node.joinArity}</div>
            )}
            {node.kind === 'branch' && node.cond && (
                <div className="score-graph-node__expr">{expr_to_string(node.cond)}</div>
            )}
            {node.kind === 'signal_emit' && (
                <>
                    <div className="score-graph-node__expr">"{node.signalId}"</div>
                    <ScoreGraphNoteBody node={node} />
                </>
            )}
            {node.kind === 'skip' && node.skipCount !== undefined && (
                <div className="score-graph-node__arity">skipped ×{node.skipCount}</div>
            )}
            {(node.kind === 'repeat' || node.kind === 'reverse') && (
                <>
                    {node.kind === 'repeat' && node.repeatCount !== undefined && (
                        <div className="score-graph-node__arity">n: {node.repeatCount}</div>
                    )}
                    <ScoreGraphSequenceSummary
                        nodes={nodes}
                        count_nodes={nodes.filter((n) => n.kind === 'state')}
                    />
                </>
            )}
        </div>
    );
}

export function ScoreGraphPlayNode({ data }: NodeProps) {
    return (
        <div className="score-graph-node score-graph-node--play">
            <Handle type="source" position={Position.Bottom} id="bottom" />
            <div className="score-graph-node__kind">play {data.index as number}</div>
        </div>
    );
}

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

export function FloatingEdge({ id, source, target, markerEnd, style }: EdgeProps) {
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
