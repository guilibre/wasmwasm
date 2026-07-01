import type { Edge, Node } from '@xyflow/react';

export function patch_to_json(nodes: Node[], edges: Edge[]): string {
    const modules: Record<string, string> = {};
    const patch: Record<string, string> = {};
    const node_map = new Map(nodes.map((n) => [n.id, n]));

    for (const node of nodes) {
        if (node.type === 'block') {
            const data = node.data as { name: string; code: string };
            modules[node.id] = data.code;
        }
    }

    for (const edge of edges) {
        const src_node = node_map.get(edge.source);
        const tgt_node = node_map.get(edge.target);
        if (!src_node || !tgt_node || !edge.sourceHandle || !edge.targetHandle) continue;

        const src_key =
            src_node.type === 'capture' ? edge.sourceHandle : `${src_node.id}_${edge.sourceHandle}`;

        const sink_key =
            tgt_node.type === 'out' || tgt_node.type === 'dac'
                ? edge.targetHandle
                : `${tgt_node.id}_${edge.targetHandle}`;

        patch[sink_key] = src_key;
    }

    return JSON.stringify({ modules, patch });
}
