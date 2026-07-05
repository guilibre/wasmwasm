import type { Edge, Node } from '@xyflow/react';
import type { OrchestraState } from './store/patch_types';

interface BlockNodeData {
    name: string;
    code: string;
}

interface InstrumentModule {
    modules: Record<string, string>;
    patch: Record<string, string>;
}

function rename_ident(code: string, name: string, prefixed: string): string {
    const re = new RegExp(`\\b${name}\\b`, 'g');
    return code.replace(re, prefixed);
}

function rewrite_non_param_declarations(code: string, block_id: string): string {
    let out = code;
    const declared = new Set<string>();

    for (const m of out.matchAll(/^\s*static\s+(\w+)\s*=/gm)) declared.add(m[1]);
    for (const m of out.matchAll(/^\s*(\w+)\s*=(?!=)/gm)) {
        if (!/^\s*(param|static)\s/.test(m[0])) declared.add(m[1]);
    }

    for (const name of declared) out = rename_ident(out, name, `${block_id}_${name}`);

    return out;
}

function build_module_graph(
    nodes: Node[],
    edges: Edge[],
    build_module_code: (block: Node) => string,
    resolve_source: (src_node: Node, source_handle: string) => string,
): InstrumentModule {
    const node_map = new Map(nodes.map((n) => [n.id, n]));
    const block_nodes = nodes.filter((n) => n.type === 'block');

    const modules: Record<string, string> = {};
    for (const block of block_nodes) modules[block.id] = build_module_code(block);

    const patch: Record<string, string> = {};
    for (const edge of edges) {
        const src_node = node_map.get(edge.source);
        const tgt_node = node_map.get(edge.target);
        if (!src_node || !tgt_node || !edge.sourceHandle || !edge.targetHandle) continue;

        const src_key = resolve_source(src_node, edge.sourceHandle);
        const sink_key =
            tgt_node.type === 'out' || tgt_node.type === 'dac'
                ? edge.targetHandle
                : `${tgt_node.id}_${edge.targetHandle}`;

        patch[sink_key] = src_key;
    }

    return { modules, patch };
}

function build_instrument_module(nodes: Node[], edges: Edge[]): InstrumentModule {
    return build_module_graph(
        nodes,
        edges,
        (block) =>
            rewrite_non_param_declarations((block.data as unknown as BlockNodeData).code, block.id),
        (src_node, source_handle) => `${src_node.id}_${source_handle}`,
    );
}

function build_global_graph(nodes: Node[], edges: Edge[]): InstrumentModule {
    const in_node_instrument_id = (node_id: string): string | null =>
        node_id.startsWith('in_') ? node_id.slice(3) : null;

    return build_module_graph(
        nodes,
        edges,
        (block) => (block.data as unknown as BlockNodeData).code,
        (src_node, source_handle) => {
            const src_instrument_id = in_node_instrument_id(src_node.id);
            return src_node.type === 'in' && src_instrument_id
                ? `${src_instrument_id}_${source_handle.replace('in_', 'out_')}`
                : `${src_node.id}_${source_handle}`;
        },
    );
}

export function orchestra_to_json(orchestra: OrchestraState): string {
    const instruments: Record<
        string,
        { modules: Record<string, string>; patch: Record<string, string> }
    > = {};
    for (const instr of orchestra.instruments) {
        instruments[instr.id] = build_instrument_module(instr.nodes, instr.edges);
    }

    const global = build_global_graph(orchestra.global_nodes, orchestra.global_edges);

    return JSON.stringify({ instruments, global });
}
