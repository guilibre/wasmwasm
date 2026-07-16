import ELK from 'elkjs/lib/elk.bundled.js';
import type { Node, Edge } from '@xyflow/react';
import type { BlockData, OutData, InData } from './patch_types';

export function handle_fraction(index: number, count: number): number {
    if (count <= 1) return 0;
    return index / (count - 1);
}

export function handle_offset(index: number, count: number): string {
    return `${handle_fraction(index, count) * 100}%`;
}

const elk = new ELK();

function spaced(count: number, width: number): number[] {
    return Array.from({ length: count }, (_, i) => handle_fraction(i, count) * width);
}

function node_size(node: Node): { width: number; height: number } {
    return {
        width: node.width ?? node.measured?.width ?? 60,
        height: node.height ?? node.measured?.height ?? 40,
    };
}

function node_ports(node: Node) {
    const { width, height } = node_size(node);
    if (node.type === 'dac') {
        const [xl, xr] = spaced(2, width);
        return [
            { id: `${node.id}__dac_l`, x: xl, y: 0 },
            { id: `${node.id}__dac_r`, x: xr, y: 0 },
        ];
    }
    if (node.type === 'adc') {
        const [xl, xr] = spaced(2, width);
        return [
            { id: `${node.id}__adc_l`, x: xl, y: height },
            { id: `${node.id}__adc_r`, x: xr, y: height },
        ];
    }
    if (node.type === 'out') {
        const num_channels = (node.data as Partial<OutData>).num_channels ?? 0;
        return spaced(num_channels, width).map((x, i) => ({
            id: `${node.id}__out_${i}`,
            x,
            y: 0,
        }));
    }
    if (node.type === 'instrument_in') {
        const num_channels = (node.data as Partial<InData>).num_channels ?? 0;
        return spaced(num_channels, width).map((x, i) => ({
            id: `${node.id}__in_${i}`,
            x,
            y: height,
        }));
    }
    if (node.type === 'in') {
        const { num_channels, num_in_channels } = node.data as Partial<InData>;
        const ports = [];
        for (const [i, x] of spaced(num_in_channels ?? 0, width).entries())
            ports.push({ id: `${node.id}__in_${i}`, x, y: 0 });
        for (const [i, x] of spaced(num_channels ?? 0, width).entries())
            ports.push({ id: `${node.id}__out_${i}`, x, y: height });
        return ports;
    }
    const data = node.data as Partial<BlockData>;
    const num_in = data.num_inputs ?? 0;
    const num_out = data.num_outputs ?? 0;
    const ports = [];
    for (const [i, x] of spaced(num_in, width).entries())
        ports.push({ id: `${node.id}__in_${i}`, x, y: 0 });
    for (const [i, x] of spaced(num_out, width).entries())
        ports.push({ id: `${node.id}__out_${i}`, x, y: height });
    return ports;
}

export async function elk_layout(nodes: Node[], edges: Edge[]): Promise<Node[]> {
    const children = nodes.map((n) => ({
        ...n,
        ...node_size(n),
        ports: node_ports(n),
        layoutOptions: { 'elk.portConstraints': 'FIXED_POS' },
        x: 0,
        y: 0,
    }));
    const dac_node_idx = children.findIndex((x) => x.type === 'dac' || x.type === 'out');
    const dac_node = dac_node_idx === -1 ? null : children[dac_node_idx];
    if (dac_node_idx !== -1) children.splice(dac_node_idx, 1);
    const adc_node_idx = children.findIndex((x) => x.type === 'adc' || x.type === 'instrument_in');
    const adc_node = adc_node_idx === -1 ? null : children[adc_node_idx];
    if (adc_node_idx !== -1) children.splice(adc_node_idx, 1);
    const graph = {
        id: 'root',
        layoutOptions: {
            'elk.algorithm': 'layered',
            'elk.direction': 'DOWN',
            'elk.layered.spacing.nodeNodeBetweenLayers': '10',
            'elk.spacing.nodeNode': '20',
            'elk.spacing.edgeNode': '10',
            'elk.spacing.edgeEdge': '10',
            'elk.layered.spacing.edgeNodeBetweenLayers': '10',
            'elk.separateConnectedComponents': 'false',
            'elk.layered.crossingMinimization.strategy': 'LAYER_SWEEP',
            'elk.layered.crossingMinimization.greedySwitch.type': 'TWO_SIDED',
            'elk.layered.nodePlacement.strategy': 'BRANDES_KOEPF',
            'elk.layered.nodePlacement.bk.fixedAlignment': 'LEFTDOWN',
            'elk.layered.compaction.postCompaction.strategy': 'EDGE_LENGTH',
            'elk.layered.compaction.postCompaction.constraints': 'QUADRATIC',
            'elk.edgeRouting': 'ORTHOGONAL',
        },
        children: children,
        edges: edges
            .filter(
                (e) =>
                    e.source !== e.target &&
                    e.sourceHandle &&
                    e.targetHandle &&
                    e.target !== dac_node?.id &&
                    e.source !== adc_node?.id,
            )
            .map((e) => ({
                id: e.id,
                sources: [`${e.source}__${e.sourceHandle}`],
                targets: [`${e.target}__${e.targetHandle}`],
            })),
    };
    const laid_out = await elk.layout(graph);

    const all_lefts = children.map((c) => c.x ?? 0);
    const all_tops = children.map((c) => c.y ?? 0);
    const all_bottoms = children.map((c) => (c.y ?? 0) + (c.height ?? 0));

    const min_x = all_lefts.length > 0 ? Math.min(...all_lefts) : 0;
    const min_y = all_tops.length > 0 ? Math.min(...all_tops) : 0;
    const max_y = all_bottoms.length > 0 ? Math.max(...all_bottoms) : 0;

    if (dac_node) {
        dac_node.x = min_x;
        dac_node.y = max_y + 20;
    }

    if (adc_node) {
        adc_node.x = min_x;
        adc_node.y = min_y - 20 - (adc_node.height ?? 0);
    }

    const fixed_nodes = [dac_node, adc_node].filter((n): n is NonNullable<typeof n> => n != null);

    return nodes.map((n) => {
        const child = [...fixed_nodes, ...(laid_out.children ?? [])].find((c) => c.id === n.id);
        return child ? { ...n, position: { x: child.x ?? 0, y: child.y ?? 0 } } : n;
    });
}
