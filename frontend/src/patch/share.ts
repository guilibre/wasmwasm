import type { Node, Edge } from '@xyflow/react';
import type { OrchestraState } from './use_patch_store';

interface MinNode {
    id: string;
    type: string;
    data: unknown;
}

interface MinEdge {
    source: string;
    target: string;
    sourceHandle: string | null;
    targetHandle: string | null;
}

interface MinInstrument {
    id: string;
    name: string;
    code: string;
    nodes: MinNode[];
    edges: MinEdge[];
}

interface SerializedPatch {
    bpm: number;
    active_id: string | null;
    orchestra_code: string;
    instruments: MinInstrument[];
}

const LAYER_GAP = 75;
const NODE_GAP = 150;

function auto_layout(nodes: MinNode[], edges: MinEdge[]): Map<string, { x: number; y: number }> {
    const layer = new Map<string, number>();
    const targets = new Set(edges.map((e) => e.target));

    for (const n of nodes) {
        if (!targets.has(n.id)) layer.set(n.id, 0);
    }

    let changed = true;
    while (changed) {
        changed = false;
        for (const e of edges) {
            const src_layer = layer.get(e.source) ?? 0;
            const tgt_layer = layer.get(e.target) ?? -1;
            const expected = src_layer + 1;
            if (e.source !== e.target && tgt_layer < expected) {
                layer.set(e.target, expected);
                changed = true;
            }
        }
    }

    const by_layer = new Map<number, string[]>();
    for (const [id, l] of layer) {
        if (!by_layer.has(l)) by_layer.set(l, []);
        by_layer.get(l)!.push(id);
    }

    const positions = new Map<string, { x: number; y: number }>();
    for (const [l, ids] of by_layer) {
        const total_width = (ids.length - 1) * NODE_GAP;
        ids.forEach((id, i) => {
            positions.set(id, {
                x: i * NODE_GAP - total_width / 2 + 300,
                y: l * LAYER_GAP + 50,
            });
        });
    }

    return positions;
}

function to_base64url(bytes: Uint8Array): string {
    let binary = '';
    for (let i = 0; i < bytes.length; i++) binary += String.fromCharCode(bytes[i]);
    return btoa(binary).replace(/\+/g, '-').replace(/\//g, '_').replace(/=/g, '');
}

function from_base64url(str: string): Uint8Array {
    const base64 = str.replace(/-/g, '+').replace(/_/g, '/');
    const binary = atob(base64);
    const bytes = new Uint8Array(binary.length);
    for (let i = 0; i < binary.length; i++) bytes[i] = binary.charCodeAt(i);
    return bytes;
}

async function compress(data: Uint8Array): Promise<Uint8Array> {
    const cs = new CompressionStream('deflate-raw');
    const writer = cs.writable.getWriter();
    writer.write(data as BufferSource);
    writer.close();
    const chunks: Uint8Array[] = [];
    const reader = cs.readable.getReader();
    while (true) {
        const { done, value } = await reader.read();
        if (done) break;
        chunks.push(value);
    }
    const total = chunks.reduce((n, c) => n + c.length, 0);
    const out = new Uint8Array(total);
    let offset = 0;
    for (const chunk of chunks) {
        out.set(chunk, offset);
        offset += chunk.length;
    }
    return out;
}

async function decompress(data: Uint8Array): Promise<Uint8Array> {
    const ds = new DecompressionStream('deflate-raw');
    const writer = ds.writable.getWriter();
    writer.write(data as Uint8Array<ArrayBuffer>);
    writer.close();
    const chunks: Uint8Array[] = [];
    const reader = ds.readable.getReader();
    while (true) {
        const { done, value } = await reader.read();
        if (done) break;
        chunks.push(value);
    }
    const total = chunks.reduce((n, c) => n + c.length, 0);
    const out = new Uint8Array(total);
    let offset = 0;
    for (const chunk of chunks) {
        out.set(chunk, offset);
        offset += chunk.length;
    }
    return out;
}

export async function patch_to_hash(orchestra: OrchestraState): Promise<string> {
    const payload: SerializedPatch = {
        bpm: orchestra.bpm,
        active_id: orchestra.active_id,
        orchestra_code: orchestra.code,
        instruments: orchestra.instruments.map((instr) => ({
            id: instr.id,
            name: instr.name,
            code: instr.code,
            nodes: instr.nodes.map((n) => ({ id: n.id, type: n.type ?? 'block', data: n.data })),
            edges: instr.edges.map((e) => ({
                source: e.source,
                target: e.target,
                sourceHandle: e.sourceHandle ?? null,
                targetHandle: e.targetHandle ?? null,
            })),
        })),
    };

    const input = new TextEncoder().encode(JSON.stringify(payload));
    const compressed = await compress(input);
    return to_base64url(compressed);
}

export async function hash_to_patch(hash: string): Promise<OrchestraState | null> {
    try {
        const raw = hash.startsWith('#') ? hash.slice(1) : hash;
        if (!raw) return null;
        const compressed = from_base64url(raw);
        const bytes = await decompress(compressed);
        const { bpm, active_id, orchestra_code, instruments } = JSON.parse(
            new TextDecoder().decode(bytes),
        ) as SerializedPatch;

        return {
            bpm,
            active_id,
            code: orchestra_code,
            instruments: instruments.map((instr) => {
                const positions = auto_layout(instr.nodes, instr.edges);
                const nodes: Node[] = instr.nodes.map((n) => ({
                    ...n,
                    position: positions.get(n.id) ?? { x: 100, y: 100 },
                    data: n.data as Record<string, unknown>,
                }));
                const edges: Edge[] = instr.edges.map((e) => ({
                    id: `e_${e.source}_${e.sourceHandle}_${e.target}_${e.targetHandle}`,
                    ...e,
                    type: e.source === e.target ? 'self_loop' : undefined,
                }));
                return { id: instr.id, name: instr.name, code: instr.code, nodes, edges };
            }),
        };
    } catch {
        return null;
    }
}
