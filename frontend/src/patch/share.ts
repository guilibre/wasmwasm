import type { Node, Edge } from '@xyflow/react';
import type { InstrumentState } from './use_patch_store';

interface MinNode {
    id: string;
    type: string;
    position: { x: number; y: number };
    data: unknown;
}

interface MinEdge {
    source: string;
    target: string;
    sourceHandle: string | null;
    targetHandle: string | null;
}

interface SerializedPatch {
    nodes: MinNode[];
    edges: MinEdge[];
    instrument?: InstrumentState;
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

export async function patch_to_hash(
    nodes: Node[],
    edges: Edge[],
    instrument: InstrumentState,
): Promise<string> {
    const payload: SerializedPatch = {
        nodes: nodes.map((n) => ({
            id: n.id,
            type: n.type ?? 'block',
            position: n.position,
            data: n.data,
        })),
        edges: edges.map((e) => ({
            source: e.source,
            target: e.target,
            sourceHandle: e.sourceHandle ?? null,
            targetHandle: e.targetHandle ?? null,
        })),
        instrument,
    };

    const json = JSON.stringify(payload);
    const input = new TextEncoder().encode(json);
    const cs = new CompressionStream('deflate-raw');
    const writer = cs.writable.getWriter();
    writer.write(input);
    writer.close();
    const chunks: Uint8Array[] = [];
    const reader = cs.readable.getReader();
    while (true) {
        const { done, value } = await reader.read();
        if (done) break;
        chunks.push(value);
    }
    const total = chunks.reduce((n, c) => n + c.length, 0);
    const compressed = new Uint8Array(total);
    let offset = 0;
    for (const chunk of chunks) {
        compressed.set(chunk, offset);
        offset += chunk.length;
    }
    return to_base64url(compressed);
}

export async function hash_to_patch(
    hash: string,
): Promise<{ nodes: Node[]; edges: Edge[]; instrument?: InstrumentState } | null> {
    try {
        const raw = hash.startsWith('#') ? hash.slice(1) : hash;
        if (!raw) return null;
        const compressed = from_base64url(raw);
        const ds = new DecompressionStream('deflate-raw');
        const writer = ds.writable.getWriter();
        writer.write(new Uint8Array(compressed) as Uint8Array<ArrayBuffer>);
        writer.close();
        const chunks: Uint8Array[] = [];
        const reader = ds.readable.getReader();
        while (true) {
            const { done, value } = await reader.read();
            if (done) break;
            chunks.push(value);
        }
        const total = chunks.reduce((n, c) => n + c.length, 0);
        const bytes = new Uint8Array(total);
        let offset = 0;
        for (const chunk of chunks) {
            bytes.set(chunk, offset);
            offset += chunk.length;
        }
        const json = new TextDecoder().decode(bytes);
        const { nodes, edges, instrument } = JSON.parse(json) as SerializedPatch;
        const full_nodes: Node[] = nodes.map((n) => ({
            ...n,
            data: n.data as Record<string, unknown>,
        }));
        const full_edges: Edge[] = edges.map((e) => ({
            id: `e_${e.source}_${e.sourceHandle}_${e.target}_${e.targetHandle}`,
            ...e,
        }));
        return { nodes: full_nodes, edges: full_edges, instrument };
    } catch {
        return null;
    }
}
