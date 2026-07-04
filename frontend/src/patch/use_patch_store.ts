import { useCallback, useEffect, useReducer, useRef, useState } from 'react';
import ELK from 'elkjs/lib/elk.bundled.js';
import { get_default_code } from './block_templates';
import { handle_fraction } from './handle_layout';
import { applyNodeChanges, applyEdgeChanges } from '@xyflow/react';
import type { Node, Edge, NodeChange, EdgeChange, Connection } from '@xyflow/react';

export interface BlockData {
    name: string;
    code: string;
    num_inputs: number;
    num_outputs: number;
    params: string[];
}

export interface OutData {
    name: string;
    num_channels: number;
}

export interface InData {
    name: string;
    num_channels: number;
}

const OUT_NAME_RE = /^OUT\s+(\d+)$/i;

export function parse_out_name(name: string): number | null {
    const m = OUT_NAME_RE.exec(name.trim());
    if (!m) return null;
    const n = parseInt(m[1], 10);
    return n > 0 ? n : null;
}

export function scan_params(code: string): string[] {
    return [...code.matchAll(/^\s*param\s+(\w+)\s*=/gm)].map((m) => m[1]);
}

export function scan_arity(code: string): { num_inputs: number; num_outputs: number } {
    const ins = [...code.matchAll(/IN\[(\d+)\]/g)].map((m) => parseInt(m[1]));
    const outs = [...code.matchAll(/OUT\[(\d+)\]/g)].map((m) => parseInt(m[1]));
    return {
        num_inputs: ins.length > 0 ? Math.max(...ins) + 1 : 0,
        num_outputs: outs.length > 0 ? Math.max(...outs) + 1 : 0,
    };
}

export interface InstrumentState {
    id: string;
    name: string;
    nodes: Node[];
    edges: Edge[];
    code: string;
}

export interface OrchestraState {
    bpm: number;
    active_id: string | null;
    instruments: InstrumentState[];
    code: string;
    global_nodes: Node[];
    global_edges: Edge[];
    global_patch_code: string;
    score_code: string;
}

export type PatchView = 'instrument' | 'global';

interface PatchState {
    orchestra: OrchestraState;
    selected_id: string | null;
    view: PatchView;
}

type PatchAction =
    | { type: 'nodes_change'; changes: NodeChange[] }
    | { type: 'edges_change'; changes: EdgeChange[] }
    | { type: 'connect'; connection: Connection }
    | { type: 'global_nodes_change'; changes: NodeChange[] }
    | { type: 'global_edges_change'; changes: EdgeChange[] }
    | { type: 'global_connect'; connection: Connection }
    | { type: 'set_view'; view: PatchView }
    | { type: 'select'; id: string | null }
    | { type: 'update_code'; id: string; code: string }
    | { type: 'update_name'; id: string; name: string }
    | { type: 'add_node'; node: Node }
    | { type: 'update_global_code'; id: string; code: string }
    | { type: 'update_global_name'; id: string; name: string }
    | { type: 'add_global_node'; node: Node }
    | { type: 'set_orchestra_bpm'; bpm: number }
    | { type: 'add_instrument' }
    | { type: 'remove_instrument'; id: string }
    | { type: 'set_instrument_code'; id: string; code: string }
    | { type: 'rename_instrument'; id: string; name: string }
    | { type: 'set_active_instrument'; id: string }
    | { type: 'set_orchestra_code'; code: string }
    | { type: 'set_global_patch_code'; code: string }
    | { type: 'set_score_code'; code: string }
    | { type: 'load'; orchestra: OrchestraState }
    | { type: 'apply_layout'; instrument_id: string; nodes: Node[] }
    | { type: 'apply_global_layout'; nodes: Node[] }
    | { type: 'undo' }
    | { type: 'redo' };

const NO_HISTORY = new Set<PatchAction['type']>([
    'select',
    'set_view',
    'set_active_instrument',
    'set_instrument_code',
    'set_orchestra_code',
    'set_global_patch_code',
    'set_score_code',
    'apply_layout',
    'apply_global_layout',
    'load',
    'undo',
    'redo',
]);

interface HistoryState {
    past: PatchState[];
    present: PatchState;
    future: PatchState[];
}

const STORAGE_KEY = 'wasmwasm_patch';

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
    if (node.type === 'capture') {
        const [xl, xr] = spaced(2, width);
        return [
            { id: `${node.id}__capture_l`, x: xl, y: height },
            { id: `${node.id}__capture_r`, x: xr, y: height },
        ];
    }
    if (node.type === 'dac') {
        const [xl, xr] = spaced(2, width);
        return [
            { id: `${node.id}__dac_l`, x: xl, y: 0 },
            { id: `${node.id}__dac_r`, x: xr, y: 0 },
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
    if (node.type === 'in') {
        const num_channels = (node.data as Partial<InData>).num_channels ?? 0;
        return spaced(num_channels, width).map((x, i) => ({
            id: `${node.id}__in_${i}`,
            x,
            y: height,
        }));
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

async function elk_layout(nodes: Node[], edges: Edge[]): Promise<Node[]> {
    const children = nodes.map((n) => ({
        ...n,
        ...node_size(n),
        ports: node_ports(n),
        layoutOptions: { 'elk.portConstraints': 'FIXED_POS' },
        x: 0,
        y: 0,
    }));
    const capture_node_idx = children.findIndex((x) => x.type === 'capture');
    const capture_node = capture_node_idx === -1 ? null : children[capture_node_idx];
    if (capture_node_idx !== -1) children.splice(capture_node_idx, 1);
    const dac_node_idx = children.findIndex((x) => x.type === 'dac' || x.type === 'out');
    const dac_node = dac_node_idx === -1 ? null : children[dac_node_idx];
    if (dac_node_idx !== -1) children.splice(dac_node_idx, 1);
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
                    e.source !== capture_node?.id &&
                    e.target !== dac_node?.id,
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

    if (capture_node) {
        capture_node.x = min_x;
        capture_node.y = min_y - 40;
    }
    if (dac_node) {
        dac_node.x = min_x;
        dac_node.y = max_y + 20;
    }

    const fixed_nodes = [capture_node, dac_node].filter(
        (n): n is NonNullable<typeof n> => n != null,
    );

    return nodes.map((n) => {
        const child = [...fixed_nodes, ...(laid_out.children ?? [])].find((c) => c.id === n.id);
        return child ? { ...n, position: { x: child.x ?? 0, y: child.y ?? 0 } } : n;
    });
}

const default_nodes = (): Node[] => [
    { id: 'capture', type: 'capture', position: { x: 0, y: 0 }, data: {} },
    {
        id: 'out',
        type: 'out',
        position: { x: 0, y: 0 },
        data: { name: 'OUT 2', num_channels: 2 } satisfies OutData,
    },
];

function in_node_id(instrument_id: string): string {
    return `in_${instrument_id}`;
}

function out_channels_of(instrument: InstrumentState): number {
    const out_node = instrument.nodes.find((n) => n.type === 'out');
    return (out_node?.data as Partial<OutData> | undefined)?.num_channels ?? 2;
}

function make_in_node(instrument: InstrumentState): Node {
    return {
        id: in_node_id(instrument.id),
        type: 'in',
        position: { x: 0, y: 0 },
        data: { name: instrument.name, num_channels: out_channels_of(instrument) } satisfies InData,
    };
}

function sync_global_in_nodes(instruments: InstrumentState[], global_nodes: Node[]): Node[] {
    const instrument_ids = new Set(instruments.map((i) => i.id));
    const kept = global_nodes.filter(
        (n) => n.type !== 'in' || instrument_ids.has(n.id.replace(/^in_/, '')),
    );
    const existing_in_ids = new Set(kept.filter((n) => n.type === 'in').map((n) => n.id));
    const updated = kept.map((n) => {
        if (n.type !== 'in') return n;
        const instrument = instruments.find((i) => in_node_id(i.id) === n.id);
        if (!instrument) return n;
        const num_channels = out_channels_of(instrument);
        const data = n.data as Partial<InData>;
        if (data.num_channels === num_channels && data.name === instrument.name) return n;
        return { ...n, data: { ...n.data, num_channels, name: instrument.name } };
    });
    const added = instruments
        .filter((i) => !existing_in_ids.has(in_node_id(i.id)))
        .map((i) => make_in_node(i));
    return [...updated, ...added];
}

const default_global_nodes = (): Node[] => [
    { id: 'master_dac', type: 'dac', position: { x: 0, y: 0 }, data: {} },
];

const DEFAULT_ORCHESTRA: OrchestraState = {
    bpm: 120,
    active_id: null,
    instruments: [],
    code: '',
    global_nodes: default_global_nodes(),
    global_edges: [],
    global_patch_code: '',
    score_code: '',
};

function normalize_orchestra(orchestra: Partial<OrchestraState>): OrchestraState {
    const instruments = (orchestra.instruments ?? []).map((i: Partial<InstrumentState>) => ({
        ...i,
        nodes: (i.nodes ?? default_nodes()).map((n) => ({ ...n, position: { x: 0, y: 0 } })),
        edges: i.edges ?? [],
        code: i.code ?? '',
    })) as InstrumentState[];
    const global_nodes = sync_global_in_nodes(
        instruments,
        (orchestra.global_nodes ?? default_global_nodes()).map((n) => ({
            ...n,
            position: { x: 0, y: 0 },
        })),
    );
    return {
        bpm: orchestra.bpm ?? 120,
        active_id: orchestra.active_id ?? null,
        instruments,
        code: orchestra.code ?? '',
        global_nodes,
        global_edges: orchestra.global_edges ?? [],
        global_patch_code: orchestra.global_patch_code ?? '',
        score_code: orchestra.score_code ?? '',
    };
}

function load_initial_patch(): PatchState {
    try {
        const saved = localStorage.getItem(STORAGE_KEY);
        if (saved) {
            const { orchestra } = JSON.parse(saved);
            if (orchestra) {
                return {
                    orchestra: normalize_orchestra(orchestra),
                    selected_id: null,
                    view: 'instrument',
                };
            }
        }
    } catch (_e) {
        console.error(_e);
    }
    return { orchestra: DEFAULT_ORCHESTRA, selected_id: null, view: 'instrument' };
}

function load_initial(): HistoryState {
    return { past: [], present: load_initial_patch(), future: [] };
}

function get_active(state: PatchState): InstrumentState | undefined {
    return state.orchestra.instruments.find((i) => i.id === state.orchestra.active_id);
}

function map_active(state: PatchState, fn: (i: InstrumentState) => InstrumentState): PatchState {
    return {
        ...state,
        orchestra: {
            ...state.orchestra,
            instruments: state.orchestra.instruments.map((i) =>
                i.id === state.orchestra.active_id ? fn(i) : i,
            ),
        },
    };
}

function patch_reducer(state: PatchState, action: PatchAction): PatchState {
    switch (action.type) {
        case 'nodes_change': {
            const active = get_active(state);
            if (!active) return state;
            const filtered_changes = action.changes.filter(
                (c) => !(c.type === 'remove' && (c.id === 'capture' || c.id === 'out')),
            );
            const removed_ids = new Set(
                filtered_changes
                    .filter((c): c is NodeChange & { type: 'remove' } => c.type === 'remove')
                    .map((c) => c.id),
            );
            return map_active(state, (i) => ({
                ...i,
                nodes: applyNodeChanges(filtered_changes, i.nodes),
                edges:
                    removed_ids.size > 0
                        ? i.edges.filter(
                              (e) =>
                                  (!removed_ids.has(e.source) && !removed_ids.has(e.target)) ||
                                  e.source === 'capture' ||
                                  e.source === 'out' ||
                                  e.target === 'capture' ||
                                  e.target === 'out',
                          )
                        : i.edges,
            }));
        }
        case 'edges_change': {
            const active = get_active(state);
            if (!active) return state;
            const edge_ids = new Set(active.edges.map((e) => e.id));
            const relevant = action.changes.filter((c) => c.type !== 'add' && edge_ids.has(c.id));
            if (relevant.length === 0) return state;
            return map_active(state, (i) => ({
                ...i,
                edges: applyEdgeChanges(relevant, i.edges),
            }));
        }
        case 'connect': {
            const active = get_active(state);
            if (!active) return state;
            const { source, sourceHandle, target, targetHandle } = action.connection;
            const occupied = active.edges.some(
                (e) => e.target === target && e.targetHandle === (targetHandle ?? null),
            );
            if (occupied) return state;
            const new_edge: Edge = {
                id: `e_${source}_${sourceHandle}_${target}_${targetHandle}`,
                source,
                sourceHandle: sourceHandle ?? null,
                target,
                targetHandle: targetHandle ?? null,
                type: undefined,
            };
            return map_active(state, (i) => ({ ...i, edges: [...i.edges, new_edge] }));
        }
        case 'global_nodes_change': {
            const filtered_changes = action.changes.filter(
                (c) =>
                    !(
                        c.type === 'remove' &&
                        state.orchestra.global_nodes.find((n) => n.id === c.id)?.type !== 'block'
                    ),
            );
            const removed_ids = new Set(
                filtered_changes
                    .filter((c): c is NodeChange & { type: 'remove' } => c.type === 'remove')
                    .map((c) => c.id),
            );
            return {
                ...state,
                orchestra: {
                    ...state.orchestra,
                    global_nodes: applyNodeChanges(filtered_changes, state.orchestra.global_nodes),
                    global_edges:
                        removed_ids.size > 0
                            ? state.orchestra.global_edges.filter(
                                  (e) => !removed_ids.has(e.source) && !removed_ids.has(e.target),
                              )
                            : state.orchestra.global_edges,
                },
            };
        }
        case 'global_edges_change': {
            const edge_ids = new Set(state.orchestra.global_edges.map((e) => e.id));
            const relevant = action.changes.filter((c) => c.type !== 'add' && edge_ids.has(c.id));
            if (relevant.length === 0) return state;
            return {
                ...state,
                orchestra: {
                    ...state.orchestra,
                    global_edges: applyEdgeChanges(relevant, state.orchestra.global_edges),
                },
            };
        }
        case 'global_connect': {
            const { source, sourceHandle, target, targetHandle } = action.connection;
            const occupied = state.orchestra.global_edges.some(
                (e) => e.target === target && e.targetHandle === (targetHandle ?? null),
            );
            if (occupied) return state;
            const new_edge: Edge = {
                id: `e_${source}_${sourceHandle}_${target}_${targetHandle}`,
                source,
                sourceHandle: sourceHandle ?? null,
                target,
                targetHandle: targetHandle ?? null,
                type: undefined,
            };
            return {
                ...state,
                orchestra: {
                    ...state.orchestra,
                    global_edges: [...state.orchestra.global_edges, new_edge],
                },
            };
        }
        case 'set_view':
            return { ...state, selected_id: null, view: action.view };
        case 'select':
            return { ...state, selected_id: action.id };
        case 'add_node':
            if (!get_active(state)) return state;
            return map_active(state, (i) => ({ ...i, nodes: [...i.nodes, action.node] }));
        case 'update_code': {
            if (!get_active(state)) return state;
            const arity = scan_arity(action.code);
            const params = scan_params(action.code);
            return map_active(state, (i) => ({
                ...i,
                edges: i.edges.filter((e) => {
                    if (e.source === action.id) {
                        const idx = parseInt((e.sourceHandle ?? '').replace('out_', ''));
                        if (!isNaN(idx) && idx >= arity.num_outputs) return false;
                    }
                    if (e.target === action.id) {
                        const idx = parseInt((e.targetHandle ?? '').replace('in_', ''));
                        if (!isNaN(idx) && idx >= arity.num_inputs) return false;
                    }
                    return true;
                }),
                nodes: i.nodes.map((n) =>
                    n.id === action.id
                        ? { ...n, data: { ...n.data, code: action.code, ...arity, params } }
                        : n,
                ),
            }));
        }
        case 'update_name': {
            const active = get_active(state);
            if (!active) return state;
            const target_node = active.nodes.find((n) => n.id === action.id);
            if (target_node?.type === 'out') {
                const num_channels = parse_out_name(action.name);
                if (num_channels === null) return state;
                const next_state = map_active(state, (i) => ({
                    ...i,
                    nodes: i.nodes.map((n) =>
                        n.id === action.id
                            ? { ...n, data: { name: `OUT ${num_channels}`, num_channels } }
                            : n,
                    ),
                }));
                const updated_active = get_active(next_state)!;
                const in_id = in_node_id(updated_active.id);
                return {
                    ...next_state,
                    orchestra: {
                        ...next_state.orchestra,
                        global_nodes: sync_global_in_nodes(
                            next_state.orchestra.instruments.map((i) =>
                                i.id === updated_active.id ? updated_active : i,
                            ),
                            next_state.orchestra.global_nodes,
                        ),
                        global_edges: next_state.orchestra.global_edges.filter((e) => {
                            if (e.source !== in_id) return true;
                            const idx = parseInt((e.sourceHandle ?? '').replace('in_', ''));
                            return isNaN(idx) || idx < num_channels;
                        }),
                    },
                };
            }
            return map_active(state, (i) => ({
                ...i,
                nodes: i.nodes.map((n) =>
                    n.id === action.id ? { ...n, data: { ...n.data, name: action.name } } : n,
                ),
            }));
        }
        case 'add_global_node':
            return {
                ...state,
                orchestra: {
                    ...state.orchestra,
                    global_nodes: [...state.orchestra.global_nodes, action.node],
                },
            };
        case 'update_global_code': {
            const arity = scan_arity(action.code);
            const params = scan_params(action.code);
            return {
                ...state,
                orchestra: {
                    ...state.orchestra,
                    global_edges: state.orchestra.global_edges.filter((e) => {
                        if (e.source === action.id) {
                            const idx = parseInt((e.sourceHandle ?? '').replace('out_', ''));
                            if (!isNaN(idx) && idx >= arity.num_outputs) return false;
                        }
                        if (e.target === action.id) {
                            const idx = parseInt((e.targetHandle ?? '').replace('in_', ''));
                            if (!isNaN(idx) && idx >= arity.num_inputs) return false;
                        }
                        return true;
                    }),
                    global_nodes: state.orchestra.global_nodes.map((n) =>
                        n.id === action.id
                            ? { ...n, data: { ...n.data, code: action.code, ...arity, params } }
                            : n,
                    ),
                },
            };
        }
        case 'update_global_name':
            return {
                ...state,
                orchestra: {
                    ...state.orchestra,
                    global_nodes: state.orchestra.global_nodes.map((n) =>
                        n.id === action.id ? { ...n, data: { ...n.data, name: action.name } } : n,
                    ),
                },
            };
        case 'set_orchestra_bpm':
            return { ...state, orchestra: { ...state.orchestra, bpm: action.bpm } };
        case 'add_instrument': {
            const n = state.orchestra.instruments.length + 1;
            const instr: InstrumentState = {
                id: `instr_${Date.now()}`,
                name: `instrument${n}`,
                nodes: default_nodes(),
                edges: [],
                code: '',
            };
            const instruments = [...state.orchestra.instruments, instr];
            return {
                ...state,
                selected_id: null,
                orchestra: {
                    ...state.orchestra,
                    active_id: instr.id,
                    instruments,
                    global_nodes: sync_global_in_nodes(instruments, state.orchestra.global_nodes),
                },
            };
        }
        case 'remove_instrument': {
            const remaining = state.orchestra.instruments.filter((i) => i.id !== action.id);
            const active_id =
                state.orchestra.active_id === action.id
                    ? (remaining[remaining.length - 1]?.id ?? null)
                    : state.orchestra.active_id;
            const in_id = in_node_id(action.id);
            return {
                ...state,
                selected_id: null,
                orchestra: {
                    ...state.orchestra,
                    active_id,
                    instruments: remaining,
                    global_nodes: sync_global_in_nodes(remaining, state.orchestra.global_nodes),
                    global_edges: state.orchestra.global_edges.filter(
                        (e) => e.source !== in_id && e.target !== in_id,
                    ),
                },
            };
        }
        case 'set_instrument_code':
            return {
                ...state,
                orchestra: {
                    ...state.orchestra,
                    instruments: state.orchestra.instruments.map((i) =>
                        i.id === action.id ? { ...i, code: action.code } : i,
                    ),
                },
            };
        case 'rename_instrument': {
            const instruments = state.orchestra.instruments.map((i) =>
                i.id === action.id ? { ...i, name: action.name } : i,
            );
            return {
                ...state,
                orchestra: {
                    ...state.orchestra,
                    instruments,
                    global_nodes: sync_global_in_nodes(instruments, state.orchestra.global_nodes),
                },
            };
        }
        case 'set_active_instrument':
            return {
                ...state,
                selected_id: null,
                orchestra: { ...state.orchestra, active_id: action.id },
            };
        case 'set_orchestra_code':
            return { ...state, orchestra: { ...state.orchestra, code: action.code } };
        case 'set_global_patch_code':
            return { ...state, orchestra: { ...state.orchestra, global_patch_code: action.code } };
        case 'set_score_code':
            return { ...state, orchestra: { ...state.orchestra, score_code: action.code } };
        case 'load':
            return {
                orchestra: normalize_orchestra(action.orchestra),
                selected_id: null,
                view: 'instrument',
            };
        case 'apply_layout':
            return {
                ...state,
                orchestra: {
                    ...state.orchestra,
                    instruments: state.orchestra.instruments.map((i) =>
                        i.id === action.instrument_id ? { ...i, nodes: action.nodes } : i,
                    ),
                },
            };
        case 'apply_global_layout':
            return {
                ...state,
                orchestra: { ...state.orchestra, global_nodes: action.nodes },
            };
        default:
            return state;
    }
}

function serialize_orchestra(orchestra: OrchestraState) {
    return {
        bpm: orchestra.bpm,
        active_id: orchestra.active_id,
        code: orchestra.code,
        instruments: orchestra.instruments.map((i) => ({
            id: i.id,
            name: i.name,
            code: i.code,
            nodes: i.nodes.map((n) => ({
                id: n.id,
                type: n.type,
                data: n.data,
            })),
            edges: i.edges.map((e) => ({
                id: e.id,
                type: e.type,
                source: e.source,
                sourceHandle: e.sourceHandle,
                target: e.target,
                targetHandle: e.targetHandle,
            })),
        })),
        global_nodes: orchestra.global_nodes.map((n) => ({
            id: n.id,
            type: n.type,
            data: n.data,
        })),
        global_edges: orchestra.global_edges.map((e) => ({
            id: e.id,
            type: e.type,
            source: e.source,
            sourceHandle: e.sourceHandle,
            target: e.target,
            targetHandle: e.targetHandle,
        })),
        global_patch_code: orchestra.global_patch_code,
        score_code: orchestra.score_code,
    };
}

function save(state: PatchState): void {
    try {
        localStorage.setItem(
            STORAGE_KEY,
            JSON.stringify({ orchestra: serialize_orchestra(state.orchestra) }),
        );
    } catch (_e) {
        console.error(_e);
    }
}

function reducer(history: HistoryState, action: PatchAction): HistoryState {
    if (action.type === 'undo') {
        if (history.past.length === 0) return history;
        const prev = history.past[history.past.length - 1];
        save(prev);
        return {
            past: history.past.slice(0, -1),
            present: prev,
            future: [history.present, ...history.future],
        };
    }
    if (action.type === 'redo') {
        if (history.future.length === 0) return history;
        const next = history.future[0];
        save(next);
        return {
            past: [...history.past, history.present],
            present: next,
            future: history.future.slice(1),
        };
    }

    const next_present = patch_reducer(history.present, action);
    if (next_present === history.present) return history;

    save(next_present);

    const only_selection =
        (action.type === 'nodes_change' && action.changes.every((c) => c.type === 'select')) ||
        (action.type === 'edges_change' && action.changes.every((c) => c.type === 'select'));

    if (NO_HISTORY.has(action.type) || only_selection) {
        return { ...history, present: next_present };
    }
    return {
        past: [...history.past, history.present],
        present: next_present,
        future: [],
    };
}

export function usePatchStore() {
    const [history, dispatch] = useReducer(reducer, undefined, load_initial);
    const state = history.present;

    const active_instrument =
        state.orchestra.instruments.find((i) => i.id === state.orchestra.active_id) ?? null;
    const view = state.view;
    const nodes =
        view === 'global' ? state.orchestra.global_nodes : (active_instrument?.nodes ?? []);
    const edges =
        view === 'global' ? state.orchestra.global_edges : (active_instrument?.edges ?? []);

    const [layout_serial, set_layout_serial] = useState(0);

    const active_instrument_ref = useRef(active_instrument);
    useEffect(() => {
        active_instrument_ref.current = active_instrument;
    });

    useEffect(() => {
        if (view !== 'instrument') return;
        const instrument = active_instrument_ref.current;
        if (!instrument) return;
        const id = instrument.id;
        elk_layout(instrument.nodes, instrument.edges).then((laid_out) => {
            dispatch({ type: 'apply_layout', instrument_id: id, nodes: laid_out });
            set_layout_serial((s) => s + 1);
        });
    }, [state.orchestra.active_id, view]);

    const global_nodes_ref = useRef(state.orchestra.global_nodes);
    const global_edges_ref = useRef(state.orchestra.global_edges);
    useEffect(() => {
        global_nodes_ref.current = state.orchestra.global_nodes;
        global_edges_ref.current = state.orchestra.global_edges;
    });

    useEffect(() => {
        if (view !== 'global') return;
        elk_layout(global_nodes_ref.current, global_edges_ref.current).then((laid_out) => {
            dispatch({ type: 'apply_global_layout', nodes: laid_out });
            set_layout_serial((s) => s + 1);
        });
    }, [view]);

    const selected_node = nodes.find((n) => n.id === state.selected_id) ?? null;

    const on_nodes_change = useCallback(
        (changes: NodeChange[]) =>
            dispatch(
                view === 'global'
                    ? { type: 'global_nodes_change', changes }
                    : { type: 'nodes_change', changes },
            ),
        [view],
    );
    const on_edges_change = useCallback(
        (changes: EdgeChange[]) =>
            dispatch(
                view === 'global'
                    ? { type: 'global_edges_change', changes }
                    : { type: 'edges_change', changes },
            ),
        [view],
    );
    const on_connect = useCallback(
        (connection: Connection) =>
            dispatch(
                view === 'global'
                    ? { type: 'global_connect', connection }
                    : { type: 'connect', connection },
            ),
        [view],
    );
    const select = useCallback((id: string | null) => dispatch({ type: 'select', id }), []);
    const set_view = useCallback((v: PatchView) => dispatch({ type: 'set_view', view: v }), []);
    const add_block = useCallback(
        (name: string, position: { x: number; y: number }) => {
            const id = `block_${Date.now()}`;
            const code = get_default_code(name);
            const arity = scan_arity(code);
            const params = scan_params(code);
            const node: Node = {
                id,
                type: 'block',
                position,
                data: {
                    name,
                    code,
                    ...arity,
                    params,
                } satisfies BlockData,
            };
            dispatch(
                view === 'global' ? { type: 'add_global_node', node } : { type: 'add_node', node },
            );
            dispatch({ type: 'select', id });
        },
        [view],
    );
    const update_code = useCallback(
        (id: string, code: string) =>
            dispatch(
                view === 'global'
                    ? { type: 'update_global_code', id, code }
                    : { type: 'update_code', id, code },
            ),
        [view],
    );
    const update_name = useCallback(
        (id: string, name: string) =>
            dispatch(
                view === 'global'
                    ? { type: 'update_global_name', id, name }
                    : { type: 'update_name', id, name },
            ),
        [view],
    );
    const set_orchestra_bpm = useCallback(
        (bpm: number) => dispatch({ type: 'set_orchestra_bpm', bpm }),
        [],
    );
    const add_instrument = useCallback(() => dispatch({ type: 'add_instrument' }), []);
    const remove_instrument = useCallback(
        (id: string) => dispatch({ type: 'remove_instrument', id }),
        [],
    );
    const set_instrument_code = useCallback(
        (id: string, code: string) => dispatch({ type: 'set_instrument_code', id, code }),
        [],
    );
    const rename_instrument = useCallback(
        (id: string, name: string) => dispatch({ type: 'rename_instrument', id, name }),
        [],
    );
    const set_active_instrument = useCallback(
        (id: string) => dispatch({ type: 'set_active_instrument', id }),
        [],
    );
    const set_orchestra_code = useCallback(
        (code: string) => dispatch({ type: 'set_orchestra_code', code }),
        [],
    );
    const set_global_patch_code = useCallback(
        (code: string) => dispatch({ type: 'set_global_patch_code', code }),
        [],
    );
    const set_score_code = useCallback(
        (code: string) => dispatch({ type: 'set_score_code', code }),
        [],
    );

    const undo = useCallback(() => dispatch({ type: 'undo' }), []);
    const redo = useCallback(() => dispatch({ type: 'redo' }), []);

    const export_patch = useCallback(() => {
        const blob = new Blob([JSON.stringify({ orchestra: state.orchestra }, null, 2)], {
            type: 'application/json',
        });
        const url = URL.createObjectURL(blob);
        const a = document.createElement('a');
        a.href = url;
        a.download = 'patch.json';
        a.click();
        URL.revokeObjectURL(url);
    }, [state.orchestra]);

    const import_patch = useCallback((file: File) => {
        const reader = new FileReader();
        reader.onload = (e) => {
            try {
                const { orchestra } = JSON.parse(e.target!.result as string);
                if (orchestra) dispatch({ type: 'load', orchestra });
            } catch (_e) {
                console.error(_e);
            }
        };
        reader.readAsText(file);
    }, []);

    const load_patch = useCallback(
        (orchestra: OrchestraState) => dispatch({ type: 'load', orchestra }),
        [],
    );

    return {
        orchestra: state.orchestra,
        nodes,
        edges,
        view,
        set_view,
        selected_id: state.selected_id,
        selected_node,
        can_undo: history.past.length > 0,
        can_redo: history.future.length > 0,
        undo,
        redo,
        on_nodes_change,
        on_edges_change,
        on_connect,
        select,
        add_block,
        update_code,
        update_name,
        set_orchestra_bpm,
        add_instrument,
        remove_instrument,
        set_instrument_code,
        rename_instrument,
        set_active_instrument,
        set_orchestra_code,
        set_global_patch_code,
        set_score_code,
        export_patch,
        import_patch,
        load_patch,
        layout_serial,
    };
}
