import { useCallback, useEffect, useReducer, useRef, useState } from 'react';
import ELK from 'elkjs/lib/elk.bundled.js';
import { get_default_code } from './block_templates';
import { applyNodeChanges, applyEdgeChanges } from '@xyflow/react';
import type { Node, Edge, NodeChange, EdgeChange, Connection } from '@xyflow/react';

export interface BlockData {
    name: string;
    code: string;
    num_inputs: number;
    num_outputs: number;
    params: string[];
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
}

interface PatchState {
    orchestra: OrchestraState;
    selected_id: string | null;
}

type PatchAction =
    | { type: 'nodes_change'; changes: NodeChange[] }
    | { type: 'edges_change'; changes: EdgeChange[] }
    | { type: 'connect'; connection: Connection }
    | { type: 'select'; id: string | null }
    | { type: 'update_code'; id: string; code: string }
    | { type: 'update_name'; id: string; name: string }
    | { type: 'add_node'; node: Node }
    | { type: 'set_orchestra_bpm'; bpm: number }
    | { type: 'add_instrument' }
    | { type: 'remove_instrument'; id: string }
    | { type: 'set_instrument_code'; id: string; code: string }
    | { type: 'rename_instrument'; id: string; name: string }
    | { type: 'set_active_instrument'; id: string }
    | { type: 'set_orchestra_code'; code: string }
    | { type: 'load'; orchestra: OrchestraState }
    | { type: 'apply_layout'; instrument_id: string; nodes: Node[] }
    | { type: 'undo' }
    | { type: 'redo' };

const NO_HISTORY = new Set<PatchAction['type']>([
    'select',
    'set_active_instrument',
    'set_instrument_code',
    'set_orchestra_code',
    'apply_layout',
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

const NODE_W = 200;
const NODE_H = 40;

function spaced(count: number, width: number): number[] {
    return Array.from({ length: count }, (_, i) => (width * (i + 1)) / (count + 1));
}

function node_ports(node: Node) {
    if (node.type === 'capture') {
        const [xl, xr] = spaced(2, NODE_W);
        return [
            { id: `${node.id}__capture_l`, x: xl, y: NODE_H },
            { id: `${node.id}__capture_r`, x: xr, y: NODE_H },
        ];
    }
    if (node.type === 'dac') {
        const [xl, xr] = spaced(2, NODE_W);
        return [
            { id: `${node.id}__dac_l`, x: xl, y: 0 },
            { id: `${node.id}__dac_r`, x: xr, y: 0 },
        ];
    }
    const data = node.data as Partial<BlockData>;
    const num_in = data.num_inputs ?? 0;
    const num_out = data.num_outputs ?? 0;
    const ports = [];
    for (const [i, x] of spaced(num_in, NODE_W).entries())
        ports.push({ id: `${node.id}__in_${i}`, x, y: 0 });
    for (const [i, x] of spaced(num_out, NODE_W).entries())
        ports.push({ id: `${node.id}__out_${i}`, x, y: NODE_H });
    return ports;
}

async function elk_layout(nodes: Node[], edges: Edge[]): Promise<Node[]> {
    const children = nodes.map((n) => ({
        width: 160,
        height: 40,
        ports: node_ports(n),
        layoutOptions: { 'elk.portConstraints': 'FIXED_POS' },
        x: 0,
        y: 0,
        ...n,
    }));
    const capture_node_idx = children.findIndex((x) => x.type === 'capture');
    const capture_node = children[capture_node_idx];
    children.splice(capture_node_idx, 1);
    const dac_node_idx = children.findIndex((x) => x.type === 'dac');
    const dac_node = children[dac_node_idx];
    children.splice(dac_node_idx, 1);
    const graph = {
        id: 'root',
        layoutOptions: {
            'elk.algorithm': 'layered',
            'elk.direction': 'DOWN',
            'elk.layered.spacing.nodeNodeBetweenLayers': '20',
            'elk.spacing.nodeNode': '10',
            'elk.spacing.edgeNode': '5',
            'elk.spacing.edgeEdge': '5',
            'elk.layered.spacing.edgeNodeBetweenLayers': '5',
            'elk.separateConnectedComponents': 'false',
            'elk.layered.crossingMinimization.strategy': 'LAYER_SWEEP',
            'elk.layered.crossingMinimization.greedySwitch.type': 'TWO_SIDED',
            'elk.layered.nodePlacement.strategy': 'BRANDES_KOEPF',
            'elk.layered.nodePlacement.bk.fixedAlignment': 'BALANCED',
            'elk.layered.compaction.postCompaction.strategy': 'EDGE_LENGTH',
            'elk.layered.compaction.postCompaction.constraints': 'QUADRATIC',
            'elk.edgeRouting': 'SPLINES',
        },
        children: children,
        edges: edges
            .filter(
                (e) =>
                    e.source !== e.target &&
                    e.sourceHandle &&
                    e.targetHandle &&
                    e.source != 'capture' &&
                    e.target != 'dac',
            )
            .map((e) => ({
                id: e.id,
                sources: [`${e.source}__${e.sourceHandle}`],
                targets: [`${e.target}__${e.targetHandle}`],
            })),
    };
    const laid_out = await elk.layout(graph);

    const all_lefts = children.map((c) => c.x ?? 0);
    const all_rights = children.map((c) => c.x ?? 0);
    const all_tops = children.map((c) => c.y ?? 0);
    const all_bottoms = children.map((c) => (c.y ?? 0) + (c.height ?? NODE_H));

    const min_x = all_lefts.length > 0 ? Math.min(...all_lefts) : 0;
    const max_x = all_rights.length > 0 ? Math.max(...all_rights) : 0;
    const min_y = all_tops.length > 0 ? Math.min(...all_tops) : 0;
    const max_y = all_bottoms.length > 0 ? Math.max(...all_bottoms) : 0;

    const margin = 50;

    capture_node.x = (max_x + min_x) / 2;
    capture_node.y = min_y - NODE_H - margin;
    dac_node.x = (max_x + min_x) / 2;
    dac_node.y = max_y + margin;

    return nodes.map((n) => {
        const child = [capture_node, dac_node, ...(laid_out.children ?? [])].find(
            (c) => c.id === n.id,
        );
        return child ? { ...n, position: { x: child.x ?? 0, y: child.y ?? 0 } } : n;
    });
}

const default_nodes = (): Node[] => [
    { id: 'capture', type: 'capture', position: { x: 0, y: 0 }, data: {} },
    { id: 'dac', type: 'dac', position: { x: 0, y: 0 }, data: {} },
];

const DEFAULT_ORCHESTRA: OrchestraState = { bpm: 120, active_id: null, instruments: [], code: '' };

function load_initial_patch(): PatchState {
    try {
        const saved = localStorage.getItem(STORAGE_KEY);
        if (saved) {
            const { orchestra } = JSON.parse(saved);
            if (orchestra) {
                orchestra.instruments = (orchestra.instruments ?? []).map(
                    (i: Partial<InstrumentState>) => ({
                        ...i,
                        nodes: (i.nodes ?? default_nodes()).map((n) => ({
                            ...n,
                            position: { x: 0, y: 0 },
                        })),
                        edges: i.edges ?? [],
                        code: i.code ?? '',
                    }),
                );
                orchestra.code = orchestra.code ?? '';
                return { orchestra, selected_id: null };
            }
        }
    } catch (_e) {
        console.error(_e);
    }
    return { orchestra: DEFAULT_ORCHESTRA, selected_id: null };
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
                (c) => !(c.type === 'remove' && (c.id === 'capture' || c.id === 'dac')),
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
                                  e.source === 'dac' ||
                                  e.target === 'capture' ||
                                  e.target === 'dac',
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
        case 'update_name':
            if (!get_active(state)) return state;
            return map_active(state, (i) => ({
                ...i,
                nodes: i.nodes.map((n) =>
                    n.id === action.id ? { ...n, data: { ...n.data, name: action.name } } : n,
                ),
            }));
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
            return {
                ...state,
                selected_id: null,
                orchestra: {
                    ...state.orchestra,
                    active_id: instr.id,
                    instruments: [...state.orchestra.instruments, instr],
                },
            };
        }
        case 'remove_instrument': {
            const remaining = state.orchestra.instruments.filter((i) => i.id !== action.id);
            const active_id =
                state.orchestra.active_id === action.id
                    ? (remaining[remaining.length - 1]?.id ?? null)
                    : state.orchestra.active_id;
            return {
                ...state,
                selected_id: null,
                orchestra: { ...state.orchestra, active_id, instruments: remaining },
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
        case 'rename_instrument':
            return {
                ...state,
                orchestra: {
                    ...state.orchestra,
                    instruments: state.orchestra.instruments.map((i) =>
                        i.id === action.id ? { ...i, name: action.name } : i,
                    ),
                },
            };
        case 'set_active_instrument':
            return {
                ...state,
                selected_id: null,
                orchestra: { ...state.orchestra, active_id: action.id },
            };
        case 'set_orchestra_code':
            return { ...state, orchestra: { ...state.orchestra, code: action.code } };
        case 'load':
            return { orchestra: action.orchestra, selected_id: null };
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
    const nodes = active_instrument?.nodes ?? [];
    const edges = active_instrument?.edges ?? [];

    const [layout_serial, set_layout_serial] = useState(0);

    const active_instrument_ref = useRef(active_instrument);
    useEffect(() => {
        active_instrument_ref.current = active_instrument;
    });

    useEffect(() => {
        const instrument = active_instrument_ref.current;
        if (!instrument) return;
        const id = instrument.id;
        elk_layout(instrument.nodes, instrument.edges).then((laid_out) => {
            dispatch({ type: 'apply_layout', instrument_id: id, nodes: laid_out });
            set_layout_serial((s) => s + 1);
        });
    }, [state.orchestra.active_id]);
    const selected_node = nodes.find((n) => n.id === state.selected_id) ?? null;

    const on_nodes_change = useCallback(
        (changes: NodeChange[]) => dispatch({ type: 'nodes_change', changes }),
        [],
    );
    const on_edges_change = useCallback(
        (changes: EdgeChange[]) => dispatch({ type: 'edges_change', changes }),
        [],
    );
    const on_connect = useCallback(
        (connection: Connection) => dispatch({ type: 'connect', connection }),
        [],
    );
    const select = useCallback((id: string | null) => dispatch({ type: 'select', id }), []);
    const add_block = useCallback((name: string, position: { x: number; y: number }) => {
        const id = `block_${Date.now()}`;
        const code = get_default_code(name);
        const arity = scan_arity(code);
        const params = scan_params(code);
        dispatch({
            type: 'add_node',
            node: {
                id,
                type: 'block',
                position,
                data: {
                    name,
                    code,
                    ...arity,
                    params,
                } satisfies BlockData,
            },
        });
        dispatch({ type: 'select', id });
    }, []);
    const update_code = useCallback(
        (id: string, code: string) => dispatch({ type: 'update_code', id, code }),
        [],
    );
    const update_name = useCallback(
        (id: string, name: string) => dispatch({ type: 'update_name', id, name }),
        [],
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
        export_patch,
        import_patch,
        load_patch,
        layout_serial,
    };
}
