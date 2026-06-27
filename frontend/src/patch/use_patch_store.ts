import { useCallback, useReducer } from 'react';
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
    | { type: 'load'; orchestra: OrchestraState };

const STORAGE_KEY = 'wasmwasm_patch';

const default_nodes = (): Node[] => [
    { id: 'capture', type: 'capture', position: { x: 100, y: 50 }, data: {} },
    { id: 'dac', type: 'dac', position: { x: 100, y: 400 }, data: {} },
];

const DEFAULT_ORCHESTRA: OrchestraState = { bpm: 120, active_id: null, instruments: [], code: '' };

function load_initial(): PatchState {
    try {
        const saved = localStorage.getItem(STORAGE_KEY);
        if (saved) {
            const { orchestra } = JSON.parse(saved);
            if (orchestra) {
                orchestra.instruments = (orchestra.instruments ?? []).map(
                    (i: Partial<InstrumentState>) => ({
                        ...i,
                        nodes: i.nodes ?? default_nodes(),
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

function reducer(state: PatchState, action: PatchAction): PatchState {
    let next: PatchState;
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
            next = map_active(state, (i) => ({
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
            break;
        }
        case 'edges_change': {
            if (!get_active(state)) return state;
            next = map_active(state, (i) => ({
                ...i,
                edges: applyEdgeChanges(action.changes, i.edges),
            }));
            break;
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
                type: source === target ? 'self_loop' : undefined,
            };
            next = map_active(state, (i) => ({ ...i, edges: [...i.edges, new_edge] }));
            break;
        }
        case 'select':
            return { ...state, selected_id: action.id };
        case 'add_node':
            if (!get_active(state)) return state;
            next = map_active(state, (i) => ({ ...i, nodes: [...i.nodes, action.node] }));
            break;
        case 'update_code': {
            if (!get_active(state)) return state;
            const arity = scan_arity(action.code);
            const params = scan_params(action.code);
            next = map_active(state, (i) => ({
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
            break;
        }
        case 'update_name':
            if (!get_active(state)) return state;
            next = map_active(state, (i) => ({
                ...i,
                nodes: i.nodes.map((n) =>
                    n.id === action.id ? { ...n, data: { ...n.data, name: action.name } } : n,
                ),
            }));
            break;
        case 'set_orchestra_bpm':
            next = { ...state, orchestra: { ...state.orchestra, bpm: action.bpm } };
            break;
        case 'add_instrument': {
            const n = state.orchestra.instruments.length + 1;
            const instr: InstrumentState = {
                id: `instr_${Date.now()}`,
                name: `instrument${n}`,
                nodes: default_nodes(),
                edges: [],
                code: '',
            };
            next = {
                ...state,
                selected_id: null,
                orchestra: {
                    ...state.orchestra,
                    active_id: instr.id,
                    instruments: [...state.orchestra.instruments, instr],
                },
            };
            break;
        }
        case 'remove_instrument': {
            const remaining = state.orchestra.instruments.filter((i) => i.id !== action.id);
            const active_id =
                state.orchestra.active_id === action.id
                    ? (remaining[remaining.length - 1]?.id ?? null)
                    : state.orchestra.active_id;
            next = {
                ...state,
                selected_id: null,
                orchestra: { ...state.orchestra, active_id, instruments: remaining },
            };
            break;
        }
        case 'set_instrument_code':
            next = {
                ...state,
                orchestra: {
                    ...state.orchestra,
                    instruments: state.orchestra.instruments.map((i) =>
                        i.id === action.id ? { ...i, code: action.code } : i,
                    ),
                },
            };
            break;
        case 'rename_instrument':
            next = {
                ...state,
                orchestra: {
                    ...state.orchestra,
                    instruments: state.orchestra.instruments.map((i) =>
                        i.id === action.id ? { ...i, name: action.name } : i,
                    ),
                },
            };
            break;
        case 'set_active_instrument':
            next = {
                ...state,
                selected_id: null,
                orchestra: { ...state.orchestra, active_id: action.id },
            };
            break;
        case 'set_orchestra_code':
            next = { ...state, orchestra: { ...state.orchestra, code: action.code } };
            break;
        case 'load':
            next = { orchestra: action.orchestra, selected_id: null };
            break;
        default:
            return state;
    }
    try {
        localStorage.setItem(STORAGE_KEY, JSON.stringify({ orchestra: next.orchestra }));
    } catch (_e) {
        console.error(_e);
    }
    return next;
}

export function usePatchStore() {
    const [state, dispatch] = useReducer(reducer, undefined, load_initial);

    const active_instrument =
        state.orchestra.instruments.find((i) => i.id === state.orchestra.active_id) ?? null;
    const nodes = active_instrument?.nodes ?? [];
    const edges = active_instrument?.edges ?? [];
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
    };
}
