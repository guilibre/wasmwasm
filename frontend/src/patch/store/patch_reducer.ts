import { applyNodeChanges, applyEdgeChanges } from '@xyflow/react';
import type { Edge, NodeChange } from '@xyflow/react';
import { scan_arity, scan_params, parse_out_name } from './code_scanning';
import {
    default_nodes,
    default_global_nodes,
    unique_instrument_id,
    in_node_id,
    sync_global_in_nodes,
} from './instrument_sync';
import type {
    OrchestraState,
    PatchState,
    PatchAction,
    HistoryState,
    InstrumentState,
    ScoreParamBindings,
} from './patch_types';

const NO_HISTORY = new Set<PatchAction['type']>([
    'select',
    'set_view',
    'set_active_instrument',
    'apply_layout',
    'apply_global_layout',
    'load',
    'undo',
    'redo',
]);

const STORAGE_KEY = 'wasmwasm_patch';
const DEFAULT_BPM = 120;

const DEFAULT_ORCHESTRA: OrchestraState = {
    bpm: DEFAULT_BPM,
    active_id: null,
    instruments: [],
    global_nodes: default_global_nodes(),
    global_edges: [],
};

function normalize_orchestra(orchestra: Partial<OrchestraState>): OrchestraState {
    const instruments = (orchestra.instruments ?? []).map((i: Partial<InstrumentState>) => ({
        ...i,
        nodes: (i.nodes ?? default_nodes()).map((n) => ({ ...n, position: { x: 0, y: 0 } })),
        edges: i.edges ?? [],
    })) as InstrumentState[];
    const global_nodes = sync_global_in_nodes(
        instruments,
        (orchestra.global_nodes ?? default_global_nodes()).map((n) => ({
            ...n,
            position: { x: 0, y: 0 },
        })),
    );
    return {
        bpm: orchestra.bpm ?? DEFAULT_BPM,
        active_id: orchestra.active_id ?? null,
        instruments,
        global_nodes,
        global_edges: orchestra.global_edges ?? [],
    };
}

function normalize_score_param_bindings(
    bindings: ScoreParamBindings | undefined,
): ScoreParamBindings {
    const normalized: ScoreParamBindings = {};
    for (const [instrument_id, source] of Object.entries(bindings ?? {})) {
        if (typeof source === 'string') normalized[instrument_id] = source;
    }
    return normalized;
}

function load_initial_patch(): PatchState {
    try {
        const saved = localStorage.getItem(STORAGE_KEY);
        if (saved) {
            const { orchestra, score_source, score_param_bindings, global_callback_source } =
                JSON.parse(saved);
            if (orchestra) {
                return {
                    orchestra: normalize_orchestra(orchestra),
                    selected_id: null,
                    view: 'instrument',
                    score_source: (score_source as string | undefined) ?? '',
                    score_param_bindings: normalize_score_param_bindings(score_param_bindings),
                    global_callback_source: (global_callback_source as string | undefined) ?? '',
                };
            }
        }
    } catch (_e) {
        console.error(_e);
    }
    return {
        orchestra: DEFAULT_ORCHESTRA,
        selected_id: null,
        view: 'instrument',
        score_source: '',
        score_param_bindings: {},
        global_callback_source: '',
    };
}

export function load_initial(): HistoryState {
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
                (c) => !(c.type === 'remove' && c.id === 'out'),
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
                                  e.source === 'out' ||
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
        case 'update_score_source':
            return { ...state, score_source: action.source };
        case 'update_score_param_bindings':
            return { ...state, score_param_bindings: action.bindings };
        case 'update_global_callback_source':
            return { ...state, global_callback_source: action.source };
        case 'set_orchestra_bpm':
            return { ...state, orchestra: { ...state.orchestra, bpm: action.bpm } };
        case 'add_instrument': {
            const n = state.orchestra.instruments.length + 1;
            const id = unique_instrument_id(`instrument${n}`, state.orchestra.instruments);
            const instr: InstrumentState = {
                id: id,
                nodes: default_nodes(),
                edges: [],
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
        case 'rename_instrument': {
            const renamed = unique_instrument_id(
                action.name,
                state.orchestra.instruments,
                action.id,
            );
            const old_in_id = in_node_id(action.id);
            const new_in_id = in_node_id(renamed);

            const instruments = state.orchestra.instruments.map((i) =>
                i.id === action.id ? { ...i, id: renamed } : i,
            );
            const global_nodes = sync_global_in_nodes(
                instruments,
                state.orchestra.global_nodes.map((n) =>
                    n.id === old_in_id ? { ...n, id: new_in_id } : n,
                ),
            );
            const global_edges = state.orchestra.global_edges.map((e) => ({
                ...e,
                source: e.source === old_in_id ? new_in_id : e.source,
                target: e.target === old_in_id ? new_in_id : e.target,
            }));

            return {
                ...state,
                orchestra: {
                    ...state.orchestra,
                    active_id:
                        state.orchestra.active_id === action.id
                            ? renamed
                            : state.orchestra.active_id,
                    instruments,
                    global_nodes,
                    global_edges,
                },
            };
        }
        case 'set_active_instrument':
            return {
                ...state,
                selected_id: null,
                orchestra: { ...state.orchestra, active_id: action.id },
            };
        case 'load':
            return {
                orchestra: normalize_orchestra(action.orchestra),
                selected_id: null,
                view: 'instrument',
                score_source: action.score_source ?? state.score_source,
                score_param_bindings: action.score_param_bindings
                    ? normalize_score_param_bindings(action.score_param_bindings)
                    : state.score_param_bindings,
                global_callback_source:
                    action.global_callback_source ?? state.global_callback_source,
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
        instruments: orchestra.instruments.map((i) => ({
            id: i.id,
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
    };
}

function save(state: PatchState): void {
    try {
        localStorage.setItem(
            STORAGE_KEY,
            JSON.stringify({
                orchestra: serialize_orchestra(state.orchestra),
                score_source: state.score_source,
                score_param_bindings: state.score_param_bindings,
                global_callback_source: state.global_callback_source,
            }),
        );
    } catch (_e) {
        console.error(_e);
    }
}

export function reducer(history: HistoryState, action: PatchAction): HistoryState {
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
