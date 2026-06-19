import { useCallback, useReducer } from 'react';
import { applyNodeChanges, applyEdgeChanges } from '@xyflow/react';
import type { Node, Edge, NodeChange, EdgeChange, Connection } from '@xyflow/react';

export interface BlockData {
    name: string;
    code: string;
    num_inputs: number;
    num_outputs: number;
}

export function scan_arity(code: string): { num_inputs: number; num_outputs: number } {
    const ins = [...code.matchAll(/IN\[(\d+)\]/g)].map((m) => parseInt(m[1]));
    const outs = [...code.matchAll(/OUT\[(\d+)\]/g)].map((m) => parseInt(m[1]));
    return {
        num_inputs: ins.length > 0 ? Math.max(...ins) + 1 : 0,
        num_outputs: outs.length > 0 ? Math.max(...outs) + 1 : 0,
    };
}

interface PatchState {
    nodes: Node[];
    edges: Edge[];
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
    | { type: 'load'; nodes: Node[]; edges: Edge[] };

const STORAGE_KEY = 'wasmwasm_patch';

function load_initial(): PatchState {
    try {
        const saved = localStorage.getItem(STORAGE_KEY);
        if (saved) {
            const { nodes, edges } = JSON.parse(saved);
            return { nodes, edges, selected_id: null };
        }
    } catch (_e) {
        void _e;
    }
    return {
        nodes: [
            { id: 'capture', type: 'capture', position: { x: 100, y: 50 }, data: {} },
            { id: 'dac', type: 'dac', position: { x: 100, y: 400 }, data: {} },
        ],
        edges: [],
        selected_id: null,
    };
}

function reducer(state: PatchState, action: PatchAction): PatchState {
    let next: PatchState;
    switch (action.type) {
        case 'nodes_change': {
            const removed_ids = new Set(
                action.changes
                    .filter((c): c is NodeChange & { type: 'remove' } => c.type === 'remove')
                    .map((c) => c.id),
            );
            const edges =
                removed_ids.size > 0
                    ? state.edges.filter(
                          (e) => !removed_ids.has(e.source) && !removed_ids.has(e.target),
                      )
                    : state.edges;
            next = { ...state, nodes: applyNodeChanges(action.changes, state.nodes), edges };
            break;
        }
        case 'edges_change':
            next = { ...state, edges: applyEdgeChanges(action.changes, state.edges) };
            break;
        case 'connect': {
            const { source, sourceHandle, target, targetHandle } = action.connection;
            const occupied = state.edges.some(
                (e) => e.target === target && e.targetHandle === (targetHandle ?? null),
            );
            if (occupied) return state;
            const new_edge: Edge = {
                id: `e_${source}_${sourceHandle}_${target}_${targetHandle}`,
                source,
                sourceHandle: sourceHandle ?? null,
                target,
                targetHandle: targetHandle ?? null,
                markerEnd: undefined,
                markerStart: undefined,
            };
            next = { ...state, edges: [...state.edges, new_edge] };
            break;
        }
        case 'select':
            return { ...state, selected_id: action.id };
        case 'add_node':
            next = { ...state, nodes: [...state.nodes, action.node] };
            break;
        case 'update_code': {
            const arity = scan_arity(action.code);
            const edges = state.edges.filter((e) => {
                if (e.source === action.id) {
                    const idx = parseInt((e.sourceHandle ?? '').replace('out_', ''));
                    if (!isNaN(idx) && idx >= arity.num_outputs) return false;
                }
                if (e.target === action.id) {
                    const idx = parseInt((e.targetHandle ?? '').replace('in_', ''));
                    if (!isNaN(idx) && idx >= arity.num_inputs) return false;
                }
                return true;
            });
            next = {
                ...state,
                edges,
                nodes: state.nodes.map((n) =>
                    n.id === action.id
                        ? { ...n, data: { ...n.data, code: action.code, ...arity } }
                        : n,
                ),
            };
            break;
        }
        case 'update_name':
            next = {
                ...state,
                nodes: state.nodes.map((n) =>
                    n.id === action.id ? { ...n, data: { ...n.data, name: action.name } } : n,
                ),
            };
            break;
        case 'load':
            next = { nodes: action.nodes, edges: action.edges, selected_id: null };
            break;
        default:
            return state;
    }
    try {
        localStorage.setItem(STORAGE_KEY, JSON.stringify({ nodes: next.nodes, edges: next.edges }));
    } catch (_e) {
        void _e;
    }
    return next;
}

export function usePatchStore() {
    const [state, dispatch] = useReducer(reducer, undefined, load_initial);

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
        dispatch({
            type: 'add_node',
            node: {
                id,
                type: 'block',
                position,
                data: { name, code: '', num_inputs: 0, num_outputs: 0 } satisfies BlockData,
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

    const export_patch = useCallback(() => {
        const blob = new Blob(
            [JSON.stringify({ nodes: state.nodes, edges: state.edges }, null, 2)],
            {
                type: 'application/json',
            },
        );
        const url = URL.createObjectURL(blob);
        const a = document.createElement('a');
        a.href = url;
        a.download = 'patch.json';
        a.click();
        URL.revokeObjectURL(url);
    }, [state.nodes, state.edges]);

    const import_patch = useCallback((file: File) => {
        const reader = new FileReader();
        reader.onload = (e) => {
            try {
                const { nodes, edges } = JSON.parse(e.target!.result as string);
                dispatch({ type: 'load', nodes, edges });
            } catch (_e) {
                void _e;
            }
        };
        reader.readAsText(file);
    }, []);

    const load_patch = useCallback(
        (nodes: Node[], edges: Edge[]) => dispatch({ type: 'load', nodes, edges }),
        [],
    );

    const selected_node = state.nodes.find((n) => n.id === state.selected_id) ?? null;

    return {
        nodes: state.nodes,
        edges: state.edges,
        selected_id: state.selected_id,
        selected_node,
        on_nodes_change,
        on_edges_change,
        on_connect,
        select,
        add_block,
        update_code,
        update_name,
        export_patch,
        import_patch,
        load_patch,
    };
}
