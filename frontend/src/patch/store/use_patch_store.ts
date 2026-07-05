import { useCallback, useEffect, useReducer, useRef, useState } from 'react';
import type { Node, NodeChange, EdgeChange, Connection } from '@xyflow/react';
import { scan_arity, scan_params } from './code_scanning';
import { elk_layout } from './elk_auto_layout';
import { reducer, load_initial } from './patch_reducer';
import type { BlockData, PatchView, OrchestraState } from './patch_types';

const raw_templates = import.meta.glob('../../templates/*.ww', {
    query: '?raw',
    import: 'default',
    eager: true,
}) as Record<string, string>;

const TEMPLATES: Record<string, string> = Object.fromEntries(
    Object.entries(raw_templates).map(([path, code]) => [
        path.replace('../../templates/', '').replace('.ww', ''),
        code,
    ]),
);

function get_default_code(name: string): string {
    return TEMPLATES[name] ?? '';
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
    const rename_instrument = useCallback(
        (id: string, name: string) => dispatch({ type: 'rename_instrument', id, name }),
        [],
    );
    const set_active_instrument = useCallback(
        (id: string) => dispatch({ type: 'set_active_instrument', id }),
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
        rename_instrument,
        set_active_instrument,
        export_patch,
        import_patch,
        load_patch,
        layout_serial,
    };
}
