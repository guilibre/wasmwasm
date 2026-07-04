import { useCallback, useEffect, useRef, useState } from 'react';
import './patch_editor.scss';
import { ReactFlow, Background, Controls, useReactFlow } from '@xyflow/react';
import type {
    NodeMouseHandler,
    EdgeMouseHandler,
    IsValidConnection,
    Node,
    Edge,
    DefaultEdgeOptions,
} from '@xyflow/react';
import '@xyflow/react/dist/style.css';
import { BlockNode } from './block_node';
import { CaptureNode } from './capture_node';
import { DacNode } from './dac_node';
import { OutNode } from './out_node';
import { InNode } from './in_node';
import { SelfLoopEdge } from './self_loop_edge';
import type { usePatchStore } from './use_patch_store';

const EDGE_TYPES = {
    self_loop: SelfLoopEdge,
};

const NODE_TYPES = {
    block: BlockNode,
    capture: CaptureNode,
    dac: DacNode,
    out: OutNode,
    in: InNode,
};

interface ContextMenu {
    x: number;
    y: number;
    node_id?: string;
    edge_id?: string;
}

interface Props {
    store: ReturnType<typeof usePatchStore>;
}

export function PatchEditor({ store }: Props) {
    const {
        nodes,
        edges,
        on_nodes_change,
        on_edges_change,
        on_connect,
        select,
        add_block,
        update_name,
    } = store;

    const rf = useReactFlow();

    useEffect(() => {
        const id = setTimeout(() => rf.fitView({ duration: 200 }), 50);
        return () => clearTimeout(id);
    }, [store.layout_serial, rf]);

    const [name_input, set_name_input] = useState<{
        x: number;
        y: number;
        screen_x: number;
        screen_y: number;
    } | null>(null);
    const [pending_name, set_pending_name] = useState('');
    const name_input_ref = useRef<HTMLInputElement>(null);
    const [ctx_menu, set_ctx_menu] = useState<ContextMenu | null>(null);

    const [rename_input, set_rename_input] = useState<{
        node_id: string;
        screen_x: number;
        screen_y: number;
    } | null>(null);
    const [pending_rename, set_pending_rename] = useState('');
    const rename_input_ref = useRef<HTMLInputElement>(null);

    const on_node_double_click: NodeMouseHandler = useCallback(
        (e, node) => {
            if (node.type === 'block') {
                select(node.id);
                return;
            }
            if (node.type === 'out') {
                const bounds = (e.currentTarget as HTMLElement)
                    .closest('.ww-canvas')
                    ?.getBoundingClientRect();
                set_rename_input({
                    node_id: node.id,
                    screen_x: e.clientX - (bounds?.left ?? 0),
                    screen_y: e.clientY - (bounds?.top ?? 0),
                });
                set_pending_rename((node.data as { name: string }).name);
                setTimeout(() => rename_input_ref.current?.focus(), 0);
            }
        },
        [select],
    );

    const commit_rename = useCallback(() => {
        if (rename_input && pending_rename.trim())
            update_name(rename_input.node_id, pending_rename.trim());
        set_rename_input(null);
        set_pending_rename('');
    }, [rename_input, pending_rename, update_name]);

    const on_key_down_rename = useCallback(
        (e: React.KeyboardEvent) => {
            if (e.key === 'Enter') commit_rename();
            else if (e.key === 'Escape') {
                set_rename_input(null);
                set_pending_rename('');
            }
        },
        [commit_rename],
    );

    const on_canvas_double_click = useCallback(
        (e: React.MouseEvent) => {
            const target = e.target as HTMLElement;
            if (!target.classList.contains('react-flow__pane')) return;
            const bounds = (e.currentTarget as HTMLElement).getBoundingClientRect();
            const pos = rf.screenToFlowPosition({ x: e.clientX, y: e.clientY });
            set_name_input({
                x: pos.x,
                y: pos.y,
                screen_x: e.clientX - bounds.left,
                screen_y: e.clientY - bounds.top,
            });
            set_pending_name('');
            setTimeout(() => name_input_ref.current?.focus(), 0);
        },
        [rf],
    );

    const commit_name = useCallback(() => {
        if (name_input && pending_name.trim())
            add_block(pending_name.trim(), { x: name_input.x, y: name_input.y });
        set_name_input(null);
        set_pending_name('');
    }, [name_input, pending_name, add_block]);

    const on_key_down_name = useCallback(
        (e: React.KeyboardEvent) => {
            if (e.key === 'Enter') commit_name();
            else if (e.key === 'Escape') {
                set_name_input(null);
                set_pending_name('');
            }
        },
        [commit_name],
    );

    const on_nodes_delete = useCallback(
        (deleted: Node[]) => {
            const ids = new Set(deleted.map((n) => n.id));
            if (ids.has(store.selected_id ?? '')) select(null);
        },
        [store.selected_id, select],
    );

    const on_node_context_menu: NodeMouseHandler = useCallback((e, node) => {
        e.preventDefault();
        set_ctx_menu({ x: e.clientX, y: e.clientY, node_id: node.id });
    }, []);

    const on_edge_context_menu: EdgeMouseHandler = useCallback((e, edge) => {
        e.preventDefault();
        set_ctx_menu({ x: e.clientX, y: e.clientY, edge_id: edge.id });
    }, []);

    const close_ctx = useCallback(() => set_ctx_menu(null), []);

    const is_valid_connection = useCallback<IsValidConnection>(
        (conn) =>
            !edges.some(
                (e: Edge) =>
                    e.target === conn.target && e.targetHandle === (conn.targetHandle ?? null),
            ),
        [edges],
    );

    const ctx_remove = useCallback(() => {
        if (!ctx_menu) return;
        if (ctx_menu.node_id) {
            on_nodes_change([{ type: 'remove', id: ctx_menu.node_id }]);
            if (ctx_menu.node_id === store.selected_id) select(null);
        }
        if (ctx_menu.edge_id) {
            on_edges_change([{ type: 'remove', id: ctx_menu.edge_id }]);
        }
        set_ctx_menu(null);
    }, [ctx_menu, on_nodes_change, on_edges_change, store.selected_id, select]);

    return (
        <div className="ww-canvas" onClick={close_ctx} onDoubleClick={on_canvas_double_click}>
            <ReactFlow
                nodes={nodes}
                edges={edges}
                nodeTypes={NODE_TYPES}
                edgeTypes={EDGE_TYPES}
                defaultEdgeOptions={{ pathOptions: { offset: 0 } } as DefaultEdgeOptions}

                onNodesChange={on_nodes_change}
                onEdgesChange={on_edges_change}
                onConnect={on_connect}
                isValidConnection={is_valid_connection}
                onNodeDoubleClick={on_node_double_click}
                onPaneClick={() => {
                    select(null);
                    close_ctx();
                }}
                onNodesDelete={on_nodes_delete}
                onNodeContextMenu={on_node_context_menu}
                onEdgeContextMenu={on_edge_context_menu}
                deleteKeyCode={['Delete', 'Backspace']}
                selectionOnDrag
                panOnDrag={[1, 2]}
                zoomOnDoubleClick={false}
                fitView
                proOptions={{ hideAttribution: true }}
            >
                <Background />
                <Controls showZoom={false} showInteractive={false} />
            </ReactFlow>

            {name_input && (
                <input
                    ref={name_input_ref}
                    className="ww-name-input"
                    style={{ left: name_input.screen_x, top: name_input.screen_y }}
                    value={pending_name}
                    onChange={(e) => set_pending_name(e.target.value)}
                    onKeyDown={on_key_down_name}
                    onBlur={commit_name}
                    placeholder="block name"
                />
            )}

            {rename_input && (
                <input
                    ref={rename_input_ref}
                    className="ww-name-input"
                    style={{ left: rename_input.screen_x, top: rename_input.screen_y }}
                    value={pending_rename}
                    onChange={(e) => set_pending_rename(e.target.value)}
                    onKeyDown={on_key_down_rename}
                    onBlur={commit_rename}
                    placeholder="OUT N"
                />
            )}

            {ctx_menu && (
                <div
                    className="ww-ctx-menu"
                    style={{ left: ctx_menu.x, top: ctx_menu.y }}
                    onClick={(e) => e.stopPropagation()}
                >
                    <button onClick={ctx_remove}>Remove</button>
                </div>
            )}
        </div>
    );
}
