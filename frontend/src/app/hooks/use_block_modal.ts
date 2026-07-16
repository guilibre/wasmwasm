import { useCallback, useMemo, useRef, useState } from 'react';
import type { Node } from '@xyflow/react';

export function useBlockModal(
    selected_block: Node | null,
    update_name: (id: string, name: string) => void,
) {
    const [editing_block_id, set_editing_block_id] = useState<string | null>(null);
    const [name_draft, set_name_draft] = useState('');
    const [modal_drag_pos, set_modal_drag_pos] = useState<{
        block_id: string;
        x: number;
        y: number;
    } | null>(null);
    const modal_drag_offset = useRef<{ dx: number; dy: number } | null>(null);
    const modal_pos = useMemo(
        () =>
            modal_drag_pos?.block_id === selected_block?.id && modal_drag_pos != null
                ? { x: modal_drag_pos.x, y: modal_drag_pos.y }
                : { x: window.innerWidth / 2 - 300, y: window.innerHeight / 2 - 210 },
        [modal_drag_pos, selected_block?.id],
    );

    const editing_name = editing_block_id === selected_block?.id;

    const start_name_edit = useCallback(() => {
        set_name_draft((selected_block?.data as { name: string })?.name ?? '');
        set_editing_block_id(selected_block?.id ?? null);
    }, [selected_block]);

    const commit_name = useCallback(() => {
        const trimmed = name_draft.trim();
        if (trimmed && selected_block) update_name(selected_block.id, trimmed);
        set_editing_block_id(null);
    }, [name_draft, selected_block, update_name]);

    const on_modal_header_mouse_down = useCallback(
        (e: React.MouseEvent) => {
            if ((e.target as HTMLElement).closest('input, button')) return;
            const block_id = selected_block?.id;
            if (!block_id) return;
            modal_drag_offset.current = {
                dx: e.clientX - modal_pos.x,
                dy: e.clientY - modal_pos.y,
            };
            const on_move = (ev: MouseEvent) => {
                if (!modal_drag_offset.current) return;
                set_modal_drag_pos({
                    block_id,
                    x: ev.clientX - modal_drag_offset.current.dx,
                    y: ev.clientY - modal_drag_offset.current.dy,
                });
            };
            const on_up = () => {
                modal_drag_offset.current = null;
                document.removeEventListener('mousemove', on_move);
                document.removeEventListener('mouseup', on_up);
            };
            document.addEventListener('mousemove', on_move);
            document.addEventListener('mouseup', on_up);
        },
        [modal_pos, selected_block?.id],
    );

    const on_name_key_down = useCallback(
        (e: React.KeyboardEvent<HTMLInputElement>) => {
            if (e.key === 'Enter') commit_name();
            else if (e.key === 'Escape') set_editing_block_id(null);
        },
        [commit_name],
    );

    return {
        name_draft,
        set_name_draft,
        modal_pos,
        editing_name,
        start_name_edit,
        commit_name,
        on_modal_header_mouse_down,
        on_name_key_down,
        set_editing_block_id,
    };
}
