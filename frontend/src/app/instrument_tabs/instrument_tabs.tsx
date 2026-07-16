import { useState } from 'react';
import type { OrchestraState, PatchView } from '../../patch/store/patch_types';
import { GLOBAL_CACHE_KEY } from '../constants';
import './instrument.scss';

interface Props {
    instruments: OrchestraState['instruments'];
    active_instrument_id: string | null;
    view: PatchView;
    on_add: () => void;
    on_remove: (id: string) => void;
    on_rename: (id: string, name: string) => void;
    on_set_active: (id: string) => void;
    on_view_change: (view: PatchView) => void;
}

export function InstrumentTabs({
    instruments,
    active_instrument_id,
    view,
    on_add,
    on_remove,
    on_rename,
    on_set_active,
    on_view_change,
}: Props) {
    const [editing_id, set_editing_id] = useState<string | null>(null);
    const [name_draft, set_name_draft] = useState('');

    const resolved_id =
        view === 'global'
            ? GLOBAL_CACHE_KEY
            : (active_instrument_id ?? instruments[0]?.id ?? GLOBAL_CACHE_KEY);
    const is_global_tab = resolved_id === GLOBAL_CACHE_KEY;

    const handle_tab_click = (id: string) => {
        if (id === GLOBAL_CACHE_KEY) {
            on_view_change('global');
        } else {
            on_set_active(id);
            on_view_change('instrument');
        }
    };

    const start_rename = (id: string, name: string) => {
        set_editing_id(id);
        set_name_draft(name);
    };

    const commit_rename = () => {
        const trimmed = name_draft.trim();
        if (trimmed && editing_id) on_rename(editing_id, trimmed);
        set_editing_id(null);
    };

    const on_key_down = (e: React.KeyboardEvent<HTMLInputElement>) => {
        if (e.key === 'Enter') commit_rename();
        else if (e.key === 'Escape') set_editing_id(null);
    };

    const handle_remove = (e: React.MouseEvent, id: string) => {
        e.stopPropagation();
        on_remove(id);
    };

    return (
        <div className="instrument__tabs">
            <div
                className={`instrument__tab${is_global_tab ? ' instrument__tab--active' : ''}`}
                onClick={() => handle_tab_click(GLOBAL_CACHE_KEY)}
            >
                <span className="instrument__tab-name">global</span>
            </div>
            {instruments.map((instr) => (
                <div
                    key={instr.id}
                    className={`instrument__tab${resolved_id === instr.id ? ' instrument__tab--active' : ''}`}
                    onClick={() => handle_tab_click(instr.id)}
                >
                    {editing_id === instr.id ? (
                        <input
                            className="instrument__tab-input"
                            autoFocus
                            value={name_draft}
                            onChange={(e) => set_name_draft(e.target.value)}
                            onKeyDown={on_key_down}
                            onBlur={commit_rename}
                            onClick={(e) => e.stopPropagation()}
                        />
                    ) : (
                        <span
                            className="instrument__tab-name"
                            onDoubleClick={(e) => {
                                e.stopPropagation();
                                start_rename(instr.id, instr.id);
                            }}
                        >
                            {instr.id}
                        </span>
                    )}
                    <button
                        className="instrument__tab-remove"
                        onClick={(e) => handle_remove(e, instr.id)}
                    >
                        ×
                    </button>
                </div>
            ))}
            <button className="instrument__add" onClick={on_add}>
                +
            </button>
        </div>
    );
}
