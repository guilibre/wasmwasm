import { useEffect, useState } from 'react';
import type { VirtualTypeScriptEnvironment } from '@typescript/vfs';
import type { OrchestraState } from '../patch/use_patch_store';
import { TsEditor } from './ts_editor';
import { get_instrument_env, INSTRUMENT_FALLBACK } from './ts_env';
import './instrument.scss';

interface Props {
    instruments: OrchestraState['instruments'];
    on_add: () => void;
    on_remove: (id: string) => void;
    on_rename: (id: string, name: string) => void;
    on_code_change: (id: string, code: string) => void;
    on_set_active: (id: string) => void;
}

export function InstrumentPanel({
    instruments,
    on_add,
    on_remove,
    on_rename,
    on_code_change,
    on_set_active,
}: Props) {
    const [env, set_env] = useState<VirtualTypeScriptEnvironment | null>(null);
    const [editing_id, set_editing_id] = useState<string | null>(null);
    const [name_draft, set_name_draft] = useState('');
    const [active_id, set_active_id] = useState<string | null>(instruments[0]?.id ?? null);

    useEffect(() => {
        get_instrument_env().then(set_env);
    }, []);

    const resolved_id = active_id ?? instruments[0]?.id ?? null;
    const active_instr = instruments.find((i) => i.id === resolved_id) ?? null;

    const handle_tab_click = (id: string) => {
        set_active_id(id);
        on_set_active(id);
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
        if (resolved_id === id) {
            const remaining = instruments.filter((i) => i.id !== id);
            set_active_id(remaining[0]?.id ?? null);
        }
        on_remove(id);
    };

    return (
        <div className="instrument">
            <div className="instrument__header">
                <div className="instrument__tabs">
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
                                        start_rename(instr.id, instr.name);
                                    }}
                                >
                                    {instr.name}
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
            </div>

            {active_instr ? (
                <div className="instrument__editor">
                    <TsEditor
                        key={active_instr.id}
                        initial_value={active_instr.code || INSTRUMENT_FALLBACK}
                        on_change={(code) => on_code_change(active_instr.id, code)}
                        env={env}
                    />
                </div>
            ) : (
                <div className="instrument__empty">
                    Adicione um instrument com <strong>+</strong>
                </div>
            )}
        </div>
    );
}
