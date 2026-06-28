import { useMemo, useState } from 'react';
import type { VirtualTypeScriptEnvironment } from '@typescript/vfs';
import type { OrchestraState } from '../patch/use_patch_store';
import { TsEditor } from './ts_editor';
import { make_instrument_env_with_params, INSTRUMENT_FALLBACK } from './ts_env';
import './instrument.scss';

interface Props {
    instruments: OrchestraState['instruments'];
    on_add: () => void;
    on_remove: (id: string) => void;
    on_rename: (id: string, name: string) => void;
    on_code_change: (id: string, code: string) => void;
    on_set_active: (id: string) => void;
    on_compile_patch: () => void;
    on_compile_instrument: () => void;
    compile_status: Map<string, 'idle' | 'compiling' | 'ok' | 'error'>;
    instrument_envs: Map<string, VirtualTypeScriptEnvironment>;
}

export function InstrumentPanel({
    instruments,
    on_add,
    on_remove,
    on_rename,
    on_code_change,
    on_set_active,
    on_compile_patch,
    on_compile_instrument,
    compile_status,
    instrument_envs,
}: Props) {
    const [editing_id, set_editing_id] = useState<string | null>(null);
    const [name_draft, set_name_draft] = useState('');
    const [active_id, set_active_id] = useState<string | null>(instruments[0]?.id ?? null);

    const resolved_id = active_id ?? instruments[0]?.id ?? null;
    const active_instr = instruments.find((i) => i.id === resolved_id) ?? null;

    const default_env = useMemo(() => make_instrument_env_with_params([]), []);
    const env = (active_instr ? instrument_envs.get(active_instr.id) : undefined) ?? default_env;

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

    const any_compiling_patch = instruments.some(
        (i) => compile_status.get(`patch:${i.id}`) === 'compiling',
    );
    const any_compiling_instr = instruments.some(
        (i) => compile_status.get(`instr:${i.id}`) === 'compiling',
    );
    const all_patches_ok =
        instruments.length > 0 &&
        instruments.every((i) => compile_status.get(`patch:${i.id}`) === 'ok');
    const all_instrs_ok =
        instruments.length > 0 &&
        instruments.every((i) => compile_status.get(`instr:${i.id}`) === 'ok');

    return (
        <div className="instrument">
            <div className="instrument__compile-actions">
                <button
                    className="instrument__compile-btn"
                    disabled={any_compiling_patch || instruments.length === 0}
                    onClick={on_compile_patch}
                >
                    {any_compiling_patch ? '...' : 'Compile Patches'}
                </button>
                {all_patches_ok && <span className="instrument__compile-ok">patches ✓</span>}
                <button
                    className="instrument__compile-btn"
                    disabled={any_compiling_instr || instruments.length === 0}
                    onClick={on_compile_instrument}
                >
                    {any_compiling_instr ? '...' : 'Compile Instruments'}
                </button>
                {all_instrs_ok && <span className="instrument__compile-ok">instruments ✓</span>}
            </div>
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
