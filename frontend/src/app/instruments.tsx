import { useMemo, useState } from 'react';
import type { VirtualTypeScriptEnvironment } from '@typescript/vfs';
import type { OrchestraState, PatchView } from '../patch/use_patch_store';
import { TsEditor } from './ts_editor';
import { make_instrument_env_with_params } from './ts_env';
import { GLOBAL_CACHE_KEY } from './constants';
import './instrument.scss';

interface Props {
    instruments: OrchestraState['instruments'];
    active_instrument_id: string | null;
    view: PatchView;
    on_add: () => void;
    on_remove: (id: string) => void;
    on_rename: (id: string, name: string) => void;
    on_code_change: (id: string, code: string) => void;
    on_set_active: (id: string) => void;
    on_compile_patch: () => void;
    on_compile_instrument: () => void;
    compile_status: Map<string, 'idle' | 'compiling' | 'ok' | 'error'>;
    instrument_envs: Map<string, VirtualTypeScriptEnvironment>;
    global_code: string;
    on_global_code_change: (code: string) => void;
    global_env: VirtualTypeScriptEnvironment | null;
    on_view_change: (view: PatchView) => void;
}

export function InstrumentPanel({
    instruments,
    active_instrument_id,
    view,
    on_add,
    on_remove,
    on_rename,
    on_code_change,
    on_set_active,
    on_compile_patch,
    on_compile_instrument,
    compile_status,
    instrument_envs,
    global_code,
    on_global_code_change,
    global_env,
    on_view_change,
}: Props) {
    const [editing_id, set_editing_id] = useState<string | null>(null);
    const [name_draft, set_name_draft] = useState('');

    const resolved_id =
        view === 'global' ? GLOBAL_CACHE_KEY : (active_instrument_id ?? instruments[0]?.id ?? null);
    const active_instr = instruments.find((i) => i.id === resolved_id) ?? null;
    const is_global_tab = resolved_id === GLOBAL_CACHE_KEY;

    const default_env = useMemo(() => make_instrument_env_with_params([]), []);
    const env = (active_instr ? instrument_envs.get(active_instr.id) : undefined) ?? default_env;

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

    const any_compiling_patch = instruments.some(
        (i) => compile_status.get(`patch:${i.id}`) === 'compiling',
    );
    const any_compiling_instr = instruments.some(
        (i) => compile_status.get(`instr:${i.id}`) === 'compiling',
    );
    const all_patches_ok =
        instruments.length > 0 &&
        instruments.every((i) => compile_status.get(`patch:${i.id}`) === 'ok') &&
        compile_status.get(`patch:${GLOBAL_CACHE_KEY}`) === 'ok';
    const all_instrs_ok =
        instruments.length > 0 &&
        instruments.every((i) => compile_status.get(`instr:${i.id}`) === 'ok') &&
        compile_status.get(`instr:${GLOBAL_CACHE_KEY}`) === 'ok';

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
                {all_patches_ok && <span className="instrument__compile-ok">✓</span>}
                <button
                    className="instrument__compile-btn"
                    disabled={any_compiling_instr || instruments.length === 0}
                    onClick={on_compile_instrument}
                >
                    {any_compiling_instr ? '...' : 'Compile Instruments'}
                </button>
                {all_instrs_ok && <span className="instrument__compile-ok">✓</span>}
            </div>
            <div className="instrument__header">
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

            {is_global_tab ? (
                <div className="instrument__editor">
                    <TsEditor
                        key={GLOBAL_CACHE_KEY}
                        initial_value={global_code}
                        on_change={on_global_code_change}
                        env={global_env ?? default_env}
                    />
                </div>
            ) : active_instr ? (
                <div className="instrument__editor">
                    <TsEditor
                        key={active_instr.id}
                        initial_value={active_instr.code}
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
