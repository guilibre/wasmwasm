import { useEffect, useRef, useState } from 'react';
import { ReactFlowProvider } from '@xyflow/react';
import WasmWasm from '../audio/compiler';
import WWEditor, { type WWEditorHandle } from './ww_editor';
import { Sidebar } from './sidebar';
import { LeftPane } from './left_pane';
import { PatchEditor } from '../patch/patch_editor';
import { usePatchStore } from '../patch/use_patch_store';
import { useInstrumentCompiler } from './use_instrument_compiler';
import { useAudioEngine } from './use_audio_engine';
import { useBlockModal } from './use_block_modal';
import { useUndoRedoShortcuts } from './use_undo_redo_shortcuts';
import { GLOBAL_CACHE_KEY } from './constants';
import './app.scss';

export default function App() {
    const [error, set_error] = useState<string | null>(null);
    const import_ref = useRef<HTMLInputElement>(null);
    const editor_ref = useRef<WWEditorHandle>(null);

    const store = usePatchStore();
    const {
        orchestra,
        selected_node,
        update_code,
        update_name,
        select,
        export_patch,
        import_patch,
        set_orchestra_bpm,
        set_orchestra_code,
        set_global_patch_code,
        add_instrument,
        remove_instrument,
        set_instrument_code,
        rename_instrument,
        set_active_instrument,
        view,
        set_view,
        undo,
        redo,
    } = store;
    const active_instrument =
        orchestra.instruments.find((i) => i.id === orchestra.active_id) ?? null;
    const selected_block = selected_node?.type === 'block' ? selected_node : null;

    useUndoRedoShortcuts(undo, redo);

    const {
        patch_cache_ref,
        compile_status,
        orchestra_env,
        instrument_envs,
        handle_instrument_code_change,
        handle_global_patch_code_change,
        compile_patch: compile_patch_impl,
        compile_instrument,
        compile_orchestra,
    } = useInstrumentCompiler(orchestra, set_instrument_code, set_global_patch_code, set_error);

    const { analysers, is_playing, is_recording, play, stop, record, get_sample_rate } =
        useAudioEngine(orchestra, patch_cache_ref, set_error);

    const compile_patch = () => compile_patch_impl(get_sample_rate());

    const {
        name_draft,
        set_name_draft,
        modal_pos,
        editing_name,
        start_name_edit,
        commit_name,
        on_modal_header_mouse_down,
        on_name_key_down,
    } = useBlockModal(selected_block, update_name);

    useEffect(() => {
        WasmWasm.ensureReady().then(() => editor_ref.current?.refresh());
    }, []);

    return (
        <div className="app">
            <div className="app__toolbar">
                <span className="app__brand">wasmwasm</span>
                <button onClick={is_playing ? () => stop(0) : play}>
                    {is_playing ? 'Stop' : 'Play'}
                </button>
                <button onClick={record}>{is_recording ? 'Recording…' : 'Record'}</button>
                <button onClick={export_patch}>Export</button>
                <button onClick={() => import_ref.current?.click()}>Import</button>
                <input
                    ref={import_ref}
                    type="file"
                    accept=".json"
                    style={{ display: 'none' }}
                    onChange={(e) => {
                        const file = e.target.files?.[0];
                        if (file) import_patch(file);
                        e.target.value = '';
                    }}
                />
                {error && <span className="app__error">{error}</span>}
            </div>

            <div className="app__workspace">
                <LeftPane
                    orchestra={orchestra}
                    on_bpm_change={set_orchestra_bpm}
                    on_add={add_instrument}
                    on_remove={remove_instrument}
                    on_rename={rename_instrument}
                    on_instrument_code_change={handle_instrument_code_change}
                    on_orchestra_code_change={set_orchestra_code}
                    on_set_active={set_active_instrument}
                    on_compile_patch={compile_patch}
                    on_compile_instrument={compile_instrument}
                    on_compile_orchestra={compile_orchestra}
                    compile_status={compile_status}
                    orchestra_env={orchestra_env}
                    instrument_envs={instrument_envs}
                    on_global_patch_code_change={handle_global_patch_code_change}
                    global_env={instrument_envs.get(GLOBAL_CACHE_KEY) ?? null}
                    view={view}
                    on_view_change={set_view}
                />

                <div className="app__patch-pane">
                    <div className="app__patch-container">
                        <div className="app__patch-tabs">
                            <div
                                className={`app__patch-tab${view === 'global' ? ' app__patch-tab--active' : ''}`}
                                onClick={() => set_view('global')}
                            >
                                global
                            </div>
                            <div
                                className={`app__patch-tab${view === 'instrument' ? ' app__patch-tab--active' : ''}`}
                                onClick={() => set_view('instrument')}
                            >
                                {active_instrument?.name ?? 'instrument'}
                            </div>
                        </div>
                        <ReactFlowProvider>
                            <PatchEditor store={store} />
                        </ReactFlowProvider>
                    </div>
                    <Sidebar analyser_l={analysers?.l ?? null} analyser_r={analysers?.r ?? null} />
                </div>
            </div>

            {selected_block && (
                <div className="app__modal" style={{ left: modal_pos.x, top: modal_pos.y }}>
                    <div className="app__panel-header" onMouseDown={on_modal_header_mouse_down}>
                        {editing_name ? (
                            <input
                                className="app__name-input"
                                autoFocus
                                value={name_draft}
                                onChange={(e) => set_name_draft(e.target.value)}
                                onKeyDown={on_name_key_down}
                                onBlur={commit_name}
                            />
                        ) : (
                            <span
                                className="app__panel-name"
                                onClick={start_name_edit}
                                title="Click to rename"
                            >
                                {(selected_block.data as { name: string }).name}
                            </span>
                        )}
                        <button onClick={() => select(null)}>×</button>
                    </div>
                    <WWEditor
                        ref={editor_ref}
                        key={selected_block.id}
                        initial_value={(selected_block.data as { code: string }).code}
                        on_change={(code) => update_code(selected_block.id, code)}
                    />
                </div>
            )}
        </div>
    );
}
