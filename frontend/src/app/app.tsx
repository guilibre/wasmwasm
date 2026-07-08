import { useEffect, useRef, useState } from 'react';
import { ReactFlowProvider } from '@xyflow/react';
import WasmWasm from '../audio/compiler';
import WWEditor, { type WWEditorHandle } from './ww_editor';
import { Sidebar } from './sidebar';
import { ScorePanel } from './score_panel';
import { InstrumentTabs } from './instrument_tabs';
import { PatchEditor } from '../patch/patch_editor';
import { usePatchStore } from '../patch/store/use_patch_store';
import { useAudioEngine } from './use_audio_engine';
import { StatusBar } from './status_bar';
import { useBlockModal } from './use_block_modal';
import { useUndoRedoShortcuts } from './use_undo_redo_shortcuts';
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
        import_error,
        storage_error,
        load_serial,
        add_instrument,
        remove_instrument,
        rename_instrument,
        set_active_instrument,
        view,
        set_view,
        undo,
        redo,
        score_source,
        update_score_source,
        score_param_bindings,
        update_score_param_bindings,
        global_callback_source,
        update_global_callback_source,
        set_orchestra_bpm,
    } = store;
    const selected_block = selected_node?.type === 'block' ? selected_node : null;

    useUndoRedoShortcuts(undo, redo);

    const { analysers, is_playing, cpu_load, play, stop } = useAudioEngine(
        orchestra,
        score_source,
        score_param_bindings,
        global_callback_source,
        set_error,
    );

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
                {(error || import_error || storage_error) && (
                    <span className="app__error">{error || import_error || storage_error}</span>
                )}
            </div>

            <div className="app__workspace">
                <ScorePanel
                    source={score_source}
                    on_change={update_score_source}
                    orchestra={orchestra}
                    score_param_bindings={score_param_bindings}
                    on_score_param_bindings_change={update_score_param_bindings}
                    global_callback_source={global_callback_source}
                    on_global_callback_source_change={update_global_callback_source}
                    on_bpm_change={set_orchestra_bpm}
                    load_serial={load_serial}
                />
                <div className="app__patch-pane">
                    <div className="app__patch-container">
                        <InstrumentTabs
                            instruments={orchestra.instruments}
                            active_instrument_id={orchestra.active_id}
                            view={view}
                            on_add={add_instrument}
                            on_remove={remove_instrument}
                            on_rename={rename_instrument}
                            on_set_active={set_active_instrument}
                            on_view_change={set_view}
                        />
                        <ReactFlowProvider>
                            <PatchEditor store={store} />
                        </ReactFlowProvider>
                        <StatusBar is_playing={is_playing} cpu_load={cpu_load} />
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
                        key={`${selected_block.id}-${load_serial}`}
                        initial_value={(selected_block.data as { code: string }).code}
                        on_change={(code) => update_code(selected_block.id, code)}
                        get_module={() => WasmWasm.getModule()}
                    />
                </div>
            )}
        </div>
    );
}
