import { useEffect, useRef, useState } from 'react';
import { ReactFlowProvider } from '@xyflow/react';
import WasmWasm from '../wasmwasm/compiler';
import WWEditor, { type WWEditorHandle } from './editors/ww_editor';
import { Sidebar } from './sidebar/sidebar';
import { ScorePanel } from './score/score_panel';
import { InstrumentTabs } from './instrument_tabs/instrument_tabs';
import { PatchEditor } from '../patch/patch_editor';
import { usePatchStore } from '../patch/store/use_patch_store';
import { useAudioEngine } from './hooks/use_audio_engine';
import { StatusBar } from './status_bar';
import { useBlockModal } from './hooks/use_block_modal';
import { useUndoRedoShortcuts } from './hooks/use_undo_redo_shortcuts';
import { useAuth } from '../auth/use_auth';
import { useRemotePatches } from '../patch/remote/use_remote_patches';
import { PatchesPanel } from '../patch/remote/patches_panel';
import './app.scss';

export default function App() {
    const [error, set_error] = useState<string | null>(null);
    const [show_patches_panel, set_show_patches_panel] = useState(false);
    const editor_ref = useRef<WWEditorHandle>(null);
    const auth = useAuth();
    const remote = useRemotePatches();

    const store = usePatchStore();
    const {
        orchestra,
        selected_node,
        update_code,
        update_name,
        select,
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
        load_patch,
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
                <button onClick={() => set_show_patches_panel(true)}>Meus Patches</button>
                {(error || storage_error) && (
                    <span className="app__error">{error || storage_error}</span>
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

            {show_patches_panel && (
                <PatchesPanel
                    auth={auth}
                    remote={remote}
                    current_data={{
                        orchestra,
                        score_source,
                        score_param_bindings,
                        global_callback_source,
                    }}
                    on_close={() => set_show_patches_panel(false)}
                    on_loaded={(data) => {
                        load_patch(data);
                        set_show_patches_panel(false);
                    }}
                />
            )}
        </div>
    );
}
