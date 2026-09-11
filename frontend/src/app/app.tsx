import { useEffect, useRef, useState } from 'react';
import { ReactFlowProvider } from '@xyflow/react';
import WasmWasm from '../audio/compiler';
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
import { useT } from '../i18n/lang_context';
import { LanguageToggle } from '../i18n/language_toggle';
import { ErrorMessage } from './error_message';
import type { AppError } from '../scorewasm/score_compile_error';
import './app.scss';

export default function App() {
    const t = useT();
    const [error, set_error] = useState<AppError | null>(null);
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
        load_score_example,
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
                <LanguageToggle />
                <button onClick={is_playing ? () => stop(0) : play}>
                    {is_playing ? t('stop') : t('play')}
                </button>
                <button onClick={export_patch}>{t('export_patch')}</button>
                <button onClick={() => import_ref.current?.click()}>{t('import_patch')}</button>
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
                    <span className="app__error">
                        {error ? (
                            <ErrorMessage
                                message={error.message}
                                line={error.line}
                                col={error.col}
                            />
                        ) : (
                            import_error || (storage_error && t(storage_error))
                        )}
                    </span>
                )}
            </div>

            <div className="app__workspace">
                <ScorePanel
                    source={score_source}
                    on_change={update_score_source}
                    on_load_score_example={load_score_example}
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
                                title={t('click_to_rename')}
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
