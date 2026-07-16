import { useEffect, useState } from 'react';
import WasmWasm from '../../audio/compiler';
import ScoreWasm from '../../scorewasm/compiler';
import { orchestra_to_json } from '../../patch/orchestra_to_json';
import type { OrchestraState, ScoreParamBindings } from '../../patch/store/patch_types';
import type { ParamIndex } from '../../audio/conductor';
import { score_scale_ambient_source, update_score_ambient } from '../editors/ts_env';
import ConductorCallbackEditor, {
    instrument_callback_example,
    global_callback_example,
} from '../editors/conductor_callback_editor';
import './conductor_panel.scss';

interface Props {
    orchestra: OrchestraState;
    score_source: string;
    score_param_bindings: ScoreParamBindings;
    on_change: (bindings: ScoreParamBindings) => void;
    global_callback_source: string;
    on_global_callback_source_change: (source: string) => void;
    on_bpm_change: (bpm: number) => void;
    load_serial: number;
}

const global_id = 'global';

export function ConductorPanel({
    orchestra,
    score_source,
    score_param_bindings,
    on_change,
    global_callback_source,
    on_global_callback_source_change,
    on_bpm_change,
    load_serial,
}: Props) {
    const [patch_param_index, set_patch_param_index] = useState<ParamIndex>({});
    const [selected, set_selected] = useState<string | null>(null);

    useEffect(() => {
        let cancelled = false;
        const patch_json = orchestra_to_json(orchestra);
        WasmWasm.ensureReady()
            .then(() => WasmWasm.get_param_index(patch_json))
            .then((index) => {
                if (!cancelled) set_patch_param_index(index);
            })
            .catch(() => {
                if (!cancelled) set_patch_param_index({});
            });
        return () => {
            cancelled = true;
        };
    }, [orchestra]);

    useEffect(() => {
        let cancelled = false;
        ScoreWasm.compile_score(score_source)
            .then((graph) => {
                if (cancelled) return;
                return update_score_ambient(score_scale_ambient_source(graph.scales));
            })
            .catch(() => {
                // invalid score source - leave the previous ambient declarations in place
            });
        return () => {
            cancelled = true;
        };
    }, [score_source, load_serial]);

    const instrument_ids = Object.keys(patch_param_index).filter((id) => id !== global_id);

    const update_source = (instrument_id: string, source: string) => {
        on_change({ ...score_param_bindings, [instrument_id]: source });
    };

    return (
        <div className="app__conductor">
            <div className="app__conductor-bpm">
                <span className="app__conductor-bpm-label">bpm</span>
                <input
                    type="number"
                    min={1}
                    className="app__conductor-bpm-input"
                    value={orchestra.bpm}
                    onChange={(e) => on_bpm_change(Number(e.target.value))}
                />
            </div>
            <div className="app__conductor-list">
                <button
                    className={'app__conductor-item' + (selected === global_id ? ' selected' : '')}
                    onClick={() => set_selected(global_id)}
                >
                    global
                </button>
                {instrument_ids.length === 0 && (
                    <span className="app__conductor-empty">nenhum instrumento encontrado</span>
                )}
                {instrument_ids.map((instrument_id) => (
                    <button
                        key={instrument_id}
                        className={
                            'app__conductor-item' + (selected === instrument_id ? ' selected' : '')
                        }
                        onClick={() => set_selected(instrument_id)}
                    >
                        {instrument_id}
                    </button>
                ))}
            </div>
            {selected === global_id && (
                <ConductorCallbackEditor
                    key={`${global_id}-${load_serial}`}
                    path={global_id}
                    initial_value={global_callback_source}
                    on_change={on_global_callback_source_change}
                    empty_value_example={global_callback_example}
                />
            )}
            {selected && selected !== global_id && (
                <ConductorCallbackEditor
                    key={`${selected}-${load_serial}`}
                    path={selected}
                    initial_value={score_param_bindings[selected] ?? ''}
                    on_change={(source) => update_source(selected, source)}
                    empty_value_example={instrument_callback_example}
                />
            )}
        </div>
    );
}
