import './score_panel.scss';
import { ConductorPanel } from './conductor_panel';
import { useEffect, useRef, useState, useCallback } from 'react';
import ScoreWasm from '../scorewasm/compiler';
import type { OrchestraState, ScoreParamBindings } from '../patch/store/patch_types';
import WWEditor from './ww_editor';

interface Props {
    source: string;
    on_change: (source: string) => void;
    orchestra: OrchestraState;
    score_param_bindings: ScoreParamBindings;
    on_score_param_bindings_change: (bindings: ScoreParamBindings) => void;
    global_callback_source: string;
    on_global_callback_source_change: (source: string) => void;
    on_bpm_change: (bpm: number) => void;
}

type Tab = 'score' | 'conductor';

const MIN_WIDTH = 220;
const MAX_WIDTH = 600;
const DEFAULT_WIDTH = 600;

export function ScorePanel({
    source,
    on_change,
    orchestra,
    score_param_bindings,
    on_score_param_bindings_change,
    global_callback_source,
    on_global_callback_source_change,
    on_bpm_change,
}: Props) {
    const [width, set_width] = useState(DEFAULT_WIDTH);
    const [collapsed, set_collapsed] = useState(false);
    const [ready, set_ready] = useState(false);
    const [tab, set_tab] = useState<Tab>('score');
    const width_ref = useRef(width);
    useEffect(() => {
        width_ref.current = width;
    }, [width]);

    useEffect(() => {
        ScoreWasm.ensureReady().then(() => set_ready(true));
    }, []);

    const on_handle_mousedown = useCallback((e: React.MouseEvent) => {
        e.preventDefault();
        const start_x = e.clientX;
        const start_w = width_ref.current;
        const on_move = (ev: MouseEvent) => {
            const delta = ev.clientX - start_x;
            set_width(Math.max(MIN_WIDTH, Math.min(MAX_WIDTH, start_w + delta)));
        };
        const on_up = () => {
            window.removeEventListener('mousemove', on_move);
            window.removeEventListener('mouseup', on_up);
        };
        window.addEventListener('mousemove', on_move);
        window.addEventListener('mouseup', on_up);
    }, []);

    if (collapsed) {
        return (
            <button
                className="app__score-panel-open"
                onClick={() => set_collapsed(false)}
                title="Abrir score"
            >
                ›
            </button>
        );
    }

    return (
        <div className="app__score-panel" style={{ width }}>
            <div className="app__score-panel-header">
                <div className="app__score-panel-tabs">
                    <button
                        className={
                            'app__score-panel-tab' +
                            (tab === 'score' ? ' app__score-panel-tab--active' : '')
                        }
                        onClick={() => set_tab('score')}
                    >
                        score
                    </button>
                    <button
                        className={
                            'app__score-panel-tab' +
                            (tab === 'conductor' ? ' app__score-panel-tab--active' : '')
                        }
                        onClick={() => set_tab('conductor')}
                    >
                        conductor
                    </button>
                </div>
                <button
                    className="app__score-panel-close"
                    onClick={() => set_collapsed(true)}
                    title="Fechar score"
                >
                    ×
                </button>
            </div>
            {tab === 'score' && ready && (
                <WWEditor
                    initial_value={source}
                    on_change={on_change}
                    get_module={() => ScoreWasm.getModule()}
                />
            )}
            {tab === 'conductor' && (
                <ConductorPanel
                    orchestra={orchestra}
                    score_param_bindings={score_param_bindings}
                    on_change={on_score_param_bindings_change}
                    global_callback_source={global_callback_source}
                    on_global_callback_source_change={on_global_callback_source_change}
                    on_bpm_change={on_bpm_change}
                />
            )}
            <div className="app__score-panel-handle" onMouseDown={on_handle_mousedown} />
        </div>
    );
}
