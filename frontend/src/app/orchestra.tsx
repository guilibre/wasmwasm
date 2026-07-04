import { useState } from 'react';
import type { VirtualTypeScriptEnvironment } from '@typescript/vfs';
import { TsEditor } from './ts_editor';
import ScoreEditor, { type ScoreEditorHandle } from './score_editor';
import './instrument.scss';

type CompileStatus = 'idle' | 'compiling' | 'ok' | 'error';

interface Props {
    bpm: number;
    code: string;
    height: number;
    on_bpm_change: (bpm: number) => void;
    on_code_change: (code: string) => void;
    on_compile: () => void;
    compile_status: CompileStatus;
    env: VirtualTypeScriptEnvironment | null;
    score_code: string;
    on_score_code_change: (code: string) => void;
    on_score_compile: () => void;
    score_compile_status: CompileStatus;
    score_editor_ref: React.Ref<ScoreEditorHandle>;
}

export function OrchestraPanel({
    bpm,
    code,
    height,
    on_bpm_change,
    on_code_change,
    on_compile,
    compile_status,
    env,
    score_code,
    on_score_code_change,
    on_score_compile,
    score_compile_status,
    score_editor_ref,
}: Props) {
    const [active_tab, set_active_tab] = useState<'orchestra' | 'score'>('orchestra');

    const commit_bpm = (
        e: React.FocusEvent<HTMLInputElement> | React.KeyboardEvent<HTMLInputElement>,
    ) => {
        const v = parseFloat((e.target as HTMLInputElement).value);
        if (isFinite(v) && v > 0) on_bpm_change(v);
    };

    const active_compile_status =
        active_tab === 'orchestra' ? compile_status : score_compile_status;
    const on_active_compile = active_tab === 'orchestra' ? on_compile : on_score_compile;

    return (
        <div className="instrument instrument--orchestra" style={{ height }}>
            <div className="instrument__header">
                <div className="instrument__tabs">
                    <div
                        className={`instrument__tab${active_tab === 'orchestra' ? ' instrument__tab--active' : ''}`}
                        onClick={() => set_active_tab('orchestra')}
                    >
                        orchestra
                    </div>
                    <div
                        className={`instrument__tab${active_tab === 'score' ? ' instrument__tab--active' : ''}`}
                        onClick={() => set_active_tab('score')}
                    >
                        score
                    </div>
                </div>
                <div className="instrument__controls">
                    {active_tab === 'orchestra' && (
                        <label className="instrument__bpm-label">
                            BPM
                            <input
                                key={bpm}
                                className="instrument__bpm-input"
                                type="number"
                                min={1}
                                max={999}
                                defaultValue={bpm}
                                onBlur={commit_bpm}
                                onKeyDown={(e) => e.key === 'Enter' && commit_bpm(e)}
                            />
                        </label>
                    )}
                    <button
                        className="instrument__compile-btn"
                        disabled={active_compile_status === 'compiling'}
                        onClick={on_active_compile}
                    >
                        {active_compile_status === 'compiling' ? '...' : 'Compile'}
                    </button>
                    {active_compile_status === 'ok' && (
                        <span className="instrument__compile-ok">✓</span>
                    )}
                    {active_compile_status === 'error' && (
                        <span className="instrument__compile-err">✗</span>
                    )}
                </div>
            </div>
            <div className="instrument__editor">
                {active_tab === 'orchestra' ? (
                    <TsEditor initial_value={code} on_change={on_code_change} env={env} />
                ) : (
                    <ScoreEditor
                        ref={score_editor_ref}
                        initial_value={score_code}
                        on_change={on_score_code_change}
                    />
                )}
            </div>
        </div>
    );
}
