import { useEffect, useState } from 'react';
import type { VirtualTypeScriptEnvironment } from '@typescript/vfs';
import { TsEditor } from './ts_editor';
import { get_orchestra_env, ORCHESTRA_FALLBACK } from './ts_env';
import './instrument.scss';

interface Props {
    bpm: number;
    code: string;
    height: number;
    on_bpm_change: (bpm: number) => void;
    on_code_change: (code: string) => void;
}

export function OrchestraPanel({ bpm, code, height, on_bpm_change, on_code_change }: Props) {
    const [env, set_env] = useState<VirtualTypeScriptEnvironment | null>(null);

    useEffect(() => {
        get_orchestra_env().then(set_env);
    }, []);

    const commit_bpm = (
        e: React.FocusEvent<HTMLInputElement> | React.KeyboardEvent<HTMLInputElement>,
    ) => {
        const v = parseFloat((e.target as HTMLInputElement).value);
        if (isFinite(v) && v > 0) on_bpm_change(v);
    };

    return (
        <div className="instrument instrument--orchestra" style={{ height }}>
            <div className="instrument__header">
                <div className="instrument__controls">
                    <span className="instrument__title">orchestra</span>
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
                </div>
            </div>
            <div className="instrument__editor">
                <TsEditor
                    key="orchestra"
                    initial_value={code || ORCHESTRA_FALLBACK}
                    on_change={on_code_change}
                    env={env}
                />
            </div>
        </div>
    );
}
