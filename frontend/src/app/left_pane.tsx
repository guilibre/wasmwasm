import { useCallback, useEffect, useRef, useState } from 'react';
import type { VirtualTypeScriptEnvironment } from '@typescript/vfs';
import type { OrchestraState, PatchView } from '../patch/use_patch_store';

import { OrchestraPanel } from './orchestra';
import { InstrumentPanel } from './instruments';
import './left_pane.scss';

const MIN_WIDTH = 200;
const MAX_WIDTH = 900;
const MIN_ORCH_HEIGHT = 200;
const MAX_ORCH_HEIGHT = 600;

interface Props {
    orchestra: OrchestraState;
    on_bpm_change: (bpm: number) => void;
    on_add: () => void;
    on_remove: (id: string) => void;
    on_rename: (id: string, name: string) => void;
    on_instrument_code_change: (id: string, code: string) => void;
    on_orchestra_code_change: (code: string) => void;
    on_set_active: (id: string) => void;
    on_compile_patch: () => void;
    on_compile_instrument: () => void;
    on_compile_orchestra: () => void;
    compile_status: Map<string, 'idle' | 'compiling' | 'ok' | 'error'>;
    orchestra_env: VirtualTypeScriptEnvironment | null;
    instrument_envs: Map<string, VirtualTypeScriptEnvironment>;
    on_global_patch_code_change: (code: string) => void;
    global_env: VirtualTypeScriptEnvironment | null;
    view: PatchView;
    on_view_change: (view: PatchView) => void;
}

export function LeftPane({
    orchestra,
    on_bpm_change,
    on_add,
    on_remove,
    on_rename,
    on_instrument_code_change,
    on_orchestra_code_change,
    on_set_active,
    on_compile_patch,
    on_compile_instrument,
    on_compile_orchestra,
    compile_status,
    orchestra_env,
    instrument_envs,
    on_global_patch_code_change,
    global_env,
    view,
    on_view_change,
}: Props) {
    const [width, set_width] = useState(() => window.innerWidth * 0.4);
    const width_ref = useRef(width);
    useEffect(() => {
        width_ref.current = width;
    }, [width]);

    const [orch_height, set_orch_height] = useState(() => window.innerHeight * 0.6);
    const orch_height_ref = useRef(orch_height);
    useEffect(() => {
        orch_height_ref.current = orch_height;
    }, [orch_height]);

    const on_width_mousedown = useCallback((e: React.MouseEvent) => {
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

    const on_height_mousedown = useCallback((e: React.MouseEvent) => {
        e.preventDefault();
        const start_y = e.clientY;
        const start_h = orch_height_ref.current;
        const on_move = (ev: MouseEvent) => {
            const delta = ev.clientY - start_y;
            set_orch_height(Math.max(MIN_ORCH_HEIGHT, Math.min(MAX_ORCH_HEIGHT, start_h + delta)));
        };
        const on_up = () => {
            window.removeEventListener('mousemove', on_move);
            window.removeEventListener('mouseup', on_up);
        };
        window.addEventListener('mousemove', on_move);
        window.addEventListener('mouseup', on_up);
    }, []);

    return (
        <div className="app__instrument-pane" style={{ width }}>
            <OrchestraPanel
                bpm={orchestra.bpm}
                code={orchestra.code}
                height={orch_height}
                on_bpm_change={on_bpm_change}
                on_code_change={on_orchestra_code_change}
                on_compile={on_compile_orchestra}
                compile_status={compile_status.get('orchestra') ?? 'idle'}
                env={orchestra_env}
            />
            <div className="app__panel-divider" onMouseDown={on_height_mousedown} />
            <InstrumentPanel
                instruments={orchestra.instruments}
                active_instrument_id={orchestra.active_id}
                view={view}
                on_add={on_add}
                on_remove={on_remove}
                on_rename={on_rename}
                on_code_change={on_instrument_code_change}
                on_set_active={on_set_active}
                on_compile_patch={on_compile_patch}
                on_compile_instrument={on_compile_instrument}
                compile_status={compile_status}
                instrument_envs={instrument_envs}
                global_code={orchestra.global_patch_code}
                on_global_code_change={on_global_patch_code_change}
                global_env={global_env}
                on_view_change={on_view_change}
            />
            <div className="app__orch-handle" onMouseDown={on_width_mousedown} />
        </div>
    );
}
