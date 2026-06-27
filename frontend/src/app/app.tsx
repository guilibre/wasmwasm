import { useEffect, useRef, useState, useCallback, useMemo } from 'react';
import { ReactFlowProvider } from '@xyflow/react';
import workletUrl from '../audio/processor.worklet.js?url';
import WasmWasm, { type PatchParams } from '../audio/compiler';
import WWEditor, { type WWEditorHandle } from './ww_editor';
import { Sidebar } from './sidebar';
import { LeftPane } from './left_pane';
import { PatchEditor } from '../patch/patch_editor';
import { usePatchStore } from '../patch/use_patch_store';
import { patch_to_json } from '../patch/patch_to_json';
import { patch_to_hash, hash_to_patch } from '../patch/share';
import './app.scss';

export default function App() {
    const audio_context_ref = useRef<AudioContext | null>(null);
    const merger_ref = useRef<GainNode | null>(null);
    const worklet_nodes_ref = useRef<Map<string, AudioWorkletNode>>(new Map());
    const mic_source_ref = useRef<MediaStreamAudioSourceNode | null>(null);
    const analyser_l_ref = useRef<AnalyserNode | null>(null);
    const analyser_r_ref = useRef<AnalyserNode | null>(null);
    const orchestra_worker_ref = useRef<Worker | null>(null);
    const [analysers, set_analysers] = useState<{ l: AnalyserNode; r: AnalyserNode } | null>(null);
    const [is_playing, set_is_playing] = useState(false);
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
        add_instrument,
        remove_instrument,
        set_instrument_code,
        rename_instrument,
        set_active_instrument,
        load_patch,
    } = store;
    const selected_block = selected_node?.type === 'block' ? selected_node : null;

    useEffect(() => {
        import('typescript');
    }, []);

    useEffect(() => {
        const worker_ref = orchestra_worker_ref;
        const nodes_ref = worklet_nodes_ref;
        const ctx_ref = audio_context_ref;
        const mic_ref = mic_source_ref;
        return () => {
            worker_ref.current?.terminate();
            nodes_ref.current.forEach((n) => n.disconnect());
            ctx_ref.current?.close();
            mic_ref.current?.mediaStream.getTracks().forEach((t) => t.stop());
        };
    }, []);

    const [sidebar_visible, set_sidebar_visible] = useState(true);
    const [editing_block_id, set_editing_block_id] = useState<string | null>(null);
    const [name_draft, set_name_draft] = useState('');
    const [modal_drag_pos, set_modal_drag_pos] = useState<{
        block_id: string;
        x: number;
        y: number;
    } | null>(null);
    const modal_drag_offset = useRef<{ dx: number; dy: number } | null>(null);
    const modal_pos = useMemo(
        () =>
            modal_drag_pos?.block_id === selected_block?.id && modal_drag_pos != null
                ? { x: modal_drag_pos.x, y: modal_drag_pos.y }
                : { x: window.innerWidth / 2 - 300, y: window.innerHeight / 2 - 210 },
        [modal_drag_pos, selected_block?.id],
    );

    const editing_name = editing_block_id === selected_block?.id;

    const start_name_edit = useCallback(() => {
        set_name_draft((selected_block?.data as { name: string })?.name ?? '');
        set_editing_block_id(selected_block?.id ?? null);
    }, [selected_block]);

    const commit_name = useCallback(() => {
        const trimmed = name_draft.trim();
        if (trimmed && selected_block) update_name(selected_block.id, trimmed);
        set_editing_block_id(null);
    }, [name_draft, selected_block, update_name]);

    const on_modal_header_mouse_down = useCallback(
        (e: React.MouseEvent) => {
            if ((e.target as HTMLElement).closest('input, button')) return;
            const block_id = selected_block?.id;
            if (!block_id) return;
            modal_drag_offset.current = {
                dx: e.clientX - modal_pos.x,
                dy: e.clientY - modal_pos.y,
            };
            const on_move = (ev: MouseEvent) => {
                if (!modal_drag_offset.current) return;
                set_modal_drag_pos({
                    block_id,
                    x: ev.clientX - modal_drag_offset.current.dx,
                    y: ev.clientY - modal_drag_offset.current.dy,
                });
            };
            const on_up = () => {
                modal_drag_offset.current = null;
                document.removeEventListener('mousemove', on_move);
                document.removeEventListener('mouseup', on_up);
            };
            document.addEventListener('mousemove', on_move);
            document.addEventListener('mouseup', on_up);
        },
        [modal_pos, selected_block?.id],
    );

    const on_name_key_down = useCallback(
        (e: React.KeyboardEvent<HTMLInputElement>) => {
            if (e.key === 'Enter') commit_name();
            else if (e.key === 'Escape') set_editing_block_id(null);
        },
        [commit_name],
    );

    useEffect(() => {
        WasmWasm.ensureReady().then(() => editor_ref.current?.refresh());
    }, []);

    useEffect(() => {
        const hash = window.location.hash;
        if (!hash) return;
        hash_to_patch(hash).then((orchestra) => {
            if (orchestra) load_patch(orchestra);
        });
    }, [load_patch]);

    const share = useCallback(async () => {
        const hash = await patch_to_hash(orchestra);
        window.location.hash = hash;
        await navigator.clipboard.writeText(window.location.href);
    }, [orchestra]);

    const needs_capture = (patch_json: string) => patch_json.includes('capture_');

    const setup_capture = async (context: AudioContext, node: AudioWorkletNode) => {
        if (!mic_source_ref.current) {
            let stream: MediaStream;
            try {
                stream = await navigator.mediaDevices.getUserMedia({
                    audio: {
                        echoCancellation: false,
                        noiseSuppression: false,
                        autoGainControl: false,
                    },
                    video: false,
                });
            } catch (e) {
                throw new Error(`Microphone unavailable: ${e instanceof Error ? e.message : e}`, {
                    cause: e,
                });
            }
            mic_source_ref.current = context.createMediaStreamSource(stream);
        }
        mic_source_ref.current.connect(node);
    };

    const play = async () => {
        set_error(null);

        if (!audio_context_ref.current) {
            const context = new AudioContext({ latencyHint: 'interactive' });
            await context.audioWorklet.addModule(workletUrl);

            const merger = context.createGain();
            const splitter = context.createChannelSplitter(2);
            const al = context.createAnalyser();
            const ar = context.createAnalyser();
            al.fftSize = 4096;
            ar.fftSize = 4096;
            al.smoothingTimeConstant = 0.75;
            ar.smoothingTimeConstant = 0.75;
            merger.connect(context.destination);
            merger.connect(splitter);
            splitter.connect(al, 0);
            splitter.connect(ar, 1);

            merger_ref.current = merger;
            analyser_l_ref.current = al;
            analyser_r_ref.current = ar;
            audio_context_ref.current = context;
        }

        set_analysers(
            analyser_l_ref.current && analyser_r_ref.current
                ? { l: analyser_l_ref.current, r: analyser_r_ref.current }
                : null,
        );

        worklet_nodes_ref.current.forEach((n) => n.disconnect());
        worklet_nodes_ref.current.clear();
        orchestra_worker_ref.current?.terminate();
        orchestra_worker_ref.current = null;

        const context = audio_context_ref.current!;
        const merger = merger_ref.current!;
        await context.resume();

        const ts = await import('typescript');

        let compiled_orchestra = '';
        if (orchestra.code.trim()) {
            try {
                compiled_orchestra = ts.transpileModule(orchestra.code, {
                    compilerOptions: { target: ts.ScriptTarget.ES2025, module: 0 },
                }).outputText;
            } catch (e) {
                set_error(`[orchestra] ${String(e)}`);
            }
        }

        const instance_counts = new Map<string, number>();
        for (const match of compiled_orchestra.matchAll(
            /instrument\s*\(\s*['"]([^'"]+)['"]\s*\)/g,
        )) {
            const name = match[1];
            instance_counts.set(name, (instance_counts.get(name) ?? 0) + 1);
        }

        let first_audio_time: number | null = null;
        const worker_instruments: {
            name: string;
            code: string;
            param_names: string[];
            state_sab: SharedArrayBuffer;
            event_sab: SharedArrayBuffer;
        }[] = [];

        const create_instance = async (
            instr: (typeof orchestra.instruments)[number],
            json: string,
            wasm: Uint8Array,
            instance_idx: number,
        ) => {
            let params: PatchParams;
            try {
                ({ params } = await WasmWasm.init_patch(context.sampleRate, json));
            } catch (e) {
                set_error(`[${instr.name}] ${String(e)}`);
                return;
            }

            const node = new AudioWorkletNode(context, 'wasm-processor', {
                numberOfInputs: 1,
                numberOfOutputs: 1,
                outputChannelCount: [2],
            });
            node.connect(merger);
            worklet_nodes_ref.current.set(`${instr.id}:${instance_idx}`, node);

            if (needs_capture(json)) {
                try {
                    await setup_capture(context, node);
                } catch (e) {
                    set_error(String(e));
                }
            }

            node.port.postMessage({ type: 'clear' });

            const perf_before = performance.now();
            const ready_promise = new Promise<number>((resolve) => {
                const handler = (e: MessageEvent) => {
                    if (e.data.type === 'ready') {
                        node.port.removeEventListener('message', handler);
                        resolve(
                            (e.data.startTime as number) + (performance.now() - perf_before) / 1000,
                        );
                    }
                };
                node.port.addEventListener('message', handler);
                node.port.start();
            });

            node.port.postMessage({ type: 'load-wasm', buffer: wasm });
            node.port.postMessage({
                type: 'load-params-sab',
                input_sab: params.input_sab,
                state_sab: params.state_sab,
                event_sab: params.event_sab,
                param_export_names: params.param_export_names,
            });

            const audio_current_time = await ready_promise;

            if (first_audio_time === null) first_audio_time = audio_current_time;

            worker_instruments.push({
                name: instr.name,
                code: instr.code,
                param_names: params.param_names,
                state_sab: params.state_sab,
                event_sab: params.event_sab,
            });
        };

        for (const instr of orchestra.instruments) {
            const json = patch_to_json(instr.nodes, instr.edges);

            let wasm: Uint8Array;
            try {
                ({ wasm } = await WasmWasm.init_patch(context.sampleRate, json));
            } catch (e) {
                set_error(`[${instr.name}] ${String(e)}`);
                continue;
            }

            const count = instance_counts.get(instr.name) ?? 1;
            for (let i = 0; i < count; i++) {
                await create_instance(instr, json, wasm, i);
            }
        }

        const compiled_instruments = worker_instruments.map((i) => ({
            ...i,
            code: i.code.trim()
                ? ts.transpileModule(i.code, {
                      compilerOptions: { target: ts.ScriptTarget.ES2025, module: 0 },
                  }).outputText
                : '',
        }));

        const worker = new Worker(new URL('../audio/orchestra_worker.ts', import.meta.url), {
            type: 'module',
        });
        worker.onmessage = (e) => {
            if (e.data.type === 'error') set_error(`[orchestra] ${e.data.message}`);
        };
        worker.postMessage({
            type: 'run',
            orchestra_code: compiled_orchestra,
            bpm: orchestra.bpm,
            sampleRate: context.sampleRate,
            audioCurrentTime: first_audio_time ?? 0,
            instruments: compiled_instruments,
        });
        orchestra_worker_ref.current = worker;

        set_is_playing(true);
    };

    const stop = async () => {
        if (mic_source_ref.current) {
            mic_source_ref.current.mediaStream.getTracks().forEach((t) => t.stop());
            mic_source_ref.current.disconnect();
            mic_source_ref.current = null;
        }
        worklet_nodes_ref.current.forEach((n) => n.disconnect());
        worklet_nodes_ref.current.clear();
        orchestra_worker_ref.current?.terminate();
        orchestra_worker_ref.current = null;
        await audio_context_ref.current?.suspend();
        set_is_playing(false);
        set_analysers(null);
    };

    return (
        <div className="app">
            <div className="app__toolbar">
                <span className="app__brand">wasmwasm</span>
                <button onClick={is_playing ? stop : play}>{is_playing ? 'Stop' : 'Play'}</button>
                <button onClick={export_patch}>Export</button>
                <button onClick={() => import_ref.current?.click()}>Import</button>
                <button onClick={share}>Share</button>
                <button onClick={() => set_sidebar_visible((v) => !v)}>
                    {sidebar_visible ? 'Hide sidebar' : 'Show sidebar'}
                </button>
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
                    on_instrument_code_change={set_instrument_code}
                    on_orchestra_code_change={set_orchestra_code}
                    on_set_active={set_active_instrument}
                />

                <div className="app__patch-pane">
                    <ReactFlowProvider>
                        <PatchEditor store={store} />
                    </ReactFlowProvider>
                    {sidebar_visible && (
                        <Sidebar
                            analyser_l={analysers?.l ?? null}
                            analyser_r={analysers?.r ?? null}
                        />
                    )}
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
