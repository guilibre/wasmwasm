import { useEffect, useRef, useState, useCallback, useMemo } from 'react';
import { ReactFlowProvider } from '@xyflow/react';
import type { VirtualTypeScriptEnvironment } from '@typescript/vfs';
import workletUrl from '../audio/processor.worklet.ts?worker&url';
import WasmWasm, { type PatchParams } from '../audio/compiler';
import WWEditor, { type WWEditorHandle } from './ww_editor';
import { Sidebar } from './sidebar';
import { LeftPane } from './left_pane';
import { PatchEditor } from '../patch/patch_editor';
import { usePatchStore } from '../patch/use_patch_store';
import { patch_to_json } from '../patch/patch_to_json';
import {
    make_orchestra_env_with_instruments,
    make_instrument_env_with_params,
    get_orchestra_env,
    type CompiledInstrument,
} from './ts_env';
import './app.scss';

export default function App() {
    const audio_context_ref = useRef<AudioContext | null>(null);
    const merger_ref = useRef<GainNode | null>(null);
    const worklet_nodes_ref = useRef<Map<string, AudioWorkletNode>>(new Map());
    const mic_source_ref = useRef<MediaStreamAudioSourceNode | null>(null);
    const analyser_l_ref = useRef<AnalyserNode | null>(null);
    const analyser_r_ref = useRef<AnalyserNode | null>(null);
    const orchestra_worker_ref = useRef<Worker | null>(null);
    const patch_cache_ref = useRef<
        Map<string, { json: string; wasm: Uint8Array; param_names: string[] }>
    >(new Map());
    const instr_code_cache_ref = useRef<Map<string, string>>(new Map());
    const [analysers, set_analysers] = useState<{ l: AnalyserNode; r: AnalyserNode } | null>(null);
    const [is_playing, set_is_playing] = useState(false);
    const [error, set_error] = useState<string | null>(null);
    const import_ref = useRef<HTMLInputElement>(null);
    const editor_ref = useRef<WWEditorHandle>(null);

    const [compile_status, set_compile_status] = useState<
        Map<string, 'idle' | 'compiling' | 'ok' | 'error'>
    >(new Map());
    const [compiled_patch_params, set_compiled_patch_params] = useState<Map<string, string[]>>(
        new Map(),
    );
    const [compiled_instrument_info, set_compiled_instrument_info] = useState<
        Map<string, CompiledInstrument>
    >(new Map());
    const [orchestra_env, set_orchestra_env] = useState<VirtualTypeScriptEnvironment | null>(null);
    const [instrument_envs, set_instrument_envs] = useState<
        Map<string, VirtualTypeScriptEnvironment>
    >(new Map());

    useEffect(() => {
        set_orchestra_env(get_orchestra_env());
    }, []);

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
        undo,
        redo,
    } = store;
    const selected_block = selected_node?.type === 'block' ? selected_node : null;

    useEffect(() => {
        const on_key = (e: KeyboardEvent) => {
            if (!e.ctrlKey && !e.metaKey) return;
            if (e.target instanceof HTMLInputElement || e.target instanceof HTMLTextAreaElement)
                return;
            if (e.key === 'z' && !e.shiftKey) {
                e.preventDefault();
                undo();
            }
            if (e.key === 'y' || (e.key === 'z' && e.shiftKey)) {
                e.preventDefault();
                redo();
            }
        };
        window.addEventListener('keydown', on_key);
        return () => window.removeEventListener('keydown', on_key);
    }, [undo, redo]);

    const prev_patch_data = useRef<Map<string, { nodes: unknown; edges: unknown }>>(new Map());
    useEffect(() => {
        for (const instr of orchestra.instruments) {
            const prev = prev_patch_data.current.get(instr.id);
            if (prev && (prev.nodes !== instr.nodes || prev.edges !== instr.edges)) {
                patch_cache_ref.current.delete(instr.id);
                set_compile_status((s) => {
                    const m = new Map(s);
                    m.delete(`patch:${instr.id}`);
                    return m;
                });
            }
            prev_patch_data.current.set(instr.id, { nodes: instr.nodes, edges: instr.edges });
        }
    }, [orchestra.instruments]);

    const handle_instrument_code_change = useCallback(
        (id: string, code: string) => {
            set_instrument_code(id, code);
            instr_code_cache_ref.current.delete(id);
            set_compile_status((s) => {
                const m = new Map(s);
                m.delete(`instr:${id}`);
                return m;
            });
        },
        [set_instrument_code],
    );

    const handle_orchestra_code_change = useCallback(
        (code: string) => {
            set_orchestra_code(code);
            set_compile_status((s) => {
                const m = new Map(s);
                m.delete('orchestra');
                return m;
            });
        },
        [set_orchestra_code],
    );

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

    const set_status = (key: string, status: 'idle' | 'compiling' | 'ok' | 'error') => {
        set_compile_status((prev) => new Map(prev).set(key, status));
    };

    const compile_patch = useCallback(async () => {
        set_error(null);
        const sample_rate = audio_context_ref.current?.sampleRate ?? 44100;
        await Promise.all(
            orchestra.instruments.map(async (instr) => {
                const key = `patch:${instr.id}`;
                set_status(key, 'compiling');
                try {
                    const json = patch_to_json(instr.nodes, instr.edges);
                    const { wasm, params } = await WasmWasm.init_patch(sample_rate, json);
                    patch_cache_ref.current.set(instr.id, {
                        json,
                        wasm,
                        param_names: params.param_names,
                    });
                    set_compiled_patch_params((prev) =>
                        new Map(prev).set(instr.id, params.param_names),
                    );
                    const env = make_instrument_env_with_params(params.param_names);
                    set_instrument_envs((prev) => new Map(prev).set(instr.id, env));
                    set_status(key, 'ok');
                } catch (e) {
                    set_error(`[patch:${instr.name}] ${String(e)}`);
                    set_status(key, 'error');
                }
            }),
        );
    }, [orchestra.instruments]);

    const compile_instrument = useCallback(async () => {
        set_error(null);
        const ts = await import('typescript');
        let new_info_map = new Map(compiled_instrument_info);
        await Promise.all(
            orchestra.instruments.map(async (instr) => {
                const key = `instr:${instr.id}`;
                set_status(key, 'compiling');
                try {
                    const compiled_code = ts.transpileModule(instr.code, {
                        compilerOptions: { target: ts.ScriptTarget.ES2025, module: 0 },
                    }).outputText;
                    instr_code_cache_ref.current.set(instr.id, compiled_code);
                    const fn_names = [
                        ...instr.code.matchAll(/^(?:async\s+)?function\s+(\w+)/gm),
                    ].map((m) => m[1]);
                    const param_names = compiled_patch_params.get(instr.id) ?? [];
                    new_info_map = new Map(new_info_map).set(instr.id, {
                        name: instr.name,
                        param_names,
                        fn_names,
                    });
                    set_status(key, 'ok');
                } catch (e) {
                    set_error(`[instr:${instr.name}] ${String(e)}`);
                    set_status(key, 'error');
                }
            }),
        );
        set_compiled_instrument_info(new_info_map);
        const env = make_orchestra_env_with_instruments([...new_info_map.values()]);
        set_orchestra_env(env);
    }, [orchestra.instruments, compiled_patch_params, compiled_instrument_info]);

    const compile_orchestra = useCallback(async () => {
        set_status('orchestra', 'compiling');
        set_error(null);
        try {
            const ts = await import('typescript');
            ts.transpileModule(orchestra.code, {
                compilerOptions: { target: ts.ScriptTarget.ES2025, module: 0 },
            });
            set_status('orchestra', 'ok');
        } catch (e) {
            set_error(`[orchestra] ${String(e)}`);
            set_status('orchestra', 'error');
        }
    }, [orchestra.code]);

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

        const instr_json = new Map<string, string>();
        const instr_wasm = new Map<string, Uint8Array>();
        const instr_compiled_code = new Map<string, string>();
        const instr_instance_counters = new Map<string, number>();

        for (const instr of orchestra.instruments) {
            const cached_patch = patch_cache_ref.current.get(instr.id);
            let json: string;
            let wasm: Uint8Array;
            if (cached_patch) {
                ({ json, wasm } = cached_patch);
            } else {
                json = patch_to_json(instr.nodes, instr.edges);
                try {
                    ({ wasm } = await WasmWasm.init_patch(context.sampleRate, json));
                } catch (e) {
                    set_error(`[${instr.name}] ${String(e)}`);
                    continue;
                }
            }
            const compiled_code =
                instr_code_cache_ref.current.get(instr.id) ??
                (instr.code.trim()
                    ? ts.transpileModule(instr.code, {
                          compilerOptions: { target: ts.ScriptTarget.ES2025, module: 0 },
                      }).outputText
                    : '');
            instr_json.set(instr.name, json);
            instr_wasm.set(instr.name, wasm);
            instr_compiled_code.set(instr.name, compiled_code);
        }

        const create_instance = async (
            instr: (typeof orchestra.instruments)[number],
            json: string,
            wasm: Uint8Array,
            instance_idx: number,
        ): Promise<{
            state_sab: SharedArrayBuffer;
            event_sab: SharedArrayBuffer;
            param_names: string[];
        } | null> => {
            let params: PatchParams;
            try {
                ({ params } = await WasmWasm.init_patch(context.sampleRate, json));
            } catch (e) {
                set_error(`[${instr.name}] ${String(e)}`);
                return null;
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

            await new Promise<void>((resolve) => {
                const handler = (e: MessageEvent) => {
                    if (e.data.type === 'ready') {
                        node.port.removeEventListener('message', handler);
                        resolve();
                    }
                };
                node.port.addEventListener('message', handler);
                node.port.start();
                node.port.postMessage({ type: 'load-wasm', buffer: wasm });
                node.port.postMessage({
                    type: 'load-params-sab',
                    input_sab: params.input_sab,
                    state_sab: params.state_sab,
                    event_sab: params.event_sab,
                    param_export_names: params.param_export_names,
                });
            });

            return {
                state_sab: params.state_sab,
                event_sab: params.event_sab,
                param_names: params.param_names,
            };
        };

        const worker = new Worker(new URL('../audio/orchestra_worker.ts', import.meta.url), {
            type: 'module',
        });
        worker.onmessage = async (e) => {
            if (e.data.type === 'error') {
                set_error(`[orchestra] ${e.data.message}`);
                return;
            }
            if (e.data.type === 'stop') {
                stop(e.data.dur ?? 1);
                return;
            }
            if (e.data.type === 'request-instance') {
                const { name, request_id } = e.data as { name: string; request_id: number };
                const json = instr_json.get(name);
                const wasm = instr_wasm.get(name);
                const instr = orchestra.instruments.find((i) => i.name === name);
                if (!json || !wasm || !instr) {
                    worker.postMessage({
                        type: 'instance-error',
                        request_id,
                        message: `Unknown instrument: ${name}`,
                    });
                    return;
                }
                const instance_idx = instr_instance_counters.get(name) ?? 0;
                instr_instance_counters.set(name, instance_idx + 1);
                const result = await create_instance(instr, json, wasm, instance_idx);
                if (!result) {
                    worker.postMessage({
                        type: 'instance-error',
                        request_id,
                        message: `Failed to create instance of ${name}`,
                    });
                    return;
                }
                worker.postMessage({
                    type: 'instance-ready',
                    request_id,
                    state_sab: result.state_sab,
                    event_sab: result.event_sab,
                    param_names: result.param_names,
                    code: instr_compiled_code.get(name) ?? '',
                });
            }
        };
        worker.postMessage({
            type: 'run',
            orchestra_code: compiled_orchestra,
            bpm: orchestra.bpm,
            sampleRate: context.sampleRate,
            audioCurrentTime: context.currentTime,
            instrument_names: [...instr_json.keys()],
        });
        orchestra_worker_ref.current = worker;

        set_is_playing(true);
    };

    const stop = async (dur: number = 1) => {
        const context = audio_context_ref.current;
        const merger = merger_ref.current;
        if (context && merger) {
            merger.gain.setValueAtTime(merger.gain.value, context.currentTime);
            merger.gain.linearRampToValueAtTime(0, context.currentTime + dur);
            await new Promise<void>((r) => setTimeout(r, dur * 1000));
            merger.gain.setValueAtTime(1, context.currentTime);
        }
        orchestra_worker_ref.current?.terminate();
        orchestra_worker_ref.current = null;
        if (mic_source_ref.current) {
            mic_source_ref.current.mediaStream.getTracks().forEach((t) => t.stop());
            mic_source_ref.current.disconnect();
            mic_source_ref.current = null;
        }
        worklet_nodes_ref.current.forEach((n) => n.disconnect());
        worklet_nodes_ref.current.clear();
        await context?.suspend();
        set_is_playing(false);
        set_analysers(null);
    };

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
                    on_orchestra_code_change={handle_orchestra_code_change}
                    on_set_active={set_active_instrument}
                    on_compile_patch={compile_patch}
                    on_compile_instrument={compile_instrument}
                    on_compile_orchestra={compile_orchestra}
                    compile_status={compile_status}
                    orchestra_env={orchestra_env}
                    instrument_envs={instrument_envs}
                />

                <div className="app__patch-pane">
                    <ReactFlowProvider>
                        <PatchEditor store={store} />
                    </ReactFlowProvider>
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
