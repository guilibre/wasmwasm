import { useEffect, useRef, useState, useCallback, useMemo } from 'react';
import { ReactFlowProvider } from '@xyflow/react';
import type { VirtualTypeScriptEnvironment } from '@typescript/vfs';
import workletUrl from '../audio/processor.worklet.ts?worker&url';
import WasmWasm, { type PatchParams, type CompiledPatch } from '../audio/compiler';
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
import { setup_midi, add_on_midi_event, remove_on_midi_event } from '../audio/helpers';
import { GLOBAL_CACHE_KEY } from './constants';
import './app.scss';

function parse_fn_sigs(
    ts: typeof import('typescript'),
    code: string,
): { fn_names: string[]; fn_sigs: string[] } {
    const source = ts.createSourceFile('instr.ts', code, ts.ScriptTarget.ES2025);
    const fn_names: string[] = [];
    const fn_sigs: string[] = [];
    for (const stmt of source.statements) {
        if (ts.isFunctionDeclaration(stmt) && stmt.name) {
            fn_names.push(stmt.name.text);
            fn_sigs.push(
                stmt.parameters
                    .map((p) => {
                        const name = ts.isIdentifier(p.name) ? p.name.text : '_';
                        const type_str = p.type ? p.type.getText(source) : 'unknown';
                        const opt = p.initializer || p.questionToken ? '?' : '';
                        return `${name}${opt}: ${type_str}`;
                    })
                    .join(', '),
            );
        }
    }
    return { fn_names, fn_sigs };
}

export default function App() {
    const audio_context_ref = useRef<AudioContext | null>(null);
    const merger_ref = useRef<GainNode | null>(null);
    const global_node_ref = useRef<AudioWorkletNode | null>(null);
    const worklet_nodes_ref = useRef<Map<string, AudioWorkletNode>>(new Map());
    const capture_nodes_ref = useRef<Set<AudioWorkletNode>>(new Set());
    const mic_source_ref = useRef<MediaStreamAudioSourceNode | null>(null);
    const analyser_l_ref = useRef<AnalyserNode | null>(null);
    const analyser_r_ref = useRef<AnalyserNode | null>(null);
    const orchestra_worker_ref = useRef<Worker | null>(null);
    const midi_fwd_ref = useRef<string | null>(null);
    const play_session_ref = useRef(0);
    const play_inflight_ref = useRef<Promise<void> | null>(null);
    const patch_cache_ref = useRef<Map<string, { json: string; compiled: CompiledPatch }>>(
        new Map(),
    );
    const recorder_node_ref = useRef<AudioWorkletNode | null>(null);
    const recorded_buffers_ref = useRef<Float32Array[]>([]);
    const recorded_length_ref = useRef(0);
    const recorded_channels_ref = useRef(2);
    const ts_ref = useRef<typeof import('typescript') | null>(null);
    const [analysers, set_analysers] = useState<{ l: AnalyserNode; r: AnalyserNode } | null>(null);
    const [is_playing, set_is_playing] = useState(false);
    const [is_recording, set_is_recording] = useState(false);
    const is_recording_ref = useRef(false);
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
    const [orchestra_env, set_orchestra_env] = useState<VirtualTypeScriptEnvironment | null>(() =>
        get_orchestra_env(),
    );
    const [instrument_envs, set_instrument_envs] = useState<
        Map<string, VirtualTypeScriptEnvironment>
    >(new Map());

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
        set_global_patch_code,
        add_instrument,
        remove_instrument,
        set_instrument_code,
        rename_instrument,
        set_active_instrument,
        view,
        set_view,
        undo,
        redo,
    } = store;
    const active_instrument =
        orchestra.instruments.find((i) => i.id === orchestra.active_id) ?? null;
    const selected_block = selected_node?.type === 'block' ? selected_node : null;
    const initial_instruments_ref = useRef(orchestra.instruments);

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

    const prev_global_data_ref = useRef<{ nodes: unknown; edges: unknown } | null>(null);
    useEffect(() => {
        const prev = prev_global_data_ref.current;
        if (
            prev &&
            (prev.nodes !== orchestra.global_nodes || prev.edges !== orchestra.global_edges)
        ) {
            patch_cache_ref.current.delete(GLOBAL_CACHE_KEY);
        }
        prev_global_data_ref.current = {
            nodes: orchestra.global_nodes,
            edges: orchestra.global_edges,
        };
    }, [orchestra.global_nodes, orchestra.global_edges]);

    const sig_parse_timers_ref = useRef<Map<string, ReturnType<typeof setTimeout>>>(new Map());

    const handle_instrument_code_change = useCallback(
        (id: string, code: string) => {
            set_instrument_code(id, code);
            set_compile_status((s) => {
                const m = new Map(s);
                m.delete(`instr:${id}`);
                return m;
            });
            if (ts_ref.current) {
                const existing_timer = sig_parse_timers_ref.current.get(id);
                if (existing_timer !== undefined) clearTimeout(existing_timer);
                const timer = setTimeout(() => {
                    sig_parse_timers_ref.current.delete(id);
                    if (!ts_ref.current) return;
                    const { fn_names, fn_sigs } = parse_fn_sigs(ts_ref.current, code);
                    set_compiled_instrument_info((prev) => {
                        const existing = prev.get(id);
                        const new_map = new Map(prev).set(id, {
                            name: existing?.name ?? id,
                            param_names: existing?.param_names ?? [],
                            fn_names,
                            fn_sigs,
                        });
                        const sigs_changed =
                            !existing ||
                            existing.fn_names.length !== fn_names.length ||
                            existing.fn_names.some((f, i) => f !== fn_names[i]) ||
                            existing.fn_sigs.some((s, i) => s !== fn_sigs[i]);
                        if (sigs_changed) {
                            set_orchestra_env(
                                make_orchestra_env_with_instruments([...new_map.values()]),
                            );
                        }
                        return new_map;
                    });
                }, 2000);
                sig_parse_timers_ref.current.set(id, timer);
            }
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

    const handle_global_patch_code_change = useCallback(
        (code: string) => {
            set_global_patch_code(code);
            set_compile_status((s) => {
                const m = new Map(s);
                m.delete(`instr:${GLOBAL_CACHE_KEY}`);
                return m;
            });
            if (ts_ref.current) {
                const existing_timer = sig_parse_timers_ref.current.get(GLOBAL_CACHE_KEY);
                if (existing_timer !== undefined) clearTimeout(existing_timer);
                const timer = setTimeout(() => {
                    sig_parse_timers_ref.current.delete(GLOBAL_CACHE_KEY);
                    if (!ts_ref.current) return;
                    const { fn_names, fn_sigs } = parse_fn_sigs(ts_ref.current, code);
                    set_compiled_instrument_info((prev) => {
                        const existing = prev.get(GLOBAL_CACHE_KEY);
                        const new_map = new Map(prev).set(GLOBAL_CACHE_KEY, {
                            name: 'global',
                            param_names: existing?.param_names ?? [],
                            fn_names,
                            fn_sigs,
                        });
                        const sigs_changed =
                            !existing ||
                            existing.fn_names.length !== fn_names.length ||
                            existing.fn_names.some((f, i) => f !== fn_names[i]) ||
                            existing.fn_sigs.some((s, i) => s !== fn_sigs[i]);
                        if (sigs_changed) {
                            set_orchestra_env(
                                make_orchestra_env_with_instruments([...new_map.values()]),
                            );
                        }
                        return new_map;
                    });
                }, 2000);
                sig_parse_timers_ref.current.set(GLOBAL_CACHE_KEY, timer);
            }
        },
        [set_global_patch_code],
    );

    const initial_global_patch_code_ref = useRef(orchestra.global_patch_code);

    useEffect(() => {
        import('typescript').then((ts) => {
            ts_ref.current = ts;
            const new_map = new Map<string, CompiledInstrument>();
            for (const instr of initial_instruments_ref.current) {
                const { fn_names, fn_sigs } = parse_fn_sigs(ts, instr.code);
                new_map.set(instr.id, {
                    name: instr.name,
                    param_names: [],
                    fn_names,
                    fn_sigs,
                });
            }
            const { fn_names, fn_sigs } = parse_fn_sigs(ts, initial_global_patch_code_ref.current);
            new_map.set(GLOBAL_CACHE_KEY, {
                name: 'global',
                param_names: [],
                fn_names,
                fn_sigs,
            });
            set_compiled_instrument_info(new_map);
            set_orchestra_env(make_orchestra_env_with_instruments([...new_map.values()]));
        });
    }, []);

    useEffect(() => {
        const worker_ref = orchestra_worker_ref;
        const nodes_ref = worklet_nodes_ref;
        const capture_ref = capture_nodes_ref;
        const ctx_ref = audio_context_ref;
        const mic_ref = mic_source_ref;
        return () => {
            nodes_ref.current.forEach((n) => {
                n.port.postMessage({ type: 'stop' });
                n.port.postMessage({ type: 'clear' });
                n.disconnect();
            });
            worker_ref.current?.terminate();
            nodes_ref.current.clear();
            capture_ref.current.clear();
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

    const clear_recording_data = () => {
        recorded_buffers_ref.current.length = 0;
        recorded_length_ref.current = 0;
    };

    const download_blob = (blob: Blob, filename: string) => {
        const url = URL.createObjectURL(blob);
        const anchor = document.createElement('a');
        anchor.href = url;
        anchor.download = filename;
        anchor.click();
        URL.revokeObjectURL(url);
    };

    const encode_wav = (
        buffers: Float32Array[],
        length: number,
        sample_rate: number,
        num_channels: number = 2,
    ): Blob => {
        const bytes_per_sample = 2;
        const block_align = num_channels * bytes_per_sample;
        const data_size = length * bytes_per_sample;
        const array_buffer = new ArrayBuffer(44 + data_size);
        const view = new DataView(array_buffer);
        let offset = 0;
        const write_string = (s: string) => {
            for (let i = 0; i < s.length; i++) {
                view.setUint8(offset++, s.charCodeAt(i));
            }
        };
        write_string('RIFF');
        view.setUint32(offset, 36 + data_size, true);
        offset += 4;
        write_string('WAVE');
        write_string('fmt ');
        view.setUint32(offset, 16, true);
        offset += 4;
        view.setUint16(offset, 1, true);
        offset += 2;
        view.setUint16(offset, num_channels, true);
        offset += 2;
        view.setUint32(offset, sample_rate, true);
        offset += 4;
        view.setUint32(offset, sample_rate * block_align, true);
        offset += 4;
        view.setUint16(offset, block_align, true);
        offset += 2;
        view.setUint16(offset, bytes_per_sample * 8, true);
        offset += 2;
        write_string('data');
        view.setUint32(offset, data_size, true);
        offset += 4;

        let sample_offset = offset;
        for (const chunk of buffers) {
            for (let i = 0; i < chunk.length; i++) {
                const sample = Math.max(-1, Math.min(1, chunk[i]));
                view.setInt16(sample_offset, sample < 0 ? sample * 0x8000 : sample * 0x7fff, true);
                sample_offset += 2;
            }
        }

        return new Blob([array_buffer], { type: 'audio/wav' });
    };

    const create_recorder_node = (context: AudioContext) => {
        const node = new AudioWorkletNode(context, 'wasm-recorder', {
            numberOfInputs: 1,
            numberOfOutputs: 0,
            channelCount: 2,
            channelCountMode: 'explicit',
        });
        node.port.onmessage = (event: MessageEvent) => {
            if (event.data.type !== 'chunk') return;
            const chunk = event.data.data as Float32Array;
            recorded_channels_ref.current = event.data.num_channels ?? 2;
            recorded_buffers_ref.current.push(chunk);
            recorded_length_ref.current += chunk.length;
        };
        return node;
    };

    const start_recording = async () => {
        if (is_recording) return;
        const context = audio_context_ref.current;
        const merger = merger_ref.current;
        if (!context || !merger) return;
        clear_recording_data();
        const node = create_recorder_node(context);
        merger.connect(node);
        recorder_node_ref.current = node;
        is_recording_ref.current = true;
        set_is_recording(true);
    };

    const stop_recording = async () => {
        const node = recorder_node_ref.current;
        if (!node) return;
        is_recording_ref.current = false;
        set_is_recording(false);

        let finished = false;
        const finish_promise = new Promise<void>((resolve) => {
            const timeout = window.setTimeout(() => {
                finished = true;
                resolve();
            }, 200);
            node.port.onmessage = (event: MessageEvent) => {
                if (event.data.type === 'chunk') {
                    const chunk = event.data.data as Float32Array;
                    recorded_channels_ref.current = event.data.num_channels ?? 2;
                    recorded_buffers_ref.current.push(chunk);
                    recorded_length_ref.current += chunk.length;
                }
                if (event.data.type === 'finished') {
                    if (!finished) {
                        finished = true;
                        window.clearTimeout(timeout);
                        resolve();
                    }
                }
            };
        });

        node.port.postMessage({ type: 'stop' });
        await finish_promise;

        node.disconnect();
        recorder_node_ref.current = null;

        const context = audio_context_ref.current;
        if (context && recorded_length_ref.current > 0) {
            const blob = encode_wav(
                recorded_buffers_ref.current,
                recorded_length_ref.current,
                context.sampleRate,
                recorded_channels_ref.current,
            );
            download_blob(blob, `wasmwasm-recording-${Date.now()}.wav`);
        }
        clear_recording_data();
    };

    const record = async () => {
        if (is_recording) return;
        set_error(null);
        if (!is_playing) await play();
        await start_recording();
    };

    const compile_patch = useCallback(async () => {
        set_error(null);
        const sample_rate = audio_context_ref.current?.sampleRate ?? 44100;
        await Promise.all([
            ...orchestra.instruments.map(async (instr) => {
                const key = `patch:${instr.id}`;
                set_status(key, 'compiling');
                try {
                    const json = patch_to_json(instr.nodes, instr.edges);
                    const compiled = await WasmWasm.compile_patch(sample_rate, json);
                    patch_cache_ref.current.set(instr.id, { json, compiled });
                    set_compiled_patch_params((prev) =>
                        new Map(prev).set(instr.id, compiled.param_names),
                    );
                    const env = make_instrument_env_with_params(compiled.param_names);
                    set_instrument_envs((prev) => new Map(prev).set(instr.id, env));
                    set_status(key, 'ok');
                } catch (e) {
                    set_error(`[patch:${instr.name}] ${String(e)}`);
                    set_status(key, 'error');
                }
            }),
            (async () => {
                const key = `patch:${GLOBAL_CACHE_KEY}`;
                set_status(key, 'compiling');
                try {
                    const json = patch_to_json(orchestra.global_nodes, orchestra.global_edges);
                    const compiled = await WasmWasm.compile_patch(sample_rate, json);
                    patch_cache_ref.current.set(GLOBAL_CACHE_KEY, { json, compiled });
                    set_compiled_patch_params((prev) =>
                        new Map(prev).set(GLOBAL_CACHE_KEY, compiled.param_names),
                    );
                    const env = make_instrument_env_with_params(compiled.param_names);
                    set_instrument_envs((prev) => new Map(prev).set(GLOBAL_CACHE_KEY, env));
                    set_status(key, 'ok');
                } catch (e) {
                    set_error(`[global] ${String(e)}`);
                    set_status(key, 'error');
                }
            })(),
        ]);
    }, [orchestra.instruments, orchestra.global_nodes, orchestra.global_edges]);

    const compile_instrument = useCallback(async () => {
        set_error(null);
        const ts = await import('typescript');
        let new_info_map = new Map(compiled_instrument_info);
        await Promise.all([
            ...orchestra.instruments.map(async (instr) => {
                const key = `instr:${instr.id}`;
                set_status(key, 'compiling');
                try {
                    const { fn_names, fn_sigs } = parse_fn_sigs(ts, instr.code);
                    const param_names = compiled_patch_params.get(instr.id) ?? [];
                    new_info_map = new Map(new_info_map).set(instr.id, {
                        name: instr.name,
                        param_names,
                        fn_names,
                        fn_sigs,
                    });
                    set_status(key, 'ok');
                } catch (e) {
                    set_error(`[instr:${instr.name}] ${String(e)}`);
                    set_status(key, 'error');
                }
            }),
            (async () => {
                const key = `instr:${GLOBAL_CACHE_KEY}`;
                set_status(key, 'compiling');
                try {
                    const { fn_names, fn_sigs } = parse_fn_sigs(ts, orchestra.global_patch_code);
                    const param_names = compiled_patch_params.get(GLOBAL_CACHE_KEY) ?? [];
                    new_info_map = new Map(new_info_map).set(GLOBAL_CACHE_KEY, {
                        name: 'global',
                        param_names,
                        fn_names,
                        fn_sigs,
                    });
                    set_status(key, 'ok');
                } catch (e) {
                    set_error(`[global] ${String(e)}`);
                    set_status(key, 'error');
                }
            })(),
        ]);
        set_compiled_instrument_info(new_info_map);
        const env = make_orchestra_env_with_instruments([...new_info_map.values()]);
        set_orchestra_env(env);
    }, [
        orchestra.instruments,
        orchestra.global_patch_code,
        compiled_patch_params,
        compiled_instrument_info,
    ]);

    const compile_orchestra = useCallback(async () => {
        set_status('orchestra', 'compiling');
        set_error(null);
        try {
            const ts = await import('typescript');
            ts.transpileModule(orchestra.code, {
                compilerOptions: {
                    target: ts.ScriptTarget.ES2025,
                    module: ts.ModuleKind.ESNext,
                },
            });
            set_status('orchestra', 'ok');
        } catch (e) {
            set_error(`[orchestra] ${String(e)}`);
            set_status('orchestra', 'error');
        }
    }, [orchestra.code]);

    const play_impl = async () => {
        set_error(null);
        const my_session = ++play_session_ref.current;

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

        worklet_nodes_ref.current.forEach((n) => {
            n.port.postMessage({ type: 'stop' });
            n.port.postMessage({ type: 'clear' });
            if (capture_nodes_ref.current.has(n)) mic_source_ref.current?.disconnect(n);
            n.disconnect();
        });
        worklet_nodes_ref.current.clear();
        capture_nodes_ref.current.clear();
        if (global_node_ref.current) {
            global_node_ref.current.port.postMessage({ type: 'stop' });
            global_node_ref.current.port.postMessage({ type: 'clear' });
            global_node_ref.current.disconnect();
            global_node_ref.current = null;
        }
        if (midi_fwd_ref.current) {
            remove_on_midi_event(midi_fwd_ref.current);
            midi_fwd_ref.current = null;
        }
        if (!orchestra_worker_ref.current) {
            orchestra_worker_ref.current = new Worker(
                new URL('../audio/orchestra_worker.ts', import.meta.url),
                { type: 'module' },
            );
        } else {
            orchestra_worker_ref.current.postMessage({ type: 'stop' });
            orchestra_worker_ref.current.onmessage = null;
        }

        const context = audio_context_ref.current!;
        const merger = merger_ref.current!;
        await context.resume();

        const clock_sab = new SharedArrayBuffer(8);
        new BigInt64Array(clock_sab)[0] = BigInt(
            Math.round(context.currentTime * context.sampleRate),
        );

        const ts = await import('typescript');

        let compiled_orchestra = '';
        if (orchestra.code.trim()) {
            try {
                compiled_orchestra = ts.transpileModule(orchestra.code, {
                    compilerOptions: {
                        target: ts.ScriptTarget.ES2025,
                        module: ts.ModuleKind.ESNext,
                    },
                }).outputText;
            } catch (e) {
                set_error(`[orchestra] ${String(e)}`);
            }
        }

        let compiled_global_code = '';
        if (orchestra.global_patch_code.trim()) {
            try {
                compiled_global_code = ts.transpileModule(orchestra.global_patch_code, {
                    compilerOptions: {
                        target: ts.ScriptTarget.ES2025,
                        module: ts.ModuleKind.ESNext,
                    },
                }).outputText;
            } catch (e) {
                set_error(`[global] ${String(e)}`);
            }
        }

        const cached_global = patch_cache_ref.current.get(GLOBAL_CACHE_KEY);
        const global_json = cached_global
            ? cached_global.json
            : patch_to_json(orchestra.global_nodes, orchestra.global_edges);
        const global_compile_promise = cached_global
            ? Promise.resolve(cached_global.compiled)
            : WasmWasm.compile_patch(context.sampleRate, global_json);

        const instr_json = new Map<string, string>();
        const instr_compiled_code = new Map<string, string>();
        const instr_instance_counters = new Map<string, number>();

        const instr_compiled = new Map<string, CompiledPatch>();

        for (const instr of orchestra.instruments) {
            const cached_patch = patch_cache_ref.current.get(instr.id);
            let json: string;
            let compiled: CompiledPatch;
            if (cached_patch) {
                ({ json, compiled } = cached_patch);
            } else {
                json = patch_to_json(instr.nodes, instr.edges);
                try {
                    compiled = await WasmWasm.compile_patch(context.sampleRate, json);
                } catch (e) {
                    set_error(`[${instr.name}] ${String(e)}`);
                    continue;
                }
            }
            const compiled_code = instr.code.trim()
                ? ts.transpileModule(instr.code, {
                      compilerOptions: {
                          target: ts.ScriptTarget.ES2025,
                          module: ts.ModuleKind.ESNext,
                      },
                  }).outputText
                : '';
            instr_json.set(instr.name, json);
            instr_compiled.set(instr.name, compiled);
            instr_compiled_code.set(instr.name, compiled_code);
        }

        const global_pin_by_instrument = new Map<string, number>();
        let global_params: PatchParams | null = null;
        try {
            const compiled_global = await global_compile_promise;
            patch_cache_ref.current.set(GLOBAL_CACHE_KEY, {
                json: global_json,
                compiled: compiled_global,
            });

            compiled_global.external_inputs.forEach((ext, pin_index) => {
                const instr = orchestra.instruments.find((i) => ext.name === `in_${i.id}_in`);
                if (instr) global_pin_by_instrument.set(instr.id, pin_index);
            });

            const global_node = new AudioWorkletNode(context, 'wasm-processor', {
                numberOfInputs: compiled_global.external_inputs.length,
                numberOfOutputs: 1,
                outputChannelCount: [compiled_global.num_out_channels],
            });
            global_node.connect(merger);
            global_node.port.start();
            const global_ready = new Promise<void>((resolve) => {
                global_node.port.onmessage = (e: MessageEvent) => {
                    if (e.data.type === 'wasm-ready') resolve();
                };
            });
            global_node.port.postMessage({
                type: 'load-wasm',
                module: compiled_global.wasm_module,
                num_out_channels: compiled_global.num_out_channels,
                external_inputs: compiled_global.external_inputs,
                is_global: true,
            });
            global_params = WasmWasm.make_params(compiled_global);
            global_node.port.postMessage({
                type: 'load-params-sab',
                input_sab: global_params.input_sab,
                event_sab: global_params.event_sab,
                param_export_names: global_params.param_export_names,
                clock_sab,
            });
            await global_ready;
            global_node_ref.current = global_node;
        } catch (e) {
            set_error(`[global] ${String(e)}`);
        }

        const create_instance = async (
            instr: (typeof orchestra.instruments)[number],
            json: string,
            compiled: CompiledPatch,
            instance_idx: number,
            session: number,
            birth_time?: number,
        ): Promise<{
            event_sab: SharedArrayBuffer;
            param_names: string[];
        } | null> => {
            const params: PatchParams = WasmWasm.make_params(compiled);

            const node = new AudioWorkletNode(context, 'wasm-processor', {
                numberOfInputs: 1,
                numberOfOutputs: 1,
                outputChannelCount: [compiled.num_out_channels],
            });
            const pin_index = global_pin_by_instrument.get(instr.id);
            if (pin_index !== undefined && global_node_ref.current) {
                node.connect(global_node_ref.current, 0, pin_index);
            }
            worklet_nodes_ref.current.set(`${instr.id}:${instance_idx}`, node);

            if (needs_capture(json)) {
                try {
                    await setup_capture(context, node);
                    capture_nodes_ref.current.add(node);
                } catch (e) {
                    set_error(String(e));
                }
            }

            node.port.start();
            node.port.postMessage({
                type: 'load-wasm',
                module: compiled.wasm_module,
                num_out_channels: compiled.num_out_channels,
                external_inputs: compiled.external_inputs,
                is_global: false,
                start_frame: birth_time != null ? Math.round(birth_time * context.sampleRate) : 0,
            });
            node.port.postMessage({
                type: 'load-params-sab',
                input_sab: params.input_sab,
                event_sab: params.event_sab,
                param_export_names: params.param_export_names,
                clock_sab,
            });

            if (play_session_ref.current !== session) {
                node.port.postMessage({ type: 'stop' });
                node.port.postMessage({ type: 'clear' });
                if (capture_nodes_ref.current.delete(node)) {
                    mic_source_ref.current?.disconnect(node);
                }
                node.disconnect();
                worklet_nodes_ref.current.delete(`${instr.id}:${instance_idx}`);
                return null;
            }

            return {
                event_sab: params.event_sab,
                param_names: params.param_names,
            };
        };

        const worker = orchestra_worker_ref.current!;
        worker.onmessage = async (e) => {
            if (e.data.type === 'setup-midi') {
                await setup_midi();
                if (!midi_fwd_ref.current) {
                    midi_fwd_ref.current = add_on_midi_event(async (params) => {
                        worker.postMessage({ type: 'midi-event', params });
                    });
                }
                worker.postMessage({ type: 'midi-ready' });
                return;
            }
            if (e.data.type === 'error') {
                set_error(`[orchestra] ${e.data.message}`);
                return;
            }
            if (e.data.type === 'stop') {
                if (e.data.session === my_session) stop(e.data.dur ?? 1, e.data.time);
                return;
            }
            if (e.data.type === 'request-instance') {
                const { name, request_id, birth_time } = e.data as {
                    name: string;
                    request_id: number;
                    birth_time: number;
                };
                const json = instr_json.get(name);
                const compiled = instr_compiled.get(name);
                const instr = orchestra.instruments.find((i) => i.name === name);
                if (!json || !compiled || !instr) {
                    worker.postMessage({
                        type: 'instance-error',
                        request_id,
                        message: `Unknown instrument: ${name}`,
                    });
                    return;
                }
                const instance_idx = instr_instance_counters.get(name) ?? 0;
                instr_instance_counters.set(name, instance_idx + 1);
                const result = await create_instance(
                    instr,
                    json,
                    compiled,
                    instance_idx,
                    my_session,
                    birth_time,
                );
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
                    instance_id: `${instr.id}:${instance_idx}`,
                    event_sab: result.event_sab,
                    param_names: result.param_names,
                    code: instr_compiled_code.get(name) ?? '',
                });
                return;
            }
            if (e.data.type === 'destroy-instance') {
                const node = worklet_nodes_ref.current.get(e.data.instance_id);
                if (node) {
                    const stop_frame = Math.round(e.data.time * context.sampleRate);
                    node.port.postMessage({ type: 'stop', frame: stop_frame });
                    const delay = Math.max(0, (e.data.time - context.currentTime) * 1000);
                    setTimeout(() => {
                        node.port.postMessage({ type: 'clear' });
                        if (capture_nodes_ref.current.delete(node)) {
                            mic_source_ref.current?.disconnect(node);
                        }
                        node.disconnect();
                        worklet_nodes_ref.current.delete(e.data.instance_id);
                    }, delay);
                }
                return;
            }
        };
        worker.postMessage({
            type: 'run',
            session: my_session,
            orchestra_code: compiled_orchestra,
            bpm: orchestra.bpm,
            sampleRate: context.sampleRate,
            audioCurrentTime: context.currentTime,
            instrument_names: [...instr_json.keys()],
            global_code: compiled_global_code,
            global_param_names: global_params?.param_names,
            global_event_sab: global_params?.event_sab,
            clock_sab,
        });
        set_is_playing(true);
    };

    const play = async () => {
        if (play_inflight_ref.current) return play_inflight_ref.current;
        const run = play_impl().finally(() => {
            play_inflight_ref.current = null;
        });
        play_inflight_ref.current = run;
        return run;
    };

    const stop = async (dur: number = 1, time?: number) => {
        const context = audio_context_ref.current;
        const merger = merger_ref.current;
        const stop_frame = context
            ? Math.round(((time ?? context.currentTime) + dur) * context.sampleRate)
            : null;

        worklet_nodes_ref.current.forEach((n) => {
            n.port.postMessage({ type: 'stop', frame: stop_frame });
        });
        if (global_node_ref.current) {
            global_node_ref.current.port.postMessage({ type: 'stop', frame: stop_frame });
        }

        if (context) {
            const start = time ?? context.currentTime;
            if (merger && dur > 0) {
                merger.gain.cancelScheduledValues(start);
                merger.gain.setValueAtTime(merger.gain.value, start);
                merger.gain.linearRampToValueAtTime(0, start + dur);
            }
            const wait = Math.max(0, (start + dur - context.currentTime) * 1000);
            await new Promise<void>((r) => setTimeout(r, wait));
        }
        if (is_recording_ref.current) {
            await stop_recording();
        }
        orchestra_worker_ref.current?.postMessage({ type: 'stop' });
        if (midi_fwd_ref.current) {
            remove_on_midi_event(midi_fwd_ref.current);
            midi_fwd_ref.current = null;
        }
        if (mic_source_ref.current) {
            mic_source_ref.current.mediaStream.getTracks().forEach((t) => t.stop());
            mic_source_ref.current.disconnect();
            mic_source_ref.current = null;
        }
        worklet_nodes_ref.current.forEach((n) => {
            n.port.postMessage({ type: 'clear' });
            n.disconnect();
        });
        worklet_nodes_ref.current.clear();
        capture_nodes_ref.current.clear();
        if (global_node_ref.current) {
            global_node_ref.current.port.postMessage({ type: 'clear' });
            global_node_ref.current.disconnect();
            global_node_ref.current = null;
        }
        await context?.suspend();
        merger?.gain.setValueAtTime(1, context?.currentTime ?? 0);
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
                <button onClick={record}>{is_recording ? 'Recording…' : 'Record'}</button>
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
                    on_global_patch_code_change={handle_global_patch_code_change}
                    global_env={instrument_envs.get(GLOBAL_CACHE_KEY) ?? null}
                    view={view}
                    on_view_change={set_view}
                />

                <div className="app__patch-pane">
                    <div className="app__patch-container">
                        <div className="app__patch-tabs">
                            <div
                                className={`app__patch-tab${view === 'global' ? ' app__patch-tab--active' : ''}`}
                                onClick={() => set_view('global')}
                            >
                                global
                            </div>
                            <div
                                className={`app__patch-tab${view === 'instrument' ? ' app__patch-tab--active' : ''}`}
                                onClick={() => set_view('instrument')}
                            >
                                {active_instrument?.name ?? 'instrument'}
                            </div>
                        </div>
                        <ReactFlowProvider>
                            <PatchEditor store={store} />
                        </ReactFlowProvider>
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
                        key={selected_block.id}
                        initial_value={(selected_block.data as { code: string }).code}
                        on_change={(code) => update_code(selected_block.id, code)}
                    />
                </div>
            )}
        </div>
    );
}
