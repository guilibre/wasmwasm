import { useEffect, useRef, useState } from 'react';
import workletUrl from '../audio/processor.worklet.ts?worker&url';
import WasmWasm, { type PatchParams, type CompiledPatch } from '../audio/compiler';
import { patch_to_json } from '../patch/patch_to_json';
import type { OrchestraState } from '../patch/use_patch_store';
import { GLOBAL_CACHE_KEY } from './constants';
import { encode_wav, download_blob } from './wav_encoder';

export function useAudioEngine(
    orchestra: OrchestraState,
    patch_cache_ref: { current: Map<string, { json: string; compiled: CompiledPatch }> },
    set_error: (error: string | null) => void,
) {
    const audio_context_ref = useRef<AudioContext | null>(null);
    const merger_ref = useRef<GainNode | null>(null);
    const global_node_ref = useRef<AudioWorkletNode | null>(null);
    const worklet_nodes_ref = useRef<Map<string, AudioWorkletNode>>(new Map());
    const capture_nodes_ref = useRef<Set<AudioWorkletNode>>(new Set());
    const mic_source_ref = useRef<MediaStreamAudioSourceNode | null>(null);
    const analyser_l_ref = useRef<AnalyserNode | null>(null);
    const analyser_r_ref = useRef<AnalyserNode | null>(null);
    const play_session_ref = useRef(0);
    const play_inflight_ref = useRef<Promise<void> | null>(null);
    const recorder_node_ref = useRef<AudioWorkletNode | null>(null);
    const recorded_buffers_ref = useRef<Float32Array[]>([]);
    const recorded_length_ref = useRef(0);
    const recorded_channels_ref = useRef(2);

    const [analysers, set_analysers] = useState<{ l: AnalyserNode; r: AnalyserNode } | null>(null);
    const [is_playing, set_is_playing] = useState(false);
    const [is_recording, set_is_recording] = useState(false);
    const is_recording_ref = useRef(false);
    const node_loads_ref = useRef<Map<string, number>>(new Map());
    const [cpu_load, set_cpu_load] = useState(0);

    const attach_cpu_metrics = (node: AudioWorkletNode, node_id: string) => {
        const existing_onmessage = node.port.onmessage;
        node.port.onmessage = (event: MessageEvent) => {
            if (event.data.type === 'cpu-metrics') {
                node_loads_ref.current.set(node_id, event.data.load);
                let total = 0;
                node_loads_ref.current.forEach((v) => (total += v));
                set_cpu_load(total);
                return;
            }
            if (existing_onmessage) {
                (existing_onmessage as (this: MessagePort, ev: MessageEvent) => void).call(
                    node.port,
                    event,
                );
            }
        };
    };

    useEffect(() => {
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
            nodes_ref.current.clear();
            capture_ref.current.clear();
            ctx_ref.current?.close();
            mic_ref.current?.mediaStream.getTracks().forEach((t) => t.stop());
        };
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

    const clear_recording_data = () => {
        recorded_buffers_ref.current.length = 0;
        recorded_length_ref.current = 0;
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
        node_loads_ref.current.clear();
        set_cpu_load(0);
        if (global_node_ref.current) {
            global_node_ref.current.port.postMessage({ type: 'stop' });
            global_node_ref.current.port.postMessage({ type: 'clear' });
            global_node_ref.current.disconnect();
            global_node_ref.current = null;
        }

        const context = audio_context_ref.current!;
        const merger = merger_ref.current!;
        await context.resume();

        const clock_sab = new SharedArrayBuffer(8);
        new BigInt64Array(clock_sab)[0] = BigInt(
            Math.round(context.currentTime * context.sampleRate),
        );

        const cached_global = patch_cache_ref.current.get(GLOBAL_CACHE_KEY);
        const global_json = cached_global
            ? cached_global.json
            : patch_to_json(orchestra.global_nodes, orchestra.global_edges);
        const global_compile_promise = cached_global
            ? Promise.resolve(cached_global.compiled)
            : WasmWasm.compile_patch(context.sampleRate, global_json);

        const instr_json = new Map<string, string>();
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
            instr_json.set(instr.id, json);
            instr_compiled.set(instr.id, compiled);
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
                node_id: 'global',
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
            attach_cpu_metrics(global_node, 'global');
            global_node_ref.current = global_node;
        } catch (e) {
            set_error(`[global] ${String(e)}`);
        }

        const create_instance = async (
            instr: (typeof orchestra.instruments)[number],
            json: string,
            compiled: CompiledPatch,
        ): Promise<void> => {
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
            const node_id = instr.id;
            worklet_nodes_ref.current.set(node_id, node);
            attach_cpu_metrics(node, node_id);

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
                start_frame: 0,
                node_id,
            });
            node.port.postMessage({
                type: 'load-params-sab',
                input_sab: params.input_sab,
                event_sab: params.event_sab,
                param_export_names: params.param_export_names,
                clock_sab,
            });

            if (play_session_ref.current !== my_session) {
                node.port.postMessage({ type: 'stop' });
                node.port.postMessage({ type: 'clear' });
                if (capture_nodes_ref.current.delete(node)) {
                    mic_source_ref.current?.disconnect(node);
                }
                node.disconnect();
                worklet_nodes_ref.current.delete(node_id);
            }
        };

        for (const instr of orchestra.instruments) {
            const json = instr_json.get(instr.id);
            const compiled = instr_compiled.get(instr.id);
            if (!json || !compiled) continue;
            await create_instance(instr, json, compiled);
        }

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
        node_loads_ref.current.clear();
        set_cpu_load(0);
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

    const get_sample_rate = () => audio_context_ref.current?.sampleRate ?? 44100;

    return {
        analysers,
        is_playing,
        is_recording,
        cpu_load,
        play,
        stop,
        record,
        get_sample_rate,
    };
}
