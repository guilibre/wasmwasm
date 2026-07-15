import { useEffect, useRef, useState } from 'react';
import ts from 'typescript';
import workletUrl from '../audio/processor.worklet.ts?worker&url';
import WasmWasm from '../audio/compiler';
import ScoreWasm from '../scorewasm/compiler';
import { orchestra_to_json } from '../patch/orchestra_to_json';
import type { OrchestraState, ScoreParamBindings } from '../patch/store/patch_types';

function transpile_callback_source(source: string): string | null {
    if (!source.trim()) return null;
    const { outputText } = ts.transpileModule(source, {
        compilerOptions: { target: ts.ScriptTarget.ES2020, module: ts.ModuleKind.None },
    });
    return outputText.replace(/^"use strict";\n/, '').replace(/;\s*$/, '');
}

function transpile_instrument_callbacks(
    score_param_bindings: ScoreParamBindings,
): Record<string, string> {
    const instrument_callbacks: Record<string, string> = {};
    for (const [instrument_id, source] of Object.entries(score_param_bindings)) {
        const transpiled = transpile_callback_source(source);
        if (transpiled) instrument_callbacks[instrument_id] = transpiled;
    }
    return instrument_callbacks;
}

export function useAudioEngine(
    orchestra: OrchestraState,
    score_source: string,
    score_param_bindings: ScoreParamBindings,
    global_callback_source: string,
    set_error: (error: string | null) => void,
) {
    const audio_context_ref = useRef<AudioContext | null>(null);
    const merger_ref = useRef<GainNode | null>(null);
    const global_node_ref = useRef<AudioWorkletNode | null>(null);
    const analyser_l_ref = useRef<AnalyserNode | null>(null);
    const analyser_r_ref = useRef<AnalyserNode | null>(null);
    const play_inflight_ref = useRef<Promise<void> | null>(null);
    const [analysers, set_analysers] = useState<{ l: AnalyserNode; r: AnalyserNode } | null>(null);
    const [is_playing, set_is_playing] = useState(false);
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
            if (event.data.type === 'conductor-error') {
                set_error(String(event.data.message));
                set_is_playing(false);
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
        return () => {
            global_node_ref.current?.disconnect();
            audio_context_ref.current?.close();
        };
    }, []);

    useEffect(() => {
        global_node_ref.current?.port.postMessage({ type: 'set-bpm', bpm: orchestra.bpm });
    }, [orchestra.bpm]);

    const play_impl = async () => {
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
        set_cpu_load(0);

        const context = audio_context_ref.current!;
        const merger = merger_ref.current!;
        await context.resume();

        const patch_json = orchestra_to_json(orchestra);

        try {
            const compiled = await WasmWasm.compile_patch(context.sampleRate, patch_json);
            const param_index = await WasmWasm.get_param_index(patch_json);
            const score_graph = await ScoreWasm.compile_score(score_source);
            const instrument_callbacks = transpile_instrument_callbacks(score_param_bindings);
            const global_callback = transpile_callback_source(global_callback_source);
            if (global_callback) instrument_callbacks['global'] = global_callback;

            const global_node = new AudioWorkletNode(context, 'wasm-processor', {
                numberOfOutputs: 1,
                outputChannelCount: [2],
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
                module: compiled.wasm_module,
                memory_bytes: compiled.memory_bytes,
                num_out_channels: 2,
                is_global: true,
                node_id: 'global',
                score_graph,
                param_index,
                instrument_callbacks,
                bpm: orchestra.bpm,
            });
            await global_ready;
            attach_cpu_metrics(global_node, 'global');
            global_node_ref.current = global_node;
        } catch (e) {
            set_error(String(e));
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

        if (global_node_ref.current) global_node_ref.current.port.postMessage({ type: 'stop' });

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
        global_node_ref.current?.disconnect();
        node_loads_ref.current.clear();
        set_cpu_load(0);
        if (global_node_ref.current) {
            global_node_ref.current.port.postMessage({ type: 'clear' });
            global_node_ref.current.disconnect();
            global_node_ref.current = null;
        }
        await context?.suspend();
        merger?.gain.setValueAtTime(1, context?.currentTime ?? 0);
        set_analysers(null);
        set_is_playing(false);
    };

    return {
        analysers,
        is_playing,
        cpu_load,
        play,
        stop,
    };
}
