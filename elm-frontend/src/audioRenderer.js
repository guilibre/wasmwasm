import WasmWasm from './wasmwasm/compiler';
import ScoreWasm from './scorewasm/compiler';
import workletUrl from './audio/processor.worklet.ts?worker&url';

function orchestra_to_json(instrument_ids) {
    const instruments = {};
    for (const id of instrument_ids) {
        instruments[id] = { modules: {}, patch: {} };
    }
    return JSON.stringify({ instruments, global: { modules: {}, patch: {} } });
}

function patch_uses_adc(patch_json) {
    const { global } = JSON.parse(patch_json);
    return Object.values(global.patch).some((src) => src === 'adc_l' || src === 'adc_r');
}

export function createAudioEngine({ onCpu, onPlaying, onError, onAnalysers }) {
    let audio_context = null;
    let merger = null;
    let global_node = null;
    let mic_stream = null;
    let mic_source = null;
    let analyser_l = null;
    let analyser_r = null;
    let play_inflight = null;
    let node_loads = new Map();

    const set_playing = (v) => onPlaying?.(v);

    const attach_cpu_metrics = (node, node_id) => {
        const existing_onmessage = node.port.onmessage;
        node.port.onmessage = (event) => {
            if (event.data.type === 'cpu-metrics') {
                node_loads.set(node_id, event.data.load);
                let total = 0;
                node_loads.forEach((v) => (total += v));
                onCpu?.(total);
                return;
            }
            if (event.data.type === 'conductor-error') {
                onError?.(String(event.data.message));
                set_playing(false);
                return;
            }
            if (existing_onmessage) {
                existing_onmessage.call(node.port, event);
            }
        };
    };

    const play_impl = async ({ bpm, instruments, scoreSource }) => {
        onError?.(null);

        if (!audio_context) {
            const context = new AudioContext({ latencyHint: 'interactive' });
            await context.audioWorklet.addModule(workletUrl);

            const m = context.createGain();
            const splitter = context.createChannelSplitter(2);
            const al = context.createAnalyser();
            const ar = context.createAnalyser();
            al.fftSize = 4096;
            ar.fftSize = 4096;
            al.smoothingTimeConstant = 0.75;
            ar.smoothingTimeConstant = 0.75;
            m.connect(context.destination);
            m.connect(splitter);
            splitter.connect(al, 0);
            splitter.connect(ar, 1);

            merger = m;
            analyser_l = al;
            analyser_r = ar;
            audio_context = context;
        }

        onAnalysers?.({ l: analyser_l, r: analyser_r });
        onCpu?.(0);
        node_loads = new Map();

        const context = audio_context;
        await context.resume();

        const patch_json = orchestra_to_json(instruments);

        try {
            const compiled = await WasmWasm.compile_patch(context.sampleRate, patch_json);
            const param_index = await WasmWasm.get_param_index(patch_json);
            const score_graph = await ScoreWasm.compile_score(scoreSource);

            const node = new AudioWorkletNode(context, 'wasm-processor', {
                numberOfInputs: 1,
                numberOfOutputs: 1,
                outputChannelCount: [2],
            });
            node.connect(merger);
            node.port.start();

            if (patch_uses_adc(patch_json)) {
                try {
                    const stream = await navigator.mediaDevices.getUserMedia({
                        audio: {
                            echoCancellation: false,
                            noiseSuppression: false,
                            autoGainControl: false,
                        },
                    });
                    mic_stream = stream;
                    const src = context.createMediaStreamSource(stream);
                    src.connect(node);
                    mic_source = src;
                } catch (e) {
                    console.error('microphone access failed', e);
                }
            }

            const ready = new Promise((resolve) => {
                node.port.onmessage = (e) => {
                    if (e.data.type === 'wasm-ready') resolve();
                };
            });
            node.port.postMessage({
                type: 'load-wasm',
                module: compiled.bytes,
                memory_bytes: compiled.memory_bytes,
                num_out_channels: 2,
                is_global: true,
                node_id: 'global',
                score_graph,
                param_index,
                instrument_callbacks: {},
                bpm,
            });
            await ready;
            attach_cpu_metrics(node, 'global');
            global_node = node;
        } catch (e) {
            onError?.(String(e));
        }

        set_playing(true);
    };

    const play = (payload) => {
        if (play_inflight) return play_inflight;
        const run = play_impl(payload).finally(() => {
            play_inflight = null;
        });
        play_inflight = run;
        return run;
    };

    const stop = async () => {
        const context = audio_context;

        if (global_node) global_node.port.postMessage({ type: 'stop' });

        if (context && merger) {
            // stop(0) do React: sem fade
            merger.gain.cancelScheduledValues(context.currentTime);
            merger.gain.setValueAtTime(merger.gain.value, context.currentTime);
        }
        global_node?.disconnect();
        node_loads = new Map();
        onCpu?.(0);
        if (global_node) {
            global_node.port.postMessage({ type: 'clear' });
            global_node.disconnect();
            global_node = null;
        }
        mic_source?.disconnect();
        mic_source = null;
        mic_stream?.getTracks().forEach((t) => t.stop());
        mic_stream = null;
        await context?.suspend();
        merger?.gain.setValueAtTime(1, context?.currentTime ?? 0);
        onAnalysers?.(null);
        set_playing(false);
    };

    const setBpm = (bpm) => {
        global_node?.port.postMessage({ type: 'set-bpm', bpm });
    };

    return { play, stop, setBpm };
}
