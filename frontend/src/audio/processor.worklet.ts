import { Conductor } from './conductor';
import type {
    GlobalCallbackHandler,
    GlobalExports,
    InstrumentCallbackHandler,
    InstrumentCallbackMap,
    InstrumentExportsMap,
    ParamIndex,
    ScoreGraph,
} from './conductor';

const inv_sample_rate = 1 / sampleRate;

function build_scale_prelude(scales: ScoreGraph['scales']): string {
    const functions = scales
        .map(({ name, values }) => {
            const n = values.length;
            return `function ${name}(degree, octave) {
                const n = ${n};
                const i = ((degree % n) + n) % n;
                return ${JSON.stringify(values)}[i] * Math.pow(2, Math.floor(degree / n)) * Math.pow(2, octave);
            }`;
        })
        .join('\n');
    const index = `const scales = [${scales.map(({ name }) => name).join(', ')}];`;
    return `${functions}\n${index}`;
}

interface WasmExports extends WebAssembly.Exports {
    main: (...args: number[]) => void;
    [key: string]: WebAssembly.ExportValue;
}

interface WasmGlobal {
    value: number;
}

class WasmProcessor extends AudioWorkletProcessor {
    main: ((...args: number[]) => void) | undefined = undefined;
    instrument_exports: InstrumentExportsMap = {};
    global_exports: GlobalExports | null = null;
    conductor: Conductor | null = null;
    heap: Float32Array | null = null;
    stopped = false;

    _ready_sent = false;
    instance: WebAssembly.Instance | null = null;
    input_buf: Float64Array | null = null;
    param_export_names: string[] = [];
    param_exports: (WasmGlobal | null)[] = [];

    node_id = '';
    _busy_time = 0;
    _available_time = 0;
    _report_window_ms = 500;

    constructor() {
        super();

        this.port.onmessage = async (event: MessageEvent) => {
            if (event.data.type === 'stop') {
                this.conductor?.destroy_all();
                this.stopped = true;
                return;
            }

            if (event.data.type === 'clear') {
                this.conductor?.destroy_all();
                this.conductor = null;
                this.heap = null;
                this.instance = null;
                this.input_buf = null;
                this.param_exports = [];
            }

            if (event.data.type === 'set-bpm') {
                this.conductor?.set_bpm(event.data.bpm as number);
                return;
            }

            if (event.data.type === 'load-wasm') {
                const wasm_page_bytes = 65536;
                const memory_bytes = (event.data.memory_bytes as number) ?? 0;
                const pages = Math.max(128, Math.ceil(memory_bytes / wasm_page_bytes));
                const memory = new WebAssembly.Memory({ initial: pages, maximum: pages });

                const instance = await WebAssembly.instantiate(
                    event.data.module as WebAssembly.Module,
                    { env: { memory } },
                );
                const exports = instance.exports as WasmExports;
                this.heap = new Float32Array(memory.buffer);
                this.instance = instance;
                this.main = exports.main;
                this.instrument_exports = {};
                this.global_exports = null;
                for (const key of Object.keys(exports)) {
                    const sep = key.indexOf('$');
                    if (sep === -1) continue;
                    const instrument_id = key.substring(0, sep);
                    const suffix = key.substring(sep + 1);

                    if (instrument_id === 'global') {
                        if (suffix !== 'set_param') continue;
                        this.global_exports = { set_param: exports[key] as never };
                        continue;
                    }

                    if (suffix !== 'instantiate' && suffix !== 'set_param') continue;
                    this.instrument_exports[instrument_id] ??= {
                        instantiate: () => -1,
                        set_param: () => -1,
                    };
                    this.instrument_exports[instrument_id][suffix] = exports[key] as never;
                }

                if (event.data.score_graph) {
                    const scale_prelude = build_scale_prelude(
                        (event.data.score_graph as ScoreGraph).scales ?? [],
                    );

                    const instrument_callbacks_source =
                        (event.data.instrument_callbacks as Record<string, string>) ?? {};
                    const instrument_callbacks: InstrumentCallbackMap = {};
                    for (const [instrument_id, js_source] of Object.entries(
                        instrument_callbacks_source,
                    )) {
                        instrument_callbacks[instrument_id] = new Function(
                            `${scale_prelude}\nreturn (\n${js_source}\n);`,
                        )() as new () => InstrumentCallbackHandler;
                    }

                    const global_callback_source = event.data.global_callback as string | undefined;
                    const global_callback = global_callback_source
                        ? (new Function(
                              `${scale_prelude}\nreturn (\n${global_callback_source}\n);`,
                          )() as new () => GlobalCallbackHandler)
                        : null;

                    this.conductor = new Conductor(
                        event.data.score_graph as ScoreGraph,
                        (event.data.param_index as ParamIndex) ?? {},
                        this.instrument_exports,
                        sampleRate,
                        event.data.bpm as number,
                        this.global_exports,
                        instrument_callbacks,
                        global_callback,
                    );
                }

                this.node_id = event.data.node_id ?? '';
                this._ready_sent = false;
                this._busy_time = 0;
                this._available_time = 0;
                if (this.param_export_names.length > 0) {
                    this.param_exports = this.param_export_names.map(
                        (n) => (exports[n] as WasmGlobal) ?? null,
                    );
                    if (this.input_buf) {
                        for (let i = 0; i < this.param_exports.length; i++) {
                            const g = this.param_exports[i];
                            if (g) g.value = this.input_buf[i];
                        }
                    }
                }
                this.port.postMessage({ type: 'wasm-ready' });
            }
        };
    }

    process(inputs: Float32Array[][], outputs: Float32Array[][]): boolean {
        const process_start = Date.now();

        if (this.stopped) return false;
        if (!this.heap) return true;

        const num_samples = outputs[0][0].length;
        const heap = this.heap;
        const num_in_channels = 2;
        const num_out_channels = 2;

        const in_base_floats = num_samples * num_out_channels;
        for (let ch = 0; ch < num_in_channels; ch++) {
            const dst = in_base_floats + ch * num_samples;
            const src = inputs[0][ch];
            if (src) heap.set(src.length === num_samples ? src : src.subarray(0, num_samples), dst);
        }

        if (this.conductor) {
            try {
                this.conductor.tick(num_samples);
            } catch (e) {
                this.port.postMessage({ type: 'conductor-error', message: String(e) });
                this.stopped = true;
                return false;
            }
        }

        this.main?.(num_samples, in_base_floats * 4, 0);

        for (let ch = 0; ch < num_out_channels; ch++) {
            const out_ch = outputs[0][ch];
            if (out_ch) {
                const start = ch * num_samples;
                out_ch.set(heap.subarray(start, start + num_samples));
            }
        }

        this._busy_time += Date.now() - process_start;
        this._available_time += num_samples * inv_sample_rate * 1000;
        if (this._available_time >= this._report_window_ms) {
            this.port.postMessage({
                type: 'cpu-metrics',
                node_id: this.node_id,
                load: this._busy_time / this._available_time,
            });
            this._busy_time = 0;
            this._available_time = 0;
        }

        return true;
    }
}

registerProcessor('wasm-processor', WasmProcessor);

export default null;
