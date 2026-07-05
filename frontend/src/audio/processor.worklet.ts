const inv_sample_rate = 1 / sampleRate;

interface WasmExports extends WebAssembly.Exports {
    init(): void;
    main: (...args: number[]) => void;
    [key: string]: WebAssembly.ExportValue;
}

interface WasmGlobal {
    value: number;
}

class WasmProcessor extends AudioWorkletProcessor {
    main: ((...args: number[]) => void) | undefined = undefined;
    instantiate: Map<string, (idx: number) => number> = new Map();
    heap: Float32Array | null = null;
    stopped = false;
    is_first_time = true;

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
                this.stopped = true;
                return;
            }

            if (event.data.type === 'clear') {
                this.heap = null;
                this.instance = null;
                this.input_buf = null;
                this.param_exports = [];
            }

            if (event.data.type === 'load-wasm') {
                const memory = new WebAssembly.Memory({ initial: 128, maximum: 128 });

                const instance = await WebAssembly.instantiate(
                    event.data.module as WebAssembly.Module,
                    { env: { memory } },
                );
                const exports = instance.exports as WasmExports;
                this.heap = new Float32Array(memory.buffer);
                this.instance = instance;
                exports.init();
                this.main = exports.main;
                Object.keys(exports)
                    .filter((k) => k.includes('instantiate'))
                    .forEach((k) =>
                        this.instantiate.set(
                            k.substring(0, k.indexOf('$')),
                            exports[k] as (idx: number) => number,
                        ),
                    );
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
        if (this.stopped) return false;
        if (!this.heap) return true;
        if (this.is_first_time) {
            this.instantiate.forEach((f) => console.log(f(0)));
            this.is_first_time = false;
        }

        const process_start = Date.now();
        const num_samples = outputs[0][0].length;
        const heap = this.heap;
        const num_in_channels = 2;
        const num_out_channels = 2;

        const in_base_floats = num_samples * num_out_channels;
        for (let ch = 0; ch < num_in_channels; ch++) {
            const dst = in_base_floats + ch * num_samples;
            const src = inputs[0][ch];
            if (src) heap.set(src.subarray(0, num_samples), dst);
        }

        this.main?.(num_samples, in_base_floats * 4, 0);

        for (let ch = 0; ch < num_out_channels; ch++) {
            const out_ch = outputs[0][ch];
            if (out_ch) out_ch.set(heap.subarray(ch * num_samples, (ch + 1) * num_samples));
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
