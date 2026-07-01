const EVENT_CAPACITY = 256;
const inv_sample_rate = 1 / sampleRate;

interface WasmExports extends WebAssembly.Exports {
    init(): void;
    main: ((...args: number[]) => void) & { length: number };
    [key: string]: WebAssembly.ExportValue;
}

interface WasmGlobal {
    value: number;
}

interface ExternalInput {
    name: string;
    channels: number;
}

class WasmProcessor extends AudioWorkletProcessor {
    main: (...args: number[]) => void = () => {};
    heap: Float32Array | null = null;
    num_out_channels = 2;
    external_inputs: ExternalInput[] = [];
    has_ext = false;
    is_global = false;
    stopped = false;
    start_frame = 0;
    stop_frame = Infinity;

    _ready_sent = false;
    instance: WebAssembly.Instance | null = null;
    input_buf: Float64Array | null = null;
    param_export_names: string[] = [];
    param_exports: (WasmGlobal | null)[] = [];
    ev_write_head: Int32Array | null = null;
    ev_read_head: Int32Array | null = null;
    ev_data: DataView | null = null;

    constructor() {
        super();

        this.port.onmessage = async (event: MessageEvent) => {
            if (event.data.type === 'stop') {
                this.stop_frame = event.data.frame ?? 0;
                this.stopped = true;
                return;
            }

            if (event.data.type === 'clear') {
                if (this.ev_read_head && this.ev_write_head) {
                    Atomics.store(this.ev_read_head, 0, Atomics.load(this.ev_write_head, 0));
                }
                this.heap = null;
                this.instance = null;
                this.input_buf = null;
                this.param_exports = [];
                this.ev_write_head = null;
                this.ev_read_head = null;
                this.ev_data = null;
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
                this.has_ext = this.main.length === 4;
                this.num_out_channels = event.data.num_out_channels ?? 2;
                this.external_inputs = event.data.external_inputs ?? [];
                this.is_global = event.data.is_global ?? false;
                this.start_frame = event.data.start_frame ?? 0;
                this._ready_sent = false;
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

            if (event.data.type === 'load-params-sab') {
                this.input_buf = new Float64Array(event.data.input_sab);
                this.ev_write_head = new Int32Array(event.data.event_sab, 0, 1);
                this.ev_read_head = new Int32Array(event.data.event_sab, 4, 1);
                this.ev_data = new DataView(event.data.event_sab, 8);
                this.param_export_names = event.data.param_export_names;
                if (this.instance) {
                    const exports = this.instance.exports as WasmExports;
                    this.param_exports = this.param_export_names.map(
                        (n) => (exports[n] as WasmGlobal) ?? null,
                    );
                    for (let i = 0; i < this.param_exports.length; i++) {
                        const g = this.param_exports[i];
                        if (g) g.value = this.input_buf![i];
                    }
                }
            }
        };
    }

    consume_events(frame: number): void {
        if (!this.ev_data) return;
        const wh = Atomics.load(this.ev_write_head!, 0);
        let rh = Atomics.load(this.ev_read_head!, 0);
        if (rh === wh) return;
        const exports = this.param_exports;
        const buf = this.input_buf!;
        while (rh !== wh) {
            const slot = (rh % EVENT_CAPACITY) * 16;
            const ev_frame = this.ev_data.getInt32(slot, true);
            if (ev_frame > frame) break;
            const idx = this.ev_data.getInt32(slot + 4, true);
            const value = this.ev_data.getFloat64(slot + 8, true);
            const g = exports[idx];
            if (g) {
                g.value = value;
                buf[idx] = value;
            }
            rh = (rh + 1) >>> 0;
        }
        Atomics.store(this.ev_read_head!, 0, rh);
    }

    process(inputs: Float32Array[][], outputs: Float32Array[][]): boolean {
        if (!this.heap) return !this.stopped;

        const num_samples = outputs[0][0].length;
        if (currentFrame + num_samples <= this.start_frame) return true;
        if (currentFrame >= this.stop_frame) return false;

        const begin = Math.max(0, this.start_frame - currentFrame);
        const end = Math.min(num_samples, this.stop_frame - currentFrame);
        const n_run = end - begin;

        this.consume_events(currentFrame);

        const heap = this.heap;

        if (this.has_ext) {
            const ext_base_floats = n_run * this.num_out_channels;
            let channel_offset = 0;
            for (let group = 0; group < this.external_inputs.length; group++) {
                const { channels } = this.external_inputs[group];
                const pin = this.is_global ? inputs[group] : inputs[0];
                for (let ch = 0; ch < channels; ch++) {
                    const dst = ext_base_floats + (channel_offset + ch) * n_run;
                    const src = pin?.[ch];
                    if (src) heap.set(src.subarray(begin, end), dst);
                    else heap.fill(0, dst, dst + n_run);
                }
                channel_offset += channels;
            }
            this.main(0, ext_base_floats * 4, n_run, this.num_out_channels);
        } else {
            this.main(0, n_run, this.num_out_channels);
        }

        if (!this._ready_sent && this.ev_data) {
            this._ready_sent = true;
            this.port.postMessage({ type: 'ready', startTime: currentFrame * inv_sample_rate });
        }

        for (let ch = 0; ch < this.num_out_channels; ch++) {
            const out_ch = outputs[0][ch];
            if (out_ch) {
                if (begin > 0) out_ch.fill(0, 0, begin);
                out_ch.set(heap.subarray(ch * n_run, (ch + 1) * n_run), begin);
                if (end < num_samples) out_ch.fill(0, end);
            }
        }

        return currentFrame + num_samples < this.stop_frame;
    }
}

registerProcessor('wasm-processor', WasmProcessor);

export default null;
