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

class WasmProcessor extends AudioWorkletProcessor {
    main: (...args: number[]) => void = () => {};
    heap: Float32Array | null = null;
    has_capture = false;
    stopped = false;

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
                const memory = new WebAssembly.Memory({ initial: 64, maximum: 64 });

                const instance = await WebAssembly.instantiate(
                    event.data.module as WebAssembly.Module,
                    { env: { memory } },
                );
                const exports = instance.exports as WasmExports;
                this.heap = new Float32Array(memory.buffer);
                this.instance = instance;
                exports.init();
                this.main = exports.main;
                this.has_capture = this.main.length === 4;
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
        if (this.stopped) return false;
        if (!this.heap) return true;

        this.consume_events(currentFrame);

        const heap = this.heap;
        const num_samples = outputs[0][0].length;
        const out_l = outputs[0][0];
        const out_r = outputs[0][1];

        if (this.has_capture) {
            const in_l = inputs[0][0];
            const in_r = inputs[0][1];
            const in_f32_offset = num_samples * 2;
            if (in_l) heap.set(in_l, in_f32_offset);
            else heap.fill(0, in_f32_offset, in_f32_offset + num_samples);
            if (in_r) heap.set(in_r, in_f32_offset + num_samples);
            else heap.fill(0, in_f32_offset + num_samples, in_f32_offset + num_samples * 2);
            this.main(0, num_samples * 8, num_samples, 2);
        } else {
            this.main(0, num_samples, 2);
        }

        if (!this._ready_sent && this.ev_data) {
            this._ready_sent = true;
            this.port.postMessage({ type: 'ready', startTime: currentFrame * inv_sample_rate });
        }

        out_l.set(heap.subarray(0, num_samples));
        if (out_r) out_r.set(heap.subarray(num_samples, num_samples * 2));

        return true;
    }
}

registerProcessor('wasm-processor', WasmProcessor);

export default null;
