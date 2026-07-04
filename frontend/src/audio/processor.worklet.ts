const EVENT_CAPACITY = 1024;
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
    clock_view: BigInt64Array | null = null;

    _ready_sent = false;
    instance: WebAssembly.Instance | null = null;
    input_buf: Float64Array | null = null;
    param_export_names: string[] = [];
    param_exports: (WasmGlobal | null)[] = [];
    ev_write_head: Int32Array | null = null;
    ev_read_head: Int32Array | null = null;
    ev_data: DataView | null = null;

    node_id = '';
    _busy_time = 0;
    _available_time = 0;
    _report_window_ms = 500;

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

            if (event.data.type === 'load-params-sab') {
                this.input_buf = new Float64Array(event.data.input_sab);
                this.ev_write_head = new Int32Array(event.data.event_sab, 0, 1);
                this.ev_read_head = new Int32Array(event.data.event_sab, 4, 1);
                this.ev_data = new DataView(event.data.event_sab, 8);
                this.param_export_names = event.data.param_export_names;
                if (event.data.clock_sab) {
                    this.clock_view = new BigInt64Array(event.data.clock_sab);
                }
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

    private get_next_event_frame(): number {
        if (!this.ev_data) return Number.MAX_SAFE_INTEGER;
        const wh = Atomics.load(this.ev_write_head!, 0);
        const rh = Atomics.load(this.ev_read_head!, 0);
        if (rh === wh) return Number.MAX_SAFE_INTEGER;
        const slot = (rh % EVENT_CAPACITY) * 16;
        return this.ev_data.getInt32(slot, true);
    }

    process(inputs: Float32Array[][], outputs: Float32Array[][]): boolean {
        if (!this.heap) return !this.stopped;

        const process_start = Date.now();
        const num_samples = outputs[0][0].length;
        if (currentFrame + num_samples <= this.start_frame) return true;
        if (currentFrame >= this.stop_frame) return false;

        const begin = Math.max(0, this.start_frame - currentFrame);
        const end = Math.min(num_samples, this.stop_frame - currentFrame);

        if (this.clock_view) Atomics.store(this.clock_view, 0, BigInt(currentFrame));

        const heap = this.heap;

        let pos = begin;
        let frame = currentFrame + begin;
        while (pos < end) {
            const next_event_frame = this.get_next_event_frame();
            const chunk_end = Math.min(end, Math.max(pos, next_event_frame - currentFrame));
            const chunk_len = chunk_end - pos;

            if (chunk_len > 0) {
                if (this.has_ext) {
                    const ext_base_floats = chunk_len * this.num_out_channels;
                    let channel_offset = 0;
                    for (let group = 0; group < this.external_inputs.length; group++) {
                        const { channels } = this.external_inputs[group];
                        const pin = this.is_global ? inputs[group] : inputs[0];
                        for (let ch = 0; ch < channels; ch++) {
                            const dst = ext_base_floats + (channel_offset + ch) * chunk_len;
                            const src = pin?.[ch];
                            if (src) heap.set(src.subarray(pos, chunk_end), dst);
                            else heap.fill(0, dst, dst + chunk_len);
                        }
                        channel_offset += channels;
                    }
                    this.main(0, ext_base_floats * 4, chunk_len, this.num_out_channels);
                } else {
                    this.main(0, chunk_len, this.num_out_channels);
                }

                for (let ch = 0; ch < this.num_out_channels; ch++) {
                    const out_ch = outputs[0][ch];
                    if (out_ch) {
                        out_ch.set(heap.subarray(ch * chunk_len, (ch + 1) * chunk_len), pos);
                    }
                }
            }

            pos = chunk_end;
            frame += chunk_len;

            if (frame < end + currentFrame) {
                this.consume_events(frame);
            }
        }

        if (begin > 0) {
            for (let ch = 0; ch < this.num_out_channels; ch++) {
                const out_ch = outputs[0][ch];
                if (out_ch) out_ch.fill(0, 0, begin);
            }
        }
        if (end < num_samples) {
            for (let ch = 0; ch < this.num_out_channels; ch++) {
                const out_ch = outputs[0][ch];
                if (out_ch) out_ch.fill(0, end, num_samples);
            }
        }

        if (!this._ready_sent && this.ev_data) {
            this._ready_sent = true;
            this.port.postMessage({ type: 'ready', startTime: currentFrame * inv_sample_rate });
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

        return currentFrame + num_samples < this.stop_frame;
    }
}

class RecorderProcessor extends AudioWorkletProcessor {
    stopped = false;

    constructor() {
        super();
        this.port.onmessage = (event: MessageEvent) => {
            if (event.data.type === 'stop') {
                this.stopped = true;
            }
        };
    }

    process(inputs: Float32Array[][]): boolean {
        const input = inputs[0];
        if (input.length >= 1 && input[0].length > 0) {
            const num_channels = input.length;
            const frame_count = input[0].length;
            const chunk = new Float32Array(frame_count * num_channels);
            for (let ch = 0; ch < num_channels; ch++) {
                const src = input[ch];
                for (let i = 0; i < frame_count; i++) {
                    chunk[i * num_channels + ch] = src[i];
                }
            }
            this.port.postMessage({ type: 'chunk', data: chunk, num_channels }, [chunk.buffer]);
        }
        if (this.stopped) {
            this.port.postMessage({ type: 'finished' });
            return false;
        }
        return true;
    }
}

registerProcessor('wasm-processor', WasmProcessor);
registerProcessor('wasm-recorder', RecorderProcessor);

export default null;
