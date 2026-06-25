const EVENT_CAPACITY = 256;
const inv_sample_rate = 1 / sampleRate;

class WasmProcessor extends AudioWorkletProcessor {
    main = () => {};
    heap = null;
    has_capture = false;

    _readySent = false;
    instance = null;
    inputBuf = null;
    paramExportNames = [];
    paramExports = [];
    evWriteHead = null;
    evReadHead = null;
    evData = null;

    constructor() {
        super();

        this.port.onmessage = async (event) => {
            if (event.data.type === 'clear') {
                if (this.evReadHead && this.evWriteHead) {
                    Atomics.store(this.evReadHead, 0, Atomics.load(this.evWriteHead, 0));
                }
                this.heap = null;
                this.instance = null;
                this.inputBuf = null;
                this.paramExports = [];
                this.evWriteHead = null;
                this.evReadHead = null;
                this.evData = null;
            }

            if (event.data.type === 'load-wasm') {
                const memory = new WebAssembly.Memory({ initial: 64, maximum: 64 });

                const { instance } = await WebAssembly.instantiate(event.data.buffer, {
                    env: { memory },
                });
                this.heap = new Float32Array(memory.buffer);
                this.instance = instance;
                instance.exports.init();
                this.main = instance.exports.main;
                this.has_capture = this.main.length === 4;
                this._readySent = false;
                if (this.paramExportNames.length > 0) {
                    this.paramExports = this.paramExportNames.map(n => instance.exports[n] ?? null);
                    if (this.inputBuf) {
                        for (let i = 0; i < this.paramExports.length; i++) {
                            if (this.paramExports[i]) this.paramExports[i].value = this.inputBuf[i];
                        }
                    }
                }
            }

            if (event.data.type === 'load-params-sab') {
                this.inputBuf = new Float64Array(event.data.input_sab);
                this.evWriteHead = new Int32Array(event.data.event_sab, 0, 1);
                this.evReadHead = new Int32Array(event.data.event_sab, 4, 1);
                this.evData = new DataView(event.data.event_sab, 8);
                this.paramExportNames = event.data.param_export_names;
                if (this.instance) {
                    this.paramExports = this.paramExportNames.map(n => this.instance.exports[n] ?? null);
                    for (let i = 0; i < this.paramExports.length; i++) {
                        if (this.paramExports[i]) this.paramExports[i].value = this.inputBuf[i];
                    }
                }
            }
        };
    }

    _consume_events(frame) {
        if (!this.evData) return;
        const wh = Atomics.load(this.evWriteHead, 0);
        let rh = Atomics.load(this.evReadHead, 0);
        if (rh === wh) return;
        const exports = this.paramExports;
        const buf = this.inputBuf;
        while (rh !== wh) {
            const slot = (rh % EVENT_CAPACITY) * 16;
            const evFrame = this.evData.getInt32(slot, true);
            if (evFrame > frame) break;
            const idx = this.evData.getInt32(slot + 4, true);
            const value = this.evData.getFloat64(slot + 8, true);
            const g = exports[idx];
            if (g) {
                g.value = value;
                buf[idx] = value;
            }
            rh = (rh + 1) >>> 0;
        }
        Atomics.store(this.evReadHead, 0, rh);
    }

    process(inputs, outputs) {
        if (!this.heap) return true;

        this._consume_events(currentFrame);

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

        if (!this._readySent && this.evData) {
            this._readySent = true;
            this.port.postMessage({ type: 'ready', startTime: currentFrame * inv_sample_rate });
        }

        out_l.set(heap.subarray(0, num_samples));
        if (out_r) out_r.set(heap.subarray(num_samples, num_samples * 2));

        return true;
    }
}

registerProcessor('wasm-processor', WasmProcessor);

export default null;
