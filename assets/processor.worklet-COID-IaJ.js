const EVENT_CAPACITY = 256;

class WasmProcessor extends AudioWorkletProcessor {
    main = () => {};
    heap = null;
    has_capture = false;
    _vizCounter = 0;
    _readySent = false;
    instance = null;
    inputBuf = null;
    stateBuf = null;
    evWriteHead = null;
    evReadHead = null;
    evData = null;
    paramExportNames = [];

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
                this.stateBuf = null;
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
            }

            if (event.data.type === 'load-params-sab') {
                this.inputBuf = new Float64Array(event.data.input_sab);
                this.stateBuf = new Float64Array(event.data.state_sab);
                this.evWriteHead = new Int32Array(event.data.event_sab, 0, 1);
                this.evReadHead = new Int32Array(event.data.event_sab, 4, 1);
                this.evData = new DataView(event.data.event_sab, 8);
                this.paramExportNames = event.data.param_export_names;
            }
        };
    }

    _consume_events(frame) {
        if (!this.evData || !this.instance) return;
        const wh = Atomics.load(this.evWriteHead, 0);
        let rh = Atomics.load(this.evReadHead, 0);
        while (rh !== wh) {
            const slot = (rh % EVENT_CAPACITY) * 16;
            const evFrame = this.evData.getInt32(slot, true);
            if (evFrame > frame) break;
            const idx = this.evData.getInt32(slot + 4, true);
            const value = this.evData.getFloat64(slot + 8, true);
            const g = this.instance.exports[this.paramExportNames[idx]];
            if (g) {
                g.value = value;
                this.inputBuf[idx] = value;
            }
            rh = (rh + 1) >>> 0;
            Atomics.store(this.evReadHead, 0, rh);
        }
    }

    _apply_params() {
        if (!this.inputBuf || !this.instance) return;
        const names = this.paramExportNames;
        const buf = this.inputBuf;
        for (let i = 0; i < names.length; i++) {
            const g = this.instance.exports[names[i]];
            if (g) g.value = buf[i];
        }
    }

    _writeback_params() {
        if (!this.stateBuf || !this.instance) return;
        const names = this.paramExportNames;
        const buf = this.stateBuf;
        for (let i = 0; i < names.length; i++) {
            const g = this.instance.exports[names[i]];
            if (g) {
                buf[i] = g.value;
                this.inputBuf[i] = g.value;
            }
        }
        buf[names.length] = currentFrame / sampleRate;

        if (!this._readySent) {
            this._readySent = true;
            this.port.postMessage({ type: 'ready', startTime: currentFrame / sampleRate });
        }
    }

    process(inputs, outputs) {
        if (!this.heap) return true;

        this._consume_events(currentFrame);
        this._apply_params();

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

        this._writeback_params();

        out_l.set(heap.subarray(0, num_samples));
        if (out_r) out_r.set(heap.subarray(num_samples, num_samples * 2));

        if (this.port && ++this._vizCounter >= 16) {
            this._vizCounter = 0;
            const buf = new Float32Array(num_samples);
            buf.set(out_l);
            this.port.postMessage({ type: 'signal', data: buf }, [buf.buffer]);
        }

        return true;
    }
}

registerProcessor('wasm-processor', WasmProcessor);

export default null;
