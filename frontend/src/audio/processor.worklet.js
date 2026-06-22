class WasmProcessor extends AudioWorkletProcessor {
    main = () => {};
    heap = null;
    has_capture = false;
    _vizCounter = 0;
    instance = null;
    paramBuf = null;
    paramExportNames = [];

    constructor() {
        super();

        this.port.onmessage = async (event) => {
            if (event.data.type === 'load-wasm') {
                const memory = new WebAssembly.Memory({ initial: 64, maximum: 64 });
                this.heap = new Float32Array(memory.buffer);

                const { instance } = await WebAssembly.instantiate(event.data.buffer, {
                    env: { memory },
                });
                this.instance = instance;
                instance.exports.init();
                this.main = instance.exports.main;
                this.has_capture = this.main.length === 4;
            }

            if (event.data.type === 'load-params-sab') {
                this.paramBuf = new Float64Array(event.data.sab);
                this.paramExportNames = event.data.paramExportNames;
            }
        };
    }

    _apply_params() {
        if (!this.paramBuf || !this.instance) return;
        const names = this.paramExportNames;
        const buf = this.paramBuf;
        for (let i = 0; i < names.length; i++) {
            const g = this.instance.exports[names[i]];
            if (g) g.value = buf[i];
        }
        buf[names.length] = currentFrame / sampleRate;
    }

    process(inputs, outputs) {
        if (!this.heap) return true;

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
