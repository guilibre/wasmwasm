class WasmProcessor extends AudioWorkletProcessor {
    main = () => {};
    heap = null;
    has_capture = false;

    constructor() {
        super();

        this.port.onmessage = async (event) => {
            if (event.data.type !== 'load-wasm') return;

            const memory = new WebAssembly.Memory({ initial: 64, maximum: 64 });
            this.heap = new Float32Array(memory.buffer);

            const { instance } = await WebAssembly.instantiate(event.data.buffer, {
                env: { memory },
            });
            instance.exports.init();
            this.main = instance.exports.main;
            this.has_capture = this.main.length === 4;
        };
    }

    process(inputs, outputs) {
        if (!this.heap) return true;

        const heap = this.heap;
        const num_samples = outputs[0][0].length;
        const out_l = outputs[0][0];
        const out_r = outputs[0][1];

        if (this.has_capture) {
            const in_l = inputs[0][0];
            const in_r = inputs[0][1];
            const in_f32_offset = num_samples * 2;
            for (let i = 0; i < num_samples; i++) {
                heap[in_f32_offset + i * 2] = in_l ? in_l[i] : 0;
                heap[in_f32_offset + i * 2 + 1] = in_r ? in_r[i] : 0;
            }
            this.main(0, num_samples * 8, num_samples, 2);
        } else {
            this.main(0, num_samples, 2);
        }

        if (out_r) {
            for (let i = 0; i < num_samples; i++) {
                out_l[i] = heap[i * 2];
                out_r[i] = heap[i * 2 + 1];
            }
        } else {
            for (let i = 0; i < num_samples; i++) {
                out_l[i] = heap[i * 2];
            }
        }

        if (this.port && currentFrame % 128 === 0) {
            this.port.postMessage({
                type: 'signal',
                data: Array.from(out_l.slice(0, 128)),
            });
        }

        return true;
    }
}

registerProcessor('wasm-processor', WasmProcessor);

export default null;
