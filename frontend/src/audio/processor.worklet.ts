export default class WasmProcessor extends AudioWorkletProcessor {
  private main: CallableFunction | undefined;
  private heap: Float32Array<ArrayBuffer> | undefined;

  public constructor() {
    super();
    this.port.onmessage = async (event) => {
      if (event.data.type !== "load-wasm") return;

      const memory = new WebAssembly.Memory({ initial: 4096, maximum: 4096 });
      this.heap = new Float32Array(memory.buffer);

      const { instance } = await WebAssembly.instantiate(event.data.buffer, {
        Math: { sin: Math.sin },
        env: { memory: memory },
      });
      this.main = instance.exports.main as CallableFunction;
    };
  }

  public process(
    _inputs: Float32Array[][],
    outputs: Float32Array[][]
  ): boolean {
    if (!this.main || !this.heap) return true;

    const output = outputs[0];
    const width = output.length;
    const height = output[0].length;
    this.main(0, height, width, 440);
    const buffer = this.heap.slice(0, width * height);
    for (let j = 0; j < height; ++j) {
      for (let i = 0; i < width; ++i) {
        output[i][j] = 0.1 * buffer[i + width * j];
      }
    }
    return true;
  }
}

registerProcessor("wasm-processor", WasmProcessor);
