export default class WasmProcessor extends AudioWorkletProcessor {
  private main: CallableFunction = () => { };
  private heap: Float32Array<ArrayBuffer> = new Float32Array();

  public constructor() {
    super();
    this.port.onmessage = async (event) => {
      if (event.data.type !== "load-wasm") return;

      const memory = new WebAssembly.Memory({ initial: 1, maximum: 1 });
      this.heap = new Float32Array(memory.buffer);

      const { instance } = await WebAssembly.instantiate(event.data.buffer, {
        env: { memory },
      });
      this.main = instance.exports.main as CallableFunction;

      (instance.exports.init_buffers as CallableFunction)();
    };
  }

  public process(
    _inputs: Float32Array[][],
    outputs: Float32Array[][]
  ): boolean {
    const width = outputs[0].length;
    const height = outputs[0][0].length;
    this.main(0, height, width);
    for (let i = 0; i < width; ++i)
      outputs[0][i].set(this.heap.subarray(i * height, (i + 1) * height));

    if (this.port && currentFrame % 128 === 0) {
      this.port.postMessage({
        type: "signal",
        data: Array.from(outputs[0][0].slice(0, 128)), // small chunk for display
      });
    }

    return true;
  }
}

registerProcessor("wasm-processor", WasmProcessor);
