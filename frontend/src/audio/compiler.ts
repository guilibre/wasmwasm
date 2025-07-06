import Module from "../wasmwasm";

export default class WasmWasm {
  private constructor() {}

  static async init(sample_freq: number): Promise<Uint8Array<ArrayBufferLike>> {
    const emscripten_module = await Module({});

    try {
      const result = emscripten_module._run_compiler(sample_freq);
      if (result !== 0) throw new Error("main returned " + result);
    } catch (err) {
      console.error("error on compilation.");
      throw err;
    }

    let buffer: Uint8Array;
    try {
      buffer = emscripten_module.FS_readFile("/tmp/output.wasm", {
        enconding: "binary",
      });
    } catch (err) {
      console.error("error reading file.");
      throw err;
    }

    return buffer;
  }
}
