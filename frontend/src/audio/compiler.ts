import Module from "../wasmwasm";

export default class WasmWasm {
  private constructor() {}

  static async init(sample_freq: number): Promise<Uint8Array<ArrayBufferLike>> {
    const emscripten_module = await Module({});

    try {
      const str = "0.1 + 0.05 -> OUT";
      const ptr = emscripten_module._malloc(str.length + 1);
      emscripten_module.HEAPU8.set(new TextEncoder().encode(str), ptr);
      emscripten_module.HEAPU8[ptr + str.length] = 0;
      const result = emscripten_module._run_compiler(sample_freq, ptr);
      emscripten_module._free(ptr);
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
