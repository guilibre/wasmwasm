import Module from "../wasmwasm";

export default class WasmWasm {
  private constructor() { }

  static async init(
    sample_freq: number,
    src: string
  ): Promise<Uint8Array<ArrayBufferLike>> {
    const emscripten_module = await Module({});

    try {
      const src_ptr = emscripten_module._malloc(src.length + 1);
      emscripten_module.HEAPU8.set(new TextEncoder().encode(src), src_ptr);
      emscripten_module.HEAPU8[src_ptr + src.length] = 0;
      const fn_response = await fetch("/functions.wasm");
      const fn_buffer = await fn_response.bytes();
      const fn_ptr = emscripten_module._malloc(fn_buffer.length);
      emscripten_module.HEAPU8.set(fn_buffer, fn_ptr);
      const result = emscripten_module._run_compiler(sample_freq, src_ptr, fn_ptr, fn_buffer.length);
      emscripten_module._free(src_ptr);
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
