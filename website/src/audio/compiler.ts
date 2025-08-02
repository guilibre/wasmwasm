import Module, { type EmscriptenModule } from "../wasmwasm/wasmwasm";

export default class WasmWasm {
  private constructor() {}

  private static emscripten_module: EmscriptenModule | null = null;

  static async init(
    sample_rate: number,
    src: string
  ): Promise<Uint8Array<ArrayBufferLike>> {
    if (!this.emscripten_module) this.emscripten_module = await Module();

    try {
      const src_ptr = this.emscripten_module._malloc(src.length + 1);
      this.emscripten_module.HEAPU8.set(new TextEncoder().encode(src), src_ptr);
      this.emscripten_module.HEAPU8[src_ptr + src.length] = 0;
      const fn_response = await fetch("/wasmwasm/math.wasm");
      const fn_buffer = await fn_response.bytes();
      const fn_ptr = this.emscripten_module._malloc(fn_buffer.length);
      this.emscripten_module.HEAPU8.set(fn_buffer, fn_ptr);
      const result = this.emscripten_module._run_compiler(
        sample_rate,
        src_ptr,
        fn_ptr,
        fn_buffer.length
      );
      this.emscripten_module._free(src_ptr);
      if (result !== 0) throw new Error("main returned " + result);
    } catch (err) {
      console.error("error on compilation.", err);
      throw err;
    }

    let buffer: Uint8Array;
    try {
      buffer = this.emscripten_module.FS_readFile("/tmp/output.wasm", {
        enconding: "binary",
      });
    } catch (err) {
      console.error("error reading file.", err);
      throw err;
    }

    return buffer;
  }
}
