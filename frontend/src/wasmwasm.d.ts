declare module "./wasmwasm.js" {
  interface EmscriptenModule {
    _free: (ptr) => void;
    _malloc: (num) => number;
    _run_compiler: (sample_rate: number, src_ptr) => number;
    FS_readFile: (...args) => Uint8Array;
    lengthBytesUTF8: (str) => number;
    stringToUTF8: (str, outPtr, maxBytesToWrite) => number;
    HEAPU8: Uint8Array;
  }

  const Module: (moduleArg: any) => Promise<EmscriptenModule>;
}

export default Module;
