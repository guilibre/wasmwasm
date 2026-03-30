declare module "./wasmwasm.js" {
  interface EmscriptenModule {
    _free: (ptr) => void;
    _malloc: (num) => number;
    _run_compiler: (sample_rate: number, src_ptr, fn_ptr, fn_size) => number;
    _lsp_diagnostics: (ptr) => number;
    _lsp_tokens: (ptr) => number;
    _lsp_completions: (ptr, line: number, col: number) => number;
    _lsp_hover: (ptr, line: number, col: number) => number;
    FS_readFile: (...args) => Uint8Array;
    lengthBytesUTF8: (str) => number;
    stringToUTF8: (str, outPtr, maxBytesToWrite) => number;
    HEAPU8: Uint8Array;
  }

  const Module: (args) => Promise<EmscriptenModule>;
}

export default Module;
