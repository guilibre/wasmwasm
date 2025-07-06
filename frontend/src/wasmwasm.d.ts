declare module "./wasmwasm.js" {
  interface EmscriptenModule {
    _run_compiler: (sample_rate: number) => number;
    FS_readFile: (string, {}) => Uint8Array;
  }

  const Module: (moduleArg: any) => Promise<EmscriptenModule>;
}

export default Module;
