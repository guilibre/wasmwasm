declare module "./wasmwasm.js" {
  interface EmscriptenModule {
    _main: () => number;
    FS_readFile: (string, {}) => Uint8Array;
  }

  const Module: (moduleArg: any) => Promise<EmscriptenModule>;
}

export default Module;
