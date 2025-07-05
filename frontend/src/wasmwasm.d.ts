declare module "./wasmwasm.js" {
  interface EmscriptenModule {
    _main: () => number;
    FS_readFile: (path: string, opts = {}) => Uint8Array;
  }

  const Module: (moduleArg: any) => Promise<EmscriptenModule>;
}

export default Module;
