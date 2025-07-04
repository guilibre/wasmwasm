declare module "./wasmwasm.js" {
  interface EmscriptenModule {
    callMain: (args: any[]) => number;
    FS_readFile: (path: string, opts = {}) => Uint8Array;
  }

  const Module: (moduleArg: any) => Promise<EmscriptenModule>;
}

export default Module;
