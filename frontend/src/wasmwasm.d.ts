declare module "./wasmwasm.js" {
  interface EmscriptenModule {
    run: () => number;
  }

  const Module: (moduleArg = {}) => Promise<EmscriptenModule>;
}

export default Module;
