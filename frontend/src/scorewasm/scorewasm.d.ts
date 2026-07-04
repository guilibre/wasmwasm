declare module './scorewasm.js' {
    interface EmscriptenModule {
        run_compiler: (sample_rate: number, patch_json: string, math_bin: Uint8Array) => Uint8Array;
        compile_score: (src: string) => string;
        lsp_diagnostics: (src: string) => string;
        lsp_tokens: (src: string) => string;
        lsp_completions: (src: string, line: number, col: number) => string;
        lsp_hover: (src: string, line: number, col: number) => string;
    }

    const Module: (args: object) => Promise<EmscriptenModule>;
}

export default Module;
