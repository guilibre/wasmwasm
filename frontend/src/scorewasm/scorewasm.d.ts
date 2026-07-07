declare module './scorewasm.js' {
    interface EmscriptenModule {
        compile_score: (source: string) => string;
        lsp_diagnostics: (src: string) => string;
        lsp_tokens: (src: string) => string;
        lsp_completions: (src: string, line: number, col: number) => string;
        lsp_hover: (src: string, line: number, col: number) => string;
    }

    const Module: (args: object) => Promise<EmscriptenModule>;
}

export default Module;
