import WasmWasm from '../audio/compiler';

export interface LspDiagnostic {
    line: number;
    col: number;
    severity: string;
    msg: string;
}

export interface LspToken {
    line: number;
    col: number;
    len: number;
    type: string;
}

export interface LspCompletion {
    label: string;
    detail: string;
}

export interface LspHoverResult {
    type: string;
}

export function get_diagnostics(src: string): LspDiagnostic[] {
    try {
        return JSON.parse(WasmWasm.getModule().lsp_diagnostics(src)) as LspDiagnostic[];
    } catch {
        return [];
    }
}

export function get_tokens(src: string): LspToken[] {
    try {
        return JSON.parse(WasmWasm.getModule().lsp_tokens(src)) as LspToken[];
    } catch {
        return [];
    }
}

export function get_completions(src: string, line: number, col: number): LspCompletion[] {
    try {
        return JSON.parse(WasmWasm.getModule().lsp_completions(src, line, col)) as LspCompletion[];
    } catch {
        return [];
    }
}

export function get_hover(src: string, line: number, col: number): LspHoverResult | null {
    try {
        const parsed = JSON.parse(WasmWasm.getModule().lsp_hover(src, line, col));
        return parsed as LspHoverResult | null;
    } catch {
        return null;
    }
}
