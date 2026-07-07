export interface LspModule {
    lsp_diagnostics: (src: string) => string;
    lsp_tokens: (src: string) => string;
    lsp_completions: (src: string, line: number, col: number) => string;
    lsp_hover: (src: string, line: number, col: number) => string;
}

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

export function get_diagnostics(mod: LspModule, src: string): LspDiagnostic[] {
    try {
        return JSON.parse(mod.lsp_diagnostics(src)) as LspDiagnostic[];
    } catch {
        return [];
    }
}

export function get_tokens(mod: LspModule, src: string): LspToken[] {
    try {
        return JSON.parse(mod.lsp_tokens(src)) as LspToken[];
    } catch {
        return [];
    }
}

export function get_completions(
    mod: LspModule,
    src: string,
    line: number,
    col: number,
): LspCompletion[] {
    try {
        return JSON.parse(mod.lsp_completions(src, line, col)) as LspCompletion[];
    } catch {
        return [];
    }
}

export function get_hover(
    mod: LspModule,
    src: string,
    line: number,
    col: number,
): LspHoverResult | null {
    try {
        const parsed = JSON.parse(mod.lsp_hover(src, line, col));
        return parsed as LspHoverResult | null;
    } catch {
        return null;
    }
}
