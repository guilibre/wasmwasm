import WasmWasm from "../audio/compiler";
import type { EmscriptenModule } from "../wasmwasm/wasmwasm";

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

function readCString(mod: EmscriptenModule, ptr: number): string {
  const heap = mod.HEAPU8;
  let end = ptr;
  while (heap[end] !== 0) end++;
  return new TextDecoder().decode(heap.subarray(ptr, end));
}

function allocString(mod: EmscriptenModule, src: string): number {
  const encoded = new TextEncoder().encode(src);
  const ptr = mod._malloc(encoded.length + 1);
  mod.HEAPU8.set(encoded, ptr);
  mod.HEAPU8[ptr + encoded.length] = 0;
  return ptr;
}

export function getDiagnostics(src: string): LspDiagnostic[] {
  try {
    const mod = WasmWasm.getModule();
    const ptr = allocString(mod, src);
    const resultPtr = mod._lsp_diagnostics(ptr);
    const json = readCString(mod, resultPtr);
    mod._free(ptr);
    return JSON.parse(json) as LspDiagnostic[];
  } catch {
    return [];
  }
}

export function getTokens(src: string): LspToken[] {
  try {
    const mod = WasmWasm.getModule();
    const ptr = allocString(mod, src);
    const resultPtr = mod._lsp_tokens(ptr);
    const json = readCString(mod, resultPtr);
    mod._free(ptr);
    return JSON.parse(json) as LspToken[];
  } catch {
    return [];
  }
}

export function getCompletions(src: string, line: number, col: number): LspCompletion[] {
  try {
    const mod = WasmWasm.getModule();
    const ptr = allocString(mod, src);
    const resultPtr = mod._lsp_completions(ptr, line, col);
    const json = readCString(mod, resultPtr);
    mod._free(ptr);
    return JSON.parse(json) as LspCompletion[];
  } catch {
    return [];
  }
}

export function getHover(src: string, line: number, col: number): LspHoverResult | null {
  try {
    const mod = WasmWasm.getModule();
    const ptr = allocString(mod, src);
    const resultPtr = mod._lsp_hover(ptr, line, col);
    const json = readCString(mod, resultPtr);
    mod._free(ptr);
    const parsed = JSON.parse(json);
    return parsed as LspHoverResult | null;
  } catch {
    return null;
  }
}
