const OUT_NAME_RE = /^OUT\s+(\d+)$/i;
const IN_NAME_RE = /^IN\s+(\d+)$/i;

export function parse_out_name(name: string): number | null {
    const m = OUT_NAME_RE.exec(name.trim());
    if (!m) return null;
    const n = parseInt(m[1], 10);
    return n > 0 ? n : null;
}

export function parse_in_name(name: string): number | null {
    const m = IN_NAME_RE.exec(name.trim());
    if (!m) return null;
    const n = parseInt(m[1], 10);
    return n >= 0 ? n : null;
}

export function scan_params(code: string): string[] {
    return [...code.matchAll(/^\s*param\s+(\w+)\s*=/gm)].map((m) => m[1]);
}

export function scan_arity(code: string): { num_inputs: number; num_outputs: number } {
    const ins = [...code.matchAll(/IN\[(\d+)\]/g)].map((m) => parseInt(m[1]));
    const outs = [...code.matchAll(/OUT\[(\d+)\]/g)].map((m) => parseInt(m[1]));
    return {
        num_inputs: ins.length > 0 ? Math.max(...ins) + 1 : 0,
        num_outputs: outs.length > 0 ? Math.max(...outs) + 1 : 0,
    };
}
