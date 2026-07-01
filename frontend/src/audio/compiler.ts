import Module, { type EmscriptenModule } from '../wasmwasm/wasmwasm';

export interface PatchParams {
    param_names: string[];
    param_export_names: string[];
    input_sab: SharedArrayBuffer;
    event_sab: SharedArrayBuffer;
}

export interface ExternalInput {
    name: string;
    channels: number;
}

export interface CompiledPatch {
    wasm: Uint8Array;
    wasm_module: WebAssembly.Module;
    param_names: string[];
    param_export_names: string[];
    defaults_by_export: Record<string, number>;
    num_out_channels: number;
    external_inputs: ExternalInput[];
}

function split_trailing_index(src: string): [string, number] | null {
    const under = src.lastIndexOf('_');
    if (under === -1 || under + 1 >= src.length) return null;
    const idx_part = src.slice(under + 1);
    if (!/^\d+$/.test(idx_part)) return null;
    return [src.slice(0, under), parseInt(idx_part, 10)];
}

function derive_channel_info(patch_json: {
    modules?: Record<string, string>;
    patch: Record<string, string>;
}): { num_out_channels: number; external_inputs: ExternalInput[] } {
    const mod_names = new Set(Object.keys(patch_json.modules ?? {}));

    const is_module_output = (src: string): boolean => {
        const under = src.lastIndexOf('_');
        if (under < 4) return false;
        const mod = src.slice(0, under - 4);
        const mid = src.slice(under - 4, under);
        return mid === '_out' && mod_names.has(mod);
    };

    let max_out_idx = -1;
    const ext_channel_counts = new Map<string, number>();

    for (const [sink, src] of Object.entries(patch_json.patch)) {
        if (sink.startsWith('out_')) {
            const idx = parseInt(sink.slice(4), 10);
            if (!isNaN(idx)) max_out_idx = Math.max(max_out_idx, idx);
        }

        if (src === 'capture_l' || src === 'capture_r') {
            const local = src === 'capture_l' ? 0 : 1;
            ext_channel_counts.set(
                'capture',
                Math.max(ext_channel_counts.get('capture') ?? 0, local + 1),
            );
        } else if (!is_module_output(src)) {
            const split = split_trailing_index(src);
            if (split) {
                const [prefix, idx] = split;
                ext_channel_counts.set(
                    prefix,
                    Math.max(ext_channel_counts.get(prefix) ?? 0, idx + 1),
                );
            }
        }
    }

    const external_inputs = [...ext_channel_counts.entries()]
        .sort(([a], [b]) => (a < b ? -1 : a > b ? 1 : 0))
        .map(([name, channels]) => ({ name, channels }));

    return {
        num_out_channels: max_out_idx >= 0 ? max_out_idx + 1 : 2,
        external_inputs,
    };
}

export default class WasmWasm {
    private constructor() {}

    private static module_promise: Promise<EmscriptenModule> | null = null;
    private static emscripten_module: EmscriptenModule | null = null;
    private static math_bin: Uint8Array | null = null;

    private static getOrInitModule(): Promise<EmscriptenModule> {
        if (!this.module_promise) {
            this.module_promise = Module({}).then((m) => {
                this.emscripten_module = m;
                return m;
            });
        }
        return this.module_promise;
    }

    static getModule(): EmscriptenModule {
        if (!this.emscripten_module) throw new Error('WasmWasm not initialized');
        return this.emscripten_module;
    }

    static async ensureReady(): Promise<void> {
        await this.getOrInitModule();
    }

    private static async getMathBin(): Promise<Uint8Array> {
        if (!this.math_bin) {
            const response = await fetch('/wasmwasm/math.wasm');
            this.math_bin = await response.bytes();
        }
        return this.math_bin;
    }

    static async compile_patch(sample_rate: number, patch_json: string): Promise<CompiledPatch> {
        const mod = await this.getOrInitModule();
        const math_bin = await this.getMathBin();
        let wasm: Uint8Array;
        try {
            wasm = mod.run_compiler(sample_rate, patch_json, math_bin);
        } catch (err) {
            console.error('error on compilation.', err);
            throw err;
        }

        const wasm_module = await WebAssembly.compile(wasm as Uint8Array<ArrayBuffer>);
        const exports = WebAssembly.Module.exports(wasm_module);

        const param_exports = exports.filter(
            (e) => e.kind === 'global' && e.name.includes('$param$'),
        );

        const param_export_names = param_exports.map((e) => e.name);
        const param_names = param_exports.map((e) => {
            const idx = e.name.indexOf('$param$');
            return e.name.slice(idx + '$param$'.length);
        });

        const parsed = JSON.parse(patch_json) as {
            modules: Record<string, string>;
            patch: Record<string, string>;
        };
        const defaults_by_export: Record<string, number> = {};
        for (const [mod_name, code] of Object.entries(parsed.modules)) {
            for (const m of code.matchAll(/^\s*param\s+(\w+)\s*=\s*([\d.eE+-]+)/gm)) {
                defaults_by_export[`${mod_name}$param$${m[1]}`] = parseFloat(m[2]);
            }
        }

        const { num_out_channels, external_inputs } = derive_channel_info(parsed);

        return {
            wasm,
            wasm_module,
            param_names,
            param_export_names,
            defaults_by_export,
            num_out_channels,
            external_inputs,
        };
    }

    static make_params(compiled: CompiledPatch): PatchParams {
        const { param_names, param_export_names, defaults_by_export } = compiled;
        const input_sab = new SharedArrayBuffer(param_names.length * 8);
        const event_sab = new SharedArrayBuffer(8 + 256 * 16);
        const input_view = new Float64Array(input_sab);
        for (let i = 0; i < param_export_names.length; i++) {
            const d = defaults_by_export[param_export_names[i]];
            if (d !== undefined) input_view[i] = d;
        }
        return { param_names, param_export_names, input_sab, event_sab };
    }

    static async init_patch(
        sample_rate: number,
        patch_json: string,
    ): Promise<{ wasm: Uint8Array; params: PatchParams }> {
        const compiled = await this.compile_patch(sample_rate, patch_json);
        return { wasm: compiled.wasm, params: this.make_params(compiled) };
    }
}
