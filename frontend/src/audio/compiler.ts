import Module, { type EmscriptenModule } from '../wasmwasm/wasmwasm';

export interface PatchParams {
    param_names: string[];
    param_export_names: string[];
    input_sab: SharedArrayBuffer;
    event_sab: SharedArrayBuffer;
}

export interface CompiledPatch {
    wasm: Uint8Array;
    wasm_module: WebAssembly.Module;
    param_names: string[];
    param_export_names: string[];
    defaults_by_export: Record<string, number>;
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

        const parsed = JSON.parse(patch_json) as { modules: Record<string, string> };
        const defaults_by_export: Record<string, number> = {};
        for (const [mod_name, code] of Object.entries(parsed.modules)) {
            for (const m of code.matchAll(/^\s*param\s+(\w+)\s*=\s*([\d.eE+-]+)/gm)) {
                defaults_by_export[`${mod_name}$param$${m[1]}`] = parseFloat(m[2]);
            }
        }

        return { wasm, wasm_module, param_names, param_export_names, defaults_by_export };
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
