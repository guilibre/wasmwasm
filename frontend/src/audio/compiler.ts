import Module, { type EmscriptenModule } from '../wasmwasm/wasmwasm';

export interface PatchParams {
    paramNames: string[];
    paramExportNames: string[];
    input_sab: SharedArrayBuffer;
    state_sab: SharedArrayBuffer;
    event_sab: SharedArrayBuffer;
}

export default class WasmWasm {
    private constructor() {}

    private static _modulePromise: Promise<EmscriptenModule> | null = null;
    private static emscripten_module: EmscriptenModule | null = null;
    private static math_bin: Uint8Array | null = null;

    private static getOrInitModule(): Promise<EmscriptenModule> {
        if (!this._modulePromise) {
            this._modulePromise = Module({}).then((m) => {
                this.emscripten_module = m;
                return m;
            });
        }
        return this._modulePromise;
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

    static async init_patch(
        sample_rate: number,
        patch_json: string,
    ): Promise<{ wasm: Uint8Array; params: PatchParams }> {
        const mod = await this.getOrInitModule();
        const math_bin = await this.getMathBin();
        let wasm: Uint8Array;
        try {
            wasm = mod.run_compiler(sample_rate, patch_json, math_bin);
        } catch (err) {
            console.error('error on compilation.', err);
            throw err;
        }

        const wasmModule = await WebAssembly.compile(wasm as Uint8Array<ArrayBuffer>);
        const exports = WebAssembly.Module.exports(wasmModule);

        const paramExports = exports.filter(
            (e) => e.kind === 'global' && e.name.includes('$param$'),
        );

        const paramExportNames = paramExports.map((e) => e.name);
        const paramNames = paramExports.map((e) => {
            const idx = e.name.indexOf('$param$');
            return e.name.slice(idx + '$param$'.length);
        });

        const input_sab = new SharedArrayBuffer(paramNames.length * 8);
        const state_sab = new SharedArrayBuffer((paramNames.length + 1) * 8);
        const event_sab = new SharedArrayBuffer(8 + 256 * 16);
        const inputView = new Float64Array(input_sab);
        const parsed = JSON.parse(patch_json) as { modules: Record<string, string> };
        const defaultsByExport: Record<string, number> = {};
        for (const [modName, code] of Object.entries(parsed.modules)) {
            for (const m of code.matchAll(/^\s*param\s+(\w+)\s*=\s*([\d.eE+-]+)/gm)) {
                defaultsByExport[`${modName}$param$${m[1]}`] = parseFloat(m[2]);
            }
        }
        for (let i = 0; i < paramExportNames.length; i++) {
            const d = defaultsByExport[paramExportNames[i]];
            if (d !== undefined) inputView[i] = d;
        }

        return { wasm, params: { paramNames, paramExportNames, input_sab, state_sab, event_sab } };
    }
}
