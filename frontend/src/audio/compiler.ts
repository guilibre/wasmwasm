import Module, { type EmscriptenModule } from '../wasmwasm/wasmwasm';

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
    ): Promise<Uint8Array<ArrayBufferLike>> {
        const mod = await this.getOrInitModule();
        const math_bin = await this.getMathBin();
        try {
            return mod.run_compiler(sample_rate, patch_json, math_bin);
        } catch (err) {
            console.error('error on compilation.', err);
            throw err;
        }
    }
}
