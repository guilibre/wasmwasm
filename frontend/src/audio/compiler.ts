import Module, { type EmscriptenModule } from '../wasmwasm/wasmwasm';

export default class WasmWasm {
    private constructor() {}

    private static emscripten_module: EmscriptenModule | null = null;
    private static math_bin: Uint8Array | null = null;

    static getModule(): EmscriptenModule {
        if (!this.emscripten_module) throw new Error('WasmWasm not initialized');
        return this.emscripten_module;
    }

    static async ensureReady(): Promise<void> {
        if (!this.emscripten_module) this.emscripten_module = await Module({});
    }

    private static async getMathBin(): Promise<Uint8Array> {
        if (!this.math_bin) {
            const response = await fetch('/wasmwasm/math.wasm');
            this.math_bin = await response.bytes();
        }
        return this.math_bin;
    }

    static async init(sample_rate: number, src: string): Promise<Uint8Array<ArrayBufferLike>> {
        if (!this.emscripten_module) this.emscripten_module = await Module({});

        const math_bin = await this.getMathBin();

        try {
            return this.emscripten_module.run_compiler(sample_rate, src, math_bin);
        } catch (err) {
            console.error('error on compilation.', err);
            throw err;
        }
    }
}
