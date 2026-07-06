import Module, { type EmscriptenModule } from '../wasmwasm/wasmwasm';

export interface CompiledPatch {
    wasm_module: WebAssembly.Module;
    memory_bytes: number;
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
        try {
            const { bytes, memory_bytes } = mod.run_compiler(sample_rate, patch_json, math_bin);
            const wasm_module = await WebAssembly.compile(bytes as Uint8Array<ArrayBuffer>);
            return {
                wasm_module,
                memory_bytes,
            };
        } catch (err) {
            console.error('error on compilation.', err);
            throw err;
        }
    }
}
