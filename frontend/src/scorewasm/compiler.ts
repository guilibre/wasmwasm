import Module, { type EmscriptenModule } from './scorewasm';
import type { ScoreGraph } from '../audio/conductor';

export default class ScoreWasm {
    private constructor() {}

    private static module_promise: Promise<EmscriptenModule> | null = null;
    private static emscripten_module: EmscriptenModule | null = null;

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
        if (!this.emscripten_module) throw new Error('ScoreWasm not initialized');
        return this.emscripten_module;
    }

    static async ensureReady(): Promise<void> {
        await this.getOrInitModule();
    }

    static async compile_score(source: string): Promise<ScoreGraph> {
        const mod = await this.getOrInitModule();
        const result = JSON.parse(mod.compile_score(source)) as ScoreGraph;
        console.log(JSON.stringify(result));
        return result;
    }
}
