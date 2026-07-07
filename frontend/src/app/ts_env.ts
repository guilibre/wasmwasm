import ts from 'typescript';
import {
    createDefaultMapFromCDN,
    createSystem,
    createVirtualTypeScriptEnvironment,
} from '@typescript/vfs';
import type { VirtualTypeScriptEnvironment } from '@typescript/vfs';

const compiler_options: ts.CompilerOptions = {
    target: ts.ScriptTarget.ES2020,
    strict: true,
    noEmit: true,
};

let env_promise: Promise<VirtualTypeScriptEnvironment> | null = null;

export function get_ts_env(): Promise<VirtualTypeScriptEnvironment> {
    if (!env_promise) {
        env_promise = createDefaultMapFromCDN(compiler_options, ts.version, true, ts).then(
            (fs_map) => {
                const system = createSystem(fs_map);
                return createVirtualTypeScriptEnvironment(system, [], ts, compiler_options);
            },
        );
    }
    return env_promise;
}

export function callback_source_file(path: string): string {
    return `/${path}.ts`;
}
