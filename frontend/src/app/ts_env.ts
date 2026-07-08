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

const callback_types_source = `interface TokenParams {
    instrument_id: string | null;
    params: Record<string, number>;
}

interface InstrumentCallbackHandler {
    call(p: Record<string, number>, ap: TokenParams[], beat: number): Record<string, number>;
}

interface GlobalCallbackHandler {
    call(ap: TokenParams[], beat: number): Record<string, number>;
}
`;

const callback_types_file = '/callback-types.d.ts';

let env_promise: Promise<VirtualTypeScriptEnvironment> | null = null;

export function get_ts_env(): Promise<VirtualTypeScriptEnvironment> {
    if (!env_promise) {
        env_promise = createDefaultMapFromCDN(compiler_options, ts.version, true, ts).then(
            (fs_map) => {
                fs_map.set(callback_types_file, callback_types_source);
                const system = createSystem(fs_map);
                return createVirtualTypeScriptEnvironment(
                    system,
                    [callback_types_file],
                    ts,
                    compiler_options,
                );
            },
        );
    }
    return env_promise;
}

export function callback_source_file(path: string): string {
    return `/${path}.ts`;
}
