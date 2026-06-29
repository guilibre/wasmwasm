import ts from 'typescript';
import { createSystem, createVirtualTypeScriptEnvironment } from '@typescript/vfs';
import type { VirtualTypeScriptEnvironment } from '@typescript/vfs';

const TS_FILE = '/index.ts';
const DEFS_FILE = '/defs.d.ts';
const TS_COMPILER_OPTIONS: ts.CompilerOptions = {
    target: ts.ScriptTarget.ES2025,
    module: ts.ModuleKind.ESNext,
};

const HELPER_DEFS = `
declare function amp_to_db(amp: number): number;
declare function choose<T>(xs: Array<T>): T;
declare function clamp(x: number, lo: number, hi: number): number;
declare function cps_to_midi(f: number): number;
declare function db_to_amp(db: number): number;
declare function gaussian(mean: number = 0, stddev: number = 1): number;
declare function lerp(a: number, b: number, t: number): number;
declare function midi_to_cps(m: number): number;
declare function rand_int(lo: number, hi: number): number;
declare function rand(lo: number, hi: number): number;
`;

const ORCHESTRA_DEFS =
    HELPER_DEFS +
    `
declare function instrument(name: string): any;
declare function sleep(seconds: number): Promise<void>;
declare function sleep_beats(beats: number): Promise<void>;
declare function on_beat(fn: (beat: number) => void): void;
declare function from_beats(beats: number): number;
declare function to_beats(num: number): number;
declare function current_time(): number;
declare function spawn(fn: () => Promise<void>): void;
`;

const INSTRUMENT_DEFS =
    HELPER_DEFS +
    `
declare function set_param(name: string, value: number): void;
declare function sleep(seconds: number): Promise<void>;
`;

export const ORCHESTRA_FALLBACK = `const i = instrument('instrument1');
while (true) {
  i.note(0.2, 440);
  await sleep_beats(1);
}`;

export const INSTRUMENT_FALLBACK = `async function note(amp: number, freq: number) {
  set_param('amp', amp);
  set_param('freq', freq);
  set_param('gate', 1);
}`;

const ts_lib_files = import.meta.glob<string>('/node_modules/typescript/lib/lib.*.d.ts', {
    eager: true,
    query: '?raw',
    import: 'default',
});

function make_env_sync(defs: string): VirtualTypeScriptEnvironment {
    const fs_map = new Map<string, string>();
    for (const [path, content] of Object.entries(ts_lib_files)) {
        fs_map.set('/' + path.split('/').pop()!, content);
    }
    fs_map.set(DEFS_FILE, defs);
    fs_map.set(TS_FILE, ' ');
    const system = createSystem(fs_map);
    return createVirtualTypeScriptEnvironment(
        system,
        [DEFS_FILE, TS_FILE],
        ts,
        TS_COMPILER_OPTIONS,
    );
}

let orchestra_env: VirtualTypeScriptEnvironment | null = null;

export function get_orchestra_env(): VirtualTypeScriptEnvironment {
    return (orchestra_env ??= make_env_sync(ORCHESTRA_DEFS));
}

export function make_instrument_env_with_params(
    param_names: string[],
): VirtualTypeScriptEnvironment {
    const unique = [...new Set(param_names)];
    const setters = unique.map((p) => `declare function set_${p}(value: number): void;`).join('\n');
    return make_env_sync(INSTRUMENT_DEFS + '\n' + setters);
}

export interface CompiledInstrument {
    name: string;
    param_names: string[];
    fn_names: string[];
}

export function make_orchestra_env_with_instruments(
    instruments: CompiledInstrument[],
): VirtualTypeScriptEnvironment {
    const overloads: string[] = [];
    for (const instr of instruments) {
        const unique_params = [...new Set(instr.param_names)];
        const iface_name = `${instr.name}Instance`;
        const setter_members = unique_params
            .map((p) => `    set_${p}(value: number): void;`)
            .join('\n');
        const fn_members = instr.fn_names
            .map((f) => `    ${f}(...args: any[]): Promise<void>;`)
            .join('\n');
        overloads.push(
            `interface ${iface_name} {\n${setter_members}\n${fn_members}\n}`,
            `declare function instrument(name: '${instr.name}'): ${iface_name};`,
            `declare function ${instr.name}(): ${iface_name};`,
        );
    }
    const extra = overloads.join('\n');
    return make_env_sync(extra + '\n' + ORCHESTRA_DEFS);
}
