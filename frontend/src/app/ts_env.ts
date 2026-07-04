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
type MidiParams =
    | { type: 'noteon'; channel: number; note: number; velocity: number; data: Uint8Array }
    | { type: 'noteoff'; channel: number; note: number; velocity: number; data: Uint8Array }
    | { type: 'cc'; channel: number; cc: number; value: number; data: Uint8Array }
    | { type: 'pitchbend'; channel: number; value: number; data: Uint8Array }
    | { type: 'raw'; channel: number; data: Uint8Array };
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
declare function scramble<T>(xs: T[]) : T[];
declare async function setup_midi();
declare function add_on_midi_event(f: (params: MidiParams) => Promise<void>): string;
declare function remove_on_midi_event(id: string);
declare function stop(fade_out: number);
`;

const ORCHESTRA_DEFS =
    HELPER_DEFS +
    `
declare function instrument(name: string): any;
declare function global(): any;
declare function sleep(seconds: number): Promise<void>;
declare function sleep_beats(beats: number): Promise<void>;
declare function on_beat(fn: (beat: number) => void);
declare function from_beats(beats: number): number;
declare function to_beats(num: number): number;
declare function current_time(): number;
declare function spawn(fn: () => Promise<void>);
interface ScoreEvent {
    readonly midinote: number;
    readonly velocity: number;
    readonly duration: number;
}
interface ScoreRuntime {
    emit(event: ScoreEvent): Promise<void>;
    wait(beats: number): Promise<void>;
}
declare class Score {
    run(runtime: ScoreRuntime): Promise<void>;
}
`;

const INSTRUMENT_DEFS = `
declare function set_param(name: string, value: number);
declare function sleep(seconds: number): Promise<void>;
declare function die();
`;

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
    const setters = unique.map((p) => `declare function set_${p}(value: number);`).join('\n');
    return make_env_sync(INSTRUMENT_DEFS + '\n' + setters);
}

export interface CompiledInstrument {
    name: string;
    param_names: string[];
    fn_names: string[];
    fn_sigs: string[];
}

export function make_orchestra_env_with_instruments(
    instruments: CompiledInstrument[],
): VirtualTypeScriptEnvironment {
    const overloads: string[] = [];
    for (const instr of instruments) {
        const iface_name = `${instr.name}Instance`;
        const fn_members = instr.fn_names
            .map((f, i) => `    ${f}(${instr.fn_sigs[i] ?? '...args: any[]'}): Promise<void>;`)
            .join('\n');
        overloads.push(
            `interface ${iface_name} {\n${fn_members}\n}`,
            `declare function instrument(name: '${instr.name}'): Promise<${iface_name}>;`,
            `declare function ${instr.name}(): Promise<${iface_name}>;`,
        );
    }
    const extra = overloads.join('\n');
    return make_env_sync(extra + '\n' + ORCHESTRA_DEFS);
}
