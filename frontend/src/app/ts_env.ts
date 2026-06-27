import ts from 'typescript';
import { createSystem, createVirtualTypeScriptEnvironment } from '@typescript/vfs';
import type { VirtualTypeScriptEnvironment } from '@typescript/vfs';

const TS_FILE = '/index.ts';
const DEFS_FILE = '/defs.d.ts';
const TS_COMPILER_OPTIONS: ts.CompilerOptions = {
    target: ts.ScriptTarget.ES2025,
    module: ts.ModuleKind.ESNext,
};

const ORCHESTRA_DEFS = `
declare function instrument(name: string): any;
declare function sleep(seconds: number): Promise<void>;
declare function sleep_beats(beats: number): Promise<void>;
declare function on_beat(fn: (beat: number) => void): void;
declare function current_time(): number;
declare function spawn(fn: () => Promise<void>): void;
`;

const INSTRUMENT_DEFS = `
declare function set_param(name: string, value: number, delay?: number): void;
declare function sleep(seconds: number): Promise<void>;
declare function sleep_beats(beats: number): Promise<void>;
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

function make_env(defs: string): Promise<VirtualTypeScriptEnvironment> {
    const fs_map = new Map<string, string>();
    for (const [path, content] of Object.entries(ts_lib_files)) {
        fs_map.set('/' + path.split('/').pop()!, content);
    }
    fs_map.set(DEFS_FILE, defs);
    fs_map.set(TS_FILE, ' ');
    const system = createSystem(fs_map);
    return Promise.resolve(
        createVirtualTypeScriptEnvironment(system, [DEFS_FILE, TS_FILE], ts, TS_COMPILER_OPTIONS),
    );
}

let orchestra_env: Promise<VirtualTypeScriptEnvironment> | null = null;
let instrument_env: Promise<VirtualTypeScriptEnvironment> | null = null;

export function get_orchestra_env(): Promise<VirtualTypeScriptEnvironment> {
    return (orchestra_env ??= make_env(ORCHESTRA_DEFS));
}

export function get_instrument_env(): Promise<VirtualTypeScriptEnvironment> {
    return (instrument_env ??= make_env(INSTRUMENT_DEFS));
}
