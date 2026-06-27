import { useEffect, useRef } from 'react';
import { EditorView, keymap } from '@codemirror/view';
import { EditorState } from '@codemirror/state';
import { defaultKeymap, indentWithTab, historyKeymap, history } from '@codemirror/commands';
import { autocompletion } from '@codemirror/autocomplete';
import { javascript } from '@codemirror/lang-javascript';
import { HighlightStyle, syntaxHighlighting } from '@codemirror/language';
import { tags } from '@lezer/highlight';
import { tsFacet, tsSync, tsHover, tsAutocomplete, tsLinter } from '@valtown/codemirror-ts';
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

const highlight_style = HighlightStyle.define([
    { tag: tags.keyword, color: '#c792ea', fontWeight: 'bold' },
    { tag: tags.number, color: '#f78c6c' },
    { tag: tags.string, color: '#c3e88d' },
    { tag: [tags.lineComment, tags.blockComment], color: '#546e7a', fontStyle: 'italic' },
    { tag: [tags.function(tags.variableName), tags.operator], color: '#89ddff' },
    { tag: [tags.variableName, tags.propertyName], color: '#82aaff' },
    { tag: tags.typeName, color: '#ffcb6b' },
]);

const editor_theme = EditorView.theme(
    {
        '&': { height: '100%', background: 'transparent', color: '#cdd6f4' },
        '.cm-scroller': {
            overflow: 'auto',
            fontFamily: 'monospace',
            fontSize: '0.7rem',
            lineHeight: '1.5',
        },
        '.cm-content': { padding: '0.5rem', caretColor: 'currentColor' },
        '.cm-line': { padding: '0' },
        '&.cm-focused': { outline: 'none' },
        '.cm-cursor': { borderLeftColor: '#cdd6f4' },
        '.cm-tooltip': { background: '#1e1e2e', border: '1px solid #45475a' },
        '.cm-tooltip-autocomplete ul li[aria-selected]': { background: '#313244' },
        '.cm-gutters': { background: 'transparent', border: 'none', color: '#585b70' },
    },
    { dark: true },
);

interface TsEditorProps {
    initial_value: string;
    on_change: (v: string) => void;
    env: VirtualTypeScriptEnvironment | null;
}

export function TsEditor({ initial_value, on_change, env }: TsEditorProps) {
    const container_ref = useRef<HTMLDivElement>(null);
    const on_change_ref = useRef(on_change);
    useEffect(() => {
        on_change_ref.current = on_change;
    });

    useEffect(() => {
        const ts_extensions = env
            ? [
                  tsFacet.of({ env, path: TS_FILE }),
                  tsSync(),
                  tsHover(),
                  autocompletion({ override: [tsAutocomplete()] }),
                  tsLinter({ diagnosticCodesToIgnore: [1308, 1375] }),
              ]
            : [];

        const view = new EditorView({
            state: EditorState.create({
                doc: initial_value,
                extensions: [
                    history(),
                    keymap.of([indentWithTab, ...historyKeymap, ...defaultKeymap]),
                    javascript({ typescript: true }),
                    syntaxHighlighting(highlight_style),
                    ...ts_extensions,
                    editor_theme,
                    EditorView.updateListener.of((update) => {
                        if (update.docChanged) on_change_ref.current(update.state.doc.toString());
                    }),
                ],
            }),
            parent: container_ref.current!,
        });
        return () => view.destroy();
    }, [env]);

    return <div ref={container_ref} style={{ height: '100%' }} />;
}
