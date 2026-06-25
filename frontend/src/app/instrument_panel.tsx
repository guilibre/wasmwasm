import { useEffect, useRef, useState } from 'react';
import { EditorView, keymap } from '@codemirror/view';
import { EditorState } from '@codemirror/state';
import { defaultKeymap, indentWithTab, historyKeymap, history } from '@codemirror/commands';
import { autocompletion } from '@codemirror/autocomplete';
import { javascript } from '@codemirror/lang-javascript';
import { HighlightStyle, syntaxHighlighting } from '@codemirror/language';
import { tags } from '@lezer/highlight';

const highlightStyle = HighlightStyle.define([
    { tag: tags.keyword, color: '#c792ea', fontWeight: 'bold' },
    { tag: tags.number, color: '#f78c6c' },
    { tag: tags.string, color: '#c3e88d' },
    { tag: [tags.lineComment, tags.blockComment], color: '#546e7a', fontStyle: 'italic' },
    { tag: [tags.function(tags.variableName), tags.operator], color: '#89ddff' },
    { tag: [tags.variableName, tags.propertyName], color: '#82aaff' },
    { tag: tags.typeName, color: '#ffcb6b' },
]);
import { tsFacet, tsSync, tsHover, tsAutocomplete, tsLinter } from '@valtown/codemirror-ts';
import ts from 'typescript';
import { createSystem, createVirtualTypeScriptEnvironment } from '@typescript/vfs';
import type { VirtualTypeScriptEnvironment } from '@typescript/vfs';
import type { PatchParams } from '../audio/compiler';

interface Props {
    params: PatchParams | null;
    code: string;
    bpm: number;
    on_code_change: (code: string) => void;
    on_bpm_change: (bpm: number) => void;
    error: string | null;
}

const FALLBACK =
    "// Example:\nwhile (true) {\n  setParam('gain', Math.random());\n  await sleepBeats(1);\n}";

const TS_FILE = '/index.ts';
const DEFS_FILE = '/defs.d.ts';
const TS_COMPILER_OPTIONS: ts.CompilerOptions = {
    target: ts.ScriptTarget.ES2022,
    module: ts.ModuleKind.ESNext,
};

const INSTRUMENT_DEFS = `
declare function setParam(name: string, value: number): void;
declare function sleep(seconds: number): Promise<void>;
declare function sleepBeats(beats: number): Promise<void>;
declare function onBeat(fn: (beat: number) => void): void;
declare function currentTime(): number;
`;

const tsLibFiles = import.meta.glob<string>('/node_modules/typescript/lib/lib.*.d.ts', {
    eager: true,
    query: '?raw',
    import: 'default',
});

let envPromise: Promise<VirtualTypeScriptEnvironment> | null = null;

function getEnv(): Promise<VirtualTypeScriptEnvironment> {
    if (!envPromise) {
        const fsMap = new Map<string, string>();
        for (const [path, content] of Object.entries(tsLibFiles)) {
            fsMap.set('/' + path.split('/').pop()!, content);
        }
        fsMap.set(DEFS_FILE, INSTRUMENT_DEFS);
        fsMap.set(TS_FILE, ' ');
        const system = createSystem(fsMap);
        envPromise = Promise.resolve(
            createVirtualTypeScriptEnvironment(
                system,
                [DEFS_FILE, TS_FILE],
                ts,
                TS_COMPILER_OPTIONS,
            ),
        );
    }
    return envPromise;
}

const editorTheme = EditorView.theme(
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

interface EditorProps {
    initialValue: string;
    onChange: (v: string) => void;
    env: VirtualTypeScriptEnvironment | null;
}

function TsEditor({ initialValue, onChange, env }: EditorProps) {
    const containerRef = useRef<HTMLDivElement>(null);
    const viewRef = useRef<EditorView | null>(null);
    const initialValueRef = useRef(initialValue);
    const onChangeRef = useRef(onChange);
    useEffect(() => {
        onChangeRef.current = onChange;
    });

    useEffect(() => {
        const tsExtensions = env
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
                doc: initialValueRef.current,
                extensions: [
                    history(),
                    keymap.of([indentWithTab, ...historyKeymap, ...defaultKeymap]),
                    javascript({ typescript: true }),
                    syntaxHighlighting(highlightStyle),
                    ...tsExtensions,
                    editorTheme,
                    EditorView.updateListener.of((update) => {
                        if (update.docChanged) onChangeRef.current(update.state.doc.toString());
                    }),
                ],
            }),
            parent: containerRef.current!,
        });
        viewRef.current = view;
        return () => view.destroy();
    }, [env]);

    return <div ref={containerRef} style={{ height: '100%' }} />;
}

export function InstrumentPanel({ code, bpm, on_code_change, on_bpm_change, error }: Props) {
    const [env, setEnv] = useState<VirtualTypeScriptEnvironment | null>(null);

    useEffect(() => {
        getEnv().then(setEnv);
    }, []);

    const commit_bpm = (
        e: React.FocusEvent<HTMLInputElement> | React.KeyboardEvent<HTMLInputElement>,
    ) => {
        const v = parseFloat((e.target as HTMLInputElement).value);
        if (isFinite(v) && v > 0) on_bpm_change(v);
    };

    return (
        <div className="instrument">
            <div className="instrument__header">
                <span className="instrument__title">Instrument</span>
                <div className="instrument__controls">
                    <label className="instrument__bpm-label">
                        BPM
                        <input
                            key={bpm}
                            className="instrument__bpm-input"
                            type="number"
                            min={1}
                            max={999}
                            defaultValue={bpm}
                            onBlur={commit_bpm}
                            onKeyDown={(e) => e.key === 'Enter' && commit_bpm(e)}
                        />
                    </label>
                </div>
            </div>
            {error && <div className="instrument__error">{error}</div>}
            <div className="instrument__editor">
                <TsEditor
                    key={code === '' ? 'empty' : 'user'}
                    initialValue={code || FALLBACK}
                    onChange={on_code_change}
                    env={env}
                />
            </div>
        </div>
    );
}
