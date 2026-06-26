import { useEffect, useRef, useState } from 'react';
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
import type { OrchestraState } from '../patch/use_patch_store';

const highlight_style = HighlightStyle.define([
    { tag: tags.keyword, color: '#c792ea', fontWeight: 'bold' },
    { tag: tags.number, color: '#f78c6c' },
    { tag: tags.string, color: '#c3e88d' },
    { tag: [tags.lineComment, tags.blockComment], color: '#546e7a', fontStyle: 'italic' },
    { tag: [tags.function(tags.variableName), tags.operator], color: '#89ddff' },
    { tag: [tags.variableName, tags.propertyName], color: '#82aaff' },
    { tag: tags.typeName, color: '#ffcb6b' },
]);

interface Props {
    orchestra: OrchestraState;
    on_bpm_change: (bpm: number) => void;
    on_add: () => void;
    on_remove: (id: string) => void;
    on_rename: (id: string, name: string) => void;
    on_instrument_code_change: (id: string, code: string) => void;
    on_orchestra_code_change: (code: string) => void;
    on_set_active: (id: string) => void;
}

const ORCHESTRA_DEFS = `
declare function instrument(name: string): any;
declare function sleep(seconds: number): Promise<void>;
declare function sleep_beats(beats: number): Promise<void>;
declare function on_beat(fn: (beat: number) => void): void;
declare function current_time(): number;
`;

const INSTRUMENT_DEFS = `
declare function set_param(name: string, value: number, delay?: number): void;
declare function sleep(seconds: number): Promise<void>;
declare function sleep_beats(beats: number): Promise<void>;
`;

const ORCHESTRA_FALLBACK = `const i = instrument('instrument1');
while (true) {
  i.note(440, 0.4);
  await sleep_beats(1);
}`;

const INSTRUMENT_FALLBACK = `async function note(amp: number, freq: number) {
  set_param('amp', amp);
  set_param('freq', freq);
  set_param('gate', 1);
}`;

const TS_FILE = '/index.ts';
const DEFS_FILE = '/defs.d.ts';
const TS_COMPILER_OPTIONS: ts.CompilerOptions = {
    target: ts.ScriptTarget.ES2025,
    module: ts.ModuleKind.ESNext,
};

const ts_lib_files = import.meta.glob<string>('/node_modules/typescript/lib/lib.*.d.ts', {
    eager: true,
    query: '?raw',
    import: 'default',
});

const orchestra_env_promise: Promise<VirtualTypeScriptEnvironment> | null = null;
const instrument_env_promise: Promise<VirtualTypeScriptEnvironment> | null = null;

function get_env(
    defs: string,
    promise_ref: { current: Promise<VirtualTypeScriptEnvironment> | null },
): Promise<VirtualTypeScriptEnvironment> {
    if (!promise_ref.current) {
        const fs_map = new Map<string, string>();
        for (const [path, content] of Object.entries(ts_lib_files)) {
            fs_map.set('/' + path.split('/').pop()!, content);
        }
        fs_map.set(DEFS_FILE, defs);
        fs_map.set(TS_FILE, ' ');
        const system = createSystem(fs_map);
        promise_ref.current = Promise.resolve(
            createVirtualTypeScriptEnvironment(
                system,
                [DEFS_FILE, TS_FILE],
                ts,
                TS_COMPILER_OPTIONS,
            ),
        );
    }
    return promise_ref.current;
}

const orchestra_env_ref = { current: orchestra_env_promise };
const instrument_env_ref = { current: instrument_env_promise };

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

interface EditorProps {
    initial_value: string;
    on_change: (v: string) => void;
    env: VirtualTypeScriptEnvironment | null;
}

function TsEditor({ initial_value, on_change, env }: EditorProps) {
    const container_ref = useRef<HTMLDivElement>(null);
    const view_ref = useRef<EditorView | null>(null);
    const initial_value_ref = useRef(initial_value);
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
                doc: initial_value_ref.current,
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
        view_ref.current = view;
        return () => view.destroy();
    }, [env]);

    return <div ref={container_ref} style={{ height: '100%' }} />;
}

const ORCHESTRATOR_ID = '__orchestrator__';

export function OrchestraPanel({
    orchestra,
    on_bpm_change,
    on_add,
    on_remove,
    on_rename,
    on_instrument_code_change,
    on_orchestra_code_change,
    on_set_active,
}: Props) {
    const [orchestra_env, set_orchestra_env] = useState<VirtualTypeScriptEnvironment | null>(null);
    const [instrument_env, set_instrument_env] = useState<VirtualTypeScriptEnvironment | null>(
        null,
    );
    const [editing_tab_id, set_editing_tab_id] = useState<string | null>(null);
    const [name_draft, set_name_draft] = useState('');
    const [active_tab, set_active_tab] = useState<string>(ORCHESTRATOR_ID);

    useEffect(() => {
        get_env(ORCHESTRA_DEFS, orchestra_env_ref).then(set_orchestra_env);
        get_env(INSTRUMENT_DEFS, instrument_env_ref).then(set_instrument_env);
    }, []);

    const active_instr =
        active_tab !== ORCHESTRATOR_ID
            ? (orchestra.instruments.find((i) => i.id === active_tab) ?? null)
            : null;

    const handle_tab_click = (id: string) => {
        set_active_tab(id);
        if (id !== ORCHESTRATOR_ID) on_set_active(id);
    };

    const commit_bpm = (
        e: React.FocusEvent<HTMLInputElement> | React.KeyboardEvent<HTMLInputElement>,
    ) => {
        const v = parseFloat((e.target as HTMLInputElement).value);
        if (isFinite(v) && v > 0) on_bpm_change(v);
    };

    const start_rename = (id: string, name: string) => {
        set_editing_tab_id(id);
        set_name_draft(name);
    };

    const commit_rename = () => {
        const trimmed = name_draft.trim();
        if (trimmed && editing_tab_id) on_rename(editing_tab_id, trimmed);
        set_editing_tab_id(null);
    };

    const on_tab_key_down = (e: React.KeyboardEvent<HTMLInputElement>) => {
        if (e.key === 'Enter') commit_rename();
        else if (e.key === 'Escape') set_editing_tab_id(null);
    };

    const handle_remove = (e: React.MouseEvent, id: string) => {
        e.stopPropagation();
        if (active_tab === id) set_active_tab(ORCHESTRATOR_ID);
        on_remove(id);
    };

    return (
        <div className="instrument">
            <div className="instrument__header">
                <div className="instrument__tabs">
                    <div
                        className={`instrument__tab${active_tab === ORCHESTRATOR_ID ? ' instrument__tab--active' : ''}`}
                        onClick={() => handle_tab_click(ORCHESTRATOR_ID)}
                    >
                        <span className="instrument__tab-name">orchestrator</span>
                    </div>
                    {orchestra.instruments.map((instr) => (
                        <div
                            key={instr.id}
                            className={`instrument__tab${active_tab === instr.id ? ' instrument__tab--active' : ''}`}
                            onClick={() => handle_tab_click(instr.id)}
                        >
                            {editing_tab_id === instr.id ? (
                                <input
                                    className="instrument__tab-input"
                                    autoFocus
                                    value={name_draft}
                                    onChange={(e) => set_name_draft(e.target.value)}
                                    onKeyDown={on_tab_key_down}
                                    onBlur={commit_rename}
                                    onClick={(e) => e.stopPropagation()}
                                />
                            ) : (
                                <span
                                    className="instrument__tab-name"
                                    onDoubleClick={(e) => {
                                        e.stopPropagation();
                                        start_rename(instr.id, instr.name);
                                    }}
                                >
                                    {instr.name}
                                </span>
                            )}
                            <button
                                className="instrument__tab-remove"
                                onClick={(e) => handle_remove(e, instr.id)}
                            >
                                ×
                            </button>
                        </div>
                    ))}
                    <button className="instrument__add" onClick={on_add}>
                        +
                    </button>
                </div>
            </div>

            {active_tab === ORCHESTRATOR_ID ? (
                <div className="instrument__orch-body">
                    <div className="instrument__controls">
                        <label className="instrument__bpm-label">
                            BPM
                            <input
                                key={orchestra.bpm}
                                className="instrument__bpm-input"
                                type="number"
                                min={1}
                                max={999}
                                defaultValue={orchestra.bpm}
                                onBlur={commit_bpm}
                                onKeyDown={(e) => e.key === 'Enter' && commit_bpm(e)}
                            />
                        </label>
                    </div>
                    <div className="instrument__editor">
                        <TsEditor
                            key="orchestra"
                            initial_value={orchestra.code || ORCHESTRA_FALLBACK}
                            on_change={on_orchestra_code_change}
                            env={orchestra_env}
                        />
                    </div>
                </div>
            ) : active_instr ? (
                <div className="instrument__editor">
                    <TsEditor
                        key={active_instr.id}
                        initial_value={active_instr.code || INSTRUMENT_FALLBACK}
                        on_change={(code) => on_instrument_code_change(active_instr.id, code)}
                        env={instrument_env}
                    />
                </div>
            ) : (
                <div className="instrument__empty">
                    Adicione um instrument com <strong>+</strong>
                </div>
            )}
        </div>
    );
}
