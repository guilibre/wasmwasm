import { useEffect, useMemo, useRef } from 'react';
import { EditorView, keymap } from '@codemirror/view';
import { EditorState, Compartment } from '@codemirror/state';
import { defaultKeymap, indentWithTab, historyKeymap, history } from '@codemirror/commands';
import { autocompletion } from '@codemirror/autocomplete';
import { javascript } from '@codemirror/lang-javascript';
import { HighlightStyle, syntaxHighlighting } from '@codemirror/language';
import { tags } from '@lezer/highlight';
import { tsFacet, tsSync, tsHover, tsLinter } from '@valtown/codemirror-ts';
import type { VirtualTypeScriptEnvironment } from '@typescript/vfs';

const TS_FILE = '/index.ts';

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

    const ts_compartment = useMemo(() => new Compartment(), []);
    const view_ref = useRef<EditorView | null>(null);

    useEffect(() => {
        const view = new EditorView({
            state: EditorState.create({
                doc: initial_value,
                extensions: [
                    history(),
                    keymap.of([indentWithTab, ...historyKeymap, ...defaultKeymap]),
                    javascript({ typescript: true }),
                    syntaxHighlighting(highlight_style),
                    editor_theme,
                    ts_compartment.of([]),
                    EditorView.updateListener.of((update) => {
                        if (update.docChanged) on_change_ref.current(update.state.doc.toString());
                    }),
                ],
            }),
            parent: container_ref.current!,
        });
        view_ref.current = view;
        return () => {
            view.destroy();
            view_ref.current = null;
        };
    }, [initial_value, ts_compartment]);

    useEffect(() => {
        const view = view_ref.current;
        if (!view) return;
        const ts_exts = env
            ? [
                  tsFacet.of({ env, path: TS_FILE }),
                  tsSync(),
                  tsHover(),
                  autocompletion({
                      override: [
                          async (ctx) => {
                              const word = ctx.matchBefore(/\w+/);
                              if (!word && !ctx.explicit) return null;
                              const doc = ctx.state.doc.toString();
                              env.updateFile(TS_FILE, doc);
                              const completions = env.languageService.getCompletionsAtPosition(
                                  TS_FILE,
                                  ctx.pos,
                                  {},
                                  {},
                              );
                              if (!completions) return null;
                              const options = completions.entries
                                  .filter((e) => e.sortText <= '15')
                                  .map((e) => ({ label: e.name, type: e.kind }));
                              return { from: word ? word.from : ctx.pos, options };
                          },
                      ],
                  }),
                  tsLinter({ diagnosticCodesToIgnore: [1308, 1375] }),
              ]
            : [];
        view.dispatch({ effects: ts_compartment.reconfigure(ts_exts) });
        if (env) env.updateFile(TS_FILE, view.state.doc.toString());
    }, [env, ts_compartment]);

    return (
        <div
            ref={container_ref}
            style={{ height: '100%' }}
            onKeyDown={(e) => e.stopPropagation()}
        />
    );
}
