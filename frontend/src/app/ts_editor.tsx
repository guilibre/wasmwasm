import { useEffect, useRef } from 'react';
import { EditorView, keymap } from '@codemirror/view';
import { EditorState } from '@codemirror/state';
import { defaultKeymap, indentWithTab, historyKeymap, history } from '@codemirror/commands';
import { autocompletion } from '@codemirror/autocomplete';
import { javascript } from '@codemirror/lang-javascript';
import { HighlightStyle, syntaxHighlighting } from '@codemirror/language';
import { tags } from '@lezer/highlight';
import { tsFacet, tsSync, tsHover, tsAutocomplete, tsLinter } from '@valtown/codemirror-ts';
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
    }, [env, initial_value]);

    return <div ref={container_ref} style={{ height: '100%' }} />;
}
