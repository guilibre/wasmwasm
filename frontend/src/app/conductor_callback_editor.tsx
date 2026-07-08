import { useEffect, useRef, useState } from 'react';
import { EditorView, keymap } from '@codemirror/view';
import { EditorState, Compartment } from '@codemirror/state';
import { defaultKeymap, indentWithTab, historyKeymap, history } from '@codemirror/commands';
import { autocompletion, closeBrackets } from '@codemirror/autocomplete';
import { javascript } from '@codemirror/lang-javascript';
import { syntaxHighlighting, HighlightStyle } from '@codemirror/language';
import { tags } from '@lezer/highlight';
import { tsFacet, tsSync, tsLinter, tsAutocomplete, tsHover } from '@valtown/codemirror-ts';
import { get_ts_env, callback_source_file } from './ts_env';

export const instrument_callback_example = `class InstrumentHandler implements InstrumentCallbackHandler {
  call(
    p: Record<string, number>,
    ap: TokenParams[],
  ): Record<string, number> {
    const amp = p['amp'] ?? 0.2;
    const atk = p['atk'] ?? 0.01;
    const rel = p['rel'] ?? (p['dur'] ?? 0.5);
    const freq = p['freq'] ?? 440;
    const pan = p['pan'] ?? 0;

    return {
      amp,
      atk,
      rel,
      freq,
      pan,
    };
  }
}`;

export const global_callback_example = `class GlobalHandler implements GlobalCallbackHandler {
  call(ap: TokenParams[]): Record<string, number> {
    return {};
  }
}`;

const ts_highlight_style = HighlightStyle.define([
    { tag: tags.keyword, color: '#c792ea', fontWeight: 'bold' },
    { tag: tags.controlKeyword, color: '#e394dc', fontWeight: 'bold' },
    { tag: tags.moduleKeyword, color: '#ff9cac', fontWeight: 'bold' },
    { tag: tags.number, color: '#f78c6c' },
    { tag: tags.bool, color: '#ff5370' },
    { tag: tags.null, color: '#c17e70' },
    { tag: tags.function(tags.variableName), color: '#82aaff' },
    { tag: tags.function(tags.propertyName), color: '#61afef' },
    { tag: tags.variableName, color: '#eeffff' },
    { tag: tags.propertyName, color: '#9ccc65' },
    { tag: tags.definition(tags.variableName), color: '#f07178' },
    { tag: tags.typeName, color: '#ffcb6b' },
    { tag: tags.className, color: '#4dd0e1' },
    { tag: tags.namespace, color: '#b2ccd6' },
    { tag: tags.operator, color: '#89ddff' },
    { tag: tags.punctuation, color: '#8b93af' },
    { tag: tags.bracket, color: '#d4d7e0' },
    { tag: tags.string, color: '#c3e88d' },
    { tag: tags.special(tags.string), color: '#e6a86e' },
    { tag: tags.comment, color: '#546e7a', fontStyle: 'italic' },
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
        '.cm-tooltip': {
            background: '#1e2030',
            border: '1px solid #2a2d3e',
            color: '#cdd6f4',
            fontFamily: 'monospace',
            fontSize: '0.7rem',
        },
        '.cm-tooltip-below': { marginTop: '4px' },
    },
    { dark: true },
);

interface Props {
    path: string;
    initial_value: string;
    on_change: (v: string) => void;
    empty_value_example: string;
}

export default function ConductorCallbackEditor({
    path,
    initial_value,
    on_change,
    empty_value_example,
}: Props) {
    const container_ref = useRef<HTMLDivElement>(null);
    const view_ref = useRef<EditorView | null>(null);
    const initial_value_ref = useRef(initial_value);
    const empty_value_example_ref = useRef(empty_value_example);
    const on_change_ref = useRef(on_change);
    const [ready, set_ready] = useState(false);

    useEffect(() => {
        on_change_ref.current = on_change;
    });

    useEffect(() => {
        let disposed = false;
        get_ts_env().then(() => {
            if (!disposed) set_ready(true);
        });
        return () => {
            disposed = true;
        };
    }, []);

    useEffect(() => {
        if (!ready) return;

        let view: EditorView | null = null;
        let file_created = false;

        get_ts_env().then((env) => {
            if (!container_ref.current) return;

            const file_path = callback_source_file(path);
            const doc_value = initial_value_ref.current || empty_value_example_ref.current;
            env.createFile(file_path, doc_value);
            file_created = true;

            const language_compartment = new Compartment();

            const update_listener = EditorView.updateListener.of((update) => {
                if (!update.docChanged) return;
                on_change_ref.current(update.state.doc.toString());
            });

            const state = EditorState.create({
                doc: doc_value,
                extensions: [
                    language_compartment.of(javascript({ typescript: true })),
                    syntaxHighlighting(ts_highlight_style),
                    tsFacet.of({ path: file_path, env }),
                    tsSync(),
                    tsLinter(),
                    autocompletion({ override: [tsAutocomplete()] }),
                    tsHover(),
                    closeBrackets(),
                    history(),
                    keymap.of([indentWithTab, ...historyKeymap, ...defaultKeymap]),
                    update_listener,
                    editor_theme,
                ],
            });

            view = new EditorView({ state, parent: container_ref.current });
            view_ref.current = view;
        });

        return () => {
            view?.destroy();
            view_ref.current = null;
            if (file_created) {
                get_ts_env().then((env) => env.deleteFile(callback_source_file(path)));
            }
        };
    }, [ready, path]);

    return <div className="app__code-wrapper" ref={container_ref} />;
}
