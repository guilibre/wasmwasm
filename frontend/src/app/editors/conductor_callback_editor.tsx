import { useEffect, useRef, useState } from 'react';
import { EditorView, keymap } from '@codemirror/view';
import { EditorState, Compartment } from '@codemirror/state';
import { defaultKeymap, indentWithTab, historyKeymap, history } from '@codemirror/commands';
import { autocompletion, closeBrackets } from '@codemirror/autocomplete';
import { javascript } from '@codemirror/lang-javascript';
import { syntaxHighlighting } from '@codemirror/language';
import { tsFacet, tsSync, tsLinter, tsAutocomplete, tsHover } from '@valtown/codemirror-ts';
import { get_ts_env, callback_source_file } from './ts_env';
import { editor_theme, ts_highlight_style } from './editor_theme';

export const instrument_callback_example = `class InstrumentHandler implements InstrumentCallbackHandler {
  call(
    p: Record<string, number>,
    ap: TokenParams[],
  ): Record<string, number> {
    const result: Record<string, number> = {};
    for (const k of Object.keys(p))
      if (typeof p[k] === 'number') result[k] = p[k];

    if (
      typeof result['freq'] !== 'number' &&
      typeof p['scale'] === 'number' &&
      typeof p['degree'] === 'number' &&
      typeof p['octave'] === 'number'
    )
      result['freq'] = 440*scales[p['scale']](p['degree'], p['octave']);

    return result;
  }
}`;

export const global_callback_example = `class GlobalHandler implements GlobalCallbackHandler {
  call(ap: TokenParams[]): Record<string, number> {
    return {};
  }
}`;

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
