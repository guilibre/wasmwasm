import { useEffect, useRef } from 'react';
import { EditorView, keymap } from '@codemirror/view';
import { EditorState } from '@codemirror/state';
import { defaultKeymap, indentWithTab, historyKeymap, history } from '@codemirror/commands';
import { javascript } from '@codemirror/lang-javascript';
import { autocompletion, closeBrackets } from '@codemirror/autocomplete';
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

function build_preamble(paramNames: string[]): string {
    if (paramNames.length === 0) return '';
    const fields = paramNames.map((n) => `  ${n}: number;`).join('\n');
    return [
        `// Available params: ${paramNames.join(', ')}`,
        `// type PatchParams = {\n${fields}\n// }`,
        `// setParam(name, value)  sleep(seconds)  sleepBeats(beats)`,
        `// onBeat(fn)  currentTime()`,
        '',
    ].join('\n');
}

const editorTheme = EditorView.theme(
    {
        '&': { height: '100%', background: 'transparent', color: '#cdd6f4' },
        '.cm-scroller': {
            overflow: 'auto',
            fontFamily: 'monospace',
            fontSize: '0.9rem',
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
}

function TsEditor({ initialValue, onChange }: EditorProps) {
    const containerRef = useRef<HTMLDivElement>(null);
    const viewRef = useRef<EditorView | null>(null);
    const initialValueRef = useRef(initialValue);
    const onChangeRef = useRef(onChange);
    useEffect(() => {
        onChangeRef.current = onChange;
    });

    useEffect(() => {
        const view = new EditorView({
            state: EditorState.create({
                doc: initialValueRef.current,
                extensions: [
                    history(),
                    keymap.of([indentWithTab, ...historyKeymap, ...defaultKeymap]),
                    javascript({ typescript: true }),
                    autocompletion(),
                    closeBrackets(),
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
    }, []);

    return <div ref={containerRef} style={{ height: '100%' }} />;
}

export function InstrumentPanel({
    params,
    code,
    bpm,
    on_code_change,
    on_bpm_change,
    error,
}: Props) {
    const preamble = params ? build_preamble(params.paramNames) : '';

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
            {preamble && <pre className="instrument__preamble">{preamble}</pre>}
            {error && <div className="instrument__error">{error}</div>}
            <div className="instrument__editor">
                <TsEditor
                    key={code === '' ? 'empty' : 'user'}
                    initialValue={code || FALLBACK}
                    onChange={on_code_change}
                />
            </div>
        </div>
    );
}
