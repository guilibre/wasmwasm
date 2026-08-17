import { EditorView } from '@codemirror/view';
import { EditorState } from '@codemirror/state';
import { defaultKeymap, indentWithTab, historyKeymap, history } from '@codemirror/commands';

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

export function initScoreEditor({ container, initialValue, onChange }) {
    const update_listener = EditorView.updateListener.of((update) => {
        if (update.docChanged) {
            onChange(update.state.doc.toString());
        }
    });

    return new EditorView({
        state: EditorState.create({
            doc: initialValue,
            extensions: [
                history(),
                keymap.of([indentWithTab, ...historyKeymap, ...defaultKeymap]),
                update_listener,
                editor_theme,
            ],
        }),
        parent: container,
    });
}
