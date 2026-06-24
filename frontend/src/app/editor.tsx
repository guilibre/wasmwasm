import { forwardRef, useEffect, useImperativeHandle, useRef, useState } from 'react';
import {
    EditorView,
    Decoration,
    ViewPlugin,
    hoverTooltip,
    keymap,
    type DecorationSet,
    type ViewUpdate,
} from '@codemirror/view';
import { EditorState, Prec, RangeSetBuilder, StateEffect } from '@codemirror/state';
import { defaultKeymap, indentWithTab, historyKeymap } from '@codemirror/commands';
import { history } from '@codemirror/commands';
import {
    getDiagnostics,
    getTokens,
    getCompletions,
    getHover,
    type LspCompletion,
} from '../lsp/lsp';

const TOKEN_CLASS: Record<string, string> = {
    keyword: 'ww-keyword',
    number: 'ww-number',
    function: 'ww-function',
    variable: 'ww-variable',
    operator: 'ww-operator',
    comment: 'ww-comment',
};

const forceHighlight = StateEffect.define<void>();

function lineStarts(src: string): number[] {
    const s = [0];
    for (let i = 0; i < src.length; i++) if (src[i] === '\n') s.push(i + 1);
    return s;
}

function buildDecorations(view: EditorView): DecorationSet {
    const src = view.state.doc.toString();
    const tokens = getTokens(src);
    const diags = getDiagnostics(src);
    const starts = lineStarts(src);

    const cls = new Array<string>(src.length).fill('');
    const err = new Array<boolean>(src.length).fill(false);

    for (const t of tokens) {
        if (t.line < 0 || t.line >= starts.length) continue;
        const from = starts[t.line] + t.col;
        const to = Math.min(from + t.len, src.length);
        const c = TOKEN_CLASS[t.type] ?? '';
        for (let i = from; i < to; i++) cls[i] = c;
    }

    for (const d of diags) {
        if (d.line < 0 || d.line >= starts.length) continue;
        const from = starts[d.line] + d.col;
        let to = from;
        while (to < src.length && /\S/.test(src[to]) && src[to] !== '\n') to++;
        for (let i = from; i < to && i < src.length; i++) err[i] = true;
    }

    const builder = new RangeSetBuilder<Decoration>();
    let i = 0;
    while (i < src.length) {
        let j = i + 1;
        while (j < src.length && cls[j] === cls[i] && err[j] === err[i]) j++;
        const c = cls[i],
            e = err[i];
        if (c || e) {
            const classes = [c, e ? 'ww-error' : ''].filter(Boolean).join(' ');
            builder.add(i, j, Decoration.mark({ class: classes }));
        }
        i = j;
    }
    return builder.finish();
}

const highlightPlugin = ViewPlugin.fromClass(
    class {
        decorations: DecorationSet;
        constructor(view: EditorView) {
            this.decorations = buildDecorations(view);
        }
        update(update: ViewUpdate) {
            const forced = update.transactions.some((tr) =>
                tr.effects.some((e) => e.is(forceHighlight)),
            );
            if (update.docChanged || forced) {
                this.decorations = buildDecorations(update.view);
            }
        }
    },
    { decorations: (v) => v.decorations },
);

const hoverPlugin = hoverTooltip((view, pos) => {
    const src = view.state.doc.toString();
    const starts = lineStarts(src);
    let line = 0;
    for (let i = 0; i < starts.length; i++) {
        if (starts[i] <= pos) line = i;
        else break;
    }
    const col = pos - starts[line];
    const result = getHover(src, line, col);
    if (!result) return null;
    return {
        pos,
        end: pos + 1,
        above: true,
        create() {
            const dom = document.createElement('div');
            dom.className = 'app__hover-tooltip';
            dom.textContent = result.type;
            return { dom };
        },
    };
});

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
        '.cm-tooltip': { background: 'transparent', border: 'none' },
    },
    { dark: true },
);

export interface EditorHandle {
    setValue(code: string): void;
    refresh(): void;
}

interface Props {
    initialValue: string;
    onChange: (v: string) => void;
}

const Editor = forwardRef<EditorHandle, Props>(function Editor({ initialValue, onChange }, ref) {
    const containerRef = useRef<HTMLDivElement>(null);
    const viewRef = useRef<EditorView | null>(null);
    const initialValueRef = useRef(initialValue);
    const onChangeRef = useRef(onChange);
    onChangeRef.current = onChange;

    const wrapperRef = useRef<HTMLDivElement>(null);
    const [completions, setCompletions] = useState<LspCompletion[]>([]);
    const completionsRef = useRef<LspCompletion[]>([]);
    const compStateRef = useRef({ from: 0, prefix: '' });
    const [compPos, setCompPos] = useState({ top: 0, left: 0 });
    const [selectedComp, setSelectedComp] = useState(0);
    const selectedCompRef = useRef(0);

    function syncCompletions(
        items: LspCompletion[],
        state?: { from: number; prefix: string },
        pos?: { top: number; left: number },
    ) {
        completionsRef.current = items;
        setCompletions(items);
        if (state) compStateRef.current = state;
        if (pos) setCompPos(pos);
        if (items.length === 0) {
            selectedCompRef.current = 0;
            setSelectedComp(0);
        }
    }

    const completionHandlers = useRef({
        apply() {
            const items = completionsRef.current;
            if (items.length === 0) return false;
            const view = viewRef.current!;
            const { from, prefix } = compStateRef.current;
            const label = items[selectedCompRef.current].label;
            const code = view.state.doc.toString();
            const newCode = code.slice(0, from) + label + code.slice(from + prefix.length);
            view.dispatch({
                changes: { from: 0, to: view.state.doc.length, insert: newCode },
                selection: { anchor: from + label.length },
            });
            syncCompletions([]);
            return true;
        },
        dismiss() {
            if (completionsRef.current.length === 0) return false;
            syncCompletions([]);
            return true;
        },
        selectNext() {
            if (completionsRef.current.length === 0) return false;
            const next = Math.min(selectedCompRef.current + 1, completionsRef.current.length - 1);
            selectedCompRef.current = next;
            setSelectedComp(next);
            return true;
        },
        selectPrev() {
            if (completionsRef.current.length === 0) return false;
            const prev = Math.max(selectedCompRef.current - 1, 0);
            selectedCompRef.current = prev;
            setSelectedComp(prev);
            return true;
        },
    });

    useImperativeHandle(ref, () => ({
        setValue(code: string) {
            const view = viewRef.current;
            if (!view) return;
            view.dispatch({ changes: { from: 0, to: view.state.doc.length, insert: code } });
            syncCompletions([]);
        },
        refresh() {
            viewRef.current?.dispatch({ effects: forceHighlight.of() });
        },
    }));

    useEffect(() => {
        const h = completionHandlers.current;

        const completionKeymap = Prec.highest(
            keymap.of([
                { key: 'Enter', run: () => h.apply() },
                { key: 'Tab', run: () => h.apply() },
                { key: 'Escape', run: () => h.dismiss() },
                { key: 'ArrowDown', run: () => h.selectNext() },
                { key: 'ArrowUp', run: () => h.selectPrev() },
            ]),
        );

        const updateListener = EditorView.updateListener.of((update: ViewUpdate) => {
            if (!update.docChanged && !update.selectionSet) return;

            const code = update.state.doc.toString();
            if (update.docChanged) onChangeRef.current(code);

            if (!update.docChanged) {
                syncCompletions([]);
                return;
            }

            const offset = update.state.selection.main.head;
            const starts = lineStarts(code);
            let line = 0;
            for (let i = 0; i < starts.length; i++) {
                if (starts[i] <= offset) line = i;
                else break;
            }
            const col = offset - starts[line];

            let wordFrom = offset;
            while (wordFrom > 0 && /\w/.test(code[wordFrom])) wordFrom--;
            const prefix = code.slice(wordFrom, offset);

            if (prefix.length === 0) {
                syncCompletions([]);
                return;
            }

            const filtered = getCompletions(code, line, col).filter(
                (c) => c.label.startsWith(prefix) && c.label !== prefix,
            );

            if (filtered.length > 0) {
                const coords = update.view.coordsAtPos(offset);
                const wRect = wrapperRef.current!.getBoundingClientRect();
                const pos = coords
                    ? { top: coords.bottom - wRect.top + 4, left: coords.left - wRect.left }
                    : undefined;
                syncCompletions(filtered, { from: wordFrom, prefix }, pos);
            } else {
                syncCompletions([]);
            }
        });

        const view = new EditorView({
            state: EditorState.create({
                doc: initialValueRef.current,
                extensions: [
                    completionKeymap,
                    history(),
                    keymap.of([indentWithTab, ...historyKeymap, ...defaultKeymap]),
                    highlightPlugin,
                    hoverPlugin,
                    updateListener,
                    editorTheme,
                ],
            }),
            parent: containerRef.current!,
        });

        viewRef.current = view;
        return () => view.destroy();
    }, []);

    return (
        <div ref={wrapperRef} className="app__code-wrapper">
            <div ref={containerRef} className="app__code" />
            {completions.length > 0 && (
                <div className="app__completions" style={{ top: compPos.top, left: compPos.left }}>
                    {completions.slice(0, 8).map((c, i) => (
                        <div
                            key={c.label}
                            className={
                                'app__completion-item' + (i === selectedComp ? ' selected' : '')
                            }
                            onMouseDown={(e) => {
                                e.preventDefault();
                                selectedCompRef.current = i;
                                completionHandlers.current.apply();
                            }}
                        >
                            <span className="app__completion-label">{c.label}</span>
                            <span className="app__completion-detail">{c.detail}</span>
                        </div>
                    ))}
                </div>
            )}
        </div>
    );
});

export default Editor;
