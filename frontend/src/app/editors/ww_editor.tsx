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
    get_diagnostics,
    get_tokens,
    get_completions,
    get_hover,
    type LspCompletion,
    type LspModule,
} from '../../lsp/lsp';
import { editor_theme } from './editor_theme';
import './ww_editor.scss';

const TOKEN_CLASS: Record<string, string> = {
    keyword: 'ww-keyword',
    number: 'ww-number',
    function: 'ww-function',
    variable: 'ww-variable',
    operator: 'ww-operator',
    comment: 'ww-comment',
};

const force_highlight = StateEffect.define<void>();

function line_starts(src: string): number[] {
    const s = [0];
    for (let i = 0; i < src.length; i++) if (src[i] === '\n') s.push(i + 1);
    return s;
}

function build_decorations(mod: LspModule, view: EditorView): DecorationSet {
    const src = view.state.doc.toString();
    const tokens = get_tokens(mod, src);
    const diags = get_diagnostics(mod, src);
    const starts = line_starts(src);

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

function make_highlight_plugin(get_module: () => LspModule) {
    return ViewPlugin.fromClass(
        class {
            decorations: DecorationSet;
            constructor(view: EditorView) {
                this.decorations = build_decorations(get_module(), view);
            }
            update(update: ViewUpdate) {
                const forced = update.transactions.some((tr) =>
                    tr.effects.some((e) => e.is(force_highlight)),
                );
                if (update.docChanged || forced) {
                    this.decorations = build_decorations(get_module(), update.view);
                }
            }
        },
        { decorations: (v) => v.decorations },
    );
}

function make_hover_plugin(get_module: () => LspModule) {
    return hoverTooltip((view, pos) => {
        const src = view.state.doc.toString();
        const starts = line_starts(src);
        let line = 0;
        for (let i = 0; i < starts.length; i++) {
            if (starts[i] <= pos) line = i;
            else break;
        }
        const col = pos - starts[line];
        const result = get_hover(get_module(), src, line, col);
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
}

export interface WWEditorHandle {
    set_value(code: string): void;
    refresh(): void;
}

interface Props {
    initial_value: string;
    on_change: (v: string) => void;
    get_module: () => LspModule;
}

const WWEditor = forwardRef<WWEditorHandle, Props>(function WWEditor(
    { initial_value, on_change, get_module },
    ref,
) {
    const container_ref = useRef<HTMLDivElement>(null);
    const view_ref = useRef<EditorView | null>(null);
    const initial_value_ref = useRef(initial_value);
    const on_change_ref = useRef(on_change);
    on_change_ref.current = on_change;
    const get_module_ref = useRef(get_module);
    get_module_ref.current = get_module;

    const wrapper_ref = useRef<HTMLDivElement>(null);
    const [completions, set_completions] = useState<LspCompletion[]>([]);
    const completions_ref = useRef<LspCompletion[]>([]);
    const comp_state_ref = useRef({ from: 0, prefix: '' });
    const [comp_pos, set_comp_pos] = useState({ top: 0, left: 0 });
    const [selected_comp, set_selected_comp] = useState(0);
    const selected_comp_ref = useRef(0);

    function sync_completions(
        items: LspCompletion[],
        state?: { from: number; prefix: string },
        pos?: { top: number; left: number },
    ) {
        completions_ref.current = items;
        set_completions(items);
        if (state) comp_state_ref.current = state;
        if (pos) set_comp_pos(pos);
        if (items.length === 0) {
            selected_comp_ref.current = 0;
            set_selected_comp(0);
        }
    }

    const completion_handlers = useRef({
        apply() {
            const items = completions_ref.current;
            if (items.length === 0) return false;
            const view = view_ref.current!;
            const { from, prefix } = comp_state_ref.current;
            const label = items[selected_comp_ref.current].label;
            const code = view.state.doc.toString();
            const new_code = code.slice(0, from) + label + code.slice(from + prefix.length);
            view.dispatch({
                changes: { from: 0, to: view.state.doc.length, insert: new_code },
                selection: { anchor: from + label.length },
            });
            sync_completions([]);
            return true;
        },
        dismiss() {
            if (completions_ref.current.length === 0) return false;
            sync_completions([]);
            return true;
        },
        select_next() {
            if (completions_ref.current.length === 0) return false;
            const next = Math.min(
                selected_comp_ref.current + 1,
                completions_ref.current.length - 1,
            );
            selected_comp_ref.current = next;
            set_selected_comp(next);
            return true;
        },
        select_prev() {
            if (completions_ref.current.length === 0) return false;
            const prev = Math.max(selected_comp_ref.current - 1, 0);
            selected_comp_ref.current = prev;
            set_selected_comp(prev);
            return true;
        },
    });

    useImperativeHandle(ref, () => ({
        set_value(code: string) {
            const view = view_ref.current;
            if (!view) return;
            view.dispatch({ changes: { from: 0, to: view.state.doc.length, insert: code } });
            sync_completions([]);
        },
        refresh() {
            view_ref.current?.dispatch({ effects: force_highlight.of() });
        },
    }));

    useEffect(() => {
        const h = completion_handlers.current;

        const completion_keymap = Prec.highest(
            keymap.of([
                { key: 'Enter', run: () => h.apply() },
                { key: 'Tab', run: () => h.apply() },
                { key: 'Escape', run: () => h.dismiss() },
                { key: 'ArrowDown', run: () => h.select_next() },
                { key: 'ArrowUp', run: () => h.select_prev() },
            ]),
        );

        const update_listener = EditorView.updateListener.of((update: ViewUpdate) => {
            if (!update.docChanged && !update.selectionSet) return;

            const code = update.state.doc.toString();
            if (update.docChanged) on_change_ref.current(code);

            if (!update.docChanged) {
                sync_completions([]);
                return;
            }

            const offset = update.state.selection.main.head;
            const starts = line_starts(code);
            let line = 0;
            for (let i = 0; i < starts.length; i++) {
                if (starts[i] <= offset) line = i;
                else break;
            }
            const col = offset - starts[line];

            let word_from = offset;
            while (word_from > 0 && /\w/.test(code[word_from])) word_from--;
            const prefix = code.slice(word_from, offset);

            if (prefix.length === 0) {
                sync_completions([]);
                return;
            }

            const filtered = get_completions(get_module_ref.current(), code, line, col).filter(
                (c) => c.label.startsWith(prefix) && c.label !== prefix,
            );

            if (filtered.length > 0) {
                const coords = update.view.coordsAtPos(offset);
                const w_rect = wrapper_ref.current!.getBoundingClientRect();
                const pos = coords
                    ? { top: coords.bottom - w_rect.top + 4, left: coords.left - w_rect.left }
                    : undefined;
                sync_completions(filtered, { from: word_from, prefix }, pos);
            } else {
                sync_completions([]);
            }
        });

        const view = new EditorView({
            state: EditorState.create({
                doc: initial_value_ref.current,
                extensions: [
                    completion_keymap,
                    history(),
                    keymap.of([indentWithTab, ...historyKeymap, ...defaultKeymap]),
                    make_highlight_plugin(() => get_module_ref.current()),
                    make_hover_plugin(() => get_module_ref.current()),
                    update_listener,
                    editor_theme,
                ],
            }),
            parent: container_ref.current!,
        });

        view_ref.current = view;
        return () => view.destroy();
    }, []);

    return (
        <div ref={wrapper_ref} className="app__code-wrapper">
            <div ref={container_ref} className="app__code" />
            {completions.length > 0 && (
                <div
                    className="app__completions"
                    style={{ top: comp_pos.top, left: comp_pos.left }}
                >
                    {completions.slice(0, 8).map((c, i) => (
                        <div
                            key={c.label}
                            className={
                                'app__completion-item' + (i === selected_comp ? ' selected' : '')
                            }
                            onMouseDown={(e) => {
                                e.preventDefault();
                                selected_comp_ref.current = i;
                                completion_handlers.current.apply();
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

export default WWEditor;
