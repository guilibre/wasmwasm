import { ViewPlugin, Decoration, type DecorationSet, type ViewUpdate } from "@codemirror/view";
import { RangeSetBuilder } from "@codemirror/state";
import { getTokens } from "../lsp";

const tokenClassMap: Record<string, string> = {
  keyword: "ww-keyword",
  number: "ww-number",
  function: "ww-function",
  variable: "ww-variable",
  operator: "ww-operator",
  comment: "ww-comment",
};

function buildDecorations(src: string, doc: { line: (n: number) => { from: number; to: number } }): DecorationSet {
  const tokens = getTokens(src);
  const builder = new RangeSetBuilder<Decoration>();

  // Sort by position to satisfy RangeSetBuilder ordering requirement
  tokens.sort((a, b) => {
    const aLine = doc.line(a.line);
    const bLine = doc.line(b.line);
    return (aLine.from + a.col) - (bLine.from + b.col);
  });

  for (const token of tokens) {
    const cls = tokenClassMap[token.type];
    if (!cls) continue;
    try {
      const line = doc.line(token.line);
      const from = line.from + token.col;
      const to = from + token.len;
      if (from < to) {
        builder.add(from, to, Decoration.mark({ class: cls }));
      }
    } catch {
      // line out of range, skip
    }
  }

  return builder.finish();
}

export const wasmwasmHighlighting = ViewPlugin.fromClass(
  class {
    decorations: DecorationSet;

    constructor(view: { state: { doc: { toString: () => string; line: (n: number) => { from: number; to: number } } } }) {
      this.decorations = buildDecorations(view.state.doc.toString(), view.state.doc);
    }

    update(update: ViewUpdate) {
      if (update.docChanged) {
        this.decorations = buildDecorations(update.view.state.doc.toString(), update.view.state.doc);
      }
    }
  },
  { decorations: (v) => v.decorations }
);
