import { linter, type Diagnostic } from "@codemirror/lint";
import { getDiagnostics } from "../lsp";

export const wasmwasmLinter = linter(
  (view: import("@codemirror/view").EditorView) => {
    const src = view.state.doc.toString();
    const raw = getDiagnostics(src);
    const result: Diagnostic[] = [];

    for (const d of raw) {
      if (d.line <= 0) continue; // skip errors without a source position
      try {
        const line = view.state.doc.line(d.line);
        const from = line.from + d.col;
        const to = Math.min(from + 1, line.to);
        result.push({ from, to, severity: "error", message: d.msg });
      } catch {
        // line out of range, skip
      }
    }

    return result;
  },
  { delay: 300 }
);
