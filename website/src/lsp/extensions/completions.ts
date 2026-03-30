import { autocompletion, type CompletionContext, type CompletionResult } from "@codemirror/autocomplete";
import { getCompletions } from "../lsp";

export const wasmwasmCompletions = autocompletion({
  override: [
    (context: CompletionContext): CompletionResult | null => {
      const word = context.matchBefore(/\w*/);
      if (!word || (word.from === word.to && !context.explicit)) return null;

      const line = context.state.doc.lineAt(context.pos);
      const raw = getCompletions(
        context.state.doc.toString(),
        line.number, // 1-based, matches tokenizer
        context.pos - line.from
      );

      if (raw.length === 0) return null;

      return {
        from: word.from,
        options: raw.map((c) => ({
          label: c.label,
          detail: c.detail,
          type: "function",
        })),
      };
    },
  ],
});
