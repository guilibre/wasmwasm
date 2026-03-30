import { hoverTooltip } from "@codemirror/view";
import { getHover } from "../lsp";

export const wasmwasmHover = hoverTooltip((view, pos) => {
  const line = view.state.doc.lineAt(pos);
  const result = getHover(
    view.state.doc.toString(),
    line.number, // 1-based, matches tokenizer
    pos - line.from
  );
  if (!result) return null;

  return {
    pos,
    above: true,
    create() {
      const dom = document.createElement("div");
      dom.className = "ww-hover-tooltip";
      dom.textContent = result.type;
      return { dom };
    },
  };
});
