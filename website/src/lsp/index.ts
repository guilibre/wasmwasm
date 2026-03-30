import type { Extension } from "@codemirror/state";
import { wasmwasmHighlighting } from "./extensions/highlighting";
import { wasmwasmLinter } from "./extensions/diagnostics";
import { wasmwasmCompletions } from "./extensions/completions";
import { wasmwasmHover } from "./extensions/hover";

export function wasmwasmExtensions(): Extension[] {
  return [wasmwasmHighlighting, wasmwasmLinter, wasmwasmCompletions, wasmwasmHover];
}
