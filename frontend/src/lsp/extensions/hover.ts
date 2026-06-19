import { hoverTooltip } from '@codemirror/view';
import { getHover } from '../lsp';

export const wasmwasmHover = hoverTooltip((view, pos) => {
    const line = view.state.doc.lineAt(pos);
    const result = getHover(view.state.doc.toString(), line.number, pos - line.from);
    if (!result) return null;

    return {
        pos,
        above: true,
        create() {
            const dom = document.createElement('div');
            dom.className = 'ww-hover-tooltip';
            dom.textContent = result.type;
            return { dom };
        },
    };
});
