import { EditorView } from '@codemirror/view';
import { HighlightStyle } from '@codemirror/language';
import { tags } from '@lezer/highlight';

export const ts_highlight_style = HighlightStyle.define([
    { tag: tags.keyword, color: '#c792ea', fontWeight: 'bold' },
    { tag: tags.controlKeyword, color: '#e394dc', fontWeight: 'bold' },
    { tag: tags.moduleKeyword, color: '#ff9cac', fontWeight: 'bold' },
    { tag: tags.number, color: '#f78c6c' },
    { tag: tags.bool, color: '#ff5370' },
    { tag: tags.null, color: '#c17e70' },
    { tag: tags.function(tags.variableName), color: '#82aaff' },
    { tag: tags.function(tags.propertyName), color: '#61afef' },
    { tag: tags.variableName, color: '#eeffff' },
    { tag: tags.propertyName, color: '#9ccc65' },
    { tag: tags.definition(tags.variableName), color: '#f07178' },
    { tag: tags.typeName, color: '#ffcb6b' },
    { tag: tags.className, color: '#4dd0e1' },
    { tag: tags.namespace, color: '#b2ccd6' },
    { tag: tags.operator, color: '#89ddff' },
    { tag: tags.punctuation, color: '#8b93af' },
    { tag: tags.bracket, color: '#d4d7e0' },
    { tag: tags.string, color: '#c3e88d' },
    { tag: tags.special(tags.string), color: '#e6a86e' },
    { tag: tags.comment, color: '#546e7a', fontStyle: 'italic' },
]);

export const editor_theme = EditorView.theme(
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
