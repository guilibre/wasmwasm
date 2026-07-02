import { useEffect } from 'react';

export function useUndoRedoShortcuts(undo: () => void, redo: () => void) {
    useEffect(() => {
        const on_key = (e: KeyboardEvent) => {
            if (!e.ctrlKey && !e.metaKey) return;
            if (e.target instanceof HTMLInputElement || e.target instanceof HTMLTextAreaElement)
                return;
            if (e.key === 'z' && !e.shiftKey) {
                e.preventDefault();
                undo();
            }
            if (e.key === 'y' || (e.key === 'z' && e.shiftKey)) {
                e.preventDefault();
                redo();
            }
        };
        window.addEventListener('keydown', on_key);
        return () => window.removeEventListener('keydown', on_key);
    }, [undo, redo]);
}
