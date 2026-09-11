import { useCallback, useMemo, useState, type ReactNode } from 'react';
import { LangContext, load_stored_lang, STORAGE_KEY, type Lang } from './lang_context';

export function LangProvider({ children }: { children: ReactNode }) {
    const [lang, set_lang_state] = useState<Lang>(load_stored_lang);

    const set_lang = useCallback((next: Lang) => {
        set_lang_state(next);
        try {
            localStorage.setItem(STORAGE_KEY, next);
        } catch {
            // localStorage unavailable - language choice just won't persist.
        }
    }, []);

    const value = useMemo(() => ({ lang, set_lang }), [lang, set_lang]);

    return <LangContext.Provider value={value}>{children}</LangContext.Provider>;
}
