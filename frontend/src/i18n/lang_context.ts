import { createContext, useCallback, useContext } from 'react';
import { STRINGS, type StringKey } from './strings';

export type Lang = 'en' | 'pt';

export const STORAGE_KEY = 'wasmwasm_lang';

export function detect_default_lang(): Lang {
    if (typeof navigator !== 'undefined' && navigator.language.toLowerCase().startsWith('pt')) {
        return 'pt';
    }
    return 'en';
}

export function load_stored_lang(): Lang {
    try {
        const stored = localStorage.getItem(STORAGE_KEY);
        if (stored === 'en' || stored === 'pt') return stored;
    } catch {
        // localStorage unavailable (private mode, quota, etc) - fall back to detection.
    }
    return detect_default_lang();
}

export interface LangContextValue {
    lang: Lang;
    set_lang: (lang: Lang) => void;
}

export const LangContext = createContext<LangContextValue | null>(null);

export function useLang(): LangContextValue {
    const ctx = useContext(LangContext);
    if (!ctx) throw new Error('useLang must be used within a LangProvider');
    return ctx;
}

export function useT(): (key: StringKey) => string {
    const { lang } = useLang();
    return useCallback((key: StringKey) => STRINGS[key][lang], [lang]);
}
