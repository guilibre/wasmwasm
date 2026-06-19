import { createContext, useContext } from 'react';

interface PatchActions {
    update_name: (id: string, name: string) => void;
}

export const PatchContext = createContext<PatchActions | null>(null);

export function usePatchActions(): PatchActions {
    const ctx = useContext(PatchContext);
    if (!ctx) throw new Error('usePatchActions must be used inside PatchContext.Provider');
    return ctx;
}
