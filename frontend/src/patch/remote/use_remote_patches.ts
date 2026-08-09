import { useCallback, useState } from 'react';
import {
    create_patch,
    delete_patch,
    get_patch,
    list_patches,
    update_patch,
} from '../../api/patches';
import type { PatchExportData, PatchSummary } from '../../api/types';
import { is_valid_patch_export } from '../store/patch_types';

export interface RemotePatchesController {
    patches: PatchSummary[];
    is_loading: boolean;
    error: string | null;
    current_patch_id: number | null;
    refresh: () => Promise<void>;
    save_new: (title: string, is_public: boolean, data: PatchExportData) => Promise<void>;
    save_existing: (data: PatchExportData) => Promise<void>;
    load: (id: number) => Promise<PatchExportData>;
    remove: (id: number) => Promise<void>;
}

export function useRemotePatches(): RemotePatchesController {
    const [patches, set_patches] = useState<PatchSummary[]>([]);
    const [is_loading, set_is_loading] = useState(false);
    const [error, set_error] = useState<string | null>(null);
    const [current_patch_id, set_current_patch_id] = useState<number | null>(null);

    const refresh = useCallback(async () => {
        set_is_loading(true);
        set_error(null);
        try {
            set_patches(await list_patches());
        } catch (e) {
            set_error(e instanceof Error ? e.message : 'Falha ao listar patches.');
        } finally {
            set_is_loading(false);
        }
    }, []);

    const save_new = useCallback(
        async (title: string, is_public: boolean, data: PatchExportData) => {
            const created = await create_patch({ title, is_public, data });
            set_current_patch_id(created.id);
            await refresh();
        },
        [refresh],
    );

    const save_existing = useCallback(
        async (data: PatchExportData) => {
            if (current_patch_id === null) throw new Error('Nenhum patch carregado.');
            await update_patch(current_patch_id, { data });
        },
        [current_patch_id],
    );

    const load = useCallback(async (id: number) => {
        const detail = await get_patch(id);
        if (!is_valid_patch_export(detail.data)) {
            throw new Error('Dados de patch inválidos recebidos do servidor.');
        }
        set_current_patch_id(id);
        return detail.data;
    }, []);

    const remove = useCallback(
        async (id: number) => {
            await delete_patch(id);
            if (current_patch_id === id) set_current_patch_id(null);
            await refresh();
        },
        [current_patch_id, refresh],
    );

    return {
        patches,
        is_loading,
        error,
        current_patch_id,
        refresh,
        save_new,
        save_existing,
        load,
        remove,
    };
}
