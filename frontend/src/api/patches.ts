import { api_request } from './client';
import type { PatchDetail, PatchExportData, PatchSummary } from './types';

export async function list_patches(): Promise<PatchSummary[]> {
    const res = await api_request<{ patches: PatchSummary[] }>('/patches');
    return res.patches;
}

export function get_patch(id: number): Promise<PatchDetail> {
    return api_request<PatchDetail>(`/patches/${id}`);
}

export function create_patch(payload: {
    title: string;
    is_public: boolean;
    data: PatchExportData;
}): Promise<{ id: number; title: string; is_public: boolean; version: number }> {
    return api_request(`/patches`, {
        method: 'POST',
        body: { ...payload, data: JSON.stringify(payload.data) },
    });
}

export function update_patch(
    id: number,
    payload: { title?: string; data: PatchExportData },
): Promise<{ id: number; version: number }> {
    return api_request(`/patches/${id}`, {
        method: 'PUT',
        body: { ...payload, data: JSON.stringify(payload.data) },
    });
}

export function delete_patch(id: number): Promise<void> {
    return api_request<void>(`/patches/${id}`, { method: 'DELETE' });
}
