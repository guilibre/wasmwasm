import type { PatchExportData } from '../patch/store/patch_types';

export type { PatchExportData };

export interface PatchSummary {
    id: number;
    title: string;
    is_public: boolean;
}

export interface PatchDetail extends PatchSummary {
    data: PatchExportData;
}

export interface CurrentUser {
    id: number;
    username: string;
}
