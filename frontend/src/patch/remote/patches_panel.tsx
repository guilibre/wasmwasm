import { useEffect, useState } from 'react';
import { ApiError } from '../../api/client';
import type { PatchExportData } from '../../api/types';
import { AuthModal } from '../../auth/auth_modal';
import type { useAuth } from '../../auth/use_auth';
import type { RemotePatchesController } from './use_remote_patches';
import './patches_panel.scss';

interface PatchesPanelProps {
    auth: ReturnType<typeof useAuth>;
    remote: RemotePatchesController;
    current_data: PatchExportData;
    on_close: () => void;
    on_loaded: (data: PatchExportData) => void;
}

export function PatchesPanel({
    auth,
    remote,
    current_data,
    on_close,
    on_loaded,
}: PatchesPanelProps) {
    const [auth_dismissed, set_auth_dismissed] = useState(false);
    const [show_save_form, set_show_save_form] = useState(false);
    const [title, set_title] = useState('');
    const [is_public, set_is_public] = useState(false);
    const [action_error, set_action_error] = useState<string | null>(null);
    const show_auth = !auth.is_loading && !auth.is_authenticated && !auth_dismissed;

    useEffect(() => {
        if (auth.is_authenticated) void remote.refresh();
    }, [auth.is_authenticated]); // eslint-disable-line react-hooks/exhaustive-deps

    const run = async (action: () => Promise<void>) => {
        set_action_error(null);
        try {
            await action();
        } catch (e) {
            set_action_error(
                e instanceof ApiError ? e.message : 'Falha de conexão com o servidor.',
            );
        }
    };

    return (
        <div className="patches-panel__overlay" onClick={on_close}>
            <div className="patches-panel" onClick={(e) => e.stopPropagation()}>
                <div className="patches-panel__header">
                    <span>Meus Patches</span>
                    <button onClick={on_close}>×</button>
                </div>

                {action_error && <span className="patches-panel__error">{action_error}</span>}

                {auth.is_authenticated && (
                    <>
                        <div className="patches-panel__save">
                            {!show_save_form ? (
                                <button onClick={() => set_show_save_form(true)}>
                                    Salvar patch atual
                                </button>
                            ) : (
                                <div className="patches-panel__save-form">
                                    <input
                                        placeholder="Título"
                                        value={title}
                                        onChange={(e) => set_title(e.target.value)}
                                    />
                                    <label>
                                        <input
                                            type="checkbox"
                                            checked={is_public}
                                            onChange={(e) => set_is_public(e.target.checked)}
                                        />
                                        Público
                                    </label>
                                    <button
                                        onClick={() =>
                                            run(async () => {
                                                await remote.save_new(
                                                    title,
                                                    is_public,
                                                    current_data,
                                                );
                                                set_show_save_form(false);
                                                set_title('');
                                                set_is_public(false);
                                            })
                                        }
                                    >
                                        Confirmar
                                    </button>
                                    <button onClick={() => set_show_save_form(false)}>
                                        Cancelar
                                    </button>
                                </div>
                            )}
                            {remote.current_patch_id !== null && (
                                <button
                                    onClick={() => run(() => remote.save_existing(current_data))}
                                >
                                    Atualizar patch carregado
                                </button>
                            )}
                        </div>

                        <ul className="patches-panel__list">
                            {remote.patches.map((p) => (
                                <li key={p.id}>
                                    <span>{p.title}</span>
                                    <span className="patches-panel__visibility">
                                        {p.is_public ? 'público' : 'privado'}
                                    </span>
                                    <button
                                        onClick={() =>
                                            run(async () => {
                                                const data = await remote.load(p.id);
                                                on_loaded(data);
                                            })
                                        }
                                    >
                                        Carregar
                                    </button>
                                    <button onClick={() => run(() => remote.remove(p.id))}>
                                        Excluir
                                    </button>
                                </li>
                            ))}
                            {remote.patches.length === 0 && !remote.is_loading && (
                                <li className="patches-panel__empty">Nenhum patch salvo ainda.</li>
                            )}
                        </ul>
                    </>
                )}
            </div>

            {show_auth && (
                <AuthModal
                    auth={auth}
                    on_close={() => {
                        set_auth_dismissed(true);
                        on_close();
                    }}
                    on_success={() => set_auth_dismissed(true)}
                />
            )}
        </div>
    );
}
