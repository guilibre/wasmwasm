import { useState } from 'react';
import { ApiError } from '../api/client';
import type { useAuth } from './use_auth';
import './auth_modal.scss';

interface AuthModalProps {
    auth: ReturnType<typeof useAuth>;
    on_close: () => void;
    on_success: () => void;
}

export function AuthModal({ auth, on_close, on_success }: AuthModalProps) {
    const [mode, set_mode] = useState<'login' | 'register'>('login');
    const [username, set_username] = useState('');
    const [password, set_password] = useState('');
    const [error, set_error] = useState<string | null>(null);
    const [is_submitting, set_is_submitting] = useState(false);

    const submit = async () => {
        set_error(null);
        set_is_submitting(true);
        try {
            if (mode === 'login') {
                await auth.login(username, password);
            } else {
                await auth.register(username, password);
            }
            on_success();
        } catch (e) {
            set_error(e instanceof ApiError ? e.message : 'Falha de conexão com o servidor.');
        } finally {
            set_is_submitting(false);
        }
    };

    return (
        <div className="auth-modal__overlay" onClick={on_close}>
            <div className="auth-modal" onClick={(e) => e.stopPropagation()}>
                <div className="auth-modal__tabs">
                    <button
                        className={mode === 'login' ? 'active' : ''}
                        onClick={() => set_mode('login')}
                    >
                        Login
                    </button>
                    <button
                        className={mode === 'register' ? 'active' : ''}
                        onClick={() => set_mode('register')}
                    >
                        Registro
                    </button>
                    <button className="auth-modal__close" onClick={on_close}>
                        ×
                    </button>
                </div>
                <form
                    onSubmit={(e) => {
                        e.preventDefault();
                        void submit();
                    }}
                >
                    <label>
                        Usuário
                        <input
                            autoFocus
                            value={username}
                            onChange={(e) => set_username(e.target.value)}
                        />
                    </label>
                    <label>
                        Senha
                        <input
                            type="password"
                            value={password}
                            onChange={(e) => set_password(e.target.value)}
                        />
                    </label>
                    {error && <span className="auth-modal__error">{error}</span>}
                    <button type="submit" disabled={is_submitting}>
                        {mode === 'login' ? 'Entrar' : 'Criar conta'}
                    </button>
                </form>
            </div>
        </div>
    );
}
