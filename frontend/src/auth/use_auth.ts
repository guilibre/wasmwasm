import { useCallback, useEffect, useState } from 'react';
import {
    get_me,
    login as api_login,
    logout as api_logout,
    register as api_register,
} from '../api/auth';
import type { CurrentUser } from '../api/types';

export function useAuth() {
    const [user, set_user] = useState<CurrentUser | null>(null);
    const [is_loading, set_is_loading] = useState(true);

    useEffect(() => {
        get_me()
            .then(set_user)
            .finally(() => set_is_loading(false));
    }, []);

    const login = useCallback(async (username: string, password: string) => {
        const current_user = await api_login(username, password);
        set_user(current_user);
        return current_user;
    }, []);

    const register = useCallback(async (username: string, password: string) => {
        const current_user = await api_register(username, password);
        set_user(current_user);
        return current_user;
    }, []);

    const logout = useCallback(async () => {
        await api_logout();
        set_user(null);
    }, []);

    return { user, is_authenticated: user !== null, is_loading, login, register, logout, set_user };
}
