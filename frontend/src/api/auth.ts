import { api_request } from './client';
import type { CurrentUser } from './types';

export function register(username: string, password: string): Promise<CurrentUser> {
    return api_request<CurrentUser>('/register', {
        method: 'POST',
        body: { username, password },
    });
}

export function login(username: string, password: string): Promise<CurrentUser> {
    return api_request<CurrentUser>('/login', {
        method: 'POST',
        body: { username, password },
    });
}

export function logout(): Promise<void> {
    return api_request<void>('/logout', { method: 'POST' });
}

export async function get_me(): Promise<CurrentUser | null> {
    try {
        return await api_request<CurrentUser>('/me');
    } catch {
        return null;
    }
}
