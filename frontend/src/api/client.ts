const API_URL = import.meta.env.VITE_API_URL ?? 'http://localhost:8080';

export class ApiError extends Error {
    status: number;

    constructor(status: number, message: string) {
        super(message);
        this.status = status;
    }
}

export async function api_request<T>(
    path: string,
    options: { method?: string; body?: unknown } = {},
): Promise<T> {
    const res = await fetch(`${API_URL}${path}`, {
        method: options.method ?? 'GET',
        credentials: 'include',
        headers: options.body !== undefined ? { 'Content-Type': 'application/json' } : {},
        body: options.body !== undefined ? JSON.stringify(options.body) : undefined,
    });

    if (!res.ok) {
        const message = await res.text().catch(() => res.statusText);
        throw new ApiError(res.status, message || res.statusText);
    }

    if (res.status === 204) return undefined as T;

    const text = await res.text();
    return text ? (JSON.parse(text) as T) : (undefined as T);
}
