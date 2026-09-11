export interface AppError {
    message: string;
    line?: number;
    col?: number;
}

function has_number(value: object, key: string): value is Record<string, number> {
    return typeof (value as Record<string, unknown>)[key] === 'number';
}

export function to_app_error(e: unknown): AppError {
    if (e && typeof e === 'object' && 'message' in e) {
        const line = has_number(e, 'line') ? e.line : undefined;
        const col = has_number(e, 'col') ? e.col : undefined;
        return { message: String((e as Error).message), line, col };
    }
    return { message: String(e) };
}
