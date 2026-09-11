const raw_templates = import.meta.glob('../../templates/*.ww', {
    query: '?raw',
    import: 'default',
    eager: true,
}) as Record<string, string>;

const TEMPLATES: Record<string, string> = Object.fromEntries(
    Object.entries(raw_templates).map(([path, code]) => [
        path.replace('../../templates/', '').replace('.ww', ''),
        code,
    ]),
);

export function get_default_code(name: string): string {
    return TEMPLATES[name] ?? '';
}
