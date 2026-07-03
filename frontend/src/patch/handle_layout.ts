export function handle_fraction(index: number, count: number): number {
    if (count <= 1) return 0;
    return index / (count - 1);
}

export function handle_offset(index: number, count: number): string {
    return `${handle_fraction(index, count) * 100}%`;
}
