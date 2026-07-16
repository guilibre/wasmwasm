import type { GraphNode, ExprNode } from '../../audio/conductor';
import { wire_to_number } from '../../audio/rational';

export function expr_to_string(expr: ExprNode): string {
    switch (expr.kind) {
        case 'number':
            return String(typeof expr.value === 'object' ? wire_to_number(expr.value) : expr.value);
        case 'string':
            return `"${expr.value}"`;
        case 'null':
            return 'null';
        case 'skip':
            return 'skip';
        case 'ident':
            return expr.name;
        case 'ternary':
            return `${expr_to_string(expr.cond)} ? ${expr_to_string(expr.then)} : ${expr_to_string(expr.else)}`;
        case 'binary': {
            const symbols: Record<string, string> = {
                add: '+',
                sub: '-',
                mul: '*',
                div: '/',
                pow: '^',
                eq: '==',
                neq: '!=',
                lt: '<',
                gt: '>',
                lte: '<=',
                gte: '>=',
                and: '&',
                or: '|',
            };
            return `${expr_to_string(expr.lhs)} ${symbols[expr.op]} ${expr_to_string(expr.rhs)}`;
        }
    }
}

export function longest_path_dur(nodes: GraphNode[]): number {
    const in_group = new Set(nodes.map((n) => n.id));
    const by_id = new Map(nodes.map((n) => [n.id, n]));
    const memo = new Map<number, number>();
    const in_progress = new Set<number>();
    const visit = (id: number): number => {
        if (memo.has(id)) return memo.get(id)!;
        if (in_progress.has(id)) return 0;
        in_progress.add(id);
        const node = by_id.get(id)!;
        const dur = node.params?.dur;
        const own_dur = dur === undefined || typeof dur === 'string' ? 0 : wire_to_number(dur);
        let best = 0;
        for (const next_id of node.next) {
            if (!in_group.has(next_id)) continue;
            const child = visit(next_id);
            if (child > best) best = child;
        }
        const result = own_dur + best;
        in_progress.delete(id);
        memo.set(id, result);
        return result;
    };
    const roots = nodes.filter(
        (n) => !nodes.some((other) => other.next.includes(n.id) && in_group.has(other.id)),
    );
    return Math.max(0, ...(roots.length > 0 ? roots : nodes).map((n) => visit(n.id)));
}

export function summarize_sequence(nodes: GraphNode[]): { instrument?: string; total_dur: number } {
    const instruments = new Set(
        nodes.map((n) => n.params?.instrument).filter((i): i is string => !!i),
    );
    const instrument =
        instruments.size === 1 ? [...instruments][0] : instruments.size > 1 ? 'many' : undefined;
    return { instrument, total_dur: longest_path_dur(nodes) };
}
