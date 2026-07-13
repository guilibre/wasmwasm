import type { ExprNode, GraphNode, ScoreGraph, TransformEntry } from './conductor';
import { eval_expr, eval_presence, SKIP } from './conductor';
import { Rational, to_seconds, wire_to_number, type WireNumber } from './rational';

export interface TracedNote {
    start_seconds: number;
    dur_seconds: number;
    freq: number;
    instrument?: string;
    params: Record<string, number | string>;
}

export interface CycleResult {
    events: TracedNote[];
    has_next: boolean;
}

export type TraceResult =
    | { kind: 'linear'; events: TracedNote[]; truncated: boolean }
    | {
          kind: 'cyclic';
          cycle_count: number;
          hit_cap: boolean;
          get_cycle: (index: number) => CycleResult;
      };

const MAX_EVENTS = 5000;
const MAX_CYCLES = 200;
const MAX_TOKEN_STEPS = 20000;

interface ActiveTransformFrame {
    transforms: TransformEntry[];
}

interface SimToken {
    node_id: number;
    time: Rational;
    params: Record<string, number | string>;
    transform_stack: ActiveTransformFrame[];
    is_root: boolean;
    cycle_index: number;
    own_visited: Set<number>;
}

interface TaggedEvent extends TracedNote {
    cycle_index: number;
}

function strip_cycle_index(e: TaggedEvent): TracedNote {
    return {
        start_seconds: e.start_seconds,
        dur_seconds: e.dur_seconds,
        freq: e.freq,
        instrument: e.instrument,
        params: e.params,
    };
}

function to_seconds_view(
    params: Record<string, WireNumber | string>,
    bpm: number,
): Record<string, number | string> {
    const result: Record<string, number | string> = {};
    for (const [name, value] of Object.entries(params))
        result[name] = typeof value === 'string' ? value : wire_to_number(value);
    if (typeof result.dur === 'number')
        result.dur = to_seconds(Rational.from_wire(result.dur), bpm);
    return result;
}

function resolve_state(
    node: GraphNode,
    transform_stack: ActiveTransformFrame[],
    bpm: number,
): {
    params: Record<string, number | string>;
    dur_beats: Rational;
    instrument?: string;
} {
    let params: Record<string, WireNumber | string> = node.params ?? {};
    for (let i = transform_stack.length - 1; i >= 0; i--) {
        const frame = transform_stack[i];
        for (const t of frame.transforms) {
            const value = eval_expr(t.expr, to_seconds_view(params, bpm));
            if (value === SKIP) continue;
            params = { ...params };
            if (value === null) delete params[t.paramName];
            else params[t.paramName] = value;
        }
    }
    const dur_beats =
        typeof params.dur === 'number' || (params.dur && typeof params.dur === 'object')
            ? Rational.from_wire(params.dur)
            : Rational.ZERO;
    const instrument = typeof params.instrument === 'string' ? params.instrument : undefined;
    return { params: to_seconds_view(params, bpm), dur_beats, instrument };
}

function resolve_freq(params: Record<string, number | string>, graph: ScoreGraph): number {
    if (typeof params.freq === 'number') return params.freq;
    if (
        typeof params.degree !== 'number' ||
        typeof params.octave !== 'number' ||
        typeof params.scale !== 'number'
    )
        return 0;
    const scale = graph.scales[params.scale];
    if (!scale || scale.values.length === 0) return 0;
    const n = scale.values.length;
    const idx = ((params.degree % n) + n) % n;
    return (
        440 *
        scale.values[idx] *
        Math.pow(2, Math.floor(params.degree / n)) *
        Math.pow(2, params.octave)
    );
}

export class ScoreTracer {
    private readonly graph: ScoreGraph;
    private readonly bpm: number;
    private readonly start_node_id: number;
    private readonly stop_after_node_id: number | undefined;
    private readonly nodes_by_id: Map<number, GraphNode>;

    constructor(
        graph: ScoreGraph,
        bpm: number,
        start_node_id: number,
        stop_after_node_id?: number,
    ) {
        this.graph = graph;
        this.bpm = bpm;
        this.start_node_id = start_node_id;
        this.stop_after_node_id = stop_after_node_id;
        this.nodes_by_id = new Map(graph.nodes.map((n) => [n.id, n]));
    }

    trace(): TraceResult {
        const events: TaggedEvent[] = [];
        const join_arrivals = new Map<number, number>();
        const root_visited_in_cycle = new Set<number>();
        let current_cycle_index = 0;
        let cyclic = false;
        let truncated = false;
        let hit_cap = false;
        let reached_stop = false;

        const spawn_from_fork = (parent: SimToken, branch_node_id: number): SimToken => ({
            node_id: branch_node_id,
            time: parent.time,
            params: {},
            transform_stack: parent.transform_stack,
            is_root: false,
            cycle_index: parent.cycle_index,
            own_visited: new Set(),
        });

        const walk = (token: SimToken, local_visited: Set<number>): SimToken[] => {
            for (;;) {
                if (events.length >= MAX_EVENTS) {
                    truncated = true;
                    hit_cap = true;
                    return [];
                }
                const node = this.nodes_by_id.get(token.node_id);
                if (!node) return [];

                if (node.kind !== 'join') {
                    if (local_visited.has(token.node_id)) return [];
                    local_visited.add(token.node_id);
                }

                if (node.kind === 'state') {
                    if (token.is_root) {
                        if (root_visited_in_cycle.has(node.id)) {
                            cyclic = true;
                            current_cycle_index++;
                            root_visited_in_cycle.clear();
                            if (current_cycle_index >= MAX_CYCLES) {
                                hit_cap = true;
                                return [];
                            }
                        }
                        root_visited_in_cycle.add(node.id);
                        token.cycle_index = current_cycle_index;
                    } else {
                        if (token.own_visited.has(node.id)) return [];
                        token.own_visited.add(node.id);
                    }

                    const { params, dur_beats, instrument } = resolve_state(
                        node,
                        token.transform_stack,
                        this.bpm,
                    );
                    const dur_seconds = to_seconds(dur_beats, this.bpm);
                    events.push({
                        start_seconds: to_seconds(token.time, this.bpm),
                        dur_seconds,
                        freq: resolve_freq(params, this.graph),
                        instrument,
                        params,
                        cycle_index: token.cycle_index,
                    });
                    token.params = params;
                    token.node_id = node.id;
                    token.time = token.time.add(dur_beats);

                    if (
                        this.stop_after_node_id !== undefined &&
                        node.id === this.stop_after_node_id
                    ) {
                        reached_stop = true;
                        return [];
                    }
                    return [token];
                }

                if (node.kind === 'passthrough') {
                    if (node.next.length === 0) return [];
                    token.node_id = node.next[0];
                    continue;
                }

                if (node.kind === 'fork') {
                    const resolved: SimToken[] = [];
                    for (const branch_id of node.next)
                        resolved.push(
                            ...walk(spawn_from_fork(token, branch_id), new Set(local_visited)),
                        );
                    return resolved;
                }

                if (node.kind === 'transform_push') {
                    token.transform_stack = [
                        ...token.transform_stack,
                        { transforms: node.transforms ?? [] },
                    ];
                    if (node.next.length === 0) return [];
                    token.node_id = node.next[0];
                    continue;
                }

                if (node.kind === 'transform_pop') {
                    token.transform_stack = token.transform_stack.slice(0, -1);
                    if (node.next.length === 0) return [];
                    token.node_id = node.next[0];
                    continue;
                }

                if (node.kind === 'branch') {
                    const cond = node.cond as ExprNode;
                    const truthy =
                        cond.kind === 'ident'
                            ? eval_presence(cond, token.params)
                            : (eval_expr(cond, token.params) ?? 0) !== 0;
                    token.node_id = node.next[truthy ? 0 : 1];
                    continue;
                }

                const arrived = (join_arrivals.get(node.id) ?? 0) + 1;
                const arity = node.joinArity ?? 1;
                if (arrived < arity) {
                    join_arrivals.set(node.id, arrived);
                    return [];
                }
                join_arrivals.set(node.id, 0);
                if (node.next.length === 0) return [];
                token.node_id = node.next[0];
            }
        };

        const live: SimToken[] = walk(
            {
                node_id: this.start_node_id,
                time: Rational.ZERO,
                params: {},
                transform_stack: [],
                is_root: true,
                cycle_index: 0,
                own_visited: new Set(),
            },
            new Set<number>(),
        );

        const local_visited = new Set<number>();
        let steps = 0;
        while (live.length > 0) {
            if (++steps > MAX_TOKEN_STEPS) {
                truncated = true;
                break;
            }
            let min_i = 0;
            for (let i = 1; i < live.length; i++)
                if (live[i].time.lessThan(live[min_i].time)) min_i = i;
            const token = live[min_i];
            live.splice(min_i, 1);

            const node = this.nodes_by_id.get(token.node_id);
            if (!node || node.next.length === 0) continue;
            token.node_id = node.next[0];
            live.push(...walk(token, local_visited));
        }

        if (!cyclic || reached_stop) {
            return {
                kind: 'linear',
                events: events.map(strip_cycle_index),
                truncated,
            };
        }

        const cycle_count = current_cycle_index + 1;
        return {
            kind: 'cyclic',
            cycle_count,
            hit_cap,
            get_cycle: (index: number): CycleResult => ({
                events: events.filter((e) => e.cycle_index === index).map(strip_cycle_index),
                has_next: index + 1 < cycle_count,
            }),
        };
    }
}
