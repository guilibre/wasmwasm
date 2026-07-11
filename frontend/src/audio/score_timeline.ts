import type { ExprNode, GraphNode, ScoreGraph, TransformEntry } from './conductor';
import { eval_expr, eval_presence } from './conductor';

export interface TracedNote {
    start_seconds: number;
    dur_seconds: number;
    freq: number;
    instrument?: string;
    params: Record<string, number>;
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
    pushInstrument?: string;
}

interface SimToken {
    node_id: number;
    time: number;
    params: Record<string, number>;
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

function to_seconds(dur: number, bpm: number): number {
    return (60 * dur) / bpm;
}

function resolve_state(
    node: GraphNode,
    transform_stack: ActiveTransformFrame[],
    bpm: number,
): { params: Record<string, number>; instrument?: string } {
    let params = node.params ?? {};
    let instrument = node.instrument;
    for (let i = transform_stack.length - 1; i >= 0; i--) {
        const frame = transform_stack[i];
        for (const t of frame.transforms) {
            const seconds_view =
                params.dur === undefined ? params : { ...params, dur: to_seconds(params.dur, bpm) };
            const value = eval_expr(t.expr, seconds_view);
            params = { ...params };
            if (value === null) delete params[t.paramName];
            else params[t.paramName] = value;
        }
        if (frame.pushInstrument !== undefined) instrument = frame.pushInstrument;
    }
    if (params.dur !== undefined) params = { ...params, dur: to_seconds(params.dur, bpm) };
    return { params, instrument };
}

function resolve_freq(params: Record<string, number>, graph: ScoreGraph): number {
    if (params.freq !== undefined) return params.freq;
    if (params.degree === undefined || params.octave === undefined || params.scale === undefined)
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
    private readonly nodes_by_id: Map<number, GraphNode>;

    constructor(graph: ScoreGraph, bpm: number, start_node_id: number) {
        this.graph = graph;
        this.bpm = bpm;
        this.start_node_id = start_node_id;
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

        const spawn_from_fork = (parent: SimToken, branch_node_id: number): SimToken => ({
            node_id: branch_node_id,
            time: parent.time,
            params: {},
            transform_stack: parent.transform_stack,
            is_root: false,
            cycle_index: parent.cycle_index,
            own_visited: new Set(),
        });

        // Walks one token forward through "instant" (zero-duration) graph nodes
        // until it settles at a `state` (returned so the caller can schedule its
        // continuation), a dead end (empty array), or - for the root token only -
        // a detected cycle boundary.
        const walk = (token: SimToken): SimToken[] => {
            const local_visited = new Set<number>();
            for (;;) {
                if (events.length >= MAX_EVENTS) {
                    truncated = true;
                    hit_cap = true;
                    return [];
                }
                if (local_visited.has(token.node_id)) return [];
                local_visited.add(token.node_id);

                const node = this.nodes_by_id.get(token.node_id);
                if (!node) return [];

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

                    const { params, instrument } = resolve_state(
                        node,
                        token.transform_stack,
                        this.bpm,
                    );
                    const dur_seconds = params.dur ?? 0;
                    events.push({
                        start_seconds: token.time,
                        dur_seconds,
                        freq: resolve_freq(params, this.graph),
                        instrument,
                        params,
                        cycle_index: token.cycle_index,
                    });
                    token.params = params;
                    token.node_id = node.id;
                    token.time += dur_seconds;
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
                        resolved.push(...walk(spawn_from_fork(token, branch_id)));
                    return resolved;
                }

                if (node.kind === 'transform_push') {
                    token.transform_stack = [
                        ...token.transform_stack,
                        { transforms: node.transforms ?? [], pushInstrument: node.pushInstrument },
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

                // join
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

        const live: SimToken[] = walk({
            node_id: this.start_node_id,
            time: 0,
            params: {},
            transform_stack: [],
            is_root: true,
            cycle_index: 0,
            own_visited: new Set(),
        });

        let steps = 0;
        while (live.length > 0) {
            if (++steps > MAX_TOKEN_STEPS) {
                truncated = true;
                break;
            }
            let min_i = 0;
            for (let i = 1; i < live.length; i++) if (live[i].time < live[min_i].time) min_i = i;
            const token = live[min_i];
            live.splice(min_i, 1);

            const node = this.nodes_by_id.get(token.node_id);
            if (!node || node.next.length === 0) continue;
            token.node_id = node.next[0];
            live.push(...walk(token));
        }

        if (!cyclic) {
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
