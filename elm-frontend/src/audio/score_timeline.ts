import type { ExprNode, GraphNode, NodeKind, ScoreGraph, TransformEntry } from './conductor';
import { eval_expr, eval_presence, SKIP } from './conductor';
import { Rational, to_seconds, wire_to_number, type WireNumber } from './rational';

export interface TracedNote {
    start_seconds: number;
    dur_seconds: number;
    start_beats: Rational;
    dur_beats: Rational;
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

type Direction = 'forward' | 'backward';

function flip(direction: Direction): Direction {
    return direction === 'forward' ? 'backward' : 'forward';
}

interface ActiveTransformFrame {
    transforms: TransformEntry[];
}

interface RepeatFrame {
    repeat_node_id: number;
    body_entry_id: number;
    body_exit_id: number;
    remaining: number;
}

interface SimToken {
    node_id: number;
    direction: Direction;
    time: Rational;
    params: Record<string, number | string>;
    transform_stack: ActiveTransformFrame[];
    skip_remaining?: number;
    repeat_stack: RepeatFrame[];
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
        start_beats: e.start_beats,
        dur_beats: e.dur_beats,
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
    private readonly prev_of: Map<number, number[]>;
    private readonly transform_push_of_pop: Map<number, number>;

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
        this.prev_of = new Map();
        for (const node of graph.nodes) {
            for (const succ of node.next) {
                const preds = this.prev_of.get(succ);
                if (preds) preds.push(node.id);
                else this.prev_of.set(succ, [node.id]);
            }
        }
        this.transform_push_of_pop = new Map(
            Object.entries(graph.transformPopOfPush).map(([push_id, pop_id]) => [
                pop_id,
                Number(push_id),
            ]),
        );
    }

    private succs_of(node: GraphNode, direction: Direction): number[] {
        return direction === 'backward' ? (this.prev_of.get(node.id) ?? []) : node.next;
    }

    private effective_kind(node: GraphNode, direction: Direction): NodeKind {
        if (direction === 'forward') return node.kind;
        if (node.kind === 'fork') return 'join';
        if (node.kind === 'join') return 'fork';
        if (node.kind === 'transform_push') return 'transform_pop';
        if (node.kind === 'transform_pop') return 'transform_push';
        return node.kind;
    }

    private loop_repeat_if_body_exit(token: SimToken, node_id: number): number | null | undefined {
        const frame = token.repeat_stack[token.repeat_stack.length - 1];
        if (frame === undefined || frame.body_exit_id !== node_id) return undefined;
        token.repeat_stack = token.repeat_stack.slice(0, -1);
        if (frame.remaining > 0) {
            token.repeat_stack = [
                ...token.repeat_stack,
                { ...frame, remaining: frame.remaining - 1 },
            ];
            return frame.body_entry_id;
        }
        const repeat_node = this.nodes_by_id.get(frame.repeat_node_id)!;
        const repeat_succs = this.succs_of(repeat_node, token.direction);
        if (repeat_succs.length > 0) return repeat_succs[0];
        const outer_looped = this.loop_repeat_if_body_exit(token, frame.repeat_node_id);
        return outer_looped === undefined ? null : outer_looped;
    }

    private advance_past_current_node(token: SimToken): boolean {
        const looped = this.loop_repeat_if_body_exit(token, token.node_id);
        if (looped !== undefined) {
            if (looped === null) return false;
            token.node_id = looped;
            return true;
        }
        const node = this.nodes_by_id.get(token.node_id);
        if (!node) return false;
        const succs = this.succs_of(node, token.direction);
        if (succs.length === 0) return false;
        token.node_id = succs[0];
        return true;
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
            direction: parent.direction,
            time: parent.time,
            params: {},
            transform_stack: parent.transform_stack,
            skip_remaining: parent.skip_remaining,
            repeat_stack: [...parent.repeat_stack],
            is_root: false,
            cycle_index: parent.cycle_index,
            own_visited: new Set(),
        });

        const walk = (token: SimToken, initial_local_visited: Set<number>): SimToken[] => {
            let local_visited = initial_local_visited;
            for (;;) {
                if (events.length >= MAX_EVENTS) {
                    truncated = true;
                    hit_cap = true;
                    return [];
                }
                const node = this.nodes_by_id.get(token.node_id);
                if (!node) return [];
                const succs = this.succs_of(node, token.direction);
                const kind = this.effective_kind(node, token.direction);

                if (
                    node.kind !== 'repeat' &&
                    kind !== 'state' &&
                    kind !== 'join' &&
                    kind !== 'fork'
                ) {
                    const looped = this.loop_repeat_if_body_exit(token, token.node_id);
                    if (looped !== undefined) {
                        if (looped === null) return [];
                        token.node_id = looped;
                        local_visited = new Set();
                        continue;
                    }
                }

                if (node.kind === 'repeat') {
                    const body_entry =
                        token.direction === 'backward'
                            ? node.repeatBodyExitId!
                            : node.repeatBodyEntryId!;
                    const body_exit =
                        token.direction === 'backward'
                            ? node.repeatBodyEntryId!
                            : node.repeatBodyExitId!;
                    token.repeat_stack = [
                        ...token.repeat_stack,
                        {
                            repeat_node_id: node.id,
                            body_entry_id: body_entry,
                            body_exit_id: body_exit,
                            remaining: node.repeatCount! - 1,
                        },
                    ];
                    token.node_id = body_entry;
                    local_visited = new Set();
                    continue;
                }

                if (kind !== 'join') {
                    if (local_visited.has(token.node_id)) return [];
                    local_visited.add(token.node_id);
                }

                if (kind === 'state') {
                    if (token.skip_remaining !== undefined && token.skip_remaining > 0) {
                        token.skip_remaining -= 1;
                        if (succs.length === 0) return [];
                        token.node_id = succs[0];
                        continue;
                    }

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
                        start_beats: token.time,
                        dur_beats,
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

                if (node.kind === 'reverse') {
                    token.node_id = node.reverseBodyExitId!;
                    token.direction = flip(token.direction);
                    continue;
                }

                if (kind === 'skip') {
                    token.skip_remaining = (token.skip_remaining ?? 0) + node.skipCount!;
                    if (succs.length === 0) return [];
                    token.node_id = succs[0];
                    continue;
                }

                if (kind === 'passthrough' || kind === 'legato') {
                    if (succs.length === 0) return [];
                    token.node_id = succs[0];
                    continue;
                }

                if (kind === 'fork') {
                    const resolved: SimToken[] = [];
                    for (const branch_id of succs)
                        resolved.push(
                            ...walk(spawn_from_fork(token, branch_id), new Set(local_visited)),
                        );
                    return resolved;
                }

                if (kind === 'transform_push') {
                    const push_node =
                        token.direction === 'backward'
                            ? this.nodes_by_id.get(this.transform_push_of_pop.get(node.id)!)!
                            : node;
                    token.transform_stack = [
                        ...token.transform_stack,
                        { transforms: push_node.transforms ?? [] },
                    ];
                    if (succs.length === 0) return [];
                    token.node_id = succs[0];
                    continue;
                }

                if (kind === 'transform_pop') {
                    token.transform_stack = token.transform_stack.slice(0, -1);
                    if (succs.length === 0) return [];
                    token.node_id = succs[0];
                    continue;
                }

                if (kind === 'branch') {
                    const cond = node.cond as ExprNode;
                    const truthy =
                        cond.kind === 'ident'
                            ? eval_presence(cond, token.params)
                            : (eval_expr(cond, token.params) ?? 0) !== 0;
                    token.node_id = succs[truthy ? 0 : 1];
                    continue;
                }

                const arrived = (join_arrivals.get(node.id) ?? 0) + 1;
                const arity =
                    token.direction === 'backward' && node.kind === 'fork'
                        ? node.next.length
                        : (node.joinArity ?? 1);
                if (arrived < arity) {
                    join_arrivals.set(node.id, arrived);
                    return [];
                }
                join_arrivals.set(node.id, 0);
                const looped = this.loop_repeat_if_body_exit(token, token.node_id);
                if (looped !== undefined) {
                    if (looped === null) return [];
                    token.node_id = looped;
                    local_visited = new Set();
                    continue;
                }
                if (succs.length === 0) return [];
                token.node_id = succs[0];
            }
        };

        const live: SimToken[] = walk(
            {
                node_id: this.start_node_id,
                direction: 'forward',
                time: Rational.ZERO,
                params: {},
                transform_stack: [],
                repeat_stack: [],
                is_root: true,
                cycle_index: 0,
                own_visited: new Set(),
            },
            new Set<number>(),
        );

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

            if (!this.advance_past_current_node(token)) continue;
            live.push(...walk(token, new Set()));
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
