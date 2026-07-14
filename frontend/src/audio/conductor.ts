export type NodeKind =
    | 'state'
    | 'fork'
    | 'join'
    | 'passthrough'
    | 'transform_push'
    | 'transform_pop'
    | 'branch'
    | 'signal_emit'
    | 'reverse'
    | 'legato';

export type BinOp =
    | 'add'
    | 'sub'
    | 'mul'
    | 'div'
    | 'mod'
    | 'pow'
    | 'eq'
    | 'neq'
    | 'lt'
    | 'gt'
    | 'lte'
    | 'gte'
    | 'and'
    | 'or';

import { Rational, to_seconds, wire_to_number, type WireNumber } from './rational';

export type ExprNode =
    | { kind: 'number'; value: WireNumber }
    | { kind: 'string'; value: string }
    | { kind: 'null' }
    | { kind: 'skip' }
    | { kind: 'ident'; name: string }
    | { kind: 'ternary'; cond: ExprNode; then: ExprNode; else: ExprNode }
    | { kind: 'binary'; op: BinOp; lhs: ExprNode; rhs: ExprNode };

export const SKIP: unique symbol = Symbol('skip');

export interface TransformEntry {
    paramName: string;
    expr: ExprNode;
}

export interface GraphNode {
    id: number;
    kind: NodeKind;
    params?: Record<string, WireNumber | string>;
    joinArity?: number;
    transforms?: TransformEntry[];
    listenChannel?: string;
    cond?: ExprNode;
    signalId?: string;
    reverseBodyEntryId?: number;
    reverseBodyExitId?: number;
    legatoId?: number;
    next: number[];
}

export interface ScoreScale {
    name: string;
    values: number[];
}

export interface ScoreGraph {
    version: number;
    scales: ScoreScale[];
    nodes: GraphNode[];
    entries: number[][];
    transformPopOfPush: Record<number, number>;
}

export interface InstrumentExports {
    instantiate: (id: number) => number;
    set_param: (id: number, index: number, value: number) => number;
}

export interface GlobalExports {
    set_param: (index: number, value: number) => number;
}

export type ParamIndex = Record<string, Record<string, number>>;
export type InstrumentExportsMap = Record<string, InstrumentExports>;

export interface TokenParams {
    instrument_id: string | null;
    params: Record<string, number | string>;
}

export interface InstrumentCallbackHandler {
    call(p: Record<string, number | string>, ap: TokenParams[]): Record<string, number>;
}

export interface GlobalCallbackHandler {
    call(ap: TokenParams[]): Record<string, number | string>;
}

export type InstrumentCallbackMap = Record<string, new () => InstrumentCallbackHandler>;

interface ActiveTransformFrame {
    transforms: TransformEntry[];
    listenChannel?: string;
}

type Direction = 'forward' | 'backward';

function flip(direction: Direction): Direction {
    return direction === 'forward' ? 'backward' : 'forward';
}

interface SignalMessage {
    params: Record<string, WireNumber | string>;
}

interface Token {
    node_id: number;
    direction: Direction;
    seconds_remaining: number;
    seconds_remaining_error: number;
    instance_id: number;
    instrument_id: string | null;
    params: Record<string, number | string>;
    dur_beats: Rational;
    transform_stack: ActiveTransformFrame[];
    pending_legato_id?: number;
}

function kahan_subtract(value: number, amount: number, error: number): [number, number] {
    const y = amount + error;
    const t = value - y;
    const new_error = t - value + y;
    return [t, new_error];
}

const max_steps_per_tick = 64;

export function eval_presence(expr: ExprNode, params: Record<string, number | string>): boolean {
    if (expr.kind !== 'ident')
        throw new Error('conductor: bare identifier expected as presence-test condition');
    return Object.prototype.hasOwnProperty.call(params, expr.name);
}

export function eval_expr(
    expr: ExprNode,
    params: Record<string, number | string>,
): number | string | null | typeof SKIP {
    switch (expr.kind) {
        case 'number':
            return wire_to_number(expr.value);
        case 'string':
            return expr.value;
        case 'null':
            return null;
        case 'skip':
            return SKIP;
        case 'ident': {
            const value = params[expr.name];
            if (value === undefined)
                throw new Error(`conductor: parameter '${expr.name}' is not defined on this atom`);
            return value;
        }
        case 'ternary': {
            const cond =
                expr.cond.kind === 'ident'
                    ? eval_presence(expr.cond, params)
                    : (eval_expr(expr.cond, params) ?? 0) !== 0;
            return cond ? eval_expr(expr.then, params) : eval_expr(expr.else, params);
        }
        case 'binary': {
            const lhs = eval_expr(expr.lhs, params);
            const rhs = eval_expr(expr.rhs, params);
            if (lhs === SKIP || rhs === SKIP)
                throw new Error("conductor: 'skip' cannot be used as an operand");
            if (lhs === null || rhs === null)
                throw new Error("conductor: 'null' cannot be used as an operand");
            if (typeof lhs === 'number' && typeof rhs === 'number')
                switch (expr.op) {
                    case 'add':
                        return lhs + rhs;
                    case 'sub':
                        return lhs - rhs;
                    case 'mul':
                        return lhs * rhs;
                    case 'div':
                        if (rhs === 0) throw new Error('conductor: division by zero');
                        return lhs / rhs;
                    case 'mod':
                        if (rhs === 0) throw new Error('conductor: division by zero');
                        return lhs % rhs;
                    case 'pow':
                        return Math.pow(lhs, rhs);
                    case 'eq':
                        return lhs === rhs ? 1 : 0;
                    case 'neq':
                        return lhs !== rhs ? 1 : 0;
                    case 'lt':
                        return lhs < rhs ? 1 : 0;
                    case 'gt':
                        return lhs > rhs ? 1 : 0;
                    case 'lte':
                        return lhs <= rhs ? 1 : 0;
                    case 'gte':
                        return lhs >= rhs ? 1 : 0;
                    case 'and': {
                        const lhs_clip = Math.max(0, Math.min(1, lhs));
                        const rhs_clip = Math.max(0, Math.min(1, rhs));
                        const result = lhs_clip * rhs_clip;
                        if (result === 0) return 0;
                        if (result === 1) return 1;
                        return Math.random() < result ? 1 : 0;
                    }
                    case 'or': {
                        const lhs_clip = Math.max(0, Math.min(1, lhs));
                        const rhs_clip = Math.max(0, Math.min(1, rhs));
                        const result = lhs_clip + rhs_clip - lhs_clip * rhs_clip;
                        if (result === 0) return 0;
                        if (result === 1) return 1;
                        return Math.random() < result ? 1 : 0;
                    }
                    default:
                        throw new Error('invalid number operation');
                }
            if (typeof lhs === 'string' && typeof rhs === 'string')
                switch (expr.op) {
                    case 'add':
                        return lhs + rhs;
                    case 'eq':
                        return lhs === rhs ? 1 : 0;
                    case 'neq':
                        return lhs !== rhs ? 1 : 0;
                    default:
                        throw new Error('invalid string operation');
                }

            throw new Error(`conductor: lhs has type ${typeof lhs} but rhs has type ${typeof rhs}`);
        }
    }
}

export class Conductor {
    private readonly nodes_by_id: Map<number, GraphNode>;
    private readonly prev_of: Map<number, number[]>;
    private readonly transform_push_of_pop: Map<number, number>;
    private readonly param_index: ParamIndex;
    private readonly exports: InstrumentExportsMap;
    private readonly global_exports: GlobalExports | null;
    private readonly sample_rate: number;
    private readonly instrument_callback_handlers: Record<string, InstrumentCallbackHandler>;
    private readonly global_callback_handler: GlobalCallbackHandler | null;

    private bpm: number;
    private next_instance_id = 0;
    private tokens: Token[] = [];
    private join_arrivals: Map<number, number> = new Map();
    private legato_voices: Map<number | string, { instance_id: number; instrument_id: string }> =
        new Map();
    private signals: Map<string, SignalMessage> = new Map();

    constructor(
        graph: ScoreGraph,
        param_index: ParamIndex,
        exports: InstrumentExportsMap,
        sample_rate: number,
        bpm: number,
        global_exports: GlobalExports | null = null,
        instrument_callbacks: InstrumentCallbackMap = {},
        global_callback: (new () => GlobalCallbackHandler) | null = null,
    ) {
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
        this.param_index = param_index;
        this.exports = exports;
        this.global_exports = global_exports;
        this.sample_rate = sample_rate;
        this.bpm = bpm;
        this.instrument_callback_handlers = Object.fromEntries(
            Object.entries(instrument_callbacks).map(([instrument_id, ctor]) => [
                instrument_id,
                new ctor(),
            ]),
        );
        this.global_callback_handler = global_callback ? new global_callback() : null;

        const initial: Token[] = [];
        for (const machine of graph.entries) {
            for (const node_id of machine) {
                initial.push({
                    node_id,
                    direction: 'forward',
                    seconds_remaining: 0,
                    seconds_remaining_error: 0,
                    instance_id: -1,
                    instrument_id: null,
                    params: {},
                    dur_beats: Rational.ZERO,
                    transform_stack: [],
                });
            }
        }
        this.tokens = this.resolve_ready(initial);
    }

    set_bpm(bpm: number): void {
        this.bpm = bpm;
    }

    tick(num_samples: number): void {
        const seconds_this_tick = num_samples / this.sample_rate;
        const tokens = this.tokens;
        let first_due = -1;
        for (let i = 0; i < tokens.length; i++) {
            const [next, next_error] = kahan_subtract(
                tokens[i].seconds_remaining,
                seconds_this_tick,
                tokens[i].seconds_remaining_error,
            );
            tokens[i].seconds_remaining = next;
            tokens[i].seconds_remaining_error = next_error;
            if (first_due === -1 && tokens[i].seconds_remaining <= 0) first_due = i;
        }
        if (first_due === -1) return;

        const ready: Token[] = [];
        let write = first_due;
        for (let i = first_due; i < tokens.length; i++) {
            const token = tokens[i];
            if (token.seconds_remaining <= 0) {
                this.destroy_instance(token);
                if (this.advance_past_current_node(token)) ready.push(token);
            } else {
                tokens[write++] = token;
            }
        }
        tokens.length = write;

        for (const token of this.resolve_ready(ready)) tokens.push(token);
    }

    destroy_all(): void {
        for (const token of this.tokens) this.destroy_instance(token);
        this.tokens = [];
        this.join_arrivals.clear();
        this.legato_voices.clear();
    }

    private resolve_ready(initial_queue: Token[]): Token[] {
        const queue = initial_queue;
        const resolved: Token[] = [];
        let steps = 0;
        let read = 0;
        while (read < queue.length) {
            const token = queue[read++];
            if (++steps > max_steps_per_tick) {
                throw new Error('conductor: exceeded max steps per tick (instant loop?)');
            }
            for (const t of this.walk_forward(token)) {
                if (t.seconds_remaining <= 0) {
                    if (this.advance_past_current_node(t)) queue.push(t);
                } else {
                    resolved.push(t);
                }
            }
        }
        return resolved;
    }

    private advance_past_current_node(token: Token): boolean {
        const node = this.nodes_by_id.get(token.node_id);
        if (!node) throw new Error(`conductor: unknown node id ${token.node_id}`);
        const succs =
            token.direction === 'backward' ? (this.prev_of.get(node.id) ?? []) : node.next;
        if (succs.length === 0) return false;
        token.node_id = succs[0];
        return true;
    }

    private destroy_instance(token: Token): void {
        if (token.instance_id === -1 || token.instrument_id === null) return;
        token.instance_id = -1;
        token.instrument_id = null;
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

    private peek_next_legato_id(start_id: number, direction: Direction): number | undefined {
        let node_id = start_id;
        const visited = new Set<number>();
        for (;;) {
            if (visited.has(node_id)) return undefined;
            visited.add(node_id);
            const node = this.nodes_by_id.get(node_id);
            if (!node) return undefined;
            const kind = this.effective_kind(node, direction);
            if (kind === 'legato') return node.legatoId;
            if (kind === 'state' || kind === 'fork' || kind === 'branch') return undefined;
            const succs = this.succs_of(node, direction);
            if (succs.length !== 1) return undefined;
            node_id = succs[0];
        }
    }

    private walk_forward(token: Token): Token[] {
        let node_id = token.node_id;
        let direction = token.direction;
        const visited = new Set<number>();
        for (;;) {
            if (visited.has(node_id))
                throw new Error(
                    `conductor: graph cycle with no state node (zero-duration infinite loop at node ${node_id})`,
                );
            visited.add(node_id);

            const node = this.nodes_by_id.get(node_id);
            if (!node) throw new Error(`conductor: unknown node id ${node_id}`);
            const succs = this.succs_of(node, direction);
            const kind = this.effective_kind(node, direction);

            if (kind === 'state') {
                token.node_id = node.id;
                token.direction = direction;
                this.enter_state(token, node);
                return [token];
            }

            if (node.kind === 'reverse') {
                node_id = node.reverseBodyExitId!;
                direction = flip(direction);
                continue;
            }

            if (kind === 'passthrough') {
                if (succs.length === 0) return [];
                node_id = succs[0];
                continue;
            }

            if (kind === 'legato') {
                token.pending_legato_id = node.legatoId;
                if (succs.length === 0) return [];
                node_id = succs[0];
                continue;
            }

            if (kind === 'fork') {
                const children: Token[] = [];
                for (const branch_id of succs) {
                    const child: Token = {
                        node_id: branch_id,
                        direction,
                        seconds_remaining: 0,
                        seconds_remaining_error: 0,
                        instance_id: -1,
                        instrument_id: null,
                        params: {},
                        dur_beats: Rational.ZERO,
                        transform_stack: [...token.transform_stack],
                    };
                    children.push(...this.walk_forward(child));
                }
                return children;
            }

            if (kind === 'transform_push') {
                const push_node =
                    direction === 'backward'
                        ? this.nodes_by_id.get(this.transform_push_of_pop.get(node.id)!)!
                        : node;
                token.transform_stack = [
                    ...token.transform_stack,
                    {
                        transforms: push_node.transforms ?? [],
                        listenChannel: push_node.listenChannel,
                    },
                ];
                if (succs.length === 0) return [];
                node_id = succs[0];
                continue;
            }

            if (kind === 'signal_emit') {
                this.signals.set(node.signalId!, {
                    params: node.params ?? {},
                });
                if (succs.length === 0) return [];
                node_id = succs[0];
                continue;
            }

            if (kind === 'transform_pop') {
                token.transform_stack = token.transform_stack.slice(0, -1);
                if (succs.length === 0) return [];
                node_id = succs[0];
                continue;
            }

            if (kind === 'branch') {
                const cond = node.cond!;
                const truthy =
                    cond.kind === 'ident'
                        ? eval_presence(cond, token.params)
                        : (eval_expr(cond, token.params) ?? 0) !== 0;
                node_id = succs[truthy ? 0 : 1];
                continue;
            }

            const arrived = (this.join_arrivals.get(node.id) ?? 0) + 1;
            const arity =
                direction === 'backward' && node.kind === 'fork'
                    ? node.next.length
                    : (node.joinArity ?? 1);
            if (arrived < arity) {
                this.join_arrivals.set(node.id, arrived);
                return [];
            }
            this.join_arrivals.set(node.id, 0);
            if (succs.length === 0) return [];
            node_id = succs[0];
        }
    }

    private apply_global_callback(): void {
        if (!this.global_callback_handler || !this.global_exports) return;
        const index_table = this.param_index['global'] ?? {};
        const result = this.global_callback_handler.call(this.tokens);
        for (const [name, value] of Object.entries(result)) {
            const index = index_table[name];
            if (index === undefined) continue;
            if (typeof value !== 'number' || Number.isNaN(value))
                throw new Error(`conductor: global callback did not return a number for '${name}'`);

            this.global_exports.set_param(index, value);
        }
    }

    private seconds_view(
        params: Record<string, WireNumber | string>,
    ): Record<string, number | string> {
        const result: Record<string, number | string> = {};
        for (const [name, value] of Object.entries(params))
            result[name] = typeof value === 'string' ? value : wire_to_number(value);
        if (typeof result.dur === 'number') result.dur = (60 * result.dur) / this.bpm;
        return result;
    }

    private enter_state(token: Token, node: GraphNode): void {
        let params = node.params ?? {};
        const legato_id = token.pending_legato_id;
        token.pending_legato_id = undefined;
        for (let i = token.transform_stack.length - 1; i >= 0; i--) {
            const frame = token.transform_stack[i];
            if (frame.listenChannel !== undefined) {
                const message = this.signals.get(frame.listenChannel);
                if (message) params = { ...params, ...message.params };
            } else {
                for (const transform of frame.transforms) {
                    const value = eval_expr(transform.expr, this.seconds_view(params));
                    if (value === SKIP) continue;
                    params = { ...params };
                    if (value === null) delete params[transform.paramName];
                    else params[transform.paramName] = value;
                }
            }
        }
        token.dur_beats =
            typeof params.dur === 'number' || (params.dur && typeof params.dur === 'object')
                ? Rational.from_wire(params.dur)
                : Rational.ZERO;
        token.params = this.seconds_view(params);
        token.seconds_remaining += to_seconds(token.dur_beats, this.bpm);

        const instrument =
            typeof token.params.instrument === 'string' ? token.params.instrument : null;
        if (instrument) {
            const exports = this.exports[instrument];
            if (!exports) throw new Error(`conductor: unknown instrument '${instrument}'`);

            const existing_voice =
                legato_id !== undefined ? this.legato_voices.get(legato_id) : undefined;
            const reuse_voice =
                existing_voice !== undefined && existing_voice.instrument_id === instrument;

            let id: number;
            if (reuse_voice) {
                id = existing_voice.instance_id;
            } else {
                id = this.next_instance_id;
                const result = exports.instantiate(id);
                if (result < 0)
                    throw new Error(
                        `conductor: failed to instantiate instrument '${instrument}' (no free slot)`,
                    );
                ++this.next_instance_id;
                if (this.next_instance_id >= 128) this.next_instance_id = 0;
            }

            token.instance_id = id;
            token.instrument_id = instrument;

            const succs = this.succs_of(node, token.direction);
            const next_legato_id =
                succs.length === 1
                    ? this.peek_next_legato_id(succs[0], token.direction)
                    : undefined;

            if (next_legato_id !== undefined) {
                this.legato_voices.set(next_legato_id, {
                    instance_id: id,
                    instrument_id: instrument,
                });
                token.params['legato'] = 1;
            } else if (legato_id !== undefined) {
                this.legato_voices.delete(legato_id);
            }

            const index_table = this.param_index[instrument] ?? {};
            const handler = this.instrument_callback_handlers[instrument];
            const params = handler ? handler.call(token.params, this.tokens) : token.params;
            for (const [name, value] of Object.entries(params)) {
                const index = index_table[name];
                if (index === undefined) continue;
                if (typeof value !== 'number' || Number.isNaN(value)) {
                    throw new Error(
                        `conductor: instrument callback for '${instrument}' did not return a number for '${name}'`,
                    );
                }
                exports.set_param(id, index, value);
            }
        }

        this.apply_global_callback();
    }
}
