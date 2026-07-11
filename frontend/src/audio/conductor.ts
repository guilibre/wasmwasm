export type NodeKind =
    'state' | 'fork' | 'join' | 'passthrough' | 'transform_push' | 'transform_pop' | 'branch';

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

export type ExprNode =
    | { kind: 'number'; value: number }
    | { kind: 'null' }
    | { kind: 'ident'; name: string }
    | { kind: 'ternary'; cond: ExprNode; then: ExprNode; else: ExprNode }
    | { kind: 'binary'; op: BinOp; lhs: ExprNode; rhs: ExprNode };

export interface TransformEntry {
    paramName: string;
    expr: ExprNode;
}

export interface GraphNode {
    id: number;
    kind: NodeKind;
    params?: Record<string, number>;
    instrument?: string;
    joinArity?: number;
    transforms?: TransformEntry[];
    pushInstrument?: string;
    cond?: ExprNode;
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
    params: Record<string, number>;
}

export interface InstrumentCallbackHandler {
    call(p: Record<string, number>, ap: TokenParams[]): Record<string, number>;
}

export interface GlobalCallbackHandler {
    call(ap: TokenParams[]): Record<string, number>;
}

export type InstrumentCallbackMap = Record<string, new () => InstrumentCallbackHandler>;

interface ActiveTransformFrame {
    transforms: TransformEntry[];
    pushInstrument?: string;
}

interface Token {
    node_id: number;
    seconds_remaining: number;
    instance_id: number;
    instrument_id: string | null;
    params: Record<string, number>;
    transform_stack: ActiveTransformFrame[];
}

const max_steps_per_tick = 64;

export function eval_presence(expr: ExprNode, params: Record<string, number>): boolean {
    if (expr.kind !== 'ident')
        throw new Error('conductor: bare identifier expected as presence-test condition');
    return Object.prototype.hasOwnProperty.call(params, expr.name);
}

export function eval_expr(expr: ExprNode, params: Record<string, number>): number | null {
    switch (expr.kind) {
        case 'number':
            return expr.value;
        case 'null':
            return null;
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
            if (lhs === null || rhs === null)
                throw new Error("conductor: 'null' cannot be used as an operand");
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
                case 'and':
                    return lhs !== 0 && rhs !== 0 ? 1 : 0;
                case 'or':
                    return lhs !== 0 || rhs !== 0 ? 1 : 0;
            }
        }
    }
}

export class Conductor {
    private readonly nodes_by_id: Map<number, GraphNode>;
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
    private legato_voices: Map<number, { instance_id: number; instrument_id: string }> = new Map();

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
                    seconds_remaining: 0,
                    instance_id: -1,
                    instrument_id: null,
                    params: {},
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
            tokens[i].seconds_remaining -= seconds_this_tick;
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
        if (node.next.length === 0) return false;
        token.node_id = node.next[0];
        return true;
    }

    private destroy_instance(token: Token): void {
        if (token.instance_id === -1 || token.instrument_id === null) return;
        token.instance_id = -1;
        token.instrument_id = null;
    }

    private walk_forward(token: Token): Token[] {
        let node_id = token.node_id;
        const visited = new Set<number>();
        for (;;) {
            if (visited.has(node_id))
                throw new Error(
                    `conductor: graph cycle with no state node (zero-duration infinite loop at node ${node_id})`,
                );
            visited.add(node_id);

            const node = this.nodes_by_id.get(node_id);
            if (!node) throw new Error(`conductor: unknown node id ${node_id}`);

            if (node.kind === 'state') {
                token.node_id = node.id;
                this.enter_state(token, node);
                return [token];
            }

            if (node.kind === 'passthrough') {
                if (node.next.length === 0) return [];
                node_id = node.next[0];
                continue;
            }

            if (node.kind === 'fork') {
                const children: Token[] = [];
                for (const branch_id of node.next) {
                    const child: Token = {
                        node_id: branch_id,
                        seconds_remaining: 0,
                        instance_id: -1,
                        instrument_id: null,
                        params: {},
                        transform_stack: [...token.transform_stack],
                    };
                    children.push(...this.walk_forward(child));
                }
                return children;
            }

            if (node.kind === 'transform_push') {
                token.transform_stack = [
                    ...token.transform_stack,
                    { transforms: node.transforms ?? [], pushInstrument: node.pushInstrument },
                ];
                if (node.next.length === 0) return [];
                node_id = node.next[0];
                continue;
            }

            if (node.kind === 'transform_pop') {
                token.transform_stack = token.transform_stack.slice(0, -1);
                if (node.next.length === 0) return [];
                node_id = node.next[0];
                continue;
            }

            if (node.kind === 'branch') {
                const cond = node.cond!;
                const truthy =
                    cond.kind === 'ident'
                        ? eval_presence(cond, token.params)
                        : (eval_expr(cond, token.params) ?? 0) !== 0;
                node_id = node.next[truthy ? 0 : 1];
                continue;
            }

            const arrived = (this.join_arrivals.get(node.id) ?? 0) + 1;
            const arity = node.joinArity ?? 1;
            if (arrived < arity) {
                this.join_arrivals.set(node.id, arrived);
                return [];
            }
            this.join_arrivals.set(node.id, 0);
            if (node.next.length === 0) return [];
            node_id = node.next[0];
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

    private to_seconds_params(params: Record<string, number>): Record<string, number> {
        if (params.dur === undefined) return params;
        return { ...params, dur: (60 * params.dur) / this.bpm };
    }

    private enter_state(token: Token, node: GraphNode): void {
        let params = node.params ?? {};
        let instrument = node.instrument;
        for (let i = token.transform_stack.length - 1; i >= 0; i--) {
            const frame = token.transform_stack[i];
            for (const transform of frame.transforms) {
                const value = eval_expr(transform.expr, this.to_seconds_params(params));
                params = { ...params };
                if (value === null) delete params[transform.paramName];
                else params[transform.paramName] = value;
            }
            if (frame.pushInstrument !== undefined) instrument = frame.pushInstrument;
        }
        token.params = this.to_seconds_params(params);
        token.seconds_remaining += token.params.dur ?? 0;

        if (instrument) {
            const exports = this.exports[instrument];
            if (!exports) throw new Error(`conductor: unknown instrument '${instrument}'`);

            const legato_id = token.params.legato_id;
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

            if (legato_id !== undefined) {
                if (token.params.legato === 1) {
                    this.legato_voices.set(legato_id, {
                        instance_id: id,
                        instrument_id: instrument,
                    });
                } else {
                    this.legato_voices.delete(legato_id);
                }
            }
        }

        this.apply_global_callback();
    }
}
