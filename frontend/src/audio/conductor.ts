export type NodeKind = 'state' | 'fork' | 'join' | 'passthrough';

export interface GraphNode {
    id: number;
    kind: NodeKind;
    params?: Record<string, number>;
    instrument?: string;
    joinArity?: number;
    next: number[];
}

export interface ScoreGraph {
    version: number;
    nodes: GraphNode[];
    entries: number[][];
}

export interface InstrumentExports {
    instantiate: (id: number) => number;
    destroy: (id: number) => number;
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

export type InstrumentCallback = (
    current_token_params: Record<string, number>,
    all_tokens_params: TokenParams[],
    bpm: number,
) => Record<string, number>;

export type GlobalCallback = (
    all_tokens_params: TokenParams[],
    bpm: number,
) => Record<string, number>;

export type InstrumentCallbackMap = Record<string, InstrumentCallback>;

interface Token {
    node_id: number;
    beats_remaining: number;
    instance_id: number;
    instrument_id: string | null;
    params: Record<string, number>;
}

const max_steps_per_tick = 100;

export class Conductor {
    private readonly nodes_by_id: Map<number, GraphNode>;
    private readonly param_index: ParamIndex;
    private readonly exports: InstrumentExportsMap;
    private readonly global_exports: GlobalExports | null;
    private readonly sample_rate: number;
    private readonly instrument_callbacks: InstrumentCallbackMap;
    private readonly global_callback: GlobalCallback | null;

    private bpm: number;
    private next_instance_id = 0;
    private tokens: Token[] = [];
    private join_arrivals: Map<number, number> = new Map();

    constructor(
        graph: ScoreGraph,
        param_index: ParamIndex,
        exports: InstrumentExportsMap,
        sample_rate: number,
        bpm: number,
        global_exports: GlobalExports | null = null,
        instrument_callbacks: InstrumentCallbackMap = {},
        global_callback: GlobalCallback | null = null,
    ) {
        this.nodes_by_id = new Map(graph.nodes.map((n) => [n.id, n]));
        this.param_index = param_index;
        this.exports = exports;
        this.global_exports = global_exports;
        this.sample_rate = sample_rate;
        this.bpm = bpm;
        this.instrument_callbacks = instrument_callbacks;
        this.global_callback = global_callback;

        const initial: Token[] = [];
        for (const machine of graph.entries) {
            for (const node_id of machine) {
                initial.push({
                    node_id,
                    beats_remaining: 0,
                    instance_id: -1,
                    instrument_id: null,
                    params: {},
                });
            }
        }
        this.tokens = this.resolve_ready(initial);
    }

    set_bpm(bpm: number): void {
        this.bpm = bpm;
    }

    tick(num_samples: number): void {
        const beats_this_tick = (num_samples / this.sample_rate) * (this.bpm / 60);

        const tokens = this.tokens;
        let first_due = -1;
        for (let i = 0; i < tokens.length; i++) {
            tokens[i].beats_remaining -= beats_this_tick;
            if (first_due === -1 && tokens[i].beats_remaining <= 0) first_due = i;
        }
        if (first_due === -1) return;

        const ready: Token[] = [];
        let write = first_due;
        for (let i = first_due; i < tokens.length; i++) {
            const token = tokens[i];
            if (token.beats_remaining <= 0) {
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
        for (const token of this.tokens) {
            this.destroy_instance(token);
        }
        this.tokens = [];
        this.join_arrivals.clear();
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
                if (t.beats_remaining <= 0) {
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
        const exports = this.exports[token.instrument_id];
        exports?.destroy(token.instance_id);
        token.instance_id = -1;
        token.instrument_id = null;
    }

    private walk_forward(token: Token): Token[] {
        let node_id = token.node_id;
        for (;;) {
            const node = this.nodes_by_id.get(node_id);
            if (!node) throw new Error(`conductor: unknown node id ${node_id}`);

            if (node.kind === 'state') {
                token.node_id = node.id;
                this.enter_state(token, node);
                this.apply_global_callback();
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
                        beats_remaining: 0,
                        instance_id: -1,
                        instrument_id: null,
                        params: {},
                    };
                    children.push(...this.walk_forward(child));
                }
                return children;
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
        if (!this.global_callback || !this.global_exports) return;
        const index_table = this.param_index['global'] ?? {};
        const result = this.global_callback(this.tokens, this.bpm);
        for (const [name, value] of Object.entries(result)) {
            const index = index_table[name];
            if (index === undefined) continue;
            if (typeof value !== 'number' || Number.isNaN(value)) {
                throw new Error(`conductor: global callback did not return a number for '${name}'`);
            }
            this.global_exports.set_param(index, value);
        }
    }

    private enter_state(token: Token, node: GraphNode): void {
        token.params = node.params ?? {};
        token.beats_remaining = (node.params?.dur ?? 0) + token.beats_remaining;

        if (node.instrument) {
            const id = this.next_instance_id;
            const exports = this.exports[node.instrument];
            if (!exports) {
                throw new Error(`conductor: unknown instrument '${node.instrument}'`);
            }
            const result = exports.instantiate(id);
            if (result < 0) {
                throw new Error(
                    `conductor: failed to instantiate instrument '${node.instrument}' (no free slot)`,
                );
            }
            token.instance_id = id;
            token.instrument_id = node.instrument;

            const index_table = this.param_index[node.instrument] ?? {};
            const callback = this.instrument_callbacks[node.instrument];
            const params = callback ? callback(token.params, this.tokens, this.bpm) : token.params;
            for (const [name, value] of Object.entries(params)) {
                const index = index_table[name];
                if (index === undefined) continue;
                if (typeof value !== 'number' || Number.isNaN(value)) {
                    throw new Error(
                        `conductor: instrument callback for '${node.instrument}' did not return a number for '${name}'`,
                    );
                }
                exports.set_param(id, index, value);
            }
            ++this.next_instance_id;
        }
    }
}
