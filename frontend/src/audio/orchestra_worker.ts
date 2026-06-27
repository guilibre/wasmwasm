const async_function = Object.getPrototypeOf(async function () {}).constructor as new (
    ...args: string[]
) => (...args: unknown[]) => Promise<void>;

const EVENT_CAPACITY = 256;
const LOOKAHEAD = 0.2;

interface InstrumentInit {
    name: string;
    code: string;
    param_names: string[];
    state_sab: SharedArrayBuffer;
    event_sab: SharedArrayBuffer;
}

interface SpawnCtxRef {
    current: { task_time: { v: number } } | null;
}

function build_instrument(
    instr: InstrumentInit,
    bpm: number,
    sample_rate: number,
    get_task_time: () => number,
    real_time: () => number,
    sync_orch_time: (t: number) => void,
    spawn_ctx_ref: SpawnCtxRef,
) {
    const ev_write_head = new Int32Array(instr.event_sab, 0, 1);
    const ev_data = new DataView(instr.event_sab, 8);
    const param_names = instr.param_names;

    const name_to_indices = new Map<string, number[]>();
    for (let i = 0; i < param_names.length; i++) {
        const entry = name_to_indices.get(param_names[i]);
        if (entry) entry.push(i);
        else name_to_indices.set(param_names[i], [i]);
    }

    let active_ctx: { v: number } | null = null;
    let active_token: object | null = null;
    let cursor: number | null = null;
    const ABORT = Symbol();

    const write_events = (name: string, value: number, t: number) => {
        const indices = name_to_indices.get(name);
        if (!indices) return;
        const frame = Math.round(t * sample_rate);
        const wh = Atomics.load(ev_write_head, 0);
        for (let k = 0; k < indices.length; k++) {
            const slot = ((wh + k) % EVENT_CAPACITY) * 16;
            ev_data.setInt32(slot, frame, true);
            ev_data.setInt32(slot + 4, indices[k], true);
            ev_data.setFloat64(slot + 8, value, true);
        }
        if (indices.length > 0) Atomics.store(ev_write_head, 0, (wh + indices.length) >>> 0);
    };

    const set_param = (name: string, value: number, delay = 0) => {
        const t = (active_ctx?.v ?? get_task_time()) + delay;
        write_events(name, value, t);
    };

    const sleep = async (s: number) => {
        const my_token = active_token;
        const my_ctx = active_ctx;
        if (my_ctx) my_ctx.v += s;
        const target = my_ctx ? my_ctx.v : get_task_time() + s;
        const wait_ms = (target - LOOKAHEAD - real_time()) * 1000;
        if (wait_ms > 0) await new Promise<void>((r) => setTimeout(r, wait_ms));
        if (active_token !== my_token) throw ABORT;
        active_ctx = my_ctx;
    };

    const sleep_beats = (beats: number) => sleep((beats * 60) / bpm);

    const fn_names = [...instr.code.matchAll(/^(?:async\s+)?function\s+(\w+)/gm)].map((m) => m[1]);
    if (fn_names.length === 0) return {};

    const factory = new Function(
        'set_param',
        'sleep',
        'sleep_beats',
        `${instr.code}\nreturn { ${fn_names.join(', ')} };`,
    );
    const raw_fns = factory(set_param, sleep, sleep_beats) as Record<
        string,
        (...args: unknown[]) => Promise<void>
    >;

    const wrapped: Record<string, (...args: unknown[]) => Promise<void>> = {};
    for (const [name, fn] of Object.entries(raw_fns)) {
        wrapped[name] = async (...args: unknown[]) => {
            const my_token = {};
            const my_spawn_ctx = spawn_ctx_ref.current;
            active_token = my_token;
            const prev = active_ctx;
            const start = Math.max(cursor ?? get_task_time(), get_task_time());
            active_ctx = { v: start };
            cursor = start;
            try {
                return await fn(...args);
            } catch (e) {
                if (e !== ABORT) throw e;
            } finally {
                spawn_ctx_ref.current = my_spawn_ctx;
                if (active_token === my_token) {
                    cursor = active_ctx?.v ?? cursor;
                    if (active_ctx) sync_orch_time(active_ctx.v);
                    active_token = null;
                }
                active_ctx = prev;
            }
        };
    }
    return wrapped;
}

self.onmessage = async (event: MessageEvent) => {
    if (event.data.type !== 'run') return;

    const { orchestra_code, bpm, sampleRate, audioCurrentTime, instruments } = event.data as {
        orchestra_code: string;
        bpm: number;
        sampleRate: number;
        audioCurrentTime: number;
        instruments: InstrumentInit[];
    };

    const perf_origin = performance.now();
    const real_time = () => audioCurrentTime + (performance.now() - perf_origin) / 1000;

    const orch_scheduled = { v: audioCurrentTime + LOOKAHEAD };
    const get_orch_time = () => orch_scheduled.v;

    const spawn_ctx_ref: SpawnCtxRef = { current: null };

    const instruments_raw = new Map<string, InstrumentInit[]>();
    for (const instr of instruments) {
        const arr = instruments_raw.get(instr.name);
        if (arr) arr.push(instr);
        else instruments_raw.set(instr.name, [instr]);
    }

    const instrument_counters = new Map<string, number>();
    const instrument = (name: string) => {
        const arr = instruments_raw.get(name) ?? [];
        const idx = instrument_counters.get(name) ?? 0;
        instrument_counters.set(name, idx + 1);
        const instr_data = arr[idx];
        if (!instr_data) return {};
        const ctx = spawn_ctx_ref.current;
        return build_instrument(
            instr_data,
            bpm,
            sampleRate,
            ctx ? () => ctx.task_time.v : get_orch_time,
            real_time,
            (t) => {
                if (t > orch_scheduled.v) orch_scheduled.v = t;
            },
            spawn_ctx_ref,
        );
    };

    const sleep = async (s: number) => {
        const ctx = spawn_ctx_ref.current;
        if (ctx) {
            ctx.task_time.v += s;
            const wait_ms = (ctx.task_time.v - LOOKAHEAD - real_time()) * 1000;
            if (wait_ms > 0) await new Promise<void>((r) => setTimeout(r, wait_ms));
            spawn_ctx_ref.current = ctx;
            if (ctx.task_time.v > orch_scheduled.v) orch_scheduled.v = ctx.task_time.v;
        } else {
            orch_scheduled.v += s;
            const wait_ms = (orch_scheduled.v - LOOKAHEAD - real_time()) * 1000;
            if (wait_ms > 0) await new Promise<void>((r) => setTimeout(r, wait_ms));
        }
    };

    const sleep_beats = (beats: number) => sleep((beats * 60) / bpm);

    let on_beat_version = 0;
    const on_beat = (fn: (beat: number) => void) => {
        const my_version = ++on_beat_version;
        const loop = async () => {
            let beat = 0;
            while (on_beat_version === my_version) {
                fn(beat++);
                await sleep_beats(1);
            }
        };
        loop();
    };

    const current_time = real_time;

    const spawn = (fn: () => Promise<void>) => {
        const task_time = { v: orch_scheduled.v };
        spawn_ctx_ref.current = { task_time };
        fn();
        spawn_ctx_ref.current = null;
    };

    const async_fn_names = [...orchestra_code.matchAll(/^[ \t]*async\s+function\s+(\w+)/gm)].map(
        (m) => m[1],
    );
    if (async_fn_names.length > 0) {
        self.postMessage({
            type: 'error',
            message: `Async functions must be defined as: const f = async () => { ... }\nFound: ${async_fn_names.join(', ')}`,
        });
        return;
    }

    try {
        const fn = new async_function(
            'instrument',
            'sleep',
            'sleep_beats',
            'on_beat',
            'current_time',
            'spawn',
            orchestra_code,
        );
        await fn(instrument, sleep, sleep_beats, on_beat, current_time, spawn);
    } catch (e) {
        self.postMessage({ type: 'error', message: String(e) });
    }
};
