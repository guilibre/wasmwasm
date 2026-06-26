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

function build_instrument(
    instr: InstrumentInit,
    bpm: number,
    sample_rate: number,
    get_orch_time: () => number,
    real_time: () => number,
    sync_orch_time: (t: number) => void,
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
        const t = (active_ctx?.v ?? get_orch_time()) + delay;
        write_events(name, value, t);
    };

    const sleep = async (s: number) => {
        const my_token = active_token;
        const my_ctx = active_ctx;
        if (my_ctx) my_ctx.v += s;
        const target = my_ctx ? my_ctx.v : get_orch_time() + s;
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
            active_token = my_token;
            const prev = active_ctx;
            active_ctx = { v: get_orch_time() };
            try {
                return await fn(...args);
            } catch (e) {
                if (e !== ABORT) throw e;
            } finally {
                if (active_token === my_token) {
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

    const instruments_map = new Map<string, Record<string, (...args: unknown[]) => unknown>[]>();
    for (const instr of instruments) {
        const built = build_instrument(instr, bpm, sampleRate, get_orch_time, real_time, (t) => {
            if (t > orch_scheduled.v) orch_scheduled.v = t;
        });
        const arr = instruments_map.get(instr.name);
        if (arr) arr.push(built);
        else instruments_map.set(instr.name, [built]);
    }

    const sleep = async (s: number) => {
        orch_scheduled.v += s;
        const wake_at = orch_scheduled.v - LOOKAHEAD;
        const wait_ms = (wake_at - real_time()) * 1000;
        if (wait_ms > 0) await new Promise<void>((r) => setTimeout(r, wait_ms));
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

    const instrument_counters = new Map<string, number>();
    const instrument = (name: string) => {
        const arr = instruments_map.get(name) ?? [];
        const idx = instrument_counters.get(name) ?? 0;
        instrument_counters.set(name, idx + 1);
        return arr[idx] ?? {};
    };

    try {
        const fn = new async_function(
            'instrument',
            'sleep',
            'sleep_beats',
            'on_beat',
            'current_time',
            orchestra_code,
        );
        await fn(instrument, sleep, sleep_beats, on_beat, current_time);
    } catch (e) {
        self.postMessage({ type: 'error', message: String(e) });
    }
};
