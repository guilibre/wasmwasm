import { HELPERS } from './helpers';
import type { MidiParams } from './helpers';

const worker_midi_listeners = new Map<string, (params: MidiParams) => Promise<void>>();
let midi_setup_resolve: (() => void) | null = null;
let abort_current: (() => void) | null = null;

function worker_setup_midi(): Promise<void> {
    return new Promise((resolve) => {
        midi_setup_resolve = resolve;
        self.postMessage({ type: 'setup-midi' });
    });
}

function worker_add_on_midi_event(f: (params: MidiParams) => Promise<void>): string {
    const id = crypto.randomUUID();
    worker_midi_listeners.set(id, f);
    return id;
}

function worker_remove_on_midi_event(id: string) {
    worker_midi_listeners.delete(id);
}

const HELPERS_WITH_MIDI = {
    ...HELPERS,
    setup_midi: worker_setup_midi,
    add_on_midi_event: worker_add_on_midi_event,
    remove_on_midi_event: worker_remove_on_midi_event,
};

const async_function = Object.getPrototypeOf(async function () {}).constructor as new (
    ...args: string[]
) => (...args: unknown[]) => Promise<void>;

const EVENT_CAPACITY = 256;
const LOOKAHEAD = 0.1;

interface InstrumentInit {
    name: string;
    code: string;
    param_names: string[];
    event_sab: SharedArrayBuffer;
    instance_id: string;
}

let next_request_id = 0;
const pending_instances = new Map<
    number,
    { resolve: (data: InstrumentInit) => void; reject: (msg: string) => void }
>();

interface SpawnCtxRef {
    current: { task_time: { v: number } } | null;
}

function build_instrument(
    instr: InstrumentInit,
    sample_rate: number,
    get_task_time: () => number,
    real_time: () => number,
    spawn_ctx_ref: SpawnCtxRef,
    abort_promise: Promise<void>,
    is_aborted: () => boolean,
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
    let cursor: number | null = null;

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

    const set_param = (name: string, value: number) => {
        const t = active_ctx?.v ?? get_task_time();
        write_events(name, value, t);
    };

    const ABORT = Symbol();
    const sleep = async (s: number) => {
        if (is_aborted()) throw ABORT;
        const my_ctx = active_ctx;
        if (my_ctx) my_ctx.v += s;
        const target = my_ctx ? my_ctx.v : get_task_time() + s;
        const wait_ms = (target - LOOKAHEAD - real_time()) * 1000;
        if (wait_ms > 0)
            await Promise.race([new Promise<void>((r) => setTimeout(r, wait_ms)), abort_promise]);
        active_ctx = my_ctx;
        if (is_aborted()) throw ABORT;
    };

    const die = () =>
        self.postMessage({ type: 'destroy-instance', instance_id: instr.instance_id });

    const fn_names = [...instr.code.matchAll(/^(?:async\s+)?function\s+(\w+)/gm)].map((m) => m[1]);
    if (fn_names.length === 0) return { kill: die };

    const unique_params = [...new Set(param_names)];
    const setter_names = unique_params.map((p) => `set_${p}`);
    const setter_fns = unique_params.map((p) => (value: number) => set_param(p, value));

    const builtins = { set_param, sleep, die };
    const factory = new Function(
        ...Object.keys(builtins),
        ...setter_names,
        ...Object.keys(HELPERS),
        `${instr.code}\nreturn { ${fn_names.join(', ')} };`,
    );
    const raw_fns = factory(
        ...Object.values(builtins),
        ...setter_fns,
        ...Object.values(HELPERS),
    ) as Record<string, (...args: unknown[]) => Promise<void>>;

    const wrapped: Record<string, (...args: unknown[]) => unknown> = {};
    for (const [name, fn] of Object.entries(raw_fns)) {
        wrapped[name] = async (...args: unknown[]) => {
            const my_spawn_ctx = spawn_ctx_ref.current;
            const prev = active_ctx;
            const start = Math.max(cursor ?? get_task_time(), get_task_time());
            active_ctx = { v: start };
            cursor = start;
            try {
                return await fn(...args);
            } catch (e) {
                if (e !== ABORT) throw e;
            } finally {
                const end_v = active_ctx?.v;
                if (end_v != null) {
                    if (cursor == null || end_v > cursor) cursor = end_v;
                    if (my_spawn_ctx && end_v > my_spawn_ctx.task_time.v)
                        my_spawn_ctx.task_time.v = end_v;
                }
                spawn_ctx_ref.current = my_spawn_ctx;
                active_ctx = prev;
            }
        };
    }
    return { ...wrapped, kill: die };
}

self.onmessage = async (event: MessageEvent) => {
    const { type } = event.data as { type: string };

    if (type === 'stop') {
        abort_current?.();
        abort_current = null;
        for (const { reject } of pending_instances.values()) reject('stopped');
        pending_instances.clear();
        worker_midi_listeners.clear();
        midi_setup_resolve = null;
        return;
    }

    if (type === 'instance-ready') {
        const { request_id, ...data } = event.data as { request_id: number } & InstrumentInit;
        pending_instances.get(request_id)?.resolve(data);
        pending_instances.delete(request_id);
        return;
    }

    if (type === 'instance-error') {
        const { request_id, message } = event.data as { request_id: number; message: string };
        pending_instances.get(request_id)?.reject(message);
        pending_instances.delete(request_id);
        return;
    }

    if (type === 'midi-ready') {
        midi_setup_resolve?.();
        midi_setup_resolve = null;
        return;
    }

    if (type === 'midi-event') {
        const { params } = event.data as { params: MidiParams };
        for (const f of worker_midi_listeners.values()) f(params);
        return;
    }

    if (type !== 'run') return;

    abort_current?.();
    let aborted = false;
    let abort_resolve!: () => void;
    const abort_promise = new Promise<void>((r) => {
        abort_resolve = r;
    });
    abort_current = () => {
        aborted = true;
        abort_resolve();
    };
    const is_aborted = () => aborted;
    const ABORT = Symbol();

    const {
        orchestra_code,
        bpm,
        sampleRate,
        audioCurrentTime,
        instrument_names,
        global_code,
        global_param_names,
        global_event_sab,
    } = event.data as {
        orchestra_code: string;
        bpm: number;
        sampleRate: number;
        audioCurrentTime: number;
        instrument_names: string[];
        global_code?: string;
        global_param_names?: string[];
        global_event_sab?: SharedArrayBuffer;
    };

    const perf_origin = performance.now();
    const real_time = () => audioCurrentTime + (performance.now() - perf_origin) / 1000;

    const orch_scheduled = { v: audioCurrentTime + LOOKAHEAD };
    const get_orch_time = () => Math.max(orch_scheduled.v, real_time() + LOOKAHEAD);

    const spawn_ctx_ref: SpawnCtxRef = { current: null };

    const global_init: InstrumentInit | null = global_event_sab
        ? {
              name: 'global',
              code: global_code ?? '',
              param_names: global_param_names ?? [],
              event_sab: global_event_sab,
              instance_id: '__global__',
          }
        : null;

    const global = () => {
        if (!global_init) return Promise.reject(new Error('No global module available'));
        const ctx = spawn_ctx_ref.current;
        return Promise.resolve(
            build_instrument(
                global_init,
                sampleRate,
                ctx ? () => ctx.task_time.v : get_orch_time,
                real_time,
                spawn_ctx_ref,
                abort_promise,
                is_aborted,
            ),
        );
    };

    const instrument = (name: string) => {
        if (is_aborted()) return Promise.reject(ABORT);
        const ctx = spawn_ctx_ref.current;
        return new Promise<Record<string, (...args: unknown[]) => unknown>>((resolve, reject) => {
            const id = next_request_id++;
            pending_instances.set(id, {
                resolve: (data: InstrumentInit) => {
                    spawn_ctx_ref.current = ctx;
                    resolve(
                        build_instrument(
                            data,
                            sampleRate,
                            ctx ? () => ctx.task_time.v : get_orch_time,
                            real_time,
                            spawn_ctx_ref,
                            abort_promise,
                            is_aborted,
                        ),
                    );
                },
                reject: (msg: string) => reject(new Error(msg)),
            });
            self.postMessage({ type: 'request-instance', name, request_id: id });
        });
    };

    const sleep = async (s: number) => {
        if (is_aborted()) throw ABORT;
        const ctx = spawn_ctx_ref.current;
        if (ctx) {
            ctx.task_time.v += s;
            const wait_ms = (ctx.task_time.v - LOOKAHEAD - real_time()) * 1000;
            if (wait_ms > 0)
                await Promise.race([
                    new Promise<void>((r) => setTimeout(r, wait_ms)),
                    abort_promise,
                ]);
            spawn_ctx_ref.current = ctx;
            if (ctx.task_time.v > orch_scheduled.v) orch_scheduled.v = ctx.task_time.v;
        } else {
            orch_scheduled.v += s;
            const wait_ms = (orch_scheduled.v - LOOKAHEAD - real_time()) * 1000;
            if (wait_ms > 0)
                await Promise.race([
                    new Promise<void>((r) => setTimeout(r, wait_ms)),
                    abort_promise,
                ]);
        }
        if (is_aborted()) throw ABORT;
    };

    const sleep_beats = (beats: number) => sleep((beats * 60) / bpm);
    const from_beats = (beats: number) => (beats * 60) / bpm;
    const to_beats = (seconds: number) => (seconds * bpm) / 60;

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

    const instrument_fns = instrument_names.map((name) => () => instrument(name));

    try {
        const stop = (dur: number = 1) => self.postMessage({ type: 'stop', dur });
        const builtins = {
            instrument,
            global,
            sleep,
            sleep_beats,
            from_beats,
            to_beats,
            on_beat,
            current_time,
            spawn,
            stop,
        };
        const fn = new async_function(
            ...Object.keys(builtins),
            ...instrument_names,
            ...Object.keys(HELPERS_WITH_MIDI),
            orchestra_code,
        );
        await fn(
            ...Object.values(builtins),
            ...instrument_fns,
            ...Object.values(HELPERS_WITH_MIDI),
        );
    } catch (e) {
        if (e !== ABORT) self.postMessage({ type: 'error', message: String(e) });
    }
};
