export function amp_to_db(amp: number): number {
    return 20 * Math.log10(amp);
}

export function choose<T>(xs: Array<T>): T {
    return xs[Math.floor(Math.random() * xs.length)];
}

export function clamp(x: number, lo: number, hi: number): number {
    return Math.max(lo, Math.min(hi, x));
}

export function cps_to_midi(f: number): number {
    return 69 + 12 * Math.log2(f / 440);
}

export function db_to_amp(db: number): number {
    return 10 ** (db / 20);
}

export function gaussian(mean: number = 0, stddev: number = 1): number {
    let u = 0;
    while (u === 0) u = Math.random();
    let v = 0;
    while (v === 0) v = Math.random();
    const z = Math.sqrt(-2.0 * Math.log(u)) * Math.cos(2.0 * Math.PI * v);
    return z * stddev + mean;
}

export function lerp(a: number, b: number, t: number): number {
    return a + (b - a) * t;
}

export function midi_to_cps(m: number): number {
    return 440 * 2 ** ((m - 69) / 12);
}

export function rand_int(lo: number, hi: number): number {
    return Math.floor(rand(lo, hi + 1));
}

export function rand(lo: number, hi: number): number {
    return lo + Math.random() * (hi - lo);
}

export function scramble<T>(xs: T[]): T[] {
    for (let i = 0; i < xs.length; ++i) {
        const j = rand_int(0, xs.length - 1);
        const tmp = xs[i];
        xs[i] = xs[j];
        xs[j] = tmp;
    }
    return xs;
}

export interface Note {
    id: string;
}

export interface Instrument {
    note_on: (params: Note) => Promise<void>;
    note_off: (params: Note) => Promise<void>;
}

class Queue<T> {
    private readonly items: T[] = [];

    enqueue(item: T): void {
        this.items.push(item);
    }

    dequeue(): T | undefined {
        return this.items.shift();
    }

    peek(): T | undefined {
        return this.items[0];
    }

    has(item: T): boolean {
        return this.items.includes(item);
    }

    delete(item: T): boolean {
        const index = this.items.indexOf(item);
        if (index === -1) return false;
        this.items.splice(index, 1);
        return true;
    }
}

class EnquedBiMap<K, V> {
    private readonly forward = new Map<K, V>();
    private readonly backward = new Map<V, K>();
    private key_order: Queue<K> = new Queue();

    enqueue(k: K, v: V): void {
        const old_value = this.forward.get(k);
        if (old_value !== undefined) this.backward.delete(old_value);

        const old_key = this.backward.get(v);
        if (old_key !== undefined) this.forward.delete(old_key);

        this.forward.set(k, v);
        this.backward.set(v, k);
        this.key_order.enqueue(k);
    }

    dequeue(): V | undefined {
        const k = this.key_order.dequeue();
        return k == undefined ? undefined : this.forward.get(k);
    }

    get_value(key: K): V | undefined {
        return this.forward.get(key);
    }

    get_key(value: V): K | undefined {
        return this.backward.get(value);
    }

    has_key(key: K): boolean {
        return this.forward.has(key);
    }

    has_value(value: V): boolean {
        return this.backward.has(value);
    }

    delete_key(key: K): boolean {
        const value = this.forward.get(key);
        if (value === undefined) return false;

        this.forward.delete(key);
        this.backward.delete(value);
        return true;
    }

    delete_value(v: V): boolean {
        const k = this.backward.get(v);
        if (k === undefined) return false;

        this.backward.delete(v);
        this.forward.delete(k);
        this.key_order.delete(k);
        return true;
    }
}

export class Poly {
    private readonly voices: Array<Instrument>;
    private readonly voice_idx: EnquedBiMap<string, number> = new EnquedBiMap();

    private constructor(voices: Array<Instrument>) {
        this.voices = voices;
    }

    static async new(voice: () => Promise<Instrument>, n_voices: number): Promise<Poly> {
        const voices = await Promise.all(
            Array.from({ length: n_voices }, async () => await voice()),
        );
        return new Poly(voices);
    }

    async note_on(params: Note) {
        const retriggered = this.voice_idx.get_value(params.id);
        if (retriggered != undefined) {
            this.voice_idx.delete_value(retriggered);
            this.voice_idx.enqueue(params.id, retriggered);
            this.voices[retriggered].note_on(params);
            return;
        }

        for (let i = 0; i < this.voices.length; ++i) {
            if (!this.voice_idx.has_value(i)) {
                this.voice_idx.enqueue(params.id, i);
                this.voices[i].note_on(params);
                return;
            }
        }

        const oldest = this.voice_idx.dequeue();
        if (oldest == undefined) return;
        const stolen_id = this.voice_idx.get_key(oldest)!;
        this.voice_idx.enqueue(params.id, oldest);
        this.voices[oldest].note_off({ id: stolen_id });
        this.voices[oldest].note_on(params);
    }

    async note_off(params: Note) {
        const idx = this.voice_idx.get_value(params.id);
        if (idx == undefined) return;
        this.voice_idx.delete_value(idx);
        this.voices[idx].note_off(params);
    }
}

export type MidiParams =
    | { type: 'noteon'; channel: number; note: number; velocity: number; data: Uint8Array }
    | { type: 'noteoff'; channel: number; note: number; velocity: number; data: Uint8Array }
    | { type: 'cc'; channel: number; cc: number; value: number; data: Uint8Array }
    | { type: 'pitchbend'; channel: number; value: number; data: Uint8Array }
    | { type: 'raw'; channel: number; data: Uint8Array };

export function make_listener_registry<T>() {
    const listeners = new Map<string, (params: T) => Promise<void>>();
    return {
        listeners,
        add: (f: (params: T) => Promise<void>): string => {
            const id = crypto.randomUUID();
            listeners.set(id, f);
            return id;
        },
        remove: (id: string) => {
            listeners.delete(id);
        },
    };
}

const midi_registry = make_listener_registry<MidiParams>();
const midi_listeners = midi_registry.listeners;
let midi_access: MIDIAccess | null = null;

export async function setup_midi() {
    if (midi_access) return;
    midi_access = await navigator.requestMIDIAccess();
    wire_inputs(midi_access);
}

export const add_on_midi_event = midi_registry.add;
export const remove_on_midi_event = midi_registry.remove;

export const HELPERS = {
    amp_to_db,
    choose,
    clamp,
    cps_to_midi,
    db_to_amp,
    gaussian,
    lerp,
    midi_to_cps,
    rand_int,
    rand,
    scramble,
    Poly,
};

function dispatch_midi(msg: MIDIMessageEvent) {
    if (!msg.data) return;
    const data = msg.data;
    const status = data[0];
    const channel = status & 0x0f;
    const type_nibble = status & 0xf0;
    let params: MidiParams;
    if (type_nibble === 0x90 && data[2] > 0)
        params = { type: 'noteon', channel, note: data[1], velocity: data[2], data };
    else if (type_nibble === 0x80 || (type_nibble === 0x90 && data[2] === 0))
        params = { type: 'noteoff', channel, note: data[1], velocity: data[2], data };
    else if (type_nibble === 0xb0)
        params = { type: 'cc', channel, cc: data[1], value: data[2], data };
    else if (type_nibble === 0xe0)
        params = { type: 'pitchbend', channel, value: ((data[2] << 7) | data[1]) - 8192, data };
    else params = { type: 'raw', channel, data };

    for (const f of midi_listeners.values()) f(params);
}

function wire_inputs(access: MIDIAccess) {
    for (const input of access.inputs.values()) input.onmidimessage = dispatch_midi;

    access.onstatechange = (e: MIDIConnectionEvent) => {
        if (e.port?.type === 'input') (e.port as MIDIInput).onmidimessage = dispatch_midi;
    };
}
