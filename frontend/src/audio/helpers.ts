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

export type MidiParams =
    | { type: 'noteon'; channel: number; note: number; velocity: number; data: Uint8Array }
    | { type: 'noteoff'; channel: number; note: number; velocity: number; data: Uint8Array }
    | { type: 'cc'; channel: number; cc: number; value: number; data: Uint8Array }
    | { type: 'pitchbend'; channel: number; value: number; data: Uint8Array }
    | { type: 'raw'; channel: number; data: Uint8Array };

export async function setup_midi() {
    if (midi_access) return;
    midi_access = await navigator.requestMIDIAccess();
    wire_inputs(midi_access);
}

export function add_on_midi_event(f: (params: MidiParams) => Promise<void>): string {
    const id = crypto.randomUUID();
    midi_listeners.set(id, f);
    return id;
}

export function remove_on_midi_event(id: string) {
    midi_listeners.delete(id);
}

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
    setup_midi,
    add_on_midi_event,
    remove_on_midi_event,
};

const midi_listeners = new Map<string, (params: MidiParams) => Promise<void>>();
let midi_access: MIDIAccess | null = null;

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
