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
};
