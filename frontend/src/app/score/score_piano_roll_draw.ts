import type { TracedNote } from '../../audio/score_timeline';

export const INSTRUMENT_COLORS = ['#89ddff', '#c3e88d', '#f78c6c', '#ffcb6b', '#f07178', '#c792ea'];

export const KEY_WIDTH = 56;
export const MIN_MIDI = 21;
export const MAX_MIDI = 108;
export const H_ZOOM_MIN = 8;
export const H_ZOOM_MAX = 400;
export const V_ZOOM_MIN = 6;
export const V_ZOOM_MAX = 40;

const BLACK_KEYS = new Set([1, 3, 6, 8, 10]);

export function color_for_instrument(name: string, palette: Map<string, string>): string {
    let color = palette.get(name);
    if (!color) {
        color = INSTRUMENT_COLORS[palette.size % INSTRUMENT_COLORS.length];
        palette.set(name, color);
    }
    return color;
}

export function freq_to_midi(freq: number): number {
    return 69 + 12 * Math.log2(freq / 440);
}

export function is_black_key(midi: number): boolean {
    return BLACK_KEYS.has(((midi % 12) + 12) % 12);
}

export function note_name(midi: number): string {
    const names = ['C', 'C#', 'D', 'D#', 'E', 'F', 'F#', 'G', 'G#', 'A', 'A#', 'B'];
    const octave = Math.floor(midi / 12) - 1;
    return `${names[((midi % 12) + 12) % 12]}${octave}`;
}

export function draw_keyboard(canvas: HTMLCanvasElement, v_zoom: number, scroll_top: number): void {
    const ctx = canvas.getContext('2d');
    if (!ctx) return;

    const dpr = window.devicePixelRatio || 1;
    const width = canvas.clientWidth;
    const height = canvas.clientHeight;
    canvas.width = width * dpr;
    canvas.height = height * dpr;
    ctx.setTransform(dpr, 0, 0, dpr, 0, 0);
    ctx.fillStyle = '#e8e6ef';
    ctx.fillRect(0, 0, KEY_WIDTH, height);

    const total_h = (MAX_MIDI - MIN_MIDI + 1) * v_zoom;
    const first_visible = MAX_MIDI - Math.floor((scroll_top + height) / v_zoom) - 1;
    const last_visible = MAX_MIDI - Math.floor(scroll_top / v_zoom) + 1;

    for (
        let midi = Math.max(MIN_MIDI, first_visible);
        midi <= Math.min(MAX_MIDI, last_visible);
        midi++
    ) {
        const y = total_h - (midi - MIN_MIDI + 1) * v_zoom - scroll_top;
        const black = is_black_key(midi);
        if (black) {
            const inset = v_zoom * 0.1;
            ctx.fillStyle = '#0a0a0d';
            ctx.fillRect(0, y + inset, KEY_WIDTH * 0.62, v_zoom - inset * 2);
            ctx.strokeStyle = '#b8b6c4';
            ctx.beginPath();
            ctx.moveTo(KEY_WIDTH * 0.62, y + v_zoom / 2);
            ctx.lineTo(KEY_WIDTH, y + v_zoom / 2);
            ctx.stroke();
        } else {
            if (!is_black_key(midi + 1)) {
                ctx.strokeStyle = '#b8b6c4';
                ctx.beginPath();
                ctx.moveTo(0, y);
                ctx.lineTo(KEY_WIDTH, y);
                ctx.stroke();
            }
            if (v_zoom >= 12) {
                ctx.fillStyle = '#5a5872';
                ctx.font = '9px monospace';
                ctx.textBaseline = 'middle';
                ctx.fillText(note_name(midi), 4, y + v_zoom / 2);
            }
        }
    }
}

export function draw_grid_and_notes(
    canvas: HTMLCanvasElement,
    events: TracedNote[],
    h_zoom: number,
    v_zoom: number,
    total_beats: number,
): void {
    const ctx = canvas.getContext('2d');
    if (!ctx) return;

    const dpr = window.devicePixelRatio || 1;
    const width = canvas.width / dpr;
    const height = canvas.height / dpr;
    ctx.setTransform(dpr, 0, 0, dpr, 0, 0);

    ctx.fillStyle = '#1a1d2e';
    ctx.fillRect(0, 0, width, height);

    const total_h = (MAX_MIDI - MIN_MIDI + 1) * v_zoom;

    for (let midi = MIN_MIDI; midi <= MAX_MIDI; midi++) {
        if (!is_black_key(midi)) continue;
        const y = total_h - (midi - MIN_MIDI + 1) * v_zoom;
        ctx.fillStyle = 'rgba(0, 0, 0, 0.18)';
        ctx.fillRect(0, y, width, v_zoom);
    }

    ctx.strokeStyle = '#2a2d3e';
    ctx.lineWidth = 1;
    ctx.beginPath();
    for (let beat = 0; beat <= Math.ceil(total_beats); beat++) {
        const x = beat * h_zoom + 0.5;
        ctx.moveTo(x, 0);
        ctx.lineTo(x, height);
    }
    ctx.stroke();

    const palette = new Map<string, string>();
    for (const e of events) {
        if (e.freq <= 0 || !e.instrument) continue;
        const midi = freq_to_midi(e.freq);
        const x = e.start_beats.toNumber() * h_zoom;
        const w = Math.max(2, e.dur_beats.toNumber() * h_zoom - 1);
        const y = total_h - (midi - MIN_MIDI + 1) * v_zoom;
        ctx.fillStyle = color_for_instrument(e.instrument, palette);
        ctx.fillRect(x, y, w, Math.max(2, v_zoom - 1));
    }
}
