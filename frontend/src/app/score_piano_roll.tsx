import { useCallback, useEffect, useMemo, useRef, useState } from 'react';
import type { ScoreGraph } from '../audio/conductor';
import { ScoreTracer, type TraceResult, type TracedNote } from '../audio/score_timeline';
import './score_piano_roll.scss';

interface Props {
    graph: ScoreGraph;
    start_node_id: number;
    stop_after_node_id?: number;
    bpm: number;
    on_close: () => void;
}

const INSTRUMENT_COLORS = ['#89ddff', '#c3e88d', '#f78c6c', '#ffcb6b', '#f07178', '#c792ea'];

const KEY_WIDTH = 56;
const MIN_MIDI = 21;
const MAX_MIDI = 108;
const H_ZOOM_MIN = 8;
const H_ZOOM_MAX = 400;
const V_ZOOM_MIN = 6;
const V_ZOOM_MAX = 40;

const BLACK_KEYS = new Set([1, 3, 6, 8, 10]);

function color_for_instrument(name: string, palette: Map<string, string>): string {
    let color = palette.get(name);
    if (!color) {
        color = INSTRUMENT_COLORS[palette.size % INSTRUMENT_COLORS.length];
        palette.set(name, color);
    }
    return color;
}

function freq_to_midi(freq: number): number {
    return 69 + 12 * Math.log2(freq / 440);
}

function is_black_key(midi: number): boolean {
    return BLACK_KEYS.has(((midi % 12) + 12) % 12);
}

function note_name(midi: number): string {
    const names = ['C', 'C#', 'D', 'D#', 'E', 'F', 'F#', 'G', 'G#', 'A', 'A#', 'B'];
    const octave = Math.floor(midi / 12) - 1;
    return `${names[((midi % 12) + 12) % 12]}${octave}`;
}

function draw_keyboard(canvas: HTMLCanvasElement, v_zoom: number, scroll_top: number): void {
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

function draw_grid_and_notes(
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

export function ScorePianoRoll({ graph, start_node_id, stop_after_node_id, bpm, on_close }: Props) {
    const tracer = useMemo(
        () => new ScoreTracer(graph, bpm, start_node_id, stop_after_node_id),
        [graph, bpm, start_node_id, stop_after_node_id],
    );
    const result: TraceResult = useMemo(() => tracer.trace(), [tracer]);

    const events = useMemo(
        () => (result.kind === 'linear' ? result.events : result.get_cycle(0).events),
        [result],
    );

    const total_beats = useMemo(() => {
        let max_beats = 1;
        for (const e of events) {
            const end = e.start_beats.add(e.dur_beats).toNumber();
            if (end > max_beats) max_beats = end;
        }
        return max_beats;
    }, [events]);

    const [h_zoom, set_h_zoom] = useState(40);
    const [v_zoom, set_v_zoom] = useState(14);
    const [scroll_top, set_scroll_top] = useState(0);
    const initialized_ref = useRef(false);

    const keys_ref = useRef<HTMLCanvasElement>(null);
    const grid_ref = useRef<HTMLCanvasElement>(null);
    const scroll_ref = useRef<HTMLDivElement>(null);

    useEffect(() => {
        if (initialized_ref.current) return;
        const container = scroll_ref.current;
        if (!container || events.length === 0) return;
        initialized_ref.current = true;

        const width = container.clientWidth;
        const height = container.clientHeight;
        const fit_h = Math.max(H_ZOOM_MIN, Math.min(H_ZOOM_MAX, width / total_beats));
        set_h_zoom(fit_h);

        const pitches = events.filter((e) => e.freq > 0).map((e) => freq_to_midi(e.freq));
        const min_midi = Math.min(...pitches, 60);
        const max_midi = Math.max(...pitches, 60);
        const padded_range = max_midi - min_midi + 4;
        const fit_v = Math.max(V_ZOOM_MIN, Math.min(V_ZOOM_MAX, height / padded_range));
        set_v_zoom(fit_v);

        requestAnimationFrame(() => {
            const total_h = (MAX_MIDI - MIN_MIDI + 1) * fit_v;
            const center_midi = (min_midi + max_midi) / 2;
            const y = total_h - (center_midi - MIN_MIDI + 1) * fit_v;
            if (container) container.scrollTop = Math.max(0, y - height / 2);
        });
    }, [events, total_beats]);

    const redraw = useCallback(() => {
        const keys = keys_ref.current;
        const grid = grid_ref.current;
        if (keys) draw_keyboard(keys, v_zoom, scroll_top);
        if (grid) draw_grid_and_notes(grid, events, h_zoom, v_zoom, total_beats);
    }, [events, h_zoom, v_zoom, scroll_top, total_beats]);

    useEffect(() => {
        redraw();
    }, [redraw]);

    const pending_anchor_ref = useRef<{
        mouse_x: number;
        mouse_y: number;
        prev_h_zoom: number;
        prev_v_zoom: number;
    } | null>(null);

    useEffect(() => {
        const grid = grid_ref.current;
        const container = scroll_ref.current;
        if (!grid) return;
        const dpr = window.devicePixelRatio || 1;
        grid.width = Math.max(1, total_beats * h_zoom) * dpr;
        grid.height = (MAX_MIDI - MIN_MIDI + 1) * v_zoom * dpr;
        grid.style.width = `${Math.max(1, total_beats * h_zoom)}px`;
        grid.style.height = `${(MAX_MIDI - MIN_MIDI + 1) * v_zoom}px`;

        const anchor = pending_anchor_ref.current;
        if (anchor && container) {
            pending_anchor_ref.current = null;
            const content_x =
                (container.scrollLeft + anchor.mouse_x) * (h_zoom / anchor.prev_h_zoom);
            const content_y =
                (container.scrollTop + anchor.mouse_y) * (v_zoom / anchor.prev_v_zoom);
            container.scrollLeft = content_x - anchor.mouse_x;
            container.scrollTop = content_y - anchor.mouse_y;
            set_scroll_top(container.scrollTop);
        }

        redraw();
    }, [h_zoom, v_zoom, total_beats, redraw]);

    const on_scroll = useCallback(() => {
        const container = scroll_ref.current;
        if (container) set_scroll_top(container.scrollTop);
    }, []);

    const h_zoom_ref = useRef(h_zoom);
    const v_zoom_ref = useRef(v_zoom);
    useEffect(() => {
        h_zoom_ref.current = h_zoom;
        v_zoom_ref.current = v_zoom;
    }, [h_zoom, v_zoom]);

    useEffect(() => {
        const container = scroll_ref.current;
        if (!container) return;
        const handler = (e: WheelEvent) => {
            if (!e.ctrlKey && !e.shiftKey) return;
            e.preventDefault();
            const dir = e.deltaY > 0 ? -1 : 1;
            const factor = dir > 0 ? 1.2 : 1 / 1.2;

            const rect = container.getBoundingClientRect();
            const mouse_x = e.clientX - rect.left;
            const mouse_y = e.clientY - rect.top;

            pending_anchor_ref.current = {
                mouse_x,
                mouse_y,
                prev_h_zoom: h_zoom_ref.current,
                prev_v_zoom: v_zoom_ref.current,
            };

            if (e.shiftKey) {
                set_v_zoom((z) => Math.max(V_ZOOM_MIN, Math.min(V_ZOOM_MAX, z * factor)));
            } else {
                set_h_zoom((z) => Math.max(H_ZOOM_MIN, Math.min(H_ZOOM_MAX, z * factor)));
            }
        };
        container.addEventListener('wheel', handler, { passive: false });
        return () => container.removeEventListener('wheel', handler);
    }, []);

    const on_pointer_down = useCallback((e: React.MouseEvent) => {
        if (e.button !== 1) return;
        e.preventDefault();
        const container = scroll_ref.current;
        if (!container) return;
        const start_x = e.clientX;
        const start_y = e.clientY;
        const start_left = container.scrollLeft;
        const start_top = container.scrollTop;
        const on_move = (ev: MouseEvent) => {
            container.scrollLeft = start_left - (ev.clientX - start_x);
            container.scrollTop = start_top - (ev.clientY - start_y);
        };
        const on_up = () => {
            window.removeEventListener('mousemove', on_move);
            window.removeEventListener('mouseup', on_up);
        };
        window.addEventListener('mousemove', on_move);
        window.addEventListener('mouseup', on_up);
    }, []);

    const on_v_zoom_in = useCallback(() => {
        set_v_zoom((z) => Math.min(V_ZOOM_MAX, z * 1.2));
    }, []);
    const on_v_zoom_out = useCallback(() => {
        set_v_zoom((z) => Math.max(V_ZOOM_MIN, z / 1.2));
    }, []);

    return (
        <div className="score-piano-roll">
            <div className="score-piano-roll__header">
                <button
                    className="score-piano-roll__vzoom"
                    onClick={on_v_zoom_out}
                    title="Zoom vertical -"
                >
                    −
                </button>
                <button
                    className="score-piano-roll__vzoom"
                    onClick={on_v_zoom_in}
                    title="Zoom vertical +"
                >
                    +
                </button>
                <button className="score-piano-roll__close" onClick={on_close} title="Fechar">
                    ×
                </button>
            </div>
            <div className="score-piano-roll__body">
                <canvas
                    ref={keys_ref}
                    className="score-piano-roll__keys"
                    style={{ width: KEY_WIDTH }}
                />
                <div
                    ref={scroll_ref}
                    className="score-piano-roll__grid-scroll"
                    onScroll={on_scroll}
                    onMouseDown={on_pointer_down}
                >
                    <canvas ref={grid_ref} className="score-piano-roll__canvas" />
                </div>
            </div>
        </div>
    );
}
