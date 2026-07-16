import { useCallback, useEffect, useMemo, useRef, useState } from 'react';
import type { ScoreGraph } from '../../audio/conductor';
import { ScoreTracer, type TraceResult } from '../../audio/score_timeline';
import {
    KEY_WIDTH,
    MIN_MIDI,
    MAX_MIDI,
    H_ZOOM_MIN,
    H_ZOOM_MAX,
    V_ZOOM_MIN,
    V_ZOOM_MAX,
    freq_to_midi,
    draw_keyboard,
    draw_grid_and_notes,
} from './score_piano_roll_draw';
import './score_piano_roll.scss';

interface Props {
    graph: ScoreGraph;
    start_node_id: number;
    stop_after_node_id?: number;
    bpm: number;
    on_close: () => void;
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
