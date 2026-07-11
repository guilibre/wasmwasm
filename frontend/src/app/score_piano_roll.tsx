import { useEffect, useMemo, useRef, useState } from 'react';
import type { ScoreGraph } from '../audio/conductor';
import { ScoreTracer, type TraceResult, type TracedNote } from '../audio/score_timeline';
import './score_piano_roll.scss';

interface Props {
    graph: ScoreGraph;
    start_node_id: number;
    bpm: number;
    on_close: () => void;
}

const INSTRUMENT_COLORS = ['#89ddff', '#c3e88d', '#f78c6c', '#ffcb6b', '#f07178', '#c792ea'];

function color_for_instrument(name: string | undefined, palette: Map<string, string>): string {
    if (!name) return '#546e7a';
    let color = palette.get(name);
    if (!color) {
        color = INSTRUMENT_COLORS[palette.size % INSTRUMENT_COLORS.length];
        palette.set(name, color);
    }
    return color;
}

function draw(canvas: HTMLCanvasElement, events: TracedNote[]): void {
    const ctx = canvas.getContext('2d');
    if (!ctx) return;

    const width = canvas.clientWidth;
    const height = canvas.clientHeight;
    const dpr = window.devicePixelRatio || 1;
    canvas.width = width * dpr;
    canvas.height = height * dpr;
    ctx.setTransform(dpr, 0, 0, dpr, 0, 0);
    ctx.fillStyle = '#1a1d2e';
    ctx.fillRect(0, 0, width, height);

    if (events.length === 0) return;

    const padding = 8;
    const max_time = Math.max(...events.map((e) => e.start_seconds + e.dur_seconds), 0.001);
    const pitches = events.filter((e) => e.freq > 0).map((e) => -Math.log2(e.freq));
    const min_pitch = pitches.length ? Math.min(...pitches) : 0;
    const max_pitch = pitches.length ? Math.max(...pitches) : 1;
    const pitch_range = Math.max(max_pitch - min_pitch, 0.001);
    const row_h = 8;
    const palette = new Map<string, string>();

    for (const e of events) {
        const x = padding + (e.start_seconds / max_time) * (width - padding * 2);
        const w = Math.max(2, (e.dur_seconds / max_time) * (width - padding * 2));
        const pitch = e.freq > 0 ? -Math.log2(e.freq) : min_pitch;
        const y = padding + ((pitch - min_pitch) / pitch_range) * (height - padding * 2 - row_h);
        ctx.fillStyle = color_for_instrument(e.instrument, palette);
        ctx.fillRect(x, y, w, row_h);
    }
}

export function ScorePianoRoll({ graph, start_node_id, bpm, on_close }: Props) {
    const tracer = useMemo(
        () => new ScoreTracer(graph, bpm, start_node_id),
        [graph, bpm, start_node_id],
    );
    const result: TraceResult = useMemo(() => tracer.trace(), [tracer]);
    const [cycle_index, set_cycle_index] = useState(0);
    const canvas_ref = useRef<HTMLCanvasElement>(null);

    const cycle = useMemo(
        () =>
            result.kind === 'cyclic'
                ? result.get_cycle(cycle_index)
                : { events: result.events, has_next: false },
        [result, cycle_index],
    );

    useEffect(() => {
        const canvas = canvas_ref.current;
        if (canvas) draw(canvas, cycle.events);
    }, [cycle]);

    return (
        <div className="score-piano-roll">
            <div className="score-piano-roll__header">
                {result.kind === 'cyclic' && (
                    <div className="score-piano-roll__cycle-nav">
                        <button
                            disabled={cycle_index === 0}
                            onClick={() => set_cycle_index((i) => Math.max(0, i - 1))}
                        >
                            ◀
                        </button>
                        <span>ciclo {cycle_index + 1}</span>
                        <button
                            disabled={!cycle.has_next}
                            onClick={() => set_cycle_index((i) => i + 1)}
                        >
                            ▶
                        </button>
                        {result.hit_cap && cycle_index === result.cycle_count - 1 && (
                            <span className="score-piano-roll__warning">
                                limite de simulação atingido
                            </span>
                        )}
                    </div>
                )}
                {result.kind === 'linear' && result.truncated && (
                    <span className="score-piano-roll__warning">
                        truncado (limite de eventos atingido)
                    </span>
                )}
                <button className="score-piano-roll__close" onClick={on_close} title="Fechar">
                    ×
                </button>
            </div>
            <canvas ref={canvas_ref} className="score-piano-roll__canvas" />
        </div>
    );
}
