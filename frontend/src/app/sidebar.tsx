import { useRef, useEffect, useState, useCallback } from 'react';
import './sidebar.scss';

interface Props {
    analyser_l: AnalyserNode | null;
    analyser_r: AnalyserNode | null;
}

const MIN_WIDTH = 180;
const MAX_WIDTH = 600;
const DEFAULT_WIDTH = 260;

export function Sidebar({ analyser_l, analyser_r }: Props) {
    const waveform_ref = useRef<HTMLCanvasElement>(null);
    const spectro_ref = useRef<HTMLCanvasElement>(null);
    const raf_ref = useRef<number>(0);
    const zoom_ref = useRef(1);
    const [width, set_width] = useState(DEFAULT_WIDTH);
    const width_ref = useRef(width);
    useEffect(() => {
        width_ref.current = width;
    }, [width]);

    const on_handle_mousedown = useCallback((e: React.MouseEvent) => {
        e.preventDefault();
        const start_x = e.clientX;
        const start_w = width_ref.current;
        const on_move = (ev: MouseEvent) => {
            const delta = start_x - ev.clientX;
            set_width(Math.max(MIN_WIDTH, Math.min(MAX_WIDTH, start_w + delta)));
        };
        const on_up = () => {
            window.removeEventListener('mousemove', on_move);
            window.removeEventListener('mouseup', on_up);
        };
        window.addEventListener('mousemove', on_move);
        window.addEventListener('mouseup', on_up);
    }, []);

    const on_waveform_wheel = useCallback((e: React.WheelEvent) => {
        e.preventDefault();
        const dir = e.deltaY > 0 ? -1 : 1;
        zoom_ref.current = Math.max(1, Math.min(32, zoom_ref.current * (dir > 0 ? 1.2 : 1 / 1.2)));
    }, []);

    useEffect(() => {
        const wc = waveform_ref.current;
        const sc = spectro_ref.current;
        if (!wc || !sc) return;

        if (!analyser_l || !analyser_r) {
            return;
        }

        wc.width = wc.offsetWidth;
        wc.height = wc.offsetHeight;
        sc.width = sc.offsetWidth;
        sc.height = sc.offsetHeight;

        const wctx = wc.getContext('2d')!;
        const sctx = sc.getContext('2d')!;

        const buf = document.createElement('canvas');
        buf.width = sc.width;
        buf.height = sc.height;
        const bctx = buf.getContext('2d')!;
        bctx.fillStyle = '#0d0f1a';
        bctx.fillRect(0, 0, buf.width, buf.height);

        const fft_size = analyser_l.fftSize;
        const bin_count = analyser_l.frequencyBinCount;

        const time_l = new Float32Array(fft_size);
        const time_r = new Float32Array(fft_size);
        const freq_l = new Float32Array(bin_count);
        const freq_r = new Float32Array(bin_count);
        const col_img = new ImageData(1, sc.height);
        const col_data = col_img.data;

        const SH = sc.height;
        const half_h = SH >> 1;
        const bin_map = new Int32Array(SH);
        for (let y = 0; y < SH; y++) {
            const local_y = y < half_h ? y : y - half_h;
            const norm = 1 - local_y / half_h;
            bin_map[y] = Math.min(bin_count - 1, Math.pow(bin_count, norm) | 0);
        }

        let col = 0;

        const draw = () => {
            raf_ref.current = requestAnimationFrame(draw);

            analyser_l.getFloatTimeDomainData(time_l);
            analyser_r.getFloatTimeDomainData(time_r);
            analyser_l.getFloatFrequencyData(freq_l);
            analyser_r.getFloatFrequencyData(freq_r);

            const WW = wc.width;
            const WH = wc.height;
            const half_w = WH >> 1;
            const zoom = zoom_ref.current;
            const visible = (fft_size / zoom) | 0;
            const offset = (fft_size - visible) >> 1;

            wctx.fillStyle = '#0d0f1a';
            wctx.fillRect(0, 0, WW, WH);
            wctx.lineWidth = 1;

            wctx.strokeStyle = '#89b4fa';
            wctx.beginPath();
            for (let x = 0; x < WW; x++) {
                const idx = (offset + (x / WW) * visible) | 0;
                const v = time_l[idx] ?? 0;
                const y = (1 - v) * 0.5 * half_w;
                if (x === 0) wctx.moveTo(x, y);
                else wctx.lineTo(x, y);
            }
            wctx.stroke();

            wctx.strokeStyle = '#a6e3a1';
            wctx.beginPath();
            for (let x = 0; x < WW; x++) {
                const idx = (offset + (x / WW) * visible) | 0;
                const v = time_r[idx] ?? 0;
                const y = half_w + (1 - v) * 0.5 * half_w;
                if (x === 0) wctx.moveTo(x, y);
                else wctx.lineTo(x, y);
            }
            wctx.stroke();

            wctx.strokeStyle = '#2a2d3e';
            wctx.beginPath();
            wctx.moveTo(0, half_w);
            wctx.lineTo(WW, half_w);
            wctx.stroke();

            if (zoom > 1) {
                wctx.fillStyle = '#6272a4';
                wctx.font = '9px monospace';
                wctx.fillText(`${zoom.toFixed(1)}×`, 4, WH - 4);
            }

            for (let y = 0; y < SH; y++) {
                const db = y < half_h ? freq_l[bin_map[y]] : freq_r[bin_map[y]];
                const t = Math.max(0, Math.min(1, (db + 96) / 96));
                const h = (240 - t * 200) / 360;
                const l = t * 0.5;
                const a = 0.8 * Math.min(l, 1 - l);
                const i = y << 2;
                col_data[i] = (hsl_channel(h, a, l, 0) * 255) | 0;
                col_data[i + 1] = (hsl_channel(h, a, l, 8) * 255) | 0;
                col_data[i + 2] = (hsl_channel(h, a, l, 4) * 255) | 0;
                col_data[i + 3] = 255;
            }
            bctx.putImageData(col_img, col, 0);

            const SW = sc.width;
            const rest = SW - col - 1;
            if (rest > 0) sctx.drawImage(buf, col + 1, 0, rest, SH, 0, 0, rest, SH);
            sctx.drawImage(buf, 0, 0, col + 1, SH, rest, 0, col + 1, SH);

            col = (col + 1) % SW;
        };

        raf_ref.current = requestAnimationFrame(draw);
        return () => cancelAnimationFrame(raf_ref.current);
    }, [analyser_l, analyser_r, width]);

    return (
        <div className="app__sidebar" style={{ width }}>
            <div className="app__sidebar-handle" onMouseDown={on_handle_mousedown} />
            <span className="app__sidebar-label">waveform</span>
            <canvas
                ref={waveform_ref}
                className="app__sidebar-canvas"
                style={{ flex: 3 }}
                onWheel={on_waveform_wheel}
            />
            <span className="app__sidebar-label">spectrum</span>
            <canvas ref={spectro_ref} className="app__sidebar-canvas" style={{ flex: 7 }} />
        </div>
    );
}

function hsl_channel(h: number, a: number, l: number, n: number): number {
    const k = (n + h * 12) % 12;
    return l - a * Math.max(-1, Math.min(k - 3, Math.min(9 - k, 1)));
}
