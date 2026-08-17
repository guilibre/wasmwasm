let raf_id = 0;
let zoom = 1;
let analyser_l = null;
let analyser_r = null;

export function setAnalysers(l, r) {
    analyser_l = l;
    analyser_r = r;
}

export function setZoom(z) {
    zoom = z;
}

export function zoomBy(delta_y) {
    const dir = delta_y > 0 ? -1 : 1;
    zoom = Math.max(1, Math.min(32, zoom * (dir > 0 ? 1.2 : 1 / 1.2)));
}

function hsl_channel(h, a, l, n) {
    const k = (n + h * 12) % 12;
    return l - a * Math.max(-1, Math.min(k - 3, Math.min(9 - k, 1)));
}

export function initSidebarRenderer({ waveform, spectrum }) {
    if (raf_id) cancelAnimationFrame(raf_id);
    raf_id = 0;

    const wctx = waveform.getContext('2d');
    const sctx = spectrum.getContext('2d');
    if (!wctx || !sctx) return;

    const buf = document.createElement('canvas');
    const bctx = buf.getContext('2d');

    let fft_size = 2048;
    let bin_count = 1024;
    let time_l = new Float32Array(fft_size);
    let time_r = new Float32Array(fft_size);
    let freq_l = new Float32Array(bin_count);
    let freq_r = new Float32Array(bin_count);
    let col = 0;
    let col_img = new ImageData(1, 1);
    let bin_map = new Int32Array(0);

    const draw = () => {
        // Para o loop quando o sidebar é colapsado e os canvas saem do DOM;
        // initSidebarRenderer() é chamado de novo ao re-expandir.
        if (!waveform.isConnected || !spectrum.isConnected) {
            raf_id = 0;
            return;
        }
        raf_id = requestAnimationFrame(draw);

        if (
            waveform.offsetWidth > 0 &&
            (waveform.width !== waveform.offsetWidth || waveform.height !== waveform.offsetHeight)
        ) {
            waveform.width = waveform.offsetWidth;
            waveform.height = waveform.offsetHeight;
        }
        if (
            spectrum.offsetWidth > 0 &&
            (spectrum.width !== spectrum.offsetWidth || spectrum.height !== spectrum.offsetHeight)
        ) {
            spectrum.width = spectrum.offsetWidth;
            spectrum.height = spectrum.offsetHeight;
        }

        const WW = waveform.width;
        const WH = waveform.height;
        const SW = spectrum.width;
        const SH = spectrum.height;

        wctx.fillStyle = '#0d0f1a';
        wctx.fillRect(0, 0, WW, WH);
        sctx.fillStyle = '#0d0f1a';
        sctx.fillRect(0, 0, SW, SH);

        if (!analyser_l || !analyser_r) {
            return;
        }

        if (analyser_l.fftSize !== fft_size) {
            fft_size = analyser_l.fftSize;
            bin_count = analyser_l.frequencyBinCount;
            time_l = new Float32Array(fft_size);
            time_r = new Float32Array(fft_size);
            freq_l = new Float32Array(bin_count);
            freq_r = new Float32Array(bin_count);
        }
        if (col_img.width !== 1 || col_img.height !== SH) {
            col_img = new ImageData(1, SH);
            bin_map = new Int32Array(SH);
            const half_h = SH >> 1;
            for (let y = 0; y < SH; y++) {
                const local_y = y < half_h ? y : y - half_h;
                const norm = 1 - local_y / half_h;
                bin_map[y] = Math.min(bin_count - 1, Math.pow(bin_count, norm) | 0);
            }
        }
        if (buf.width !== SW || buf.height !== SH) {
            buf.width = SW;
            buf.height = SH;
            bctx.fillStyle = '#0d0f1a';
            bctx.fillRect(0, 0, SW, SH);
        }

        analyser_l.getFloatTimeDomainData(time_l);
        analyser_r.getFloatTimeDomainData(time_r);
        analyser_l.getFloatFrequencyData(freq_l);
        analyser_r.getFloatFrequencyData(freq_r);

        const half_w = WH >> 1;
        const visible = (fft_size / zoom) | 0;
        const offset = (fft_size - visible) >> 1;

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

        const col_data = col_img.data;
        const half_h = SH >> 1;
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

        const rest = SW - col - 1;
        if (rest > 0) sctx.drawImage(buf, col + 1, 0, rest, SH, 0, 0, rest, SH);
        sctx.drawImage(buf, 0, 0, col + 1, SH, rest, 0, col + 1, SH);

        col = (col + 1) % SW;
    };

    draw();
}
