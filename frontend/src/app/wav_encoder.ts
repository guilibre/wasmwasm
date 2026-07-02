export function encode_wav(
    buffers: Float32Array[],
    length: number,
    sample_rate: number,
    num_channels: number = 2,
): Blob {
    const bytes_per_sample = 2;
    const block_align = num_channels * bytes_per_sample;
    const data_size = length * bytes_per_sample;
    const array_buffer = new ArrayBuffer(44 + data_size);
    const view = new DataView(array_buffer);
    let offset = 0;
    const write_string = (s: string) => {
        for (let i = 0; i < s.length; i++) {
            view.setUint8(offset++, s.charCodeAt(i));
        }
    };
    write_string('RIFF');
    view.setUint32(offset, 36 + data_size, true);
    offset += 4;
    write_string('WAVE');
    write_string('fmt ');
    view.setUint32(offset, 16, true);
    offset += 4;
    view.setUint16(offset, 1, true);
    offset += 2;
    view.setUint16(offset, num_channels, true);
    offset += 2;
    view.setUint32(offset, sample_rate, true);
    offset += 4;
    view.setUint32(offset, sample_rate * block_align, true);
    offset += 4;
    view.setUint16(offset, block_align, true);
    offset += 2;
    view.setUint16(offset, bytes_per_sample * 8, true);
    offset += 2;
    write_string('data');
    view.setUint32(offset, data_size, true);
    offset += 4;

    let sample_offset = offset;
    for (const chunk of buffers) {
        for (let i = 0; i < chunk.length; i++) {
            const sample = Math.max(-1, Math.min(1, chunk[i]));
            view.setInt16(sample_offset, sample < 0 ? sample * 0x8000 : sample * 0x7fff, true);
            sample_offset += 2;
        }
    }

    return new Blob([array_buffer], { type: 'audio/wav' });
}

export function download_blob(blob: Blob, filename: string) {
    const url = URL.createObjectURL(blob);
    const anchor = document.createElement('a');
    anchor.href = url;
    anchor.download = filename;
    anchor.click();
    URL.revokeObjectURL(url);
}
