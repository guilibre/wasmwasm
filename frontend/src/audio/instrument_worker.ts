const AsyncFunction = Object.getPrototypeOf(async function () {}).constructor as new (
    ...args: string[]
) => (...args: unknown[]) => Promise<void>;

let paramBuf: Float64Array | null = null;
let paramNames: string[] = [];
let currentBpm = 120;
let beatIntervalId: ReturnType<typeof setInterval> | null = null;

self.onmessage = async (event: MessageEvent) => {
    if (event.data.type === 'stop') {
        clearInterval(beatIntervalId!);
        beatIntervalId = null;
        self.close();
        return;
    }

    if (event.data.type === 'run') {
        const {
            code,
            sab,
            paramNames: names,
            bpm,
        } = event.data as {
            code: string;
            sab: SharedArrayBuffer;
            paramNames: string[];
            bpm: number;
        };

        paramBuf = new Float64Array(sab);
        paramNames = names;
        currentBpm = bpm;

        const paramCount = names.length;

        const setParam = (name: string, value: number) => {
            const idx = paramNames.indexOf(name);
            if (idx !== -1 && paramBuf) paramBuf[idx] = value;
        };

        const sleep = (s: number) => new Promise<void>((r) => setTimeout(r, s * 1000));

        const sleepBeats = (beats: number) => sleep((beats * 60) / currentBpm);

        const currentTime = () => (paramBuf ? paramBuf[paramCount] : 0);

        const onBeat = (fn: (beat: number) => void) => {
            let beat = 0;
            const ms = (60 * 1000) / currentBpm;
            clearInterval(beatIntervalId!);
            beatIntervalId = setInterval(() => fn(beat++), ms);
        };

        try {
            const fn = new AsyncFunction(
                'setParam',
                'sleep',
                'sleepBeats',
                'onBeat',
                'currentTime',
                code,
            );
            await fn(setParam, sleep, sleepBeats, onBeat, currentTime);
        } catch (e) {
            self.postMessage({ type: 'error', message: String(e) });
        }
    }
};
