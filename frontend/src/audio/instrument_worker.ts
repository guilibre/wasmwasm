const AsyncFunction = Object.getPrototypeOf(async function () {}).constructor as new (
    ...args: string[]
) => (...args: unknown[]) => Promise<void>;

const EVENT_CAPACITY = 256;

let stateBuf: Float64Array | null = null;
let evWriteHead: Int32Array | null = null;
let evData: DataView | null = null;
let paramNames: string[] = [];
let currentBpm = 120;
let workerSampleRate = 44100;

self.onmessage = async (event: MessageEvent) => {
    if (event.data.type === 'stop') {
        self.close();
        return;
    }

    if (event.data.type === 'run') {
        const {
            code,
            state_sab,
            event_sab,
            paramNames: names,
            bpm,
            sampleRate,
            audioCurrentTime,
        } = event.data as {
            code: string;
            state_sab: SharedArrayBuffer;
            event_sab: SharedArrayBuffer;
            paramNames: string[];
            bpm: number;
            sampleRate: number;
            audioCurrentTime: number;
        };

        stateBuf = new Float64Array(state_sab);
        evWriteHead = new Int32Array(event_sab, 0, 1);
        evData = new DataView(event_sab, 8);
        paramNames = names;
        currentBpm = bpm;
        workerSampleRate = sampleRate;

        const paramCount = names.length;
        const currentTime = () => (stateBuf ? stateBuf[paramCount] : 0);

        let scheduledTime = audioCurrentTime;

        const setParam = (name: string, value: number) => {
            const idx = paramNames.indexOf(name);
            if (idx === -1 || !evData || !evWriteHead) return;
            const frame = Math.round(scheduledTime * workerSampleRate);
            const wh = Atomics.load(evWriteHead, 0);
            const slot = (wh % EVENT_CAPACITY) * 16;
            evData.setInt32(slot, frame, true);
            evData.setInt32(slot + 4, idx, true);
            evData.setFloat64(slot + 8, value, true);
            Atomics.store(evWriteHead, 0, (wh + 1) >>> 0);
        };

        const sleep = async (s: number) => {
            scheduledTime += s;
            const wallWait = scheduledTime - currentTime();
            if (wallWait > 0) {
                await new Promise<void>((r) => setTimeout(r, wallWait * 1000));
            } else {
                scheduledTime = currentTime();
                await new Promise<void>((r) => setTimeout(r, 0));
            }
        };

        const sleepBeats = (beats: number) => sleep((beats * 60) / currentBpm);

        let onBeatVersion = 0;
        const onBeat = (fn: (beat: number) => void) => {
            const myVersion = ++onBeatVersion;
            const loop = async () => {
                let beat = 0;
                while (onBeatVersion === myVersion) {
                    fn(beat++);
                    await sleepBeats(1);
                }
            };
            loop();
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
            scheduledTime = currentTime();
            await fn(setParam, sleep, sleepBeats, onBeat, currentTime);
        } catch (e) {
            self.postMessage({ type: 'error', message: String(e) });
        }
    }
};
