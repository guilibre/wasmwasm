import { useEffect, useRef, useState } from "react";
import workletUrl from "../audio/processor.worklet.ts?url";
import WasmWasm from "../audio/compiler";

export default function App() {
  const audioContextRef = useRef<AudioContext | null>(null);
  const workletNodeRef = useRef<AudioWorkletNode | null>(null);
  const [code, setCode] = useState(
    "* 0.1 (sin (* (* 880 PI) TIME)) > OUT"
  );
  const [isPlaying, setIsPlaying] = useState(false);
  const [signal, setSignal] = useState<number[]>([]);
  const canvasRef = useRef<HTMLCanvasElement>(null);

  const setupAudio = async () => {
    if (!audioContextRef.current) {
      const context = new AudioContext();
      await context.audioWorklet.addModule(workletUrl);

      const node = new AudioWorkletNode(context, "wasm-processor");
      node.connect(context.destination);

      audioContextRef.current = context;
      workletNodeRef.current = node;

      node.port.postMessage({
        type: "load-wasm",
        buffer: await WasmWasm.init(context.sampleRate, code),
        code: code,
      });

      node.port.onmessage = (e) => {
        if (e.data.type === "signal") setSignal(e.data.data);
      };
    } else {
      if (workletNodeRef.current && audioContextRef.current) {
        workletNodeRef.current.port.postMessage({
          type: "load-wasm",
          buffer: await WasmWasm.init(
            audioContextRef.current.sampleRate,
            code
          ),
          code: code,
        });
      }
    }
  };

  const toggleAudio = async () => {
    const context = audioContextRef.current;

    if (!isPlaying) {
      await setupAudio();
      await context?.resume();
      setIsPlaying(true);
    } else {
      await context?.suspend();
      setIsPlaying(false);
    }
  };

  useEffect(() => {
    const canvas = canvasRef.current;
    if (!canvas || signal.length === 0) return;

    const ctx = canvas.getContext("2d");
    if (!ctx) return;

    ctx.clearRect(0, 0, canvas.width, canvas.height);
    ctx.beginPath();

    signal.forEach((val, i) => {
      val = 10 * val;
      const x = (i / signal.length) * canvas.width;
      const y = (1 - (val + 1) / 2) * canvas.height;
      if (i === 0) ctx.moveTo(x, y);
      else ctx.lineTo(x, y);
    });

    ctx.strokeStyle = "#2c3e50";
    ctx.stroke();
  }, [signal]);

  return (
    <div style={{ padding: "1rem", fontFamily: "sans-serif" }}>
      <h1>WASM Synth</h1>
      <canvas
        ref={canvasRef}
        width={512}
        height={100}
        style={{ border: "1px solid #ccc", marginTop: "1rem", width: "100%" }}
      />
      <textarea
        rows={6}
        style={{ width: "100%", fontFamily: "monospace", fontSize: "1rem" }}
        value={code}
        onChange={(e) => setCode(e.target.value)}
      />
      <button onClick={toggleAudio} style={{ marginTop: "1rem" }}>
        {isPlaying ? "Stop" : "Play"}
      </button>
    </div>
  );
}
