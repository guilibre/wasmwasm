import { useRef, useState } from "react";
import workletUrl from "../audio/processor.worklet.ts?url";
import WasmWasm from "../audio/compiler";

export default function App() {
  const audioContextRef = useRef<AudioContext | null>(null);
  const workletNodeRef = useRef<AudioWorkletNode | null>(null);
  const [code, setCode] = useState("0.1 * sin (2 * PI * TIME * 440) -> OUT");
  const [isPlaying, setIsPlaying] = useState(false);

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
        buffer: await WasmWasm.init(1 / context.sampleRate, code),
        code: code,
      });
    } else {
      const node = workletNodeRef.current;
      if (node && audioContextRef.current) {
        node.port.postMessage({
          type: "load-wasm",
          buffer: await WasmWasm.init(
            1 / audioContextRef.current.sampleRate,
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

  return (
    <div style={{ padding: "1rem", fontFamily: "sans-serif" }}>
      <h1>WASM Synth</h1>
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
