import { useEffect, useRef } from "react";
import workletUrl from "../audio/processor.worklet.ts?url";
import WasmWasm from "../audio/compiler";

export default function App() {
  const didRun = useRef(false);
  let audio_context: AudioContext | undefined = undefined;

  useEffect(() => {
    window.addEventListener("click", async () => {
      if (audio_context?.state === "running") {
        audio_context.suspend();
        return;
      }
      if (!audio_context) audio_context = new AudioContext();

      audio_context.resume();
      await audio_context.audioWorklet.addModule(workletUrl);
      const node = new AudioWorkletNode(audio_context, "wasm-processor");
      node.connect(audio_context.destination);

      node.port.postMessage({
        type: "load-wasm",
        buffer: await WasmWasm.init(1 / 44100),
      });
    });
  }, []);

  didRun.current = true;
  return <></>;
}
