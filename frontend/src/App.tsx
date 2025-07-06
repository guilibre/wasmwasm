import { useEffect, useRef } from "react";
import Module from "./wasmwasm";

export default function App() {
  const didRun = useRef(false);

  useEffect(() => {
    if (didRun.current) return;

    try {
      Module({}).then((main_module) => {
        try {
          const result = main_module._main();
          if (result !== 0) throw new Error("main returned " + result);
        } catch (err) {
          console.error("error on compilation:", err);
          return;
        }

        let buffer: Uint8Array;
        try {
          buffer = main_module.FS_readFile("/tmp/output.wasm", {
            enconding: "binary",
          });
        } catch (err) {
          console.error("error reading file:", err);
          return;
        }

        try {
          const memory = new WebAssembly.Memory({ initial: 16, maximum: 32 });

          WebAssembly.instantiate(buffer, {
            Math: { sin: Math.sin },
            env: { memory: memory },
          }).then(({ instance }) => {
            try {
              const heapF32 = new Float32Array(memory.buffer);
              instance.exports.main(0, 2, 8);
              console.log(heapF32.slice(0, 16));
            } catch (err) {
              console.error("error executing main:", err);
              return;
            }
          });
        } catch (err) {
          console.error("error instantiating:", err);
          return;
        }
      });
    } catch (err) {
      console.error("general error:", err);
    }

    didRun.current = true;
  }, []);

  return <></>;
}
