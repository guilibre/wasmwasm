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
          WebAssembly.instantiate(buffer, { Math: { sin: Math.sin } }).then(
            ({ instance, module }) => {
              try {
                console.log(instance.exports.main?.(1));
                console.log(module);
              } catch (err) {
                console.error("error executing main:", err);
                return;
              }
            }
          );
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
