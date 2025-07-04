import { useEffect, useRef } from "react";
import Module from "./wasmwasm";

export default function App() {
  const didRun = useRef(false);

  useEffect(() => {
    if (didRun.current) return;

    Module({
      print: () => {},
    }).then((main_module) => {
      try {
        const result = main_module.callMain(["teste"]);
        if (result !== 0) throw new Error("main returned " + result.toString());

        const buffer = main_module.FS_readFile("/tmp/output.wasm", {
          enconding: "binary",
        });

        WebAssembly.instantiate(buffer).then(({ instance, module }) => {
          console.log(instance.exports.main?.());
          console.log(module);
        });
      } catch (err) {
        console.error("error on compilation:", err);
      }
    });

    didRun.current = true;
  }, []);

  return <></>;
}
