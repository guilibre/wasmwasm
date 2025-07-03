import Module from "./wasmwasm.js";

export default function App() {
  Module({ arguments: ["test"] });
  return <></>;
}
