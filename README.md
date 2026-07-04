# wasmwasm

**[https://guilibre.github.io/wasmwasm](https://guilibre.github.io/wasmwasm)**

A functional audio synthesis language that compiles to WebAssembly in the browser. Write signal-processing expressions, hit Play, and hear the result in real time.

## How it works

wasmwasm is a small compiled language designed for audio synthesis. Programs describe how to compute a single audio sample per frame. The compiler (written in C++, built with Emscripten) runs entirely in the browser — source code is compiled on-the-fly to a WebAssembly module, which is then executed inside a Web Audio `AudioWorkletProcessor`.

``` text
source code → tokenizer → parser → AST → type inference → Binaryen IR → .wasm → AudioWorklet
```

The IR lowering pass includes a small optimizer: bounded recursive functions called with a
compile-time-constant counter (e.g. fixed-step numerical integrators) are unrolled into
straight-line code instead of real recursive calls. See [docs/language.md](docs/language.md#automatic-unrolling).

## Language

Programs consist of variable bindings and output assignments. Lines beginning with `#` are comments.

See [docs/language.md](docs/language.md) for the full language reference and [docs/tutorial.md](docs/tutorial.md) for a step-by-step tutorial.

### Quick example

```WASMWASM
freq = 440
static two_pi = 2 * PI
static dt = freq * two_pi / SAMPLE_RATE
static t = 0
OUT[0] <- 0.2 * sin t
t = t > two_pi ? t + dt : t + dt - two_pi
```

## Building

**Requirements:** Emscripten, CMake 3.14+, Node.js

```bash
# Linux / Mac
./build.sh

# Windows
./build.ps1
```

The build scripts compile the C++ compiler to WebAssembly and copy the output into the frontend source tree.

```bash
cd frontend
npm install
npm run dev   # http://localhost:5173
```

## Project structure

```text
patch_compiler/         C++ compiler source
  src/
    parser/       tokenizer and parser
    ast/          AST nodes
    types/        type inference (Hindley-Milner)
    ir/           intermediate representation, lowering, and optimization passes
    routing/      audio graph routing for patches
    lsp/          Language Server Protocol support
  math/           math intrinsics compiled separately
  app/            Emscripten bindings
  tests/          compiler test suite (ctest)
frontend/         Vite + React frontend
  src/
    app/          editor, oscilloscope, spectrogram
    audio/        Web Audio compiler and orchestra worker
    patch/        visual node-based routing editor
docs/             language reference and tutorial
```

## License

See [LICENSE](LICENSE).
