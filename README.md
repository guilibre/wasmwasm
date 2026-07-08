# wasmwasm

**[https://guilibre.github.io/wasmwasm](https://guilibre.github.io/wasmwasm)**

A functional audio synthesis and composition toolchain that compiles to WebAssembly in the browser. Write instrument voices and scores, hit Play, and hear the result in real time.

## How it works

wasmwasm is two small compiled languages plus a runtime that ties them together, all running in the browser:

- **Instrument language** (`patch_compiler`) — describes how to compute a single audio sample per frame for one voice: oscillators, envelopes, filters.
- **Score language** (`score_compiler`) — describes when and how those voices play: sequencing, rhythm, pitch, parallel forks, and parameter transforms.
- **Conductor** (`frontend/src/audio/conductor.ts`) — a runtime that walks a compiled score, instantiating and driving instrument instances tick by tick inside a Web Audio `AudioWorkletProcessor`.

Both compilers are written in C++ and built with Emscripten; source is compiled on-the-fly, entirely client-side.

```text
instrument source ─→ patch_compiler ─→ .wasm  ─┐
                                                 ├─→ Conductor (AudioWorklet) ─→ audio out
score source      ─→ score_compiler ─→ JSON graph ─┘
```

`patch_compiler`'s IR lowering pass includes a small optimizer: bounded recursive functions
called with a compile-time-constant counter (e.g. fixed-step numerical integrators) are unrolled
into straight-line code instead of real recursive calls. See
[docs/language.md](docs/language.md#automatic-unrolling).

## Languages

- [docs/language.md](docs/language.md) — instrument language reference ([tutorial](docs/tutorial.md))
- [docs/score-language.md](docs/score-language.md) — score language reference ([tutorial](docs/score-tutorial.md))
- [docs/conductor.md](docs/conductor.md) — how a compiled score is executed at runtime, and how to hook in live JS callbacks

### Quick examples

An instrument voice:

```WASMWASM
freq = 440
static two_pi = 2 * PI
static dt = freq * two_pi / SAMPLE_RATE
static t = 0
OUT[0] <- 0.2 * sin t
t = t > two_pi ? t + dt : t + dt - two_pi
```

A score that plays a melody through it:

```SCORE
melody = (C E G C)@{instrument: "Lead"}!1/2;
play melody;
```

## Building

**Requirements:** Emscripten, CMake 3.14+, Node.js

```bash
# Linux / Mac
./build.sh

# Windows
./build.ps1
```

The build scripts compile both C++ compilers to WebAssembly and copy the output into the frontend source tree.

```bash
cd frontend
npm install
npm run dev   # http://localhost:5173
```

## Project structure

```text
patch_compiler/         C++ instrument compiler
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
score_compiler/    C++ score compiler
  src/
    parser/       tokenizer and parser
    ast/          AST nodes
    resolve/      expands the AST into a graph of state/fork/join/transform nodes
    backend/      graph → JSON serialization
    lsp/          Language Server Protocol support
  app/            Emscripten bindings
frontend/         Vite + React frontend
  src/
    app/          editor, oscilloscope, spectrogram, score/conductor panels, instrument tabs
    audio/        Web Audio compiler and the Conductor score-graph interpreter
    patch/        visual node-based routing editor
    scorewasm/    compiled score_compiler output
    wasmwasm/     compiled patch_compiler output
    templates/    bundled .ww instrument templates
docs/             language references and tutorials
```

## License

See [LICENSE](LICENSE).
