# wasmwasm

A functional audio synthesis language that compiles to WebAssembly in the browser. Write signal-processing expressions, hit Play, and hear the result in real time.

## How it works

wasmwasm is a small compiled language designed for audio synthesis. Programs describe how to compute a single audio sample per frame. The compiler (written in C++, built with Emscripten) runs entirely in the browser — source code is compiled on-the-fly to a WebAssembly module, which is then executed inside a Web Audio `AudioWorkletProcessor`.

**Pipeline:**

```
source code → tokenizer → parser → AST → type inference → Binaryen IR → .wasm → AudioWorklet
```

## Language

Programs consist of expressions and assignments. The special variable `OUT` is the audio output (expected in the range `[-1, 1]`). `TIME` and `PI` are built-in constants.

### Basic synthesis

```
0.2 * sin (TIME * 440 * 2 * PI) > OUT
```

### Functions (lambdas)

```
{amp freq. amp * sin (TIME * freq * 2 * PI)} > osc
0.2 * osc 1.0 440 > OUT
```

### Buffers (persistent state between frames)

Buffers hold values that persist across audio frames — essential for filters, delays, and oscillators with feedback.

```
:buffer x 1 {i. 0.0}
```

This declares a buffer named `x` of size 1, initialized to `0.0`. Read from the buffer like a variable; write to it with `>`.

### Built-in functions

| Function | Description |
|----------|-------------|
| `sin x` | Sine |
| `cos x` | Cosine |
| `sign x` | Sign (±1) |
| `fract x` | Fractional part |
| `clip x` | Clamp to `[-1, 1]` |

### Comments

```
# this is a comment
```

## Examples

### Harmonic oscillator

```
:buffer xb 1 {x. 0.0}
:buffer yb 1 {y. 0.5}

1   > k
0.1 > h

xb > x
yb > y

y - k * x * h/2 > y
x +     y * h   > x
y - k * x * h/2 > y

x > xb
y > yb

0.2 * clip x > OUT
```

### Karplus-Strong (plucked string)

```
:buffer x 1024 {x. 10 * fract (47684873451.123783453 * sin (x/1024))}
:buffer y 1024 {x. 0}

0.51 > alpha

x > x1
y > y1

0.1 * (alpha * x + (1.0 - alpha) * x1) > tmp1
tmp1 > x
0.99 * (alpha * y + (1.0 - alpha) * y1) > tmp2
tmp1 + tmp2 > y

0.2 * clip y > OUT
```

More examples (Chua's circuit, double pendulum) are available in `website/examples/`.

## Building

**Requirements:** Emscripten, CMake, Node.js

```bash
sh build.sh
cd website
npm install
npm run dev
```

`build.sh` compiles the C++ compiler to WebAssembly and copies the output into the website source tree. The dev server is then available at `http://localhost:5173`.

### Windows

```powershell
./build.ps1
cd website
npm install
npm run dev
```

## Project structure

```
compiler/         C++ compiler source
  src/
    parser/       tokenizer and parser
    ast/          AST nodes
    types/        type inference (Hindley-Milner)
    code_gen/     Binaryen-based WebAssembly emitter
  math/           math intrinsics compiled separately (sin, cos, etc.)
  app/            CLI entry point (for local testing)
website/          Vite + React frontend
  src/
    audio/        AudioWorklet processor and compiler glue
    app/          UI (editor + oscilloscope)
  examples/       example programs
```

## License

See [LICENSE](LICENSE).
