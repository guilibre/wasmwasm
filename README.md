# wasmwasm

**[https://guilibre.github.io/wasmwasm](https://guilibre.github.io/wasmwasm)**

A functional audio synthesis language that compiles to WebAssembly in the browser. Write signal-processing expressions, hit Play, and hear the result in real time.

## How it works

wasmwasm is a small compiled language designed for audio synthesis. Programs describe how to compute a single audio sample per frame. The compiler (written in C++, built with Emscripten) runs entirely in the browser — source code is compiled on-the-fly to a WebAssembly module, which is then executed inside a Web Audio `AudioWorkletProcessor`.

```
source code → tokenizer → parser → AST → type inference → Binaryen IR → .wasm → AudioWorklet
```

## Language

Programs consist of expressions and variable bindings. Lines beginning with `#` are comments.

### Variables

```
freq = 440
OUT[0] <- 0.2 * sin (TIME * freq * 2 * PI)
```

`OUT[i]` is the audio output for channel `i` (expected range `[-1, 1]`). `TIME`, `PI`, and `SAMPLE_RATE` are built-in constants.

### Functions (lambdas)

Functions are defined with `{params. body}` and applied by juxtaposition. Multiple parameters are curried.

```
osc = {amp freq. amp * sin (TIME * freq * 2 * PI)}
OUT[0] <- osc 0.2 440
```

### Buffers (persistent state)

Buffers hold values that persist across audio frames — essential for filters, delays, and feedback oscillators.

```
x = delay N {i. init_expr}
```

`N` is the buffer size (1–1024). The lambda initializes each element by index.

| Syntax         | Meaning                                   |
| -------------- | ----------------------------------------- |
| `@buf`         | Read from previous frame (delay 1)        |
| `@[n]buf`      | Read with delay offset `n`                |
| `buf <- value` | Write to buffer (takes effect next frame) |

### Operators

| Category        | Operators            |
| --------------- | -------------------- |
| Arithmetic      | `+`  `-`  `*`  `/`   |
| Exponentiation  | `x ^ y`              |
| Comparison      | `<`  `>`             |
| Logical         | `&`  `\|`  `!`       |
| Conditional     | `cond ? then : else` |
| Unary           | `-x`                 |

`^` is right-associative and has higher precedence than `*` and `/`: `2 * x^3` means `2 * (x^3)`.

### Built-in functions

| Function       | Description                     |
| -------------- | ------------------------------- |
| `sin x`        | Sine                            |
| `cos x`        | Cosine                          |
| `exp x`        | Exponential (eˣ)                |
| `log x`        | Natural logarithm               |
| `sqrt x`       | Square root                     |
| `floor x`      | Round down to nearest integer   |
| `ceil x`       | Round up to nearest integer     |
| `round x`      | Round to nearest integer        |
| `sign x`       | Sign (±1)                       |
| `fract x`      | Fractional part                 |
| `clip x`       | Clamp to `[-1, 1]`              |
| `uniform x y`  | Uniform random in [x, y]        |
| `gaussian x y` | Gaussian random (μ=x, σ=y)      |

### `static`

A `static` binding is initialized once at startup and persists across frames without requiring an explicit buffer.

```
static phase = 0.0
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

```
compiler/         C++ compiler source
  src/
    parser/       tokenizer and parser
    ast/          AST nodes
    types/        type inference (Hindley-Milner)
    ir/           intermediate representation
    code_gen/     Binaryen-based WebAssembly emitter
  math/           math intrinsics compiled separately (sin, cos, etc.)
  app/            Emscripten bindings
frontend/         Vite + React frontend
  src/
    app/          editor, oscilloscope, spectrogram
    patch/        visual node-based routing editor
  examples/       example programs
```

## License

See [LICENSE](LICENSE).
