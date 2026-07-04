# wasmwasm language reference

A wasmwasm program runs once per audio frame. Each line is either a binding or an output assignment. Lines beginning with `#` are comments.

## Variables

```WASMWASM
param amp = 0
param amp_ = 0
param freq = 440
param freq_ = 440
static two_pi = 2 * PI
static t = 0
static dt_base = two_pi / SAMPLE_RATE
OUT[0] <- amp_ * sin t
dt = freq_ * dt_base
t = t < two_pi ? t + dt : t + dt - two_pi
amp_ = amp_ + 0.1*(amp - amp_)
freq_ = freq_ + 0.1*(freq - freq_)
```

`OUT[i]` writes to audio output channel `i`. The expected range is `[-1, 1]`.

Variables are re-evaluated every frame in order. Bindings later in the file can reference bindings earlier in the file.

## Output

```WASMWASM
OUT[0] <- left_signal
OUT[1] <- right_signal
```

## Static bindings

A `static` binding is initialized once at startup and retains its value across frames. Use it to accumulate phase, counters, or any state that must persist without an explicit delay.

```WASMWASM
static t = 0
t = t + 1
```

## Functions

Functions are defined with `{params. body}` and applied by juxtaposition. Multiple parameters are curried automatically.

```WASMWASM
param amp = 0
param amp_ = 0
param freq = 440
param freq_ = 440

sin_osc = {amp freq.
  static two_pi = 2 * PI
  static t = 0
  static dt_base = two_pi / SAMPLE_RATE
  dt = freq * dt_base
  t = t < two_pi ? t + dt : t + dt - two_pi
  amp * sin t
}

OUT[0] <- sin_osc amp_ freq_
amp_ = amp_ + 0.1*(amp - amp_)
freq_ = freq_ + 0.1*(freq - freq_)

```

Nested application:

```WASMWASM
clip_osc = {amp freq. clip (osc amp freq)}
```

## Recursion

Functions can call themselves by name:

```WASMWASM
sum = {n. n < 1 ? 0 : n + sum (n - 1)}
```

### Automatic unrolling

When a recursive function is called with a compile-time constant and its body has the shape
`counter <cmp> literal ? base : ... recurse (counter - literal) ...`, the compiler unrolls it into
straight-line code instead of a real recursive call — no runtime branching, no call overhead. This
matters for tight per-sample loops, like fixed-step numerical integrators:

```WASMWASM
fixedpoint = {n t1 t2.
  n < 1 ? [t1, t2] : (
    t1_ = t1 + step t1 t2
    t2_ = t2 + step t2 t1
    fixedpoint (n - 1) t1_ t2_
  )
}

result = fixedpoint 4 t1 t2   # unrolled into 4 straight-line steps at compile time
```

Unrolling only fires when the whole function body is a single conditional, the condition compares
one parameter against a literal, the counter decreases by a literal amount each recursive call, and
the call site's counter argument is itself a constant. Anything else falls back to an ordinary
recursive call — unrolling is purely a speed optimization and never changes program behavior.

## Params

`param` declares a named parameter with a default value. Params are exposed as controllable inputs in the UI and can be overwritten from the orchestra.

```WASMWASM
param amp = 0.2
param freq = 440
```

## Arrays

Fixed-size arrays of floats. Elements are re-evaluated every frame.

### Declaration

```WASMWASM
a = [1, 2, 3]          # array literal
b = array 3 {i. i + 1} # constructor: index lambda, size must be >= 1
```

### Static arrays

A `static` array behaves like N individual static variables — each element persists across frames.

```WASMWASM
static ts = array 5 {_. 0}
```

### Array functions

| Function           | Type                                                   | Description                                    |
| ------------------ | ------------------------------------------------------ | ---------------------------------------------- |
| `foldr init f arr` | `Float -> (Float -> Float -> Float) -> Array -> Float` | Right fold. Lambda receives `{elem acc. ...}`  |
| `map f arr`        | `(Float -> Float) -> Array -> Array`                   | Apply `f` to each element, produce new array   |
| `zip f arr1 arr2`  | `(Float -> Float -> Float) -> Array -> Array -> Array` | Apply `f` element-wise to two same-size arrays |

### Example — additive synthesis with phase accumulation

```WASMWASM
static two_pi = 2 * PI
static ts  = array 5 {_. 0}
static dts = array 5 {i. (i + 1) * two_pi / SAMPLE_RATE}

OUT[0] <- foldr 0 {t acc. acc + sin t} ts
ts = zip {x y. x + y} ts dts
```

## Delays

Delays hold arrays of values that persist across frames — essential for filters, delays, and feedback.

```WASMWASM
x = delay N {i. init_expr}
```

`N` is the delay size (4096). The lambda initializes each element by index `i`.

| Syntax         | Meaning                                      |
| -------------- | -------------------------------------------- |
| `@buf`         | Read from the previous frame (delay of 1)    |
| `@[n]buf`      | Read with an explicit delay offset `n`       |
| `buf <- value` | Write to the delay (takes effect next frame) |

Example — one-pole lowpass filter:

```WASMWASM
param cutoff = 0.1
x = delay 1 {_. 0}
x <- @x + cutoff * (IN[0] - @x)
OUT[0] <- @x
```

## Inputs

`IN[i]` reads from audio input channel `i`. Used in modules connected via the patch editor.

## Operators

| Category       | Operators                        | Notes                                         |
| -------------- | -------------------------------- | --------------------------------------------- |
| Arithmetic     | `+`  `-`  `*`  `/`               |                                               |
| Exponentiation | `x ^ y`                          | Right-associative, higher precedence than `*` |
| Comparison     | `<`  `>`  `<=`  `>=`  `==`  `!=` |                                               |
| Logical        | `&`  `\|`  `!`                   |                                               |
| Conditional    | `cond ? then : else`             |                                               |
| Unary negation | `-x`                             |                                               |

Precedence (highest to lowest): `^`, unary `-`, `*` `/`, `+` `-`, `<` `>`, `&` `|`, `?:`.

## Built-in functions

All math functions operate on `Float`.

| Function       | Description                       |
| -------------- | --------------------------------- |
| `sin x`        | Sine                              |
| `cos x`        | Cosine                            |
| `exp x`        | Exponential (eˣ)                  |
| `log x`        | Natural logarithm                 |
| `sqrt x`       | Square root                       |
| `abs x`        | Absolute value                    |
| `tanh x`       | Hyperbolic tangent                |
| `floor x`      | Round down                        |
| `ceil x`       | Round up                          |
| `round x`      | Round to nearest integer          |
| `sign x`       | Sign (−1, 0, or 1)                |
| `fract x`      | Fractional part (`x - floor x`)   |
| `clip x`       | Clamp to `[−1, 1]`                |
| `min x y`      | Minimum of two values             |
| `max x y`      | Maximum of two values             |
| `uniform x y`  | Uniform random sample in `[x, y]` |
| `gaussian x y` | Gaussian sample (μ=x, σ=y)        |

## Built-in constants

| Name          | Description                             |
| ------------- | --------------------------------------- |
| `PI`          | π (3.14159…)                            |
| `SAMPLE_RATE` | Audio sample rate in Hz (e.g. 44100)    |
| `OUT`         | Audio output array — write via `OUT[i]` |

## Type system

Types are inferred automatically using Hindley-Milner. The base types are `Float`, `Int`, `Bool`, and `Void`. Function types are curried. You never write type annotations.

## Orchestra

The orchestra is a JavaScript sequencer that runs alongside the audio engine. It controls instrument params over time using three built-in functions:

| Function             | Description                                       |
| -------------------- | ------------------------------------------------- |
| `instrument(name)`   | Returns the functions defined for that instrument |
| `sleep(seconds)`     | Wait for a duration in seconds                    |
| `sleep_beats(beats)` | Wait for a number of beats (relative to BPM)      |
| `on_beat(fn)`        | Call `fn(beat)` on every beat                     |

Each instrument can expose JavaScript functions using standard `function` syntax in its orchestra code field. These functions receive `set_param(name, value)` and `sleep`/`sleep_beats` automatically.

Example instrument orchestra code:

```js
async function play(freq, dur) {
  set_param('freq', freq)
  set_param('amp', 0.3)
  await sleep_beats(dur)
  set_param('amp', 0)
}
```

Example top-level orchestra code:

```js
const synth = instrument('synth')
while (true) {
    await synth.play(440, 0.5)
    await synth.play(550, 0.5)
    await synth.play(660, 1)
}
```
