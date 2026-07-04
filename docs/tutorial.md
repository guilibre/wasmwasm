# wasmwasm tutorial

This tutorial introduces wasmwasm step by step. Each section builds on the previous one. Open the [live editor](https://guilibre.github.io/wasmwasm), paste the examples, and hit Play.

## 1. Hello, sine

The simplest possible program: a sine oscillator at 440 Hz.

```WASMWASM
static two_pi = 2 * PI
static t = 0
static dt_base = two_pi / SAMPLE_RATE
OUT[0] <- 0.2 * sin t
dt = 440 * dt_base
t = t < two_pi ? t + dt : t + dt - two_pi
```

`static` bindings are initialized once and persist across frames. Here `t` is the current phase angle, incremented by `dt` each frame and wrapped at `two_pi`.

`OUT[0]` writes the computed sample to the left audio channel.

## 2. Variables

Pull the frequency and amplitude out into named variables:

```WASMWASM
freq = 440
amp = 0.2
static two_pi = 2 * PI
static t = 0
static dt_base = two_pi / SAMPLE_RATE
OUT[0] <- amp * sin t
dt = freq * dt_base
t = t < two_pi ? t + dt : t + dt - two_pi
```

Variables are re-evaluated every frame in document order.

## 3. Functions

Package the oscillator into a reusable function:

```WASMWASM
osc = {amp freq.
  static two_pi = 2 * PI
  static t = 0
  static dt_base = two_pi / SAMPLE_RATE
  result = amp * sin t
  dt = freq * dt_base
  t = t < two_pi ? t + dt : t + dt - two_pi
  result
}

OUT[0] <- osc 0.2 440
OUT[1] <- osc 0.2 554
```

Functions are defined with `{params. body}` and applied by juxtaposition. Multiple parameters are curried: `osc 0.2 440` means `(osc 0.2) 440`.

## 4. Params

`param` exposes a value as a controllable input in the UI. It has a default and can be overwritten from the orchestra or the patch editor.

```WASMWASM
param amp = 0.2
param freq = 440
static two_pi = 2 * PI
static t = 0
static dt_base = two_pi / SAMPLE_RATE
OUT[0] <- amp * sin t
dt = freq * dt_base
t = t > two_pi ? t + dt : t + dt - two_pi
```

Now you can automate `freq` and `amp` from the orchestra without changing the instrument code.

## 5. Delays and delay

Delays store arrays that persist across frames. Use them for filters, echoes, and feedback.

A simple one-pole lowpass filter:

```WASMWASM
param cutoff = 0.1
x = delay 1 {_. 0}
x <- @x + cutoff * (IN[0] - @x)
OUT[0] <- @x
```

`delay 1 {_. 0}` creates a delay of size 1 initialized to zero. `@x` reads the previous frame's value. `x <- value` schedules a write that takes effect next frame.

An echo with 0.5 s feedback:

```WASMWASM
buf_size = floor (SAMPLE_RATE * 0.5)
echo = delay buf_size {_. 0}
write_head = delay 1 {_. 0}
i = floor @write_head
echo[i] <- IN[0] + 0.5 * @[1]echo
OUT[0] <- @[1]echo
write_head <- fract (@write_head + 1 / buf_size)
```

## 6. Recursion

Functions can call themselves:

```WASMWASM
# Compute the nth harmonic partial
partial = {n freq amp.
  n < 1
    ? 0
    : amp * sin (freq * n) + partial (n - 1) freq (amp * 0.7)
}

OUT[0] <- partial 6 (440 * 2 * PI / SAMPLE_RATE) 0.15
```

Since `partial` is called with a fixed count (`6`) and counts down by 1 each call, the compiler
unrolls it into straight-line code automatically — no extra work on your part, just faster
generated code for tight per-sample loops. See [language.md](language.md#automatic-unrolling) for
the exact shape the compiler recognizes.

## 7. Multiple outputs and the patch editor

A module can have multiple inputs (`IN[i]`) and outputs (`OUT[i]`), making it composable in the patch editor.

A stereo reverb tail module:

```WASMWASM
param mix = 0.5
# ... filter chain reading IN[0] ...
OUT[0] <- dry * (1 - mix) + wet_l * mix
OUT[1] <- dry * (1 - mix) + wet_r * mix
```

In the patch editor, drag connections between module outputs and inputs to build a signal graph. The DAC node is the final stereo output.

## 8. Orchestra

The orchestra is a JavaScript sequencer that controls instrument params over time. Each instrument exposes functions in its orchestra code field:

**Instrument "synth" — wasmwasm code:**

```WASMWASM
param amp = 0
param freq = 440

static amp_ = 0
static freq_ = 440

static two_pi = 2 * PI
static t = 0
static dt_base = two_pi / SAMPLE_RATE

OUT[0] <- amp_ * sin t

dt = freq_ * dt_base
t = t < two_pi ? t + dt : t + dt - two_pi
freq_ = freq_ + 0.1*(freq - freq_)
amp_ = amp_ + 0.01*(amp - amp_)
```

**Instrument "synth" — orchestra code (JavaScript):**

```js
async function note(amp: number, freq: number) {
  set_param('amp', amp);
  set_param('freq', freq);
  set_param('gate', 1);
}
```

**Top-level orchestra code:**

```js
const synth = instrument('synth')
while (true) {
  await synth.note(440, 0.5)
  await synth.note(550, 0.5)
  await synth.note(660, 1)
  await synth.note(550, 0.5)
}
```

`set_param(name, value)` changes a param immediately. `sleep_beats(n)` waits for `n` beats at the current BPM. `on_beat(fn)` fires a callback on every beat.

## Next steps

- Read [language.md](language.md) for the complete reference
- Try adding a second instrument and mixing them at the orchestra level
- Build a signal chain in the patch editor using the routing graph
