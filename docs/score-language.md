# Score language reference

The score language is a small DSL for describing *when* and *how* instruments
play - rhythm, pitch, sequencing, and live parameter control. It compiles
(via `score_compiler`) to a JSON graph that the `Conductor` runtime
(`frontend/src/audio/conductor.ts`) walks at playback time, instantiating
[instrument-language](language.md) voices as it goes.

If `language.md` describes *what a sound is*, this document describes
*when it happens*.

## Declarations

A program is a sequence of declarations, each of one of three kinds,
followed by any number of `play` statements:

```SCORE
name = { instrument: "Lead" freq: 440 dur: 1 }   # block ("atomic") declaration
name = C E G                                     # composition declaration
name = [0, 4, 7]                                 # scale declaration
```

### Block declarations

```SCORE
lead = { instrument: "Lead" attack: 0.01 }
```

A block is a set of `field: value` pairs. `instrument` (optional) names an
instrument compiled by `patch_compiler`; every other field becomes a numeric
parameter (`freq`, `dur`, or any custom instrument parameter) evaluated at
compile time. A declaration whose body is only a block and nothing else is
**atomic** - it represents a single event, not a sequence.

### Composition declarations

Anything that isn't a bare block or an array is a *composition expression* -
see [Composition expressions](#composition-expressions) below.

### Scale declarations

```SCORE
major = [0, 2, 4, 5, 7, 9, 11]
```

An array of numbers, sorted and de-duplicated at compile time. (Scales are
carried through to the compiled graph for use by callback handlers - see
[conductor.md](conductor.md) - they aren't otherwise interpreted by the
compiler.)

## Built-in notes

Twenty-one atomic constants are predefined, covering the seven natural
degrees of a major scale with sharp (`s`) and flat (`b`) variants:

```SCORE
A As Ab  B Bs Bb  C Cs Cb  D Ds Db  E Es Eb  F Fs Fb  G Gs Gb
```

Each is a block with `freq = 440 * 2^(semitone/12)` and `dur = 1`. You may
redeclare one of these names yourself, but only with another atomic value
(a block, or a reference that resolves to one) - a redeclaration is used to
shadow a note with a different sound, not to turn it into a sequence.

## Composition expressions

A composition expression is built out of *terms*. Multiple terms written one
after another (separated by whitespace) play **in sequence**:

```SCORE
melody = C E G C
```

### Grouping

Parentheses group a sub-sequence into a single term, so it can be combined
with `@`, `|>`, or `!`:

```SCORE
(C E G C)!1/2
```

### Fork - `&`

`&` plays terms **in parallel**; the branches join back together once all of
them finish:

```SCORE
play kick & (C E G)
```

### Atomic join - `@`

`@` merges one atomic value's params (and instrument, if it sets one) onto
another. Both sides must be atomic - a block literal, a note, or a variable
that resolves to one:

```SCORE
lead@C          # play the "lead" voice at C's pitch/duration
(C E G C)@{instrument: "Lead"}   # apply an instrument to a whole sequence
```

The right-hand side's fields win where both sides define the same field.

### Octave suffixes - `'` and `,`

Written directly after an atom, `'` shifts its `freq` up an octave and `,`
shifts it down (both are sugar for a `freq` transform):

```SCORE
C E G C'   # ...then C one octave up
```

### Self-reference (loops)

A composition that refers to its own name inside its body loops forever:

```SCORE
clock = C clock
play clock
```

Note that `,` immediately after an atom is always the octave-down suffix
(above), never a term separator - sequencing is done with whitespace alone.

## Pipe operators

Pipe operators wrap a whole sub-expression and apply while inside it.

### `transform` / `by`

```SCORE
expr |> transform <param> by <expr>
```

Pushes a parameter transform that applies to every atomic event reached
while inside `expr`. The transform expression can reference the *current*
value of any parameter by name (e.g. `freq`), and evaluates to a new value
for `<param>` - or to `null` to remove it.

```SCORE
melody |> transform freq by freq * 2
```

### `reverse`

```SCORE
expr |> reverse
```

Reverses the playback order of `expr`.

### `!` (duration shorthand)

```SCORE
expr ! <expr>
```

Shorthand for `expr |> transform dur by <expr>`:

```SCORE
(C E G C)!1/2    # halve every note's duration
```

## Expressions

Used for block field values and transform amounts:

- Numbers: `440`, `0.5`
- Identifiers: only valid inside a `transform ... by` expression, where they
  read the current value of a parameter (e.g. `freq`)
- Arrays: `[a, b, c]` (only valid in a scale declaration)
- Ternary: `cond ? then : else`
- Binary operators: `+ - * / ^ == != < > <= >=`
- `null`

## `play`

```SCORE
play <composition expression>;
```

Each `play` statement is one entry point into the compiled graph; a program
may have several.

## Comments

Lines starting with `#` are comments.

## Example

```SCORE
melody = (C E G C)@{instrument: "Lead"}!1/2;
play melody;
```

See [conductor.md](conductor.md) for how this compiles and runs, and
[score-tutorial.md](score-tutorial.md) for a guided walkthrough.
