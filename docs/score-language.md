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

An array of numbers, sorted and de-duplicated at compile time. Scales are
carried through to the compiled graph (as a top-level `scales` array) for use
by callback handlers - see [conductor.md](conductor.md).

A block field named `scale` may reference a scale declaration by name:

```SCORE
major = [2^(0/12), 2^(2/12), 2^(4/12), 2^(5/12), 2^(7/12), 2^(9/12), 2^(11/12)]
melody = ({degree: 0} {degree: 1} {degree: 2})@{octave: 4}@{scale: major}
```

At compile time `scale: major` resolves to the numeric index of `major`
within the compiled graph's `scales` array - it's an error to reference an
undefined scale. `degree` and `octave` are not special to the compiler; they
are ordinary numeric fields, by convention interpreted by a callback handler
together with the referenced scale to compute `freq` at playback time (see
[conductor.md](conductor.md#callback-handlers-live-control)).

### Predefined scales

Five scales are predefined, as frequency ratios (`2^(semitone/12)`) over
their semitone steps within an octave:

- `chromatic`: `0 1 2 3 4 5 6 7 8 9 10 11`
- `major`: `0 2 4 5 7 9 11`
- `minor`: `0 2 3 5 7 8 10`
- `harmonic_minor`: `0 2 3 5 7 8 11`
- `melodic_minor`: `0 2 3 5 7 9 11`

Declaring your own scale with one of these names overrides it (in place, at
the same index in the compiled `scales` array) rather than adding a
duplicate.

## Built-in notes

Twenty-one atomic constants are predefined, covering the seven natural
degrees of a major scale with sharp (`s`) and flat (`b`) variants:

```SCORE
A As Ab  B Bs Bb  C Cs Cb  D Ds Db  E Es Eb  F Fs Fb  G Gs Gb
```

Each is a block with `freq = 440 * 2^(semitone/12)`, `dur = 1`,
`degree = <semitone mod 12>`, `octave = 4`, and `scale` set to the index of
the predefined `chromatic` scale - so `scales[p['scale']](p['degree'],
p['octave'])` (see [conductor.md](conductor.md#scale-functions)) works for
built-in notes too, not just custom `{degree, octave, scale}` blocks. You may
redeclare one of these names yourself, but only with another atomic value (a
block, or a reference that resolves to one) - a redeclaration is used to
shadow a note with a different sound, not to turn it into a sequence.

## Composition expressions

A composition expression is built out of *terms*. Multiple terms written one
after another (separated by whitespace) play **in sequence**:

```SCORE
melody = C E G C
```

### Grouping

Parentheses group a sub-sequence into a single term, so it can be combined
with `@`, `!`, or `|>`:

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

`@` merges parameters onto an expression. Two forms:

```SCORE
lead@C          # merge another atomic value's params (and instrument) onto lead
melody@{freq: freq * 2}   # push a per-field transform over melody
```

**`@name`** (or `@{...}` written with only literal values) merges one atomic
value's params (and instrument, if it sets one) onto another, resolved at
compile time. Both sides must be atomic - a block literal, a note, or a
variable that resolves to one:

```SCORE
lead@C          # play the "lead" voice at C's pitch/duration
(C E G C)@{instrument: "Lead"}   # apply an instrument to a whole sequence
```

**`@{field: expr; ...}`** pushes a field-by-field transform that applies to
every atomic event reached while inside the left-hand expression - which can
be any composition, not just an atomic value. Each field's expression can
reference the *current* value of any parameter by name (e.g. `freq`), and
evaluates to a new value for that field - or to `null` to remove it:

```SCORE
melody@{freq: freq * 2}                 # play melody an octave up
melody@{freq: freq * 2; instrument: "Lead"}   # several fields at once
```

In both forms, the right-hand side's fields win where both sides (or the
current value and the new one) define the same field. `@` chains, so
`expr@{a: 1}@{b: 2}` applies both.

### `!` (duration shorthand)

```SCORE
expr ! <expr>
```

Shorthand for `expr @ {dur: <expr>}`:

```SCORE
(C E G C)!1/2    # halve every note's duration
```

### Octave suffixes - `'` and `,`

Written directly after an atom, `'` shifts it up an octave and `,` shifts it
down. Both are sugar for two guarded atomic joins (`@{...}`): `freq` is
multiplied/divided by 2 wherever it's present, and - independently - `octave` is
incremented/decremented by 1 wherever *it's* present. A note's `freq` and a
`{degree: ...}@{octave: ...}` block's `octave` respond to the same suffix, so
both styles compose the same way:

```SCORE
C E G C'                                # ...then C one octave up (freq * 2)
{degree: 0} @ {octave: 4}
{degree: 0}'@{octave: 4}                # octave becomes 5
```

### Legato - `~`

`~` between two terms in a sequence marks legato: the left term gets
`legato: true` and `legato_id: <n>` params, tying it to the term that
follows. The last term in a `~` chain gets `legato: false` instead (but
keeps the same `legato_id`):

```SCORE
a ~ b ~ c   # a, b, c all share one legato_id; a and b get legato: true, c gets legato: false
```

`legato_id` is unique per `~` chain (a fresh id every time a chain starts),
letting the runtime tell separate legato groups apart - e.g. `a~b c~d` uses
two different ids. `repeat` and `reverse` also assign a fresh `legato_id` to
each copy they produce, so a `~` chain inside a repeated/reversed
composition still gets a distinct id per repetition. The note affected is
always the last real note of the
term on the left of `~` - for a multi-note group this is its last note, not
its first (`(C E) ~ D` only marks `E`). `~` cannot connect a term that forks
(`&`), since a fork has no single "last note". Terms not adjacent to `~` get
no `legato`/`legato_id` params at all.

`~` can also tie into a self-referential loop (see below): `body ~ name`,
where `name` is the composition's own name, marks `body`'s last note as
tying into the loop's first note the next time it plays - the loop restarts
without retriggering:

```SCORE
melody = C D E ~ melody   # E ties into C every time the loop repeats
```

### Self-reference (loops)

A composition that refers to its own name inside its body loops forever:

```SCORE
clock = C clock
play clock
```

Note that `,` immediately after an atom is always the octave-down suffix
(above), never a term separator - sequencing is done with whitespace alone.

### choose

```SCORE
choose <predicate> <a> <b>
```

`choose` is a term (not a pipe) that picks between two compositions at
playback time: it plays `a` if `<predicate>` is true, `b` otherwise. Anywhere
a note or composition is expected, `choose ...` can appear instead.

The predicate is an [expression](#expressions) evaluated against the
parameters of the last atomic event reached before the `choose` - the same
"current value of a parameter" semantics as an `@{...}` field value.
Combined with self-reference, this lets a loop stop (or branch) based on how
it was last played, instead of repeating forever unconditionally:

```SCORE
verse = (C E G C)@{count: 0};
song =
  verse
  choose count < 3 (song@{count: count + 1}) outro;

play song;
```

There's no mutable variable that truly accumulates across iterations here -
`count` is a parameter baked into each atomic event, and `@{count: count +
1}` recomputes it from whatever value is currently in scope each time
through the loop. Test this kind of counter carefully; it's easy to end up
with a predicate that's always (or never) true.

Note the two different meanings of `&`: between composition terms (as in
[Fork](#fork---), or as `<a>`/`<b>` here) it means "play in parallel"; inside
an expression (the predicate above, an `@{...}` field value, a ternary
condition) it means logical AND. Which one applies is determined by where
`&` appears in the grammar, not by the character itself.

### emit

```SCORE
emit "<id>" {field: value; ...}
```

`emit` is a term (not a pipe) that broadcasts a set of constant fields (and
optionally an `instrument`) to a named **signal** - a global mailbox
identified by a string, independent of any variable or scope. It's
instantaneous: it doesn't play a note or take any time, it just publishes
the fields and immediately continues to whatever follows it in the
sequence. Unlike `@{...}`, `emit`'s field values can't reference the
current value of a parameter - they're plain constants, evaluated the same
way as a block declaration's fields.

This is the only way for two independent voices (two `play` machines, or
two `&` branches that never rejoin) to communicate at playback time - see
[`listen`](#listen) for the receiving end.

## Pipe operators

Pipe operators wrap a whole sub-expression and apply while inside it.

### `reverse`

```SCORE
expr |> reverse
```

Reverses the playback order of `expr`.

### `repeat`

```SCORE
expr |> repeat n
```

Repeats `expr` `n` times in sequence. `n` is a compile-time constant, at
least `1`; a fractional `n` is floored (`repeat 5.31` behaves like
`repeat 5`). `expr` may not itself be an unbounded self-referential loop.

### listen

```SCORE
expr |> listen "<id>"
```

Merges whatever fields were most recently [`emit`](#emit)ted to signal
`"<id>"` (and its `instrument`, if it set one) onto every atomic event
reached while inside `expr` - the receiving half of `emit`. Unlike `@{...}`,
what gets merged isn't known at compile time: it's whatever the *last*
`emit "<id>"` anywhere in the piece last broadcast, at the moment each note
inside `expr` plays. If nothing has ever `emit`ted to `"<id>"` yet, `listen`
has no effect - the fields it would have merged are simply absent.

```SCORE
lead = (C E G)@{instrument: "Lead"} emit "pitch" {freq: 880};
follow = D!4 |> listen "pitch";

play lead & follow;
```

Because it depends on real-time ordering between independent voices, two
runs of the same score can `listen` a different value if the timing of
`emit`/`listen` across voices changes (e.g. after editing durations). This
is by design - `emit`/`listen` is for live coupling between voices, not a
deterministic compile-time construct like `@{...}`.

## Expressions

Used for block field values, `@{...}` field values, and `choose` predicates:

- Numbers: `440`, `0.5`
- `true` / `false`: sugar for `1` / `0`
- Identifiers: only valid inside an `@{...}` field value or a `choose`
  predicate, where they read the current value of a parameter (e.g. `freq`)
- Arrays: `[a, b, c]` (only valid in a scale declaration)
- Ternary: `cond ? then : else`
- Binary operators: `+ - * / % ^ == != < > <= >= & |`
- `null`

`&` and `|` are logical AND/OR here - see the note on `&` under
[choose](#choose) for how this differs from `&` between composition terms.
Precedence (highest to lowest): `^`, unary `-`, `* / %`, `+ -`,
comparisons (`== != < > <= >=`), `&`, `|`, `?:`.

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
