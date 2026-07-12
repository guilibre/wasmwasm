# Score tutorial

This is a step-by-step introduction to the [score language](score-language.md).
It assumes you already have at least one instrument (see
[tutorial.md](tutorial.md)) compiled and available by name - we'll call it
`"Lead"` below.

## 1. Play a single note

```SCORE
play C;
```

`C` is one of the [built-in note constants](score-language.md#built-in-notes),
an atomic value with a `freq` and a `dur`, but no instrument. Nothing will
actually sound yet, because no instrument is attached.

## 2. Attach an instrument

```SCORE
play C@{instrument: "Lead"};
```

`@` merges the block's fields (here, just `instrument`) onto `C`. Now the
event carries both a pitch and a voice to play it with.

## 3. Play a sequence

Terms written one after another play in order:

```SCORE
play (C E G C)@{instrument: "Lead"};
```

Parentheses group `C E G C` into a single term so `@` applies to the whole
sequence, not just the first note.

## 4. Give it a name

Pull the melody out into its own declaration so it can be reused and
transformed:

```SCORE
melody = (C E G C)@{instrument: "Lead"};
play melody;
```

## 5. Adjust timing

`!` is shorthand for scaling `dur`:

```SCORE
melody = (C E G C)@{instrument: "Lead"}!1/2;
play melody;
```

Every note in `melody` now lasts half as long.

## 6. Add a second voice in parallel

`&` forks playback - branches run at the same time and rejoin when both
finish:

```SCORE
bass = { instrument: "Lead" freq: 110 dur: 2 }
melody = (C E G C)@{instrument: "Lead"}!1/2;

play melody & bass;
```

## 7. Transform pitch dynamically

`@{field: expr; ...}` rewrites parameters for everything inside it, and each
field's expression can reference that parameter's current value:

```SCORE
melody = (C E G C)@{instrument: "Lead"}!1/2;
play melody@{freq: freq * 2};
```

This plays the same melody an octave up (equivalent to appending `'` to each
note, but expressed as an explicit transform).

## 8. Loop it

A composition that refers to itself keeps playing forever:

```SCORE
clock = (C E G C)@{instrument: "Lead"}!1/2 clock;
play clock;
```

## 9. Conditional loops with `choose`

`choose <predicate> <a> <b>` plays `a` if `<predicate>` is true, `b`
otherwise - checked against the parameters of the last note played before
it. Combined with self-reference (step 8), this lets a loop stop instead of
running forever:

```SCORE
verse = (C E G C)@{instrument: "Lead"; count: 0}!1/2;
outro = (C, C,)@{instrument: "Lead"}!2;
song = verse choose count < 3 (song@{count: count + 1}) outro;

play song;
```

`song` plays `verse`, then either loops back into itself (bumping `count`)
or falls through to `outro`, depending on whether `count` is still under 3.
This is the compile-time counterpart to step 9's live control: `choose`
picks a path based on parameters baked into the score itself, while a JS
callback reacts to state from outside it.

## 10. Live control from JS

Everything so far is fixed at compile time. To react to what's currently
playing - e.g. add drift, follow a live scale, or read UI state - author an
`InstrumentCallbackHandler` or `GlobalCallbackHandler` in the conductor
callback editor. See [conductor.md](conductor.md#callback-handlers-live-control)
for the interface and how it's wired into playback.
