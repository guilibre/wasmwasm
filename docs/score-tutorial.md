# Score tutorial

This is a step-by-step introduction to the [score language](score-language.md).
It assumes you already have at least one instrument (see
[tutorial.md](tutorial.md)) compiled and available by name - we'll call it
`"Lead"` below.

## 1. Play a single note

```SCORE
play C;
```

`C` is one of the [built-in note constants](score-language.md#built-in-notes)
- an atomic value with a `freq` and a `dur`, but no instrument. Nothing will
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

`transform ... by ...` rewrites a parameter for everything inside it, and
can reference the parameter's current value:

```SCORE
melody = (C E G C)@{instrument: "Lead"}!1/2;
play melody |> transform freq by freq * 2;
```

This plays the same melody an octave up (equivalent to appending `'` to each
note, but expressed as an explicit transform).

## 8. Loop it

A composition that refers to itself keeps playing forever:

```SCORE
clock = (C E G C)@{instrument: "Lead"}!1/2 clock;
play clock;
```

## 9. Live control from JS

Everything so far is fixed at compile time. To react to what's currently
playing - e.g. add drift, follow a live scale, or read UI state - author an
`InstrumentCallbackHandler` or `GlobalCallbackHandler` in the conductor
callback editor. See [conductor.md](conductor.md#callback-handlers-live-control)
for the interface and how it's wired into playback.
