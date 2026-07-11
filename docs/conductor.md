# Conductor: running a compiled score

`score_compiler` compiles [score language](score-language.md) source into a
JSON graph. `Conductor` (`frontend/src/audio/conductor.ts`) interprets that
graph in the browser, driving `patch_compiler` instrument instances in real
time. This document describes that runtime.

## Compile pipeline

```text
score source -> compile_score (score_compiler, Emscripten) -> JSON ScoreGraph
```

`compile_score` is exported from `score_compiler/app/bindings.cpp` and wraps
`compile_to_json`. The frontend's `ScoreGraph`/`GraphNode`/`ExprNode`/
`TransformEntry` types (`frontend/src/audio/conductor.ts`) mirror the C++
structs written by `score_compiler/src/backend/codegen.cpp`. `ExprNode.op`
(for `{kind: 'binary'}` nodes) is one of `add sub mul div mod pow eq neq lt
gt lte gte and or`, matching the score language's
[expression operators](score-language.md#expressions).

A graph node has a `kind`:

| kind             | meaning                                               |
| ---------------- | ------------------------------------------------------ |
| `state`          | a single instrument event (`params`, optional `instrument`) |
| `fork`           | spawns one token per outgoing branch                  |
| `join`           | waits for `joinArity` incoming tokens before continuing |
| `passthrough`    | plumbing node (self-reference loops), no-op at runtime |
| `transform_push` | pushes a parameter transform onto the token's stack   |
| `transform_pop`  | pops the most recent transform                        |
| `branch`         | evaluates `cond` (an `ExprNode`) against the token's current params, then follows `next[0]` if true or `next[1]` if false - compiled from [`choose`](score-language.md#choose) |

`ScoreGraph.entries` lists one node-id list per `play` statement - the
starting points for tokens when playback begins.

## Token-based execution model

`Conductor` advances a list of **tokens** each audio tick. A token tracks:

- `node_id` - its current position in the graph
- `seconds_remaining` - time left before its current `state` node finishes
- `instance_id` / `instrument_id` - the currently-instantiated instrument voice, if any
- `params` - the resolved parameters for the current `state`
- `transform_stack` - active `transform_push` frames inherited from ancestor nodes

On construction, `Conductor` seeds one token per graph entry and immediately
walks each forward (`walk_forward`) through any leading `passthrough`,
`fork`, `transform_push`/`pop`, `branch`, or `join` nodes until it reaches a
`state` node or dead-ends.

`tick(num_samples)` is called once per audio-processing block:

1. Subtract elapsed time from every token's `seconds_remaining`.
2. For each token that has reached zero: destroy its instrument instance (if
   any), advance it past its current node, and re-walk it forward.
3. Newly produced tokens (from forks, or continuing sequences) are appended.

Graph walking inside a single tick is bounded by `max_steps_per_tick` (64) -
if a cycle produces events with zero duration, `Conductor` throws rather than
looping forever. Within a single `walk_forward` call, revisiting the same
node before reaching a `state` (a cycle of `fork`/`join`/`branch`/
`transform_push`/`transform_pop`/`passthrough` nodes with no `state` in
between) is also detected and throws immediately, rather than spinning
forever inside that call.

## Entering a `state` node

When a token reaches a `state` node (`enter_state`):

1. Its `transform_stack` is applied outermost-in: each active `transform`
   entry re-evaluates its expression against the current params and
   overwrites (or, if it evaluates to `null`, deletes) that parameter; the
   most recently pushed `pushInstrument` wins.
2. `dur` (in beats) is converted to seconds using the current BPM
   (`60 * dur / bpm`) and added to the token's `seconds_remaining`.
3. If an instrument is set, `Conductor` calls `instantiate(id)` on that
   instrument's `InstrumentExports` (from `patch_compiler`'s compiled
   output), then `set_param` for each resolved parameter that the
   instrument exposes (via a per-instrument `param_index`).

## Legato

A `state` node produced by the score language's `~` operator carries two
extra params: `legato` (1 on every note but the last of a `~` chain, 0 on
the last) and `legato_id` (unique per chain, including per copy produced by
`repeat`/`reverse`).

When entering a `state` whose `legato_id` matches a chain `Conductor` is
already tracking (`legato_voices`, keyed by `legato_id`), it reuses that
chain's `instance_id` instead of calling `instantiate` again - only
`set_param` runs, so the instrument voice glides into the new params instead
of being retriggered. `instantiate` only runs for the first note of a chain
(or any note with no `legato_id` at all). After resolving params, `legato:
1` records the voice under `legato_id` for the next note to pick up;
`legato: 0` removes it, since the chain ends there. If a chain's instrument
name were to change mid-chain, the voice isn't reused (a fresh `instantiate`
runs instead) to avoid handing one instrument's voice to another.

## Wiring instruments in

`Conductor` is constructed with:

- `graph: ScoreGraph` - the compiled score
- `param_index: ParamIndex` - maps `instrument name -> param name -> index`
- `exports: InstrumentExportsMap` - maps instrument name to its compiled
  `{ instantiate, set_param }` (produced by `patch_compiler`)
- `sample_rate`, `bpm`
- optionally `global_exports: GlobalExports`, for parameters not tied to any
  one instrument instance

## Callback handlers (live control)

Two kinds of user-authored JS handlers can hook into playback, edited via
`conductor_callback_editor.tsx` in the UI:

- **`InstrumentCallbackHandler`** - one per instrument name. Called with the
  token's resolved params and the full active-token list just before an
  instrument is instantiated; its return value overrides the params sent to
  `set_param`.
- **`GlobalCallbackHandler`** - called once per `state` entry (regardless of
  instrument) with the active-token list; its return value is applied via
  `GlobalExports.set_param` for any matching global parameter name.

Both receive `TokenParams[]` (`{ instrument_id, params }` for every currently
active token), which lets a handler react to what else is playing right now -
e.g. detuning a voice based on how many other notes are active.

### Scale functions

For every scale declared in the score (`major = [...]`, see
[score-language.md](score-language.md#scale-declarations)), the worklet
synthesizes a same-named function available directly in callback code - no
extra parameter needed:

```js
major(degree, octave) // -> values[((degree % n) + n) % n] * 2^floor(degree/n) * 2^octave
```

where `n` is the scale's length. It returns a plain multiplier; combine it
with your own root frequency and the note's `scale`/`degree`/`octave` params
(see [score-language.md](score-language.md#scale-declarations)) to compute
`freq`:

```js
class GlobalCallbackHandler {
    call(ap) {
        const p = ap[0]?.params;
        if (!p || p.degree === undefined || p.octave === undefined) return {};
        return { freq: 440 * major(p.degree, p.octave) };
    }
}
```

A `scale` param (as set by `@{scale: <name>}`, see
[score-language.md](score-language.md#scale-declarations)) is only a numeric
index, not itself callable - to dispatch on it dynamically, use the
also-synthesized `scales` array, indexed in the same order as the compiled
graph's `scales`:

```js
class GlobalCallbackHandler {
    call(ap) {
        const p = ap[0]?.params;
        if (!p || p.scale === undefined || p.degree === undefined || p.octave === undefined)
            return {};
        return { freq: 440 * scales[p.scale](p.degree, p.octave) };
    }
}
```

The in-browser callback editor also type-checks calls to these - `conductor_panel.tsx`
recompiles the current score source and pushes
`declare function <name>(degree: number, octave: number): number;` plus
`declare const scales: ((degree: number, octave: number) => number)[];` ambient
declarations into the shared TS environment (`ts_env.ts`) whenever the score
or scale definitions change.

## UI

- `score_panel.tsx` - the score source editor, paired with `score_graph_view.tsx`
  for a visual rendering of the compiled graph.
- `conductor_panel.tsx` / `conductor_callback_editor.tsx` - authoring
  `InstrumentCallbackHandler`/`GlobalCallbackHandler` code.
- `instrument_tabs.tsx` - switching between and managing multiple instruments
  within a project.
