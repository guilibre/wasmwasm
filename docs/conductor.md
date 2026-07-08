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
structs written by `score_compiler/src/backend/codegen.cpp`.

A graph node has a `kind`:

| kind             | meaning                                               |
| ---------------- | ------------------------------------------------------ |
| `state`          | a single instrument event (`params`, optional `instrument`) |
| `fork`           | spawns one token per outgoing branch                  |
| `join`           | waits for `joinArity` incoming tokens before continuing |
| `passthrough`    | plumbing node (self-reference loops), no-op at runtime |
| `transform_push` | pushes a parameter transform onto the token's stack   |
| `transform_pop`  | pops the most recent transform                        |

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
`fork`, `transform_push`/`pop`, or `join` nodes until it reaches a `state`
node or dead-ends.

`tick(num_samples)` is called once per audio-processing block:

1. Subtract elapsed time from every token's `seconds_remaining`.
2. For each token that has reached zero: destroy its instrument instance (if
   any), advance it past its current node, and re-walk it forward.
3. Newly produced tokens (from forks, or continuing sequences) are appended.

Graph walking inside a single tick is bounded by `max_steps_per_tick` (64) -
if a cycle produces events with zero duration, `Conductor` throws rather than
looping forever.

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

## UI

- `score_panel.tsx` - the score source editor, paired with `score_graph_view.tsx`
  for a visual rendering of the compiled graph.
- `conductor_panel.tsx` / `conductor_callback_editor.tsx` - authoring
  `InstrumentCallbackHandler`/`GlobalCallbackHandler` code.
- `instrument_tabs.tsx` - switching between and managing multiple instruments
  within a project.
