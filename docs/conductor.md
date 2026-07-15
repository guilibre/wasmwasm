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

| kind             | meaning                                                                                                                                                                        |
| ---------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------ |
| `state`          | a single instrument event (`params`, optional `instrument`)                                                                                                                    |
| `fork`           | spawns one token per outgoing branch                                                                                                                                           |
| `join`           | waits for `joinArity` incoming tokens before continuing                                                                                                                        |
| `passthrough`    | plumbing node (self-reference loops), no-op at runtime                                                                                                                         |
| `transform_push` | pushes a parameter transform onto the token's stack - compiled from [`@{...}`](score-language.md#atomic-join---), or (with `listenChannel` instead of `transforms`) from [`listen`](score-language.md#listen) |
| `transform_pop`  | pops the most recent transform                                                                                                                                                 |
| `branch`         | evaluates `cond` (an `ExprNode`) against the token's current params, then follows `next[0]` if true or `next[1]` if false - compiled from [`choose`](score-language.md#choose) |
| `signal_emit`    | writes `params`/`instrument` to the global `signalId` mailbox, then continues - compiled from [`emit`](score-language.md#emit)                                                 |
| `reverse`        | walks the subgraph rooted at `reverseBodyEntryId`/`reverseBodyExitId` backward - compiled from [`reverse`](score-language.md#reverse)                                          |
| `legato`         | marks the token so the next `state` it reaches reuses/sustains a voice, carrying `legatoId` - compiled from [`~`](score-language.md#legato---)                                |
| `skip`           | adds `skipCount` to the token's remaining skip counter - compiled from [`skip`](score-language.md#skip)                                                                        |
| `repeat`         | walks the subgraph rooted at `repeatBodyEntryId`/`repeatBodyExitId` `repeatCount` times in sequence, looping back to the entry each time the exit is reached - compiled from [`repeat`](score-language.md#repeat) |

`reverse`, `skip`, and `repeat` are the only pipe operators resolved at
playback time rather than compile time - see [Reverse](#reverse) below. None
of them clone the body they wrap, so none of them make the compiled graph
grow with the size of a count/parameter (`n` in `repeat n`/`skip n`) - only
a single marker node is added regardless of how large `n` is.

While a token's remaining skip counter is above zero, each `state` node it
reaches is treated as a no-op passthrough (no instrument instantiated, no
params applied) and the counter is decremented instead of the state being
entered - only once the counter reaches zero does the next `state` play
normally. Forking while the counter is nonzero copies it onto every child
token, so each branch counts down independently from the fork point.

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

1. Its `transform_stack` is applied outermost-in: each active frame either
   re-evaluates its `transform` entries against the current params and
   overwrites (or, if an entry evaluates to `null`, deletes) that parameter,
   or - if the frame has a `listenChannel` instead - merges in whatever
   `params`/`instrument` are currently in `Conductor`'s `signals` map under
   that channel (see [Signals](#signals) below), with no effect if that
   channel has never been written to; the most recently pushed
   `pushInstrument` wins over either.
2. `dur` (in beats) is converted to seconds using the current BPM
   (`60 * dur / bpm`) and added to the token's `seconds_remaining`.
3. If an instrument is set, `Conductor` calls `instantiate(id)` on that
   instrument's `InstrumentExports` (from `patch_compiler`'s compiled
   output), then `set_param` for each resolved parameter that the
   instrument exposes (via a per-instrument `param_index`).

## Reverse

`reverse` (`expr |> reverse`) is not resolved at compile time like the other
pipe operators - `score_compiler` emits a single `reverse` node that points
at `expr`'s untouched body (`reverseBodyEntryId`/`reverseBodyExitId`), and
`Conductor` walks that body backward while inside it, using a `prev`
adjacency map it builds once (lazily, from every node's `next`) at
construction time. While walking backward:

- `fork` and `join` swap roles (a `fork` encountered backward behaves like a
  `join`, and vice versa), since a fork's outgoing branches become converging
  paths when walked in reverse.
- `transform_push` and `transform_pop` swap roles the same way, using
  `transformPopOfPush` (also serialized into the graph) to find the paired
  node's `transforms`/`listenChannel` when a `transform_pop` needs to act as
  a push.
- `state`, `passthrough`, `signal_emit`, `legato`, and `branch` behave the
  same, just following `prev` edges instead of `next` - a `legato` node
  needs no special-casing here, since marking "the next node reached"
  works identically regardless of which direction that next node is
  reached from.

A token records its current `direction` (`'forward'` or `'backward'`) so
that later, when it advances past a `state` node whose duration has
elapsed, it continues stepping the right way even outside of `walk_forward`.
Nesting a `reverse` inside another `reverse` flips the direction again, back
to forward.

## Repeat

`repeat` (`expr |> repeat n`) is, like `reverse`, resolved at playback time
rather than compile time - `score_compiler` compiles `expr`'s body exactly
once and emits a single `repeat` node pointing at it
(`repeatBodyEntryId`/`repeatBodyExitId`/`repeatCount`), instead of cloning
the body `n` times. This keeps the compiled graph's size independent of
`n`, however large.

Each token tracks a `repeat_stack` of active repeat frames (`{
repeat_node_id, body_entry_id, body_exit_id, remaining }`), pushed when the
token enters a `repeat` node (jumping to `body_entry_id`) and popped when it
reaches `body_exit_id`:

- If `remaining > 0`, the token loops back to `body_entry_id` and
  `remaining` is decremented - the body plays again from the top.
- If `remaining === 0`, the token continues past the `repeat` node to
  whatever follows it in the static graph (`succs_of(repeat_node, ...)`).

Because the body is the same set of graph nodes on every iteration (not a
fresh clone), any state a node carries across visits - most notably a
`legato` node's id, and `join_arivals`/`transform_stack` bookkeeping - is
naturally shared and correctly reused between repetitions, the same way it
would be for a plain non-repeated sequence played twice in a row.

Forking (`&`) inside a repeated body copies the current token's
`repeat_stack` onto every child token, so each branch continues counting
down its own remaining repetitions independently - the same pattern used
for `skip_remaining` and `transform_stack`. `repeat` composes with `reverse`
by tracking `body_entry_id`/`body_exit_id` relative to the token's current
walk direction at the moment the `repeat` node is entered, so a `repeat`
nested inside a `reverse` (or vice versa) loops through the body in
whichever direction it's currently being walked.

## Legato

The score language's `~` operator compiles to a dedicated `legato` graph
node (see the kind table above), not a param on a `state`. Each `~` chain
shares one `legatoId` across every `legato` node it produces (a fresh id
per chain). Since `repeat` no longer clones the body it wraps, a `~` chain
inside a repeated body keeps the same `legatoId` across every repetition -
the runtime's per-id voice bookkeeping is unaffected by how many times the
same `legato` node is walked through.

While walking the graph (`walk_forward`), passing through a `legato` node
records its `legatoId` on the token (`pending_legato_id`), to be consumed
by the next `state` the token reaches. In `enter_state`:

- The id used to look up an existing voice in `legato_voices` is whatever
  the token is carrying (`pending_legato_id`, cleared immediately after
  reading) - i.e. the id of the `legato` node most recently passed through
  on the way to this `state`.
- Whether to *keep* that voice alive for the next note is decided by
  looking ahead: `Conductor` follows this `state`'s outgoing edge,
  transparently skipping over any `passthrough`/`transform_push`/
  `transform_pop` nodes in between, to see whether a `legato` node comes
  next. If it does, the voice just used is recorded under that node's
  `legatoId` for the next `state` to pick up; if not, any voice recorded
  under the id this `state` itself carried is removed, since the chain
  ends here.

If entering a `state` whose lookup id matches a chain `Conductor` is
already tracking, it reuses that chain's `instance_id` instead of calling
`instantiate` again - only `set_param` runs, so the instrument voice glides
into the new params instead of being retriggered. `instantiate` only runs
for the first note of a chain (or any note with no matching id at all). If
a chain's instrument name were to change mid-chain, the voice isn't reused
(a fresh `instantiate` runs instead) to avoid handing one instrument's
voice to another.

## Signals

`Conductor` keeps a single `signals: Map<string, {params, instrument?}>`,
global for the whole graph and the whole run - not scoped to a token, a
`play` statement, or a `repeat` copy. A `signal_emit` node
(compiled from [`emit`](score-language.md#emit)) writes to it (last write
wins, whichever token reaches it first in real time); a `transform_push`
frame with `listenChannel` set (compiled from
[`listen`](score-language.md#listen)) reads from it in `enter_state`, as
described above. This is the only channel through which two independent
tokens - two `play` machines, or two `fork`/`&` branches that never rejoin -
can affect each other's params at runtime; everything else in the graph is
local to a single token's lineage.

Because reads happen live against whatever the map currently holds, the
same score can resolve a `listen` differently across runs (or across small
edits to unrelated timing) if it changes which voice's `emit` lands first -
this is expected, not a bug: `emit`/`listen` is explicitly for live,
order-dependent coupling between voices, unlike the compile-time-resolved
`@{...}`.

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

### Graph view and piano roll

`score_graph_view.tsx` renders the compiled `ScoreGraph` as a node diagram
(ReactFlow/ELK). To keep long melodies readable, consecutive `state` nodes
with nothing but a straight, single-entry/single-exit link between them
(no `fork`/`join`/`branch`/`transform_push`/`transform_pop` in the way) are
compacted into one visual box showing a short summary (instrument, note
count, total duration) instead of every note.

Double-clicking a node opens a **piano roll** (`score_piano_roll.tsx`) -
a `<canvas>` view with time on the X axis and pitch (`-log2(freq)`) on the
Y axis, colored by instrument. It's built by *simulating* playback from that
node with `ScoreTracer` (`frontend/src/audio/score_timeline.ts`) - a dry-run
reimplementation of `Conductor`'s `walk_forward`/`enter_state` (no
`AudioContext`, no instrument instantiation) that records `{start_seconds,
dur_seconds, freq, instrument, params}` events instead:

- If the double-clicked node was a compacted box, the trace stops right
  after that box's last node, so the piano roll shows only what's
  compacted there - not everything that plays afterward.
- If the graph loops back through a `state` the trace has already seen (a
  self-referential composition, see
  [Self-reference](score-language.md#self-reference-loops) and
  [choose](score-language.md#choose)), the trace instead reports a single
  **cycle** of events (one lap through the loop, with its `transform_stack`
  carried over exactly as it would be at runtime, so parameters that evolve
  across iterations - e.g. a `choose` loop counter - show their real value)
  rather than looping forever. Rests/silent states (no `instrument`, or no
  resolvable `freq`) are filtered out before drawing, since they aren't
  notes. Safety caps (`MAX_EVENTS`, `MAX_CYCLES`, `MAX_TOKEN_STEPS`) bound
  the simulation so a runaway score can't hang the tab.
