# passes

A pass is a named stage of one graph. Each pass has the same shape, but keeps
its own values, rules, entries, entry domains, planes, and callbacks. Passes
form an acyclic dependency graph. Both `Run` and `TrySolve` execute a stable topological plan;
the default legacy mode retains the original creation-order chain and
immediately-previous-pass constraints.

This is useful when one rule set should not have to describe an entire result
at once. A first pass can lay terrain, a second can place foliage, and another
can eventually place roads or buildings.

The pass API is part of `TGraph`; a separate pipeline object is not required.
This guide introduces pass identity and the compatibility behavior. The
[pass-DAG contract](pass-dags.md) specifies modes, dependency roles, named
cross-pass requirements, planning, selective regeneration, and reports.
The separate [bounded negotiation contract](pass-negotiation.md) specifies how
complete atomic pipeline rounds may chronologically reopen a prior assignment.
The [finite count contract](pass-counts.md) adds inclusive neighborhood ranges
with an explicit choice between matching offsets and distinct provider cells.

## labels and indices

A new `TGraph` exposes its first pass on first use:

- its index is `0`;
- its initial label is the empty string;
- `TotalPassCount` is `1`.

Pass-zero construction is deliberately lazy and occurs on the first graph API
operation. This gives a derived class time to finish a parameterized
constructor before its virtual pass factory and initialization hook run, and
keeps native FPC and pas2js lifecycle behavior identical. The laziness is not
otherwise observable through the public API: asking for `TotalPassCount`,
`PassGraph[0]`, rules, entries, or other pass-scoped state materializes pass
zero.

Pass indices are zero-based and remain stable until `Reset`. New passes are
appended in creation order. Labels are unique. `Reset` destroys all pass
graphs and creates a fresh pass zero, so saved `PassGraph` references are not
valid afterward.

Use `CurrentPass` to label or rename the currently selected pass:

```pascal
LGraph.CurrentPass := 'terrain';
```

Assigning `CurrentPass` does not select another existing label. It renames the
current pass and raises an exception if the new label is already in use. Use
`SwitchToPass` to select an existing pass or create a new one:

```pascal
LGraph.SwitchToPass('foliage');       //selects or creates foliage
LGraph.SwitchToPass('terrain');       //selects the existing terrain pass
LGraph.SwitchToPass(1);               //selects by zero-based index
LGraph.SwitchToPass('props', LIndex); //also returns the stable index
```

The selection is exposed through `CurrentPass` and `CurrentPassIndex`.
`TotalPassCount` includes pass zero.

## pass-scoped state

The usual graph API works against the selected pass. These members are
pass-scoped:

- values introduced by `AddValue`, including their positive relative weights;
- `RuleGroups` and `Rules`;
- `HasDefinition` and the canonical `CopyRegisteredValues` inspection snapshot;
- `Entry[X, Y, Z]`, including the default indexed property;
- `Planes`;
- `SelectionCallback`;
- `InvalidStateCallback`.

For example, adding `land` while `terrain` is selected does not add `land` to
the `foliage` rule set. Assigning an entry in one pass does not assign the entry
at the same coordinate in another pass.

`HasDefinition` checks both the deterministic value registry and the legacy
public `RuleGroups` dictionary. `CopyRegisteredValues` returns an independent
array in exact `AddValue` order, allowing domain adapters to detect a caller
that changed one view without the other before accepting or capturing a model.

Weights are pass-scoped with their values. Use `AddValue(Value, Weight)` when
registering a value or assign `Rules[Value].Weight` afterward. The complete
weight vector in one defined pass is independent of every other pass:

```pascal
LGraph.SwitchToPass('terrain');
LGraph.AddValue('land', 4);
LGraph.AddValue('water', 1);

LGraph.SwitchToPass('foliage');
LGraph.AddValue('tree', 1);
LGraph.AddValue('none', 8);
```

These weights affect `TrySolve`; legacy `Run` remains callback-driven and
uniform by default. A definitionless copying pass has no solver weight vector.

A newly created pass inherits the selection and invalid-state callbacks of the
pass immediately before it. The callback fields are separate after creation,
so they may be changed per pass:

```pascal
LGraph.PassGraph[0].SelectionCallback := SelectTerrain;
LGraph.PassGraph[1].SelectionCallback := SelectFoliage;
```

Derived graph classes are preserved for every pass. Override
`DoInitializePass` to initialize pass-local subclass fields; it runs for pass
zero, later passes, and the fresh pass zero created by `Reset`. Override
`DoCreatePass` only when the pass object itself needs a custom factory. A pass
is registered in `PassGraph` before `DoInitializePass` runs, so initialization
code may safely inspect its stable identity and the pipeline.

The hook is an initialization transaction for pass-local configuration. It
may add values and rules or set pass-local callbacks and subclass fields, but
it may not rename or switch passes, create another pass, reshape, run, reset,
or change pipeline-wide mode or wrapping. Those operations raise
`EInvalidOperation`; if initialization fails, registration of the new pass is
rolled back without leaving its label or index behind.

Callbacks receive the root graph as `AGraph`. During a run,
`AGraph.CurrentPass` and `AGraph.CurrentPassIndex` identify the pass being
solved.

## shared graph settings

All passes share one shape and coordinate system. `Reshape` applies the new
width, height, and depth to every existing pass. A pass created later receives
the current dimensions.

`WrapNeighbors` and `Mode` are also pipeline-wide. Changing either setting on
the root or through a `PassGraph` applies it to every pass. Changing wrapping
relinks the neighbors in each pass.

When a wrapped dimension has length one, its two directional links point back
to the same entry. Those self-arcs still enforce directional rules: a candidate
must be compatible with itself in each wrapped direction that returns to the
entry. Required self-support is candidate-specific: one candidate's required
self-rule cannot make another required-only candidate eligible.

This shared shape is what makes a coordinate such as `(3, 4, 0)` refer to the
same location throughout the pipeline, even though each pass has a different
entry object there.

## accessing a pass without selecting it

`PassGraph[Index]` returns the pass-local `TGraph` without changing
`CurrentPass`:

```pascal
LTerrain := LGraph.PassGraph[0];
LFoliage := LGraph.PassGraph[1];

WriteLn(LTerrain.Entry[2, 1, 0].Value);
WriteLn(LFoliage.Entry[2, 1, 0].Value);
```

An invalid index raises `ERangeError`. Pass graphs are owned by the root graph
and must not be freed by the caller. A pass graph reports its own stable label
and index through `CurrentPass` and `CurrentPassIndex`; the root graph reports
the pass currently selected by the coordinator.

Pass-local operations such as `AddValue`, entry access, allowed-value domains,
rules, planes, and callbacks operate on that pass. Pipeline operations such as
`Run`, `Reshape`, `SwitchToPass`, `WrapNeighbors`, and `Mode` forward to the root. `Reset` is the
exception: call it on the root graph. Calling it through `PassGraph` raises
`EInvalidOperation` rather than destroying the receiver during its own method
call. Root `Reset` preserves the first pass's callbacks, the pipeline `Seed`,
and pipeline-wide mode and wrapping settings while clearing dimensions, rules,
values and their weights, entry domains, and additional passes. Reset prepares
and initializes its replacement pass before discarding the old pipeline; if the initialization
hook raises, the old passes, dimensions, values, selected pass, and seed remain
intact.

`ForEachPass` is available when every pass should be inspected or configured.
It visits passes in index order and restores the caller's selected pass even
if a callback switches passes:

```pascal
procedure VisitPass(const AGraph: TGraph; const ALabel: String;
  const AIndex: Integer);
begin
  WriteLn(AIndex, ': ', ALabel, ' has ', AGraph.Planes.Count, ' plane(s)');
end;

LGraph.ForEachPass(VisitPass);
```

There are overloads for an ordinary procedure and an object method. They avoid
compiler-specific nested callbacks and are supported by both FPC and pas2js.
Native FPC also retains the original nested-procedure overload for source
compatibility. Pass a captured nested procedure without `@`; portable code
should prefer the ordinary procedure or object-method overload.

## running the pipeline

`Run` always coordinates the complete pipeline, including when it is called
through a `PassGraph`.

The coordinator:

1. saves the pass selected by the caller;
2. rewinds every pass's random stream from the pipeline `Seed`;
3. visits passes from index `0` through `TotalPassCount - 1`;
4. clears that pass's prior generated values, while retaining caller-assigned
   locks, then runs each pass that has value or rule definitions;
5. copies the preceding output into a pass with no definitions;
6. restores the pass selected by the caller, even if an exception occurs.

A selection or invalid-state callback may inspect or switch passes through the
root graph. After that callback returns, the coordinator resumes the pass it
was solving. When the full run ends, the original selection is restored.
Callbacks may switch to any existing pass, but cannot create a new pass while
the pipeline is running; attempting that raises `EInvalidOperation` and leaves
the pass registry unchanged.

An empty pass copies entry values from the pass immediately before it. Empty
entries remain empty. Copied values are marked as generated, so the snapshot
is refreshed on every run. Caller-assigned destination locks are preserved;
all other cells follow the preceding snapshot. Rules are not copied. Pass zero
has no preceding pass, so `Run` leaves a definitionless pass zero unchanged.

Assigning `Entry.Value` creates a caller-controlled lock. Locks survive later
runs; in a defined pass they are validated against directional and
previous-pass constraints, while in a definitionless pass they override that
cell of the copied snapshot. Solver output is exposed by the read-only
`Entry.Generated` property and is cleared and regenerated on the next run.
Assigning a generated value to `Entry.Value` again, even without changing its
text, promotes it to a lock. `ClearValue` removes either kind of value and
allows generation at that cell.

`SetAllowedValues(X, Y, Z, Values)` adds a caller-owned initial domain without
assigning a value. It is pass-local, canonicalized into that pass's `AddValue`
order, and persists when values are cleared or regenerated. `Run` and
`TrySolve` both intersect it with locks and other constraints. An assigned
empty array is an explicit contradiction; `ClearAllowedValues` removes the
domain instead. `HasAllowedValues` and `CopyAllowedValues` provide unambiguous,
detached inspection. Reshaping replaces the entry storage and clears domains
from every pass. Domain mutation while a pipeline is running is rejected.

If a lock violates its constraints, `InvalidStateCallback` may provide a valid
replacement. Without a repair, `Run` raises `EInvalidOperation`; it never
silently retains the invalid value.

`SelectionCallback` must likewise return one of the valid values it receives.
If it returns anything outside that domain, `InvalidStateCallback` gets one
chance to repair the choice. A missing or still-invalid repair raises
`EInvalidOperation` before the value is stored.

If filtering leaves an unassigned entry with no valid value at all,
`InvalidStateCallback` may repair mutable legacy model state and propose a
value. The graph recalculates the domain after the callback; the proposal must
belong to that refreshed domain. Otherwise `Run` raises `EInvalidOperation`
and leaves the entry unassigned instead of inventing a value. Writing directly
to the callback's mutable `AEntry` argument does not bypass this check; that
write is cleared before the proposal is validated.

## atomic reference solving

`TrySolve` is the opt-in alternative to the legacy traversal described above.
It maintains domains, propagates to a fixed point, observes by minimum
remaining values for unit-weight models or deterministic weighted Shannon
entropy otherwise, and can backtrack. More importantly for pass composition,
it stages the complete pipeline before changing any entry.

```pascal
LOptions := DefaultGraphSolveOptions;
LOptions.CaptureTrace := True;
if not LGraph.TrySolve(LOptions, LReport) then
  WriteLn('Failed pass: ', LReport.FailedPassIndex);
```

Trace capture is optional and defaults to false. When enabled, the report
contains one chronological transaction trace plus a contiguous
`TraceStart`/`TraceCount` slice for every pass. Cross-pass removals identify the
provider through `DependencyPassIndex` and link backward to that provider's
stage or skip event. `wfc_trace` supplies lookup, detached pass/entry queries,
formatting, structural/hash validation, and coordinate conversion. The
[causal-trace contract](traces.md) documents the event schema and shared
native/pas2js inspector.

Defined passes solve against the staged output immediately before them. A
later definitionless pass stages a copy of the preceding result, with its own
caller locks taking precedence. Definitionless pass zero remains unchanged
because it has no input. Only after every pass validates does `TrySolve` commit
new generated values.

A contradiction or exhausted backtrack limit returns `False` with structured
evidence and leaves all pass entries and caller domains, the caller's selected
pass, and pre-call random states unchanged. Malformed model or topology data
raises an exception before entry commit. Legacy selection/invalid-state
callbacks and traversal hooks are not invoked by `TrySolve`; `Run` remains
available when that extension model is required. Exact algorithm, constraint,
counter, and error semantics are in the [reference solver documentation](solver.md).

`TrySolveNegotiated` is the opt-in outer search for a different question: if a
complete provider assignment is locally valid but makes a later pass fail, can
another whole assignment work? It retains one ordinary report per atomic
round, excludes exact completed assignments in chronological pass order, and
commits only the first complete success. Its local solver budget and outer pass
budget are distinct rather than interchangeable; replaying exact exclusions
consumes local backtracks too.

`TryRegenerateNegotiatedFrom` uses that unchanged search only inside the exact
descendant closure of canonical requested roots. Clean ancestors and siblings
remain immutable inputs and never become choice frames. This repair horizon is
separately versioned and reported; it is not inferred from a contradiction or
widened on failure. See [selective pass negotiation](selective-negotiation.md).

The reusable [2D world ecosystem](world2d.md) applies this transaction to a
typed terrain → biome → foliage pipeline. Its separate validator and portable
layer signatures provide a domain-level check that the generic core does not
need to know about.

The [sequence adapter](sequences.md) composes context-bearing latent states
without exposing their private graph keys. A sequence pass can require its
projected public token from a named token pass, and an ordinary downstream
value can require one or more public tokens projected from a named latent
sequence pass. Both are exact same-coordinate `RequireFromPass` alternatives
and participate in the normal atomic dependency plan.

Projection maps can also relate different public vocabularies. The
[music foundation](music.md) makes a melody pass depend on both a latent rhythm
pass and a latent harmony pass: action must match rhythm, and every sounding
pitch class must match harmony. Those named dependencies are two ANDed groups;
each target-token map may contain multiple source-token alternatives. The
adapter expands only at the private-state boundary. Its N-source bundle
preflights every model, provider label, dependency edge, and complete map
before changing any pass rule, so a malformed later provider cannot leave an
earlier dependency partially installed. The standard
[text pass composition](text.md#pass-composition) uses the same mechanism for
structure -> lexical -> punctuation.

For definitions that must be exchanged without capturing a live graph,
`wfcrules=1` preserves exact hand-authored local rules and `wfcpipeline=1`
describes typed resources, pass modes, the dependency DAG, built-in projection
bridges, and public token requirements. The current immutable recipe layer and
its deliberate boundary before runtime compilation are documented in
[portable rules and pipeline recipes](pipeline-artifacts.md).

## constraints from the previous pass

`RequirePrevious` filters a value using the entry at the same coordinate in
the immediately preceding pass:

```pascal
LGraph.AddValue('tree').RequirePrevious('land');
LGraph.AddValue('scrub').RequirePrevious(['land', 'sand']);
LGraph.AddValue('none');
```

Here, `tree` is a candidate only when the previous pass contains `land` at the
same coordinate. `scrub` accepts either `land` or `sand`. `none` has no
previous-pass restriction and remains a candidate anywhere.

The supplied values are alternatives: matching any one is sufficient. If the
previous entry is empty, a value with `RequirePrevious` is rejected. Normal
directional rules are applied as well, so this is an additional filter rather
than a replacement for `NewRule`.

During `TrySolve`, the previous value comes from the current transaction's
staged output. A failed later pass therefore neither reads stale data nor
partially commits an earlier pass.

Calling `RequirePrevious` in pass zero raises `EInvalidOperation`, because
there is no earlier pass that could satisfy the constraint.

## terrain to foliage

This small example pre-seeds terrain, then lets the foliage pass place trees
only on land. The deterministic callback makes the output easy to inspect on
both native FPC and pas2js.

```pascal
program pass_example;

{$mode delphi}{$H+}

uses
  SysUtils,
  wfc;

function SelectFirst(const {%H-}AGraph: TGraph;
  const AEntry: TGraphEntry;
  const AValid: TGraphValues): TGraphValue;
begin
  if Length(AValid) = 0 then
    Result := AEntry.Value
  else
    Result := AValid[0];
end;

var
  LGraph: TGraph;
  X: Integer;
begin
  LGraph := TGraph.Create.Reshape(4, 1, 1);
  try
    LGraph.WrapNeighbors := False;

    //pass zero: label and seed the terrain
    LGraph.CurrentPass := 'terrain';
    LGraph.AddValue('land');
    LGraph.AddValue('water');

    for X := 0 to 3 do
      if (X mod 2) = 0 then
        LGraph.Entry[X, 0, 0].Value := 'land'
      else
        LGraph.Entry[X, 0, 0].Value := 'water';

    //pass one: tree is legal only over land; none is legal everywhere
    LGraph.SwitchToPass('foliage');
    LGraph.AddValue('tree').RequirePrevious('land');
    LGraph.AddValue('none');
    LGraph.SelectionCallback := SelectFirst;

    LGraph.Run;

    for X := 0 to 3 do
      WriteLn(
        LGraph.PassGraph[0].Entry[X, 0, 0].Value,
        ' -> ',
        LGraph.PassGraph[1].Entry[X, 0, 0].Value
      );
  finally
    LGraph.Free;
  end;
end.
```

The result is:

```text
land -> tree
water -> none
land -> tree
water -> none
```

The terrain entries remain unchanged because foliage owns a different set of
entries.

## deterministic pass streams

`Seed` belongs to the complete pipeline. Each pass receives its own portable
random stream derived from that seed and its stable zero-based index. Renaming
a pass or appending a later pass therefore does not change an existing pass's
stream. Extra random choices in one pass do not advance another pass's stream,
although a changed weight vector can change that pass's output and therefore
the valid domains seen later.

Every `Run` and ordinary `TrySolve` rewinds all streams. Negotiated solving
does the same for every complete round; rejected rounds restore their stream
state, and final success exposes only the winning round's state. Calls to
`RandomIndex` outside generation cannot perturb the next result, and `Reset`
keeps the same seed. During a legacy callback, `AGraph.RandomIndex` consumes
the stream for the pass currently being solved even if that callback
temporarily switches `CurrentPass`.

Set an explicit seed whenever a result must replay across processes or
targets:

```pascal
LGraph.Seed := $DEADBEEF;
LGraph.Run;
```

The exact contract, algorithm version, custom callback rules, and limits are
documented in [deterministic generation](determinism.md).

## current limitations

Version 2 dependency planning is deliberately acyclic. `RequirePrevious` and
`RequireFromPass` preserve same-coordinate compatibility;
`RequireFromPassAt` reads one signed offset, `RequireAnyFromPass` reads an
explicit finite any-of-neighborhood, and `RequireCountFromPass` bounds matches
over an explicit stencil. These are exact hard value comparisons, not radius
searches, global quotas, distance metrics, soft predicates, or a
conflict-directed repair language. Transform mode has one source. Sequence
projection helpers now bridge exact public tokens and private latent states in
both directions; exact N-source sequence token-map bundles are atomic.
`wfc_pattern2d_graph` expresses wrapped, same-shape, depth-one overlapping
pattern projection as one exact provider clause per footprint coordinate and
validates the materialized public pass independently at commit time. General
projection schemas remain future work. Generic pass rules can place projected
values in finite offset clauses, but adapters do not infer resampling,
arithmetic predicates, soft preferences, or arbitrary many-cell semantic joins.

The staged DAG remains one-way within ordinary `TrySolve`. A downstream
requirement filters its candidate domain against provider values already
staged earlier in topological order. If that downstream pass fails, the
transaction rolls back without reopening a provider. Selective regeneration
deliberately starts a new transaction over the chosen dependent closure.

Pass Negotiation v1 adds a separate bounded sequence of full one-way rounds.
It is global chronological rather than conflict-directed: an unrelated later
completed pass can consume budget before the provider named by the failure.
It excludes exact whole assignments, can be exponential, and does not permit
DAG cycles. Selective Negotiation v1 restricts the same search to an explicit
descendant-closed horizon but does not discover a minimal horizon or negotiate
clean providers. Soft objectives, partial or cell-minimal nogoods,
minimal-change repair, conflict-directed search, and cyclic fixed points remain
research work rather than implied properties.

A failed legacy `Run` restores pass selection but is not a transaction over
generated cell values. `TrySolve` and `TryRegenerateFrom` are transactional.
The version-2 reference solver has a stable opt-in trace hash and a checked
console inspector. The separate [restart coordinator](restarts.md) adds
whole-transaction budget-limited retries and optional elapsed timing; it does
not change ordinary or selective solving. Trace v1 still does not provide
interactive stepping, live domain snapshots, complete
failed-clause/minimal-core explanations, an event cap, or streaming capture.

## native FPC and pas2js

The pass implementation and public callback types are written for both native
FPC and pas2js. The same `TGraph`, `SwitchToPass`, `PassGraph`, `ForEachPass`,
`DependsOn`, `TransformFrom`, `Run`, `TrySolve`, `TrySolveNegotiated`,
`TrySolveRestarted`, `TrySolveNegotiatedRestarted`,
`TryRegenerateFrom`, `TryRegenerateNegotiatedFrom`,
`RequirePrevious`, `RequireFromPass`, `RequireFromPassAt`, and
`RequireAnyFromPass`, and `RequireCountFromPass` calls are used on both targets.
`CaptureTrace`, portable
trace hashes, full and selective negotiation transcript hashes, per-pass slices, and the
`wfc_trace` query/validation helpers have matching native FPC and pas2js
fixtures as well.

The host program is responsible only for presentation: a console, Lazarus
form, canvas, WebAudio player, or other UI can read the same pass results. For
cross-target examples and tests, assign an explicit `Seed`, route any
randomness used by custom callbacks through `AGraph.RandomIndex`, and compare
pass values in stable pass/index order.
