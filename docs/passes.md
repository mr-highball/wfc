# passes

A pass is a named stage of one graph. Each pass has the same shape, but keeps
its own values, rules, entries, planes, and callbacks. Both `Run` and
`TrySolve` process passes in order, so a later pass can constrain its values
from the result immediately before it.

This is useful when one rule set should not have to describe an entire result
at once. A first pass can lay terrain, a second can place foliage, and another
can eventually place roads or buildings.

The pass API is part of `TGraph`; a separate pipeline object is not required.

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

- values introduced by `AddValue`;
- `RuleGroups` and `Rules`;
- `Entry[X, Y, Z]`, including the default indexed property;
- `Planes`;
- `SelectionCallback`;
- `InvalidStateCallback`.

For example, adding `land` while `terrain` is selected does not add `land` to
the `foliage` rule set. Assigning an entry in one pass does not assign the entry
at the same coordinate in another pass.

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

Pass-local operations such as `AddValue`, entry access, rules, planes, and
callbacks operate on that pass. Pipeline operations such as `Run`, `Reshape`,
`SwitchToPass`, `WrapNeighbors`, and `Mode` forward to the root. `Reset` is the
exception: call it on the root graph. Calling it through `PassGraph` raises
`EInvalidOperation` rather than destroying the receiver during its own method
call. Root `Reset` preserves the first pass's callbacks, the pipeline `Seed`,
and pipeline-wide mode and wrapping settings while clearing dimensions, rules,
values, and additional passes. Reset prepares and initializes its replacement
pass before discarding the old pipeline; if the initialization hook raises,
the old passes, dimensions, values, selected pass, and seed remain intact.

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
remaining values, and can backtrack. More importantly for pass composition, it
stages the complete pipeline before changing any entry.

```pascal
LOptions := DefaultGraphSolveOptions;
if not LGraph.TrySolve(LOptions, LReport) then
  WriteLn('Failed pass: ', LReport.FailedPassIndex);
```

Defined passes solve against the staged output immediately before them. A
later definitionless pass stages a copy of the preceding result, with its own
caller locks taking precedence. Definitionless pass zero remains unchanged
because it has no input. Only after every pass validates does `TrySolve` commit
new generated values.

A contradiction or exhausted backtrack limit returns `False` with structured
evidence and leaves all passes, the caller's selected pass, and pre-call random
states unchanged. Malformed model or topology data raises an exception before
entry commit. Legacy selection/invalid-state callbacks and traversal hooks are
not invoked by `TrySolve`; `Run` remains available when that extension model is
required. Exact algorithm, constraint, counter, and error semantics are in the
[reference solver documentation](solver.md).

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
although changed output can still change the valid domains seen later.

Every `Run` and `TrySolve` rewinds all streams. Calls to `RandomIndex` outside
generation cannot perturb the next result, and `Reset` keeps the same seed.
During a legacy callback, `AGraph.RandomIndex` consumes the stream for the pass
currently being solved even if that callback temporarily switches
`CurrentPass`.

Set an explicit seed whenever a result must replay across processes or
targets:

```pascal
LGraph.Seed := $DEADBEEF;
LGraph.Run;
```

The exact contract, algorithm version, custom callback rules, and limits are
documented in [deterministic generation](determinism.md).

## current limitations

The implemented pass behavior is intentionally small and sequential:

- `RequirePrevious` can inspect only the immediately preceding pass;
- it checks only the same coordinate;
- there are no named-pass predicates, coordinate offsets, neighborhood
  queries, or general cross-pass expressions yet;
- pass dependencies are a linear creation order, not a dependency graph;
- an empty pass copies only the immediately preceding values;
- a failed `Run` restores pass selection but is not yet a transaction over
  generated cell values; changes completed before the failure can remain and
  the next run will clear and regenerate solver-owned output;
- `TrySolve` is transactional across the current linear pipeline, but there are
  no named overlays, dependency DAGs, selective regeneration, or bounded
  feedback between passes yet;
- the version-1 reference solver is unweighted and has no restart policy,
  timing data, or stable trace hash.

These limits keep the current contract clear. More expressive cross-pass
queries and specialized pass layers belong to later milestones and can be
added without changing the meaning of the pass API documented here.

## native FPC and pas2js

The pass implementation and public callback types are written for both native
FPC and pas2js. The same `TGraph`, `SwitchToPass`, `PassGraph`, `ForEachPass`,
`Run`, `TrySolve`, and `RequirePrevious` calls are used on both targets.

The host program is responsible only for presentation: a console, Lazarus
form, canvas, WebAudio player, or other UI can read the same pass results. For
cross-target examples and tests, assign an explicit `Seed`, route any
randomness used by custom callbacks through `AGraph.RandomIndex`, and compare
pass values in stable pass/index order.
