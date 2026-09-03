# passes

A pass is a named stage of one graph. Each pass has the same shape, but keeps
its own values, rules, entries, planes, and callbacks. Calling `Run` processes
the passes in order, so a later pass can constrain its values from the result
immediately before it.

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
call. Root `Reset` preserves the first pass's callbacks and pipeline-wide mode
and wrapping settings while clearing dimensions, rules, values, and additional
passes. Reset prepares and initializes its replacement pass before discarding
the old pipeline; if the initialization hook raises, the old passes,
dimensions, values, and selected pass remain intact.

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
2. visits passes from index `0` through `TotalPassCount - 1`;
3. clears that pass's prior generated values, while retaining caller-assigned
   locks, then runs each pass that has value or rule definitions;
4. copies the preceding output into a pass with no definitions;
5. restores the pass selected by the caller, even if an exception occurs.

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
- the solver used inside each pass is the legacy greedy traversal solver. It
  assigns values as it walks the graph; it is not yet the future reference
  solver with maintained domains, entropy selection, propagation to a fixed
  point, contradiction reports, and backtracking.

These limits keep the current contract clear. More expressive cross-pass
queries and the reference propagating solver belong to later milestones, and
can be added without changing the meaning of the pass API documented here.

## native FPC and pas2js

The pass implementation and public callback types are written for both native
FPC and pas2js. The same `TGraph`, `SwitchToPass`, `PassGraph`, `ForEachPass`,
`Run`, and `RequirePrevious` calls are used on both targets.

The host program is responsible only for presentation: a console, Lazarus
form, canvas, WebAudio player, or other UI can read the same pass results. For
cross-target examples and tests, use an explicit deterministic selection
callback and compare pass values rather than relying on platform random-number
implementations to choose the same sequence.
