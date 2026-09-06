# Rooted port connectivity

Local adjacency does not ensure that two towns share a road, or that an
upstairs room is reachable from an entrance. `TGraph.RequireConnectivity`
adds that global condition to the ordinary reference solver. It propagates
during search, participates in backtracking, and receives an independent
final traversal before a pass transaction commits.

The opt-in contract is `WFC_GRAPH_CONNECTIVITY_VERSION = 1`. It adds no
third-party dependency. Existing models without connectivity descriptors keep
their original choices, random consumption, trace encodings, and versions.

## A small complete example

```pascal
program ConnectedCorridor;
{$mode delphi}{$H+}
uses SysUtils, wfc;
var
  Graph: TGraph;
  Root: TGraphPosition;
  Terminals: TGraphPositions;
  Profiles: TGraphConnectivityValues;
  Report: TGraphSolveReport;
begin
  Graph := TGraph.Create;
  try
    Graph.Seed := 42;
    Graph.Reshape(5, 1, 1);
    Graph.WrapNeighbors := False;
    Graph.AddValue('empty');
    Graph.AddValue('road');
    Root.X := 0; Root.Y := 0; Root.Z := 0;
    SetLength(Terminals, 1);
    Terminals[0] := Root;
    Terminals[0].X := 4;
    SetLength(Profiles, 1);
    Profiles[0] := MakeGraphConnectivityValue('road', [gdEast, gdWest]);
    Graph.RequireConnectivity(MakeGraphConnectivityConstraint(
      'entrance-to-exit', Root, Terminals, Profiles));
    if not Graph.TrySolve(DefaultGraphSolveOptions, Report) then
      raise Exception.Create('No connected corridor');
    WriteLn(Graph.Entry[2, 0, 0].Value); // road: the middle is a bottleneck
  finally
    Graph.Free;
  end;
end.
```

The graph forces all five cells to participate without needing a decision.
The size is only this example's fixture, not a library limit.

## Exact meaning

A descriptor has a nonempty `LabelText`, one explicit `Root`, optional fixed
`RequiredPositions`, participating `Values`, and `RequireAllParticipants`.
Each value profile contains its public value, `Openings`, and
`RequiredByValue` (default false). Unprofiled values are nonparticipants;
a profiled value with no openings is an isolated participant.

For a complete assignment:

- The root and every fixed required coordinate must select participating
  values and belong to the root's connected component.
- Every selected value whose profile has `RequiredByValue = True` must
  belong to that component.
- With `RequireAllParticipants = True`, every selected profiled value must
  belong to that component. With false, optional disconnected participants
  are allowed. The constraint does not silently remove those components.
- A traversable edge needs an opening in the movement direction at the
  current cell, the inverse opening at its neighbor, reciprocal actual
  neighbor links, and a mutually compatible value pair under ordinary
  adjacency rules. Two nearby road-looking values need not be connected.

All registered descriptors are ANDed. Separate descriptors can describe
different networks over the same assignment. A descriptor does not create
an upstream dependency or read another pass. Apply terrain requirements to
road candidates, require connectivity on that road pass, then let housing
read the resulting roads. A downstream terminal cannot retroactively become
an upstream requirement: model its coordinate/role in the routing pass too.

North increases Y, east increases X, and up increases Z, as in the graph's
existing coordinate convention. All six directions work. Wrapping can alias
directions to the same physical cell; it does not duplicate a terminal or
connect a self-loop to another component. Callers can require several
entrances by choosing one root and listing the others as terminals. This is
not the weaker condition "each room reaches any entrance".

## Ownership and editing

`RequireConnectivity` returns the receiver for fluent chaining and targets
its active pass. A `PassGraph` receiver always targets its own pass.
Registration deeply copies records, arrays, and sets. Profiles are stored in
canonical `AddValue` order; fixed coordinates are sorted in flattened cell
order and deduplicated. Duplicate profile values are rejected, even when the
duplicate profiles look identical. All profile values must already be
registered and nonempty, and all root/terminal coordinates must be in bounds.

Registering the same label and canonical descriptor again is idempotent.
Reusing the label with a different descriptor raises an exception before
changing the graph. Use `RemoveConnectivity(Label)` then
`RequireConnectivity(...)` to replace it; these are two explicit operations,
not an atomic replacement API. Removing an unknown label is a no-op.
`ClearConnectivity` clears only the receiver's pass.
`CopyConnectivityConstraints` returns detached descriptions in registration
order; their zero-based ordinals are used by diagnostic reports.

Constraint edits while running are rejected. `Reshape` preserves coordinate
anchors and preflights every pass before changing any entries: shrinking
past a root or terminal fails atomically. `Reset` creates fresh passes and
removes the old metadata. Changing a constraint does not itself regenerate
or hide already committed entries; application owners should mark their
public result dirty until a new solve succeeds.

## Search, transactions, and diagnostics

The feature works with `TrySolve`, selective regeneration, ordinary and
selectively scoped pass negotiation, and restart scheduling. It uses the
existing local/pass backtrack budgets; no special retry budget is silently
added. Legacy greedy `Run` rejects a pipeline containing connectivity before
rewinding random streams or changing entries. It cannot safely promise this
global condition.

The solver builds a possible graph from current domains. It rejects
unreachable mandatory cells, removes required-by-value candidates outside
the possible root component, and forces participation at mandatory cut
vertices. Root and terminal participation also propagate. Deletions use the
ordinary reversible domain trail, and the graph is recomputed after branch
restoration. Iterative graph walks avoid a recursion-depth ceiling.

Possible edges may use different candidate values at a shared cell. A
possible path is therefore not proof that one consistent assignment can use
the whole path. The propagator is sound but does not claim generalized arc
consistency. Complete assignments receive an exact check inside search;
failures backtrack. A separate public-value traversal guards the staged
candidate before commit. Entry hooks and domain commit hooks remain subject
to the ownership and rollback guards. Constrained-pass neighbor links are
also snapshotted: a commit hook cannot redefine the validated topology, even
on a reused layer. Rollback restores those links without invoking the hooks.
Live compatibility is traversed again after the domain commit hook. Direct
legacy `Rules`/`RuleGroups` edits are caller-owned model side effects, not
part of the entry/neighbor journal: an invalidating edit cannot publish a
disconnected result, but the caller must restore its model before retrying.
Commit validators should inspect rather than redefine the model.

Selective regeneration validates connectivity on preserved passes before
using them. If an edited descriptor invalidates a preserved layer, it raises
an exception asking the caller to include that pass in regeneration. It
does not rewrite an upstream layer outside the selected closure. A failed
active solve retains the prior entries and random state, as before.

`gckConnectivity` contradictions and `gtckConnectivity` trace causes carry
`ConstraintIndex`; ordinary reports/events use `-1`. The index belongs to the
reported pass's descriptor registry. It is not a cell index or a minimal
unsatisfiable set. Connectivity event hashes and contradiction transcript
hashes include the opt-in version and ordinal; old causes keep their original
byte encoding. Trace inspection uses `wfc_trace`, including the independent
validator and the `connectivity=N` formatted field. Inspect traces against
the same model metadata that produced them.

## Current boundary

This is an experimental global graph primitive, not a shortest-path,
minimum-road, flow-capacity, directed reachability, or biconnectivity solver.
It does not require every port to be paired: use adjacency/domain rules to
forbid dangling ports when the domain needs that condition. It adds no
arbitrary cell cap, but finite graph storage, checked integer capacities,
available memory, and search complexity remain real constraints.

The fluent graph API and [portable recipe extension](pipeline-connectivity.md)
both preserve these descriptors. Connectivity-bearing recipes use
`wfcpipeline=3`; training sources do not yet author connectivity profiles.
The older settlement and voxel adapters keep their existing
versioned semantics; in particular, their previous post-generation
reachability checks have not silently changed into this new solver mode.

See the [Connected Routes demo](../examples/passes/06_ConnectedRoutes/README.md)
for terrain/roads/housing and multi-floor circulation, and the
[research record](research/rooted-connectivity-v1.md) for the finite evidence,
soundness argument, counterexample, and remaining work.
