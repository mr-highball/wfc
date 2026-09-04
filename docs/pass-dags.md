# pass dependency DAGs and selective regeneration

The original pass pipeline answers an important design question: how can a
generator add information in stages without forcing terrain, ecology, roads,
housing, and decoration into one combinatorial value space? A dependency DAG
extends that idea beyond a single chain. Independent branches can read a
shared foundation, later passes can join those branches, and a local change can
regenerate only the layers that actually depend on it.

This document defines the version-1 dependency and selective-regeneration
model. The implementation remains part of `TGraph`; there is no external
scheduler or runtime dependency.

## formal model

Let `P` be the finite set of passes. Every pass has:

- a stable zero-based creation index;
- a unique, mutable label;
- a pass mode;
- a stable-index-ordered set of direct dependency edges; and
- pass-local values, rules, locks, generated output, and random stream.

An edge `A -> B` means that `B` depends on committed or staged output from
`A`. The graph must remain acyclic. References bind to stable indices when
they are declared, so later renaming changes presentation but never retargets
an edge or constraint.

Execution uses a stable Kahn topological order. Whenever more than one pass is
ready, the pass with the lowest creation index runs first. This makes the
order deterministic without making creation order a dependency. `ForEachPass`
continues to enumerate creation order because it is an inspection API, not an
execution scheduler.

The versioned contract is identified by:

```pascal
WFC_PIPELINE_ALGORITHM_VERSION = 1
```

## dependency roles

An edge may serve more than one role:

- an explicit ordering/data dependency added by `DependsOn`;
- the protected predecessor edge required by legacy mode;
- the single protected source of a transform pass;
- the protected predecessor used by `RequirePrevious`; or
- a protected source used by `RequireFromPass`.

The implementation retains these roles rather than storing an untyped Boolean
edge. `RemoveDependency` and `ClearDependencies` therefore cannot silently
disconnect a transform source or invalidate a registered rule. Duplicate
explicit dependencies and removal of an absent optional edge are idempotent;
self-dependencies and cycles are rejected immediately and atomically.

## pass modes

`TGraphPassMode` makes copy behavior explicit while preserving existing code:

- `gpmLegacy` is the default. A defined pass solves a fresh pass-local layer;
  definitionless pass zero is retained; and a later definitionless pass copies
  its immediate predecessor. The predecessor chain is protected.
- `gpmOverlay` owns an independent named layer. A defined overlay solves that
  layer; a definitionless overlay retains caller locks and clears previous
  generated cells.
- `gpmTransform` has exactly one source selected by `TransformFrom`. A defined
  transform solves its output vocabulary against declared input constraints;
  a definitionless transform copies its source, with destination locks taking
  precedence.

Legacy mode is not an alias for either new mode. Its historical combination of
fresh defined passes and copying definitionless passes remains intact so
existing pipelines retain byte-for-byte behavior.

An overlay branch can replace its inherited legacy edge and declare its real
inputs explicitly:

```pascal
Graph.SwitchToPass('biome');
Graph.PassMode := gpmOverlay;
Graph.ClearDependencies;
Graph.DependsOn('terrain');
```

A transform source is both a copy source and a dependency:

```pascal
Graph.SwitchToPass('terrain-snapshot');
Graph.TransformFrom('terrain');
```

## named cross-pass constraints

`RequireFromPass` filters a candidate using the value at the same coordinate
in a named source pass:

```pascal
Graph.AddValue('house')
  .RequireFromPass('terrain', 'land')
  .RequireFromPass('hydrology', 'dry')
  .RequireFromPass('biome', 'plains')
  .RequireFromPass('roads', 'trail');
```

Calling it also declares the corresponding dependency edge. Labels are
resolved once and stored as stable indices.

For candidate value `v`, coordinate `c`, source pass `s`, and the ordered set
of accepted values `A(v,s)`, one source requirement is:

```text
output(s,c) is in A(v,s)
```

Repeated calls for the same source add alternatives, so values within one
source are OR. Requirements from distinct sources are conjunctive, so source
groups are AND:

```text
allowed(v,c) = local_rules(v,c)
               AND for every required source s:
                     output(s,c) is in A(v,s)
```

An empty source entry cannot satisfy a named requirement. These filters do not
replace same-layer directional rules; both must hold. `RequirePrevious`
retains its exact historical meaning and diagnostic while internally
protecting the predecessor edge it reads.

Version 1 deliberately uses same-coordinate equality against finite value
sets. Coordinate offsets, source neighborhoods, arbitrary predicates, and
feedback are future expression layers, not implicit behavior in this
contract.

## selective descendant closure

`TryRegenerateFrom` accepts one or more root passes. The dirty set is the least
set `D` satisfying:

```text
every requested root is in D
if A is in D and A -> B, then B is in D
```

In other words, it regenerates the named roots and their transitive
dependents. It does not regenerate an index suffix. If hydrology and biome are
siblings and roads depends on both, regenerating hydrology can execute
hydrology, roads, housing, and foliage while preserving a later-indexed biome
pass.

```pascal
Options := DefaultGraphSolveOptions;
if not Graph.TryRegenerateFrom(['hydrology'], Options, Report) then
  raise Exception.Create('selective regeneration failed');
```

Unselected layers are reused as immutable snapshots. Selected passes solve or
copy in stable topological order into staging buffers. Only after the complete
dirty closure succeeds are selected generated entries committed. Caller locks
remain caller-owned. On contradiction, backtrack exhaustion, malformed input,
or a commit-hook exception:

- every entry in every pass retains its pre-call value, empty state, and
  generated/lock ownership;
- the caller's selected pass is restored; and
- every pre-call random-stream state is restored.

On success, skipped-pass entry state and random streams remain byte-for-byte
unchanged. Executed streams rewind from their stable creation-index-derived
seed, so unrelated branch insertion and unrelated regeneration do not perturb
them.

## reports and inspection

`TGraphSolveReport.PipelineAlgorithmVersion` identifies the coordinator
contract. `ExecutionOrder` contains only passes actually attempted, in
topological order. Per-pass reports remain indexed by stable creation index and
add:

- `Executed`;
- `ExecutionOrdinal`, or `-1` when not executed; and
- `Disposition`: `gpdNotRun`, `gpdReused`, `gpdCleared`, `gpdCopied`,
  `gpdSolved`, or `gpdFailed`.

Zero decisions do not prove that a pass was skipped: a propagation-only solve
or definitionless copy can also have zero decisions. Consumers should use the
explicit execution fields.

Named constraint failures use `gckPassDependency` and report the stable source
index through `DependencyPassIndex`. Historical `RequirePrevious` failures
remain `gckPreviousPass`.

## deterministic replay identity

Exact dependency-pipeline replay requires:

- `WFC_PIPELINE_ALGORITHM_VERSION`;
- stable pass creation order and labels;
- each pass mode and transform source;
- dependency edges in canonical stable source-index order;
- every named source requirement and accepted-value order;
- graph shape, wrapping, locks, values, rules, weights, and callbacks;
- the pipeline seed and random/solver algorithm versions; and
- for selective runs, the ordered requested root labels.

Topological tie-breaking uses stable indices, while per-pass random streams are
also derived from stable indices. Renaming a pass therefore changes a
human-facing replay manifest but not an already-bound in-memory dependency or
random stream.

## current boundary

The dependency graph is intentionally acyclic. Cyclic design problems require
an explicit bounded negotiation, repair, or fixed-point protocol with its own
termination and replay contract; treating a cycle as an arbitrary execution
order would hide a materially different algorithm.

Cross-pass requirements currently read `TGraphValue` layers with the same
shape. An overlapping-pattern solve instead contains private latent pattern
keys and yields public tokens only after checked projection. Connecting that
layer requires a future projection-aware transaction that stages and validates
the projected grid before dependent passes read it. The dependency API does
not pretend those two representations are interchangeable.
