# pass dependency DAGs and selective regeneration

The original pass pipeline answers an important design question: how can a
generator add information in stages without forcing terrain, ecology, roads,
housing, and decoration into one combinatorial value space? A dependency DAG
extends that idea beyond a single chain. Independent branches can read a
shared foundation, later passes can join those branches, and a local change can
regenerate only the layers that actually depend on it.

This document defines the version-2 dependency, spatial-read, and
selective-regeneration model. The implementation remains part of `TGraph`;
there is no external scheduler or runtime dependency.

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
WFC_PIPELINE_ALGORITHM_VERSION = 2
```

## dependency roles

An edge may serve more than one role:

- an explicit ordering/data dependency added by `DependsOn`;
- the protected predecessor edge required by legacy mode;
- the single protected source of a transform pass;
- the protected predecessor used by `RequirePrevious`; or
- a protected source used by `RequireFromPass`, `RequireFromPassAt`, or
  `RequireAnyFromPass`.

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

`RequireFromPass` remains the compatibility spelling for a merged zero-offset
clause. Pipeline v2 generalizes that contract with signed finite offsets:

```pascal
Graph.AddValue('house')
  .RequireFromPassAt('terrain', MakeGraphOffset(0, 0, 0),
    ['land', 'forest'])
  .RequireAnyFromPass('terrain', [
    MakeGraphPassMatchTerm(MakeGraphOffset(-1, 0, 0),
      ['water', 'marsh']),
    MakeGraphPassMatchTerm(MakeGraphOffset(1, 0, 0),
      ['water', 'marsh'])
  ]);
```

`TGraphOffset` contains signed `DeltaX`, `DeltaY`, and `DeltaZ` components.
The provider coordinate is the consumer coordinate plus that displacement.
`RequireFromPassAt` accepts one finite value set at one exact offset.
`RequireAnyFromPass` accepts a finite ordered array of `(offset, values)`
terms. It is a neighborhood primitive because callers can explicitly list any
shape—cardinal, diagonal, vertical, temporal, or domain-specific—without
embedding those concepts in the core.

### clause algebra

For candidate `v`, consumer coordinate `c`, provider pass `s`, offset `d`, and
finite value set `A`, define:

```text
match(s, c, d, A) = provider(s, resolve(c + d)) is in A
```

An unresolved bounded coordinate and an empty provider entry both make
`match` false. The public calls compose as follows:

- values within one exact-offset clause are OR;
- repeated `RequireFromPassAt` calls with the same provider and exact offset
  merge their values as OR;
- terms within one `RequireAnyFromPass` call are OR, and every term's values
  are OR;
- different offsets declared with `RequireFromPassAt`, separate any-clauses,
  and clauses from any other provider are AND.

Thus the example above means `(land OR forest) at self` AND `((water OR
marsh) west OR (water OR marsh) east)`. Repeating `RequireFromPass` retains
its historical merged zero-offset OR behavior. When `RequirePrevious` and a
named zero-offset clause refer to the same predecessor, their accepted values
also retain the compatibility merge; nonzero and any-clauses remain separate
AND requirements.

Empty value arrays, empty any-term arrays, and terms with empty values are
rejected at construction time. Duplicate-offset terms merge, term order is
canonical signed X/Y/Z order, and accepted values deduplicate in first-seen
order. Offset arithmetic is checked before sampling, so an `Integer`
displacement cannot silently wrap an unsigned coordinate.

### boundaries and staging

When `WrapNeighbors` is false, every dimension is bounded. Any sampled
coordinate below zero or at/above that dimension's size is an unresolved
non-match. Therefore an exact-offset clause fails at that cell, while an
any-clause can still succeed through another in-bounds term. If every term is
out of bounds, the any-clause fails.

When `WrapNeighbors` is true, each displaced component wraps independently by
the shared graph dimension. Large positive and negative offsets resolve to the
same coordinate they denote; the declared offsets themselves remain replay
inputs.

Every spatial requirement declares a protected dependency edge immediately.
The provider must already exist, have the same shape, and precede the consumer
in the acyclic dependency plan. Both `Run` and `TrySolve` execute providers
before consumers. `TrySolve` reads the provider's staged output from the
current atomic transaction; legacy `Run` reads output produced earlier in its
nontransactional execution. These are hard candidate filters, not soft scores,
late callbacks, or reads from a partially solved peer.

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

Set `TGraphSolveOptions.CaptureTrace` to retain the complete attempted
transaction rather than only its aggregate report. Every pass owns a
contiguous `TraceStart`/`TraceCount` slice. Selective regeneration emits
`gtekPassSkipped` for reused immutable inputs; executed passes emit begin and
stage/fail lifecycle events; and the final commit or rollback remains outside
all pass slices.

A candidate removed by a named or spatial requirement uses
`gtckPassDependency`, identifies the stable provider through
`DependencyPassIndex`, and links its `CauseEventId` to that provider's stage or
skip event. This makes the already-planned provider -> consumer relationship
visible in the causal record without introducing a separate scheduler. The
versioned trace hash and `wfc_trace` validation/query helpers are portable
across native FPC and pas2js. See [causal solve traces](traces.md) and the
shared `examples/passes/02_TraceInspector` console example.

## deterministic replay identity

Exact dependency-pipeline replay requires:

- `WFC_PIPELINE_ALGORITHM_VERSION`;
- stable pass creation order and labels;
- each pass mode and transform source;
- dependency edges in canonical stable source-index order;
- every named source requirement, signed offset, any-clause term, and
  accepted-value order;
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

Causal Trace v1 records the lowest stable failed provider when several
cross-pass clauses reject one candidate. It does not retain every failed term,
derive a minimal contradiction set, expose live domain snapshots for
interactive stepping, cap the event count, or stream events. Those additions
need explicit deterministic and versioned contracts rather than silently
changing the meaning of a complete trace.

Cross-pass requirements read `TGraphValue` layers with the same shape at
exactly declared finite offsets. They do not perform radius expansion,
distance calculation, counting, arbitrary predicates, resampling, or soft
scoring. An overlapping-pattern solve instead contains private latent pattern
keys and yields public tokens only after checked projection. Connecting that
layer requires a future projection-aware transaction that stages and validates
the projected grid before dependent passes read it. The dependency API does
not pretend those two representations are interchangeable.
