# causal solve traces

`TGraph.TrySolve` and `TGraph.TryRegenerateFrom` can capture a deterministic
causal record of an attempted pass transaction. The trace includes initial
domain filtering, observations, candidate removals, contradictions, abandoned
branches, restoration, pass staging, selective reuse, and the final atomic
commit or rollback.

Trace capture is implemented in project-owned Pascal and has no dependency
beyond repository units and the standard FPC or pas2js RTL.

## enabling capture

Capture is deliberately opt-in:

```pascal
uses
  SysUtils,
  wfc,
  wfc_trace;

var
  Graph: TGraph;
  Options: TGraphSolveOptions;
  Report: TGraphSolveReport;
  Validation: TGraphTraceValidationReport;
begin
  Graph := TGraph.Create.Reshape(8, 8, 1);
  try
    { configure values, rules, passes, and locks }
    Options := DefaultGraphSolveOptions;
    Options.CaptureTrace := True;
    if not Graph.TrySolve(Options, Report) then
      WriteLn('transaction failed in pass ', Report.FailedPassIndex);

    if not ValidateGraphTrace(Graph, Report, Validation) then
      raise Exception.Create(
        DescribeGraphTraceValidationIssue(Validation.Issue));
    WriteLn(GraphTraceSignatureHex(Report.TraceHash));
  finally
    Graph.Free;
  end;
end;
```

`DefaultGraphSolveOptions` sets `CaptureTrace` to `False`. A disabled report
has `TraceCaptured = False`, `TraceHash = 0`, an empty `Trace`, and a
`TraceStart` of `-1` with `TraceCount = 0` for every pass. It retains the same
assignments, counters, status, and random-stream position as the pre-trace
solver path.

Always initialize a local `TGraphSolveOptions` with
`DefaultGraphSolveOptions` before overriding fields. Local Pascal records are
not guaranteed to be zeroed; assigning only one field can leave another field
undefined.

When enabled, `TraceCaptured` is true and `Trace` is a chronological array of
`TGraphTraceEvent`. Every event receives an `EventId` equal to its zero-based
array index. `CauseEventId` is `-1` when no preceding event in this trace
caused the event, or it is the ID of a strictly earlier event. Caller-domain
and caller-lock inputs are external causal roots and therefore use `-1`.
Following cause IDs always terminates and cannot form a cycle.

## transaction event kinds

| Event | Meaning |
| --- | --- |
| `gtekPassBegin` | A dirty pass began preparation and solving. |
| `gtekInitialCandidateRemoved` | A lock, caller domain, or prior-pass requirement excluded a candidate before propagation. |
| `gtekDecision` | The solver selected one branch alternative. The event itself does not change the domain. |
| `gtekCandidateRemoved` | Decision pruning, adjacency propagation, or required-support propagation removed one candidate. |
| `gtekContradiction` | A domain became impossible or final validation failed. |
| `gtekBacktrack` | The solver abandoned a failed decision frame. |
| `gtekCandidateRestored` | Backtracking restored one candidate from the removal trail. |
| `gtekPassStaged` | One pass produced a validated staged result; it is not yet committed. |
| `gtekPassFailed` | A pass ended without a staged solution. |
| `gtekPassSkipped` | Selective regeneration reused this pass as immutable input. |
| `gtekPipelineCommit` | Every staged pass committed successfully. This is the final event of a solved report. |
| `gtekPipelineRollback` | The transaction failed and committed no staged solver output. This is the final event of an unsolved report. |

The trace is a record of the complete attempted transaction, not only the
winning branch. A successful solve can contain contradictions, backtracks,
and restorations before its final `gtekPipelineCommit`. This is intentional:
discarding abandoned branches would make it impossible to replay or explain
why the retained alternative was reached.

## cause kinds

| Cause | Meaning |
| --- | --- |
| `gtckNone` | No more specific solver cause applies, such as the root decision event. |
| `gtckCallerDomain` | A caller-owned `SetAllowedValues` restriction, including a definitionless destination's own domain, excluded the state. |
| `gtckCallerLock` | A non-generated entry value excluded other candidates or was invalid. |
| `gtckDecision` | A selected branch removed its alternatives. |
| `gtckAdjacency` | Directional compatibility with a neighbor removed the candidate. |
| `gtckPassDependency` | A staged provider failed a cross-pass requirement or supplied a copied value rejected by a definitionless destination. |
| `gtckRequiredSupport` | No eligible physical neighbor could trigger a required-only candidate. |
| `gtckBacktrack` | Restoration was caused by abandoning a failed branch. |
| `gtckFinalValidation` | Independent assignment validation found an invalid final relation. |
| `gtckTransaction` | Pass or pipeline lifecycle bookkeeping caused the event. |

Caller-domain and caller-lock events are external roots and are never
attributed to `gtekPassBegin`. Source-derived definitionless failures link to
the source pass instead.

A non-root decision links to the prior change that made its cell observable
and inherits that event's public cause kind. A retry decision links to the
backtrack that enabled it.

For a cross-pass initial removal, `PassIndex` identifies the consumer,
`DependencyPassIndex` identifies the provider, and `CauseEventId` points into
the provider pass. It normally reaches that provider's `gtekPassStaged` event,
or its `gtekPassSkipped` event during selective regeneration. This preserves
the difference between “the model never allowed this value” and “this terrain
or structure layer made the value unavailable.”

If several provider clauses reject one candidate, version 1 records the
lowest stable provider-pass index. Finite any-of clauses are evaluated exactly,
but the trace does not yet encode every failed term or a minimal failed clause.
That richer evidence belongs in a later schema version.

## event fields

Pass-scoped events use a valid zero-based `PassIndex`. Pipeline commit and
rollback use `PassIndex = -1`. Unavailable entry, value, neighbor, and
dependency indices are `-1`.

`EntryIndex` and `NeighborIndex` use the graph's X-fastest storage order. The
utility function `TryGraphEntryIndexToPosition` converts an entry index to
`TGraphPosition` without relying on private graph state.

Candidate events carry both `ValueIndex` and `Value`. `ValueIndex` is the
stable `AddValue` order used by replay and hashing. `Value` is a human-facing
inspection copy and is checked against the selected pass's current registered
values by `ValidateGraphTrace`.

Inspect `HasDirection` before using `Direction`. When no direction applies,
`HasDirection` is false and `Direction` retains the canonical `gdNorth`
sentinel.

`DecisionDepth` is zero-based. Events outside a decision frame use zero.
`DomainCountBefore` and `DomainCountAfter` describe the affected cell around
candidate removal or restoration. Metadata events use `0 -> 0`. A decision
event also has equal counts because its child removal events carry the actual
decrements.

Each `TGraphPassSolveReport` exposes a half-open slice of the complete event
array through `TraceStart` and `TraceCount`. Pass events are emitted
contiguously. The final pipeline event is intentionally outside every pass
slice.

## stable trace signatures

`WFC_TRACE_VERSION = 1` identifies the event schema.
`WFC_TRACE_HASH_VERSION = 1` identifies its portable signature encoding.
`CalculateGraphTraceHash` recomputes a report signature, and
`GraphTraceSignatureHex` renders exactly eight uppercase hexadecimal digits.

The signature uses a 32-bit FNV-1a stream with explicit overflow semantics.
It begins with the ASCII identity `wfc-graph-trace`, then mixes:

1. trace-schema and trace-hash versions;
2. seed;
3. random, solver, graph-model, and pipeline algorithm versions;
4. pass count; and
5. every event's numeric fields in chronological order.

Cardinals are encoded little-endian. Signed integers use one sign byte followed
by an unsigned 32-bit magnitude, including a defined representation for
`Low(Integer)`. Booleans use one byte. Event `Value` strings are deliberately
excluded: `ValueIndex` is the model-stable identity, and native FPC byte strings
must not accidentally diverge from pas2js UTF-16 strings.

The hash identifies this numeric trace under its versioned solver metadata. It
is not a cryptographic integrity primitive and is not a complete serialized
model identity. Store the model or its own signature beside a trace artifact
when traces from different models will be compared.

## query and validation utilities

The `wfc_trace` unit is the dependency-free public inspection layer:

- `GraphTraceEventKindName`, `GraphTraceCauseKindName`, and
  `GraphTraceDirectionName` return stable lowercase names;
- `TryFindGraphTraceEvent` and `FindGraphTraceEvent` follow event IDs;
- `CopyGraphTraceEventsForPass` and `CopyGraphTraceEventsForEntry` return
  detached chronological subsets;
- `TryGraphEntryIndexToPosition` recovers public coordinates;
- `FormatGraphTraceEvent` emits one fixed-order, line-safe record; and
- `ValidateGraphTrace` checks IDs, direct causal roles, exact domain deltas,
  pass slices, graph bounds, value identities, directions, dependencies,
  field invariants, terminal status, and the recomputed signature.

The formatter percent-escapes whitespace, control characters, and nonliteral
value code points so an inspected value cannot inject another event line.

Validation proves structural consistency plus the direct causal relationships
guaranteed by the version-1 contract. It does not rerun the solver, reconstruct
every intermediate domain, or prove that the graph rules themselves are
semantically correct.

## inspector example

The shared example under `examples/passes/02_TraceInspector` solves
terrain -> settlement -> foliage, independently checks its output and trace,
prints every event, and walks one rejected foliage candidate backward to the
provider-pass event. Native FPC and pas2js/Node run the same Pascal unit and
produce the same seed-zero event stream.

```text
fpc -B -Mdelphi -Sa -Cr -Co -Ci -Fusrc -Fuexamples/passes/02_TraceInspector -FUbuild/trace-inspector/native/units -FEbuild/trace-inspector/native/bin examples/passes/02_TraceInspector/TraceInspector.lpr
build/trace-inspector/native/bin/TraceInspector
```

```text
pas2js -B -Tnodejs -Mdelphi -Fusrc -Fuexamples/passes/02_TraceInspector -FUbuild/trace-inspector/pas2js/units -FEbuild/trace-inspector/pas2js/bin examples/passes/02_TraceInspector/TraceInspectorNode.lpr
node build/trace-inspector/pas2js/bin/TraceInspectorNode.js
```

The current golden contract is 27 events with trace hash `73C4B9A2`.

## cost and current limits

Disabled capture does not allocate trace arrays or per-cell causal storage.
Enabled kernel and public traces grow geometrically and are trimmed to their
exact event count before publication. Memory use is therefore proportional to
the full attempted search, including candidates restored from abandoned
branches.

Version 1 intentionally has no event cap, streaming sink, compressed artifact,
timing data, minimal-unsatisfiable-core extraction, or counterfactual repair.
Callers should leave capture disabled for very large production searches unless
they intend to retain the complete evidence. Future limits or streaming must
remain deterministic and explicitly versioned; silently dropping events would
make a trace look complete when it is not.
