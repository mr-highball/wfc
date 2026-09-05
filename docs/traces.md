# causal solve traces

Opt-in [rooted connectivity](connectivity.md) adds `gtckConnectivity` and a
pass-local `ConstraintIndex`. Ordinary events use `-1`; connectivity events
use the descriptor's zero-based registration ordinal. Connectivity-derived
decisions retain that ordinal through their cause link. Only connectivity
causes extend the trace hash encoding, so unconstrained trace goldens remain
unchanged. The formatter adds `connectivity=N` and the independent validator
checks descriptor bounds and causal identity against the producing model.

Trace v1 event chronology and hashes remain unchanged. The pass report also
retains its original `TraceStart`/`TraceCount` fields for source and transcript
compatibility. Most reports contain one contiguous run of events per pass, so
those fields describe the familiar half-open slice. A custom
`DoValidateCommit` can instead reject an earlier active pass after a later pass
has staged. Its contradiction and failure events form a second chronological
run for the earlier pass; the legacy fields are deliberately not rewritten to
pretend that the intervening events belong to it.

Trace Layout v1 is the additive representation for that case.
For a captured report, `TryBuildGraphTraceLayout` validates the graph, report,
events, chronological lifecycle, and legacy metadata, then derives detached
canonical maximal ranges for every pass. `ValidateGraphTraceLayout` revalidates
a supplied layout against the report, and `CopyGraphTraceLayout` makes a deep
copy. The original
`ValidateGraphTrace` contract is unchanged: it continues to require one
contiguous legacy slice and therefore continues to reject a genuine late
failure attributed to an earlier pass. Use the layout API when consuming all
reports returned by a graph with a derived commit validator. Do not reinterpret
either validation failure as a successful solve or reattribute the failure to
another pass.

`TGraph.TrySolve` and `TGraph.TryRegenerateFrom` can capture a deterministic
causal record of an attempted pass transaction. The trace includes initial
domain filtering, observations, candidate removals, contradictions, abandoned
branches, restoration, pass staging, selective reuse, and the final atomic
commit or rollback.

`TrySolveNegotiated` retains one such ordinary Trace-v1 report for every
complete-pipeline round. It does not splice provider revisits from separate
rounds into one event array.

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
  Layout: TGraphTraceLayout;
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

    if not TryBuildGraphTraceLayout(Graph, Report, Layout,
        Validation) then
      raise Exception.Create(
        DescribeGraphTraceValidationIssue(Validation.Issue));
    WriteLn(GraphTraceSignatureHex(Report.TraceHash));
  finally
    Graph.Free;
  end;
end;
```

`DefaultGraphSolveOptions` sets `CaptureTrace` to `False`. A capture-disabled report
has `TraceCaptured = False`, `TraceHash = 0`, an empty `Trace`, and a
`TraceStart` of `-1` with `TraceCount = 0` for every pass. Its derived layout
has no event ranges and uses `-1` for the terminal event index. With no event
chronology to inspect, validation checks those capture-disabled sentinels; it
does not independently attest the aggregate solve lifecycle. A report returned
by the solver retains the same assignments, counters, status, and random-stream
position as the pre-trace solver path.

Live observation is independent of retained capture. Attach a borrowed
`TGraph.TraceSink` to receive the same events synchronously, with delivery
counts and the whole-stream hash in `Report.TraceDelivery`. A sink does not
change the capture-disabled fields above. The project-owned
`TGraphTraceWindowSink` can retain a caller-sized recent suffix without retaining
the full search. See [streaming traces](trace-streaming.md) for ownership,
observer failures, per-attempt boundaries, and explicit window completeness.

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
| `gtekContradiction` | A domain became impossible, final validation failed, or a complete assignment matched an exact negotiation exclusion. |
| `gtekBacktrack` | The solver abandoned a failed decision frame. |
| `gtekCandidateRestored` | Backtracking restored one candidate from the removal trail. |
| `gtekPassStaged` | One pass produced a validated staged result; it is not yet committed. |
| `gtekPassFailed` | A pass failed. Late commit validation may revoke its provisional staging; staged values are never public before pipeline commit. |
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
| `gtckFinalValidation` | Independent assignment validation or a derived whole-candidate commit validator found an invalid final relation. |
| `gtckTransaction` | Pass or pipeline lifecycle bookkeeping caused the event. |
| `gtckExactAssignmentExclusion` | A complete pass assignment exactly matched a versioned outer-negotiation exclusion. |

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

An exact assignment exclusion is checked after every cell in one pass is
assigned. Its contradiction is therefore pass-scoped but entryless:
`EntryIndex`, `ValueIndex`, `NeighborIndex`, and `DependencyPassIndex` are
`-1`; the cause is `gtckExactAssignmentExclusion`. The pass report increments
`ExcludedAssignments`. Escaping that complete vector uses the ordinary local
backtrack/restoration trail.

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

That generic `Value` is graph-level data, not automatically a domain-safe
public token. Sequence adapters deliberately register model-qualified private
keys such as `@wfcs...`, so a raw graph trace over those passes may contain the
same keys. `TWfcTextPassPipeline` handles this boundary explicitly: it validates
the raw trace first, preserves the numeric core hash in
`TWfcTextPassReport.TraceHash`, projects every candidate to a public token plus
numeric `StateIndex`, and publishes those events through
`TWfcTextPassReport.Trace`. It clears `Solve.Trace`, `Solve.TraceHash`, and the
generic legacy pass metadata so the stripped `TGraphSolveReport` remains a valid
capture-disabled report. Advanced callers using the exposed graph directly can
still request the raw graph trace and are responsible for that lower-level
representation.

Inspect `HasDirection` before using `Direction`. When no direction applies,
`HasDirection` is false and `Direction` retains the canonical `gdNorth`
sentinel.

`DecisionDepth` is zero-based. Events outside a decision frame use zero.
`DomainCountBefore` and `DomainCountAfter` describe the affected cell around
candidate removal or restoration. Metadata events use `0 -> 0`. A decision
event also has equal counts because its child removal events carry the actual
decrements.

Each `TGraphPassSolveReport` retains the original `TraceStart` and
`TraceCount` metadata. Pass solve events are normally contiguous. A late
commit rejection of the last executed pass remains contiguous too. When a
late rejection names an earlier active pass, its final contradiction/failure
suffix is separated from that pass's first run by later-pass events. The
canonical `TGraphTraceLayout.Passes[I].Ranges` represents both runs without
reordering or copying events. Its ranges are half-open, ordered, nonempty,
maximal, and nonoverlapping. The final pipeline event is identified separately
by `TerminalEventIndex` and never belongs to a pass range.

The captured layout validator checks the reused-pass prefix, the actual
`ExecutionOrder`, one active pass at a time, and the staged/failed lifecycle.
The only permitted revisit is a final-validation contradiction followed by
that already-staged pass's failure and pipeline rollback. The contradiction
must link back to that pass's staging event and agree with the failure report.
Arbitrary rehashed interleaving is not another valid layout.

Ordinary failure summaries must agree with the final contradiction's location,
neighbor, direction, and connectivity ordinal. Unambiguous cause kinds must
match too. Initial-domain events record the last candidate removal, which can
differ from the filter used for the aggregate failure classification. For
those cases the validator checks the permitted classification family and any
declared provider, not a uniquely proven failed filter. Counters are checked
for representable nonnegative values, not independently reconstructed.

## negotiated rounds

`TGraphNegotiationReport` keeps rounds separate rather than constructing one
aggregate trace. Each rejected `Attempts[I].SolveReport` is an ordinary failed
transaction ending in `gtekPipelineRollback`. `FinalReport` is the sole
terminal round and ends in commit on `gnsSolved`, or rollback on every other
negotiation status. Every contained report can be passed independently to
`TryBuildGraphTraceLayout`; reports with one range per pass also retain their
original `ValidateGraphTrace` compatibility.

The negotiation transcript records chronological round order, the pass chosen
for each outer backtrack, its execution ordinal, and the complete excluded
value-index assignment. Its `TranscriptHash` is separately governed by
`WFC_PASS_NEGOTIATION_HASH_VERSION` and recomputed by
`CalculateGraphNegotiationTranscriptHash`; it is not a Trace-v1 hash. This
two-level shape avoids noncontiguous per-pass slices such as provider -> failed
consumer -> revisited provider.

When capture is disabled, every contained ordinary report keeps the normal
capture-disabled invariants. The transcript still covers the ordinary reports,
options, exact exclusions, and terminal outcome; their trace arrays and hashes
are simply empty and zero.

## stable trace signatures

`WFC_TRACE_VERSION = 1` identifies the event schema.
`WFC_TRACE_HASH_VERSION = 1` identifies its portable signature encoding.
`WFC_TRACE_LAYOUT_VERSION = 1` identifies the detached range layout.
`CalculateGraphTraceHash` recomputes a report signature, and
`GraphTraceSignatureHex` renders exactly eight uppercase hexadecimal digits.

The layout is derived metadata, not another event stream or artifact
fingerprint. It does not contribute to the Trace-v1 hash, alter negotiation or
restart transcripts, or change legacy report fields. Its copied trace hash and
event count are checked against the report;
`ValidateGraphTraceLayout` additionally requires exact equality with the
canonical current ranges. Mismatched or noncanonical ranges are rejected; this
is structural validation, not cryptographic report identity, and an identical
valid layout may be reused.

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

Negotiation transcript signatures have the same diagnostic—not
cryptographic—role. The exact assignment arrays in `Attempts` remain
authoritative and are included directly in the transcript hash input.

## query and validation utilities

The `wfc_trace` unit is the dependency-free public inspection layer:

- `GraphTraceEventKindName`, `GraphTraceCauseKindName`, and
  `GraphTraceDirectionName` return stable lowercase names;
- `TryFindGraphTraceEvent` and `FindGraphTraceEvent` follow event IDs;
- `CopyGraphTraceEventsForPass` and `CopyGraphTraceEventsForEntry` return
  detached chronological subsets;
- `TryBuildGraphTraceLayout` derives canonical maximal per-pass ranges,
  `ValidateGraphTraceLayout` checks an existing layout, and
  `CopyGraphTraceLayout` deep-copies one;
- `TryGraphEntryIndexToPosition` recovers public coordinates;
- `FormatGraphTraceEvent` emits one fixed-order, line-safe record; and
- `ValidateGraphTrace` checks IDs, direct causal roles, exact domain deltas,
  legacy pass slices, graph bounds, value identities, directions, dependencies,
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
prints the derived range or ranges for every pass plus every event, and walks
one rejected foliage candidate backward to the provider-pass event. The
native FPC host runs the shared Pascal unit and produces the same seed-zero
event stream. A second two-pass fixture rejects pass zero after pass one stages,
proves rollback, and prints pass-zero ranges `0..1,4..5`.

```text
fpc -B -Mdelphi -Sa -Cr -Co -Ci -Fusrc -Fuexamples/passes/02_TraceInspector -FUbuild/trace-inspector/native/units -FEbuild/trace-inspector/native/bin examples/passes/02_TraceInspector/TraceInspector.lpr
build/trace-inspector/native/bin/TraceInspector
```

The solved golden remains 27 events with trace hash `73C4B9A2`. The separate
late-rejection fixture retains trace hash `B27D0AE0`.
The inspector also delivers the original 27-event solve live with full capture
disabled, retains only IDs `22..26`, and reports 22 dropped events plus the
unchanged whole-stream hash. Cause IDs outside that window stay intact and are
labelled as unavailable; the suffix is never presented as a complete trace.
The [research record](research/chronological-trace-layout-v1.md) preserves the
counterexample, unchanged hashes, finite tests, and representation tradeoff.

## cost and current limits

With both capture disabled and no sink, no trace arrays or per-cell causal
storage are allocated; the graph uses only a fixed-size recorder object.
Full public capture grows geometrically and is trimmed to its exact event
count before publication. It no longer duplicates the entire kernel trace.
Retained memory is still proportional to the full attempted search, including
candidates restored from abandoned branches. Direct numeric-kernel callers can
also opt into full kernel capture independently.

Sink-only observation retains no kernel/public event arrays or event-sized
local/global mapping. Trace-related preparation and causal summaries require
O(cells × values + cells + passes) storage, independent of event count. This
does not remove the solver's ordinary model/domain/trail costs. An optional
recent-event window adds storage proportional to its chosen capacity.

Building a layout performs linear scans over the published events and owns
only detached pass/range arrays. Its worst-case range count is proportional to
the event count; ordinary reports use one range per participating pass. The
layout is a snapshot, so callers must rebuild it after replacing or changing
the report. `ValidateGraphTraceLayout` rejects mismatched or noncanonical
ranges but does not keep the source report alive or establish cryptographic
identity.

Negotiated capture multiplies that cost by the number of rounds because every
rejected report is retained. Whole-assignment chronological enumeration can be
exponential, so production callers should set both solver and pass budgets
before enabling complete attempt traces.

Trace v1 has no arbitrary event cap, but its IDs/counts retain their checked
`Integer` capacity. [Delivery v1 and Window v1](trace-streaming.md) add live
observation and explicit suffix retention without changing those event or hash
contracts. Compressed persisted artifacts, interactive stepping, per-event
timing, minimal-unsatisfiable-core extraction, and counterfactual repair remain
future work. For very large searches, leave full capture disabled and choose a
sink whose own memory and output costs fit the application.
