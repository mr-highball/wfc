# Chronological trace layout v1

This record concerns a concrete pass-transaction failure, not a new graph
algorithm. The implementation and fixtures are project-owned Pascal under
the repository's MIT license.

## Counterexample

Two single-cell passes stage values `A` and `B`. A final commit validator
then rejects pass zero. The transaction correctly restores the previous
entries and random streams, but its evidence must retain this chronology:

```text
0  pass 0 begins
1  pass 0 stages
2  pass 1 begins
3  pass 1 stages
4  pass 0 contradicts final validation, caused by event 1
5  pass 0 fails, caused by event 4
6  pipeline rolls back, caused by event 5
```

Pass zero owns `[0,2)` and `[4,6)`, not `[0,4)`. A first-event index plus a
total event count cannot describe that membership as one contiguous slice.
The original producer emits the correct chronology and hash, but its legacy
single-slice validator rejects this report. Moving the final rejection
earlier would falsify chronology; moving it to pass one would falsify
attribution. Rewriting the legacy slice fields would also change existing
negotiation and restart transcript encodings.

## Additive representation

`WFC_TRACE_LAYOUT_VERSION = 1` identifies detached, maximal, half-open ranges
into the unchanged Trace-v1 event array. Pass membership and chronology are
separate concerns. The final pipeline event is indexed separately and belongs
to no pass range.

The new validator checks event fields and causal roles, actual execution
order, reuse, staging, failure, and canonical range coverage. A staged pass
may be revisited only by the final-validation contradiction/failure/rollback
suffix, with the original staging event as its cause. Arbitrary rehashed
interleaving is rejected. The old validator intentionally keeps its original
single-slice contract; production consumers that need the expanded case use
`TryBuildGraphTraceLayout`.

Layout construction does not rewrite the source report, draw random values,
invoke a solver, or retain another event copy. A layout copy owns detached
pass and range arrays. Invalid construction returns a default, unpublished
layout. A supplied layout is checked against the report and its canonical
ranges, not trusted because it carries a matching diagnostic hash.

## Reproduction and evidence

The two-pass fixture uses seed 55 and has these unchanged Trace-v1 hashes:

| Commit result | Hash | Legacy single-slice validator | Range validator |
| --- | --- | --- | --- |
| Both passes commit | `709F3450` | accepts | accepts |
| Last pass rejected | `460B78DC` | accepts | accepts |
| Earlier pass rejected | `B27D0AE0` | rejects | accepts |

Run the normal native or browser conformance gate. The independent
[`wfc_trace_layout_test`](../../test/wfc_trace_layout_test.lpr) builds real
reports for first/middle/last and entryless failures, selective reuse,
nonnumeric topological order, definitionless passes, local backtracking,
negotiation, and restarts. It also mutates and rehashes reports, supplies
overlapping/missing/reordered ranges, checks detached ownership, and injects
malformed host numbers and sparse oversized arrays in the browser target.

The checked FPC 3.2.2 and development-compiler runs each pass 1,608 checks.
The pas2js browser run passes 2,024 checks, including its host-value cases.
Every nested negotiation/restart report is inspected separately, and deriving
its layout leaves its original transcript signature unchanged. These are
finite regression results, not a proof for every possible user callback.

The shared [Trace Inspector](../../examples/passes/02_TraceInspector/README.md)
prints the original successful terrain/settlement/foliage trace and a separate
real late-rejection example. Its original success hash `73C4B9A2` is unchanged.
The same helper executes in both native and browser conformance.

## Boundaries

This is structural trace validation, not semantic model validation or
cryptographic provenance. In particular, an initial-domain event names the
last removal, which need not uniquely identify the aggregate failed filter.
The validator must not invent a more specific explanation than the event
schema records. With capture disabled there is no chronology to attest.

The layout is an in-memory index, not a streaming trace sink or persisted
artifact format. Full trace storage and the graph's checked index capacity
remain real resource constraints. Streaming capture, richer clause evidence,
and domain-state reconstruction are separate work.

See the [API contract](../traces.md) for exact fields and usage.
