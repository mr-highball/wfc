# bounded pass negotiation

`TGraph.TrySolveNegotiated` is an opt-in coordinator for cases where an
ordinary staged pass pipeline chooses a valid provider assignment that makes a
later pass impossible. It can reject that complete provider assignment, reopen
an earlier pass, and try the pipeline again. The search is deterministic,
bounded, transactional, and implemented in the project-owned Pascal core.

This is a separate algorithm from `TrySolve`:

- `TrySolve` remains the one-way Pipeline v2 transaction and retains its replay
  behavior;
- `TrySolveNegotiated` performs complete-pipeline rounds around that ordinary
  solver;
- the dependency graph remains acyclic; and
- there is no selectively regenerated negotiation overload in version 1.

The public negotiation contract is identified by:

```pascal
WFC_PASS_NEGOTIATION_ALGORITHM_VERSION = 1
WFC_PASS_NEGOTIATION_HASH_VERSION = 1
```

## basic use

```pascal
var
  Options: TGraphNegotiationOptions;
  Report: TGraphNegotiationReport;
begin
  Options := DefaultGraphNegotiationOptions;
  Options.SolveOptions.MaxBacktracks := 1024;
  Options.SolveOptions.CaptureTrace := True;
  Options.MaxPassBacktracks := 32;

  if not Graph.TrySolveNegotiated(Options, Report) then
    WriteLn('negotiation ended with status ', Ord(Report.Status));
end;
```

Always begin with `DefaultGraphNegotiationOptions`. It initializes the nested
ordinary solve options and sets a finite pass budget. The current defaults are
`SolveOptions.MaxBacktracks = 256`, trace capture disabled, and
`MaxPassBacktracks = 64`. Negative local or pass limits raise `ERangeError`
before graph state or random streams change.

## terms

A **round** is one complete ordinary pipeline attempt. Every round uses the
same stable topological pass order and produces one ordinary
`TGraphSolveReport`.

A **choice frame** is a completed defined pass whose exact value-index
assignment can still be changed. Definitionless passes are never choice
frames. A defined pass made entirely from caller-owned locks is also skipped:
excluding its assignment could not change it and would only consume budget.
Prior solver-generated values do not become locks and remain negotiable on a
new call.

An **assignment exclusion** is one exact, complete array of stable registered
value indices for one pass. For a two-cell provider, excluding `[0, 1]`
rejects that ordered pair only; it does not ban value `0` or value `1`
individually and does not become a caller domain.

A **pass backtrack** records one rejected round and adds one such exclusion to
the selected choice frame.

## version-1 algorithm

Negotiation v1 is chronological over whole pass assignments:

1. Build the complete stable topological execution plan. Every pass is dirty;
   version 1 has no negotiated form of `TryRegenerateFrom`.
2. Run one ordinary atomic pipeline round from the graph seed, applying any
   exact assignment exclusions accumulated for each pass.
3. If the round succeeds, commit that final staged pipeline once and return
   `gnsSolved`.
4. If the local solver reports `gssBacktrackLimit`, stop with
   `gnsSolverBacktrackLimit`.
5. Otherwise scan the round's actual `ExecutionOrder` backward and select the
   latest completed choice frame. The terminal failed pass, a not-yet-run
   pass, a definitionless pass, and a caller-locked-only pass are not frames.
6. If no frame remains, stop with `gnsContradiction`. If selecting another
   frame would exceed `MaxPassBacktracks`, stop with
   `gnsPassBacktrackLimit`.
7. Append the failed round to `Attempts`, record the selected pass and its
   complete assignment, exclude that assignment for the current earlier-pass
   prefix, clear exclusions belonging to every later pass in the full
   topological order, and start the next round.

The coordinator is deliberately **global chronological**, not
conflict-directed. It does not jump directly to
`Contradiction.DependencyPassIndex`. A completed unrelated pass later in the
execution order is a choice frame and may consume pass budget before the
search reaches the provider that caused the visible downstream failure. This
simple rule gives the algorithm a portable total order; it is also an explicit
cost and not presented as a minimal repair.

## independent branches and joins

Consider two independent providers `A` and `B`, followed by a consumer `C`
that depends on both:

```text
A --\
     > C
B --/
```

Stable topological order is `A, B, C`. If `C` fails, `B` is the latest
completed choice frame. Negotiation enumerates complete `B` assignments while
the current `A` assignment remains fixed. If exclusions exhaust `B`, that
round fails while solving `B`; `B` is no longer a completed frame, so the
coordinator backtracks `A`. Changing the earlier prefix clears every later
`B` exclusion, and `B` is enumerated again in the new `A` context.

This is chronological nested-loop enumeration over pass assignments. It is
not a simultaneous flattened CSP, and no cyclic dependency edge is introduced.

## two distinct budgets

`TGraphNegotiationOptions` contains two limits with different units:

- `SolveOptions.MaxBacktracks` is the ordinary reference-solver limit applied
  independently to each pass in each round;
- `MaxPassBacktracks` bounds how many completed pass assignments the outer
  coordinator may reject and reopen.

Both must be large enough. An exact assignment exclusion is checked when a
local pass reaches a complete assignment. Encountering a banned assignment is
a local contradiction. Replaying the accumulated exclusions and advancing to
the next local alternative therefore consumes ordinary local backtracks as
well as the already-recorded outer pass backtracks. A positive pass budget
does not override a zero or exhausted `SolveOptions.MaxBacktracks`.

`MaxPassBacktracks = 0` performs exactly one ordinary one-way round. If that
round fails after a negotiable provider completed, the negotiation status is
`gnsPassBacktrackLimit`, `PassBacktracks` is zero, `Attempts` is empty, and
`FinalReport` is the ordinary failed report.

The outer algorithm can be exponential in the number of passes, cells, and
available assignments. Whole-assignment exclusions are intentionally exact,
so they do not generalize one failure into a broad learned clause. Use finite
budgets and inspect both kinds of counters.

## atomicity and ownership

Each rejected round is a normal atomic pipeline failure. It commits no staged
entry. Assignment exclusions live only inside the current negotiation call;
they do not mutate registered values, rules, caller domains, or caller locks.

Only the first complete successful round commits, and it commits once. If the
call ends in contradiction or either limit, every entry's value, empty state,
and generated/lock ownership remain exactly as they were before the call. The
call also restores the caller-selected pass and every pre-call random-stream
state. Commit-hook behavior is therefore the same as one successful ordinary
transaction, not one commit per round.

A caller lock is never negotiated away. A pass consisting entirely of locks
cannot become a choice frame; if no earlier mutable completed frame exists,
the search terminates as `gnsContradiction`.

## statuses and reports

`TGraphNegotiationReport.Status` is one of:

| Status | Meaning |
| --- | --- |
| `gnsSolved` | The final round validated and committed the complete pipeline. |
| `gnsContradiction` | The round failed and no completed negotiable choice frame remained. |
| `gnsSolverBacktrackLimit` | A pass needed another local restoration after `SolveOptions.MaxBacktracks` was exhausted. |
| `gnsPassBacktrackLimit` | Another completed pass assignment could be excluded, but the outer budget was exhausted. |

The report records the seed, negotiation algorithm version, completed outer
`PassBacktracks`, rejected `Attempts`, sole terminal `FinalReport`, and portable
`TranscriptHash`.

`Attempts` contains rejected-and-reopened rounds only. Its length equals the
number of completed pass backtracks. Each
`TGraphNegotiationAttemptReport` owns:

- the ordinary `SolveReport` for that failed round;
- `BacktrackedPassIndex` and its round-local
  `BacktrackedExecutionOrdinal`; and
- the complete copied `ExcludedAssignment` in stable value-index and entry
  order.

`FinalReport` is always the last round, whether it solved, proved a
contradiction, or hit a limit. Total normally returned rounds are therefore
`Length(Attempts) + 1`.

An ordinary per-pass report additionally exposes `ExcludedAssignments`, the
number of exact complete assignments rejected in that pass attempt. A terminal
local exhaustion can use `gckExcludedAssignment`; it is distinct from a model
contradiction because the excluded vector came from the outer search.

## traces and transcript identity

Negotiation does not flatten multiple rounds into one Trace-v1 stream. If
`SolveOptions.CaptureTrace` is enabled, every rejected attempt and the final
round retains its own ordinary, independently valid `TGraphSolveReport`:

- pass events remain contiguous within that report;
- its terminal event is exactly one pipeline commit or rollback; and
- its `TraceHash` remains governed by `WFC_TRACE_VERSION` and
  `WFC_TRACE_HASH_VERSION`.

When a complete local assignment matches an exclusion, its trace records an
entryless contradiction with cause `gtckExactAssignmentExclusion`. The
per-pass `ExcludedAssignments` counter records the same event. See
[causal solve traces](traces.md).

`TranscriptHash` is a separate 32-bit portable FNV-1a summary. Its versioned
input includes the negotiation options and status, seed, pass-backtrack count,
every rejected ordinary report in chronological order, selected pass and
execution ordinal, every exact excluded value index, and the final ordinary
report. `CalculateGraphNegotiationTranscriptHash` recomputes it.

The hash is a replay diagnostic, not assignment identity or cryptographic
integrity. Exact exclusions remain present in `Attempts`; implementations must
compare those complete arrays rather than relying on their hashes.

Exact replay requires the ordinary graph, random, solver, model, pipeline, and
trace identities plus the negotiation algorithm/hash versions, both budgets,
trace-capture setting, full topological plan, and complete caller input. See
[deterministic generation](determinism.md).

## checked fixtures

The focused native/pas2js conformance fixtures preserve the ordinary solver as
their baseline and use caller domains as satisfiability oracles:

| Fixture | One-way trace | Oracle trace | Negotiation transcript |
| --- | --- | --- | --- |
| one cell | `42AF302E` | `AE6B5907` | `6F76591D` |
| two cells | `F63B2846` | `0F79873D` | `04CCD398` |
| provider exhaustion | ordinary rounds retained separately | not applicable | `11F3B018` |
| independent `A`/`B` -> `C` join | ordinary rounds retained separately | not applicable | `41AF694A` |

The two-cell case proves that an exclusion owns a complete ordered vector, not
independent per-cell bans. Provider exhaustion proves that an exhausted pass
fails locally instead of repeating an excluded vector; with no earlier frame,
the standalone fixture terminates as `gnsContradiction`. The
independent-provider join proves the prior-frame case and records the `B, B, A`
backtrack path, including clearing and re-enumerating later `B` context. The
published experimental seed set is exactly `{0}`. The portable
[`03_PassNegotiation`](../examples/passes/03_PassNegotiation/README.md)
example makes the one-cell round sequence inspectable from native FPC and
pas2js/Node.

The experimental method, measurements, limitations, and stopping rules are
recorded in [Pass Negotiation v1 research notes](research/pass-negotiation-v1.md).

## current boundary

Version 1 deliberately does not provide:

- negotiated selective regeneration;
- conflict-directed backjumping or clause learning;
- minimal-change or weighted repair objectives;
- a proof that the result is closest to the pre-call assignment;
- compact partial-assignment nogoods;
- cyclic dependency graphs; or
- a flattened global domain view across passes.

These are possible later algorithms, not undocumented interpretations of this
one. The current runtime path uses repository units and the applicable standard
FPC/pas2js RTL only.

The current evidence does not compare negotiation with an equivalent flattened
single-pass model. Runtime, state-count, memory, and search-efficiency claims
against flattening remain a required next experiment.
