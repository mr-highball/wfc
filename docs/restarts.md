# deterministic whole-transaction restarts

`TGraph.TrySolveRestarted` and `TGraph.TrySolveNegotiatedRestarted` are
opt-in coordinators around complete transactional solve attempts. They keep a
caller's `Graph.Seed` as the public base seed, derive a deterministic effective
seed for each attempt, and retry only when the local solver exhausts its
backtrack budget.

Restarts are useful when a finite local budget is intentional but a different
deterministic branch order may succeed. They do not turn budget exhaustion into
proof, hide contradictions, or change the behavior of `TrySolve`,
`TrySolveNegotiated`, or selective regeneration.

## one-way use

```pascal
uses wfc;

var
  Graph: TGraph;
  Solve: TGraphSolveOptions;
  Restarts: TGraphRestartOptions;
  Report: TGraphRestartReport;
begin
  Graph := TGraph.Create;
  try
    Graph.Seed := 0;
    { Configure dimensions, values, passes, constraints, and caller locks. }

    Solve := DefaultGraphSolveOptions;
    Solve.MaxBacktracks := 32;
    Solve.CaptureTrace := True;

    Restarts := DefaultGraphRestartOptions;
    Restarts.MaxRestarts := 8;
    Restarts.MaxBacktracksPerAttempt := 128;
    Restarts.Schedule := grschCappedDoubling;

    if Graph.TrySolveRestarted(Solve, Restarts, Report) then
      WriteLn('winner seed: ', Report.Attempts[Report.Restarts].Seed)
    else
      WriteLn('terminal status: ', Ord(Report.Status));
  finally
    Graph.Free;
  end;
end;
```

Attempt zero uses the original seed and original local backtrack budget. With
`MaxRestarts = 0`, its Boolean result, output, terminal ordinary report, and
stream state match a direct `TrySolve` call. `Attempts` always includes the
terminal attempt, so a normal returned report satisfies:

```text
Length(Report.Attempts) = Report.Restarts + 1
```

`Report.Restarts` is therefore the zero-based index of the terminal attempt,
not the configured allowance. Every attempt records its index, effective seed,
effective local budget, complete ordinary report, and optional elapsed
diagnostic. `FinalReport` repeats the terminal ordinary report for convenient
status and failure inspection.

## negotiated use

`TrySolveNegotiatedRestarted` accepts a `TGraphNegotiationOptions` value and
stores a complete `NegotiationReport` for each outer attempt:

```pascal
Negotiation := DefaultGraphNegotiationOptions;
Negotiation.SolveOptions.MaxBacktracks := 16;
Negotiation.MaxPassBacktracks := 8;

Restarts := DefaultGraphRestartOptions;
Restarts.MaxRestarts := 4;

Solved := Graph.TrySolveNegotiatedRestarted(
  Negotiation, Restarts, Report);
```

Each restart begins a fresh complete negotiation, including a fresh set of
exact-assignment exclusions. It does not carry an inner nogood from one
effective seed to another. An exhausted pass-negotiation budget is terminal;
outer restarts cannot reinterpret it as a local solver limit.
The existing inner negotiation may reopen an upstream assignment after a
semantic commit rejection; that is still inner pass negotiation, not a new
outer seed restart.

## schedules and bounds

`DefaultGraphRestartOptions` uses zero restarts, a fixed schedule, a
`High(Integer)` per-attempt ceiling, and disabled timing.

`grschFixed` applies the original `SolveOptions.MaxBacktracks` to every
attempt. `grschCappedDoubling` applies this exact integer sequence:

```text
budget(0) = initial
budget(i) = min(cap, budget(i - 1) * 2)
```

The implementation saturates before signed overflow. An initial budget of
zero remains zero; doubling does not invent a positive budget. The ceiling
must be at least the original budget, because attempt zero is never clipped.
`GraphRestartBacktrackBudget` exposes the exact policy calculation for display,
validation, and replay tooling.

`MaxRestarts` must be nonnegative and less than `High(Integer)` so the total
attempt count is representable. The initial budget, cap, and requested attempt
index must also be nonnegative, and the index cannot exceed `MaxRestarts`.
Invalid policy values raise `ERangeError` before solving, without mutating
entries or random streams.

The coordinator allocates one retained report per executed attempt. Each
report can contain a full captured solver trace and, for negotiated mode, every
inner negotiation round. Callers should therefore choose restart, local, and
pass allowances together with whether to enable `CaptureTrace`, rather than
treating `MaxRestarts` as a free limit.

## stopping rules

The outer status describes why the complete policy stopped:

| Status | Meaning |
| --- | --- |
| `grsSolved` | The terminal attempt solved and committed. |
| `grsContradiction` | The underlying attempt ended in `gssContradiction`, including a semantic commit rejection; it was not retried. |
| `grsRestartLimit` | The terminal attempt exhausted its local backtrack budget and no restart remained. |
| `grsPassBacktrackLimit` | Negotiated solving exhausted its pass budget; it was not retried. |

Only an ordinary terminal `gssBacktrackLimit` advances to another effective
seed. `grsContradiction` is terminal for this policy, but it is not a general
claim that the complete multi-pass model is unsatisfiable. An ordinary one-way
attempt can exhaust a consumer after committing to one staged provider
assignment without reopening other provider assignments, and a final semantic
validator can reject an otherwise complete candidate. A restart limit is a
bounded negative result. Exceptions from validation or publication propagate;
they are not silently converted into attempts.

All attempts use the existing pipeline transaction. A failed attempt restores
generated entries, caller locks, selected pass, and random streams before the
next one starts. Only a successful terminal attempt commits, and it exposes the
winning attempt's stream state. `Graph.Seed` is never replaced by the effective
winner seed. External side effects performed by custom hooks are still outside
the graph transaction and cannot be rolled back.

## replay identity

`WFC_RESTART_ALGORITHM_VERSION` and `WFC_RESTART_HASH_VERSION` are both `1`.
`DeriveGraphRestartSeed(BaseSeed, 0)` returns `BaseSeed`; later version-one
attempts XOR the base with the documented 32-bit `fmix32` avalanche of the
attempt index. In unsigned 32-bit arithmetic, for an index greater than zero:

```text
x = index
x = (x XOR (x >> 16)) * $85EBCA6B mod 2^32
x = (x XOR (x >> 13)) * $C2B2AE35 mod 2^32
effectiveSeed = baseSeed XOR x XOR (x >> 16)
```

Shifts are logical; the implementation explicitly computes low-word products
to preserve checked native/browser parity. For base seed zero, the first
effective seeds are:

```text
attempt 0: $00000000
attempt 1: $514E28B7
attempt 2: $30F4C306
attempt 3: $85F0B427
```

Replay the whole policy with the same model, inputs, base seed, ordinary or
negotiated options, restart allowance, schedule, and ceiling. Do not assign the
winning effective seed back to the graph and assume that this recreates custom
hooks which intentionally inspect the public base seed.

`CalculateGraphRestartTranscriptHash` has overloads for one-way and negotiated
options. It covers the declared policy, ordered attempts, effective seeds and
budgets, statuses, and complete nested solve information rather than trusting
stored inner hashes. The hash is a compact restart-policy replay check, not a
cryptographic digest, model/output fingerprint, or substitute for the retained
reports. Restart support does not reinterpret existing canonical model,
pipeline-run, or result artifacts; persist those artifacts under their existing
contracts.

## diagnostic timing

Set `Restarts.MeasureTime := True` to request monotonic elapsed measurements
for the whole coordinator and each attempt. `TimingAvailable` is authoritative;
an unavailable clock leaves the corresponding elapsed value at zero. A clock
failure does not change search, status, output, rollback, or transcript.

Timing is diagnostic and host-dependent. It is excluded from restart
transcript identity, has no deterministic golden value, and does not impose a
deadline or cancellation. `wfc_timing` also exposes
`TryReadWfcMonotonicMilliseconds` and `WfcElapsedMilliseconds` for callers that
need the same checked monotonic measurement primitive.

The whole-policy interval covers attempt coordination and retained report
construction after preflight, ending before the final transcript hash.
Per-attempt intervals cover the underlying solve or negotiation call. Hosts
may return a valid zero interval for work below their clock resolution.
The protected `DoReadMonotonicMilliseconds` hook must remain observational and
side-effect-free. Clock exceptions and invalid/backward readings are treated
as unavailable timing; arbitrary external side effects in an override cannot
be undone by the graph.

## checked example

The shared [Deterministic Restarts example](../examples/passes/05_DeterministicRestarts/README.md)
contains one-way recovery, immediate contradiction, capped budget growth, and
negotiated recovery. Its native host accepts no arguments for deterministic
output, `--selftest` for portable conformance, or `--timing` to add optional
diagnostics. The shared helper and wrapper test also compile for the browser
target.

Restarts are complete-pipeline attempts. Version one does not provide a
selective restart horizon, cross-attempt conflict learning, parallel attempts,
wall-clock cancellation, or a performance guarantee. It preserves the same
acyclic pass model, ordinary solver constraints, and negotiated inner search
documented in [the reference solver](solver.md) and
[bounded pass negotiation](pass-negotiation.md).
