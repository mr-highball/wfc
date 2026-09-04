# Pass Negotiation v1 research record

- **Status:** experimental, reproducible reference algorithm
- **Algorithm:** `WFC_PASS_NEGOTIATION_ALGORITHM_VERSION = 1`
- **Transcript encoding:** `WFC_PASS_NEGOTIATION_HASH_VERSION = 1`
- **Published seed set:** `{0}`

## question and hypothesis

An acyclic pass pipeline avoids the state-space explosion of flattening every
domain into one value vocabulary, but ordinary one-way staging can retain an
upstream choice that makes a later pass impossible. The research question is:

> Can a small, deterministic outer search recover from that downstream
> contradiction while preserving pass ownership, ordinary solver semantics,
> native/pas2js replay, and whole-call atomicity?

The version-1 hypothesis is that chronological exclusion of exact completed
pass assignments is a useful, inspectable baseline. It should solve fixtures
where a compatible composition exists, retain exact evidence for every rejected
round, and terminate predictably under separate local and pass budgets.

The hypothesis does not claim efficient global optimization, minimal repair,
or conflict-directed search.

## baseline and method

The baseline is ordinary `TGraph.TrySolve`: passes execute once in stable
topological order, local chronological backtracking stays inside each pass,
and a downstream contradiction rolls the complete transaction back without
reopening a staged provider.

The experiment wraps that unchanged baseline with
`TGraph.TrySolveNegotiated`. After a failed round it selects the latest
completed negotiable pass, records and excludes that pass's complete stable
value-index assignment, clears exclusions belonging to all later passes when
the prefix changes, and reruns the complete pipeline from the same seed. Only
the final successful round commits.

This is whole-assignment, global chronological enumeration. It is intentionally
simple enough to specify and reproduce before experimenting with partial
nogoods or conflict-directed backjumping.

## fixtures

### one-cell provider and consumer

Seed zero selects `marsh` in `terrain`; the sole `housing` candidate requires
`meadow`. The one-way baseline fails. A caller domain forcing `meadow` is the
satisfiability oracle. One pass backtrack excludes the exact terrain assignment
and lets the local solver restore its next alternative.

Expected portable identities:

- one-way Trace v1: `42AF302E`;
- caller-domain oracle Trace v1: `AE6B5907`; and
- negotiated transcript: `6F76591D`.

### two-cell exact vector

The provider has two cells. The rejected vector is recorded and compared as a
complete ordered assignment. Excluding `[0, 1]` must not ban either component
from every other vector. A caller-domain oracle proves the compatible result
without using negotiation.

Expected portable identities:

- one-way Trace v1: `F63B2846`;
- caller-domain oracle Trace v1: `0F79873D`; and
- negotiated transcript: `04CCD398`.

### independent providers and a join

Independent passes `A` and `B` both feed `C`. The stable plan is `A, B, C`.
The fixture checks nested chronological enumeration: exhaust `B` assignments
under fixed `A`, backtrack `A` when `B` can no longer complete, clear the stale
later `B` exclusions, and enumerate `B` again under the new `A` prefix.

This fixture distinguishes chronological search from a dependency-cause jump.
The chosen frame is the latest completed pass, not necessarily the provider
named by the final contradiction. Its transcript is `41AF694A` and records the
expected `B, B, A` pass-backtrack sequence before later `B` context is cleared
and enumerated again.

### provider exhaustion

A provider whose exact assignments are exhausted must fail locally rather than
repeat an excluded vector. Because that pass did not complete in the exhausted
round, it cannot be selected as a choice frame. The standalone fixture has no
earlier completed choice frame and therefore terminates as
`gnsContradiction`; the join fixture separately proves selection of a preceding
frame and clearing of the exhausted provider's now-stale exclusions. The
portable standalone transcript is `11F3B018`.

### limits and ownership

Additional cases cover:

- a zero pass budget reproducing the one-way terminal report;
- a positive pass budget with zero local backtracks stopping at the exact
  exclusion rather than escaping it;
- failed-call restoration of entries, generated flags, selected pass, and
  every random stream;
- replay on an already solved graph and on an independent twin;
- caller-locked-only providers terminating without wasting pass budget; and
- negative bounds failing preflight without mutation.

## published portable identities

All published fixtures use seed `0`.

| Fixture | One-way trace | Oracle trace | Negotiation transcript | Outer path |
| --- | --- | --- | --- | --- |
| one cell | `42AF302E` | `AE6B5907` | `6F76591D` | terrain |
| two cells | `F63B2846` | `0F79873D` | `04CCD398` | terrain |
| provider exhaustion | per-round Trace v1 | not applicable | `11F3B018` | provider twice, then terminal local exhaustion |
| independent `A`/`B` -> `C` join | per-round Trace v1 | not applicable | `41AF694A` | `B, B, A` with later `B` context reset |

## measured observables

Each fixture records:

- outcome and terminal status;
- total rounds (`Length(Attempts) + 1`);
- outer `PassBacktracks`;
- selected pass index and round-local execution ordinal;
- exact excluded value-index arrays;
- per-pass decisions, propagations, contradictions, local backtracks, and
  excluded-assignment counts;
- ordinary Trace-v1 hashes for each round;
- the complete negotiation transcript hash;
- committed graph state and ownership; and
- post-run random-stream parity against an untouched twin.

The conformance sources run under the current native compiler, the stable FPC
3.2.2 line, and pas2js/Node. Check totals are intentionally not part of this
research identity; semantic assertions and portable goldens are.

## findings

The bounded fixtures support the narrow hypothesis:

- exact assignment exclusion can recover a compatible upstream alternative;
- one-cell and multi-cell exclusions retain their intended identity;
- failed rounds are transactionally inert and only final success commits;
- independent runs replay the complete report and transcript; and
- ordinary `TrySolve` remains isolated and reproduces its baseline trace after
  negotiated solving.

The experiments also expose important costs:

1. **The two budgets are distinct, not interchangeable.** Reaching a complete
   excluded assignment creates a local contradiction. Replaying exclusions
   therefore consumes local solver backtracks in every affected pass attempt.
   Outer pass budget alone cannot make progress.
2. **Whole assignments are coarse nogoods.** They preserve exactness and make
   diagnostics straightforward, but the number of possible vectors grows
   exponentially with cells and values.
3. **Chronological search can spend unrelated work.** The coordinator does not
   follow the reported dependency cause. A later independent completed pass
   may be enumerated before the causally relevant provider.
4. **A changed prefix invalidates later knowledge.** Later-pass exclusions are
   contextual. They must be cleared and rediscovered when an earlier pass
   changes.
5. **Attempt evidence grows with the search.** Every rejected round retains an
   ordinary report and, when requested, its complete Trace-v1 event array.
6. **The method is full-pipeline only.** Reopening providers during selective
   regeneration needs a separate horizon, reuse, and replay contract.

These are algorithm properties, not implementation accidents.

The current experiment does not construct or benchmark an equivalent flattened
single-pass model. It therefore makes no claim that the pass search uses less
time, memory, propagation, or state than flattening. That controlled comparison
is explicitly deferred to the next experiment.

## stopping rules

Every experiment sets both limits before execution:

- stop with `gnsSolverBacktrackLimit` when an ordinary pass attempt exhausts
  `SolveOptions.MaxBacktracks`;
- stop with `gnsPassBacktrackLimit` before adding an exclusion beyond
  `MaxPassBacktracks`;
- stop with `gnsContradiction` when no completed negotiable choice frame
  remains; or
- stop with `gnsSolved` at the first fully validated pipeline.

No benchmark may report a budget-limited run as proof of unsatisfiability.
Likewise, a solved run proves one compatible composition, not a minimal or
optimal repair.

## reproducibility

Run the repository's checked native gate with `build.ps1` or `build.sh`, and
run the same negotiation conformance source through pas2js/Node. The portable
one-cell demonstration and its commands are documented under
[`examples/passes/03_PassNegotiation`](../../examples/passes/03_PassNegotiation/README.md).

Successful reproduction requires exact agreement on ordinary trace goldens,
negotiation transcripts, assignment arrays, counters, output, rollback, and
random-stream checks. The implementation uses only project units and the
applicable standard RTL.

## next experiments

Version 1 is the baseline for comparing:

- conflict-directed backjumping against global chronological order;
- partial-assignment and clause-shaped nogoods against exact vectors;
- an equivalent flattened-model baseline measuring rule/state count, runtime,
  memory, propagation, and backtracking;
- bounded minimal-change objectives;
- broader selective negotiated regeneration experiments beyond the delivered
  explicit-horizon algorithm;
- compact or streaming attempt evidence; and
- further domain integrations in 3D and text, plus richer 2D/music fixtures.

Selective Negotiation v1 and the first 2D and music integrations are now
published separately in
[`selective-pass-negotiation-v1.md`](selective-pass-negotiation-v1.md), the
[negotiated 2D repair guide](../../examples/2D/04_NegotiatedRepair/README.md),
and
[`music-negotiated-variation-v1.md`](music-negotiated-variation-v1.md).

Those mechanisms should keep separate algorithm versions and publish the same
fixtures, seed sets, counters, negative findings, and stopping rules before
becoming stable API claims.
