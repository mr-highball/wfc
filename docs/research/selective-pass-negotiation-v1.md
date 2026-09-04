# Selective Pass Negotiation v1 research record

- **Status:** experimental, reproducible reference algorithm
- **Scope algorithm:** `WFC_SELECTIVE_NEGOTIATION_ALGORITHM_VERSION = 1`
- **Transcript encoding:** `WFC_SELECTIVE_NEGOTIATION_HASH_VERSION = 1`
- **Nested search:** Pass Negotiation v1
- **Published seed set:** `{0}`

## question and hypothesis

Full-pipeline pass negotiation can recover from a downstream contradiction by
enumerating earlier complete pass assignments, but reopening every mutable pass
is too broad for an editor or simulation that changed one owned subsystem. The
research question is:

> Can exact chronological pass negotiation be restricted to an explicit
> descendant-closed repair horizon while preserving clean layers and random
> streams, ordinary selective-regeneration evidence, deterministic replay, and
> whole-call atomicity?

The version-1 hypothesis is deliberately narrow: canonical requested roots
plus their transitive dependency descendants provide a deterministic,
inspectable authorization boundary. Search inside that boundary should retain
the existing whole-assignment negotiation behavior, while passes outside it
remain immutable even if a wider search could find a solution.

This does not hypothesize minimal edits, conflict-directed repair, efficient
global optimization, or automatic discovery of the best repair horizon.

## baselines

The experiment keeps three independently versioned baselines:

1. `TryRegenerateFrom` performs one atomic selective attempt over an explicit
   descendant closure and never reopens a completed active pass.
2. `TrySolveNegotiated` searches the complete pipeline with Pass Negotiation
   v1 and has no clean-pass boundary.
3. `TryRegenerateNegotiatedFrom` uses the first baseline's exact scope and the
   second baseline's exact chronological search only within that scope.

The first two APIs are controls, not aliases for the new method. Their existing
reports, outputs, and portable identities must remain unchanged.

## method

For requested labels `L`:

1. Resolve every label to a stable pass index before solving.
2. Deduplicate and sort those indices in stable creation order to produce
   `R = RequestedRootIndices`.
3. Synchronize implicit dependency roles.
4. Compute the least set `A` containing `R` and every transitive consumer of a
   pass already in `A`.
5. Record `A` in stable full-pipeline topological order as
   `ActivePassIndices`.
6. Run ordinary selective rounds over exactly `A`, retaining all other passes
   as immutable staged inputs.
7. On failure, choose the latest completed mutable defined pass in the active
   round order, exclude its complete value-index assignment, and repeat within
   the same horizon.
8. Commit only the first completely validated successful active closure.

An ancestor outside `A` is never added, never selected as a choice frame, and
never has its random stream rewound or consumed. This remains true when the
terminal contradiction identifies that ancestor as a provider.

## checked fixture families

### bounded repair

The primary fixture begins from a committed multi-pass state, changes an
authorized root input, and creates a downstream contradiction that ordinary
selective regeneration cannot repair after its first valid-but-incompatible
active assignment. Selective negotiation excludes that exact assignment and
recovers another active composition without reopening a clean provider.

The fixture records canonical roots, active topological order, the rejected
assignment, round reports, local and outer counters, final public layers, and
the selective transcript.

### too-narrow horizon

A leaf-only request is intentionally unable to change a clean provider. The
method must contradict or exhaust within that leaf scope rather than silently
widening it. A second call with an explicitly earlier root demonstrates that a
wider caller-authorized descendant closure can search additional frames.

This pair distinguishes a repair horizon from dependency-cause backjumping.

### canonical roots and joins

Multiple roots are supplied in different orders and with duplicates. Their
reports must contain the same ascending stable `RequestedRootIndices`, the
same union descendant closure in stable topological order, and the same
transcript. Shared join descendants execute once per round.

### clean branch immutability

An independent or sibling pass outside the active closure is snapshotted
before the call. A successful repair must preserve its entries, empty flags,
generated/lock ownership, and random stream exactly. A failed repair extends
that check to every pass, every stream, and caller selection.

### zero outer budget

With `MaxPassBacktracks = 0`, selective negotiation performs one ordinary
selective attempt. The nested `Search.FinalReport` must equal the
`TryRegenerateFrom` report produced by an independent twin with the same
canonical roots, solve options, and pre-call state. No rejected attempt is
invented.

### version isolation

The existing full-pipeline Pass Negotiation v1 fixtures run unchanged. A
full-scope selective call may contain the same nested search evidence, but its
outer scope transcript remains separately versioned and includes its canonical
root and active arrays. Ordinary full and selective solves also retain their
prior identities.

### guards and ownership

Additional cases cover unknown and empty root sets, negative budgets,
caller-locked-only active passes, local backtrack exhaustion, outer-budget
exhaustion, reused providers, replay on independent twins, and transcript
tampering. Preflight failures and unsuccessful calls must leave graph and
random state unchanged.

## measured observables

The conformance fixtures record:

- Boolean outcome and `Search.Status`;
- scope and nested-search algorithm versions;
- canonical requested-root and active-pass arrays;
- total rounds, rejected attempts, and outer pass backtracks;
- every chosen frame and complete excluded assignment;
- per-pass decisions, propagations, contradictions, local backtracks, and
  excluded-assignment counts;
- ordinary Trace-v1 reports and hashes for each round when capture is enabled;
- the nested Pass Negotiation v1 transcript;
- the outer selective transcript and independent recomputation;
- final committed active output and ownership; and
- clean and failed-call entry, selection, and random-stream parity.

The published fixtures use seed `0`. The packaged negotiated-repair demo and
its four goldens were reproduced byte-for-byte under current FPC 3.3.1, stable
FPC 3.2.2, and pas2js/Node from the same shared Pascal unit. The focused
conformance suite reports 124 checks and zero failures on all three hosts from
one portable Pascal source. The check count is intentionally not part of the
research identity; semantic assertions and portable transcript/output goldens
are.

## fixed outcomes

The primary repair fixture has stable creation and execution indices
`terrain=0`, `climate=1`, `roads=2`, `housing=3`, and `decor=4`. Its committed
seed-zero baseline is:

```text
meadow | rain | trail | home | porch
```

After housing is constrained to `market`, the fixed outcomes are:

| Call | Requested roots | Active passes | Outcome | Portable transcript |
| --- | --- | --- | --- | --- |
| housing-root selective | `[3]` | `[3,4]` | contradiction, no choice frame | `7A595E38` outer |
| roads-root selective | `[2]` | `[2,3,4]` | solved after excluding `roads=[trail]` | `80926222` nested, `E29050A0` outer |
| full-pipeline negotiated control | all passes | `[0,1,2,3,4]` | solved after the same roads exclusion | `9DB789E4` |

The roads-root and full-pipeline controls both commit:

```text
meadow | rain | plaza | market | market-lights
```

The separate contextual-join fixture requests independent roots `[1,2]`,
activates `[1,2,3]`, and follows frame path `2,2,1`: it exhausts `B` before
reopening `A`, clears the stale `B` exclusions for the new prefix, and solves
with outer transcript `DE454137`. Its stable pass at index `0` remains outside
the horizon and caller-owned.

The five-pass full-pipeline control `9DB789E4` is a fixture-specific Pass
Negotiation v1 transcript, not a replacement for the original one-cell
Negotiation v1 golden `6F76591D`. The focused isolation check retains that
earlier value unchanged.

## findings

The checked fixtures establish these claims and no broader ones:

1. **Scope can be an authorization boundary.** A caller can permit changes to
   roots and their descendants without exposing clean ancestors or siblings to
   search.
2. **Chronological search composes with selective staging.** Exact active pass
   assignments can be rejected across ordinary selective rounds while clean
   providers remain readable.
3. **Root canonicalization removes spelling-order noise.** Duplicate labels
   and caller ordering do not change the canonical scope or transcript.
4. **Zero outer budget preserves the ordinary baseline.** The single terminal
   ordinary report remains the evidence for what one-way selective regeneration
   attempted.
5. **Separate versioning protects earlier contracts.** Selective scope adds an
   outer identity without redefining Pipeline v2 or Pass Negotiation v1.

They also retain the negative and boundary observations: the housing-root
horizon cannot repair the request, a caller-locked roads assignment is never
negotiated away, zero pass budget leaves the ordinary selective final report
unchanged while reporting the distinct outer limit, and zero local budget
cannot escape a recorded whole-roads exclusion. Empty or unknown roots and
negative budgets fail before mutation.

## costs and negative results

Selective scope reduces which passes may be enumerated; it does not change the
worst-case complexity of those that remain. A pass with `c` cells and `v`
possible values can still expose up to `v^c` complete assignments, and joins
can form a product across active choice frames. Exact exclusion storage grows
with the number and size of rejected vectors.

The algorithm also intentionally accepts a negative result: a too-narrow
horizon can fail even though a full-pipeline or wider-root search succeeds.
That is not incompleteness relative to the requested scope. It is enforcement
of the caller's ownership boundary.

Whole-assignment exclusions provide exact reproducible evidence but do not
identify the smallest conflicting cell subset. Chronological frame selection
does not follow the contradiction's provider cause and can spend budget on an
unrelated later active frame. No fixture is described as cell-minimal,
pass-minimal, conflict-directed, or more efficient than a flattened global
model.

## stopping rules

The experiment stops with the nested Pass Negotiation v1 statuses:

- `gnsSolved` at the first validated active closure;
- `gnsContradiction` when no completed mutable active frame remains;
- `gnsSolverBacktrackLimit` when a local active solve exhausts its limit; or
- `gnsPassBacktrackLimit` before another active assignment exclusion would
  exceed the outer limit.

No stop condition widens `ActivePassIndices`.

## reproducibility and dependency boundary

Reproduction requires the complete pipeline definition and committed pre-call
state, canonical roots and active closure, graph/solver/random/trace versions,
both Pass Negotiation v1 versions, both selective-negotiation versions, both
budgets, exact exclusions, and seed `0`.

The implementation and experiments are project-owned Pascal using repository
units and the applicable standard FPC or pas2js RTL. No third-party solver,
graph library, serializer, or hosted service participates in scope planning,
search, validation, hashing, or replay.

The first non-spatial application is recorded independently in
[`music-negotiated-variation-v1.md`](music-negotiated-variation-v1.md). It
uses the same exact descendant-closure contract to repair harmony and melody
while retaining rhythm as an immutable clean provider; it does not extend the
algorithmic claims made here.

## next experiments

Later work may compare:

- several caller-chosen horizons for the same edit;
- full versus selective negotiation state counts, memory, and round counts;
- dependency-cause backjumping against chronological frame selection;
- partial or cell-level nogoods against complete assignment exclusions;
- explicit edit-distance objectives against first-solution search; and
- equivalent flattened models where a faithful representation is practical.

Those experiments require new algorithms, fixtures, and versioned claims.
They are not implied by Selective Negotiation v1.
