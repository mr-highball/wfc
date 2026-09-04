# NegotiatedRepair

`NegotiatedRepair` is the executable Selective Negotiation v1 fixture. It
starts from a committed five-pass, one-cell district, asks for housing that is
incompatible with the current road, and compares two explicit repair horizons:

```text
terrain -> roads -> housing -> decor
climate ------------------------^
```

The stable pass indices and topological execution order are `terrain=0`,
`climate=1`, `roads=2`, `housing=3`, and `decor=4`. Every pass is an overlay.
`home` requires a `trail`; `market` requires a `plaza`; decor is the exact
join of housing and climate.

Both thin hosts call the same self-checking Pascal unit. Native FPC and
pas2js/Node need only repository units and their applicable standard RTL.

## two repair horizons

The seed-zero baseline is:

```text
meadow | rain | trail | home | porch
```

The fixture then applies a caller-owned `market` constraint to housing. Calling
`TryRegenerateNegotiatedFrom('housing', ...)` reports requested roots `[3]`
and active passes `[3,4]`. Roads is an ancestor outside that descendant
closure, so it is a reused immutable input and never a choice frame. The
horizon cannot satisfy `market` over `trail`; it returns a housing dependency
contradiction with no pass backtrack and restores the complete transaction.

Calling `TryRegenerateNegotiatedFrom('roads', ...)` instead reports roots `[2]`
and active passes `[2,3,4]`. The first round repeats `trail`, contradicts the
housing constraint, and records that complete roads assignment as excluded.
The one permitted pass backtrack reopens roads and the next round commits:

```text
meadow | rain | plaza | market | market-lights
```

Terrain and climate stay outside the horizon. The demo checks their values and
random streams remain unchanged. It also checks exact pass dispositions, the
rejected-round cause, every captured causal trace, transcript recomputation,
atomic failure, same-seed replay, and equivalence with a separately run
full-pipeline negotiated search. The focused conformance suite separately
checks that a caller-owned terrain lock outside the horizon remains
caller-owned after successful repair.

The horizon is deliberate: Selective Negotiation v1 does not widen a
too-narrow request. See the
[selective-negotiation contract](../../../docs/selective-negotiation.md) for
the canonical root and descendant-closure equation.

## build and run

From the repository root, create the named output directories and compile the
native host:

```text
fpc -B -Mdelphi -Sa -Cr -Co -Ci -Fusrc -Fuexamples/2D/04_NegotiatedRepair -FUbuild/examples/negotiated-repair/native/units -FEbuild/examples/negotiated-repair/native/bin examples/2D/04_NegotiatedRepair/NegotiatedRepair.lpr
build/examples/negotiated-repair/native/bin/NegotiatedRepair
```

Compile the Node host with a configured pas2js compiler and matching RTL:

```text
pas2js -B -Tnodejs -Mdelphi -Fusrc -Fuexamples/2D/04_NegotiatedRepair -FUbuild/examples/negotiated-repair/pas2js/units -FEbuild/examples/negotiated-repair/pas2js/bin examples/2D/04_NegotiatedRepair/NegotiatedRepairNode.lpr
node build/examples/negotiated-repair/pas2js/bin/NegotiatedRepairNode.js
```

The repository-wide `build.ps1` and `build.sh` gates also compile and smoke-test
the native host. Hosted CI compiles and runs the Node host as a second edge.

## expected output

Both hosts emit these lines exactly:

```text
NegotiatedRepair: terrain -> roads -> housing -> decor <- climate
Seed: 0
Selective negotiation versions: algorithm=1 hash=1
Baseline: terrain=meadow climate=rain roads=trail housing=home decor=porch
Requested housing: market
Leaf horizon: requested=[3] active=[3,4] status=contradiction roads=reused hash=7A595E38
Roads horizon: requested=[2] active=[2,3,4]
Rejected round 0: roads=[trail] housing=contradiction
Repaired: terrain=meadow climate=rain roads=plaza housing=market decor=market-lights
Selective rounds: 2 pass backtracks: 1
Reused providers: terrain,climate values-and-rng=preserved
Nested negotiation hash: 80926222
Selective transcript hash: E29050A0
Full baseline: active=[0,1,2,3,4] transcript=9DB789E4
Deterministic replay: identical transcript and output
Self-check: passed
```

`80926222` identifies the nested Pass Negotiation v1 search. `E29050A0`
additionally commits the separately versioned selective scope. `7A595E38`
identifies the failed housing-only scope. `9DB789E4` is this five-pass
fixture's full-pipeline baseline; it is not the separate one-cell Negotiation
v1 conformance golden `6F76591D`, which the focused suite also checks remains
unchanged. A change to one of these goldens requires an explicit version and
compatibility decision, not an automatic constant update.

## limits and troubleshooting

- `MaxPassBacktracks` and `SolveOptions.MaxBacktracks` are separate budgets.
  The former limits whole-pass rounds; the latter limits local cell search in
  each round.
- Version 1 excludes complete pass assignments chronologically. Its worst-case
  search is exponential. It is not conflict-directed and does not claim a
  cell-minimal, pass-minimal, change-minimal, or horizon-minimal repair.
- A housing-root contradiction is expected. If it succeeds, an ancestor was
  reopened or the fixture constraints changed; if the roads-root case fails,
  inspect the retained rejected-round report before changing either budget.
- An unknown or empty root list is caller error. Duplicate or reordered valid
  labels are canonicalized by stable pass index before the scope is hashed.
- Compiler output belongs under the ignored `build/` tree. No package manager,
  service, JavaScript library, playback stack, or platform UI toolkit is part
  of this example.

The focused
[selective-negotiation conformance suite](../../../test/wfc_selective_negotiation_test.lpr)
covers additional root canonicalization, a multi-root join, zero-budget
ordinary-report parity, rollback, provider immutability, and isolation from
ordinary selective and full-pipeline behavior.
