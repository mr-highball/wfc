# Declared composition for independent musical voices

Big Boss, 2026-11-01. Branch `codex/pythian-context-composition`, based on
`448b2a3e25beded8503e94b5f4705b7156eca966`. No branch merge is implied.

The motivating caller committed an upstream symbolic context stream before
conditional independent voices could test it. Later voice infeasibility could
not negotiate that already committed context. A standalone two-cell WFC-only
probe demonstrated the integration gap: the generic graph solved an abstract
`a`/`b` source projected into harmony, while the primary voice verifier correctly
rejected the undeclared extra pass under its previous closed-layout contract.

The repair extends the primary voice configuration with declared auxiliary
sequence models and token projections. It uses the existing graph dependency
planner, sequence graph adapters, negotiated solver, independent voice capture
and bounded stream. Musical codecs stay with the caller. Arbitrary undeclared
passes remain invalid. There is no second solver or compatibility implementation.

The sequence graph adapter now shares public projection-rule preparation between
graph installation and independent token proof. Empty alternatives remain real
denials. The voice verifier additionally authenticates `MinimumFollowingCells`
along with the original boundary fields. All declared frontiers commit together
after full-window proof; no provisional suffix is published.

The first stream fixture used an order-one corpus while intending to encode
constant self transitions. Order one has zero history and intentionally allows
unobserved pair combinations. The retained run reached its ordinary configured
negotiation limit; this was a fixture-model mistake, not evidence that a larger
budget or shorter lookahead repaired composition. The corrected order-two
fixture encodes those intended transitions and keeps seed, window and budgets.
Native and actual pas2js both find the same negotiation escape seed (1).

Current focused evidence: 136 new native checks with zero unfreed blocks and
136 actual pas2js checks. Four no-composition signatures also match a fresh
standalone build against unchanged448b2a3 exactly on both targets. Existing
graph187, partial-projection29 and stream1834
native checks pass with clean heaps. The complete preparation suite also passes
533791 checks, 132 original abort/retries and 456086 actual callbacks with zero
unfreed blocks. Complete [upstream CI65](https://github.com/mr-highball/wfc/actions/runs/41277896792)
passes all five Linux/Windows/macOS stable, official trunk and actual-browser
jobs at37c2755. The Pythian typed-context finite/stream consumer also passes44
native and actual pas2js checks, including an extracted-package pas2js build.
Its own CI/package completion gates remain separate. Actual downstream musical
integration and its original failure are separate acceptance gates; no useful
musical outcome follows automatically from these generic checks.

Generated local evidence is retained outside source control in the dedicated
`context-composition-20261101-boss` build/cache/run packet. The original caller's
four rejected compatibility-filter candidates and minimal reproduction remain
in its separate `unguided-form-20261101-boss` packet. No recordings or saved
operator answers are fixtures in this repository.

The first downstream instrumental stream exposed an additional acceptance
defect at37c2755: `CopyFrames` still used total model count minus two as its
pitched voice count. A percussion auxiliary was decoded as an ensemble token,
raising before any audio. Finite proof and the initial generated-layer checks
had missed that endpoint. The segment now retains the actual declared voice
count; frame copies exclude auxiliary tokens and remain detached. The expanded
native/actual pas2js fixture passes144 checks, including the original default
signature oracle. Preserve CI65 as success for its exact source; qualify this
repair with its own CI before accepting the downstream task.
