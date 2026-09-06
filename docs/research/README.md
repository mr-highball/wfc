# Research records

These records explain WFC's pass-based methods, representation experiments,
counterexamples, and measured fixtures. They are project research notes, not
a claim that the ecosystem is finished or that each method is novel in the
literature. Each record owns its version, evidence, and limitations.

For the original pass-system question, start with
[Pass Negotiation](pass-negotiation-v1.md), then
[Pattern-Projected Pass Composition](pattern-projected-passes-v1.md).
For unlike-sized layers, continue with the reproducible
[Mapped World footprint study](mapped-world-footprints-v1.md): a corner can be
clear while the space a house actually occupies is not.
For a non-spatial application of the same idea, read
[Independent Voices](independent-voices-v1.md).
The [roadmap](../../ROADMAP.md) distinguishes delivered mechanisms from
the broader work still required.

## Search and pass repair

| Record | Question and boundary |
| --- | --- |
| [Indexed decision selection](decision-index-v1.md) | Replace repeated whole-grid observation scans with a reversible-domain-aware index while retaining exact choices, traces and random calls. Controlled whole-solve measurements, not a universal speedup or compressed-domain claim. |
| [Pass Negotiation v1](pass-negotiation-v1.md) | Recover from downstream contradictions by excluding exact completed upstream assignments. A chronological, bounded baseline; not conflict-directed or optimal search. |
| [Selective Pass Negotiation v1](selective-pass-negotiation-v1.md) | Restrict negotiation to requested roots and their descendants while preserving clean providers. The authorized repair horizon is not a minimal-edit objective. |
| [Rooted connectivity v1](rooted-connectivity-v1.md) | Propagate required reachability through possible ports and reversible domains. Finite independent oracles and a [recorded export matrix](rooted-connectivity-v1.csv); not general relative-efficiency evidence. |

## Representations, learning, and composition

| Record | Question and boundary |
| --- | --- |
| [Pattern-Projected Pass Composition v1](pattern-projected-passes-v1.md) | Materialize overlapping latent patterns as a public pass consumed by later semantic layers. Version 1 covers wrapped, same-shape, depth-one hard constraints, not general resampling. |
| [Overlapping volumes and public constraints v1](overlapping-volumes-v1.md) | Literal cuboid learning, exact six-direction overlap and inverse public XYZ domains, including aliased offsets. Native/browser conformance; not yet a full training/workspace or interactive volume vertical slice. |
| [Volume Observation Orbits v1](volume-observation-orbits-v1.md) | Aggregate transformed six-direction observations exactly, checked against literal coordinate transforms. Scalar token payloads do not rotate; cardinal relations are not joint 3D footprints. |
| [Model-to-voxel pass experiment v1](model-to-voxel-pass-v1.md) | Connect learned terrain to authored structural interpretations and offset foliage constraints. Independent semantic validation checks the bridge; the fixture does not establish chunked scalability. |
| [Mapped World footprints v1](mapped-world-footprints-v1.md) | Compose coarse terrain, fine foliage and inset housing in integer world coordinates. A generated interior blocker distinguishes point-model validity from full-footprint clearance; explicit provider repair is bounded, not guaranteed or minimum-change. |
| [Portable Pipeline Bundle v1](portable-pipeline-bundle-v1.md) | Reconstruct supported declarative pipelines from canonical recipes, runs, and results. This is a closed, versioned execution contract, not serialization of arbitrary live graphs or callbacks. |
| [Portable public quotas v1](portable-value-quotas-v1.md) | Lower whole-pass public quantities through exact-copy and latent projection layers, then independently recount before commit. Same-shape supported bridges only; not generalized count optimization. |

## Music

| Record | Question and boundary |
| --- | --- |
| [Music Negotiated Variation v1](music-negotiated-variation-v1.md) | Repair harmony and melody within a selected closure while preserving rhythm and a locked motif. A small feasibility/transaction fixture, not a musical-quality study. |
| [Music Studio form experiment v1](music-studio-form-v1.md) | Compare rhythmic context choices over a fixed seed set, with a [checked result matrix](music-studio-form-v1.csv). The admitted language changes, so this is not an equivalent-model solver benchmark. |
| [Independent Voices v1](independent-voices-v1.md) | Coordinate separately learned voice vocabularies through collective harmony without a joint-frame vocabulary. Distinct truth-table and temporal-corpus fixtures; compatible tuples can still require combinatorial search. |

## Transactions and evidence

| Record | Question and boundary |
| --- | --- |
| [Text-pass publication](text-pass-publication-v1.md) | Reproduce a public-capture failure after graph commit, then move semantic acceptance inside rollback. Atomic output does not imply rollback of caller-authored model edits. |
| [Chronological trace layout v1](chronological-trace-layout-v1.md) | Represent a late rejection of an earlier pass without rewriting event order or legacy hashes. Structural evidence is not semantic validation or cryptographic provenance. |
| [Streaming causal traces v1](streaming-causal-traces-v1.md) | Deliver the same event stream without hidden full-history retention, using full capture as a comparison oracle. Bounded retention reports missing evidence; delivery is synchronous and numeric capacities remain finite. |

## Reproduce and interpret a result

Use the [build guide](../building.md) for native FPC gates, pas2js staging,
and browser execution with the included FPC server and checker. Each record
links its relevant fixtures, tests, or demo. A compiled browser program is
not evidence that its asynchronous execution or file transaction completed.

Historical measurements remain attached to their recorded versions and
execution paths. In particular, a historical pas2js number does not by itself
prove coverage in today's browser gate. Check the current test manifest,
executed results, and any record-specific execution note before repeating a
coverage claim.

Keep these distinctions when comparing experiments:

- A valid assignment proves that fixture's constraints, not aesthetic quality.
- Exhausting a search budget is not proof of unsatisfiability.
- Changing the admitted language is a modeling experiment, not an isolated
  comparison of solver efficiency.
- A deterministic hash is a regression identity, not an independent semantic
  validator or a cryptographic proof.
- Finite conformance checks do not establish a universal performance bound.

## Add an experiment

Radical ideas are welcome. Make them falsifiable and reproducible:

1. State the question, the representation, and a counterexample that would
   reject the hypothesis. Name the algorithm and artifact versions involved.
2. Define the baseline. For an efficiency comparison, establish that both
   encodings admit the same public outputs before comparing search costs.
3. Publish the complete fixture, provenance, seed set, budgets, and stopping
   rule. Use project-authored MIT-compatible fixtures; do not silently import
   third-party code or data.
4. Supply an independent checker or oracle where practical. Record failed
   runs, exhausted budgets, and unexpected cases alongside successes.
5. Record compiler/runtime versions and reproduction commands. Distinguish
   native execution, browser execution, and compilation-only checks.
6. Separate observed findings, inferences, and open questions. Version changed
   semantics and retain the earlier record rather than rewriting its results.

Implement portable methods in project-owned Pascal, following the
[dependency boundary](../building.md#dependency-boundary). Source, fixtures,
and explanatory records remain under the repository's [MIT license](../../LICENSE).
