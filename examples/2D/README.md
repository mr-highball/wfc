# 2D examples

[`01_MultiPassWorld`](01_MultiPassWorld/README.md) is the dedicated portable 2D
ecosystem demo. It generates terrain, then biome, then foliage with the atomic
reference solver, validates the result independently, renders every layer, and
prints stable per-layer signatures. Its native FPC host uses portable model
and validation units without external dependencies.

[`02_BrowserWorld`](02_BrowserWorld/README.md) compiles the same model,
validator, and shared showcase fixture to a real browser application. It shows
the three layers on synchronized canvases, supports deterministic seeds,
wrapping, and cell locks, and exposes an exact seed-zero self-test for headless
Chrome. Its generated JavaScript is staged under `build/`, not committed.

[`03_SelectiveSettlement`](03_SelectiveSettlement/README.md) is the first
dependency-DAG domain. Terrain branches into hydrology and biome; roads join
those branches; housing and foliage add later constraints. Its portable native
FPC/pas2js fixture changes one hydrology cell, regenerates only hydrology and
its transitive dependents, verifies untouched layers byte-for-byte, proves
failed edits roll back, and restores the exact baseline.

[`04_NegotiatedRepair`](04_NegotiatedRepair/README.md) demonstrates bounded
backward repair inside a caller-chosen descendant horizon. A housing-only
horizon cannot change its reused roads provider and fails atomically; a
roads-root horizon reopens roads, housing, and decor, excludes one exact roads
assignment, and succeeds without changing terrain, climate, or either reused
provider's random stream. Native FPC replay identical output
and versioned transcript hashes.

[`05_LearnedPatternWorld`](05_LearnedPatternWorld/README.md) learns wrapped
`2x2` terrain structure from two token grids at runtime, turns private pattern
anchors into an exact public terrain pass, then solves foliage and structure in
the same atomic DAG. Its native FPC self-checks pin model and
layer signatures, validate every overlap and projected contribution, prove no
private key escaped, and exercise failed structure-only regeneration with exact
entry and random-stream rollback. A checked
[`wfcpipeline`/`wfcrun`/`wfcresult` bundle](05_LearnedPatternWorld/pipeline/recipe.wfcpipeline)
uses bridge-v2 inverse lowering to turn eight portable public terrain locks into
private pattern constraints and reproduces all three canonical layers.

The older [text-rendered tiled world](../text/01_SimpleTiledWorld/SimpleTiledWorld.lpr)
remains a compact single-pass legacy-`Run` example.

Reusable terrain/biome/foliage types live in `wfc_world2d`, with semantic
checks in `wfc_world2d_validate`. The settlement specialization and its
independent checker live in `wfc_world2d_settlement` and
`wfc_world2d_settlement_validate`. The shared causal-trace console inspector
now exposes complete pass events and backward provider links; an interactive
2D domain/decision stepper remains roadmap work. See the
[2D documentation](../../docs/world2d.md),
[selective-negotiation contract](../../docs/selective-negotiation.md),
[causal-trace contract](../../docs/traces.md), [roadmap](../../ROADMAP.md), and
[examples index](../README.md).
