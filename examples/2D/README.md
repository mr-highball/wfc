# 2D examples

[`01_MultiPassWorld`](01_MultiPassWorld/README.md) is the dedicated portable 2D
ecosystem demo. It generates terrain, then biome, then foliage with the atomic
reference solver, validates the result independently, renders every layer, and
prints stable per-layer signatures. The same source builds with native FPC and
pas2js/Node without external dependencies.

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

The older [text-rendered tiled world](../text/01_SimpleTiledWorld/SimpleTiledWorld.lpr)
remains a compact single-pass legacy-`Run` example.

Reusable terrain/biome/foliage types live in `wfc_world2d`, with semantic
checks in `wfc_world2d_validate`. The settlement specialization and its
independent checker live in `wfc_world2d_settlement` and
`wfc_world2d_settlement_validate`. Full domain/trace inspectors remain roadmap work. See the
[2D documentation](../../docs/world2d.md), [roadmap](../../ROADMAP.md), and
[examples index](../README.md).
