# Portable mapped pipelines

Mapped pipelines separate a recipe's spatial meaning from the requested size
of one generation. Terrain, foliage and housing may use different grids while
their public constraints refer to the same integer world. The core geometry
is described in [mapped passes](mapped-passes.md).

The additive implementation is under integration on `expansion`. Focused
native/browser tests exist; complete integration, the editable composition
workspace and Mapped World import/export UI are separate remaining work. This
document describes the API and format contract, not a release-completion claim.

## Ownership

| Object | Owns | Does not own |
| --- | --- | --- |
| Recipe | Resources and provenance; per-pass rank, origin, pitch and wrap; vocabulary, adapters, dependencies and mapped policies | Requested cell dimensions or generated output |
| Run | Complete ordered per-pass cell extents, seed, strategy, budgets, public locks and domains; detached resolved layouts | A borrowed recipe pointer or generated output |
| Result | All-pass layout table, terminal diagnostics and evidence identity; complete public layers when solved | Private latent values or proof that an arbitrary claimed result was replayed |

Changing the output extent does not require retraining a resource or editing
the immutable recipe. A learned sample's size and a learned pattern's footprint
are not output pitch. Every resource retains its existing source description,
license identifier and fingerprint.

Ranks constrain local axes: rank one has Y=Z=1 cells, and rank two has Z=1.
Origin and pitch remain meaningful on all three world axes, including the
thickness of a two-dimensional slab. There is no implicit padding, rescaling,
clipping, nearest-neighbor conversion or divisibility requirement between grids.

The new `wfc_pipeline_layout` unit depends only on `SysUtils` and `wfc_lattice`.
Its immutable `TWfcPipelineLayoutTable` owns layouts, per-pass counts and prefix
offsets in O(number of passes) storage. `wfc_pipeline_mapping` adds recipe-aware
relationship checks and the independent public mapped-policy evaluator. Neither
preflight constructs a dense graph.

## Explicit opt-in and compatibility

Old model constructors retain the existing recipe 1–4 selection. An explicit
spatial constructor appends these two arguments to the complete constructor
that already accepts versions, quotas and connectivity:

```pascal
const APassMappingVersion: Integer;
const APassTopologies: TWfcPipelinePassTopologies
```

Supply `WFC_PIPELINE_PASS_MAPPING_VERSION` (1) and exactly one topology per pass.
The existing `ARank` and `AWrapNeighbors` arguments must equal topology zero's
rank and wrap; conflicting metadata is rejected. On a spatial recipe those
global properties are pass-zero views, not defaults applied to other passes.
Use `PassTopologyAt` or `CopyPassTopologies` for the complete geometry. Legacy
recipes expose synthesized zero-origin, unit-pitch topologies through those
same accessors. Copies are detached, including nested vectors.

Version selection is deliberate, not an attempt to minimize document size:

| Construction | Recipe | Run | Result |
| --- | --- | --- | --- |
| Old recipe and old uniform numeric run constructor | Existing 1–4 | 1 | 1 |
| Explicit spatial recipe, even with uniform/default geometry | 5 | 2 | 2 |
| Old recipe and explicit per-pass extent run constructor | Existing 1–4 | 2 | 2 |

Explicit extents may bind an old recipe when all its actual read/copy/bridge
relationships permit them. A numeric run constructor supplies the same extent
to every pass; if that conflicts with a pass's rank, construction fails. It
does not invent a compatible shape or silently reduce dimensions.

Old valid document bytes, recipe/run/result signatures, deterministic outputs
and evidence identities are compatibility gates. New capabilities are signed
only in their new representations; unrelated algorithm versions are not
globally bumped. The recipe pins both the portable mapping declaration version
and core `WFC_PASS_MAPPING_VERSION`. Runs/results pin the resolved layout and
core mapping versions. These signatures detect accidental differences, not
authenticity or malicious replacement of an entire bundle.

## Declaring mapped policies

`TWfcPipelineRequirementKind` adds the tagged `wprqMapped` kind. Its
`MappedQuery: TWfcPipelineMappedQuery` contains:

- `Kind`: point, complete consumer-cell coverage, or explicit region coverage;
- `Match`: all selected provider cells, or an inclusive matching-cell count;
- `MinimumOffset` and `MaximumOffset`: signed world-space offsets;
- `AllowedProviderTokens`: a nonempty set in provider-vocabulary order;
- `MinimumMatches` and `MaximumMatches`: count bounds.

Use `MakeWfcPipelineMappedRequirement(ConsumerPass, ConsumerToken,
ProviderPass, Query)`. Both endpoints must be public, and the recipe must
declare their dependency explicitly. Mapped declarations are ANDed with other
requirements on the consumer value. They do not define an inverse projection
or authorize changes to the provider.

Point and complete-cell queries require zero unused maximum offsets. All-match
queries require zero count fields. Regions have positive half-open XYZ span;
counts satisfy `0 <= minimum <= maximum <= High(Integer)`, with point maximum
at most one. A minimum larger than the actual coverage is unsatisfiable, not
malformed. Wrapped coverage counts unique provider cells, even if a region
crosses many periods. Bounded partial coverage fails even for a zero minimum.

Old exact/any/count tags never read the appended mapped payload. Old term-based
factories do not accept the mapped tag. This matters for existing Pascal code
that initializes only the original record fields.

`WfcPipelineMappedQueryGeometry` uses a placeholder graph value solely to reuse
the pure core numeric/geometry validation. It is not a public-token conversion.
The compiler separately checks native graph-string round trips and collisions
before installing actual provider tokens.

## Invoking and inspecting

The extent overload keeps all the usual run controls:

```pascal
Run := TWfcPipelineRun.Create(Recipe, Extents, Seed, Strategy,
  MaxBacktracks, MaxPassBacktracks, CaptureTrace, Locks, Domains);
Output := ExecuteWfcPipeline(Recipe, Run);
```

The caller owns both objects. `CompileWfcPipeline(Recipe, Extents)` also exposes
the layout-aware compiled owner directly; its recipe must outlive it. A run
owns detached resolved geometry and does not require its original recipe to
remain alive merely to inspect or serialize the run.

Use run/result `PassLayoutAt`, `PassTopologyAt`, `PassCellCount`,
`PassOffsetAt`, `CopyPassExtents` and `CopyPassLayouts`. `TotalCellCount` sums
every pass, including exact-copy aliases. Result `LayerLayoutAt` follows that
layer's declared pass. The old Width/Height/Depth properties refer to pass zero.

For counts 2, 5 and 3, prefixes are 0, 2 and 7. Pass 1 cell 4 and pass 2 cell 2
therefore have keys 6 and 9. Multiplying the pass index by the root's two cells
would incorrectly give both cells key 6. Resolving an alias to its materialized
input owner is a separate semantic operation; it does not erase that alias's
actual storage slot or budget cost.

Before allocating a graph, the shared resolver checks every endpoint and actual
layout relationship. Legacy index-space requirements, copy passes and current
pattern/sequence bridges still require all ten layout fields to match. An
order-only dependency imposes no such restriction. A same-layout latent/public
bridge pair may sit behind an unrelated smaller or differently ranked root;
its inverse locks/domains, quotas and connectivity use the pair's own layout.

Compilation creates neutral passes, configures their layouts, installs declared
edges and definitions, then applies their final modes. This avoids treating an
as-yet definitionless legacy pass as an unintended copy during initialization.
The commit hook independently checks recipe policies and rejects mutations of
the borrowed graph that violate the compiled contract.

## Canonical wire additions

All formats retain strict ASCII canonical text, percent-encoded public tokens,
ordered records, checked counts and a terminal `end`. Use the encoders rather
than hand-authoring signatures. The existing artifact grammar is documented in
[pipeline artifacts](pipeline-artifacts.md).

Recipe 5 records `pattern3d-present=true` or `pattern3d-present=false` after
the legacy algorithm-version rows. It retains all
supported combinations of Pattern3D, quotas and connectivity. Its spatial
section contains `pass-mapping-version=1`, `graph-pass-mapping-version=1`,
`pass-topologies=N`, then:

```text
pass-topology=index,rank,originX,originY,originZ,pitchX,pitchY,pitchZ,wrap
```

Boolean fields, including each layout's `wrap`, use canonical `true`/`false`,
not numeric flags. Pattern3D algorithm rows are present only when its presence
flag is true; quota and connectivity sections are always explicit in recipe5,
with zero version/count when absent.

A `mapped-v1` requirement has zero legacy terms, followed by:

```text
mapped=requirementIndex,point|cell|region,all|count,minX,minY,minZ,maxX,maxY,maxZ,minMatches,maxMatches,tokenCount
mapped-token=requirementIndex,tokenIndex,encodedPublicToken
```

Run 2 retains width/height/depth as a checked pass-zero view, followed by
`layout-version=1`, `mapping-version=1`, `extents=N` and exactly N rows:

```text
extent=passIndex,cellsX,cellsY,cellsZ
```

Result 2 similarly adds `layout-version=1`, `mapping-version=1`, `layouts=N`
and all N full layouts, including private passes and failed-result owners:

```text
layout=passIndex,rank,originX,originY,originZ,pitchX,pitchY,pitchZ,cellsX,cellsY,cellsZ,wrap
```

Each table is complete, not a set of sparse overrides. Result layouts must
match the bound recipe and run before public token arrays are allocated. Layer
lengths use their referenced pass's count, not a global product. A failed result
has no partial layers; both its entry and neighbor indices belong to its
`PassIndex`, not to `DependencyPassIndex`.

## Validation is not replay

Solved result construction rechecks public mapped policies from public tokens,
as it does quotas and connectivity. It does not assert that inaccessible latent
values were solved correctly. The inspector prints per-pass topology, extents,
mapped query records and each public cell's local coordinates and half-open
world box. Its record/byte limit bounds the report, not the generated score or
world. It retains `evidence=claimed-not-replayed` and `full-solution=not-proven`.

Use the existing `wfc_run`, `wfc_validate` and `wfc_inspect` applications for the
shared canonical artifact/replay route. `TWfcArtifactDocument.RequireReplay`
performs a fresh complete execution and compares the full canonical result,
not only a saved hash. Inspecting or decoding alone does not perform that replay.
Legacy result1's low-level external-graph capture retains its documented
dimension/seed/label/value checks; result2 capture additionally requires exact
layouts on every private and public pass. Neither arbitrary external-graph
capture proves the supplied graph came from the declared recipe.

## Boundaries and verification

Finite portable artifacts retain explicit input, token and execution resource
envelopes. Aggregate work is charged from actual pass/public counts rather than
maximum-grid padding. Those boundaries are not a desired composition length.
This implementation does not yet stream an unbounded spatial world, provide
general inverse mapped constraints, infer a minimal provider conflict set, or
turn the single-source Training Studio into a multi-resource composer.

The maintained tests include layout ownership/prefix indices, an independent
literal-box and periodic-image oracle, model/text/run/result contracts,
unlike-layout runtime and bridge cases, and the actual artifact inspect/replay
route. [The artifact fixture](../test/wfc_pipeline_mapped_artifact_test.lpr)
executes terrain/foliage/housing twice with one recipe: 69 and 138 actual pass
cells. It compares complete serialized artifacts and checks the farthest
house's own coordinates. [The geometry fixture](../test/wfc_pipeline_mapping_test.lpr)
also distinguishes a clear corner from a blocked interior and verifies huge
wrapped world spans without allocating a footprint-sized list.

The native-only `wfc_pipeline_mapped_process_test` invokes the three real FPC
executables with file and standard-input artifacts, checks solved and nonsolved
replay, rejects malformed geometry and wrong bindings, and preserves its saved
fixtures. These tools emit stdout; no CLI destination-file transaction or
atomic publication feature is implied by the harness's own collision checks.

Complete native/browser integration, CLI process conformance, clean package
consumption and final cross-target byte parity are required before this
extension is called release-ready. The existing fixed-grid Mapped World UI
does not yet expose these portable import/export or general layout-editing APIs.
