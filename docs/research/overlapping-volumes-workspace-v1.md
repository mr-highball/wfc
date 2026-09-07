# Portable overlapping-volume workspaces v1

Record date: 2026-09-06 UTC. This is a project implementation and conformance
record, not a claim of algorithmic novelty, physical validity, or general
spatial quality. It extends the earlier
[overlapping-volume record](overlapping-volumes-v1.md), which established
literal cuboid learning and graph-owned public constraints. The
[API guide](../overlapping-3d.md) includes complete Pascal examples.

## Question and implemented path

Can the actual joint-volume model survive an editable training source,
portable recipe, public XYZ invocation, independent validation and replay,
while supporting a useful view that cannot silently change the result?

```text
authored XYZ samples + source boundary/symmetry + public policy
                         |
                   wfclearn=6
                         |
              actual cuboid corpus learner
                         |
         wfcp=2 resource inside wfcpipeline=4
                         |
public XYZ locks/domains -> inverse masks -> latent patterns
                         |                       |
                  quotas/connectivity       public output -> downstream passes
                                                 |
                          validated run/result artifacts and explicit replay
                                                 |
                          token-volume view: hide / cut / yaw / SVG
```

The maintained implementation uses project-owned MIT Pascal units and the
FPC/pas2js runtime. The view reuses the existing isometric projector and SVG
writer; it does not introduce a geometry or generation dependency. Source,
recipe, run and result remain separate documents, not a new opaque workspace
bundle.

## Model and artifact identity

[`wfc_training`](../../src/wfc_training.pas) appends `wtkPattern3D` and calls
the real overlapping-volume corpus learner. Its seven-argument factory is
`MakeWfcTrainingOptions(Kind,Boundary,Symmetry,PW,PH,PD,Order)`, with order
zero for this kind. `MakeWfcTrainingSample(Name,W,H,D,Tokens)` records each
independent source in X-fast, then Y, then Z order. The older six-argument
options factory is unchanged and defaults the appended depth to one.

The learned resource preserves exact payload tokens, all six overlap
directions, raw occurrence weights, source shapes, boundary and symmetry.
Training does not substitute six directional counts for a cuboid payload.
The default recipe contains private `patterns` and public `output`, with
`wprkPattern3D`, `wpakPattern3D` and `wpbkPattern3DProjection` supplying the
new resource, adapter and bridge. A borrowed model returned by
`BorrowPattern3DResource` remains owned by the immutable recipe.

Feature selection is deliberately narrow:

| Envelope | Selection and change | Preserved behavior |
| --- | --- | --- |
| `wfcp=2` | Existing rank-3 cuboid artifact | 2D `wfcp=1` is not reinterpreted |
| `wfclearn=6` | Only `kind=pattern3d`; explicit source and footprint XYZ, order zero, both optional policy registries | Existing kinds retain version 1–5 selection, canonical bytes and fingerprints |
| `wfcpipeline=4` | A 3D resource, adapter or projection bridge is present; two additional capability version fields | Recipes without that feature retain version 1–3 selection, bytes and signatures |
| `wfcpipeline-run=1`, `wfcpipeline-result=1` | Existing envelopes carry the new recipe and its public XYZ constraints/results | No global invocation or replay-envelope bump |

Recipe v4 inserts `pattern3d-graph-adapter-version=1` and
`pattern3d-bridge-version=1` immediately after `sequence-bridge-version`.
Even an unused typed 3D resource selects v4. Empty quota/connectivity
registries encode version zero and count zero; populated registries encode
version one and a positive count. Wrong feature/version combinations reject,
rather than accepting v4 as a blanket upgrade for an old recipe.

The appended `PatternDepth`, `Pattern3DGraphAdapterVersion` and
`Pattern3DBridgeVersion` fields are normalized for legacy callers **without
reading their uninitialized appended storage**. A feature-domain signature
component is added only for 3D recipes. Existing compiler, graph, solver and
runtime algorithm versions are not globally incremented. Tests retain the
old literal artifacts and checksums, including native poisoned storage and
JavaScript getters that throw if an old path reads a new field.

Exact private pattern keys still bind footprint XYZ and actual token
payloads, not just local palette indices or a hash. Model validation decodes
the actual `wfcp=2` resource, accounts for its six dense relation planes, and
keeps full equality checks after hash routing. Checksums identify canonical
contexts; they are not cryptographic authentication.

The codecs are
[`wfc_training_text`](../../src/wfc_training_text.pas) and
[`wfc_pipeline_text`](../../src/wfc_pipeline_text.pas), with immutable recipe
validation in [`wfc_pipeline_model`](../../src/wfc_pipeline_model.pas).

## Public constraints and topology

The declarative bridge uses one shared **wrapped rank-3 XYZ extent**. Source
extraction may be open or wrapped independently. In particular, an open
training source intentionally produces a wrapped output recipe while the
embedded model retains open source provenance. This neither manufactures
missing seam patterns nor guarantees the requested torus is solvable.
Low-level open projection retains its larger halo, but that is not an
unlike-sized declarative pass mapping.

For a constrained public voxel `q`, footprint offset `o` and domain `D(q)`,
the covering private anchor is `a = (q-o) mod extent`. Retain a candidate
pattern `p` only when `payload(p,o)` belongs to `D(q)`. The runtime intersects
every such condition. Multiple offsets reaching one anchor on a small torus
are not interchangeable: they inspect different payload entries and must
all hold. Public exact-copy aliases and separate projection bridges also
intersect their contributions; they cannot overwrite a previous mask.

A source palette token need not occur at every offset for the model to be
valid. It can simply be unavailable in wrapped public output. Rejecting the
whole model would incorrectly discard otherwise feasible pattern subsets.
An explicitly empty public domain, by contrast, is a hard contradiction.

Public quotas count each public voxel once. Their latent candidate
membership is determined by the footprint token at `(0,0,0)`, not by counting
every covering observation. Rooted connectivity similarly binds each latent
candidate to that public token's profile and preserves the six world-axis
ports, including Up/Down. Source symmetry does not rotate authored policy.
Completed public output is independently recounted and traversed, so latent
lowering is not the sole acceptance check. Aliases retain their own policy
declarations while lowering through their materialized owner.

The implementation lives in
[`wfc_pipeline_compile`](../../src/wfc_pipeline_compile.pas),
[`wfc_pipeline_runtime`](../../src/wfc_pipeline_runtime.pas) and
[`wfc_pipeline_connectivity`](../../src/wfc_pipeline_connectivity.pas).
Independent private-pattern and public-projection validation precedes commit;
a failed solve does not publish partially filled public layers. Ordinary
downstream requirements still use public tokens, including nonzero Z
offsets. Explicit replay rebuilds the named recipe/run and compares the
canonical result, including valid non-solved terminal statuses. Merely
decoding a valid recipe does not execute or prove it solvable.

## Workspace lifecycle and user entry points

[`TWfcTrainingWorkspace`](../../src/wfc_training_workspace.pas) distinguishes
editable source, trained recipe, configured run and solved result.
`ConfigureVolumeRun` requires explicit depth; the rank-2 convenience method
does not silently flatten a volume. Public locks use
`MakeWfcPipelineCellLock(Workspace.PublicPassIndex,X,Y,Z,Token)` rather than
private keys. Detached `OutputTokens` retain the complete XYZ result.

Source edits invalidate derived artifacts, run edits invalidate the previous
result, and rejected policy edits cannot leave stale derived output
exportable. Quota replacement preserves connectivity and vice versa. An
editable source retained after a learning failure is not a solved result.
Standalone model export rejects authored quota/connectivity policy instead
of silently losing it; source and recipe exports retain that policy.

In [Training Studio](../../examples/learning/05_TrainingStudio/README.md),
open `index.html?preset=7` through the included FPC server. Preset 7,
**Arched lattice / overlapping volumes**, learns 35 joint `2×2×2` patterns
from 64 authored source cells using gravity-preserving D4. Its default
`4×4×4` output retains public locks `stone(0,0,0)`, `leaf(2,2,1)` and
`air(1,1,2)`. The checked default signatures are source `C87487A5`, recipe
`E63D8AD3`, and result `32E7DE7F`. Preset 5 remains a separate radius-one
volume adjacency example, not a renamed overlapping model.

The maintained native
[`TrainingStudioVolume`](../../examples/learning/05_TrainingStudio/TrainingStudioVolume.lpr)
uses the same preset and locks and emits SVG without launching a viewer.
Run `TrainingStudioVolume --help` for its command interface. The default
512-cell budget is an explicit entry-point policy, not a solver duration or
unbounded-capacity claim. Optional new-file publication uses the existing
atomic no-replace writer; it never replaces an existing file. Its Unix path
requires hard links and a trusted parent directory. That filesystem path
was not exercised by the local Windows checks below.

## A view is not a different result

[`wfc_token_volume_view`](../../src/wfc_token_volume_view.pas) accepts public
XYZ tokens and a palette independently of the workspace. It builds exposed
unit-cube faces and delegates projection, stable painter order and hit
testing to [`wfc_voxel3d_isometric`](../../src/wfc_voxel3d_isometric.pas),
then SVG export to [`wfc_voxel3d_svg`](../../src/wfc_voxel3d_svg.pas).

Hiding one palette token, cutting to Z layers `0..N-1`, and choosing one of
four yaws affect only presentation. Neighboring visible cubes cull shared
faces even when their tokens differ. The output boundary remains visible:
the view is a finite cutout, not a periodic mesh. Safe `token-N` metadata
preserves exact cell XYZ without injecting raw Unicode tokens or private
pattern keys into renderer identifiers.

Studio face selection maps the projected quad back to the ordinary public
XYZ editor. Selecting a cell is not a lock mutation; applying a lock is an
explicit subsequent run edit. SVG view signatures may change after view
edits, but the full canonical result and its signature must not. Stone,
leaf and air names/colors are display conventions, not materials with
physical, collision, support or structural-engineering guarantees.

## Checks executed for this integration

The following new portable suites were compiled with checked FPC 3.2.2 and
3.3.1, each for Win32 and Win64, and also executed as pas2js output in actual
Edge using the included FPC server/capture/check workflow:

| Suite | Checks per native run | Checks in browser |
| --- | ---: | ---: |
| [Pipeline model](../../test/wfc_pipeline_pattern3d_model_test.lpr) | 45 | 124 |
| [Pipeline codec](../../test/wfc_pipeline_pattern3d_text_test.lpr) | 78 | 78 |
| [Training and policy](../../test/wfc_training_pattern3d_test.lpr) | 571 | 681 |
| [Runtime and replay](../../test/wfc_pipeline_pattern3d_runtime_test.lpr) | 590 | 590 |
| [Public-token view](../../test/wfc_token_volume_view_test.lpr) | 2,259 | 2,369 |

Additional browser assertions construct malformed JavaScript values that
ordinary statically typed Pascal cannot supply. Counts are assertions, not
independent research experiments or a proof over all possible corpora.

Training checks compare canonical learned-model bytes directly against the
core learner across both source boundaries, all four symmetries and
independent cuboid samples. All four quota/connectivity presence combinations
round-trip through source v6, recipe v4, invocation/result and exact replay.
The independent source checksum fixture is `9B8F3815`; the full-volume recipe
model fixture is `F85EEF40`, and the literal minimal v4 recipe is `B65F7944`.

Runtime checks include far-Z locks, XYZ domains, two chained public aliases,
eight seeds and both scheduling strategies; all public voxels are compared
against an independent expected volume. Contradictory aliases, empty
domains, impossible quotas and vertical connectivity reject without partial
public output. Separate bridges and small-torus offset aliases must AND
their private masks. Tampered private/public output fails independent
validation, including a corrupted voxel outside the first Z slice.

View checks independently enumerate world-space faces and all four yaws,
unlike-token culling, cuts, hiding, exact budgets, empty visible scenes,
detached data and Unicode-safe metadata. Native and browser SVG goldens
agree. A zero face budget rejects rather than truncating a nonempty scene.

The separate [workspace suite](../../test/wfc_training_pattern3d_workspace_test.lpr)
passed 383 native and 383 actual-browser checks. Studio's actual-browser
self-test also passed its existing preset/policy/circular loops and the
new `data-overlapping-volume`, `data-overlapping-volume-view` and
`data-overlapping-volume-recovery` markers, alongside `data-self-test`.
The native SVG entry point compiled and ran with checked stable FPC on both
Win32 and Win64; 99 private process assertions covered strict arguments,
deterministic cross-target SVG, alternate extents, view-only edits, new-file
publication and failures without partial stdout or replaced output.

Existing pipeline model/text fixtures (62/82 native checks), runtime/compile
(62/48), public quota integration (260), and rooted connectivity (420) remain
green; the latter four suites also ran in actual Edge. Training's existing
open, volume, circular, quota and connectivity suites were rerun across the
four native variants and browser. These are local Windows/Edge results;
this record does not claim hosted Linux or macOS validation of the
integration. Earlier core-only evidence remains in the preceding
research record and is not counted as a rerun of this integration.

Local ignored evidence, when reproducing in the authoring checkout:

- `build/hosted-ci/pipeline-pattern3d/model/VALIDATION.md` and per-target logs;
- `build/hosted-ci/training-pattern3d/VERIFICATION.md`;
- `build/hosted-ci/pipeline-pattern3d-runtime/VALIDATION.md`;
- `build/hosted-ci/training-pattern3d/svg/VERIFICATION.md`;
- `build/private-volume-integration/root/browser/selftest.dom`.

These private logs are not distributed fixtures. Maintained portable tests
and their literal assertions are the reproducible repository evidence.

## Costs, explicit bounds, and remaining work

The training profile is deliberately smaller than the low-level learner:
65,536 total source tokens, 64 footprint cells, 16,777,216 learning visits,
8 MiB source text, and 4 MiB aggregate encoded source tokens. Interactive
Studio defaults to 512 source tokens, 128 learned items and 512 output cells;
the general workspace exposes its larger documented limits. Raising an
interactive budget does not remove format, model, runtime or memory bounds.

The immutable model still permits at most 1,024 patterns and a dense
`6*P*P` relation matrix. Its 16 MiB text bound is not a bound on expanded
memory. Exact per-pattern token dictionaries may amplify compact input;
individual and aggregate exact-key length must fit `Integer`, and available
RAM can be lower. The prior record describes equality-checked slab indexing
and collision behavior; this slice does not replace the representation with
sparse relations.

Run cells are bounded by 4,194,304 and aggregate pass cells by 16,777,216.
Runtime inverse lowering separately preflights 1,048,576 contributions,
16,777,216 candidate visits and 4,194,304 private indices cumulatively across
applicable bridges. Quota and connectivity lowering have their own work
limits. An adversarial 16-cubed footprint with 257 public constraints rejects
at the contribution bound before graph allocation. Search budgets count
operations, not elapsed time; no throughput or wall-time guarantee was
measured here.

The view defaults to 131,072 exposed quads and accepts an explicit caller
budget. It validates hidden and cut-away cells too, counts faces before
large quad allocation, and rejects rather than truncates. Fixed-subcell
products must fit `Integer`; the inherited projector also enforces its
32,767-pixel content-span and checked fitting limits. This is a view budget,
not a changed solver capacity.

Generation and training remain finite, in-memory and synchronous. Open-halo
declarative layouts, arbitrary cross-pass resampling, streaming spatial
chunks, cancellation, richer geometry/material semantics and larger-scale
authoring remain future work. Passing the authored lattice or any finite
fixture does not establish aesthetically good results for every corpus,
global physical support, or unlimited world size.

## Reproduction

Use the repository's native build and browser test workflow documented in
[development tools](../development-tools.md). For an isolated native run,
create fresh unit/output directories and compile a suite, for example:

```text
fpc -B -Mdelphi -Sa -Cr -Co -Ci -Fusrc -Futools -FU<new-units> -FE<new-bin> test/wfc_pipeline_pattern3d_runtime_test.lpr
```

Use separate directories for compiler versions, targets and pas2js, then
execute each test and require zero failures. The workspace/Studio suites
also need `-Fuexamples/learning/05_TrainingStudio`. A browser check must run
the generated program in an actual browser through the included FPC tools;
JavaScript compilation alone is not execution evidence. The native volume
companion is discoverable with `TrainingStudioVolume --help`, and the
interactive entry is `index.html?preset=7`. No external generation,
training, geometry or hosting library is added by this slice.

The complete `TrainingVolume` example in the API guide was extracted from
the document, compiled with checked stable FPC for Win32, and executed. It
reported result `78A650B2` and 52 visible faces after a presentation-only
depth cut and yaw change; the far-Z public lock remained in the full result.
