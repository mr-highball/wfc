# Learned Terraces 3D

The reusable `wfc_terraces3d` owner composes three ordinary WFC passes:

```text
learned volume tokens → authored socket/support variants → spatial foliage
terrain                structure                          foliage
```

This connects cardinal volume learning to authored domain constraints. The
learner does not infer physics, module sockets, or plant rules. Those remain
explicit, independently checked requirements on downstream passes.

## The experiment

Two independent 3 × 3 × 5 volumes describe occupied column heights:

```text
2 3 4    3 3 2
3 4 3    4 3 3
4 3 2    3 2 2
```

Each column begins with `ground`, continues with zero or more `rock` cells,
ends its occupied portion with `soil`, and has `air` above. The full six
direction planes are learned with open boundaries and horizontal D4 symmetry.
D4 preserves gravity; cube rotation would incorrectly teach these scalar
labels in inverted orientations.

Only the generated floor (`ground`) and ceiling (`air`) are pinned. The
intermediate output is solved, not copied from a sample or an authored
height blueprint. The default output is 6 × 5 × 5, larger than either sample.

| Learned token | Structural choices | Constraint retained |
| --- | --- | --- |
| ground | bedrock | Provides vertical support |
| rock | stone, basalt | Load sockets; requires and provides support |
| soil | grass, flowerbed | Load below, open above; requires support |
| air | air | Open vertical sockets; empty cell |

Foliage `none` is allowed anywhere. A `fern` needs structural air at its own
coordinate and `grass` at (X,Y,Z−1); `flowers` similarly need a
`flowerbed`. Plant support is explicitly cross-pass, not a false
same-foliage-layer support flag.

## Owner API and lifecycle

```pascal
Owner := TTerraces3D.Create(6, 5, 5, 55);
Scene := nil;
try
  Options := DefaultTerraces3DOptions;
  if not Owner.TryGenerate(Options, Scene, Report) then
    raise Exception.Create('No terrace composition committed');
  WriteLn(Scene.Signature);
finally
  Scene.Free;
  Owner.Free;
end;
```

The returned `TTerraces3DScene` is caller-owned and detached. Its terrain
accessor and borrowed immutable structure/foliage scenes contain public
tokens, prototypes, and rotations, not graph keys. Free the outer scene
only; never free its borrowed child scenes separately.

- `SetTerrainToken(X,Y,Z,Token)` sets a finite user cell domain. Empty clears
  the edit and restores the floor/ceiling policy. Invalid tokens or boundary
  edits fail before changing state.
- `SetFoliage(X,Y,Z,PrototypeId)` accepts `none`, `fern`, `flowers`, or an
  empty string to clear the domain.
- `TryGenerate` always requests the entire pipeline. A new session has no
  baseline, but it can always attempt full generation.
- `TryRegenerateFrom(t3sFoliage,...)` preserves both earlier passes.
  `t3sStructure` includes foliage, and `t3sTerrain` includes all descendants.
  A selective request needs a baseline and cannot bypass an earlier dirty edit.
- Search options independently bound local backtracking and excluded provider
  assignments. A target-only negotiation cannot reopen a preserved ancestor.
- `DirtyStage=-1` means no pending input edit. Failed attempts preserve the
  dirty marker and prior graph baseline. They return no new public scene.
  Previously returned detached scenes remain valid historical snapshots.
- `SetSeed` changes future deterministic search streams, not a captured
  scene. The browser treats seed/shape edits as a new session.
- `DoAcceptCandidate` is a protected virtual extension hook for additional
  policy. It sees a borrowed candidate inside the core entry/RNG transaction.
  Returning false or raising cannot publish the pending capture.

Independent validation rechecks all learned directional relations,
floor/ceiling policy, the exact token-to-prototype projection, kit sockets,
same-kit vertical support, and both foliage spatial relations. It executes
before core commit, while rollback snapshots still exist. The ordinary
voxel adapter's public `Capture` continues to reject a running graph; only
this owner reads the final candidate internally at the commit hook.

The public negotiation report retains numeric pass/value indices and projects
every trace display value to its model token or prototype, including rejected
rounds. Existing index-based trace/transcript hashes remain unchanged.

## Reusable model-to-voxel bridge

`wfc_voxel3d_passes` now adds these families without changing the original
kit-to-kit bridge records or version:

- `TVoxel3DModelPassProjectionRule(s)`: target prototype/yaw selector plus
  `AllowedSourceTokens: TWfcModelTokens`.
- `TVoxel3DModelPassSpatialTerm(s)`: signed XYZ offset plus allowed tokens.
- `TVoxel3DModelPassSpatialClause(s)`: target selector, world or target-yaw
  offset frame, and OR terms. Separate clauses remain AND.
- Matching `MakeVoxel3DModelPass...` helpers deep-copy caller arrays.
- `ValidateVoxel3DProjectionFromModelPass` /
  `RequireVoxel3DProjectionFromModelPass`, and corresponding
  `...SpatialClausesFromModelPass` calls take a target kit, bound target
  adapter, borrowed source model, stable source pass index, and rules/clauses.

A complete projection covers every target variant exactly once. Each source
set is nonempty, known, unique, and canonicalized in model vocabulary order.
One source token may allow several target variants; one target rule may
accept several source tokens. Spatial clauses are intentionally partial.

All configuration checks and expansions finish before the first graph write.
The source is resolved from the target's own pass root by index, never by a
mutable root selection. Distinct passes, acyclic dependencies, exact bound
target kit identity, compatible shapes, and source model definition are
checked. Open out-of-bounds offsets cannot match; wrapping uses the core's
existing coordinate semantics. Target-yaw offsets rotate XY once and keep Z.

`WfcModelDefinitionMatchesGraph(Model, PassGraph)` verifies ordered registry
values, lossless token conversion, weights, exact finite local rule rows,
deny-all directions, rank/shape compatibility, and boundary/wrap agreement.
Pass a stable child `Root.PassGraph[Index]`, not the root selection proxy.
Rank 1 requires height/depth 1; rank 2 requires depth 1; rank 3 permits any
positive output shape. Sample dimensions do not prescribe output dimensions.

This proof concerns the applied local model definition. It intentionally
ignores entries, caller domains, and additive cross-pass requirements, and
does not certify a training provenance claim. Configuration validation also
does not replace independent solved-output validation. Repeated requirement
calls are additive, not replacement; install canonical maps once.

No portable artifact format changed. Serializing recipes with a private
voxel consumer is a separate future adapter contract.

## Presentation, limits, and verification

`wfc_terraces3d_view` projects public integer mesh faces through the existing
fixed-subcell isometric/SVG libraries. Foliage is shown as inset,
half-height block glyphs. Native and browser presentations share the exact
Pascal SVG encoder, cell metadata, and four-yaw projection.

There is no fixed output-size cap in this owner. Width and height must be
positive and depth at least 3; dimensions, coordinates, cell products, seeds,
and search counters must be representable exact integers. Generation is a
full-volume in-memory solve: storage and search cost grow with the volume
and can exhaust the process's available memory. This is not chunked or
streamed generation. The separate renderer also has checked fixed-coordinate
and screen-span limits. Large generation does not imply it can be displayed
in a single SVG.

The portable suites cover bridge preflight and unchanged failure state,
source-model tampering, same-cell/spatial/yaw maps, wrapped seams,
negotiation, independent scene validation, detached output, selective
provider preservation, valid/invalid dirty edits, final-hook rejection and
exceptions against idle RNG controls, browser numeric injection, and
deterministic geometry metadata.

Default seed-zero witnesses are composition
`1:6D695B99:2D23CF62` and view `C3D25917`. These are regression witnesses,
not proofs of quality, general solvability, or search scalability.

See the [demo commands](../examples/3D/04_LearnedTerraces/README.md) and
[experiment record](research/model-to-voxel-pass-v1.md).
