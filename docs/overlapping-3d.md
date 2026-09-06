# Overlapping 3D volumes

A latent value now represents a genuine `W × H × D` token footprint, not a
stack of independent 2D slices. Neighboring footprints must agree throughout
their shared volume. A public projection exposes ordinary semantic tokens to
later constraint passes.

The five project-owned MIT Pascal units work with native FPC and pas2js:

| Unit | Responsibility |
| --- | --- |
| [wfc_volume_symmetry](../src/wfc_volume_symmetry.pas) | Literal signed-axis transforms of XYZ volumes |
| [wfc_pattern3d](../src/wfc_pattern3d.pas) | Immutable model, six-direction overlap compilation, capture, projection, independent validation |
| [wfc_pattern3d_learn](../src/wfc_pattern3d_learn.pas) | Separate-sample footprint extraction and raw frequencies |
| [wfc_pattern3d_text](../src/wfc_pattern3d_text.pas) | Strict canonical `wfcp=2` volume artifacts |
| [wfc_pattern3d_graph](../src/wfc_pattern3d_graph.pas) | Wrapped public-pass bridge, inverse public domains, transactional owner, composition signatures |

They use repository code and the FPC/pas2js runtime, with no external
generation, geometry, or training library. This is a core volume-footprint
workflow, not a claim that every 3D ecosystem integration is complete.

## Learn, constrain, and consume public tokens

This small native program demonstrates both maintained entry points. The
first uses the owner’s public inverse lock. The second builds an extensible
three-pass graph with the free bridge and a semantic foliage consumer.
The eight input tokens are X-fast, then Y, then Z.

```pascal
{ SPDX-License-Identifier: MIT }
program OverlappingVolume;
{$mode delphi}{$H+}
uses SysUtils, wfc, wfc_model, wfc_pattern3d,
  wfc_pattern3d_learn, wfc_pattern3d_graph;

const Source: array[0..7] of String =
  ('land','water','water','land','water','land','land','water');
var
  Sample: TWfcModelTokens;
  Model: TWfcOverlappingModel3D;
  Owner: TWfcPattern3DPassPipeline;
  Composition: TWfcPattern3DComposition;
  OwnerReport: TWfcPattern3DPassReport;
  Graph: TGraph;
  Search: TGraphNegotiationOptions;
  SearchReport: TGraphNegotiationReport;
  Patterns: TWfcPatternGrid3D;
  Terrain: TWfcTokenGrid3D;
  Validation: TWfcOverlapping3DValidationReport;
  I: Integer;
begin
  SetLength(Sample,8);
  for I := 0 to 7 do Sample[I] := TWfcModelToken(Source[I]);
  Model := LearnOverlappingModel3D(Sample,2,2,2,2,2,2,wmbWrap,wmsNone);
  try
    { The owner lowers a public lock into every covering latent anchor. }
    Owner := TWfcPattern3DPassPipeline.Create(
      DefaultWfcPattern3DPassConfig(Model,4,2,4,0));
    Composition := nil;
    try
      Owner.LockPublicCell(3,1,3,'land');
      if not Owner.TryGenerate(Composition,OwnerReport) then
        raise Exception.Create('locked volume did not solve');
      Terrain := Composition.CopyProjection;
      if Terrain.Tokens[31] <> 'land' then
        raise Exception.Create('public lock was not respected');
      WriteLn('locked signature: ',
        WfcPattern3DCompositionSignatureHex(Composition.Signature));
    finally Composition.Free; Owner.Free; end;

    { A separate extensible DAG: patterns -> terrain -> foliage. }
    Graph := TGraph.Create;
    try
      Graph.Reshape(4,2,4);
      Graph.WrapNeighbors := True;
      Graph.Seed := 0;
      Graph.CurrentPass := 'patterns';
      Graph.PassMode := gpmOverlay;
      Graph.ClearDependencies;
      ApplyOverlappingModel3DToGraph(Model,Graph);

      Graph.SwitchToPass('terrain');
      Graph.PassMode := gpmOverlay;
      Graph.ClearDependencies;
      ApplyOverlappingProjectionFromPass3D(Model,Graph,'patterns');
      { A caller-assigned value on this fresh entry is an explicit lock. }
      Graph.Entry[3,1,3].Value := 'land';

      Graph.SwitchToPass('foliage');
      Graph.PassMode := gpmOverlay;
      Graph.ClearDependencies;
      Graph.AddValue('tree');
      Graph.AddValue('reed');
      Graph.Rules['tree'].RequireFromPass('terrain','land');
      Graph.Rules['reed'].RequireFromPass('terrain','water');

      { This free bridge does not own inverse masks. Negotiation may reopen
        an earlier completed pattern assignment when the public lock rejects it. }
      Search := DefaultGraphNegotiationOptions;
      if not Graph.TrySolveNegotiated(Search,SearchReport) then
        raise Exception.Create('volume DAG did not solve within its budgets');
      if not CaptureSolvedOverlappingProjectionPass3D(Model,
        Graph.PassGraph[0],Graph.PassGraph[1],Patterns,Terrain,Validation) then
        raise Exception.Create(DescribeOverlapping3DIssue(Validation.Issue));
      if (Terrain.Tokens[31] <> 'land') or
        (Graph.PassGraph[2].Entry[3,1,3].Value <> 'tree') then
        raise Exception.Create('semantic consumer or public lock disagrees');
      WriteLn('public (3,1,3): land -> tree');
    finally Graph.Free; end;
  finally Model.Free; end;
end.
```

The two examples intentionally use different constraint mechanisms:

- `TWfcPattern3DPassPipeline` owns its two-pass graph and pushes public domains
  back into the latent domains before solving. Its graph is not exposed for
  arbitrary additional passes.
- The free `ApplyOverlappingProjectionFromPass3D` bridge lets you build a larger
graph. A caller lock on its public pass is a normal graph constraint; it does
  **not** automatically invoke the owner’s inverse-domain implementation.
  The example uses [pass negotiation](pass-negotiation.md) so a rejected public
  lock can reopen an earlier completed pattern assignment. Ordinary
  `TrySolve` remains forward staged solving, not unrestricted global search.

The model is caller-owned and must outlive its owner/graph usage. Free the
returned composition yourself. Copy methods detach nested arrays;
`Model.CompiledModel` and the owner’s `Model` property are borrowed references.
The public bridge also requires lossless conversion between native graph
strings and Unicode model tokens. Tokens that cannot round-trip through the
host graph-string representation reject during preflight, rather than being
silently substituted. Valid Unicode in a learned model or artifact alone does
not guarantee that every native graph-string environment can represent it.

## Literal extraction and source symmetry

`LearnOverlappingModel3D` takes tokens, source width/height/depth, footprint
width/height/depth, source boundary, and symmetry. The corpus overload takes
`TWfcLearnVolumeSamples` from `wfc_learn3d`, followed by the same footprint
and policy arguments. Source samples may have different shapes.

`WFC_OVERLAPPING_3D_ALGORITHM_VERSION = 1` fixes this order:

1. Establish the palette from first appearances in the **original** sample
   order, scanning X fastest, then Y, then Z, before augmentation.
2. Visit samples in supplied order, then literal transforms in their defined
   order, then origins X fastest, Y, Z inside each transformed source.
3. The first occurrence of an exact payload assigns its pattern index.
   Every occurrence increments its raw positive weight, including symmetric
   duplicates; weights are not normalized probabilities.

For an open source, each axis must contain the footprint, and one source
contributes `(SW-W+1)*(SH-H+1)*(SD-D+1)` observations per transform. For a
wrapped source, every source cell is an origin: `SW*SH*SD` observations per
transform. A wrapped footprint may exceed its source dimensions. Modular
lookup repeats that source’s cells; it never reads another sample.

| Symmetry | Literal transforms | Footprint requirement |
| --- | ---: | --- |
| `wmsNone` | 1 | Any supported cuboid |
| `wmsD4` | 8 | Square XY footprint; depth independent |
| `wmsCubeRotations` | 24 | Cubic footprint |
| `wmsCubeFull` | 48 | Cubic footprint |

Sources can remain rectangular cuboids for all four policies. Transforming a
source permutes its shape as well as its contents. D4 preserves positive Z;
cube rotations retain determinant +1, while cube-full additionally includes
the reflected, determinant −1 transforms. A uniquely labeled eight-corner
cube therefore has 24 proper orientations and 24 additional reflected ones.
A symmetric payload can have fewer distinct patterns; its repeated literal
observations still contribute their full raw weight.

The version-1 transform order is explicit: axis permutations
`012,021,102,120,201,210`, with sign masks `0..7` within each permutation.
Mask bits 0/1/2 reverse destination X/Y/Z. Cube24 filters to determinant +1;
cube48 retains all transforms. D4 keeps permutations `012,102` and masks
`0..3`; none keeps identity alone. Identity comes first. This new volume
ordering is not the old 2D learner’s quarter-turn enumeration.

Construction independently checks complete symmetry-orbit closure and equal
raw weights within each orbit. Augmentation is a declared model policy, not
an inference that a source or application is physically mirror-symmetric.

## Overlap and output projection

Compatibility is structural, not a count of observed neighboring windows.
For a neighbor displaced by `(dx,dy,dz)`, compare
`A[x,y,z]` with `B[x-dx,y-dy,z-dz]` wherever both coordinates lie inside
their footprints. Directions are N=(0,−1,0), E=(1,0,0), S=(0,1,0),
W=(−1,0,0), U=(0,0,1), and D=(0,0,−1). Up really is positive Z.

A unit axis has an empty overlap in that direction, so all pairs are
compatible there. Patterns from different samples can connect when their
overlap is identical; that is structural generalization, not extraction
across a fabricated corpus seam. Compiled allowed relations have count 1;
pattern weights retain their observed frequencies.

Source boundary and output boundary are separate choices. For an anchor
grid `GW × GH × GD`:

- Open projection yields
  `(GW+W-1) × (GH+H-1) × (GD+D-1)`, including the outer footprint halo.
- Wrapped projection yields `GW × GH × GD`; every footprint contribution
  wraps onto that same output volume, and all repeated writes must agree.

`CaptureSolvedPatternGrid3D` captures the supplied **whole XYZ graph**, not a
Z slice. `TryProjectOverlappingPatternGrid3D` projects a validated assignment;
`ValidateOverlappingProjection3D` independently checks caller-supplied output
against every footprint contribution. Reports distinguish anchors, relations,
and projection contributions. A projection mismatch records the anchor plus
its XYZ footprint offset, not just a flat output index.

Low-level grid capture/projection supports open and wrapped output. The
maintained public-pass bridge and `TWfcPattern3DPassPipeline` currently support
**wrapped, same-sized XYZ passes only**. They do not resample, resize, map an
open halo into a larger pass, or provide general pass-to-pass coordinate maps.
An open-trained model may be used with wrapped output, but some such models
cannot tile that topology; learning success is not a satisfiability promise.

## Inverse public domains and transactional publication

The owner exposes `SetPublicDomain(X,Y,Z,Values)`,
`LockPublicCell(X,Y,Z,Value)`, `ClearPublicDomain`,
`ClearPublicDomains`, `HasPublicDomain`, and `CopyPublicDomain`.

Accepted token sets are deduplicated into model palette order. Unknown tokens
or invalid coordinates reject before changing the stored domain. An assigned
empty set means contradiction; it is different from clearing the constraint.

For each public cell and **every** footprint offset `(PX,PY,PZ)`, inverse
masking restricts the latent anchor at `(X-PX,Y-PY,Z-PZ)`, wrapped to the
output shape. All restrictions are intersected before solving. When several
offsets reach the same anchor on a small torus, every offset still matters:
their masks are ANDed, never discarded as duplicate coordinates. This also
handles footprints larger than an output axis.

The forward public bridge expresses the corresponding requirement for every
offset. Matching patterns at one offset are alternatives; requirements at
different offsets must all hold. Tokens that cannot occupy all required
wrapped footprint positions remain unavailable, without forbidding an
otherwise feasible subset of the model palette.

Accepted seed/domain edits invalidate current committed-result availability.
A malformed edit leaves the prior state untouched. After an unchanged-config
solve failure, an earlier committed result remains available through
`TryCopyCommitted`; the graph’s entry and random-stream snapshots roll back.
A previously returned composition is caller-owned immutable data, not a live
view of the owner’s latest settings.

For domain-specific final checks, derive from `TWfcPattern3DPassPipeline`
and override:

```pascal
function DoValidateProjection(const APatterns: TWfcPatternGrid3D;
  const AProjection: TWfcTokenGrid3D;
  out AReport: TWfcOverlapping3DValidationReport): Boolean; override;
```

Call the inherited validator, then perform deterministic, inspection-only
checks. Returning `False` with an invalid report rejects publication while
the graph transaction is still live; exceptions also propagate through
rollback. Report public XYZ coordinates directly with footprint offsets −1,
or report an anchor plus offsets when referring to a wrapped contribution.
Do not mutate the model, owner configuration, or external systems inside the
callback. External side effects are not rolled back. A custom full graph uses
the ordinary [graph commit-validation hook](solver.md#pipeline-transaction).

The same source order, policies, model bytes, graph topology, domain edits,
seed, solve budgets, and deterministic callback reproduce the same result.
`TWfcPattern3DComposition.Signature` is a versioned deterministic checksum of
the model context and complete XYZ assignment/projection. It is not a
cryptographic integrity guarantee or a substitute for independent validation.

## Canonical volume artifact and exact latent identities

`EncodeWfcPattern3DText` / `DecodeWfcPattern3DText` use `wfcp=2`, explicitly
`rank=3`, XYZ source shapes and footprint, source boundary/symmetry, the
ordered palette, and ordered payloads with raw weights. Tokens use canonical
ASCII percent encoding; records end in LF. The final
`relations=overlap` declaration derives compatibility from the payloads,
rather than serializing a quadratic relation table.

For example, a one-token, one-cell model is:

```text
wfcp=2
rank=3
samples=1
s=0,1,1,1
footprint=1,1,1
boundary=open
symmetry=none
directions=N,E,S,W,U,D
palette=1
t=0,land
patterns=1
p=0,1,0
relations=overlap
end
```

The decoder requires canonical spelling/order and a valid immutable model.
The old [2D pattern format](patterns.md), `wfcp=1`, its decoder, and its output
bytes are unchanged; the new volume format is not a silent reinterpretation
of a 2D artifact.

Latent graph keys use reserved `@p3v1;` syntax. Each carries footprint XYZ and
an exact actual-token payload encoded through a local first-appearance
dictionary, plus a leading routing hash. Reordering a model’s palette indices
does not change an otherwise identical payload key. Different actual tokens
or footprint dimensions cannot be equated merely because local indices match.

The routing hash is non-authoritative: complete key/payload equality remains
required, including after a hash collision. Treat externally supplied keys
as untrusted input, not proofs of model identity or public domain labels.
Keys do not encode every source-provenance or weight field; the bridge also
checks ordered source keys, weights, rules, denials, ownership, and topology.
The public bridge rejects reserved-key syntax in its palette. Persist public
tokens and canonical models, not private graph keys.

## Resource and scaling boundaries

These are explicit immutable-resource limits, not promises that every
maximum-size combination will fit available memory:

| Resource | Version-1 bound |
| --- | ---: |
| Source samples | 65,536 |
| Any source dimension | 4,194,304 |
| Cells per source and total corpus cells | 4,194,304 |
| Any footprint dimension and cells per footprint | 4,096 |
| Palette tokens | 4,096 |
| Unique patterns | 1,024 |
| Total retained pattern cells | 4,194,304 |
| Dense relation slots | 6,291,456 |
| Canonical volume text | 16,777,216 ASCII characters; 70,667 lines |

Checked products/counts and strict finite integer validation run before the
corresponding owned allocations. The relation matrix still contains
`6*P*P` Integer slots: 24 MiB at P=1,024 on native 32-bit-Integer targets,
before allocation overhead or other data. Equality-checked overlap-slab
classes avoid rescanning full slabs for every final matrix entry; hash
collisions still require full comparisons. This is not sparse relation
storage and not a constant-time construction claim.

Exact keys can amplify a compact input: a long palette token may occur in
many per-pattern dictionaries. Each final ASCII key and their aggregate
length must fit `High(Integer)`; aggregate capacity is checked before
creating any exact keys. Encoded palette strings already exist at that
preflight. This is representable-size protection, **not** a physical-RAM cap:
native/JavaScript strings, temporary copies, patterns, masks, graphs, solver
domains, and traces can exhaust memory much earlier.

Projection work also includes every anchor/footprint contribution, and inverse
domains may allocate a candidate mask for each affected anchor. Checked
Integer work bounds do not make arbitrarily large grids cheap. Generation is
finite, in memory, with existing bounded search options; this milestone does
not provide chunk streaming, cancellation, or an infinite-volume solver.

## Verified scope and pending integration

The [core tests](../test/wfc_pattern3d_test.lpr) compare a separate literal
transform/extraction oracle, all six overlap planes, chirality, source isolation,
wrap seams, full-hash collisions, detached ownership, and malformed native/JS
inputs. [Codec tests](../test/wfc_pattern3d_text_test.lpr) exercise canonical
volume artifacts; [pass tests](../test/wfc_pattern3d_passes_test.lpr) exercise
semantic consumers, inverse masks, torus aliasing, independent public capture,
and transactional replay. Finite tests are evidence, not a proof over all
possible models.
The [overlapping-volume experiment](research/overlapping-volumes-v1.md)
records the measured scope, checks, and remaining performance boundaries.

Still pending: training-source/workspace support for overlapping volumes,
declarative pipeline-resource lowering, artifact-family dispatch in the
inspection/validation tools, and a fully featured browser volume demo.
Existing radius-one volume training and 2D pattern tools do not automatically
gain this new footprint capability. General resampling between passes,
streamed spatial solving, and broader 3D authoring remain separate work.
