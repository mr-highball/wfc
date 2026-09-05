# Learning full volumes

`wfc_learn3d` learns radius-one, six-face constraints from ordered tokenized
volumes. It returns the same immutable `TWfcModel` used by the existing
learner, graph adapter, training tools, and pass recipes. The implementation
is project-owned Pascal with the standard RTL only, shared by FPC and pas2js.

Try preset 5 in [Training Studio](../examples/learning/05_TrainingStudio/README.md),
or replay the complete [adjacency3d source-to-result bundle](../examples/learning/04_TrainingDocuments/README.md).
Both native and browser demos validate actual Z-neighbor constraints, not
independent 2D boards displayed on top of each other.

## Shapes, tokens, and observations

`MakeLearnSample3D(Tokens, Width, Height, Depth)` constructs a detached
`TWfcLearnVolumeSample`. `LearnModel3DCorpus(Samples, Boundary, Symmetry)`
accepts an ordered array of these records. Samples can have different positive
extents; no observation crosses from one sample into the next. The convenience
function `LearnModel3D` accepts one token array and all three dimensions.

Flattening is X-fast, then Y, then Z:

```pascal
Index := X + Width * Y + Width * Height * Z;
Model := LearnModel3D(Tokens, Width, Height, Depth, wmbOpen, wmsNone);
try
  Graph := TGraph.Create.Reshape(OutputWidth, OutputHeight, OutputDepth);
  try
    Graph.WrapNeighbors := False;
    Graph.Seed := 55;
    ApplyModelToGraph(Model, Graph);
    Options := DefaultGraphSolveOptions;
    if not Graph.TrySolve(Options, Report) then
      raise Exception.Create('volume solve did not succeed');
  finally Graph.Free end;
finally Model.Free end;
```

Use `SysUtils`, `wfc`, `wfc_model`, and `wfc_learn3d`. The caller supplies
the token array, positive dimensions, graph/model variables, and ordinary
solve options/report. Source boundary policy and output graph topology are
separate choices at this low-level API; exported training recipes carry the
source policy into their run topology automatically.

The model convention remains consistent with the row-major 2D learner:

| Direction | Target offset from source | Historical `TGraph.NewRule` key |
| --- | --- | --- |
| `wmdNorth` / `N` | `(0,-1,0)` | `gdNorth` |
| `wmdEast` / `E` | `(1,0,0)` | `gdWest` |
| `wmdSouth` / `S` | `(0,1,0)` | `gdSouth` |
| `wmdWest` / `W` | `(-1,0,0)` | `gdEast` |
| `wmdUp` / `U` | `(0,0,1)` | `gdDown` |
| `wmdDown` / `D` | `(0,0,-1)` | `gdUp` |

The graph rule key describes its owner relative to the candidate, hence the
reversal. The graph also historically names increasing Y north. Use the
adapter instead of interpreting enum ordinals as interchangeable directions.

Open observations skip absent neighbors. Wrapped observations resolve each
axis independently, including a length-one self-neighbor. First-seen vocabulary
order follows original sample index and flattened position. Weights and
relations retain raw integer counts, with checked arithmetic. A positive
relation becomes Boolean allowed support in the current solver; its frequency
does not bias that edge. A zero-support active row becomes explicit denial,
not a wildcard.

## Explicit orientation policy

| Policy | Observations per sample | Meaning |
| --- | --- | --- |
| `wmsNone` / `none` | 1 | Keep every axis and direction distinct |
| `wmsD4` / `d4` | 8 | Four yaw rotations and four reflected variants; fix +Z and -Z |
| `wmsCubeRotations` / `cube24` | 24 | All proper signed-axis cube rotations |
| `wmsCubeFull` / `cube48` | 48 | All signed-axis permutations, including reflections |

Use `none` when absolute orientation matters. D4 preserves vertical direction,
which is useful for layered terrain. Cube policies deliberately permit vertical
and horizontal directions to exchange: a learned floor can become a wall.
The policy is corpus-wide; differently oriented sources must be prepared by
the caller or learned as separate compatible resources/passes.

These transforms move coordinates, **not token payloads**. Labels such as
`stone` can be rotation-neutral; `stair-facing-east` is not automatically
renamed when rotated. There is no socket, mesh, pitch, or semantic remapping.
Do not augment directional labels unless their interpretation really is
invariant under the chosen transforms.

Repeated transforms are counted, including symmetric samples. For radius-one
scalar labels, cube48 has exactly twice cube24's weights and relation counts,
but identical Boolean support. Reflections do not add chirality information
to this representation. See the [observation-orbit derivation](research/volume-observation-orbits-v1.md)
for why, and for the independent literal-transform checks.

## Model and artifact compatibility

Rank 3 requires all six directions and explicit positive source depth.
`SampleDepth` reports the first sample; `SampleShapeAt` and `CopySampleShapes`
retain the ordered full corpus. `MergeWfcModels` supports compatible volumes:
rank, boundary, symmetry, and active directions must match. It reindexes the
ordered vocabularies, concatenates shapes, and adds raw counts without making
cross-sample seams.

`CopyRelations` retains **four** dense planes for rank 1/2 and uses six for
rank 3. Query inactive up/down relations on a legacy model to get zero; invalid
indices still raise. Use `WfcModelStoredDirectionCount(Rank)` when allocating
dense input arrays. Do not size legacy arrays from the expanded enum's maximum.
Existing numeric constructors and two-argument shape helpers still work for
rank 1/2. Their owned depth is normalized to one, without reading an appended
uninitialized record field. Use the explicit-depth constructor or shape helper
for rank 3; the old numeric constructor rejects it.

Canonical model encoding selects by rank and sample count:

| Model | Text profile | Shapes |
| --- | --- | --- |
| Rank 1/2, one sample | `wfcm=1` | Original width/height fields |
| Rank 1/2, multiple samples | `wfcm=2` | `s=index,width,height` |
| Rank 3, one or more samples | `wfcm=3` | `s=index,width,height,depth` |

Volume documents use `directions=N,E,S,W,U,D`, retain existing value/relation
record syntax and ordering, and accept the four symmetry codes above. The
strict reader rejects rank/profile mismatches, missing depth, extra fields,
noncanonical integers, invalid Unicode, incorrect direction sets, nonreciprocal
relations, and trailing data. Rank-1/2 model and training golden bytes remain
unchanged. The outer pipeline format already supports rank 3 and needs no bump.

## Editable training and pass composition

Volumes use `kind=adjacency3d` in `wfclearn=2`. For example:

```text
wfclearn=2
name=vertical-cycle
license=MIT
source=project-authored%20example
kind=adjacency3d
boundary=wrap
symmetry=none
footprint=0,0
order=0
samples=1
sample=0,1,1,2,column
token=0,0,lower
token=0,1,upper
end
```

Keep LF endings and the final LF. A sample record has exactly five fields:
index, width, height, depth, name. The token count is the checked product of
all three dimensions. With `none`, the example learns horizontal self-support
and vertical alternation. It is not a gravity simulation; the names carry no
special meaning. Cube augmentation would deliberately mix those relationships.

Programmatic training uses the depth overload:

```pascal
Samples[0] := MakeWfcTrainingSample('column', 1, 1, 2, Tokens);
Options := MakeWfcTrainingOptions(wtkAdjacency3D, wmbWrap, wmsNone, 0, 0, 0);
```

Construct the immutable training document with explicit metadata, then call
`LearnWfcTrainingRecipe` or the `wfc_learn` executable. The recipe exposes a
public `output` pass backed by a normal `wprkModel` resource. Other passes can
consume its labels through the existing same-cell, exact-offset, neighborhood,
and count requirements. The training exporter does not invent those later
passes or their semantic rules; construct the broader recipe explicitly.

The shared workspace requires
`ConfigureVolumeRun(SolveOptions, Depth, Locks, Domains)`. Its older flat API
rejects rank 3. Run/result artifacts retain full XYZ coordinates and depth.
Changing dimensions or constraints invalidates old output before validation;
a failed draft cannot export a preceding successful result as its own.

Source/license declarations remain attached to recipes. Volume sample manifests
include all dimensions, and depth participates in version-2 training and sample
fingerprints. These 32-bit fingerprints are replay identifiers, not security
hashes or license verification. The standalone model must travel with its source
or recipe to preserve declared provenance.

## Capacity and remaining work

This API solves finite volumes. It does not claim unlimited in-memory space
or bounded solving time. The existing model envelope permits 1,024 values and
4,194,304 source cells total; six direction planes require at most 6,291,456
relation slots. The training-document envelope is smaller: 65,536 tokens and
4,096 samples. Augmentation contributes to its extraction budget. Pipeline
aggregate accounting charges all six planes, including absent serialized
relations. Text, vocabulary, arithmetic, and solver/run budgets also apply.

The Studio's caller-selected interactive policy is 512 total output cells.
Native callers can select the larger documented workspace/run envelope.
Neither policy makes an unsatisfiable wrapped size possible: the alternating
checkerboard requires even cycle lengths. Locks and budgets are inputs, not
promises of a successful solve.

Overlapping 3D footprints, orientation-aware token transforms, raw voxel/mesh
file import, streamed/chunked volume generation, and semantic physical
validation remain separate extensions. A learned label model is not an authored
`TVoxel3DKit`; it has no inferred sockets, prototype meshes, support strength,
or scene identity. The [model-to-voxel bridge and Learned Terraces](learned-terraces3d.md)
now compose such a learned public-token provider with explicitly authored
socket/support structures and spatial foliage. The semantics are authored,
not inferred by the learner; the owner validates them before commit.
