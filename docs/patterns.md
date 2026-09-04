# overlapping 2D patterns

Radius-one token adjacency answers whether two individual values may touch.
Overlapping-pattern WFC preserves a larger local arrangement: each latent
graph value represents an entire rectangular token footprint, and adjacent
latent values are compatible only when every shared footprint cell agrees.
This retains corners, short motifs, diagonals, and other multi-cell structure
that a single-token model necessarily forgets.

The implementation is split into three dependency-free Pascal units:

- `wfc_pattern2d` contains the immutable pattern model, compatibility
  compiler, solved-assignment capture, projection, and independent validators;
- `wfc_pattern2d_learn` extracts deterministic patterns from one sample or an
  ordered heterogeneous corpus; and
- `wfc_pattern2d_text` reads and writes strict canonical `wfcp=1` artifacts.

They use only repository units and the FPC/pas2js runtime libraries.

## basic use

```pascal
uses
  SysUtils,
  wfc,
  wfc_model,
  wfc_learn,
  wfc_pattern2d,
  wfc_pattern2d_learn,
  wfc_pattern2d_text;

var
  Assignment: TWfcPatternGrid2D;
  Graph: TGraph;
  Learned: TWfcOverlappingModel2D;
  Loaded: TWfcOverlappingModel2D;
  Options: TGraphSolveOptions;
  Output: TWfcTokenGrid2D;
  PatternReport: TWfcOverlapping2DValidationReport;
  Report: TGraphSolveReport;
  Sample: TWfcModelTokens;
  Text: String;
begin
  { Fill Sample with a row-major 5x5 token grid. }
  Learned := LearnOverlappingModel2D(Sample, 5, 5,
    2, 2, wmbWrap, wmsD4);
  try
    Text := EncodeWfcPattern2DText(Learned);
    Loaded := DecodeWfcPattern2DText(Text);
    try
      Graph := TGraph.Create.Reshape(16, 10, 1);
      try
        Graph.Seed := 0;
        Graph.WrapNeighbors := True;
        ApplyOverlappingModel2DToGraph(Loaded, Graph);
        Options := DefaultGraphSolveOptions;
        if not Graph.TrySolve(Options, Report) then
          raise Exception.Create('pattern solve failed');

        if not CaptureSolvedPatternGrid2D(Loaded, Graph, 0,
            Assignment, PatternReport) then
          raise Exception.Create(
            DescribeOverlapping2DIssue(PatternReport.Issue));
        if not TryProjectOverlappingPatternGrid2D(Loaded,
            Assignment, Output, PatternReport) then
          raise Exception.Create(
            DescribeOverlapping2DIssue(PatternReport.Issue));
        if not ValidateOverlappingProjection2D(Loaded,
            Assignment, Output, PatternReport) then
          raise Exception.Create(
            DescribeOverlapping2DIssue(PatternReport.Issue));
      finally
        Graph.Free;
      end;
    finally
      Loaded.Free;
    end;
  finally
    Learned.Free;
  end;
end;
```

`ApplyOverlappingModel2DToGraph` applies to the exact graph or pass-local graph
the caller supplies. It does not reshape the graph, select or create a pass,
change wrapping, choose a seed, or solve.

## deterministic extraction

`WFC_OVERLAPPING_2D_ALGORITHM_VERSION = 1` fixes the extraction identity.
For `LearnOverlappingModel2DCorpus`, ordering is:

1. source sample order;
2. transform order; and
3. origin row-major order within the transformed source.

The palette is established separately from the original, untransformed
samples in source order and row-major first-occurrence order. The first
observed unique payload fixes its pattern index. Repeated payloads checked-add
to its raw positive weight.

With `wmbOpen`, only fully contained footprints are observed and every source
must be at least as large as the footprint. With `wmbWrap`, every source cell
is an origin and footprint lookup wraps within that source only. A wrapped
footprint may be larger than its source; modular lookup defines the repeated
payload without overflowing coordinate arithmetic. No payload ever spans two
corpus samples.

`wmsD4` uses the same explicit eight-transform order as radius-one learning:
identity, three clockwise quarter turns, then an X mirror followed by those
four rotations. It initially requires a square footprint, although source
grids may be rectangular. The pattern set must be closed under all eight
transforms and every member of a transform orbit has the same raw weight.

## structural compatibility

Relations are derived from payload structure, not merely from which extracted
windows happened to be neighbors. For source pattern `A`, target pattern `B`,
width `W`, and height `H`:

| Direction from `A` | Cells that must agree |
| --- | --- |
| north | `A[x,y] = B[x,y+1]`, `y = 0 .. H-2` |
| east | `A[x,y] = B[x-1,y]`, `x = 1 .. W-1` |
| south | `A[x,y] = B[x,y-1]`, `y = 1 .. H-1` |
| west | `A[x,y] = B[x+1,y]`, `x = 0 .. W-2` |

A zero-sized overlap is vacuously compatible. Width-one footprints therefore
allow every horizontal pair, height-one footprints allow every vertical pair,
and a `1x1` pattern model is weighted independent sampling.

Structural compatibility can legitimately connect patterns learned from
different samples when their overlapping cells agree. That is generalization
from equal local structure, not a fabricated corpus seam: extraction never
constructs a payload using cells from two sources.

The compiled `TWfcModel` stores each allowed structural relation as Boolean
count `1`; pattern weights retain observed frequencies. Its stable private
keys are `@p0`, `@p1`, and so on. `CompiledModel` is a borrowed immutable
reference owned by the wrapper. Applications should normally use
`ApplyOverlappingModel2DToGraph` and must never persist private keys as their
domain output.

## latent assignment and token projection

The solved graph is a grid of pattern anchors, not a grid of public palette
tokens. `CaptureSolvedPatternGrid2D` converts an explicitly supplied solved
graph plane into `TWfcPatternGrid2D` and independently validates every east
and south overlap, including wrapped seams and self-neighbors.

Projection writes every cell of every assigned footprint. All repeated writes
must agree. For an anchor grid `GW x GH`:

- open projection is `(GW + W - 1) x (GH + H - 1)`; and
- wrapped projection is `GW x GH`, with projected coordinates wrapped to that
  shape.

`TryProjectOverlappingPatternGrid2D` performs the projection.
`ValidateOverlappingProjection2D` checks a caller-supplied result again from
the assignment and payloads rather than trusting the projector. Reports count
checked anchors, relations, and footprint contributions and identify invalid
indices, empty or unknown graph values, bad overlaps, and changed output
tokens.

Source boundary and output boundary are deliberately different data:

- `SourceBoundary` records how training footprints were extracted;
- `TWfcPatternGrid2D.Boundary` records how an assignment is validated and
  projected; and
- `TGraph.WrapNeighbors` controls the actual solve topology.

Loading or applying a model never silently copies one policy into another.

Because latent keys and projected tokens are different domains, projection is
also an explicit pass boundary. A later pass cannot honestly consume the
latent graph as though it contained palette tokens. General projection-aware
pass dependencies remain future pipeline work; the current API keeps that
distinction visible instead of hiding a lossy conversion.

## canonical `wfcp=1`

The pattern artifact is standalone rather than an embedded `.wfcm`. It stores
the information that the generic model IR does not have: source shapes,
footprint, public palette, row-major pattern payloads, and the complete
structural relation set. Private compiled keys never appear.

A minimal exact document is:

```text
wfcp=1
rank=2
samples=1
s=0,1,1
footprint=1,1
boundary=wrap
symmetry=none
directions=N,E,S,W
palette=1
t=0,A
patterns=1
p=0,1,0
relations=4
r=N,0,0
r=E,0,0
r=S,0,0
r=W,0,0
end
```

Every document is ASCII with LF line endings and one required final LF.
Palette tokens use the same canonical UTF-8 percent encoding as `.wfcm`.
Pattern records contain `index,weight,palette-index...`, with exactly
`W x H` row-major payload cells. Relation records are ordered north, east,
south, west, then source pattern, then target pattern.

The decoder rejects reordered or unknown fields, noncanonical integers or
escapes, invalid Unicode, duplicate or unused palette values, duplicate or
malformed payloads, impossible weight totals, invalid D4 orbits, missing or
invented relations, nonreciprocal data, CRLF, a missing final LF, and trailing
data. It independently recomputes every structural relation and requires the
decoded model to re-encode byte-for-byte to the input.

The explicit relation list is redundant by design: it makes replay semantics
human-inspectable while independent recomputation detects corruption or a
compatibility-algorithm mismatch.

## graph representation boundary

Some open samples yield edge-only patterns with no support in one direction.
The specialized model retains them honestly. The historical graph rule API
treats an absent or empty rule as a wildcard, so such a row cannot represent
“allow nothing.” Application raises `empty-support-not-representable` instead
of silently widening the model. Wrapped samples guarantee that every extracted
pattern has at least its observed neighbors as structural support.

Dense pattern sets also exposed a construction bottleneck in the generic
adapter. `ApplyModelToGraph` now prebuilds complete reciprocal rule arrays
after full validation, then installs them once. This preserves the public rule
order and later fluent inverse synchronization while avoiding a whole-graph
fixed-point rebuild for every dense edge.

## replay identity and current limits

Exact retraining requires the ordered original token grids and shapes,
footprint, source boundary, symmetry, and
`WFC_OVERLAPPING_2D_ALGORITHM_VERSION`. A canonical artifact contains all data
needed to solve and project but deliberately does not contain the original
corpus. Solving additionally depends on graph shape and wrapping, locks, seed,
solver options, and solver/random versions.

This first implementation is cardinal 2D with rectangular footprints and
optional square-footprint D4 augmentation. It uses a dense compatibility
matrix and portable linear first-seen lookup for deterministic identity.
Sparse acceleration, 3D patterns, provenance/source licenses, raw image
ingestion, and projection-aware pass composition remain later milestones.
The complete native and pas2js example is
[LearnPatterns](../examples/learning/03_LearnPatterns/README.md).
