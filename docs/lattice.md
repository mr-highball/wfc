# Exact signed-world lattices

[`wfc_lattice`](../src/wfc_lattice.pas) is the project-owned MIT coordinate
kernel for layers with different cell counts, origins, and resolutions. It
uses only the FPC runtime, works under pas2js, and does not depend on `wfc`,
the solver, a renderer, or a random-number generator. Its contract is version
`WFC_LATTICE_VERSION = 1`.

A layout describes **axis-aligned integer cells**, not physical geometry or
an interpolation filter. A world point selects one provider cell; a world
box selects every provider cell it intersects. Consumers can therefore use
their own cell boxes to query differently sized providers without pretending
that their array indices coincide.

## Layout and bounds

```pascal
TWfcLatticeVector = record
  X, Y, Z: Integer;
end;
TWfcLatticeLayout = record
  Cells, Origin, Pitch: TWfcLatticeVector;
  Wrap: Boolean;
end;
TWfcLatticeBox = record
  Minimum, Maximum: TWfcLatticeVector;
end;
```

All three axes are present, including for a one-layer or one-row layout.
`Cells` and `Pitch` must be strictly positive. `Origin` may be negative.
Wrapping applies to all three axes. There is no rotation, scaling by a
fractional factor, or separate wrap flag for each axis in this version.

For each axis with cell coordinate `c`, origin `o`, and pitch `p`, its world
interval is `[o + c*p, o + (c+1)*p)`. The upper endpoint is excluded. Two
intervals that only touch at an endpoint do not intersect. A layout's complete
world extent is `[Origin, Origin + Cells*Pitch)` on each axis.

`MakeWfcLatticeLayout(Width, Height, Depth, Wrap)` supplies origin `(0,0,0)`
and pitch `(1,1,1)`. The longer overload takes explicit `Origin` and `Pitch`
vectors before `Wrap`. Both constructors validate the layout, as do
`ValidateWfcLatticeLayout`, `WfcLatticeCellCount`, `SameWfcLatticeLayout`, and
the mapping functions. Layout equality includes all dimensions, origins,
pitches, and the wrap flag.

## Points and half-open boxes

`TryWfcLatticePoint(Layout, World, Cell)` computes each unwrapped cell index
as `floor((World - Origin) / Pitch)`. It uses mathematical floor, not Pascal's
toward-zero integer division: with origin `0` and pitch `2`, world `-1`
belongs to unwrapped cell `-1`, not `0`.

For bounded layouts an outside point returns `False` and clears the output
vector. For wrapped layouts the index is reduced to its nonnegative residue
modulo that axis's cell count. Negative coordinates and coordinates several
periods away are valid. The point input and output may name the same record.

`WfcLatticeCellBox(Layout, Cell)` returns the exact box of a valid local cell
index. It does not wrap an out-of-range local index: that is an error.

`TryWfcLatticeCoverage(Layout, WorldBox, Coverage)` requires a **positive box
span on every axis**. For a bounded layout the entire box must fit in the
provider extent. A partially overlapping box returns `False` and canonical
empty coverage; it never silently clips the box or succeeds with no cells.
For wrapped layouts every periodic image is considered, but each local
provider cell is returned only once.

Because the endpoints and pitches are integers, the intersected unwrapped
cell interval on one axis is exactly:

```text
first = floor((box.minimum - origin) / pitch)
last  = floor((box.maximum - 1 - origin) / pitch)
```

These inclusive cell indices implement the half-open world-box convention.
For wrapping, an interval that reaches every local cell becomes one full
interval. Otherwise its nonnegative residues produce one interval, or two
sorted intervals when it crosses the seam. A query slightly shorter than a
world period can still touch every cell if its endpoints are not aligned.

## Lazy, unique coverage

Coverage owns no footprint-sized array:

```pascal
TWfcLatticeInterval = record
  First, Last: Integer;             { inclusive }
end;
TWfcLatticeAxisCoverage = record
  IntervalCount: Integer;           { 0, 1, or 2 }
  Intervals: array[0..1] of TWfcLatticeInterval;
  Count: Integer;                   { unique cells on this axis }
end;
TWfcLatticeCoverage = record
  X, Y, Z: TWfcLatticeAxisCoverage;
end;
```

Intervals are sorted, disjoint, and nonadjacent; adjacent intervals must be
merged. Unused interval slots contain zeroes. `Default(TWfcLatticeCoverage)`
is canonical empty coverage. Every nonempty coverage has nonempty X, Y, and
Z axes. The public count and ordinal functions validate these invariants,
including counts and numeric bounds, even for manually authored records.

`WfcLatticeCoverageCellCount` returns the checked Cartesian-product count.
`WfcLatticeCoverageCell(Coverage, Ordinal)` resolves a zero-based ordinal
directly in canonical flat order: **X fastest, then Y, then Z**. It does not
scan earlier cells. Query storage and per-ordinal work are constant with
respect to the covered cell count. Iterating every ordinal is, naturally,
linear in that count; downstream constraints still pay for cells they visit.

This standalone example queries a seam-crossing box against five cells
whose world pitch is two:

```pascal
program LatticeCoverageExample;
{$mode delphi}{$H+}
uses SysUtils, wfc_lattice;
var
  Layout: TWfcLatticeLayout;
  Box: TWfcLatticeBox;
  Coverage: TWfcLatticeCoverage;
  Cell: TWfcLatticeVector;
  I: Integer;
begin
  Layout := MakeWfcLatticeLayout(5, 1, 1,
    MakeWfcLatticeVector(-5, 0, 0),
    MakeWfcLatticeVector(2, 1, 1), True);
  Box.Minimum := MakeWfcLatticeVector(-6, 0, 0);
  Box.Maximum := MakeWfcLatticeVector(-4, 1, 1);
  if not TryWfcLatticeCoverage(Layout, Box, Coverage) then
    raise Exception.Create('The requested box is not covered');
  for I := 0 to WfcLatticeCoverageCellCount(Coverage) - 1 do
  begin
    Cell := WfcLatticeCoverageCell(Coverage, I);
    WriteLn(Cell.X, ',', Cell.Y, ',', Cell.Z);
  end;
end.
```

Output is `0,0,0` followed by `4,0,0`, regardless of the order in which those
periodic images occur along the world box. With wrapping disabled, this same
box is rejected because it starts before the bounded provider extent.

## Exactness and capacity

There is no project-imposed cell-count or query-footprint cap. There are
explicit numeric limits:

- Every public scalar must be a signed 32-bit `Integer`; dimensions and
  pitches must be positive. pas2js additionally rejects fractional,
  nonfinite, out-of-range, or incorrectly typed JavaScript values and
  non-Boolean wrap flags.
- The product of the three cell counts must not exceed `High(Integer)`.
- Every axis's world origin and exclusive final endpoint must fit in
  `Low(Integer)..High(Integer)`. An exclusive endpoint at `High(Integer)+1`
  is not representable by this API.
- Query endpoints have the same signed range; boxes must have positive
  span on each axis. Coverage counts and their Cartesian product must fit
  `Integer`; an ordinal must select an existing covered cell.

A negative-origin layout may have a legal world span larger than
`High(Integer)`. For example, three cells with pitch `1431655765` starting
at `Low(Integer)` end exactly at `High(Integer)`, a span of `4294967295`.
This is accepted. The implementation checks endpoint capacity **before**
multiplying cell count by pitch, and uses exact wide numeric intermediates
for differences, floor correction, and seam handling. These intermediates
remain below `2^33`, within both native Double and JavaScript Number's exact
integer range. No 62-bit potentially inexact product or bitwise narrowing
is used to decide validity.

Invalid layouts, malformed records, invalid local cells/ordinals, and
nonpositive query boxes raise `EWfcLattice`. Ordinary bounded misses return
`False`. There is no silent clipping or saturation. Representable size is
not a promise that allocating or solving a graph of that size will fit
memory; this unit only keeps the mapping itself lazy.

## Verification and scope

[`wfc_lattice_test`](../test/wfc_lattice_test.lpr) compares small XYZ layouts
and boxes with a separate forward interval-image intersection oracle,
including negative origins, nonaligned pitches, seams, repeated periods,
and exact touching endpoints. It also checks signed-coordinate extremes,
overflow preflight, canonical records, aliasing, and huge lazy ordinals.

Local checked FPC 3.2.2 and 3.3.1 Win32/Win64 runs each pass 168,687
assertions over 22,032 oracle cases. The same Pascal program in actual Edge
passes 168,887 assertions, including 200 additional malformed-JavaScript
checks, using the included FPC server and browser-validation tools. Local
evidence is recorded in the ignored
`build/hosted-ci/lattice/VALIDATION.md`. These counts describe that verified
snapshot, not an assertion of execution on every supported operating system.

The lattice API supplies exact spatial correspondence. It does not itself
add pass dependencies, evaluate constraints, change solver ordering, project
an overlapping model, or serialize a pipeline. Those integrations must state
their own mapping and compatibility contracts.
