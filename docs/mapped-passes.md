# World-space mapped passes

`WFC_PASS_MAPPING_VERSION = 1` adds opt-in, integer-world relationships between
passes with different dimensions, origins, cell pitches, and boundary modes.
The implementation is shared by native FPC and pas2js and has no additional
runtime dependencies. Existing solver, trace, negotiation, and pipeline version
numbers do not change.

A coarse terrain cell can cover sixteen foliage cells. A building footprint can
then require every underlying foliage cell to be clear, including cells that
are nowhere near a corner. A dependency controls execution order; a mapped
query specifies exactly which provider cells a candidate reads.

## Configure the coordinate systems

Use `wfc` and `wfc_lattice`. A layout contains `Cells`, `Origin`, and `Pitch`
vectors plus `Wrap`. Cell `(x,y,z)` occupies the half-open world box
`[Origin + cell * Pitch, Origin + (cell + 1) * Pitch)`.

This example covers one 32 × 24 world with three different grids:

```pascal
Graph.CurrentPass := 'terrain';
Graph.PassMode := gpmOverlay;
Graph.AddValue('land');
Graph.AddValue('water');

Graph.SwitchToPass('foliage');
Graph.PassMode := gpmOverlay;
Graph.AddValue('clear');
Graph.AddValue('tree');

Graph.SwitchToPass('housing');
Graph.PassMode := gpmOverlay;
Graph.AddValue('empty');
Graph.AddValue('house');

SetLength(Layouts, 3); //Layouts: TWfcLatticeLayouts
Layouts[0] := MakeWfcLatticeLayout(8, 6, 1,
  MakeWfcLatticeVector(0, 0, 0), MakeWfcLatticeVector(4, 4, 1), False);
Layouts[1] := MakeWfcLatticeLayout(32, 24, 1, False);
Layouts[2] := MakeWfcLatticeLayout(3, 2, 1,
  MakeWfcLatticeVector(4, 4, 0), MakeWfcLatticeVector(8, 8, 1), False);
Graph.ConfigurePassLayouts(Layouts);

Graph.PassGraph[1].Rules['tree'].RequireMappedFromPass('terrain',
  MakeGraphPassCellQuery(['land']));
Graph.Rules['house']
  .RequireMappedFromPass('terrain', MakeGraphPassCellQuery(['land']))
  .RequireMappedFromPass('foliage', MakeGraphPassCellQuery(['clear']));
```

Each housing cell reads four terrain cells and sixty-four foliage cells. The
six housing sites are inset from the world boundary. An `empty` site remains a
valid fallback when a house cannot satisfy its footprint. If a house is locked
or required by its entry domain, incompatible foliage produces a contradiction
instead of silently choosing the fallback.

`ConfigurePassLayouts` requires exactly one valid layout per existing pass. It
validates every layout and existing cross-pass relationship, prepares all new
entry storage, and only then publishes the replacement. Invalid input or an
entry-factory exception preserves all old layouts, values, locks, domains, and
pass selection. Success clears values, caller locks, and entry domains, just
like `Reshape`, while retaining rules, weights, dependencies, and constraints.
Connectivity anchors must fit the proposed dimensions. Layout records and
arrays are copied; changing the input afterward cannot change the graph.
Entry factories run under the pass-initialization lifecycle guard: they may
inspect existing storage but cannot recursively reshape, reset, switch/create
passes, or change pipeline-wide settings. As with other user hooks, factories
must not directly mutate public rule/entry state; arbitrary caller side effects
are not journaled by storage allocation.

Select overlay mode before configuring unlike empty passes. A definitionless
legacy pass copies its predecessor; that is an index-space read, not merely
an order-only dependency, and therefore requires identical layouts.

## Choose what a candidate reads

Every offset below is in **world units**, relative to the lower world corner
of the consumer cell. It is not a provider-cell or consumer-cell index offset.

| Factory | Provider cells inspected |
| --- | --- |
| `MakeGraphPassPointQuery(Offset, Values)` | The one cell containing the world point. |
| `MakeGraphPassCellQuery(Values)` | Every cell intersecting the consumer cell's complete world box. |
| `MakeGraphPassCellQuery(Offset, Values)` | Every cell intersecting the translated complete box. |
| `MakeGraphPassRegionQuery(MinimumOffset, MaximumOffset, Values)` | Every cell intersecting the explicit half-open relative box. |

The default match mode is `gpmmAll`: every inspected cell must be nonempty and
contain one of the accepted values. Touching only a face, edge, or corner does
not count as a box intersection. In 2D, use depth and Z pitch of one; region
queries still need a positive Z extent, for example `[0,1)`.

To count accepted cells instead, wrap a query in an inclusive range:

```pascal
Graph.Rules['garden'].RequireMappedFromPass('foliage',
  MakeGraphPassCountQuery(
    MakeGraphPassRegionQuery(MakeGraphOffset(-2, -2, 0),
      MakeGraphOffset(10, 10, 1), ['tree']), 2, 6));
```

This requires two through six tree cells in the stated world-space region.
Other values and empty cells contribute zero. It counts **unique provider
cells**, not offsets, world-space volume, or overlapping area. A partial
intersection counts as one cell. A wrapped query covering several periods
still counts each provider cell once.

Point counts permit only `0 <= minimum <= maximum <= 1`. Coverage counts permit
`0 <= minimum <= maximum <= High(Integer)`; a minimum larger than the actual
coverage is simply unsatisfiable. All-match queries require zero count fields.

## Boundaries and numeric capacity

A bounded provider must contain the **entire** query. A partially outside
footprint fails, including a `0..0` count query; there is no clipping or vacuous
success. This intentionally differs from legacy finite-offset counts, where
an unresolved individual term contributes zero. Wrapped providers wrap their
own coordinates independently of the consumer's neighbor topology.

All dimensions and pitches are positive integers. Each pass's cell count and
every world endpoint must fit signed `Integer`; a world span may exceed
`High(Integer)` when its negative origin and positive endpoint both fit.
Registration and reconfiguration prove the query endpoints representable for
every possible consumer cell before mutation. Fractional, nonfinite, wrongly
typed, and out-of-range JavaScript numeric input is rejected, not truncated.
The independent [lattice contract](lattice.md) specifies exact arithmetic and
lazy coverage enumeration. There is no footprint-sized temporary coordinate
array or arbitrary mapped-visit cap; practical memory and search costs remain.

## Composition, ownership, and repair

Each registration is ANDed with every other mapped or legacy requirement,
directional rule, entry domain, and applicable global constraint. Values inside
one query are alternatives. Repeating the same query is idempotent, including
reordered or repeated accepted values. Factories and registration detach the
accepted-value arrays.

The provider must already exist. Its stable index is bound at registration;
renaming it does not retarget the query. Registration infers a protected
requirement dependency. Invalid queries, self-dependencies, cycles, and edits
during execution are rejected before installing the clause.

Both `Run` and the `TrySolve` family evaluate the same mapped predicate. The
reference solver reads staged provider output. Failed finite and selective
attempts restore each pass's differently sized storage and random stream.
Negotiation may reopen upstream assignments only within its authorized scope
and budget. A housing-only repair cannot silently alter foliage. An authorized
foliage repair also reopens its dependent housing, while independent passes
remain unchanged. Full chronological negotiation can spend its budget
enumerating unrelated earlier assignments; mapping does not add a minimal
repair or optimality guarantee.

Trace coordinates and entry indices remain local to the reported pass. Mapped
failure uses the existing named-provider attribution; traces do not include a
new list of every intersected cell or a minimal conflicting footprint.

## Compatibility and scope

- `PassLayout` is a detached snapshot of the selected pass, or the receiver
  when accessed through `PassGraph[Index]`.
- Root `Dimension` retains its historical default-grid meaning (pass zero).
  A direct pass graph's `Dimension` describes that pass. Do not use the root
  dimensions to enumerate all passes after configuring unlike layouts.
- New passes inherit pass zero's default layout, not the previously selected
  pass's layout. `Reshape` restores uniform dimensions, origin zero, unit pitch,
  and the root wrapping setting across all passes. `Reset` restores an empty,
  unshaped pass whose layout cannot yet be used for mapped reads.
- Assigning `WrapNeighbors` still updates and relinks **every** pass. Use
  `ConfigurePassLayouts` to specify different per-pass wrapping. `Mode` remains
  pipeline-wide.
- Legacy `RequirePrevious`, `RequireFromPass`, `RequireFromPassAt`, any/count
  stencils, and copy/transform sources require identical dimensions, origin,
  pitch, and wrapping. They do not automatically become mapped reads.
- Existing sequence, overlapping-pattern, and voxel projection bridges retain
  their identical-layout contracts. Their complete preflights reject unlike
  layouts before adding values, masks, requirements, or dependencies. A latent
  pattern key is still not a public terrain token.
- The additive [portable mapped pipeline extension](portable-mapped-passes.md)
  is under integration with recipe5/run2/result2 and per-invocation pass extents.
  It preserves the old uniform formats. The
  [Mapped World workbench](../examples/passes/07_MappedWorld/README.md) still
  provides editable constraints on a fixed mixed-resolution showcase; its
  portable import/export UI and general layout editing are separate remaining
  ecosystem work, not implied by the core API.

Executable conformance is in `test/wfc_mapped_passes_test.lpr`,
`test/wfc_lattice_test.lpr`, and `test/wfc_pass_bridge_layout_test.lpr`. All are
included in the standard native gate and portable browser test discovery.
The mapped suite contains a literal world-intersection oracle, the inset
terrain/foliage/housing scenario above, interior-obstacle failure and repair,
wrapped unique counts, transaction rollback, and backward-compatibility guards.
