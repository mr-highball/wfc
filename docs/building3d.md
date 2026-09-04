# Building 3D

Building 3D v1 is a dependency-free, depth-aware building pipeline for FPC
and pas2js. It composes the renderer-neutral voxel foundation through one
transactional four-pass graph:

```text
footprint -> structure -> envelope/roof -> props
```

The implementation is project-owned portable Pascal. Runtime code uses only
repository units and the applicable standard RTL; no engine, mesh library,
scene format, JSON package, or media dependency defines the model.

## The four passes

Every pass has the same width, height, and depth. A footprint is therefore a
full 3D massing field, not an implicit 2D layer that later passes somehow read
at `z=0`. `TBuilding3DBlueprint` stores one typed role per cell in X-fastest,
then Y, then Z order and defaults every cell to `b3frVoid`.

The standard roles are:

- void;
- ground shell;
- ordinary shell;
- interior air;
- a reachable feature cell;
- a lintel;
- a roof cell; and
- north-, east-, south-, or west-facing entrances.

Applying a blueprint turns every role into an exact caller-owned domain in the
footprint pass. `AllowFootprintRoles` may instead assign several alternatives
to a cell, while `ClearFootprintDomain` leaves that cell to the footprint
model. This means a blueprint can be wholly authored, partially generated, or
eventually supplied by a separate learner without changing the downstream
contract.

Structure consumes only those public roles. It produces distinct void,
interior, and required-reachable feature air; a supporting ground shell;
weighted wall/window choices; a rotation-checked door; a spanning lintel; and
a roof-span module. Elevated wall and window variants require immediate
support and provide support to the cell above. Doors use north/south or
east/west yaw variants according to the entrance role.

Envelope/roof is a semantic overlay projected from the committed structure.
It maps air to none, foundation/wall/lintel to facade, windows and doors to
their trim, and roof spans to roof finish. Props then consume the committed
footprint, structure, and envelope results. Ordinary cells must be empty; a
feature cell must receive either a lamp or plant and must still be feature air
with no same-cell envelope.

All four passes use `gpmOverlay`. They retain isolated values, rules, domains,
and output. `TryGenerate` stages every result and commits only when all four
passes solve. `TryRegenerateFrom` reuses unaffected ancestors and recomputes
the selected pass plus its transitive consumers.

## Private voxel keys and the pass bridge

Voxel adapters deliberately use deterministic but private graph keys. Domain
code should never parse, store, print, or expose them. The
`wfc_voxel3d_passes` bridge translates public prototype selectors into those
keys only while installing checked pass requirements.

A selector is a prototype identifier plus a finite yaw set. A projection rule
maps selected target variants to selected source variants at the same cell.
Projection maps are complete and unambiguous:

- every target variant must occur in exactly one rule;
- every selector must resolve to at least one actual variant;
- duplicate source selection within one rule is rejected; and
- kits, adapters, graph roots, pass identities, and dependency cycles are
  checked before mutation.

Call `ValidateVoxel3DProjectionFromPass` to preflight a map without changing
the graph. `RequireVoxel3DProjectionFromPass` performs the same complete
preflight and then installs it.

Spatial clauses extend the bridge beyond same-cell projection. Each term is an
exact signed offset plus a finite source-variant set. Terms within one clause
are OR alternatives; separate clauses remain AND constraints. World-frame
offsets keep their axes. Target-yaw offsets rotate with each selected target
variant:

| Yaw | `(dx, dy, dz)` becomes |
| --- | --- |
| 0 | `(dx, dy, dz)` |
| 90 | `(dy, -dx, dz)` |
| 180 | `(-dx, -dy, dz)` |
| 270 | `(-dy, dx, dz)` |

Rotation rejects the one signed-integer value that cannot be negated. The
complete expanded clause set is built before the first requirement is added.
This bridge is reusable by future modular interiors, multi-floor circulation,
facade attachments, utilities, and load-state models; it is not specific to
the built-in building kit.

## Creating and solving a building

The following small fixture has a south entrance, a supported upper wall, and
a roof. Real callers normally fill every intended massing cell, leaving the
remaining blueprint cells at their default void role.

```pascal
uses
  SysUtils,
  wfc,
  wfc_building3d,
  wfc_building3d_validate;

var
  Blueprint: TBuilding3DBlueprint;
  Building: TBuilding3D;
  Config: TBuilding3DConfig;
  Report: TGraphSolveReport;
  Validation: TBuilding3DValidationReport;
begin
  Blueprint := TBuilding3DBlueprint.Create(3, 2, 3);
  try
    Blueprint.SetRole(1, 0, 0, b3frEntranceSouth);
    Blueprint.SetRole(1, 0, 1, b3frLintel);
    Blueprint.SetRole(1, 0, 2, b3frRoof);
    Blueprint.SetRole(1, 1, 0, b3frFeature);
    Blueprint.SetRole(1, 1, 1, b3frInterior);
    Blueprint.SetRole(1, 1, 2, b3frRoof);

    Config := DefaultBuilding3DConfig;
    Config.Seed := 42;
    Building := TBuilding3D.Create(Blueprint, Config);
    try
      if not Building.TryGenerate(Report) then
        raise Exception.CreateFmt('failed in pass %d',
          [Report.FailedPassIndex]);
      if not ValidateBuilding3D(Building, Validation) then
        raise Exception.Create(
          DescribeBuilding3DValidationIssue(Validation.Issue));
      WriteLn(Building.PipelineSignature);
    finally
      Building.Free;
    end;
  finally
    Blueprint.Free;
  end;
end;
```

This abbreviated fixture is useful for API shape, but default validation also
requires a boundary-facing entrance and all required feature cells to be
reachable. The complete standard fixture is in
`examples/3D/02_MultiPassBuilding`.

## Public output and capture

The typed accessors never return private graph keys:

- `FootprintRoleAt` returns `TBuilding3DFootprintRole`;
- `StructureKindAt` returns `TBuilding3DStructureKind`;
- `EnvelopeKindAt` returns `TBuilding3DEnvelopeKind`;
- `PropKindAt` returns `TBuilding3DPropKind`;
- `StageTokenAt` returns the public role or prototype identifier; and
- `StageRotationAt` returns the expanded voxel yaw.

`CaptureStructureScene`, `CaptureEnvelopeScene`, and `CapturePropsScene`
produce immutable `TVoxel3DScene` snapshots. Captured scenes deep-copy their
variant tables and cell indices, so they remain valid after the building,
graph, adapters, and kits are destroyed. The caller owns and frees each
returned scene.

The structure scene can be passed directly to `BuildVoxel3DMesh`. The current
envelope layer is semantic surface classification rather than displaced
geometry, and the prop kit uses unit solid markers. A later renderer may
interpret those layers without placing an engine type in the canonical API.

`PipelineSignature` hashes model, blueprint, pass-bridge, solver, graph, and
pipeline versions; dimensions and topology; all three kit identities; and
every public token/rotation in canonical traversal order. It is portable
between native FPC and pas2js. The seed is an input to generation, while the
signature describes the resulting public building.

## Independent validation

Solver success proves the installed graph constraints. It is not used as a
substitute for domain validation. `ValidateBuilding3D` independently checks:

- the complete structure-to-footprint relation, including door yaw;
- the exact envelope classification for every structure kind;
- feature-only prop placement and its required air/envelope state;
- all structure socket relations and immediate support;
- the presence and boundary orientation of an entrance by default; and
- reachability of every structure variant marked as required.

The first issue includes a stable kind, stage, coordinate, expected public
semantics, actual public semantics, and the underlying voxel issue when
applicable. `TBuilding3DValidationOptions.Structure` exposes the voxel
validator options; `RequireFeature` can additionally require at least one
feature/prop site.

## Ownership and mutation

`TBuilding3D` owns its root graph, three built-in kits, and their adapters.
Callers must not free objects returned by the `Graph`, `StructureKit`,
`EnvelopeKit`, or `PropsKit` properties. The building destroys adapters before
the graph and then destroys the kits. Blueprints are copied into pass domains
and may be freed immediately after construction or `ApplyBlueprint`.

The public root graph is an advanced escape hatch retained for composition and
inspection. Direct mutation can bypass the building's solved-state tracking;
canonical callers should prefer typed domain methods and should validate
before capture, rendering, serialization, or publication.
`DefinitionMatchesPipeline` checks the exact four pass labels, overlay modes,
dependency sets, footprint registry, adapter bindings, and local voxel
definitions. Validation, canonical captures, and signatures reject an extra
or modified pass definition.

Changing a footprint domain invalidates the high-level readable state but
retains the last committed transaction so
`TryRegenerateFrom(b3sFootprint, ...)` can recompute the complete dependent
closure. Regenerating from a later stage is rejected while the footprint is
dirty. A full `TryGenerate` is also valid. Changing only the seed leaves the
committed building readable; call `TryRegenerateFrom` to intentionally produce
a new selected closure.

## Current limits

Version 1 support is immediate and local. Roof spans and lintels are explicit
spanning module categories; the library does not yet claim beam sizing,
cumulative load, material stress, or arbitrary cantilever analysis. Those can
be introduced as state-expanded structural kits and exact spatial clauses
without changing core WFC.

Wrapped buildings are supported by the underlying graph, but the default
entrance validator requires a bounded external face. Boundary-relative
attachments should use explicit padding or roles rather than assuming a
missing neighbor is a value: an unresolved bounded offset does not satisfy a
spatial clause.

The v1 demo is a native and pas2js/Node textual/mesh proof. A graphical browser
viewer and optional engine adapter remain separate presentation milestones;
neither is required to generate, validate, capture, sign, or mesh a building.
