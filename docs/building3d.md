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

The structure scene can be passed directly to `BuildVoxel3DMesh`. The
envelope layer is semantic surface classification rather than displaced
geometry, and the prop kit uses unit solid markers. The project-owned Building
view interprets those layers without placing an engine type in the canonical
API.

`PipelineSignature` hashes model, blueprint, pass-bridge, solver, graph, and
pipeline versions; dimensions and topology; all three kit identities; and
every public token/rotation in canonical traversal order. It is portable
between native FPC and pas2js. The seed is an input to generation, while the
signature describes the resulting public building.

## Immutable Building views and lineage

`wfc_building3d_view` is the presentation bridge between a solved building and
any renderer. `BuildBuilding3DView` returns a caller-owned immutable
`TBuilding3DView` in one of four modes:

| Mode | Geometry and style |
| --- | --- |
| `b3vmFootprint` | Exposed unit faces for non-void massing roles, styled by footprint semantics. |
| `b3vmStructure` | The exact exposed structure mesh with structure material and prototype semantics. |
| `b3vmEnvelopeRoof` | The same structure geometry, styled from the same-cell envelope/roof classification. |
| `b3vmComplete` | The envelope-styled structure followed by the exact exposed prop mesh. |

Envelope/roof does not fabricate a duplicate mesh: it is a semantic overlay on
the matching structure face. Complete mode retains the structure prototype on
those faces, identifies the `envelope-roof` layer, and derives material and
semantic presentation from the envelope classification. Prop faces are exact
mesh faces in the `props` layer.

Each `TBuilding3DViewFace` contains its fixed-subcell quad, geometry kind, and
`TBuilding3DViewLineage`. The lineage is a complete public same-cell account
of footprint role/token plus structure, envelope/roof, and prop kind,
prototype, material, and rotation. It deliberately contains no graph adapter
key. The snapshot records the source seed, dimensions, building signature,
pipeline signature, and mode; `CopyFaces` and `CopyQuads` return detached
arrays. It remains readable if the source building is later changed or freed.

The shared seed-zero showcase fixes the following observable results:

| View result | Exact value |
| --- | --- |
| Pipeline signature | `1:F1EF0EB6` |
| Footprint faces | `110` |
| Structure faces | `134` |
| Envelope/roof faces | `134` |
| Complete faces | `140` |

These counts prove that the envelope is not duplicated and that complete mode
adds the showcase's six exposed prop faces.

## Fixed-integer projection and graphical hosts

`TBuilding3DView.CopyQuads` is accepted directly by
`ProjectVoxel3DIsometric`. The projector uses signed fixed-subcell world
coordinates where `1024` units equal one building cell. Camera options are
integer horizontal, plan-vertical, and elevation steps plus a margin and one
of four quarter-turn yaws. Checked arithmetic rejects unrepresentable input.
The immutable result contains auto-fitted integer bounds, a complete portable
view signature, a deep-copied stable far-to-near painter list, and
reverse-painter convex hit testing.

The implementation uses explicit stable sorting rather than a host sort, and
does not depend on floating point, trigonometry, or 64-bit integer arithmetic.
The projected metadata remains public: cell, direction, rotation, layer,
prototype, material, and semantic fields are preserved for SVG, Canvas2D,
inspection, or a future adapter.

`examples/3D/03_BrowserBuilding` exposes two thin hosts over these same
commands:

- `Building3DSvg.lpr` is a native FPC executable that writes canonical,
  LF-terminated SVG with its view signature and optional public polygon
  metadata. It is deterministic and uses only repository units plus the
  standard RTL.
- `BrowserBuilding.lpr` is a pas2js workbench that paints the command list with
  Canvas2D. It supports arbitrary unsigned 32-bit seeds, descendant
  regeneration, all four presentation modes, the four yaws, Z clipping,
  painted-face selection, validation and solve summaries, and public four-pass
  cell lineage.

The normal `build.ps1` and `build.sh` gates compile the three presentation
conformance suites, build the native SVG host, and smoke-test its seed-zero
artifact. The browser site is staged separately:

```powershell
.\build-browser-building3d.ps1
```

```bash
bash ./build-browser-building3d.sh
```

Serve `build/browser/building3d/www` and append `?selftest=1`. A passing
browser fixture finishes with pipeline signature `1:F1EF0EB6`, yaw-zero
complete-view signature `AC7290C0`, and `140` faces. The in-page test also
checks command hit testing, a nontransparent Canvas2D result, quarter-turn
signature change, four-turn exact recovery, and structure/complete mode
recovery. The view-command signature is portable; byte-identical Canvas2D
pixels are not claimed because browser raster antialiasing is platform
behavior. See the [graphical example guide](../examples/3D/03_BrowserBuilding/README.md)
for the focused native command and complete DOM fixture.

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

The standard v1 examples now include native and pas2js/Node textual/mesh
proofs, deterministic native SVG, and an interactive pas2js/Canvas2D
workbench. There is no standard-RTL interactive native window. Engine or
window-system integrations remain optional edge adapters and must consume the
public command model without leaking dependencies into generation,
validation, view construction, signatures, or canonical artifacts.

The isometric projector intentionally supports four fixed yaws rather than
arbitrary camera transforms. Its stable painter ordering is suitable for the
current nonintersecting axis-aligned voxel surfaces; arbitrary intersecting
geometry needs a later generalized visibility/depth renderer. These limits do
not change the exact Building view, lineage, SVG, hit-test, or replay
contracts.
