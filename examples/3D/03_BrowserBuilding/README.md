# Building 3D graphical workbench

This example is the standard graphical presentation of the shared
`7 x 5 x 3` Building 3D showcase. It does not contain a second generator. The
native SVG executable and the pas2js browser workbench both use the same
project-owned Pascal path:

```text
footprint -> structure -> envelope/roof -> props
        Building 3D view -> fixed-integer isometric commands
                           -> SVG or Canvas2D
```

The generation, independent validation, view construction, camera projection,
painter ordering, hit testing, signatures, and SVG encoding are repository
units. The native path needs only the standard FPC RTL. The browser edge uses
the pas2js browser RTL and the platform Canvas2D API; it does not introduce a
JavaScript framework or rendering library.

## View and lineage contract

`BuildBuilding3DView` creates an immutable, renderer-neutral snapshot with one
of four modes:

- `footprint` shows the typed massing roles;
- `structure` shows the exact exposed structure mesh;
- `envelope-roof` styles those same structure faces from the same-cell
  envelope/roof pass; and
- `complete` appends the exact exposed prop mesh to the envelope-styled
  structure.

Envelope/roof is semantic classification, not a second solid. The presentation
therefore does not duplicate a structure face to invent an envelope mesh.
Every face retains public cell coordinates and the footprint, structure,
envelope/roof, and prop lineage for that cell. Prototype, material, semantic,
rotation, and layer metadata are public tokens; private graph keys do not cross
the view boundary.

The seed-zero fixture is a checked replay contract:

| Result | Exact value |
| --- | --- |
| Pipeline signature | `1:F1EF0EB6` |
| Complete yaw-zero view signature | `AC7290C0` |
| Footprint faces | `110` |
| Structure faces | `134` |
| Envelope/roof faces | `134` |
| Complete faces | `140` |

The view projector uses signed fixed-subcell 3D coordinates, integer screen
steps, four quarter-turn yaws, checked arithmetic, explicit stable
far-to-near sorting, and reverse-painter hit testing. It uses no floating
point, trigonometry, 64-bit integer dependency, or host-provided sort.

## Native deterministic SVG

The normal native gate compiles the presentation tests and the SVG executable,
then writes the seed-zero artifact beneath the ignored build tree:

```powershell
.\build.ps1
```

```bash
./build.sh
```

To build only the SVG example from the repository root, first create the
output directories, then run:

```text
fpc -B -Mdelphi -Sa -Cr -Co -Ci -Fusrc -Fuexamples/3D/common -FUbuild/examples/building3d-svg/units -FEbuild/examples/building3d-svg/bin examples/3D/03_BrowserBuilding/Building3DSvg.lpr
build/examples/building3d-svg/bin/Building3DSvg 0 build/examples/building3d-svg/seed-zero.svg
```

Use `Building3DSvg.exe` for the executable name on Windows. Its arguments are
an optional unsigned 32-bit seed followed by an optional output path; without
them it uses the showcase default seed and `building3d.svg`. The encoder emits
canonical LF-terminated SVG with the view signature and, by default, public
cell/layer/prototype/material/semantic metadata on each polygon.

This is a deterministic vector artifact, not an interactive native window.
An optional native engine or window-system adapter may consume the same
commands later, but it remains outside the dependency-free standard path.

## Interactive browser workbench

Compile the Pascal browser entry point and stage the complete static site with
one of the dedicated scripts:

```powershell
.\build-browser-building3d.ps1
```

```bash
bash ./build-browser-building3d.sh
```

Set `PAS2JS` or pass PowerShell's `-Compiler` option to select a compiler. The
site is staged in `build/browser/building3d/www`; generated JavaScript and
compiler units stay under `build/`. Serve that directory rather than opening
the page directly, for example:

```text
build/native/bin/wfc_serve --root build/browser/building3d/www --port 8080
```

Build the server with the native gate first; use `wfc_serve.exe` on Windows.
See [development tools](../../../docs/development-tools.md) for its loopback-only serving boundary.

Open `http://localhost:8080/`. The workbench can:

- generate any unsigned 32-bit seed or advance to the next seed;
- regenerate from structure, envelope/roof, or props while preserving the
  pipeline's transactional dependency behavior;
- switch among all four view modes;
- rotate through the four yaws with buttons or arrow keys;
- clip the visible Z range;
- select a painted face on the canvas; and
- inspect validation, solve counts, signatures, face metadata, and all four
  public pass values at the selected cell.

Append `?selftest=1` for the deterministic browser fixture. It generates seed
zero, checks the exact pipeline, confirms reverse-painter hit testing and a
nontransparent Canvas2D result, proves that one quarter turn changes the view
signature and four restore it, and checks structure/complete mode recovery.
The completed document contract is:

```text
data-state="solved"
data-self-test="passed"
data-signature="1:F1EF0EB6"
data-view-signature="AC7290C0"
data-face-count="140"
```

The signed command list is deterministic across native FPC and pas2js.
Canvas2D pixels are not claimed byte-identical across browsers because raster
antialiasing is platform behavior.

## Deliberate limits

This first projector has four fixed yaws and an isometric camera, not arbitrary
camera matrices. Its stable painter list is appropriate for the current
nonintersecting, axis-aligned voxel surfaces; arbitrary intersecting geometry
will need a later generalized visibility/depth renderer. There is currently no
dependency-free interactive native window. These limits do not affect the
generation, validation, immutable view, portable view signature, hit-test, or
canonical SVG contracts.
