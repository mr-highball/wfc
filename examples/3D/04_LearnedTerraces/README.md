# Learned Terraces

An FPC/pas2js volume study: learned terrain → socket/support structure →
spatially constrained foliage. No additional runtime libraries are required.

## Native SVG

The normal native gate builds and runs `LearnedTerraces` and writes
`build/native/bin/terraces3d.svg`.

```powershell
.\build.ps1
.\build\native\bin\LearnedTerraces.exe 55 terraces.svg 8 6 5
```

```bash
bash ./build.sh
./build/native/bin/LearnedTerraces 55 terraces.svg 8 6 5
```

Arguments are `[seed] [output.svg] [width height depth]`. The default is
seed 0 and 6 × 5 × 5. The named SVG output is overwritten on success.

## Browser demo

```powershell
.\build-browser-terraces.ps1
.\build\native\bin\wfc_serve.exe --root build/browser/terraces/www --port 4197
```

```bash
bash ./build-browser-terraces.sh
./build/native/bin/wfc_serve --root build/browser/terraces/www --port 4197
```

Open [Learned Terraces](http://127.0.0.1:4197/). Both staging scripts accept
the same compiler selection conventions as the other demos
(`-Compiler` or `PAS2JS`). Build the included FPC server with the native gate
first. No package manager, external server runtime, engine, or media library
is involved.

Change the seed or dimensions and press Generate for a new full baseline.
Cell constraints are zero-based and apply only after pressing Apply.
Select a value matching the chosen layer. Clear removes that cell's user edit.
Repair from the earliest layer permitted to change: terrain includes both
descendants, structure includes foliage, and foliage-only preserves both
providers. A full Generate also retains cell edits in the current session;
New session clears them.

Preview and download are invalidated whenever session inputs, search budgets,
or applied cell constraints change. A contradiction publishes nothing;
clear the conflicting edit or enlarge the repair scope. Rotation changes
presentation only. Both preview and download use the same deterministic SVG
as the native host.

Append `?selftest=1` to run the real-page checks. The maintained browser gate
requires the exact scene/view goldens and checks selective preservation,
edit invalidation, contradiction recovery, and seed → new session → Generate.
It tests the actual staged HTML, CSS, and pas2js program.

Read the [library contract](../../../docs/learned-terraces3d.md) for corpus
construction, ownership, protected hooks, model-to-voxel maps, and resource
limits. The plant blocks are display glyphs, not imported meshes or a
biological/engineering simulation.
