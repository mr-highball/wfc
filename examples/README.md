# examples

The examples are a mixture of runnable demonstrations and early experiments.
This index distinguishes what works now from the ecosystem still described in
the [roadmap](../ROADMAP.md).

| Area | Entry point | Targets | Current proof and dependencies |
| --- | --- | --- | --- |
| Multi-pass 2D world | `2D/01_MultiPassWorld/MultiPassWorld.lpr` | Native FPC, pas2js/Node | Uses the reusable 2D units, solves terrain → biome → foliage atomically, independently validates every cell/relation, and prints matching portable signatures without external dependencies. |
| Interactive browser world | `2D/02_BrowserWorld/BrowserWorld.lpr` | pas2js/browser | Runs the same model and validator in a responsive three-layer canvas UI with seeds, wrapping, cell locks, and an exact headless-browser fixture. |
| Text-rendered 2D world | `text/01_SimpleTiledWorld/SimpleTiledWorld.lpr` | Native FPC, pas2js/Node | Builds and runs from the same Pascal source, prints and accepts an optional replay seed, and needs no external dependency. This is world generation rendered as text, not a text-prediction model. |
| Building-kit console | `3D/01_SimpleBuildingKit/tester.lpr` | Native FPC | Builds without Castle Game Engine, but currently generates and renders a depth-one slice. It does not yet prove vertical 3D constraints. |
| Castle viewer shell | `3D/01_SimpleBuildingKit/castle-demo/` | Native Castle Game Engine | The project shell and assets exist, but its game state does not yet call WFC or render generated building geometry. |
| A-major music experiment | `music/01_simple_A_major/simple_a_major.lpi` | Native Lazarus/LCL | Legacy optional experiment using the SoundShop submodule and SDL2 playback. |
| Learned-riff music experiment | `music/02_simple_song_riffs/simple_song_riffs.lpi` | Native Lazarus/LCL | Legacy optional experiment using manually inferred note adjacency, SoundShop, and SDL2 playback. |

The first HTML/browser UI now exercises the real browser target and document
host. A complete domain/trace inspector, text-prediction demo,
depth-greater-than-one building demonstration, and connected Castle
visualization remain roadmap work.

## dependency-free builds

Create the named `units` and `bin` output directories before compiling. From
the repository root, the native tiled world is:

```text
fpc -B -Mdelphi -Sa -Cr -Co -Ci -Fusrc -FUbuild/examples/text/native/units -FEbuild/examples/text/native/bin examples/text/01_SimpleTiledWorld/SimpleTiledWorld.lpr
```

With a configured pas2js RTL installation, the same source targets Node.js:

```text
pas2js -B -Tnodejs -Mdelphi -Fusrc -FUbuild/examples/text/pas2js/units -FEbuild/examples/text/pas2js/bin examples/text/01_SimpleTiledWorld/SimpleTiledWorld.lpr
node build/examples/text/pas2js/bin/SimpleTiledWorld.js
```

Pass an optional unsigned 32-bit seed as the first argument on either target,
for example `SimpleTiledWorld.exe 3735928559` or
`node SimpleTiledWorld.js 3735928559`.

The multi-pass world uses the same dependency-free pattern:

```text
fpc -B -Mdelphi -Sa -Cr -Co -Ci -Fusrc -Fuexamples/2D/common -FUbuild/examples/2d/native/units -FEbuild/examples/2d/native/bin examples/2D/01_MultiPassWorld/MultiPassWorld.lpr
build/examples/2d/native/bin/MultiPassWorld 0
```

```text
pas2js -B -Tnodejs -Mdelphi -Fusrc -Fuexamples/2D/common -FUbuild/examples/2d/pas2js/units -FEbuild/examples/2d/pas2js/bin examples/2D/01_MultiPassWorld/MultiPassWorld.lpr
node build/examples/2d/pas2js/bin/MultiPassWorld.js 0
```

The browser host has dedicated entry points that compile its Pascal program
and stage a complete static site beneath `build/browser/world2d/www`:

```text
bash ./build-browser.sh
```

```text
.\build-browser.ps1
```

Serve that directory locally and append `?selftest=1` for the deterministic
headless-browser contract. See the [browser example](2D/02_BrowserWorld/README.md)
and [build documentation](../docs/building.md) for compiler overrides, controls,
and expected state.

The native building-kit console additionally needs its unit directory:

```text
fpc -B -Mdelphi -Sa -Cr -Co -Ci -Fusrc -Fuexamples/3D/01_SimpleBuildingKit/castle-demo/code -FUbuild/examples/3d/native/units -FEbuild/examples/3d/native/bin examples/3D/01_SimpleBuildingKit/tester.lpr
```

These commands keep new compiler output under the ignored `build` tree.

## optional legacy music experiments

Initialize their playback dependency only when working on these examples:

```text
git submodule update --init --recursive examples/music/SoundShop
```

Then build their Lazarus projects with `lazbuild` or open the `.lpi` files in
Lazarus:

```text
lazbuild -B --no-write-project examples/music/01_simple_A_major/simple_a_major.lpi
lazbuild -B --no-write-project examples/music/02_simple_song_riffs/simple_song_riffs.lpi
```

Both require an SDL2 shared library at runtime. SoundShop is GPL-3.0, so this
playback integration remains optional and separate from the dependency-free
MIT path. A standard ecosystem demo must replace it or clearly preserve that
license boundary.

## Castle shell

The Castle project may be compiled from its own directory with Castle Game
Engine's editor or `castle-engine compile`, or through its Lazarus project when
the Castle packages are registered. It is retained as a viewer starting point,
not presented as a working WFC demonstration yet. Asset provenance and the
generated-geometry connection remain roadmap work.
