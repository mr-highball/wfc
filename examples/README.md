# examples

The examples are a mixture of runnable demonstrations and early experiments.
This index distinguishes what works now from the ecosystem still described in
the [roadmap](../ROADMAP.md).

| Area | Entry point | Targets | Current proof and dependencies |
| --- | --- | --- | --- |
| Spatial pass constraints | `passes/01_SpatialDependencies/SpatialDependencies.lpr` and `SpatialDependenciesNode.lpr` | Native FPC, pas2js/Node | Solves terrain before settlement and foliage, checks exact-offset AND clauses plus finite any-neighbor OR clauses, contrasts bounded and wrapped edges, rejects an out-of-bounds probe, and replays portable signatures using only repository units and the standard RTL. |
| Multi-pass 2D world | `2D/01_MultiPassWorld/MultiPassWorld.lpr` | Native FPC, pas2js/Node | Uses the reusable 2D units, solves terrain → biome → foliage atomically, independently validates every cell/relation, and prints matching portable signatures without external dependencies. |
| Interactive browser world | `2D/02_BrowserWorld/BrowserWorld.lpr` | pas2js/browser | Runs the same model and validator in a responsive three-layer canvas UI with seeds, wrapping, cell locks, and an exact headless-browser fixture. |
| Selective settlement | `2D/03_SelectiveSettlement/SelectiveSettlement.lpr` | Native FPC, pas2js/Node | Solves a six-layer dependency DAG, edits hydrology, regenerates only its dependent closure, independently validates the result, proves rollback and exact recovery, and uses no external dependency. |
| Learned tiles | `learning/01_LearnTiles/LearnTiles.lpr` | Native FPC, pas2js/Node | Learns weighted cardinal constraints from a tokenized sample, serializes the immutable model canonically, generates a seeded grid, and independently validates every emitted adjacency without external dependencies. |
| Learned corpus | `learning/02_LearnCorpus/LearnCorpus.lpr` | Native FPC, pas2js/Node | Learns one directed model from two differently sized samples, proves their local wraps and absent cross-sample seams, round-trips canonical `wfcm=2`, and independently validates generated orientation. |
| Overlapping patterns | `learning/03_LearnPatterns/LearnPatterns.lpr` | Native FPC, pas2js/Node | Learns weighted `2x2` structure from heterogeneous grids with D4 augmentation, round-trips strict `wfcp=1`, solves private latent patterns, independently validates every overlap and projected token contribution, and prints a portable signature without external dependencies. |
| Learned sequence | `sequence/01_LearnSequence/LearnSequence.lpr` | Native FPC, pas2js/Node | Learns bounded order-2 latent states from a pretokenized UTF-8 corpus, round-trips strict `wfcs=1`, constrains public projection, solves and independently validates the path, and exposes no private graph key or external dependency. |
| Pass-composed music | `music/03_PassComposition/PassComposition.lpr` | Native FPC, pas2js/Node | Solves harmony and rhythm before a jointly constrained melody, rebuilds an exact score, round-trips strict `wfcmusic=1` and project-owned SMF bytes, and needs no playback or external dependency. |
| Text-rendered 2D world | `text/01_SimpleTiledWorld/SimpleTiledWorld.lpr` | Native FPC, pas2js/Node | Builds and runs from the same Pascal source, prints and accepts an optional replay seed, and needs no external dependency. This is world generation rendered as text, not a text-prediction model. |
| Voxel 3D foundation | `../test/wfc_voxel3d_test.lpr` (library conformance; standard demo next) | Native FPC, pas2js/Node | Proves deterministic yaw variants, exact six-face sockets, vertical support, captured scene signatures, independent entrance/connectivity validation, wrapped seams, and renderer-neutral integer meshes using only repository units and the standard RTL. |
| Building-kit console | `3D/01_SimpleBuildingKit/tester.lpr` | Native FPC | Builds without Castle Game Engine, but its current fixture reaches a no-valid-value failure before rendering. It does not yet prove vertical 3D constraints. |
| Castle viewer shell | `3D/01_SimpleBuildingKit/castle-demo/` | Native Castle Game Engine | The project shell and assets exist, but its game state does not yet call WFC or render generated building geometry. |
| A-major music experiment | `music/01_simple_A_major/simple_a_major.lpi` | Native Lazarus/LCL | Legacy optional experiment using the SoundShop submodule and SDL2 playback. |
| Learned-riff music experiment | `music/02_simple_song_riffs/simple_song_riffs.lpi` | Native Lazarus/LCL | Legacy optional experiment using manually inferred note adjacency, SoundShop, and SDL2 playback. |

The first HTML/browser UI now exercises the real browser target and document
host. A complete domain/trace inspector, text-prediction demo, standard
depth-greater-than-one voxel building demonstration, and connected Castle
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

The selective-settlement example exercises branching, joins, named cross-pass
constraints, and descendant-only regeneration:

```text
fpc -B -Mdelphi -Sa -Cr -Co -Ci -Fusrc -Fuexamples/2D/common -FUbuild/examples/settlement/native/units -FEbuild/examples/settlement/native/bin examples/2D/03_SelectiveSettlement/SelectiveSettlement.lpr
build/examples/settlement/native/bin/SelectiveSettlement 0
```

```text
pas2js -B -Tnodejs -Mdelphi -Fusrc -Fuexamples/2D/common -FUbuild/examples/settlement/pas2js/units -FEbuild/examples/settlement/pas2js/bin examples/2D/03_SelectiveSettlement/SelectiveSettlement.lpr
node build/examples/settlement/pas2js/bin/SelectiveSettlement.js 0
```

See the [selective-settlement guide](2D/03_SelectiveSettlement/README.md) and
the [pass-DAG contract](../docs/pass-dags.md) for its dependency shape,
constraints, transactional edit story, and replay identity.

The spatial-dependency example is the focused Pipeline v2 proof. Native FPC
and pas2js/Node use separate thin hosts over the same Pascal unit:

```text
fpc -B -Mdelphi -Sa -Cr -Co -Ci -Fusrc -FUbuild/examples/spatial/native/units -FEbuild/examples/spatial/native/bin examples/passes/01_SpatialDependencies/SpatialDependencies.lpr
build/examples/spatial/native/bin/SpatialDependencies 0
```

```text
pas2js -B -Tnodejs -Mdelphi -Fusrc -FUbuild/examples/spatial/pas2js/units -FEbuild/examples/spatial/pas2js/bin examples/passes/01_SpatialDependencies/SpatialDependenciesNode.lpr
node build/examples/spatial/pas2js/bin/SpatialDependenciesNode.js 0
```

See the [spatial-dependency guide](passes/01_SpatialDependencies/README.md)
for its clauses, boundary proof, self-check, and golden output.

The learned-tiles example exercises training, canonical model I/O, graph
adaptation, weighted solving, and independent output validation from one
portable source:

```text
fpc -B -Mdelphi -Sa -Cr -Co -Ci -Fusrc -FUbuild/examples/learning/native/units -FEbuild/examples/learning/native/bin examples/learning/01_LearnTiles/LearnTiles.lpr
build/examples/learning/native/bin/LearnTiles 0
```

```text
pas2js -B -Tnodejs -Mdelphi -Fusrc -FUbuild/examples/learning/pas2js/units -FEbuild/examples/learning/pas2js/bin examples/learning/01_LearnTiles/LearnTiles.lpr
node build/examples/learning/pas2js/bin/LearnTiles.js 0
```

Its optional first argument is the unsigned 32-bit replay seed. See the
[learned-tiles guide](learning/01_LearnTiles/README.md) for the sample,
invariants, output, and current radius-one limitation.

The corpus example exercises ordered heterogeneous shapes and the version-2
model profile from the same two targets:

```text
fpc -B -Mdelphi -Sa -Cr -Co -Ci -Fusrc -FUbuild/examples/learning-corpus/native/units -FEbuild/examples/learning-corpus/native/bin examples/learning/02_LearnCorpus/LearnCorpus.lpr
build/examples/learning-corpus/native/bin/LearnCorpus 0
```

```text
pas2js -B -Tnodejs -Mdelphi -Fusrc -FUbuild/examples/learning-corpus/pas2js/units -FEbuild/examples/learning-corpus/pas2js/bin examples/learning/02_LearnCorpus/LearnCorpus.lpr
node build/examples/learning-corpus/pas2js/bin/LearnCorpus.js 0
```

See the [corpus-learning guide](learning/02_LearnCorpus/README.md) for the exact
samples, deterministic ordering, no-seam invariant, and expected output.

The overlapping-pattern example preserves multi-cell structure through an
explicit latent assignment and projection boundary:

```text
fpc -B -Mdelphi -Sa -Cr -Co -Ci -Fusrc -FUbuild/examples/pattern/native/units -FEbuild/examples/pattern/native/bin examples/learning/03_LearnPatterns/LearnPatterns.lpr
build/examples/pattern/native/bin/LearnPatterns 0
```

```text
pas2js -B -Tnodejs -Mdelphi -Fusrc -FUbuild/examples/pattern/pas2js/units -FEbuild/examples/pattern/pas2js/bin examples/learning/03_LearnPatterns/LearnPatterns.lpr
node build/examples/pattern/pas2js/bin/LearnPatterns.js 0
```

See the [overlapping-pattern guide](learning/03_LearnPatterns/README.md) and
the [model documentation](../docs/patterns.md) for extraction, compatibility,
projection, replay identity, and the current pass-composition boundary.

The learned-sequence example exercises typed BOS boundaries, raw counts,
structural order-2 recombination, public-token domains, strict canonical text,
and independent latent-path validation:

```text
fpc -B -Mdelphi -Sa -Cr -Co -Ci -Fusrc -FUbuild/examples/sequence/native/units -FEbuild/examples/sequence/native/bin examples/sequence/01_LearnSequence/LearnSequence.lpr
build/examples/sequence/native/bin/LearnSequence 0
```

```text
pas2js -B -Tnodejs -Mdelphi -Fusrc -FUbuild/examples/sequence/pas2js/units -FEbuild/examples/sequence/pas2js/bin examples/sequence/01_LearnSequence/LearnSequence.lpr
node build/examples/sequence/pas2js/bin/LearnSequence.js 0
```

See the [sequence example guide](sequence/01_LearnSequence/README.md) and
[sequence-model documentation](../docs/sequences.md) for the bounded/open
learning contract, derived wrapped-cycle semantics, pass projection, and
versioned replay identity.

The pass-composed music example exercises exact cross-vocabulary maps over
three latent sequence models, score reconstruction, strict `wfcmusic=1`, and
the project-owned format-0 MIDI exporter:

```text
fpc -B -Mdelphi -Sa -Cr -Co -Ci -Fusrc -FUbuild/examples/music/native/units -FEbuild/examples/music/native/bin examples/music/03_PassComposition/PassComposition.lpr
build/examples/music/native/bin/PassComposition 0
```

```text
pas2js -B -Tnodejs -Mdelphi -Fusrc -FUbuild/examples/music/pas2js/units -FEbuild/examples/music/pas2js/bin examples/music/03_PassComposition/PassComposition.lpr
node build/examples/music/pas2js/bin/PassComposition.js 0
```

Create the named output directories first. See the
[pass-composition guide](music/03_PassComposition/README.md) and
[music foundation](../docs/music.md) for the exact cell, score, artifact, and
MIDI contracts. This is a console/Node proof; it does not claim a browser UI or
playback.

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

## isolated legacy music experiments

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
