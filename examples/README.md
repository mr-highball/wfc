# examples

The examples are a mixture of runnable demonstrations and early experiments.
This index distinguishes what works now from the ecosystem still described in
the [roadmap](../ROADMAP.md).

| Area | Entry point | Targets | Current proof and dependencies |
| --- | --- | --- | --- |
| Music Studio | [music/05_MusicStudio](music/05_MusicStudio/README.md) | Native FPC, pas2js/browser | Original training phrases, public locks, selective negotiated repair, piano roll, project-owned PCM/WAV synthesis, user-initiated browser playback, and exact score/MIDI/WAV exports without a third-party runtime. |
| Ensemble Studio | [music/06_EnsembleStudio](music/06_EnsembleStudio/README.md) | Native FPC, pas2js/browser | Synchronized held bass, chord accompaniment, and upper voice; common-excerpt training; harmony/rhythm constraints, voice locks and selective repair; user-defined score length; exact score/MIDI export and bounded PCM preview. |
| Spatial pass constraints | `passes/01_SpatialDependencies/SpatialDependencies.lpr` | Native FPC | Solves terrain before settlement and foliage, checks exact-offset AND clauses plus finite any-neighbor OR clauses, contrasts bounded and wrapped edges, rejects an out-of-bounds probe, and replays portable signatures using only repository units and the standard RTL. |
| Neighborhood counts | [passes/04_NeighborhoodCounts](passes/04_NeighborhoodCounts/README.md) | Native FPC, pas2js/browser | Composes terrain and roads into a local market probe with lower/upper count ranges, explicit matching-offset versus distinct-cell modes, flood rejection, bounded selective road repair, and stale-output invalidation. |
| Connected routes | [passes/06_ConnectedRoutes](passes/06_ConnectedRoutes/README.md) | Native FPC/SVG, pas2js/browser | Uses solver-propagated reciprocal ports for terrain → roads → housing and multi-floor circulation, with independent BFS, alternative crossings, upstream-preserving repair, and explicit optional-component semantics. |
| Deterministic restarts | [passes/05_DeterministicRestarts](passes/05_DeterministicRestarts/README.md) | Native FPC; shared browser conformance | Checks complete ordinary and negotiated attempts, effective seeds, fixed/capped-doubling budgets, rollback, independently sampled outputs, and replay transcripts. Optional timing is diagnostic only. |
| Causal trace inspector | `passes/02_TraceInspector/TraceInspector.lpr` | Native FPC | Captures and validates a deterministic terrain -> settlement -> foliage transaction, prints its portable trace hash, pass slices, and all 27 events, then follows a rejected foliage candidate back to its settlement provider event. The host emits the same event stream using only repository units and the standard RTL. |
| Bounded pass negotiation | `passes/03_PassNegotiation/PassNegotiation.lpr` | Native FPC | Proves ordinary one-way staging fails for `marsh`, then excludes that exact provider assignment and reopens terrain to commit `meadow|cottage` in two deterministic rounds. The host enforces the same counters and transcript using only repository units and the standard RTL. |
| Multi-pass 2D world | `2D/01_MultiPassWorld/MultiPassWorld.lpr` | Native FPC | Uses the reusable 2D units, solves terrain → biome → foliage atomically, independently validates every cell/relation, and prints matching portable signatures without external dependencies. |
| Interactive browser world | `2D/02_BrowserWorld/BrowserWorld.lpr` | pas2js/browser | Runs the same model and validator in a responsive three-layer canvas UI with seeds, wrapping, cell locks, and an exact headless-browser fixture. |
| Selective settlement | `2D/03_SelectiveSettlement/SelectiveSettlement.lpr` | Native FPC | Solves a six-layer dependency DAG, edits hydrology, regenerates only its dependent closure, independently validates the result, proves rollback and exact recovery, and uses no external dependency. |
| Negotiated descendant repair | `2D/04_NegotiatedRepair/NegotiatedRepair.lpr` | Native FPC | Contrasts a too-narrow housing repair horizon with a successful roads-root horizon, excludes one exact roads assignment, verifies immutable provider values and random streams, and replays separately versioned selective and nested transcripts using only repository units and the standard RTL. |
| Learned-pattern world | `2D/05_LearnedPatternWorld/LearnedPatternWorld.lpr` | Native FPC | Learns wrapped `2x2` terrain structure at runtime, materializes private pattern anchors as a public pass, composes foliage and structure, independently validates every contribution, and proves exact downstream rollback/recovery. Its checked `pipeline/` bundle also exercises bridge-v2 inverse lowering from portable public locks. |
| Learned tiles | `learning/01_LearnTiles/LearnTiles.lpr` | Native FPC | Learns weighted cardinal constraints from a tokenized sample, serializes the immutable model canonically, generates a seeded grid, and independently validates every emitted adjacency without external dependencies. |
| Learned corpus | `learning/02_LearnCorpus/LearnCorpus.lpr` | Native FPC | Learns one directed model from two differently sized samples, proves their local wraps and absent cross-sample seams, round-trips canonical `wfcm=2`, and independently validates generated orientation. |
| Editable training documents | [learning/04_TrainingDocuments](learning/04_TrainingDocuments/README.md) | Native FPC | Four pretokenized corpora train through the project-owned CLI into cardinal, pattern, or sequence recipes; exact source/model/recipe/run/result bundles retain declared provenance and replay through real validator/runner processes. |
| Training Studio | [learning/05_TrainingStudio](learning/05_TrainingStudio/README.md) | Native FPC, pas2js/browser | Editable corpora, explicit Unicode-scalar text import, seeded runs, public locks, failure inspection, and artifact exports share one invalidation-aware Pascal workspace; five native presets and an event-path browser fixture validate the workflow. |
| Overlapping patterns | `learning/03_LearnPatterns/LearnPatterns.lpr` | Native FPC | Learns weighted `2x2` structure from heterogeneous grids with D4 augmentation, round-trips strict `wfcp=1`, solves private latent patterns, independently validates every overlap and projected token contribution, and prints a portable signature without external dependencies. |
| Learned sequence | `sequence/01_LearnSequence/LearnSequence.lpr` | Native FPC | Learns bounded order-2 latent states from a pretokenized UTF-8 corpus, round-trips strict `wfcs=1`, constrains public projection, solves and independently validates the path, and exposes no private graph key or external dependency. |
| Text constraint completion | `text/02_ConstraintCompletion/ConstraintCompletion.lpr` | Native FPC | Learns project-authored raw text with the project-owned Unicode-scalar tokenizer, exposes exact feasible domains, composes prefix/suffix/mask/interior locks, distinguishes prefix from whole-sample boundaries, independently validates anchored infill, and replays without an external dependency. |
| Three-pass text composition | `text/03_PassComposition/TextPassComposition.lpr`, and `BrowserTextPassComposition.lpr` | Native FPC, pas2js/browser | Gives structure, lexical choice, and punctuation separate latent owners; constrains the surface from both prior passes; independently validates and renders exact fragments; publishes a private-key-safe trace; and provides an interactive lock/contradiction workbench with an exact browser fixture. Repository units and the standard RTL are the complete runtime path. |
| Pass-composed music | `music/03_PassComposition/PassComposition.lpr` | Native FPC | Solves harmony and rhythm before a jointly constrained melody, rebuilds an exact score, round-trips strict `wfcmusic=1` and project-owned SMF bytes, and needs no playback or external dependency. |
| Negotiated music variation | `music/04_NegotiatedVariation/NegotiatedVariation.lpr` | Native FPC | Preserves a public melody motif, proves an ordinary atomic regeneration failure, reopens an explicit provider horizon through bounded negotiation, validates the immutable composition, and replays strict `wfcmusicpass=1` without exposing latent keys or adding a runtime dependency. |
| Text-rendered 2D world | `text/01_SimpleTiledWorld/SimpleTiledWorld.lpr` | Native FPC | Builds and runs from the same Pascal source, prints and accepts an optional replay seed, and needs no external dependency. This is world generation rendered as text, not a text-prediction model. |
| Voxel 3D foundation | `../test/wfc_voxel3d_test.lpr` | Native FPC | Proves deterministic yaw variants, exact six-face sockets, vertical support, captured scene signatures, independent entrance/connectivity validation, wrapped seams, and renderer-neutral integer meshes using only repository units and the standard RTL. |
| Multi-pass Building 3D | `3D/02_MultiPassBuilding/MultiPassBuilding.lpr` | Native FPC | Runs one depth-three footprint -> structure -> envelope/roof -> props DAG, keeps voxel keys private through checked prototype maps, validates support/entrance/reachability and every cross-layer cell independently, captures an integer mesh, and replays a portable public signature without external dependencies. |
| Graphical Building 3D | `3D/03_BrowserBuilding/Building3DSvg.lpr` and `BrowserBuilding.lpr` | Native FPC/SVG, pas2js/browser | Builds one immutable public-lineage view over the shared four-pass showcase, projects fixed-integer commands through four yaws, writes deterministic SVG, and renders an interactive Canvas2D workbench with picking and an exact seed-zero browser fixture. It uses repository units plus the applicable standard RTL; Canvas2D is only the browser edge. |
| Building-kit console | `3D/01_SimpleBuildingKit/tester.lpr` | Native FPC | Builds without Castle Game Engine, but its current fixture reaches a no-valid-value failure before rendering. It does not yet prove vertical 3D constraints. |
| Castle viewer shell | `3D/01_SimpleBuildingKit/castle-demo/` | Native Castle Game Engine | The project shell and assets exist, but its game state does not yet call WFC or render generated building geometry. |
| A-major music experiment | `music/01_simple_A_major/simple_a_major.lpi` | Native Lazarus/LCL | Legacy optional experiment using the SoundShop submodule and SDL2 playback. |
| Learned-riff music experiment | `music/02_simple_song_riffs/simple_song_riffs.lpi` | Native Lazarus/LCL | Legacy optional experiment using manually inferred note adjacency, SoundShop, and SDL2 playback. |

The 2D field instrument and Building 3D workbench exercise the real browser
target and document host. The causal-trace console inspector now proves
portable event capture, validation, hashing, pass slices, and backward cause
links. The text pass workbench adds public three-layer lineage, locks, and
contradiction/recovery inspection. Interactive trace stepping, live global
domain views, richer failed-clause/minimal-core explanations, streaming
capture, and an arbitrary-corpus completion editor remain roadmap work. The
Castle shell is retained only as an
optional native-engine edge; the standard 3D graphical path no longer waits
on it.

## dependency-free builds

Create the named `units` and `bin` output directories before compiling. From
the repository root, the native tiled world is:

```text
fpc -B -Mdelphi -Sa -Cr -Co -Ci -Fusrc -FUbuild/examples/text/native/units -FEbuild/examples/text/native/bin examples/text/01_SimpleTiledWorld/SimpleTiledWorld.lpr
```

Pass an optional unsigned 32-bit seed as the first argument,
for example `SimpleTiledWorld.exe 3735928559`.

The multi-pass world uses the same dependency-free pattern:

```text
fpc -B -Mdelphi -Sa -Cr -Co -Ci -Fusrc -Fuexamples/2D/common -FUbuild/examples/2d/native/units -FEbuild/examples/2d/native/bin examples/2D/01_MultiPassWorld/MultiPassWorld.lpr
build/examples/2d/native/bin/MultiPassWorld 0
```

The selective-settlement example exercises branching, joins, named cross-pass
constraints, and descendant-only regeneration:

```text
fpc -B -Mdelphi -Sa -Cr -Co -Ci -Fusrc -Fuexamples/2D/common -FUbuild/examples/settlement/native/units -FEbuild/examples/settlement/native/bin examples/2D/03_SelectiveSettlement/SelectiveSettlement.lpr
build/examples/settlement/native/bin/SelectiveSettlement 0
```

See the [selective-settlement guide](2D/03_SelectiveSettlement/README.md) and
the [pass-DAG contract](../docs/pass-dags.md) for its dependency shape,
constraints, transactional edit story, and replay identity.

The negotiated-repair example applies bounded pass negotiation to an explicit
descendant horizon. Its native hosts share one self-checking Pascal
unit:

```text
fpc -B -Mdelphi -Sa -Cr -Co -Ci -Fusrc -Fuexamples/2D/04_NegotiatedRepair -FUbuild/examples/negotiated-repair/native/units -FEbuild/examples/negotiated-repair/native/bin examples/2D/04_NegotiatedRepair/NegotiatedRepair.lpr
build/examples/negotiated-repair/native/bin/NegotiatedRepair
```

The host prints the seed-zero proof: the housing-root horizon fails with
hash `7A595E38`; the roads-root repair has nested negotiation hash `80926222`
and selective transcript hash `E29050A0`; the full-pipeline comparison has
transcript `9DB789E4`. See the
[negotiated-repair guide](2D/04_NegotiatedRepair/README.md) and
[Selective Negotiation v1 contract](../docs/selective-negotiation.md) for the
scope equation, immutable-provider boundary, complete output, and limitations.

The spatial-dependency example is the focused Pipeline v2 proof. A native FPC
host calls the shared Pascal demonstration unit:

```text
fpc -B -Mdelphi -Sa -Cr -Co -Ci -Fusrc -FUbuild/examples/spatial/native/units -FEbuild/examples/spatial/native/bin examples/passes/01_SpatialDependencies/SpatialDependencies.lpr
build/examples/spatial/native/bin/SpatialDependencies 0
```

See the [spatial-dependency guide](passes/01_SpatialDependencies/README.md)
for its clauses, boundary proof, self-check, and golden output.

The causal-trace inspector uses a thin native host over a shared Pascal unit:

```text
fpc -B -Mdelphi -Sa -Cr -Co -Ci -Fusrc -Fuexamples/passes/02_TraceInspector -FUbuild/trace-inspector/native/units -FEbuild/trace-inspector/native/bin examples/passes/02_TraceInspector/TraceInspector.lpr
build/trace-inspector/native/bin/TraceInspector
```

Both produce the 27-event seed-zero trace hash `73C4B9A2`, validate the report,
and print the same backward chain from foliage event `15` to settlement event
`13`. See [causal solve traces](../docs/traces.md) for the event schema,
signature contract, query helpers, and current limits.

The bounded pass-negotiation example makes the one-way failure and repaired
composition executable in the native host:

```text
fpc -B -Mdelphi -Sa -Cr -Co -Ci -Fusrc -Fuexamples/passes/03_PassNegotiation -FUbuild/pass-negotiation/native/units -FEbuild/pass-negotiation/native/bin examples/passes/03_PassNegotiation/PassNegotiation.lpr
build/pass-negotiation/native/bin/PassNegotiation
```

See the [pass-negotiation guide](passes/03_PassNegotiation/README.md) for the
exact excluded assignment, bounded chronological behavior, portable goldens,
and fail-fast self-check.

The [Deterministic Restarts example](passes/05_DeterministicRestarts/README.md)
adds a checked console host over one portable helper. Run
`build/native/bin/RestartPolicies --selftest` after the native gate, or
`--timing` for optional elapsed diagnostics. Its policies keep budget
exhaustion distinct from terminal contradictions; they do not assert that
restarts outperform a larger uninterrupted search.

The standard Building 3D host uses a thin native entry point over the
same depth-three Pascal demonstration unit:

```text
fpc -B -Mdelphi -Sa -Cr -Co -Ci -Fusrc -Fuexamples/3D/common -Fuexamples/3D/02_MultiPassBuilding -FUbuild/examples/building3d/native/units -FEbuild/examples/building3d/native/bin examples/3D/02_MultiPassBuilding/MultiPassBuilding.lpr
build/examples/building3d/native/bin/MultiPassBuilding 0
```

See the [example guide](3D/02_MultiPassBuilding/README.md) and
[Building 3D contract](../docs/building3d.md) for its massing roles, checked
voxel pass maps, validation, mesh extraction, and exact public output.

The graphical Building 3D example presents the same showcase through one
shared immutable command model. The checked native gate compiles its focused
isometric, SVG, and Building-view suites and writes a deterministic seed-zero
SVG under `build/native/bin`:

```text
.\build.ps1
```

```text
./build.sh
```

The interactive pas2js site has dedicated staging scripts:

```text
.\build-browser-building3d.ps1
```

```text
bash ./build-browser-building3d.sh
```

Serve `build/browser/building3d/www` and append `?selftest=1` for the checked
browser contract: seed-zero pipeline `1:F1EF0EB6`, yaw-zero complete view
`AC7290C0`, and `140` faces. The workbench exposes seeds, descendant
regeneration, four presentation modes, four camera yaws, Z clipping, face
picking, and public four-pass lineage. See the
[graphical example guide](3D/03_BrowserBuilding/README.md) for the focused
native SVG command, compiler overrides, controls, and deliberate renderer
limits.

The learned-tiles example exercises training, canonical model I/O, graph
adaptation, weighted solving, and independent output validation from one
portable source:

```text
fpc -B -Mdelphi -Sa -Cr -Co -Ci -Fusrc -FUbuild/examples/learning/native/units -FEbuild/examples/learning/native/bin examples/learning/01_LearnTiles/LearnTiles.lpr
build/examples/learning/native/bin/LearnTiles 0
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

See the [corpus-learning guide](learning/02_LearnCorpus/README.md) for the exact
samples, deterministic ordering, no-seam invariant, and expected output.

The overlapping-pattern example preserves multi-cell structure through an
explicit latent assignment and projection boundary:

```text
fpc -B -Mdelphi -Sa -Cr -Co -Ci -Fusrc -FUbuild/examples/pattern/native/units -FEbuild/examples/pattern/native/bin examples/learning/03_LearnPatterns/LearnPatterns.lpr
build/examples/pattern/native/bin/LearnPatterns 0
```

See the [overlapping-pattern guide](learning/03_LearnPatterns/README.md) and
the [model documentation](../docs/patterns.md) for extraction, compatibility,
projection, replay identity, and pass composition.

The learned-pattern world carries that public projection into a real four-pass
domain pipeline:

```bash
mkdir -p build/examples/pattern-world/native/units build/examples/pattern-world/native/bin
fpc -B -Mdelphi -Sa -Cr -Co -Ci -Fusrc \
  -Fuexamples/2D/05_LearnedPatternWorld \
  -FUbuild/examples/pattern-world/native/units \
  -FEbuild/examples/pattern-world/native/bin \
  examples/2D/05_LearnedPatternWorld/LearnedPatternWorld.lpr
build/examples/pattern-world/native/bin/LearnedPatternWorld 0
```

Seed zero learns `17` patterns and pins canonical bytes `2130`, model hash
`9BF802CC`, terrain hash `EBBC9390`, and pipeline hash `38FE98C4`. See the
[learned-pattern world guide](2D/05_LearnedPatternWorld/README.md) and the
[experiment record](../docs/research/pattern-projected-passes-v1.md). The
[portable pipeline bundle](2D/05_LearnedPatternWorld/pipeline/recipe.wfcpipeline)
reconstructs the same three public layers from a canonical recipe and run using
only repository units and the standard RTL.

The learned-sequence example exercises typed BOS boundaries, raw counts,
structural order-2 recombination, public-token domains, strict canonical text,
and independent latent-path validation:

```text
fpc -B -Mdelphi -Sa -Cr -Co -Ci -Fusrc -FUbuild/examples/sequence/native/units -FEbuild/examples/sequence/native/bin examples/sequence/01_LearnSequence/LearnSequence.lpr
build/examples/sequence/native/bin/LearnSequence 0
```

See the [sequence example guide](sequence/01_LearnSequence/README.md) and
[sequence-model documentation](../docs/sequences.md) for the bounded/open
learning contract, derived wrapped-cycle semantics, pass projection, and
versioned replay identity.

The constraint-completion example learns raw Unicode-scalar text, combines a
prefix, suffix, interior lock, and positional mask, prints the exact feasible
domain, and proves that prefix continuation has different endpoint semantics
from whole-sample generation:

```text
fpc -B -Mdelphi -Sa -Cr -Co -Ci -Fusrc -Fuexamples/text/02_ConstraintCompletion -FUbuild/examples/text-completion/native/units -FEbuild/examples/text-completion/native/bin examples/text/02_ConstraintCompletion/ConstraintCompletion.lpr
build/examples/text-completion/native/bin/ConstraintCompletion 0
```

See the [constraint-completion guide](text/02_ConstraintCompletion/README.md)
and [text foundation](../docs/text.md) for scalar offsets, extent semantics,
domain analysis, independent validation, replay inputs, and the non-LLM scope.

The text pass-composition example uses three aligned sequence owners, exact
cross-vocabulary maps, a versioned fragment surface, and a sanitized causal
trace:

```text
fpc -B -Mdelphi -Sa -Cr -Co -Ci -Fusrc -Fuexamples/text/03_PassComposition -FUbuild/examples/text-passes/native/units -FEbuild/examples/text-passes/native/bin examples/text/03_PassComposition/TextPassComposition.lpr
build/examples/text-passes/native/bin/TextPassComposition 0
```

Seed zero renders `A sun rises brightly!` with portable signature
`1:69ABA6CE`. See the
[text pass-composition guide](text/03_PassComposition/README.md) for the pass
DAG, trace and validation contracts, browser commands, and the explicit
one-way cascade boundary.

The pass-composed music example exercises exact cross-vocabulary maps over
three latent sequence models, score reconstruction, strict `wfcmusic=1`, and
the project-owned format-0 MIDI exporter:

```text
fpc -B -Mdelphi -Sa -Cr -Co -Ci -Fusrc -FUbuild/examples/music/native/units -FEbuild/examples/music/native/bin examples/music/03_PassComposition/PassComposition.lpr
build/examples/music/native/bin/PassComposition 0
```

Create the named output directories first. See the
[pass-composition guide](music/03_PassComposition/README.md) and
[music foundation](../docs/music.md) for the exact cell, score, artifact, and
MIDI contracts. This is a console proof; it does not claim a browser UI or
playback.

The negotiated-variation example uses the reusable owner and an explicit
repair horizon. The native host shares the same fixture unit. Create the
named native output directories first; the linked guide gives both
PowerShell and POSIX commands:

```text
fpc -B -Mdelphi -Sa -Cr -Co -Ci -Fusrc -Fuexamples/music/04_NegotiatedVariation -FUbuild/examples/music-variation/native/units -FEbuild/examples/music-variation/native/bin examples/music/04_NegotiatedVariation/NegotiatedVariation.lpr
build/examples/music-variation/native/bin/NegotiatedVariation 0
```

See the
[negotiated-variation guide](music/04_NegotiatedVariation/README.md) for the
exact failure, lock, bounded-search, replay, and non-optimality contracts.

The text pass workbench has dedicated staging entry points and writes its
complete static site beneath `build/browser/text-passes/www`:

```text
bash ./build-browser-text.sh
```

```text
.\build-browser-text.ps1
```

Serve that directory and append `?selftest=1` for the checked seed-zero text,
three-pass signature, public trace hash, deliberate contradiction, and exact
recovery contract.

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

## Music Studio

The [Music Studio guide](music/05_MusicStudio/README.md) is the entry point
for audible, interactive music generation. Its shared Pascal owner trains
four original short phrases, exposes harmony/rhythm/melody public locks,
preserves motifs through bounded selective negotiation, and independently
validates every current composition. The native host exports score and
composition text, MIDI, and the same project-owned four-second PCM/WAV.
The pas2js browser host adds layer grids, a piano roll, public-token editing,
failure evidence, user-initiated playback, and downloads. Build it with
`build-browser-music.ps1` or `build-browser-music.sh`; serve
`build/browser/music/www` and append `?selftest=1` for the checked workflow.

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
not presented as a working WFC demonstration. The project-owned native SVG
and pas2js/Canvas2D paths are the standard graphical examples. Connecting an
interactive native engine remains an optional adapter task, and the shell's
legacy asset provenance is still unestablished.
