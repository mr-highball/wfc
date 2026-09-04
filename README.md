# WFC

WFC is a constraint-driven generation library written in Pascal for Free
Pascal Compiler (FPC) and pas2js. It models user-defined values on a 2D or 3D
graph, applies directional constraints, and supports dependency-planned passes
whose rules and results remain separate.

The pass system is the larger idea: generate terrain first, then foliage,
roads, housing, or any other layer while constraining each stage from the
result before it. The same approach can be specialized for world generation,
modular 3D structures, music, text, and other discrete design problems.

> **Project status:** the original API and greedy traversal solver remain
> available. The opt-in reference solver now provides fixed-point propagation,
> deterministic weighted Shannon-entropy observation, bounded backtracking,
> explicit deny-all adjacency, caller-owned per-cell domains, structured
> contradiction reports, atomic dependency-DAG pipelines, named cross-pass
> constraints, and selective descendant regeneration on native FPC and
> pas2js. The first specialized ecosystem now includes independently
> validated terrain → biome → foliage and six-layer settlement worlds with
> portable signatures; the former also has an interactive browser presentation
> compiled from the same Pascal model. Deterministic training primitives learn cardinal
> constraints and raw weights from ordered heterogeneous 1D/2D corpora, merge
> compatible models, apply immutable results to a graph, and round-trip them
> through a strict canonical text format. A second learner extracts weighted
> overlapping 2D footprints, compiles exact structural overlap, captures
> latent assignments, and independently validates projected token grids
> through canonical `wfcp=1` artifacts. The sequence foundation learns bounded
> order-N models from pretokenized UTF-8 corpora, derives structural
> suffix/prefix recombination, applies open or derived wrapped graph domains,
> composes latent and public-token passes in either direction, and round-trips
> strict canonical `wfcs=1` artifacts. Music Foundation v1 adds an exact
> integer score IR, canonical melody/rhythm/harmony cells, two-source latent
> pass projection, strict `wfcmusic=1` score text, a project-owned SMF
> format-0/1 codec, and a format-0 score exporter. Its portable three-pass
> example runs on native FPC and pas2js/Node without a playback dependency.
> Pipeline v2 adds exact signed-offset and finite any-of-neighborhood reads
> across staged provider passes. Voxel Foundation v1 adds immutable
> rotation-aware prototypes, explicit six-face socket compatibility, vertical
> support, captured scenes, independent validation, and renderer-neutral
> integer surface meshes. Building passes and viewers remain tracked in the
> [roadmap](ROADMAP.md).

## Features

- 2D and 3D graph topology with optional wrapped boundaries
- fluent rules over caller-defined string values
- compatibility-preserving wildcard rules plus explicit directional deny-all
- caller-owned, pass-local per-cell domains with canonical value ordering
- required directional rules and selection/invalid-state callbacks
- stable, labeled, zero-based passes with isolated values, rules, and outputs
- deterministic dependency-DAG execution with stable creation-index tie breaks
- explicit legacy, overlay, and transform pass modes
- named same-coordinate, exact-offset, and finite any-of-neighborhood
  cross-pass constraints with selective descendant-only regeneration
- transactional full and selective solves with selected-pass restoration
- compatibility-preserving empty-pass copying and previous-pass constraints
- explicit pipeline seeds with stable, independent per-pass random streams
- an opt-in propagating solver with positive relative weights, deterministic
  fixed-point Shannon entropy, exact unit-weight MRV compatibility, and bounded
  backtracking
- atomic staging, independent validation, and structured execution reports
- typed 2D terrain/biome/foliage and selective-settlement libraries with
  independent semantic checkers
- versioned, fixed-token 2D layer signatures shared by native FPC and pas2js
- matching seeded golden fixtures on native FPC and pas2js/Node
- an interactive browser world with synchronized layers, locks, and a seeded
  headless-browser conformance fixture
- immutable, versioned one-layer model data shared by native FPC and pas2js
- deterministic first-seen learning for ordered heterogeneous tokenized 1D/2D
  corpora with independent open or wrapped boundaries and explicit D4
  augmentation
- checked deterministic merging of compatible learned models without inventing
  cross-sample seam relations
- strict canonical `.wfcm` text with UTF-8 percent encoding and byte-exact
  decode/re-encode validation
- deterministic overlapping 2D pattern extraction with heterogeneous corpora,
  open/wrapped sources, square-footprint D4 augmentation, structural
  compatibility, explicit latent-to-token projection, and strict `.wfcp`
  replay artifacts
- deterministic bounded sequence learning with typed BOS history, raw counts,
  order-N latent states, structural suffix/prefix recombination, explicit
  public-token projection, and strict canonical `wfcs=1` artifacts
- immutable exact music scores with complete per-voice timelines and exact
  meter-boundary validation
- strict `wm1` melody, `wr1` rhythm, and `wh1` harmony cells with lossless
  aligned monophonic projection and rebuild
- rhythm + harmony -> melody pass composition through public-token maps over
  private latent sequence states
- strict canonical `wfcmusic=1` score text, a project-owned SMF format-0/1
  byte codec, and deterministic format-0 score export
- [immutable voxel kits](docs/voxel3d.md) with deterministic yaw variants,
  explicit socket relations, support-aware six-direction graph compilation,
  portable scene signatures, independent connectivity validation, and integer
  quad meshes
- iterative traversal without a graph-sized call stack
- extension hooks for custom graph and entry behavior
- one Pascal core for native FPC and pas2js

## Basic use

```pascal
uses
  SysUtils,
  wfc;

var
  Graph: TGraph;
  Options: TGraphSolveOptions;
  Report: TGraphSolveReport;
begin
  Graph := TGraph.Create.Reshape(5, 5, 1);
  try
    Graph.AddValue('A')
      .NewRule(AllDirections, ['A', 'B']);

    Graph.AddValue('C')
      .NewRule([gdNorth, gdSouth], ['A', 'B']);

    Options := DefaultGraphSolveOptions;
    if not Graph.TrySolve(Options, Report) then
      raise Exception.CreateFmt('WFC failed in pass %d',
        [Report.FailedPassIndex]);
  finally
    Graph.Free;
  end;
end;
```

For a complete terrain-to-foliage pipeline, including `SwitchToPass`,
`PassGraph`, and `RequirePrevious`, see [pass-system semantics](docs/passes.md).
For dependency roles, pass modes, `RequireFromPassAt`,
`RequireAnyFromPass`, boundary behavior, topological execution, and selective
regeneration, see [pass DAGs](docs/pass-dags.md).
For the reference algorithm, atomicity contract, reports, and exact constraint
semantics, see the [reference solver](docs/solver.md).
For exact replay behavior, callback requirements, and algorithm versioning,
see [deterministic generation](docs/determinism.md).
For the reusable world model, typed locks, validator, signatures, and console
and browser demonstrations, see the [2D ecosystem](docs/world2d.md).
For learned frequencies and adjacency, immutable model data, graph adaptation,
the `.wfcm` format, and exact replay inputs, see
[model learning and priming](docs/learning.md).
For multi-cell extraction, structural overlap, latent assignment, projection,
independent validation, and the `.wfcp` format, see
[overlapping 2D patterns](docs/patterns.md).
For bounded order-N learning, typed BOS boundaries, open and derived wrapped
generation, pass projection, and canonical `wfcs=1` text, see
[sequence models](docs/sequences.md).
For the exact score model, fixed-quantum cells, music pass projection,
`wfcmusic=1`, Standard MIDI Files, and the optional playback boundary, see the
[music foundation](docs/music.md).

## Build and test

Run the checked native build and conformance suite from the repository root:

```powershell
.\build.ps1
```

```bash
./build.sh
```

Both entry points compile with checked FPC options, keep all output under
`build/`, run the core, 2D ecosystem, selective-settlement, radius-one
learning, overlapping-pattern, sequence, score/cell, music-graph, SMF, and
score-to-MIDI suites, smoke-test the portable console examples—including the
bounded/wrapped spatial dependency proof—and preserve failure exit codes.
The repository also includes an FPM package, a runtime-only Lazarus package,
and the same conformance sources for pas2js/Node. Separate `build-browser.ps1` and
`build-browser.sh` entry points stage the interactive browser world without
committing generated JavaScript.
See [building and testing](docs/building.md) for compiler overrides, package
commands, output paths, pas2js setup, and browser self-test details.

The core, specialized units, conformance suites, portable examples—including
the pass-composed music example—and building-kit console need no submodule.
Only the two legacy music playback experiments require Lazarus/LCL, SDL2, and
the optional GPL-3.0 SoundShop submodule; they are isolated from the
dependency-free MIT build path. See the [examples index](examples/README.md)
for exact status and commands.

The portable foundation is project-owned Pascal. Whenever a capability can
reasonably be implemented here instead of adding a library, the project
implements and maintains its own FPC/pas2js version; when that choice is
debatable, project-owned Pascal is the default. Core and runtime units depend
only on repository units and the applicable standard FPC/pas2js RTL. Engines,
viewers, and media backends may be optional adapters, and development tools may
assist builds, tests, conversion, or inspection, but none may leak into
core/runtime APIs or define canonical algorithms, models, artifacts,
validation, or replay behavior.

## Direction

The [roadmap](ROADMAP.md) covers the remaining reference-solver work, richer
pass composition, higher-dimensional and cross-pass learning, validation tools,
2D/3D/music/text ecosystems, pas2js playgrounds, reproducible research,
documentation, and release provenance. Current examples are indexed under
[examples](examples/README.md).

## License

WFC is released under the [MIT License](LICENSE).

**Tip jar**

- BTC: `bc1q55qh7xptfgkp087sfr5ppfkqe2jpaa59s8u2lz`
- LTC: `LPbvTsFDZ6EdaLRhsvwbxcSfeUv1eZWGP6`
