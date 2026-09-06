# WFC

Constraint-driven generation in Pascal, for Free Pascal Compiler (FPC) and
pas2js. Describe values, their allowed relationships, and the layers that
depend on them. Generate terrain, then roads, housing, and foliage—or apply
the same principle to structures, music, text, and other discrete designs.

The original fluent API remains the center of the project. The portable
implementation uses project-owned Pascal and the applicable compiler RTL;
browser demos run compiled Pascal using standard browser APIs. The included
FPC server hosts them locally.

**Status:** an expanding 0.x ecosystem, not a finished 1.0 release. The core
supports deterministic propagation, weighted observation, backtracking,
transactional pass DAGs, selective regeneration, and bounded pass negotiation.
Count-range clauses, rooted port connectivity, and opt-in deterministic restarts extend those contracts
without changing existing replay behavior. Domain libraries, learners,
portable artifacts, native tools, and ten interactive browser demos are
implemented and tested. The [roadmap](ROADMAP.md) records the remaining work
and explicit exit gates.

## Start here

| What you want | Where to start |
| --- | --- |
| Understand and author passes | [Pass basics](docs/passes.md), [dependency DAGs](docs/pass-dags.md) |
| Require connected roads or circulation | [Rooted port connectivity](docs/connectivity.md) |
| Build and run locally | [Build guide](docs/building.md), [FPC development tools](docs/development-tools.md) |
| Try an interactive demo | [Demo table below](#demos), [complete examples index](examples/README.md) |
| Learn models from examples | [Learning](docs/learning.md), [training documents and CLI](docs/training.md) |
| Save and replay a pipeline | [Portable recipes, runs, and results](docs/pipeline-artifacts.md) |
| Extend or evaluate the system | [Roadmap](ROADMAP.md), [research records](docs/research/README.md) |

## A small pass-based program

This program generates land/water, then chooses a compatible foliage value
at every coordinate. Each pass owns its own values and rules; foliage reads
terrain through an explicit named requirement.

```pascal
program TwoPasses;

{$mode delphi}{$H+}

uses SysUtils, wfc;

var
  Graph: TGraph;
  Options: TGraphSolveOptions;
  Report: TGraphSolveReport;
begin
  Graph := TGraph.Create;
  try
    Graph.Seed := 42;
    Graph.Reshape(16, 12, 1);
    Graph.WrapNeighbors := False;

    Graph.CurrentPass := 'terrain';
    Graph.PassMode := gpmOverlay;
    Graph.AddValue('land');
    Graph.AddValue('water');

    Graph.SwitchToPass('foliage');
    Graph.PassMode := gpmOverlay;
    Graph.AddValue('grass').RequireFromPass('terrain', 'land');
    Graph.AddValue('reeds').RequireFromPass('terrain', 'water');

    Options := DefaultGraphSolveOptions;
    if not Graph.TrySolve(Options, Report) then
      raise Exception.CreateFmt('WFC failed in pass %d',
        [Report.FailedPassIndex]);

    WriteLn(Graph.PassGraph[0].Entry[0, 0, 0].Value);
    WriteLn(Graph.PassGraph[1].Entry[0, 0, 0].Value);
  finally
    Graph.Free;
  end;
end.
```

Save this as `TwoPasses.lpr`. Once the native build below has created its
output directories, compile it with
`fpc -Mdelphi -Fusrc -FUbuild/native/units -FEbuild/native/bin TwoPasses.lpr`.
The [build guide](docs/building.md) also covers isolated builds, FPM, and the
runtime-only Lazarus package.

Directional rules express same-layer adjacency. Named requirements can read
the same coordinate, a signed offset, any matching declared neighbor, or an
inclusive [count range](docs/pass-counts.md). Explicit modes distinguish
matching offsets from distinct provider cells when wrapped offsets alias.

Ordinary solving follows `prepare → solve → validate → commit`. A failed
reference transaction restores its previous entries and random streams.
[Selective regeneration](docs/pass-dags.md) reopens an explicit descendant
closure; [negotiation](docs/pass-negotiation.md) may revisit complete upstream
assignments within a separate budget. [Restarts](docs/restarts.md) retry local
backtrack exhaustion with versioned effective seeds. None implies a global
optimum, minimal edit, or a proof that every budget-limited model is impossible.

## Demos

Every standard demo uses repository code and licensed source material.
The native and browser hosts share Pascal generation and validation code.

| Domain | Native presentation | Interactive pas2js demo |
| --- | --- | --- |
| 2D worlds | Typed terrain, biome, foliage; settlement and repair examples | [2D Pass Workbench](examples/2D/02_BrowserWorld/README.md) |
| Learned 3D terrain | Learned volume → socket/support structure → spatial foliage; exact SVG | [Learned Terraces](examples/3D/04_LearnedTerraces/README.md) |
| 3D buildings | Validated multi-floor geometry and deterministic SVG | [Building 3D](examples/3D/03_BrowserBuilding/README.md) |
| Text | Structure → lexical → punctuation composition | [Text Pass Workbench](examples/text/03_PassComposition/README.md) |
| Music | Score, MIDI, WAV, streamed arrangements, import/training tools | [Music Studio](examples/music/05_MusicStudio/README.md) |
| Polyphonic music | Synchronized chord-capable voices, training, repair, streamed audio and MIDI | [Ensemble Studio](examples/music/06_EnsembleStudio/README.md) |
| Independent musical roles | Learned role vocabularies, novel vertical combinations, collective harmony, streamed WAV/MIDI | [Voice Studio](examples/music/07_VoiceStudio/README.md) |
| Training | Editable 1D/2D/3D corpora → models → recipes → validated results | [Training Studio](examples/learning/05_TrainingStudio/README.md) |
| Pass counts | Lower/upper bounds, wrapped aliases, scoped repair | [Neighborhood Counts](examples/passes/04_NeighborhoodCounts/README.md) |
| Connected routes | Solver-propagated roads and multi-floor circulation, independent BFS, SVG, scoped repair | [Connected Routes](examples/passes/06_ConnectedRoutes/README.md) |

Music Studio accepts user-defined composition duration and streams newly
solved sections with bounded memory. There is no fixed minute cap; numeric
capacity, storage, and local search constraints still apply. Its current
streamed arranger remains monophonic. The separate
[ensemble libraries](docs/music-ensemble.md) generate synchronized polyphonic
scores and [stream continued polyphonic segments and audio](docs/music-ensemble-stream.md),
with [two-pass streamed MIDI](docs/music-midi-stream.md),
without retaining the whole composition. Finite preview and score adapters
still have their own resource bounds.
The separate [independent-voice libraries](docs/music-voices.md) learn one
chord-capable vocabulary per role and generate new vertical combinations
under collective harmony, shared rhythm, ranges, and optional pair gaps.
Their [research record](docs/research/independent-voices-v1.md) distinguishes
the exact coverage proof from musical breadth and search-performance claims.
See [arrangements](docs/music-arrangement.md)
and [MIDI import and selected-excerpt training](docs/music-import.md).

The [examples index](examples/README.md) includes exact commands, fixtures,
additional console experiments, and the status of isolated legacy demos.

## Build, test, and host

Run the checked native build and conformance suite:

```powershell
.\build.ps1
```

```bash
bash ./build.sh
```

Both scripts preserve nonzero failure exits and place build output under
`build/`. The maintained gate covers the core, domain validators, learners,
artifact codecs, native tools, and console demonstrations. Portable
conformance sources also compile and execute in real browsers.

For example, build and host Music Studio using the included FPC server:

```powershell
.\build-browser-music.ps1 -Compiler 'C:/path/to/pas2js.exe'
.\build\native\bin\wfc_serve.exe --root build/browser/music/www --port 4177
```

The shell equivalents are `bash ./build-browser-music.sh` and
`build/native/bin/wfc_serve --root build/browser/music/www --port 4177`.
Set `PAS2JS` to your compiler path when it is not on `PATH`.
Open `http://127.0.0.1:4177/`; stop the foreground server with Ctrl+C.

The [build guide](docs/building.md) covers all ten browser staging scripts,
compiler versions and overrides, package builds, and CI.
The [development-tool guide](docs/development-tools.md) covers the FPC server,
browser completion capture, evidence checker, and browser conformance runner. Generated browser
code is build output, not a second hand-maintained implementation.

## Framework guides

| Area | Contracts and APIs |
| --- | --- |
| Solver and replay | [Reference solver](docs/solver.md), [determinism](docs/determinism.md), [restarts and timing](docs/restarts.md) |
| Pass composition and diagnosis | [DAGs](docs/pass-dags.md), [counts](docs/pass-counts.md), [negotiation](docs/pass-negotiation.md), [selective negotiation](docs/selective-negotiation.md), [causal traces](docs/traces.md), [streaming and bounded inspection](docs/trace-streaming.md) |
| Learned representations | [Cardinal models](docs/learning.md), [overlapping patterns](docs/patterns.md), [sequences](docs/sequences.md), [training](docs/training.md) |
| Spatial domains | [2D worlds](docs/world2d.md), [voxels](docs/voxel3d.md), [learned terraces](docs/learned-terraces3d.md), [Building 3D](docs/building3d.md) |
| Music and text | [Exact music model](docs/music.md), [polyphonic ensembles](docs/music-ensemble.md), [independent voices](docs/music-voices.md), [audio](docs/music-audio.md), [arrangements](docs/music-arrangement.md), [import](docs/music-import.md), [text completion](docs/text.md) |
| Portable workflow | [Pipeline artifacts and tools](docs/pipeline-artifacts.md), [dependency policy](docs/dependencies.md) |

The remaining work includes richer harmonic models, general voice-leading
constraints, independently learned rhythm roles, larger
spatial topologies, interactive domain/trace inspection, stronger repair
strategies, more importers, and complete release/provenance tooling.
Research claims must have reproducible fixtures and counterexamples; the
current pass-negotiation baseline does not claim greater efficiency than an
equivalent flattened model.

## License

Project-authored source is under the [MIT license](LICENSE). The standard
runtime and demos use project-owned implementations and the applicable
compiler RTL. External engines and legacy media adapters remain optional,
isolated, and subject to their own licenses; see the
[dependency policy](docs/dependencies.md) and [examples index](examples/README.md).
