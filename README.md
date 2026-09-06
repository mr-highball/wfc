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
portable artifacts, native tools, and interactive browser demos have
executable conformance fixtures. The [roadmap](ROADMAP.md) records the remaining work
and explicit exit gates.

## Start here

| What you want | Where to start |
| --- | --- |
| Understand and author passes | [Pass basics](docs/passes.md), [dependency DAGs](docs/pass-dags.md) |
| Compose coarse and fine spatial layers | [Mapped passes](docs/mapped-passes.md), [Mapped World workbench](examples/passes/07_MappedWorld/README.md) |
| Require connected roads or circulation | [Rooted port connectivity](docs/connectivity.md), [training-source authoring](docs/training-connectivity.md) |
| Require exact or bounded output quantities | [Whole-pass value quotas](docs/value-quotas.md), [portable quota recipes](docs/pipeline-value-quotas.md) |
| Build and run locally | [Build guide](docs/building.md), [FPC development tools](docs/development-tools.md) |
| Try an interactive demo | [Demo table below](#demos), [complete examples index](examples/README.md) |
| Learn models from examples | [Learning](docs/learning.md), [training documents and CLI](docs/training.md) |
| Learn cyclic text, rhythms or event patterns | [Circular sequence training](docs/sequences.md#circular-source-training), [Training Studio](docs/training-studio.md) |
| Save and replay a pipeline | [Portable recipes, runs, and results](docs/pipeline-artifacts.md) |
| Assemble independently authored or learned pipelines | [Immutable fragment composition](docs/pipeline-composition.md) |
| Edit a bound pipeline and authorize repair | [Preparation and input replacement](docs/pipeline-preparation.md), [prepared sessions](docs/pipeline-sessions.md) |
| Save edit history and restore a live workspace | [Workspace journals, exact replay, atomic authoring and CLI](docs/pipeline-workspaces.md) |
| Check or inspect saved work | [Artifact-family validation, inspection, and exact replay](docs/artifact-tools.md) |
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
Opt-in [mapped passes](docs/mapped-passes.md) give each layer its own integer
world layout: coarse terrain, fine foliage, and larger building footprints can
read exact points or every intersecting provider cell, including unique-cell
count ranges. The [Mapped World workbench](examples/passes/07_MappedWorld/README.md)
lets you edit domains, locks, housing demands, and sampling on three fixed
showcase grids. Its [reproducible footprint study](docs/research/mapped-world-footprints-v1.md)
contrasts a clear corner with an interior blocker and demonstrates explicitly
authorized upstream repair. The additive [portable mapped pipeline extension](docs/portable-mapped-passes.md)
keeps these concerns separate: recipes own per-pass topology and mapped policies, runs
own independent extents, and results retain each layer's actual layout. The
workbench's general layout editing and portable import/export UI remain open.

[Pipeline composition](docs/pipeline-composition.md) combines complete authored
or learned recipes with explicit names, preserving projection bridges, aliases,
policies, topology, and resource provenance. It produces an immutable recipe;
the caller supplies run extents and executes it through the existing runtime.
The [prepared-session API](docs/pipeline-sessions.md) now adds immediate input
edits, revision-bound explicit repair plans, cumulative pending requirements and
complete detached solver/state evidence for one bound recipe. It keeps caller
ownership and scoped reuse distinct from fresh invocation results. The
[workspace layer](docs/pipeline-workspaces.md) adds complete ordered journals,
explicit epochs, exact replay and atomic edit/repair publication through shared
Pascal APIs and an included FPC CLI. Imported journals remain unverified claims
until their complete actual evidence replays. A general interactive editor and
migration of the Mapped World/Ensemble Studio interfaces remain separate work.

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
| Mixed-resolution worlds | Coarse terrain, fine foliage, inset housing; footprint inspection, scoped repair, shared SVG | [Mapped World](examples/passes/07_MappedWorld/README.md) |
| Learned 3D terrain | Learned volume → socket/support structure → spatial foliage; exact SVG | [Learned Terraces](examples/3D/04_LearnedTerraces/README.md) |
| 3D buildings | Validated multi-floor geometry and deterministic SVG | [Building 3D](examples/3D/03_BrowserBuilding/README.md) |
| Text | Structure → lexical → punctuation composition | [Text Pass Workbench](examples/text/03_PassComposition/README.md) |
| Music | Score, MIDI, WAV, streamed arrangements, import/training tools | [Music Studio](examples/music/05_MusicStudio/README.md) |
| Polyphonic music | Synchronized chord-capable voices, training, repair, streamed audio and MIDI | [Ensemble Studio](examples/music/06_EnsembleStudio/README.md) |
| Independent musical roles | Learned role vocabularies, novel vertical combinations, collective harmony, streamed WAV/MIDI | [Voice Studio](examples/music/07_VoiceStudio/README.md) |
| Training | Editable 1D/2D/3D corpora → models → recipes → validated results | [Training Studio](examples/learning/05_TrainingStudio/README.md) |
| Overlapping volumes | Joint XYZ learning, saved public constraints/replay, native cutaway SVG | [Training Studio lattice preset](examples/learning/05_TrainingStudio/README.md#native-volume-svg) |
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
The [musical-form planner](docs/music-form.md) adds bar-level form,
harmonic-intent, and gesture passes. Ensemble Studio's developed profile uses
them for question/answer/contrast/return phrases, with exact acoustic masks
and an independently checked realization instead of extending one repeated bar.
The optional [native Ensemble download host](docs/ensemble-http-downloads.md)
streams user-selected-duration WAVE to ordinary browser downloads, including
trusted-LAN clients without the direct browser file-system picker.
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

The [build guide](docs/building.md) covers the browser staging scripts,
compiler versions and overrides, package builds, and CI.
The [development-tool guide](docs/development-tools.md) covers the FPC server,
browser completion capture, evidence checker, and browser conformance runner. Generated browser
code is build output, not a second hand-maintained implementation.

## Framework guides

| Area | Contracts and APIs |
| --- | --- |
| Solver and replay | [Reference solver](docs/solver.md), [determinism](docs/determinism.md), [restarts and timing](docs/restarts.md) |
| Pass composition and diagnosis | [DAGs](docs/pass-dags.md), [counts](docs/pass-counts.md), [negotiation](docs/pass-negotiation.md), [selective negotiation](docs/selective-negotiation.md), [causal traces](docs/traces.md), [streaming and bounded inspection](docs/trace-streaming.md) |
| Learned representations | [Cardinal models](docs/learning.md), [2D overlapping patterns](docs/patterns.md), [3D overlapping volumes](docs/overlapping-3d.md), [sequences](docs/sequences.md), [training](docs/training.md) |
| Spatial domains | [2D worlds](docs/world2d.md), [mapped passes](docs/mapped-passes.md), [voxels](docs/voxel3d.md), [learned terraces](docs/learned-terraces3d.md), [Building 3D](docs/building3d.md) |
| Music and text | [Exact music model](docs/music.md), [polyphonic ensembles](docs/music-ensemble.md), [independent voices](docs/music-voices.md), [audio](docs/music-audio.md), [arrangements](docs/music-arrangement.md), [import](docs/music-import.md), [text completion](docs/text.md) |
| Portable workflow | [Pipeline artifacts](docs/pipeline-artifacts.md), [editable workspaces and exact history replay](docs/pipeline-workspaces.md), [validation and inspection tools](docs/artifact-tools.md), [dependency policy](docs/dependencies.md) |

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
compiler RTL. The former external-engine and media-library demo integrations
have been removed; see the
[dependency policy](docs/dependencies.md) and [examples index](examples/README.md).
