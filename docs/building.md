# Building and testing

The dependency-free build covers the core, causal-trace kernel/public/utility
contracts, specialized 2D and settlement, radius-one model-learning,
overlapping-pattern and projected-pattern passes, sequence, Unicode-scalar
text completion, three-pass text composition, exact music-score, music
projection, Standard MIDI File,
score-export, deterministic PCM/WAVE rendering, Music Studio,
negotiated music variation/result replay, full and selective
pass negotiation, canonical numeric text primitives, immutable authored-rule
models, declarative pipeline recipes, recipe-bound run artifacts, the closed
pipeline compiler/runtime, immutable public result artifacts, strict run/result
codecs, the recipe validator and pipeline runner applications, the canonical
learned-pattern-world recipe/run/result bundle, and voxel-3D
units and their conformance suites, plus
the checked voxel pass bridge and multi-pass Building 3D owner/validator,
pass-aware view, fixed-integer isometric projector, and canonical SVG encoder.
It also runs seeded smoke checks of the portable console demos, including the
causal-trace inspector, bounded pass negotiation, negotiated descendant repair,
anchored text infill, three-pass text composition, and negotiated music
variation, the four-pass learned-pattern world, and writes a checked seed-zero
Building SVG artifact.
It does not initialize the optional legacy music submodule or build the
unfinished Castle Game Engine viewer. The 2D world, three-pass text workbench,
Building 3D, Training Studio, and Music Studio have separate pas2js browser entry
points described below.

FPC 3.2.2 is the supported stable compiler. The current FPC development
compiler is also exercised as a compatibility canary.

## Dependency boundary

Runtime units may use repository units and the applicable standard FPC/pas2js
RTL. The dependency admission rule is hard: when a needed capability can
reasonably be implemented and maintained in portable Pascal, or that judgment
is debatable, the dependency is rejected and WFC owns the FPC/pas2js
implementation. Optional tools may build, test, profile, render, convert, or
inspect project artifacts, but tool-specific units and types must not enter
core/runtime `uses` clauses or public APIs. Canonical artifacts, validation,
generation, and replay remain usable without those tools.

The sequence learner, extent-aware graph adapter, exact public-domain analyzer,
validator, canonical `wfcs=1` codec, Unicode-scalar tokenizer, and text
completion/validation owner follow this boundary. The exact music score,
fixed-quantum cell codecs,
cross-model projection maps, persistent pass owner, strict `wfcmusic=1` score
and `wfcmusicpass=1` result codecs, raw SMF format-0/1 codec, and format-0
score exporter and fixed-point PCM/WAVE renderer are project-owned portable
Pascal. The voxel kit, semantic
validator, and integer surface mesh are
likewise project-owned and expose no engine or renderer type. The voxel pass
bridge, typed Building 3D owner, cross-layer validator, and shared
native/pas2js demonstration are project-owned as well. The immutable Building
view, fixed-integer command projection, hit testing, and SVG encoder are also
portable project units. Causal capture, hashing, formatting, query, and
validation are likewise project-owned Pascal in `wfc` and `wfc_trace`.
The `wfcpipeline=1`, `wfcpipeline-run=1`, and `wfcpipeline-result=1` codecs,
recipe compiler, transactional runtime, result capture, validator application,
and command-line parsing are also shared native/pas2js Pascal. Native and Node
hosts add only bounded byte I/O and process-exit plumbing.

Playback systems, editors, native window/engine
adapters, and media backends remain optional edge integrations; none is
required to learn, serialize, solve, validate, export, mesh, project, or write
the canonical graphical artifact.
The GPL-3.0 SoundShop submodule, Lazarus/LCL, and SDL2 occur only in two
explicitly legacy examples.
Here and throughout the build guide, “dependency-free” means no third-party
runtime library: repository units, the standard compiler RTL, and the selected
host/toolchain APIs remain present. The full decision and optional-adapter
rules are in the [dependency policy](dependencies.md).

## One-command native gate

From the repository root, use the entry point for your shell:

```powershell
.\build.ps1
```

```bash
./build.sh
```

Both scripts rebuild with assertions and range, overflow, and I/O checks, run
`wfc_test`, `wfc_world2d_test`, `wfc_world2d_settlement_test`,
`wfc_learn_test`, `wfc_pattern2d_test`, `wfc_pattern2d_passes_test`,
`wfc_sequence_test`,
`wfc_text_test`, `wfc_text_passes_test`, `wfc_negotiation_test`,
`wfc_selective_negotiation_test`, `wfc_voxel3d_test`,
`wfc_voxel3d_isometric_test`,
`wfc_voxel3d_svg_test`, `wfc_building3d_test`,
`wfc_building3d_view_test`, `wfc_midi_smf_test`,
`wfc_music_test`,
`wfc_music_graph_test`, `wfc_music_midi_test`, `wfc_music_passes_test`,
`wfc_music_passes_text_test`, `wfc_music_audio_test`, `wfc_music_studio_test`,
`wfc_text_codec_test`, `wfc_rule_model_test`,
`wfc_rule_text_test`, `wfc_pipeline_model_test`, `wfc_pipeline_text_test`,
`wfc_pipeline_run_test`, `wfc_pipeline_run_text_test`,
`wfc_pipeline_compile_test`, `wfc_pipeline_result_test`,
`wfc_pipeline_result_text_test`, `wfc_pipeline_runtime_test`,
`wfc_token_lookup_test`, `wfc_validate_app_test`, `wfc_run_app_test`,
`wfc_training_test`, `wfc_training_text_test`, `wfc_learn_app_test`,
`wfc_text_training_test`, `wfc_training_workspace_test`,
`wfc_learned_pattern_world_bundle_test`,
`wfc_trace_reference_test`,
`wfc_trace_test`, and `wfc_trace_utility_test`, then compile and smoke-test the
portable console examples with seed `0`, including the bounded/wrapped spatial
dependency self-check, causal-trace inspector, bounded pass-negotiation proof,
negotiated descendant repair, anchored text completion, three-pass text
composition, negotiated music variation, the learned-pattern world, and
depth-three Building 3D pipeline, all five Training Studio presets, and the
Music Studio generation/repair/audio self-test. The 32-case Music Studio form
probe is compared with its checked-in raw CSV;
the multi-pass and
selective-settlement worlds also run with their default seeds. Finally, the
native `Building3DSvg` host generates and validates
`build/native/bin/building3d-seed-zero.svg`.
A checked process suite also runs both native pipeline tools against the
canonical files in `test/fixtures/pipeline-cli` and
`examples/2D/05_LearnedPatternWorld/pipeline`: file and standard-input paths,
exact validator and result bytes (including the domain-sized bundle), quiet
output, and the documented invalid, usage, I/O, solved, and non-solved exit
classes are all exercised.
A second, 25-case process suite checks `wfc_learn`, `wfc_validate`, and
`wfc_run` against all four source/model/recipe/run/result bundles in
`examples/learning/04_TrainingDocuments`. It covers file/stdin training,
standalone output, recipe validation, solved replay, quiet/version behavior,
and invalid/usage/I/O diagnostics. The native training-text suite requires
that absolute fixture directory as its first argument; the build supplies it.
A compiler error, failed check,
or example failure produces a nonzero exit code. Compiler units and binaries
are written beneath `build/native/`; running the gate does not modify tracked
source files.

Set `FPC` to select another compiler. Additional compiler arguments may be
passed explicitly:

```powershell
$env:FPC = 'C:\FPC\3.2.2\bin\i386-win32\fpc.exe'
.\build.ps1 -CompilerOptions @('-O2')
```

```bash
FPC=/opt/fpc/bin/fpc ./build.sh -O2
```

## Headless pipeline tools

The native and pas2js/Node builds expose the same project-owned application
logic through thin hosts. The recipe-only validator accepts one file or
standard input:

```text
wfc-validate recipe [--quiet | --emit-canonical] [--] INPUT
```

Default success output is a one-line static summary. `--emit-canonical` emits
the exact strictly verified `wfcpipeline=1` input, and `--quiet` emits nothing.
This command validates the recipe and its embedded resources; it does not
compile or solve.

The runner accepts one recipe and its bound run artifact:

```text
wfc-run [--quiet] [--] RECIPE RUN
```

These are the distribution-facing command names used in help and diagnostics.
The checked repository build writes the native source-host names
`build/native/bin/wfc_validate[.exe]` and
`build/native/bin/wfc_run[.exe]`; the training host is
`build/native/bin/wfc_learn[.exe]`. Direct pas2js builds write
`wfc_validate_node.js`, `wfc_run_node.js`, and `wfc_learn_node.js` in the
selected output directory. The native training entry is named
`tools/wfc_learn_cli.lpr` to avoid an object collision with the learner unit;
compile it with `-owfc_learn` (`-owfc_learn.exe` on Windows).
See [training documents](training.md) for its
CLI contract and examples.
Packagers may expose the documented hyphenated names without changing the
shared application units or artifact contracts.

One positional path may be `-`, but both cannot read standard input. By
default, solved and non-solved executions both emit one canonical
`wfcpipeline-result=1` document. `--quiet` suppresses that output. Exit `0`
means solved, `1` means an invalid recipe/run/executable invocation, `2` is a
usage error, `3` is an I/O error, `4` means a valid canonical non-solved
result, and `70` is an unexpected internal failure. The complete lifecycle,
ownership, safety bounds, and CLI contracts are in
[Portable pipeline artifacts](pipeline-artifacts.md).

## FPM package

`fpmake.pp` describes the runtime `wfc` package. Its only declared dependency,
`rtl-generics`, is part of the standard FPC distribution; no third-party
runtime library is required. Bootstrap FPMake with the compiler, then build
the package:

```powershell
New-Item -ItemType Directory -Force `
  build\fpm\bootstrap\units, build\fpm\bootstrap\bin |
  Out-Null
fpc -B -Mobjfpc -Sa -Cr -Co -Ci `
  -FUbuild\fpm\bootstrap\units -FEbuild\fpm\bootstrap\bin fpmake.pp
.\build\fpm\bootstrap\bin\fpmake.exe build
```

```bash
mkdir -p build/fpm/bootstrap/units build/fpm/bootstrap/bin
fpc -B -Mobjfpc -Sa -Cr -Co -Ci \
  -FUbuild/fpm/bootstrap/units -FEbuild/fpm/bootstrap/bin fpmake.pp
./build/fpm/bootstrap/bin/fpmake build
```

Some self-contained compiler layouts do not expose installed package metadata
through their default configuration. If FPMake cannot resolve `rtl` or
`rtl-generics`, append `--globalunitdir=<FPC installation prefix>` and, when
needed, `--compiler=<path to fpc>` to the `fpmake build` command.

FPMake writes a target-specific `wfc-*.fpm` metadata file at the repository
root. That generated file is ignored; compiled units are written beneath
`build/fpm/units/`.

## Lazarus package and project

`wfc.lpk` is a runtime-only package. It does not depend on the LCL. Build the
package and the conformance project without allowing Lazarus to rewrite their
metadata:

```text
lazbuild -B --no-write-project wfc.lpk
lazbuild -B --no-write-project test/wfc_test.lpi
```

The package output is written to `build/lazarus/package/<target>`. The test
executable is written to `build/lazarus/test/bin`, with its units kept in the
adjacent `units/<target>` directory. Run `wfc_test.exe` on Windows or
`wfc_test` on other native targets.

## pas2js and Node.js

The conformance sources can be compiled for Node.js when pas2js and its
matching RTL are configured:

```bash
mkdir -p build/pas2js/units build/pas2js
pas2js -B -Tnodejs -Mdelphi -Fusrc \
  -FUbuild/pas2js/units -FEbuild/pas2js test/wfc_test.lpr
node build/pas2js/wfc_test.js

mkdir -p build/pas2js/world-units build/pas2js/world
pas2js -B -Tnodejs -Mdelphi -Fusrc \
  -FUbuild/pas2js/world-units -FEbuild/pas2js/world \
  test/wfc_world2d_test.lpr
node build/pas2js/world/wfc_world2d_test.js

mkdir -p build/pas2js/settlement-units build/pas2js/settlement
pas2js -B -Tnodejs -Mdelphi -Fusrc \
  -FUbuild/pas2js/settlement-units -FEbuild/pas2js/settlement \
  test/wfc_world2d_settlement_test.lpr
node build/pas2js/settlement/wfc_world2d_settlement_test.js

mkdir -p build/pas2js/learning-units build/pas2js/learning
pas2js -B -Tnodejs -Mdelphi -Fusrc \
  -FUbuild/pas2js/learning-units -FEbuild/pas2js/learning \
  test/wfc_learn_test.lpr
node build/pas2js/learning/wfc_learn_test.js

mkdir -p build/pas2js/pattern-units build/pas2js/pattern
pas2js -B -Tnodejs -Mdelphi -Fusrc \
  -FUbuild/pas2js/pattern-units -FEbuild/pas2js/pattern \
  test/wfc_pattern2d_test.lpr
node build/pas2js/pattern/wfc_pattern2d_test.js

mkdir -p build/pas2js/pattern-pass-units build/pas2js/pattern-pass
pas2js -B -Tnodejs -Mdelphi -Fusrc \
  -FUbuild/pas2js/pattern-pass-units -FEbuild/pas2js/pattern-pass \
  test/wfc_pattern2d_passes_test.lpr
node build/pas2js/pattern-pass/wfc_pattern2d_passes_test.js

mkdir -p build/pas2js/sequence-units build/pas2js/sequence
pas2js -B -Tnodejs -Mdelphi -Fusrc \
  -FUbuild/pas2js/sequence-units -FEbuild/pas2js/sequence \
  test/wfc_sequence_test.lpr
node build/pas2js/sequence/wfc_sequence_test.js

mkdir -p build/pas2js/text-units build/pas2js/text
pas2js -B -Tnodejs -Mdelphi -Fusrc \
  -FUbuild/pas2js/text-units -FEbuild/pas2js/text \
  test/wfc_text_test.lpr
node build/pas2js/text/wfc_text_test.js

mkdir -p build/pas2js/text-passes-units build/pas2js/text-passes
pas2js -B -Tnodejs -Mdelphi -Fusrc \
  -FUbuild/pas2js/text-passes-units -FEbuild/pas2js/text-passes \
  test/wfc_text_passes_test.lpr
node build/pas2js/text-passes/wfc_text_passes_test.js

mkdir -p build/pas2js/negotiation-units build/pas2js/negotiation
pas2js -B -Tnodejs -Mdelphi -Fusrc \
  -FUbuild/pas2js/negotiation-units -FEbuild/pas2js/negotiation \
  test/wfc_negotiation_test.lpr
node build/pas2js/negotiation/wfc_negotiation_test.js

mkdir -p build/pas2js/selective-negotiation-units \
  build/pas2js/selective-negotiation
pas2js -B -Tnodejs -Mdelphi -Fusrc \
  -FUbuild/pas2js/selective-negotiation-units \
  -FEbuild/pas2js/selective-negotiation \
  test/wfc_selective_negotiation_test.lpr
node build/pas2js/selective-negotiation/wfc_selective_negotiation_test.js

mkdir -p build/pas2js/voxel-units build/pas2js/voxel
pas2js -B -Tnodejs -Mdelphi -Fusrc \
  -FUbuild/pas2js/voxel-units -FEbuild/pas2js/voxel \
  test/wfc_voxel3d_test.lpr
node build/pas2js/voxel/wfc_voxel3d_test.js

mkdir -p build/pas2js/building-units build/pas2js/building
pas2js -B -Tnodejs -Mdelphi -Fusrc \
  -FUbuild/pas2js/building-units -FEbuild/pas2js/building \
  test/wfc_building3d_test.lpr
node build/pas2js/building/wfc_building3d_test.js

mkdir -p build/pas2js/trace-units build/pas2js/trace
for trace_test in wfc_trace_reference_test wfc_trace_test \
  wfc_trace_utility_test
do
  pas2js -B -Tnodejs -Mdelphi -Fusrc \
    -FUbuild/pas2js/trace-units -FEbuild/pas2js/trace \
    "test/${trace_test}.lpr"
  node "build/pas2js/trace/${trace_test}.js"
done

mkdir -p build/pas2js/music-units build/pas2js/music
for music_test in wfc_midi_smf_test wfc_music_test \
  wfc_music_graph_test wfc_music_midi_test \
  wfc_music_passes_test wfc_music_passes_text_test \
  wfc_music_audio_test wfc_music_studio_test
do
  pas2js -B -Tnodejs -Mdelphi -Fusrc \
    -Fuexamples/music/05_MusicStudio \
    -FUbuild/pas2js/music-units -FEbuild/pas2js/music \
    "test/${music_test}.lpr"
  node "build/pas2js/music/${music_test}.js"
done

mkdir -p build/pas2js/artifact-units build/pas2js/artifact
for artifact_test in wfc_text_codec_test wfc_rule_model_test \
  wfc_rule_text_test wfc_pipeline_model_test wfc_pipeline_text_test \
  wfc_pipeline_run_test wfc_pipeline_run_text_test \
  wfc_pipeline_compile_test wfc_pipeline_result_test \
  wfc_pipeline_result_text_test wfc_pipeline_runtime_test \
  wfc_token_lookup_test wfc_validate_app_test wfc_run_app_test \
  wfc_training_test wfc_training_text_test wfc_learn_app_test \
  wfc_text_training_test wfc_training_workspace_test \
  wfc_learned_pattern_world_bundle_test
do
  pas2js -B -Tnodejs -Mdelphi -Fusrc -Futools \
    -Fuexamples/2D/05_LearnedPatternWorld \
    -Fuexamples/learning/05_TrainingStudio \
    -FUbuild/pas2js/artifact-units -FEbuild/pas2js/artifact \
    "test/${artifact_test}.lpr"
  node "build/pas2js/artifact/${artifact_test}.js"
done
```

The portable multi-pass host uses the same target:

```bash
mkdir -p build/pas2js/world-example-units build/pas2js/world-example
pas2js -B -Tnodejs -Mdelphi -Fusrc \
  -Fuexamples/2D/common \
  -FUbuild/pas2js/world-example-units -FEbuild/pas2js/world-example \
  examples/2D/01_MultiPassWorld/MultiPassWorld.lpr
node build/pas2js/world-example/MultiPassWorld.js 0
```

The dependency-DAG settlement host is portable in the same way:

```bash
mkdir -p build/pas2js/settlement-example-units build/pas2js/settlement-example
pas2js -B -Tnodejs -Mdelphi -Fusrc \
  -Fuexamples/2D/common \
  -FUbuild/pas2js/settlement-example-units \
  -FEbuild/pas2js/settlement-example \
  examples/2D/03_SelectiveSettlement/SelectiveSettlement.lpr
node build/pas2js/settlement-example/SelectiveSettlement.js 0
```

The negotiated descendant-repair host shares one self-checking unit between
native FPC and Node:

```bash
mkdir -p build/pas2js/repair-example-units build/pas2js/repair-example
pas2js -B -Tnodejs -Mdelphi -Fusrc \
  -Fuexamples/2D/04_NegotiatedRepair \
  -FUbuild/pas2js/repair-example-units \
  -FEbuild/pas2js/repair-example \
  examples/2D/04_NegotiatedRepair/NegotiatedRepairNode.lpr
node build/pas2js/repair-example/NegotiatedRepairNode.js
```

The learned-tiles training and generation host is portable in the same way:

```bash
mkdir -p build/pas2js/learning-example-units build/pas2js/learning-example
pas2js -B -Tnodejs -Mdelphi -Fusrc \
  -FUbuild/pas2js/learning-example-units -FEbuild/pas2js/learning-example \
  examples/learning/01_LearnTiles/LearnTiles.lpr
node build/pas2js/learning-example/LearnTiles.js 0
```

The heterogeneous corpus host exercises canonical `wfcm=2` on Node.js:

```bash
mkdir -p build/pas2js/corpus-example-units build/pas2js/corpus-example
pas2js -B -Tnodejs -Mdelphi -Fusrc \
  -FUbuild/pas2js/corpus-example-units -FEbuild/pas2js/corpus-example \
  examples/learning/02_LearnCorpus/LearnCorpus.lpr
node build/pas2js/corpus-example/LearnCorpus.js 0
```

The overlapping-pattern host exercises canonical `wfcp=1`, latent capture,
and independently checked projection:

```bash
mkdir -p build/pas2js/pattern-example-units build/pas2js/pattern-example
pas2js -B -Tnodejs -Mdelphi -Fusrc \
  -FUbuild/pas2js/pattern-example-units -FEbuild/pas2js/pattern-example \
  examples/learning/03_LearnPatterns/LearnPatterns.lpr
node build/pas2js/pattern-example/LearnPatterns.js 0
```

The learned-pattern world materializes that projection inside a four-pass DAG
and proves exact rollback and recovery:

```bash
mkdir -p build/pas2js/pattern-world-units build/pas2js/pattern-world
pas2js -B -Tnodejs -Mdelphi -Fusrc \
  -Fuexamples/2D/05_LearnedPatternWorld \
  -FUbuild/pas2js/pattern-world-units -FEbuild/pas2js/pattern-world \
  examples/2D/05_LearnedPatternWorld/LearnedPatternWorldNode.lpr
node build/pas2js/pattern-world/LearnedPatternWorldNode.js 0
```

The sequence host exercises bounded order-N learning, canonical `wfcs=1`,
latent solving, and independently checked public-token projection:

```bash
mkdir -p build/pas2js/sequence-example-units build/pas2js/sequence-example
pas2js -B -Tnodejs -Mdelphi -Fusrc \
  -FUbuild/pas2js/sequence-example-units -FEbuild/pas2js/sequence-example \
  examples/sequence/01_LearnSequence/LearnSequence.lpr
node build/pas2js/sequence-example/LearnSequence.js 0
```

The text host shares Unicode-scalar learning, exact domain analysis, anchored
infill, and independent validation between native FPC and Node:

```bash
mkdir -p build/pas2js/text-example-units build/pas2js/text-example
pas2js -B -Tnodejs -Mdelphi -Fusrc \
  -Fuexamples/text/02_ConstraintCompletion \
  -FUbuild/pas2js/text-example-units -FEbuild/pas2js/text-example \
  examples/text/02_ConstraintCompletion/ConstraintCompletionNode.lpr
node build/pas2js/text-example/ConstraintCompletionNode.js 0
```

The text pass host runs the reusable structure -> lexical -> punctuation owner,
independent cross-layer validator, exact fragment renderer, and sanitized
causal trace:

```bash
mkdir -p build/pas2js/text-pass-example-units \
  build/pas2js/text-pass-example
pas2js -B -Tnodejs -Mdelphi -Fusrc \
  -Fuexamples/text/03_PassComposition \
  -FUbuild/pas2js/text-pass-example-units \
  -FEbuild/pas2js/text-pass-example \
  examples/text/03_PassComposition/TextPassCompositionNode.lpr
node build/pas2js/text-pass-example/TextPassCompositionNode.js 0
```

Seed zero emits `A sun rises brightly!`, showcase signature `1:69ABA6CE`, and
the same numeric trace hash as native FPC. The models and semantic maps are
project-authored Pascal fixture data; this demonstration does not infer a
grammar taxonomy from raw prose.

The music host exercises harmony + rhythm -> melody pass composition, exact
score reconstruction, strict `wfcmusic=1`, and the project-owned format-0 MIDI
exporter:

```bash
mkdir -p build/pas2js/music-example-units build/pas2js/music-example
pas2js -B -Tnodejs -Mdelphi -Fusrc \
  -FUbuild/pas2js/music-example-units -FEbuild/pas2js/music-example \
  examples/music/03_PassComposition/PassComposition.lpr
node build/pas2js/music-example/PassComposition.js 0
```

This host validates artifacts held in memory. It does not provide a browser UI
or playback backend.

The negotiated-variation host exercises public motif locks, atomic ordinary
failure, bounded provider reopening, selective repair scope, independent
composition validation, and strict public-result replay:

```bash
mkdir -p build/pas2js/music-variation-units \
  build/pas2js/music-variation
pas2js -B -Tnodejs -Mdelphi -Fusrc \
  -Fuexamples/music/04_NegotiatedVariation \
  -FUbuild/pas2js/music-variation-units \
  -FEbuild/pas2js/music-variation \
  examples/music/04_NegotiatedVariation/NegotiatedVariationNode.lpr
node build/pas2js/music-variation/NegotiatedVariationNode.js 0
```

Its finite local and pass-backtrack budgets are part of the example contract;
success proves a deterministic compatible result, not edit-minimal or
musically optimal repair.

The Pipeline v2 spatial host uses a thin Node entry point over the same Pascal
unit as the native executable:

```bash
mkdir -p build/pas2js/spatial-example-units build/pas2js/spatial-example
pas2js -B -Tnodejs -Mdelphi -Fusrc \
  -FUbuild/pas2js/spatial-example-units \
  -FEbuild/pas2js/spatial-example \
  examples/passes/01_SpatialDependencies/SpatialDependenciesNode.lpr
node build/pas2js/spatial-example/SpatialDependenciesNode.js 0
```

It checks exact-offset and finite any-neighbor clauses, bounded rejection,
wrapped sampling, independent validation, and same-seed replay without an
external runtime library.

The Causal Trace v1 inspector uses the same shared-source pattern:

```bash
mkdir -p build/pas2js/trace-example-units build/pas2js/trace-example
pas2js -B -Tnodejs -Mdelphi -Fusrc \
  -Fuexamples/passes/02_TraceInspector \
  -FUbuild/pas2js/trace-example-units \
  -FEbuild/pas2js/trace-example \
  examples/passes/02_TraceInspector/TraceInspectorNode.lpr
node build/pas2js/trace-example/TraceInspectorNode.js
```

It validates and prints the complete terrain -> settlement -> foliage event
stream, per-pass slices, portable hash, and a backward provider-pass cause
chain. Native and Node output is identical; no inspection library beyond the
repository units and standard RTL is required. The full schema and limits are
documented in [`docs/traces.md`](traces.md).

The bounded full-pipeline negotiation proof uses the same pattern:

```bash
mkdir -p build/pas2js/negotiation-example-units \
  build/pas2js/negotiation-example
pas2js -B -Tnodejs -Mdelphi -Fusrc \
  -Fuexamples/passes/03_PassNegotiation \
  -FUbuild/pas2js/negotiation-example-units \
  -FEbuild/pas2js/negotiation-example \
  examples/passes/03_PassNegotiation/PassNegotiationNode.lpr
node build/pas2js/negotiation-example/PassNegotiationNode.js
```

The depth-three Building 3D host also uses one shared Pascal implementation
behind thin native and Node entry points:

```bash
mkdir -p build/pas2js/building-example-units \
  build/pas2js/building-example
pas2js -B -Tnodejs -Mdelphi -Fusrc \
  -Fuexamples/3D/common \
  -Fuexamples/3D/02_MultiPassBuilding \
  -FUbuild/pas2js/building-example-units \
  -FEbuild/pas2js/building-example \
  examples/3D/02_MultiPassBuilding/MultiPassBuildingNode.lpr
node build/pas2js/building-example/MultiPassBuildingNode.js 0
```

It solves and independently validates footprint -> structure ->
envelope/roof -> props, captures the structure mesh, and emits only public
roles, prototype identities, rotations, and portable signatures. No viewer or
engine package is needed.

The fixed-integer projector, canonical SVG encoder, and pass-aware Building
view have matching Node conformance programs:

```bash
for view_test in wfc_voxel3d_isometric_test wfc_voxel3d_svg_test
do
  mkdir -p "build/pas2js/${view_test}-units" \
    "build/pas2js/${view_test}"
  pas2js -B -Tnodejs -Mdelphi -Fusrc \
    -FU"build/pas2js/${view_test}-units" \
    -FE"build/pas2js/${view_test}" "test/${view_test}.lpr"
  node "build/pas2js/${view_test}/${view_test}.js"
done

mkdir -p build/pas2js/building-view-units build/pas2js/building-view
pas2js -B -Tnodejs -Mdelphi -Fusrc -Fuexamples/3D/common \
  -FUbuild/pas2js/building-view-units \
  -FEbuild/pas2js/building-view test/wfc_building3d_view_test.lpr
node build/pas2js/building-view/wfc_building3d_view_test.js
```

A standalone `pas2js` executable is not enough when its RTL unit paths are
missing. Use the compiler and RTL from the same installation.

## pas2js browser world

The browser entry points compile the browser-target Pascal program and stage
its HTML, CSS, and generated `BrowserWorld.js` together:

```powershell
.\build-browser.ps1 -Compiler 'C:\path\to\pas2js.exe'
```

```bash
PAS2JS=/opt/pas2js/bin/pas2js bash ./build-browser.sh
```

Omit the compiler override when `pas2js` is on `PATH`. Both scripts keep units
in `build/browser/world2d/units` and the complete static site in
`build/browser/world2d/www`. The shell entry point converts repository and
output paths for Cygwin, MSYS, and Git Bash before invoking a Windows compiler.
They concatenate the matching pas2js browser RTL into `BrowserWorld.js`, so the
staged page does not depend on an unstaged runtime script.

Serve the staged site from the repository root, for example:

```text
python -m http.server 8080 --directory build/browser/world2d/www
```

Open `http://localhost:8080/` for the interactive viewer. The deterministic
browser fixture at `http://localhost:8080/?selftest=1` succeeds only when the
body finishes with `data-state="solved"`, `data-self-test="passed"`, and the
seed-zero signature
`data-signature="1:5B0DD75D:08022AF1:A40D0955"`. Generated JavaScript and
compiler units remain under the ignored `build/` tree; source distributions do
not commit them.

## pas2js browser text passes

The three-pass text workbench compiles the same showcase owner and validator
used by native FPC and Node, then stages its HTML, CSS, and generated
`BrowserTextPassComposition.js` together:

```powershell
.\build-browser-text.ps1 -Compiler 'C:\path\to\pas2js.exe'
```

```bash
PAS2JS=/opt/pas2js/bin/pas2js bash ./build-browser-text.sh
```

The complete static site is written to `build/browser/text-passes/www`.
Serve that directory and append `?selftest=1`. The self-test generates twice,
proves exact replay, loads an incompatible structure/lexical lineage, proves
transactional contradiction, clears all caller constraints, and proves exact
recovery including the adapter's endpoint domains.

Success leaves the body with `data-state="solved"`,
`data-self-test="passed"`, `data-signature="1:69ABA6CE"`,
`data-output-signature="1:69ABA6CE"`, `data-pass-count="3"`,
`data-trace-hash="2412171679"`, and
`data-output="A sun rises brightly!"`. The DOM is a host presentation edge;
the three models, projection maps, solver, renderer, validator, trace
projection, and self-test remain project-owned Pascal.

## pas2js browser Building 3D

The graphical Building 3D workbench has its own dependency-free staging
entry point over the shared Pascal showcase, immutable view, and fixed-integer
command model:

```powershell
.\build-browser-building3d.ps1 -Compiler 'C:\path\to\pas2js.exe'
```

```bash
PAS2JS=/opt/pas2js/bin/pas2js bash ./build-browser-building3d.sh
```

The complete static site is written to `build/browser/building3d/www`.
Serve that directory and append `?selftest=1`; success is reported by
`data-state="solved"`, `data-self-test="passed"`, pipeline signature
`1:F1EF0EB6`, view signature `AC7290C0`, and face count `140`. The native SVG
host and full graphical contract are documented in
[`docs/building3d.md`](building3d.md).

## pas2js browser Training Studio

The Training Studio uses the shared corpus, training, workspace, and replay
units. Build and stage its static browser host with:

```powershell
.\build-browser-training.ps1 -Compiler 'C:\path\to\pas2js.exe'
```

```bash
PAS2JS=/opt/pas2js/bin/pas2js bash ./build-browser-training.sh
```

Serve `build/browser/training/www` and append `?selftest=1`. The test exercises
all five presets, source/run invalidation, public-token locks, contradiction
recovery, and stale-import rejection. The final overlapping-checkerboard
fixture must report `data-state="solved"`, `data-self-test="passed"`,
`data-source-signature="0FA2C5EA"`, `data-recipe-signature="DBCBA621"`,
`data-result-signature="947C4AFD"`, and `data-cell-count="16"`.
See [the Studio guide](training-studio.md) for the editing workflow and the
bounded synchronous execution policy.

## pas2js browser Music Studio

Music Studio uses the same corpus, persistent pass owner, independent
validator, exact score, MIDI exporter, and PCM/WAVE renderer as its native
and Node hosts:

```powershell
./build-browser-music.ps1 -Compiler 'C:/path/to/pas2js.exe'
```

```bash
PAS2JS=/opt/pas2js/bin/pas2js bash ./build-browser-music.sh
```

Serve `build/browser/music/www` and append `?selftest=1`. Its event-driven
workflow proves pending-input invalidation, an exact opening motif, an
ordinary failed edit, bounded selective repair, and recovery. Final body
attributes include `data-state="solved"`, `data-self-test="passed"`,
`data-composition-signature="216F6EBB"`, `data-score-signature="4167E7E5"`,
`data-midi-signature="86E4DCA3"`, `data-wave-signature="64679FF8"`,
`data-cell-count="16"`, `data-pass-count="3"`, `data-midi-bytes="123"`,
`data-wav-bytes="352844"`, and `data-audio-play-events="0"`.
The self-test prepares playable bytes but does not auto-play or save files.
See [the Studio guide](../examples/music/05_MusicStudio/README.md) for commands,
controls, limitations, and native/Node export behavior.

## Hosted pas2js gate

The hosted pas2js gate uses exact official upstream pas2js and FPC-source
revisions, verifies both source-archive SHA-256 digests, and caches the resulting
3.3.1 toolchain. It runs every portable conformance source, including the voxel
foundation, the recipe/run/compiler/runtime/result artifact suites, the token
lookup, recipe-validator, and pipeline-runner suites, eight music suites, three
causal-trace suites, and
the full and selective pass-negotiation suites; the tiled-world, learned-tiles,
learned-corpus, overlapping-pattern, sequence, anchored-completion,
three-pass-text, pass-composed-music, negotiated-music-variation, Music Studio,
spatial-dependency,
causal-trace-inspector, pass-negotiation, and negotiated-repair smoke tests;
and the multi-pass and selective-settlement worlds with both seed zero and
their default seeds under Node.js 22.23.2. The two Node pipeline hosts also run
the same exact 18-case process suite as the native hosts, using both the small
CLI fixture set and the learned-pattern-world bundle. The gate then builds
the Node training host and runs the same 25-case training process suite
against all four bundled training documents. The Music Studio form probe
checks its full 32-row CSV. It also builds all five browser targets,
serves each staged site, and checks their exact body-state contracts in
headless Chrome. A pinned development compiler is used
because the official 3.2.0 binary release cannot resolve the suite's portable
overloaded plain-procedure callback call.

## Continuous integration

The hosted workflow runs the checked native gate with FPC 3.2.2 on Linux,
macOS, and Windows. The Linux lane also builds the FPM and Lazarus packages,
runs the core Lazarus project, and verifies that generation leaves the checkout
clean. A separate Linux lane runs the complete core, pipeline-artifact,
token-lookup, recipe-validator, pipeline-runner, 2D, voxel-3D, Building 3D,
learning, sequence, music, pass-composition, causal-trace, full-negotiation,
and selective-negotiation pas2js/Node.js gate, including the negotiated-repair
host, the text-training/workspace suites and five Studio presets, plus all
five real browser self-tests in headless Chrome. A canary runs
against the current official FPC development image and records the image digest
and compiler revision in the job log. Submodules are deliberately disabled for
every gate.
