# Building and testing

The dependency-free build covers the core, causal-trace kernel/public/utility
contracts, specialized 2D and settlement, radius-one model-learning,
overlapping-pattern and projected-pattern passes, sequence, Unicode-scalar
text completion, three-pass text composition, exact music-score, music
projection, Standard MIDI File,
score-export, deterministic PCM/WAVE rendering, Music Studio, synchronized
polyphonic frames and training, the ensemble pass owner and Ensemble Studio,
independent voice training/graphs/frontiers and Voice Studio,
negotiated music variation/result replay, full and selective
pass negotiation, count-range pass clauses, rooted port connectivity, deterministic whole-transaction
restarts and optional diagnostic timing, canonical numeric text primitives, immutable authored-rule
models, declarative pipeline recipes, recipe-bound run artifacts, the closed
pipeline compiler/runtime, immutable public result artifacts, strict run/result
codecs, the recipe validator and pipeline runner applications, the canonical
learned-pattern-world recipe/run/result bundle, and voxel-3D
units and their conformance suites, plus
the checked voxel pass bridges, learned-volume terrace owner, and multi-pass Building 3D owner/validator,
pass-aware view, fixed-integer isometric projector, and canonical SVG encoder.
It also runs seeded smoke checks of the portable console demos, including the
causal-trace inspector, bounded pass negotiation, negotiated descendant repair,
anchored text infill, three-pass text composition, and negotiated music
variation, the four-pass learned-pattern world, and writes a checked seed-zero
Building SVG artifact.
The original A-major and manually authored riff studies are also plain FPC
console programs, with owned MIDI and streaming WAVE export. No submodule,
external media library, or engine package is needed. The 2D world, three-pass text workbench,
Building 3D, Training Studio, Music Studio, Ensemble Studio, Voice Studio,
Neighborhood Counts, Connected Routes, and Learned Terraces have separate pas2js browser entry
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
portable project units. Causal capture, live delivery, bounded recent-event
windows, hashing, formatting, query, and validation are likewise project-owned
Pascal in `wfc`, `wfc_trace`, and `wfc_trace_stream`.
The `wfcpipeline=1`, `wfcpipeline-run=1`, and `wfcpipeline-result=1` codecs,
recipe compiler, transactional runtime, result capture, validator application,
and command-line parsing are also shared native/pas2js Pascal. Native
hosts add only bounded byte I/O and process-exit plumbing.

Presentation adapters may use direct host APIs behind project-owned Pascal
interfaces. A separately fetched media, window, or engine library is not a
standard demo prerequisite; the former legacy integrations have been removed.
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
`wfc_text_test`, `wfc_text_passes_test`, `wfc_text_pass_transaction_test`,
`wfc_negotiation_test`,
`wfc_selective_negotiation_test`, `wfc_restart_test`, `wfc_timing_test`,
`wfc_restart_demo_test`, `wfc_pass_count_test`, `wfc_pipeline_count_test`,
`wfc_connectivity_reference_test`, `wfc_connectivity_test`,
`wfc_connectivity_trace_test`, `wfc_connectivity_demo_test`,
`wfc_count_demo_test`, `wfc_voxel3d_test`,
`wfc_voxel3d_isometric_test`,
`wfc_voxel3d_svg_test`, `wfc_voxel3d_model_passes_test`,
`wfc_terraces3d_test`, `wfc_terraces3d_view_test`, `wfc_building3d_test`,
`wfc_building3d_view_test`, `wfc_midi_smf_test`,
`wfc_music_test`, `wfc_music_studies_test`,
`wfc_music_graph_test`, `wfc_music_midi_test`, `wfc_music_passes_test`,
`wfc_music_passes_text_test`, `wfc_music_audio_test`, `wfc_music_studio_test`,
`wfc_music_midi_import_test`, `wfc_music_training_test`,
`wfc_music_arrangement_test`, `wfc_music_audio_stream_test`,
`wfc_music_studio_arrangement_test`, `wfc_music_import_app_test`,
`wfc_music_ensemble_test`, `wfc_music_ensemble_graph_test`,
`wfc_music_ensemble_passes_test`, `wfc_music_ensemble_training_test`,
`wfc_music_ensemble_demo_test`,
`wfc_sequence_segment_test`, `wfc_music_ensemble_stream_test`,
`wfc_music_ensemble_audio_test`, `wfc_music_ensemble_stream_demo_test`,
`wfc_midi_stream_test`, `wfc_music_ensemble_midi_test`,
`wfc_music_ensemble_midi_stream_demo_test`,
`wfc_sequence_partial_projection_test`, `wfc_music_voices_training_test`,
`wfc_music_voices_graph_test`, `wfc_music_voices_stream_test`,
`wfc_music_voices_demo_test`,
`wfc_browser_dom_test`, `wfc_browser_args_test`, `wfc_browser_socket_test`,
`wfc_browser_websocket_test`, `wfc_browser_cdp_test`, `wfc_browser_capture_test`,
`wfc_serve_test`,
`wfc_text_codec_test`, `wfc_rule_model_test`,
`wfc_rule_text_test`, `wfc_pipeline_model_test`, `wfc_pipeline_text_test`,
`wfc_pipeline_run_test`, `wfc_pipeline_run_text_test`,
`wfc_pipeline_compile_test`, `wfc_pipeline_result_test`,
`wfc_pipeline_result_text_test`, `wfc_pipeline_runtime_test`,
`wfc_token_lookup_test`, `wfc_validate_app_test`, `wfc_run_app_test`,
`wfc_training_test`, `wfc_training_text_test`, `wfc_learn_app_test`,
`wfc_learn3d_test`, `wfc_model3d_text_test`, `wfc_training3d_test`,
`wfc_training_volume_workspace_test`,
`wfc_text_training_test`, `wfc_training_workspace_test`,
`wfc_learned_pattern_world_bundle_test`,
`wfc_trace_reference_test`, `wfc_trace_reference_stream_test`,
`wfc_trace_test`, `wfc_trace_stream_test`, `wfc_trace_window_test`,
`wfc_trace_utility_test`, `wfc_trace_layout_test`, and
`wfc_trace_inspector_test`, then compile and smoke-test the
portable console examples with seed `0`, including the bounded/wrapped spatial
dependency self-check, causal-trace inspector, bounded pass-negotiation proof,
negotiated descendant repair, anchored text completion, three-pass text
composition, negotiated music variation, the learned-pattern world, and
depth-three Building 3D pipeline, all six Training Studio presets, and the
Music Studio generation/repair/audio self-test. The Deterministic Restarts
console host checks four policies, exact transcripts, independently sampled
outputs, and failed-attempt rollback; its shared helper also runs in the
portable browser conformance suite. See [restarts and timing](restarts.md).
The 32-case Music Studio form
probe is compared with its checked-in raw CSV;
the FPC server also runs live HTTP integration checks, and the native
`MusicStudioRender` process suite exercises requested durations, frame counts,
no-overwrite publication, and a competing destination created during export;
the multi-pass and
selective-settlement worlds also run with their default seeds. Finally, the
native `Building3DSvg` host generates and validates
`build/native/bin/building3d-seed-zero.svg`. The learned-volume host also
writes `build/native/bin/terraces3d.svg`; its three passes, transaction checks,
and deterministic presentation are covered by the same native gate.
The two original music studies are built as `simple_a_major` and
`simple_song_riffs`. Their shared `wfc_music_studies_test` runs on FPC and
pas2js; the FPC `wfc_music_studies_process_test` checks command arguments,
user-selected note counts and exact fractional-second WAVE timing,
deterministic MIDI/WAVE files, format errors, and no-overwrite publication.
See the [A-major](../examples/music/01_simple_A_major/README.md) and
[riff](../examples/music/02_simple_song_riffs/README.md) usage guides.
A checked process suite also runs both native pipeline tools against the
canonical files in `test/fixtures/pipeline-cli` and
`examples/2D/05_LearnedPatternWorld/pipeline`: file and standard-input paths,
exact validator and result bytes (including the domain-sized bundle), quiet
output, and the documented invalid, usage, I/O, solved, and non-solved exit
classes are all exercised.
A second, 30-case process suite checks `wfc_learn`, `wfc_validate`, and
`wfc_run` against all five source/model/recipe/run/result bundles in
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

The native builds expose the same project-owned application
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
`build/native/bin/wfc_learn[.exe]`. Portable application logic is also tested
in real browsers through the [included FPC tools](development-tools.md).
The native training entry is named
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

The maintained JavaScript execution target is the browser. Use the dedicated
staging scripts below and the included [FPC development tools](development-tools.md).

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
build/native/bin/wfc_serve --root build/browser/world2d/www --port 8080
```

Build the server with the native gate first; use `wfc_serve.exe` on Windows.
The server binds to loopback by default. See
[development tools](development-tools.md#access-from-a-trusted-local-network)
for opt-in trusted-LAN serving on one explicit address.

Open `http://localhost:8080/` for the interactive viewer. The deterministic
browser fixture at `http://localhost:8080/?selftest=1` succeeds only when the
body finishes with `data-state="solved"`, `data-self-test="passed"`, and the
seed-zero signature
`data-signature="1:5B0DD75D:08022AF1:A40D0955"`. Generated JavaScript and
compiler units remain under the ignored `build/` tree; source distributions do
not commit them.

## pas2js browser text passes

The three-pass text workbench compiles the same showcase owner and validator
used by native FPC, then stages its HTML, CSS, and generated
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

## pas2js Learned Terraces

Build `build-browser-terraces.ps1` or `bash ./build-browser-terraces.sh`, then
serve `build/browser/terraces/www` with the included FPC server. The page
provides seed and dimension inputs, per-cell terrain/foliage constraints,
scoped repair, rotation, and SVG download. Its `?selftest=1` page verifies
scene `1:6D695B99:2D23CF62`, view `C3D25917`, invalidation, selective
preservation, failure recovery, and new-session generation. See the
[demo guide](../examples/3D/04_LearnedTerraces/README.md) and
[library contract](learned-terraces3d.md).

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
all six presets, full-depth volume slices and XYZ locks, source/run invalidation,
public-token locks, contradiction
recovery, and stale-import rejection. The final overlapping-checkerboard
fixture must report `data-state="solved"`, `data-self-test="passed"`,
`data-source-signature="0FA2C5EA"`, `data-recipe-signature="DBCBA621"`,
`data-result-signature="947C4AFD"`, and `data-cell-count="16"`.
See [the Studio guide](training-studio.md) for the editing workflow and the
bounded synchronous execution policy.

## pas2js browser Music Studio

Music Studio uses the same corpus, persistent pass owner, independent
validator, exact score, MIDI exporter, and PCM/WAVE renderer as its native
command-line host:

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
Its separate cooperative long-composition checks require
`data-arrangement-test="passed"`, exact frame counts for 4, 6, and 180 seconds,
and cancellation, stale-result, write-failure, and final-commit markers.
These lengths are test cases, not a duration policy. A counting fake browser
file backend exercises asynchronous save lifecycle without touching disk.
See [the Studio guide](../examples/music/05_MusicStudio/README.md) for commands,
controls, limitations, and native export behavior.

## pas2js browser Ensemble Studio

Build the polyphonic studio with `build-browser-ensemble.ps1` or
`bash ./build-browser-ensemble.sh`, using the same compiler override conventions
as the other browser staging scripts. Serve `build/browser/ensemble/www` with
the included FPC server. Its shared Pascal helper and controller event path are
also exercised by portable browser conformance.

The native gate builds `EnsembleStudio` and runs `--selftest`, then checks the
streaming `EnsembleStudioRender` and two-pass `EnsembleStudioMidiRender` hosts
with native process tests. The
[example guide](../examples/music/06_EnsembleStudio/README.md) describes controls,
native exports, user-defined score lengths, and the separate bounded audio
preview. [The ensemble contract](music-ensemble.md) specifies voice continuity,
training selections, exact versus allowed harmony, and transaction behavior.
The separate [streaming contract](music-ensemble-stream.md) describes bounded
local generation, sustained voices, incremental PCM, and exact length accounting.
The [MIDI streaming contract](music-midi-stream.md) adds bounded event counting
and deterministic forward-only replay, with transport-specific duration bounds.
The browser runner requires `data-stream-self-test=passed`,
`data-stream-release=passed`, `data-midi-stream-self-test=passed`, and
`data-midi-stream-release=passed` on the
ensemble stream controller test in addition to the synchronous harness marker.
An unawaited or unfinished file transaction cannot satisfy that gate.

## Connected Routes browser and native hosts

For the spatial connectivity workbench, build
`build-browser-connectivity.ps1` or `bash ./build-browser-connectivity.sh`
and serve `build/browser/connectivity/www` with the included FPC server.
The native gate builds `ConnectedRoutes`, runs its selftest, and exercises
both 2D and multi-floor SVG exports plus no-replace behavior in
`wfc_connectivity_process_test`. See its
[host guide](../examples/passes/06_ConnectedRoutes/README.md) and
[constraint contract](connectivity.md).

## Voice Studio browser and native hosts

The independent-role studio stages the same portable training, graph,
continuation and export helpers used by the native host:

```powershell
.\build-browser-voices.ps1 -Compiler 'C:/path/to/pas2js.exe'
.\build\native\bin\wfc_serve.exe --root build/browser/voices/www --port 4179
```

```bash
PAS2JS=/opt/pas2js/bin/pas2js bash ./build-browser-voices.sh
build/native/bin/wfc_serve --root build/browser/voices/www --port 4179
```

Open `http://127.0.0.1:4179/`; append `?selftest=1` for the browser fixture.
Build the included server with the native gate first. The browser test's
asynchronous transaction gates include `data-voice-stream-self-test=passed`
and `data-voice-stream-release=passed`, in addition to its ordinary test
marker. A synchronous test return alone is insufficient.

The native `VoiceStudioRender` host selects `--format wave` or `--format midi`
and accepts user-defined `--seconds`, an unsigned `--seed`, and a new `--output`
path. For example:

```bash
build/native/bin/VoiceStudioRender --format wave --seconds 8 --seed 1 --output voices.wav
build/native/bin/VoiceStudioRender --format midi --seconds 8 --seed 1 --output voices.mid
```

Append `.exe` on Windows. The native host does not replace an existing output.
The [example guide](../examples/music/07_VoiceStudio/README.md) describes search
budgets, streaming progress, and browser file support. See
[Independent Voices](music-voices.md) for reusable APIs and
[the research record](research/independent-voices-v1.md) for the separate
graph truth table and authored temporal corpus.

`wfc_music_voices_browser_test` exercises the asynchronous browser controller
and file helper only in the browser gate; it is not a native no-op test.

## Neighborhood Counts browser workbench

Stage the shared terrain/roads/market count demo with
`build-browser-counts.ps1` or `bash ./build-browser-counts.sh`. Serve
`build/browser/counts/www` with the included FPC server and append
`?selftest=1`. Its checked contract includes the solved default output key,
lower/upper contradictions, flood rejection, wrapped alias modes, bounded
selective road repair, and pending-input invalidation. The native gate builds
the three focused count suites and runs `NeighborhoodCounts --selftest`.
See the [demo guide](../examples/passes/04_NeighborhoodCounts/README.md) for
commands, the complete marker contract, and explicit scope limits.

## Hosted pas2js gate

The hosted browser gate builds the ten pas2js demos with a matching compiler
and RTL. It serves standalone demo self-tests and the ensemble/independent-voice
controllers' portable conformance hosts with the project-owned FPC server,
executes them in headless Chrome, waits for complete rendered body contracts
with the FPC capture client, and independently checks the saved DOM with the
FPC checker. Both conformance and standalone demos use this same maintained
runner; no virtual-time snapshot is used as completion evidence.
It also compiles every portable standalone conformance program for the browser
and executes those pages through the same FPC tools. An additional browser-only
entry regression loads all ten actual demo pages and their self-test queries,
including the HTML bootstrap and awaited controller checks. Native socket, DOM-parser,
and renderer-process tests remain native. The source-derived test manifest
rejects missing staged programs; see [development tools](development-tools.md)
for the reproducible staging and runner commands.
The native gate runs the full conformance suite, command-line process tests,
and reproducible research fixtures. Historical pas2js parity measurements are
not assertions that every console fixture has a browser host.

## Continuous integration

The hosted workflow runs the checked native gate with FPC 3.2.2 on Linux,
macOS, and Windows. The Linux lane also builds the FPM and Lazarus packages,
runs the core Lazarus project, and verifies that generation leaves the checkout
clean. A separate Linux lane builds all ten browser demos and executes
standalone demo self-tests plus portable browser conformance (including the
Ensemble Studio and Voice Studio controllers) in headless Chrome using the included FPC
development tools. A canary runs
against the current official FPC development image and records the image digest
and compiler revision in the job log. The checkout contains no submodules.

On 2026-09-06 UTC, [CI run 34005958438](https://github.com/mr-highball/wfc/actions/runs/34005958438)
passed all five jobs for `b3bd1aea028ce908075b30d1dbe4731359f4c285`:
stable FPC 3.2.2 on Linux, macOS, and Windows; development FPC on Linux;
and the pinned pas2js browser lane. The successful gate includes Linux package
and clean-checkout checks, native browser completion/rejection fixtures, and
the real browser demo-entry checks. This is evidence for that revision, not
a claim that all roadmap work is complete.

The earlier [dated native verification record](verification/hosted-native-2026-09-06.md)
links the observed compiler matrix, test coverage, package checks, and known
browser-lane failure at that revision. It is historical evidence, not a
substitute for the current workflow result.
