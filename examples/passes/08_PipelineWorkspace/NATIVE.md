# Native Pipeline Workspace

The native FPC host uses the same `pipeline_workspace_workbench`, `pipeline_workspace_view` and `pipeline_workspace_presets` as the [browser editor](README.md). It builds an actual initial workspace from complete imported definitions or either included preset, then exports canonical artifacts and an exact selected-pass SVG. It does not replace the existing `wfc_workspace` history-authoring CLI.

From the repository root:

```powershell
.\build-workspace.ps1 -Compiler 'C:/path/to/fpc.exe'
.\build\workspace\native\bin\PipelineWorkspace.exe --help
```

```sh
FPC=/path/to/fpc bash ./build-workspace.sh
./build/workspace/native/bin/PipelineWorkspace --help
```

The standalone builder produces `PipelineWorkspace`, `wfc_workspace` and `wfc_serve` in `build/workspace/native/bin`, with `.exe` on Windows. It starts no server or solve. The normal root native gate also produces them in `build/native/bin`. FPC is the only native compiler/runtime prerequisite; browser use additionally compiles with pas2js. See the [browser guide](README.md#build-and-serve) for explicit FPC hosting.

The host depends on FPC `Classes`/`SysUtils`, project runtime units, the three shared demo units and the included `tools/wfc_atomic_new_file.pas`. There are no external packages, alternate language runtimes, downloaded corpora or test-fixture dependencies. Its filesystem host is intentionally native-only; the shared workbench, presets and view remain usable by a separately built pas2js entry point.

## One initial execution, complete artifacts

Choose **one** definition source:

```text
PipelineWorkspace.exe --preset landscape --output-dir NEW_DIRECTORY
PipelineWorkspace.exe --preset sequence --output-dir NEW_DIRECTORY
PipelineWorkspace.exe --recipe RECIPE_FILE --run RUN_FILE --output-dir NEW_DIRECTORY
```

The parent directory must already exist; `NEW_DIRECTORY` must not exist. An explicit `--output-dir` requests all four fixed output filenames:

- `recipe.wfc`: the complete canonical recipe copied from the actual workbench.
- `run.wfc`: the complete canonical applied run copied from that workbench.
- `journal.wfc`: the complete canonical journal with the accepted begin-epoch and initial-attempt actions and exact captured evidence.
- `selected-pass.svg`: the shared renderer's exact selected local XY window at one Z, carrying real local/world coordinates and explicit currentness labels.

The host calls `BeginEpoch` and `ExecuteInitial` on **the same shared workbench**. It does not allocate or manipulate a graph directly, replace outputs, synthesize solver results, omit a failed initial attempt, retry, expand search budgets or reseed. Presets perform actual rule-model construction/inline sequence learning; their builders do not pre-generate output. Imported documents go through the complete canonical graph-free inspector before the actual begin/initial workflow.

Normal `Solved=False` is recorded and exported with its real state. The SVG is then explicitly `NOT CURRENT`, not a safe or successful output. A failed initial attempt has no successful baseline; the saved history still faithfully records the attempt. Failure to serialize, satisfy an output/resource policy, render the requested window or publish a file is an exception, not a recorded-unsolved success.

Importing `recipe.wfc` and `run.wfc` starts a **new** history. It does not restore the old journal. Continue an existing history through the included workspace CLI described below.

## Composition geometry versus presentation

The two preset builders have fixed role counts but caller-defined layouts. There is no host maximum width, height, depth or duration. Existing versioned geometry/run/model bounds and caller-selected logical policy allowances still apply; this is not an infinite-memory or solve-feasibility promise.

For preset mode, repeat this option for distinct pass indices:

```text
--pass INDEX,RANK,CX,CY,CZ,OX,OY,OZ,PX,PY,PZ,WRAP
```

The twelve fields are pass index, rank, three local cell counts, three signed world origin coordinates, three positive world pitches and wrap `0|1`. Numbers are canonical decimal: no leading plus, whitespace, leading zero padding, fractions or exponent notation. Signed origins may be negative, including the signed Integer boundary when geometry permits it. Rank1 requires local Y/Z = 1; rank2 requires local Z = 1; all world axes remain meaningful. Explicit windows and layout rows are never silently clipped or normalized.

Unspecified pass rows retain that preset's **documented illustrative default**. A changed row does not automatically resize, align or re-origin its providers, projection or aliases. Supply all linked rows when resizing a learned sequence. Invalid source/projection/alias geometry is rejected by the core rather than resampled.

Landscape roles are terrain 0, foliage 1, housing 2. Its default extents are 8×6, 32×24, 3×2 with pitches 4, 1, 8 and housing inset 4, totaling 822 actual cells. Full-cell land and clear-coverage clauses are discrete model predicates, not a universal physical building-safety validator. The default leaves house/vacant and terrain/foliage choices open; weights favor useful clear/land regions but do not force a house or guarantee repair.

Sequence roles are independent public grid 0, private learned states 1, public projection 2, exact public alias 3. Default sequence extents are 12 cells for each linked pass; the separate grid is 5×3, totaling 51 allocated-pass cells including alias storage. The project-authored MIT inline `step/turn/rest` corpus is actually learned, not a synthetic output or a neural-model dependency. Order/boundary/extent constraints and finite training vocabulary determine possible paths; a requested length is not a guarantee of variation, musical quality or feasibility.

Presentation options are separate:

```text
--view PASS,X,Y,Z,WIDTH,HEIGHT
--cell-pixels N
--limit view-cells=N
--limit svg-bytes=N
```

Without `--view`, the explicit host default chooses the first public pass, local origin/Z = 0, width `min(8,pass width)` and height `min(6,pass height)`. This is a default presentation window, not a composition-size cap. A supplied `--view` must fit exactly; it is never truncated to 8×6 or to the viewport. Default cell pixels 32, maximum rendered cells 4096 and SVG bytes 8388608 are all adjustable. Private state passes cannot be selected as a public grid. A definition with no public pass is valid for other library workflows but not this visual demo host.

The SVG shows one local grid slice. It is not a composited world renderer, photorealistic terrain, independent footprint validator or current-output claim when generation failed. It always uses `UseBaseline=False`: after an initial attempt there is no prior historical baseline to request. Cell/label markup escaping and world-footprint metadata come from the shared renderer; the native host does not recreate its rendering logic.

## Options and strictness

Preset-only run options:

```text
--seed N
--strategy one-way|negotiated
--local-backtracks N
--pass-backtracks N
--trace 0|1
```

Defaults are seed 7, negotiated, 128 local and 16 pass backtracks, no trace. Seed accepts the entire Cardinal range 0..4294967295. Both backtrack budgets may be **zero**; existing run-format maxima apply. There is no hidden seed search or fallback strategy.

Landscape-only `--weights LAND,WATER,CLEAR,TREE,HOUSE,VACANT` accepts positive Integer weights; defaults 32, 1, 32, 1, 8, 1. Sequence-only `--order N`, `--boundary open|wrap`, and `--extent whole|prefix|suffix|fragment|wrap` control the real learner and projection extent; defaults 2/open/whole. Source observations and structural graph wrapping are explicit, separate choices. Wrapped graph rows need wrap extent; incompatible requests are not rewritten for the caller.

Imported recipe/run mode refuses every preset-only option, including geometry, seed, strategy, trace and weights. This prevents a silently altered invocation from being presented as the imported artifact. Construct a desired complete run through the public Pascal model/run API or workbench controller if its inputs/options should change. The import accepts legacy or mapped formats supported by the existing inspector; it does not upgrade a legacy artifact solely to fit the demo.

Every option requires a value except standalone `--help`. Unknown options, empty fields, conflicting modes, duplicate scalar options, duplicate pass indices, duplicate named limits and irrelevant preset-specific options reject. Option order is immaterial: pass overrides are applied only after the selected preset's defaults exist. No provenance string is treated as a local path or URL; only the two explicit input paths are read. Files are read without trimming, newline conversion or comment stripping, with recipe+run bytes charged against the shared context-text allowance before their inspector.

## Caller-adjustable policy

`--limit NAME=N` overrides one positive Integer limit. All policy versions are explicitly 1; unsupported format versions are not CLI toggles. Defaults match the included workspace CLI's policy so the saved small initial histories can be continued with that CLI's defaults. The host initializes **every** policy field, not an incomplete record with implicit magic values.

| Limit name | Default | Core policy field |
| --- | ---: | --- |
| recipes | 1024 | Journal.MaxRecipes |
| runs | 16384 | Journal.MaxRuns |
| context-bytes | 67108864 | Journal.MaxContextTextBytes |
| actions | 16384 | Journal.MaxActions |
| roots | 65536 | Journal.MaxRootReferences |
| evidence-bytes | 67108864 | Journal.MaxEvidenceTextBytes |
| journal-bytes | 268435456 | Journal.MaxEncodedTextBytes |
| input-cells | 1048576 | Replacement.MaxRetainedCellRecords |
| input-values | 4194304 | Replacement.MaxRetainedValueItems |
| input-visits | 64000000 | Replacement.MaxCandidateVisits |
| public-cells | 1048576 | Outcome.MaxPublicCellRecords |
| token-bytes | 16777216 | Outcome.MaxEncodedTokenBytes |
| report-passes | 65536 | Outcome.MaxReportPassRecords |
| trace-events | 1048576 | Outcome.MaxTraceEvents |
| excluded-values | 4194304 | Outcome.MaxExcludedAssignmentItems |
| outcome-bytes | 33554432 | Evidence.MaxTextBytes |
| outcome-lines | 1048576 | Evidence.MaxLines |
| epochs | 1024 | Replay.MaxEpochs |
| solves | 16384 | Replay.MaxSolveActions |
| instantiated-cells | 16777216 | Replay.MaxInstantiatedCellRecords |
| replay-evidence-bytes | 67108864 | Replay.MaxEvidenceTextBytes |
| view-cells | 4096 | Slice.MaxRenderedCells |
| svg-bytes | 8388608 | Slice.MaxSvgBytes |

These limits are logical input, history, capture, work or encoded-output allowances according to the corresponding core API. They are not one combined peak-heap budget, a wall-clock deadline or a guarantee that the solver's previously allocated reports fit into memory. The host retains several complete text/state representations while generating the export; it does not claim streaming journal capture. View limits constrain presentation, not all graph cells or the detached state copy made before rendering. Large compositions can require deliberately larger workspace limits even when only a small window is rendered.

## Example invocations

These Windows examples assume the executables have been built, the shell is in `build/workspace/native/bin` (or uses that prefix for each executable), and `D:\WfcSamples` already exists. On other platforms omit `.exe` and use an existing local parent directory. Use a fresh output directory name for every invocation.

Default landscape, inspecting the actual housing grid:

```powershell
.\PipelineWorkspace.exe --preset landscape --output-dir D:\WfcSamples\landscape-01 --view 2,0,0,0,3,2
```

A larger explicitly aligned landscape with signed origins, independent pass extents and a 35-cell housing viewport:

```powershell
.\PipelineWorkspace.exe --preset landscape --output-dir D:\WfcSamples\landscape-02 --seed 55 --pass 0,2,16,12,1,-32,-24,0,4,4,1,0 --pass 1,2,64,48,1,-32,-24,0,1,1,1,0 --pass 2,2,7,5,1,-28,-20,0,8,8,1,0 --view 2,0,0,0,7,5 --local-backtracks 256 --pass-backtracks 32
```

A 64-cell learned sequence, with all three linked grids explicitly resized; SVG shows 32 cells, **not** a repeated or shortened 64-cell invocation:

```powershell
.\PipelineWorkspace.exe --preset sequence --output-dir D:\WfcSamples\sequence-01 --pass 1,1,64,1,1,11,-3,5,3,2,1,0 --pass 2,1,64,1,1,11,-3,5,3,2,1,0 --pass 3,1,64,1,1,11,-3,5,3,2,1,0 --view 2,0,0,0,32,1
```

Start a new initial execution from the complete exported definitions, preserving their seed/options/layouts:

```powershell
.\PipelineWorkspace.exe --recipe D:\WfcSamples\landscape-01\recipe.wfc --run D:\WfcSamples\landscape-01\run.wfc --output-dir D:\WfcSamples\landscape-copy --view 2,0,0,0,3,2
```

## Continue the saved history using existing tools

This demo deliberately does not duplicate the seven `wfc_workspace` commands. Inspection distinguishes unverified stored claims from execution; replay verifies complete actual evidence bytes. For a successful initial landscape journal, these commands inspect it, verify it, preview an explicit terrain-root repair and save that repair to a **new** journal file:

```powershell
.\wfc_workspace.exe inspect --input D:\WfcSamples\landscape-01\journal.wfc
.\wfc_workspace.exe replay --input D:\WfcSamples\landscape-01\journal.wfc
.\wfc_workspace.exe preview --input D:\WfcSamples\landscape-01\journal.wfc --run D:\WfcSamples\landscape-01\run.wfc --roots 0
.\wfc_workspace.exe repair --input D:\WfcSamples\landscape-01\journal.wfc --run D:\WfcSamples\landscape-01\run.wfc --roots 0 --output D:\WfcSamples\landscape-01\repair-01.wfc
```

The root 0 choice is explicit authority for terrain and its downstream dependents; it is not inferred from a UI selection. To author changed locks/domains, first construct a complete canonical desired run, then use `wfc_workspace edit`, followed by preview/repair with that same desired input set and explicit roots. Every accepted edit remains a separate action, including lock followed by clear. For the sequence preset, root 1 is the private producer; editing its projected alias does not silently authorize root 1 repair.

A failed initial attempt has no successful baseline and cannot be selectively repaired merely by picking broader roots. Use `wfc_workspace begin --input OLD_JOURNAL --recipe RECIPE --run CORRECTED_RUN --output NEW_EPOCH_JOURNAL`, then `wfc_workspace initial --input NEW_EPOCH_JOURNAL --output NEW_ATTEMPT_JOURNAL`. This retains the old history and makes the new epoch explicit. The core/CLI, not this demo, decide scope permission and exact replay truth. Apply larger matching limits to continuation commands when the saved history needs them.

## Publication, errors and exit status

All definition validation, graph work, complete canonical serialization and SVG generation finish before the host attempts to create the output directory. `CreateDir` is used for exactly that requested new directory; no parent chain is silently created. Every file then uses the existing `TWfcAtomicNewFile` helper: an exclusive sibling temporary file, completed writes, then OS no-replace publication. A file that appears concurrently is not overwritten. Use a trusted existing parent directory; this is not a hostile filesystem/symlink sandbox.

The four files are published sequentially, **not an atomic directory transaction**. If the third publication fails, the first two may remain as complete files. The host reports each completed path, retains the requested directory, reports the published-file count on failure and does not recursively delete anything. Its helper handles only its own exact temporary sibling cleanup. No crash-durable directory fsync is promised. Retry with a new directory; never reinterpret a partial export as a complete saved workspace.

| Exit | Meaning |
| --- | --- |
| 0 | Initial attempt solved and all requested files published; also standalone help |
| 2 | CLI usage, mode, duplicate, output-directory preflight or selected-window error |
| 3 | Invalid definition, geometry, typed preset/view error or other uncategorized validation exception |
| 4 | Workspace replay/authoring refusal, including relevant policy/scope failures |
| 5 | Native file/directory/stream I/O error; check reported retained outputs |
| 10 | Initial attempt was normally unsolved, its exact history and NOT CURRENT SVG were successfully exported |

Exit 10 is set only after **all four** publications succeed. A later I/O failure cannot masquerade as an exported unsolved history. Source, helper and imported definitions are never modified. No process is left running after this CLI returns.

## Reproduce functional checks

The maintained native gate runs `pipeline_workspace_native_process_test` against this real executable, the existing workspace CLI, and `pipeline_workspace_native_fixture`. The independent fixture decodes canonical recipe/run/journal documents, replays their complete evidence through the public API and checks actual cells, layouts and SVG metadata. It does not trust the host's printed status alone.

Focused checked stable FPC 3.2.2 and trunk 3.3.1 verification passed on Win32 and Win64: each target ran 37 real child-process cases and 468 harness checks, plus 981 independent fixture checks. The 28 complete artifacts from seven export directories matched byte for byte across all four targets. Cases include both presets, larger signed layouts, a 64-cell sequence with a 32-cell view, identical-definition import, zero solve budgets, explicit unsolved exit10, policy refusals, invalid/duplicate arguments and preserved existing output paths. These checks do not establish hostile-filesystem race protection, crash durability, visual quality or universal solve success.

The process harness takes exactly four positional arguments: `HOST_EXE WORKSPACE_CLI_EXE FIXTURE_EXE NEW_WORK_DIRECTORY`. Its final directory must not exist; its parent must exist. It retains per-child stdout/stderr, argv, statuses and outputs. Expected refusal/unsolved child statuses are individually asserted; the overall harness must still return success. See [development tools](../../../docs/development-tools.md#build-a-complete-pipeline-workspace-example) for the integrated command and [workspace contracts](../../../docs/pipeline-workspaces.md) for history/ownership details.
