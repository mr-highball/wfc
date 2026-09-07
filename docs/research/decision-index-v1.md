# Indexed decision selection: exact replay, less repeated scanning

Status: implementation experiment in the maintained reference kernel, not a
new solver algorithm or a general performance guarantee. Project-owned Pascal,
MIT licensed. Solver algorithm version 2 and all artifact versions are unchanged.

## Question and hypothesis

Can decision selection scale with the cells changed by propagation, while
retaining every observable choice of the existing deterministic solver?

The scan baseline visits all `C` cells on every observation, even when only
one cell changed. For `C` independent unresolved cells it makes `C` decisions
and one final observation: `C*(C+1)` cell visits. With nonuniform weights it
also evaluates entropy `C*(C+1)/2` times. These are operation counts derived
from that specific fixture, not timings or claims about arbitrary WFC models.

The hypothesis is that an indexed heap improves large, sparsely changing
decision workloads without changing solutions, failed searches, random-call
bounds or results, reports, or causal traces. Dense propagation, large value
catalogs, global constraints, and small grids are explicit counterweights to
the hypothesis, not excluded failed measurements.

## Mechanism

The implementation is in `src/wfc_solver_reference.pas`; applications keep
using the existing `TGraph` facade. The heap contains precisely the unresolved
cells at observation time. Keys are:

- normalized unit weights: `(domain count, original CellOrder rank)`;
- other weights: `(existing EntropyQ16, original CellOrder rank)`.

There is no additional domain-count tie-break on the weighted path. Equal
Q16 entropies preserve traversal rank even if their exact real-valued entropies
or candidate counts differ.

The index is built bottom-up after initial propagation. Each candidate removal
and restoration marks its cell in a deduplicated dirty list, independent of the
propagation queue. At the next fixed point, each dirty cell's cached key is
updated and immediately repaired before another cached key is changed.
Comparisons never read live keys of other dirty cells. A solved cell leaves;
a restored unresolved cell re-enters with its original rank. Repairs can move
in either direction because Shannon entropy is not monotone under candidate
removal. Search frames, alternative order, domains, propagation order, and
trace emission are unchanged. Heap layout is derived, not trailed state.

With `D` distinct dirty cells, construction costs `O(C)`, a refresh costs
`O(D log C)`, and the next-cell lookup costs `O(1)`. Marking each individual
domain change is constant time. Weighted key refreshes still use the existing
fixed-point logarithm. Rounds with nearly all cells dirty can cost more than
the old linear observation scan.

Five Integer arrays plus one byte array add `21*C` payload bytes on the
tested 32-bit-Integer native targets (336 KiB at 16,384 cells), excluding
allocation headers. This is additional memory, not compressed domains.
JavaScript arrays do not have this native byte layout. `C*V` domain storage,
six `V*V` relation planes, removal trails, decision alternatives, global
constraint recounts, and retained traces are not optimized by this change.

## Frozen baseline and semantic acceptance

The baseline is the exact scan kernel from commit
`e1b91949bc85de18116901cd877c364ffbf4e9d5`, not a scan reimplemented after
seeing the heap's output. The current test and benchmark can be compiled
against that source by placing it in a separate unit-search directory.
Other required units come from the checkout; record their revision if they
have changed since this experiment. Use different output-unit directories
for every compiler, architecture, and baseline/candidate build.

For a manual comparison, create `build/decision-baseline/src`,
`build/decision-baseline/units`, `build/decision-baseline/bin`,
`build/decision-index/units`, and `build/decision-index/bin`. Save the output
of `git show e1b9194:src/wfc_solver_reference.pas` as
`build/decision-baseline/src/wfc_solver_reference.pas`. Then compile the same
maintained benchmark against each implementation:

```text
fpc -B -Mdelphi -O2 -Cr -Co -Ci -Sa -Fubuild/decision-baseline/src -Fusrc -Futools -FUbuild/decision-baseline/units -FEbuild/decision-baseline/bin tools/wfc_solver_benchmark.lpr
fpc -B -Mdelphi -O2 -Cr -Co -Ci -Sa -Fusrc -Futools -FUbuild/decision-index/units -FEbuild/decision-index/bin tools/wfc_solver_benchmark.lpr
```

Use the same FPC executable for both commands. The recorded Windows x64
measurements additionally pass `-Px86_64 -Twin64` to both. Check the build
logs to confirm the baseline really uses the archived unit, and run the two
executables with identical options from the matrix below. Do not reuse a
cached `.ppu` from another compiler or implementation.

[`wfc_decision_index_test.lpr`](../../test/wfc_decision_index_test.lpr)
serializes assignments, the solve return value and status, every report and
contradiction field, every trace-event field, and all random-call bounds and
returned tickets. Normal portable runs compare two bounded-integer digests
against baseline-only captured goldens. Native builds with
`-dDECISION_INDEX_TRANSCRIPT` additionally emit the full transcripts for
byte-for-byte comparison, removing hash-collision ambiguity. Captured,
disabled, and sink-only trace modes must agree on their applicable fields.
`DECISION_INDEX_CAPTURE` is an explicit baseline-authoring mode, not an
acceptance mode: never use it on changed code to bless a mismatch.

The corpus includes empty/singleton graphs, implicit and GCD-normalized unit
weights, nonuniform weights, shuffled traversal, masked initial domains,
locks, directed arcs, self-neighbors, required-only support, exclusions,
exhaustion and backtrack limits, connectivity, and overlapping quotas.
Coverage assertions require solved, contradictory and limited searches,
restoration, both entropy-change directions, all trace kinds and the relevant
global-constraint causes. This is finite differential evidence, not a proof
over every possible model. Existing public graph, trace, negotiation, pattern,
sequence, connectivity and quota conformance provide separate regression gates.

The final corpus has 25 groups, eight variants and seeds `{0, 1, 4, 7919}`:
800 cases and 57,297 checks on each tested native/browser target. One explicit
tie uses weights `[16777216, 1, 1]`: a two-value and a three-value domain both
have Q16 score zero. Reversing traversal order must select the larger domain
first in one variant. An accidental candidate-count tie-break therefore
cannot hide behind a coincidentally unchanged final solution.

FPC 3.2.2 and 3.3.1, each targeting Windows i386 and x86_64, produced identical
baseline/candidate full transcripts with SHA-256
`53E97FF83B3871739B64A1846FAD9C7FABA5B1C098612168328E876447B0CFEF`.
The same golden corpus passed in an actual Edge/pas2js 3.3.1 browser through
the included FPC serving/capture/checking tools. This is not a claim of local
Linux/macOS execution or pixel-level demo inspection.

## Reproducing whole-solve measurements

The native-only [`wfc_solver_benchmark`](../../tools/wfc_solver_benchmark.lpr)
is built by `build.ps1` and `build.sh`. It uses only repository units, FPC RTL,
and OS APIs. Its monotonic clock is shared with the included native browser
tooling; this benchmark opens no sockets and requires no browser or server.

```text
wfc_solver_benchmark --cells 4096 --values 4 --weights skewed --topology independent --compatibility dense --trace 0 --repeat 3
wfc_solver_benchmark --cells 256 --values 1024 --weights unit --topology line --compatibility equal --trace 0 --repeat 3
```

`--help` describes strict option values and explicit benchmark safety bounds;
these are not generation-duration or library-domain limits. `--trace 1`
includes in-memory event capture. The random stream resets to seed 1 for every
repetition. Equal compatibility on a line propagates an equality constraint;
on independent cells there are no edges for that relation to constrain.

Each timed interval is the complete `SolveReferenceModel` call, including
validation, allocations, propagation, decisions, reporting, and solver
destruction. Model construction, previous retained-output release, and result
checksums are outside it. Total/minimum/maximum elapsed milliseconds,
assignment checksum, random-draw count, and solver counters are printed.
Sub-clock-resolution results may be zero: zero does not mean zero work or
infinite speedup. Compare identical compiler flags and all non-timing fields.
The benchmark refuses an unsolved fixture or divergent repeat checksum/draw
count. Exact trace/RNG compatibility is established by the differential suite,
not by the timing program's shorter checksum.

The build gates version-smoke the tool and run a 32-cell weighted line with
trace enabled. CI has no timing threshold: shared-host scheduling is not a
deterministic solver contract.

## Recorded measurements, 2026-09-06

[All 240 raw rows](decision-index-v1.csv) are retained, including zeros and
cases where the index took longer. `stable64` is FPC
3.2.2-rrelease_3_2_2-0-g0d122c4953; `current64` is
3.3.1-20634-gd7f522a561. Both target Windows x86_64 on Windows 10 Home
10.0.19045, AMD Ryzen 5 1600 (six cores/twelve threads). Builds used
`-B -Mdelphi -O2 -Cr -Co -Ci -Sa -Px86_64 -Twin64`, with separate baseline
and candidate unit/binary directories and identical remaining unit paths.
No Linux/macOS or browser timings are inferred from these Windows results.

There are 60 fixed fixtures per compiler, each run on both kernels:

- Independent: `C={256,1024,4096,16384}`, `V={2,4}`, unit/skewed weights,
  trace off/on; respective repetitions `{8,2,1,1}`.
- Tiny controls: `C={1,8,32}`, `V=4`, both weights and trace modes,
  1,000 repetitions.
- Line controls: `(C,V)={(256,64),(128,128),(64,512),(16,1024)}`,
  both weights and dense/equal compatibility, trace off, one repetition.

Each executable first received a small warm-up invocation. Baseline and
candidate were interleaved per fixture, reversing order on alternate fixtures.
The stopping rule was the complete fixed matrix, not reaching a desired
ratio. Every pair matched status, solver counters, trace-event count,
random-draw count and assignment checksum. Large results are single-solve
observations, not distributions or confidence intervals. Other heavy local
builds were paused, but normal OS activity was not controlled.

Selected totals in milliseconds (the CSV records each minimum/maximum too):

| Compiler | Fixture | Repetitions | Scan | Index |
| --- | --- | ---: | ---: | ---: |
| 3.2.2 x64 | Independent 16,384 cells / 4 values, unit, no trace | 1 | 1,281 | 47 |
| 3.2.2 x64 | Same, skewed weights | 1 | 21,141 | 31 |
| 3.3.1 x64 | Independent 16,384 cells / 4 values, unit, no trace | 1 | 1,282 | 32 |
| 3.3.1 x64 | Same, skewed weights | 1 | 20,485 | 31 |
| 3.2.2 x64 | Independent 32 cells / 4 values, unit, no trace | 1,000 | 47 | 62 |
| 3.3.1 x64 | Independent 32 cells / 4 values, unit, trace | 1,000 | 47 | 63 |
| 3.3.1 x64 | Dense line 16 cells / 1,024 values, unit | 1 | 31 | 47 |
| 3.3.1 x64 | Equal-only line 16 cells / 1,024 values, unit | 1 | 266 | 250 |

The large independent fixtures support the stated selection-heavy hypothesis.
They have no adjacency work or backtracking and must not be presented as
end-to-end speedups for every music or world generator. The equal-only line
makes one decision and is dominated by unchanged support propagation.
Small/control differences are often approximately one observed clock quantum
(15–16 ms); they show no consistent universal win, not statistically proven
regression magnitudes. Tiny/fast rows with zero elapsed remain in the CSV and
are not assigned speedup ratios. Totals sum separately timed, millisecond-
quantized solves rather than a single high-resolution batch interval.

The published CSV normalizes line endings only; all values from the raw run
are preserved. The benchmark and complete fixed parameter matrix above are
the reproduction interface; generated executables and browser profiles are
not required or distributed as research assets.

## Limits and next experiments

- A decision-index improvement is not evidence that pass-based models beat
  equivalent flattened models. That remains a separate controlled experiment.
- High-value support scans, dense dirty batches, connectivity and quota
  propagation, branch histories, and public graph compilation can dominate
  application runtime. This patch does not change them.
- Tiny workloads may lose to allocation and heap overhead. Publish controls,
  not only the fastest ratio. Host/compiler effects require remeasurement.
- The byte-domain backend is unchanged. Packed/sparse relation support and
  genuinely chunked spatial solving need their own correctness and resource
  experiments.
- This finite benchmark does not establish latency guarantees, cancellation,
  constant-memory infinite-world generation, or unrestricted musical duration.
