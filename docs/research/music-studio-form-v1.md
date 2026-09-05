# Music Studio form experiment v1

Status: reproducible modeling experiment, not a solver-performance claim.
All corpus material and experiment code are project-authored under MIT.

## Question and hypothesis

When harmony, rhythm, and melody are learned separately from short aligned
phrases, does fixing the complete rhythmic form make it easier for bounded
chronological pass negotiation to find a jointly valid result in this fixture?

Hypothesis: preserving the two-bar attack/hold/rest form will reduce
incompatible provider combinations enough to increase the number of solved
seeds under a fixed pass budget. This changes the admitted language, so it is
**not** a fair comparison of equivalent models or algorithms.

## Protocol

[MusicStudioFormProbe.lpr](../../examples/music/05_MusicStudio/MusicStudioFormProbe.lpr)
uses the same four source phrases exposed by
[MusicStudioCorpus](../../examples/music/05_MusicStudio/music_studio_workbench.pas).
Each sample is separate; corpus boundaries never create learned transitions.

Fixed parameters:

- 16 cells, 240 ticks/cell, 480 ticks/quarter, 4/4, 500000 microseconds/quarter;
- one melody voice, pitches C4..C5, velocity 96, two final rest cells;
- harmony and melody context order 3, whole-sequence extent;
- pass order harmony, rhythm, melody; melody must satisfy both providers;
- reference default selection/weights and per-pass deterministic seed streams;
- full chronological negotiation, local backtrack limit 256, pass limit 16,
  trace capture disabled;
- fresh pipeline for every case, no locks, no automatic retries or reseeding;
- exact seeds 0..15 in ascending order, for each of rhythm orders 3 and 16.

Stopping rule: the first solved composition or terminal bounded search
failure. A solved case must pass the independent Studio semantic validator;
a failed case must return no composition. The probe asserts the complete
solved-seed matrix and requires every remaining result to be a pass limit.

Metrics are status, number of rounds, pass backtracks, summed decisions,
propagations, contradictions and local backtracks over all attempted rounds,
public composition signature on success, and full negotiation transcript
hash. Rounds include the terminal round. These counters are not elapsed time
or peak memory measurements.

## Reproduce

From the repository root:

```bash
mkdir -p build/music-form/native/units build/music-form/native/bin
fpc -B -Mdelphi -Sa -Cr -Co -Ci -Fusrc -Fuexamples/music/05_MusicStudio \
  -FUbuild/music-form/native/units -FEbuild/music-form/native/bin \
  examples/music/05_MusicStudio/MusicStudioFormProbe.lpr
./build/music-form/native/bin/MusicStudioFormProbe > build/music-form/native/results.csv
```

On Windows, create those directories with PowerShell
and run the native `.exe`. Compare CSV as text after normalizing host CRLF
to LF; do not discard rows or columns. The checked-in
[raw results](music-studio-form-v1.csv) contain all 32 cases, not only successes.
The native gates build/run this probe and compare against that file.

## Observations

| Rhythm context | Solved | Solved seeds | Pass-limit cases |
| --- | --- | --- | --- |
| Order 3 | 1/16 | 10 | 15 |
| Order 16 | 7/16 | 0, 4, 6, 7, 13, 14, 15 | 9 |

The order-16 seed-zero case solves in one round with 4 decisions and 813
propagations. Its public signature is `216F6EBB`.
The sole order-3 success, seed 10, takes five rounds/four pass backtracks,
81 decisions, 3572 propagations, 14 contradictions, and 14 local backtracks.

A counterexample to a per-seed monotonic improvement is seed 10 itself:
order 3 solves, but order 16 exhausts its pass budget. All exhausted cases
reach 17 rounds/16 pass backtracks. Each corpus phrase supplies a known legal
composition under the respective learned models; failure here is finite
search exhaustion, not proof that the model is unsatisfiable.

## Interpretation and limits

The result supports the narrow hypothesis on this deliberately small fixed
seed set. Keeping a complete rhythmic form reduces one source of independent
provider variation, but the admitted language is smaller. It also changes
state ordering, branch choices, and search trajectories. Increased completion
frequency cannot be attributed solely to fewer combinations.

The Studio therefore exposes its fixed form honestly and uses seed zero as
a checked demonstration, not as evidence that every seed works. It never
substitutes a known corpus phrase after a failure.

This experiment establishes neither novelty nor superiority over existing
constraint solvers. There is no flattened equivalent-model baseline, memory/
time benchmark, subjective listening study, general success probability,
musical quality metric, or guarantee that more context helps. Harmony remains
pitch-class matching, not functional harmony or voice leading.

Next investigations: preserve an explicit phrase/form provider while allowing
rhythmic variants; compare equivalent flattened and pass-based models;
measure conflict-directed exclusions and edit locality; explore motifs
transferred across music and spatial domains. Each must retain negative cases
and distinguish a modeling restriction from a search improvement.
