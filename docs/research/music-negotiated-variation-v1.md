# Music Negotiated Variation v1 research record

Execution note: pas2js figures below are historical measurements. Maintained
execution paths are native FPC and the documented browser demos; those earlier
figures do not establish browser coverage for this experiment.

- **Fixture:** `examples/music/04_NegotiatedVariation`
- **Seed:** `0`
- **Pass order:** `harmony=0`, `rhythm=1`, `melody=2`
- **Selective request:** roots `[0]`, active descendant closure `[0,2]`
- **Nested search:** Pass Negotiation v1
- **Scope:** Selective Negotiation v1

## question

Can the existing exact, bounded negotiation algorithms recover one small
music variation when an ordinary one-way regeneration repeats an upstream
harmony that makes a requested melody ending impossible, while a rhythm
provider outside the authorized repair closure remains unchanged?

This record tests feasibility and transaction boundaries only. It does not
test musical quality or search efficiency.

## fixture

The project-owned pipeline learns two order-4, four-cell harmony samples, one
order-4 common rhythm sample, and two order-4, four-cell melody samples. Both
melody samples share the same two-cell C4 opening motif and differ in the final
E4 or G4 attack/hold pair. The matching harmony samples share the motif pitch
class and differ in the ending pitch class. Melody depends conjunctively on
public rhythm action and public harmony pitch class.

Seed zero first commits a complete baseline. The edit preserves the public
two-cell melody motif and constrains the ending to the other learned branch.
The mutation changes caller-owned melody constraints, not committed public
output.

The calls then have these scopes:

| Call | Requested roots | Active passes | Reused pass |
| --- | --- | --- | --- |
| Ordinary selective regeneration from harmony | `[0]` | `[0,2]` | rhythm `[1]` |
| Selective negotiated regeneration from harmony | `[0]` | `[0,2]` | rhythm `[1]` |

Because every dirty pass rebuilds its random stream from the same pipeline
seed, the ordinary call repeats the baseline harmony. Melody cannot satisfy
the requested ending, so the whole attempt rolls back. The negotiated call
retains that rejected ordinary report, excludes the complete harmony
assignment, and searches another round. The second round selects the other
harmony and commits it atomically with the requested melody. Rhythm is staged
from committed output and reported as reused.

The owner also performs complete capture and music-domain validation inside
the graph's tentative commit transaction. A candidate that satisfies graph
relations but violates melody continuation is rejected as final validation
before publication, with exact entry and random-stream rollback. The focused
conformance fixture covers that boundary separately from this variation's
provider-reopening transcript.

## required observations

The self-check requires all of the following before printing success:

1. Seed zero produces the pinned baseline public composition and signature.
2. The public opening motif is identical before and after repair.
3. Ordinary selective regeneration fails at `melody` from `harmony` and does
   not replace the baseline composition.
4. The ordinary report distinguishes reused rhythm from attempted harmony and
   melody work.
5. Selective negotiation reports requested roots `[0]`, active indices
   `[0,2]`, exactly one rejected round, and harmony pass `0` as the excluded
   chronological choice frame.
6. The negotiated result uses the requested ending, retains the motif, and
   leaves rhythm byte-for-byte equal to the baseline public rhythm cells.
7. Independent public relation validation succeeds without trusting graph
   internals.
8. The copied score validates, strict `wfcmusic=1` round-trips exactly, and
   the project-owned SMF decoder/writer reproduces the canonical MIDI bytes.
9. Public composition, score, MIDI, nested negotiation, and selective
   negotiation signatures match their seed-zero portable goldens.

The pinned seed-zero evidence is:

| Evidence | Value |
| --- | --- |
| Baseline ending and composition signature | E4 (`64`), `4194643A` |
| Requested/repaired ending and composition signature | G4 (`67`), `77B72044` |
| Canonical score-text FNV-1a | `6F92E033` |
| Canonical MIDI-byte FNV-1a | `4BC35E03` |
| Nested Pass Negotiation v1 transcript | `A91E0706` |
| Selective Negotiation v1 transcript | `38EE8F80` |
| Canonical `wfcmusicpass=1` size | `685` bytes |

Native FPC reproduces the checked values also measured in historical pas2js runs.

## stopping rules and complexity

Two independent bounds apply. `SolveOptions.MaxBacktracks` limits ordinary
local solver search inside one pass attempt. `MaxPassBacktracks` limits how
many exact completed provider assignments the coordinator may exclude. A run
that reaches either limit is budget-limited evidence, not proof that no
compatible composition exists.

Whole-assignment exclusions are intentionally exact and reproducible, but the
number of assignment vectors is exponential in the number of cells and their
values. Chronological choice-frame selection is not conflict-directed and may
redo unrelated work in a larger graph. Changing an earlier assignment also
invalidates later contextual exclusions.

## non-claims

The first compatible bounded result is not claimed to be a minimal edit,
closest variation, optimal harmony, best voice leading, or aesthetically
preferred result. The fixture has no soft objective and publishes no claim of
speed, memory, propagation, or backtracking advantage over an equivalent
flattened model. It has fixed quantization, one voice, pitch-class harmony,
and no playback or interactive editor.

Any later objective search, partial nogood, conflict-directed repair, phrase
horizon, polyphonic representation, or flattened comparison requires a new
algorithm or experiment version and its own fixtures, counters, and negative
findings.
