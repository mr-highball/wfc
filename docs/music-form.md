# Bounded musical form planning

`wfc_music_form` adds bar-resolution intent above the existing exact sounding
harmony, rhythm, and ensemble layers. Its version-1 planner develops a finite
composition as question, answer, contrast, and return phrases. It chooses from
a caller-owned catalog using the existing Pascal `TGraph` solver; it does not
use an LLM, an external music package, or an audio runtime.

The library does not hardcode a key, MIDI instrument, chord spelling, or pitch
temperament. Ensemble Studio's `developed-period-v1` profile supplies one small
authored catalog. The original `structural-v1` profile remains a separate,
unchanged structural fixture.

## What is planned and what is solved

Each phrase has three genuine graph passes, all at bar resolution:

| Pass | Definition and constraints |
| --- | --- |
| `form` | Fixed, position-specific phrase role, functional intent, motif family, and cadence according to the versioned grammar below. |
| `harmonic-intent` | Selects a catalog harmony realizing each required function. Adjacent bars, including the preceding phrase boundary, must satisfy the configured chord-slot motion bound. |
| `gesture` | Selects an exact catalog realization, respecting both prior passes, motif family, role/cadence eligibility, contrast activity, and upper-line boundary motion. |

Form is an explicit authored grammar, not a claim that the solver invents a
new large-scale form on every run. Harmonic alternatives, eligible gestures,
and distinct realizations remain solver choices. Negotiated solving can revise
an earlier choice **within the current phrase** when a later pass rejects it.
There is no repair of already returned phrases and no guarantee that an
arbitrary user catalog admits a solution.

The bar's `HarmonicFunction` is structural intent, not the sounding pitch-class
set at every cell. A rest, non-chord melodic tone, or held chord must still be
validated by the existing exact acoustic projections. A realization catalog
is not proof that an adapter actually played those notes: the adapter must
independently check its realized frames, including attack/hold semantics and
the declared entry/exit upper-line attacks.

## Version-1 phrase grammar

The normal phrase roles cycle by absolute phrase index: question, answer,
contrast, return. With the default four bars per phrase, a 16-bar composition
has these functional sequences:

| Role | Four-bar functions | Last-bar cadence | Motif family |
| --- | --- | --- | --- |
| Question | tonic, expansion, predominant, dominant | half | theme |
| Answer | tonic, predominant, dominant, tonic | authentic | theme |
| Contrast | expansion, predominant, predominant, dominant | half | contrast |
| Return | tonic, predominant, dominant, tonic | authentic | theme |

Return realizes the theme's authored contour family and eligible variations.
It does **not** promise exact replay of the opening phrase's gesture sequence,
chords, voicings, or pitches. The theme and contrast motif IDs must differ.
Catalog authors are responsible for making those IDs correspond to genuinely
related or contrasting realizations, rather than merely relabeling one bar.

`PhraseBars` is caller-selectable. A closing answer/return ends on tonic and,
when it has at least two bars, places dominant immediately before tonic. Its
first bar is tonic when at least three bars fit; remaining interior bars are
predominant. A question/contrast ends on dominant, starts on tonic/expansion
when at least two bars fit, and uses predominant immediately before the final
bar when at least three bars fit. Other question interiors are expansion;
other contrast interiors are predominant.

The requested extent always wins. A final question/contrast becomes return;
a composition fitting in its first phrase uses answer. The closing phrase is
shortened when needed. A one-bar closing phrase is tonic; two bars are
dominant–tonic. Only the final bar of each phrase has a non-`none` cadence.

`TotalCells` need not be divisible by `CellsPerBar`. A final partial bar has an
explicit `CellCount` and is never padded past the requested extent. The adapter
uses only that prefix of the authored realization. A very short prefix can
state a tonic without audibly playing an entire sustained cadence; the planner
does not claim otherwise. A partial final bar has no subsequent boundary at
which its full-template exit anchor would be consumed.

## Policy and catalog contract

`DefaultWfcMusicFormConfig(TotalCells, Seed)` initializes policy, **not a musical
catalog**. Populate `Harmonies`, `Gestures`, and `Realizations` before creating a
cursor or calling the planner. The defaults are 8 cells/bar, 4 bars/phrase,
theme motif 0, contrast motif 1, maximum chord-slot and melody-boundary motion
7, and minimum contrast attacks 4. Search options use
`DefaultGraphNegotiationOptions`.

- Harmony motion pitches are nonnegative integer coordinates, sorted into a
  common number of slots. `MaxChordMotion` bounds the absolute change of
  **each corresponding sorted slot**, not the sum of changes. This is not a
  claim of full contrapuntal voice tracking, parallel-fifth avoidance, or
  in-bar voice-leading validation.
- Realization `EntryPitch` and `ExitPitch` identify its first and last
  upper-line attacks in the catalog's coordinate system. `MaxMelodyMotion`
  bounds the previous exit to the next entry, including across a release/rest.
  It does not constrain every melodic interval inside the authored bar.
- `AttackCount` is the declared count of upper-line attacks in a full gesture.
  The contrast minimum applies only to **non-cadence contrast bars**. A quieter
  cadential gesture is intentionally allowed. The adapter must verify the
  count against the authored realization.
- Several realization entries may share harmony and gesture indices. They
  remain distinct exact realizations and may carry different attack anchors.
- Labels are nonempty, unique within their harmony/gesture registry, and
  printable ASCII. Indices, counts, and pitches are checked exact integers;
  malformed browser numbers are rejected rather than truncated.

The dense local-adjacency adapter uses the existing versioned model envelope:
at most `WFC_MODEL_MAX_VALUE_COUNT` (currently 1024) harmonies, gestures, or
realizations. A catalog's relation storage is quadratic in its size. This is
a **finite catalog/resource bound**, not an arbitrary maximum playing time.
Phrase size is also a caller-selected finite resource choice; large phrases
cost more memory and search than the default four bars.

## Ownership, iteration, and failures

This example uses the developed demo profile as one catalog provider; other
applications can supply their own `TWfcMusicFormConfig` arrays.

```pascal
Config := EnsembleStudioDevelopedFormConfig(RequestedCells, Seed);
Cursor := TWfcMusicFormCursor.Create(Config);
try
  repeat
    Frontier := Cursor.CopyFrontier;
    Step := Cursor.Next(Plan, Report);
    if Step <> wmaspProduced then Break;
    try
      if not ValidateWfcMusicFormPhrase(Config, Frontier, Plan, Failure) then
        raise Exception.Create(Failure);
      for I := 0 to Plan.BarCount - 1 do
      begin
        Bar := Plan.BarAt(I);
        { Resolve Bar.RealizationIndex with your exact-frame adapter.
          Consume only Bar.CellCount cells, then independently audit them. }
      end;
    finally
      Plan.Free;
    end;
  until False;
  if Step = wmaspFailed then
    raise Exception.Create(Cursor.Failure);
finally
  Cursor.Free;
end;
```

The cursor deep-copies its configuration, including nested chord-slot arrays.
Caller edits do not change an existing cursor. `CopyWfcMusicFormConfig` also
returns a detached copy. The standalone planning and validation functions
borrow their arguments only for the synchronous call.

Every successful `Next` transfers a newly owned immutable phrase plan to the
caller. `BarAt` returns an unmanaged record and `CopyBars` returns a detached
array. The cursor retains no returned plan or duration-sized history: it keeps
its finite catalog and the next-bar/previous-harmony/previous-realization
boundary, and releases the phrase-sized graph before returning.

The final successful call still returns `wmaspProduced`; `Status` is then
`wmasCompleted`, and the following call returns `wmaspCompleted` with a nil
plan. Cancellation is checked between synchronous calls. A contradiction or
exhausted search budget returns `wmaspFailed`, publishes no plan, preserves
the last committed frontier, and is terminal. Unexpected exceptions mark the
cursor failed and propagate; they are not silently rerolled. Choose a new
catalog/policy/seed or create a new cursor explicitly to retry.

`TryPlanWfcMusicFormPhrase` supports direct planning from a supplied frontier.
It validates the boundary's extent, alignment, and realization consistency.
That frontier is a caller-supplied witness, **not authentication of earlier
history**. Planning after the full extent is an argument error; the cursor's
normal completion protocol avoids that call.

## Portable identity and independent validation

Absolute positions use `TWfcMusicArrangementWide`: native `Int64` and the
pas2js exact integer representation, through `2^53 - 1` cells. There is no
three-minute target or fixed total bar limit. The shared arithmetic envelope
is explicit; larger values, nonintegers, infinities, and NaN are rejected.
No allocation scales with `TotalCells`. Tests materialize the first phrase
and the last partial phrase near that envelope without generating the
intervening composition.

Each phrase seed is `WfcMusicArrangementSectionSeed(Config.Seed,
AbsolutePhraseIndex)`. It does not depend on playback segment size, wall
time, previous search duration, or discarded intermediate objects. Identical
catalog/configuration/frontier inputs replay identically on checked stable
and current FPC and pas2js.

`ConfigSignature` hashes the versioned musical catalog, extent, seed, and
policies. Search budgets and trace capture are operational and excluded.
`Signature` additionally hashes the exact phrase bars, seed, and boundary.
These portable 32-bit signatures are useful regression identifiers, **not
collision-proof equality or proof of musical validity**.

`ValidateWfcMusicFormBars` independently checks detached records against the
grammar, catalog, exact extent, and all motion policies without trusting a
solver report or hash. `ValidateWfcMusicFormPhrase` performs that same audit
and then checks plan provenance and signatures. The acoustic adapter must
add its independent intended-versus-realized frame audit before publishing
audio or advancing its own stream frontier.

The focused regression program is `test/wfc_music_form_test.lpr`. It covers
16-bar structure and replay, arbitrary shortened phrase/bar extents, detached
ownership, malformed catalogs and browser numbers, unsatisfiable motion and
cadence policies, unchanged failure frontiers, cancellation, and exact wide
positions. The separate Ensemble profile and developed-studio tests check
actual notes, holds, motif realization, and finite/stream integration.
