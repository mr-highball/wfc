# Synchronized polyphonic music

The ensemble libraries add chord-capable, independent voices to pass-based
music generation. They build on the existing exact score, sequence learner,
and transactional graph; the monophonic music APIs and replay formats are
unchanged.

An ensemble frame is one quantum of time containing an ordered cell for each
voice. A cell is a rest, an attack, or a hold, with the complete sorted list of
pitch/velocity pairs for a sounding voice. A bass can hold while a chord is
reattacked and a melody rests. Neither of those events retriggers the bass.

This is joint-frame learning: the vocabulary contains observed vertical
combinations, while sequence contexts permit temporal recombination. It does
not construct the Cartesian product of independent voice vocabularies, infer
arbitrary counterpoint, or claim that every musically meaningful combination
has been learned.

For independently learned pitch roles that can form unobserved vertical
combinations, use the additive [Independent Voices layer](music-voices.md).
It retains these frame/hold codecs but replaces the single joint consumer
with separate role passes and collective harmony witnesses. The two learning
representations remain distinct; this guide describes the joint-frame path.

## Units and ownership

| Unit | Responsibility |
| --- | --- |
| `wfc_music_ensemble` | Voice cells, frame/rhythm/set codecs, exact score projection and reconstruction |
| `wfc_music_ensemble_graph` | Continuation-model preflight and distinct rhythm/exact-harmony/allowed-harmony maps |
| `wfc_music_ensemble_passes` | Reusable three-pass owner, public locks, negotiation, selective regeneration, immutable compositions, independent commit validation |
| `wfc_music_ensemble_training` | Ordered multi-voice, common-excerpt training documents |
| `wfc_music_ensemble_stream` | Bounded local pass graphs with exact continuation frontiers and caller-defined total ticks |
| `wfc_music_ensemble_audio` | Incremental chord-capable PCM, exact rational timing, held phases, and bounded release look-behind |

All are project-owned Pascal using repository units and the compiler RTL.
Constructors and copy accessors detach managed arrays, including nested tones.
The pipeline borrows immutable sequence models: keep them alive until the
pipeline is freed. It deep-copies its score template. Returned compositions
and their copied scores are caller-owned and survive destruction of the
pipeline and models.

## Representation and continuity

`TWfcMusicEnsembleFrame.Voices[I]` is voice slot `I` throughout a timeline.
`TWfcMusicVoiceCell` contains `Action` and `Tones`. The action enumeration is
the existing `wmcaRest`, `wmcaAttack`, `wmcaHold`.

- A rest has no tones; silence is not a pitch sentinel.
- A sound has at least one tone, with strictly increasing nonnegative pitches
  and a velocity in `1..127` for every tone.
- A hold requires a sounding previous cell in the same voice with exactly the
  same pitches and velocities.
- A finite timeline cannot begin with a hold, change voice arity, or be empty.
- Equal consecutive attacks remain separate score spans. Only holds extend
  an attack; consecutive rests are merged during score reconstruction.

`ValidateWfcMusicEnsembleFrames` checks the temporal contract.
`WfcMusicEnsembleFrameCanStart` and `WfcMusicEnsembleFrameCanFollow` expose the
same start/edge predicates for model construction. A malformed individual
frame raises an error; an otherwise valid but incompatible edge returns false.

One chord cell has a shared onset and end for all its tones, matching the
exact-score chord model. Use separate voices when individual tones need
independent note-offs. In particular, do not turn imported overlapping voices
into a succession of global chord slices: that would change attacks and holds.

`ProjectWfcMusicScoreToEnsembleFrames(Score, QuantumTicks)` requires the entire
score and every span to align exactly to the positive quantum. It retains
score voice order, including silent voices. `RebuildWfcMusicEnsembleSpans`
returns voice-major canonical spans; `RebuildWfcMusicEnsembleScore` also copies
track/voice/meter/tempo metadata from a template with matching voice count and
exact timeline length. Aligned score → frames → score is lossless.

## Canonical token families

All three new families are version one. They are strict ASCII with unsigned
canonical decimal fields: no signs, leading zeroes, whitespace, unknown
versions, missing fields, or trailing fields. Counts are checked against the
remaining encoded data before arrays are allocated.

| Family | Form and example |
| --- | --- |
| Ensemble frame | `wme1:<voice-count>:<voice-body>...` |
| Voice body within a frame | `r`, or `a:<tone-count>:<pitch>:<velocity>...`, or `h:<tone-count>:<pitch>:<velocity>...` |
| Rhythm vector | `wmer1:<voice-count>:<action>...`, for example `wmer1:3:h:a:r` |
| Pitch-class set | `wmhs1:<steps>:<count>:<class>...`, for example `wmhs1:12:3:0:4:7` |

For example, `wme1:2:a:1:48:80:h:2:60:90:64:75` describes a new bass note
and a held two-tone chord. It is a valid vocabulary token, but not a valid
first frame of a finite score. Individual and bulk codecs deliberately do not
invent preceding context; temporal validation is separate.

Pitch-class sets are strictly increasing, unique integer arrays within the
declared positive step system. They are not restricted to twelve-tone masks.
The empty set is `wmhs1:12:0` for twelve-step music. Octave duplicates disappear
from the set projection, not from score tones or voices.

## Harmony and rhythm are different constraints

Let `S(F)` be the union of pitch classes of every sounding tone in frame `F`,
including holds. The graph adapter has separate rule builders:

| Builder | Required relationship |
| --- | --- |
| `BuildWfcMusicEnsembleRhythmProjectionRules` | Every ordered voice action equals the provider rhythm vector |
| `BuildWfcMusicEnsembleExactHarmonyProjectionRules` | `S(F)` equals the provider set |
| `BuildWfcMusicEnsembleAllowedHarmonyProjectionRules` | `S(F)` is a subset of the provider set |

Silence therefore matches only an empty exact set, but any allowed palette in
the same step system.
A nonempty allowed palette does not require a note to sound. Learning exact
sonority evidence does not automatically infer an allowed scale or chord
policy; choosing that interpretation is explicit in the caller's configuration.

`RequireWfcMusicEnsembleFromPasses` atomically joins the rhythm and harmony
providers. The returned rule arrays also compose through
`RequireSequenceProjectionMapsFromPasses` with additional distinct named
providers. Alternatives within one map are OR; separate provider maps are AND.
Duplicate provider labels remain invalid. Private latent keys never become a
public voice, pitch, or lock format.

Before applying an ensemble model, `ValidateWfcMusicEnsembleModel` checks every
observed start state and every structurally compatible state edge for valid
voice continuity. Generic order-one sequence models allow every token pair;
valid training samples alone do not make such a model safe for holds. Unsafe
models fail preflight with an actionable continuation error. Order one remains
valid when its permitted transitions are genuinely safe, such as a rest/attack
vocabulary. The adapter does not silently change generic sequence adjacency
after model application.

## Pipeline workflow

`TWfcMusicEnsembleConfig` specifies the three sequence models, score template,
positive quantum, seed, extent, and harmony mode. Defaults are `wseWhole` and
`wmehmExact`. The fixed public layers are `harmony`, `rhythm`, and `ensemble`.
Harmony and rhythm are independent providers; ensemble requires both.

The pipeline follows the existing fluent style:

```pascal
Config := DefaultWfcMusicEnsembleConfig(ScoreTemplate, QuantumTicks, Seed);
Config.Models.Harmony := HarmonyModel;
Config.Models.Rhythm := RhythmModel;
Config.Models.Ensemble := EnsembleModel;
Config.HarmonyMode := wmehmExact;

Pipeline := TWfcMusicEnsemblePipeline.Create(Config);
try
  Pipeline.LockVoiceCells(0, 0, BassOpening);
  if Pipeline.TryGenerateNegotiated(DefaultGraphNegotiationOptions,
      Composition, Report) then
  try
    Score := Composition.CopyScore;
    try
      { Validate, serialize, export, or render the exact score. }
    finally
      Score.Free;
    end;
  finally
    Composition.Free;
  end;
finally
  Pipeline.Free;
end;
```

Declare `Report` as `TWfcMusicEnsembleNegotiationReport`, `Composition` as
`TWfcMusicEnsembleComposition`, and `BassOpening` as `TWfcMusicVoiceCells`.
The [Ensemble Studio example](../examples/music/06_EnsembleStudio/README.md)
contains complete model construction, training, hosts, and checked use.

`LockVoiceCells` intersects the ensemble vocabulary by one voice slot, leaving
other slots free among matching observed frames. It never synthesizes an
unobserved joint token. `LockEnsembleFrames`, `IntersectAllowedTokens`,
`IntersectTokenConstraints`, and `IntersectLockedSpan` constrain complete
public layer tokens. Empty allowed domains are valid contradictions, not
instructions to ignore a lock. `ClearAllowedTokens` restores the saved
endpoint baseline at that position.

Ordinary, negotiated, and selectively regenerated operations reuse `TGraph`.
Changed provider constraints reopen the necessary descendant closure. A seed
change invalidates all generated layers. A fresh selective request includes
the dirty initial providers rather than requiring an unavailable baseline.

Before publication, the owner independently checks captured state paths,
projected public tokens, caller constraints, each voice's continuation,
rhythm equality, and the chosen harmony relation. It rebuilds and compares the
exact score while graph rollback is still available. Failure returns no new
composition; previously returned immutable compositions remain snapshots, not
evidence that the edited pipeline is current.

`wseWhole` requires observed start and end boundaries. `wsePrefix` requires an
observed start but may end at an interior state, with the sounding spans ending
at the requested finite score boundary. Other extents are not accepted by this
owner: they would need explicit incoming voice/sustain context. A desired
length must still have a feasible model path; the library does not pad or loop
a recording to conceal a contradiction.

## Synchronized excerpt training

`BuildWfcMusicEnsembleTrainingDocument` takes one ordered array of distinct
source voice indices and independently named common time selections. Frame
slot `I` always means `VoiceIndices[I]` in every sample. Silent slots remain
present. Different documents may choose different voice arities or orders.

The projection is explicit: `wmetpFrame`, `wmetpRhythmVector`, or
`wmetpExactPitchClassSet`. Each selection becomes a separate sequence sample;
no transition is invented across selection boundaries. The resulting immutable
training document uses the existing training codec and CLI/learner workflow.

Selections must be quantum-aligned and contain at least the requested order's
number of frames. They need not cover complete measures. A selection that cuts
a sounded span in **any selected voice** is rejected, including a chord or held
bass. Silence may be cropped. Unselected voices and spans outside the selected
ranges do not impose alignment requirements on the excerpt.

Retain the ordered source voice mapping, quantum, tuning, score/import identity,
and metadata as provenance. Frame tokens encode musical content and slot order,
not original voice identifiers or an inferred physical duration. The pipeline's
template and quantum supply that context. The trainer never converts exact
observations into an inferred allowed-harmony policy.

## Replay, capacity, and current boundaries

The new frame, graph, pipeline, validation, signature, and training contracts
have separate version-one constants. Composition identity includes the seed,
quantum, finite extent, harmony interpretation, ordered public layers, and
canonical exact score. It is a compact deterministic check, not a cryptographic
digest or complete model identity. Replay also requires the same sequence
models, constraints, solver options, and model construction order.

Existing `wm1`, `wr1`, `wh1`, and `wfcmusicpass=1` retain their monophonic
meanings. Do not put ensemble tokens into that old composition format. Exact
multi-voice scores already serialize as `wfcmusic=1`; generic sequence/training
artifacts can carry the new tokens. A dedicated ensemble-composition container
is not supplied. The separately versioned [streaming protocol](music-ensemble-stream.md)
does not alter these finite composition formats or signatures.

There is no arbitrary minute, voice-count, or chord-tone cap in the frame
representation. Integer-indexed storage and expanded counts are checked before
materialization; host memory still matters. Whole-score projection retains all
frames and tones, rather than claiming bounded-memory long-form generation.
Existing sequence-model and training-document resource limits still apply at
those adapters. Joint vocabulary diversity can reach them before timeline
length does.

The existing MIDI and PCM preview adapters have their own voice, pitch, tone,
duration, and frame limits. An adapter rejection must not erase a valid score,
silently shorten it, or redefine its requested length. The existing streamed
Music Studio arranger remains monophonic; it is not a polyphonic export path.
The separate ensemble stream supplies polyphonic generation and PCM output
with bounded timeline memory; it does not turn a continued segment into a
standalone finite score. A separate [MIDI stream](music-midi-stream.md) counts
and replays these frames into bounded forward-only file output.
[Independent role recombination](music-voices.md) is a separate implemented
path, with its own [proof corpus and research boundary](research/independent-voices-v1.md).
Expressive performance, general voice leading, independent rhythm-role
learning, and richer harmonic policies remain ecosystem work.
