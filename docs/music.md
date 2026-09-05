# music foundation

The music foundation turns a validated, exact score into discrete sequence
cells, composes learned rhythm and harmony passes into a melody pass, rebuilds
the score, and exports a Standard MIDI File. All of that code is project-owned
Pascal shared by native FPC and pas2js. Playback is deliberately outside the
foundation.

The separate [ensemble extension](music-ensemble.md) adds synchronized
chord-capable voices, exact or allowed pitch-class sets, multi-voice excerpt
training, and a polyphonic pass owner. The original monophonic contracts below
remain unchanged.

The first vertical slice proves this pipeline:

```text
rhythm plan ----\
                 > melody cells -> exact score -> wfcmusic=1 -> SMF format 0
harmony plan ---/
```

The rhythm and harmony plans are themselves solved latent order-N sequences.
The melody pass depends on both named passes in the same atomic `TGraph`
transaction. The portable
[PassComposition example](../examples/music/03_PassComposition/README.md)
exercises the complete path.

## exact score model

`wfc_music` defines the immutable `TWfcMusicScore` intermediate
representation. Time, pitch, velocity, and rational values are integers; no
floating-point rounding is part of validation or replay.

A valid score has these structural invariants:

- ticks per quarter, steps per octave, and score length are positive;
- there is at least one track, voice, meter change, tempo change, and span;
- track IDs are nonempty, valid UTF-8, and unique; names are valid UTF-8;
- voice IDs are nonempty, valid UTF-8, and unique, and every voice refers to an
  existing track;
- meter and tempo changes are strictly ordered, begin at tick zero, and occur
  before the score end;
- meter numerators are positive and denominators are positive powers of two;
- every meter produces an exact positive integer measure length in ticks;
- a meter change falls on a complete measure boundary of the preceding meter,
  and the score ends on a complete measure boundary of the final meter;
- tempo values are positive microseconds per quarter note;
- spans are in canonical voice-major order, and each voice occupies one
  contiguous block of spans;
- each voice exactly partitions `[0, LengthTicks)`: starts at zero, has no gap
  or overlap, and ends exactly at the score length;
- every duration is positive; a rest has no tones, a note has one tone, and a
  chord has at least two tones;
- adjacent rests in one voice are merged;
- pitches are nonnegative, velocities are in `1..127`, and chord tones are
  strictly increasing by pitch.

Constructor input arrays are copied. Indexed accessors return values or deep
copies, so later caller mutations cannot alter an accepted score. Rational
helpers reduce nonnegative fractions and reject a zero denominator, overflow,
or an inexact conversion from quarter-note duration to ticks.

This model is intentionally more general than MIDI: it permits an arbitrary
positive number of steps per octave and nonnegative pitches beyond MIDI's
seven-bit range. Export adapters must state and validate any narrower boundary.

## fixed-quantum sequence cells

`wfc_music_sequence` bridges exact spans and the generic bounded order-N
sequence learner. A caller chooses a positive `QuantumTicks`; every projected
span start and duration must align exactly to it, and the score length must be
divisible by it.

A melody quantum is one of:

- `attack`: begin a note with a pitch and velocity;
- `hold`: continue the immediately preceding sound with the same pitch and
  velocity;
- `rest`: silence for this quantum.

The canonical, version-1 ASCII tokens are:

| Model | Canonical cells |
| --- | --- |
| Melody | `wm1:r`, `wm1:a:<pitch>:<velocity>`, `wm1:h:<pitch>:<velocity>` |
| Rhythm | `wr1:r`, `wr1:a`, `wr1:h` |
| Harmony | `wh1:r:<steps>:0`, `wh1:p:<steps>:<pitch-class>` |

All integers are unsigned canonical decimal with no leading zero. Melody
pitch is nonnegative, velocity is `1..127`, harmony steps are positive, and a
pitch class is in `0..steps-1`. A harmony rest always has pitch class zero.
Decoders reject non-ASCII input, unknown versions or actions, missing or extra
fields, and noncanonical integers.

`ProjectWfcMusicVoiceToMelodyCells` is lossless for aligned rests and
monophonic notes. It rejects chords instead of flattening them. The inverse,
`RebuildWfcMusicVoiceSpans`, validates that a hold follows an attack or hold
with exactly the same pitch and velocity. It merges adjacent rest cells and
treats every attack as a new note, even when its pitch matches the preceding
note. Under those conditions, projection and rebuild preserve the voice
timeline exactly.

`ProjectWfcMusicMelodyToRhythm` retains only attack/hold/rest action.
`ProjectWfcMusicMelodyToHarmony` maps every sounding pitch to its positive
modulo pitch class and emits an explicit harmony rest for silence.
`LearnWfcMusicMelodySequence` is the typed convenience entry point into the
generic learner; rhythm and harmony tokens can use the same learner directly.

## latent cross-model projection

An order-N sequence model solves private latent states containing history,
not only the visible token. `wfc_music_graph` builds public-token projection
maps and lets the generic adapter expand each source token to every matching
private state. No private graph key enters application code, artifacts, or
validation output.

`RequireWfcMusicMelodyFromPasses` adds two separately named dependency groups
to the active melody pass:

- melody action must equal the rhythm pass action;
- every sounding melody pitch modulo `StepsPerOctave` must equal the harmony
  pass pitch class.

Both groups must match at every coordinate. Alternatives within one projection
rule remain an OR set. A melody rest accepts every harmony token by design, so
a harmony sequence may continue across silence; rhythm still requires the
rest action. Malformed or incomplete projection maps fail during preflight,
before the first dependency rule is installed or the graph commits any pass
output.

The lower-level generic API is `TWfcSequenceProjectionRules`,
`MakeWfcSequenceProjectionRule`, `TWfcSequenceProjectionBinding`, and
`MakeWfcSequenceProjectionBinding`.
`RequireWfcMusicMelodyFromPasses` builds one rhythm binding and one harmony
binding, then calls `RequireSequenceProjectionMapsFromPasses`. The generic
bundle checks both complete maps, model identities, provider labels, and
dependency edges before changing the target pass. Its single-map counterparts
remain `ValidateSequenceProjectionMapFromPass` and
`RequireSequenceProjectionMapFromPass`. These APIs can express other music
relations without adding domain knowledge to the graph core.

## persistent pass owner and negotiated variation

`wfc_music_passes` turns the focused three-pass relation into a reusable,
persistent owner. `TWfcMusicPassConfig` keeps the harmony, rhythm, and melody
sequence models caller-owned while the pipeline deep-copies its exact
single-voice score template. The template fixes ticks per quarter, meter,
tempo, track/voice metadata, score length, and step system; its length must be
an exact positive multiple of the configured quantum.

The stable typed layer order is harmony `0`, rhythm `1`, and melody `2`.
`TWfcMusicPassPipeline` exposes public-token intersections, atomic token
constraint sets, exact locked spans, and a melody-cell lock helper. These APIs
translate public cells through the existing sequence adapter; callers never
need a latent state key. Models must outlive the owner, but subsequent caller
mutation of the score-template arrays or object cannot change its captured
configuration.

The generation surface distinguishes four operations:

- ordinary initial generation and descendant-only regeneration use one-way
  transactional solving;
- negotiated initial generation may reopen any completed provider within a
  finite pass-backtrack budget;
- selective negotiated regeneration accepts explicit typed roots and searches
  only their exact descendant closure;
- every failure leaves the previously committed graph values and any earlier
  caller-owned composition object unchanged.

Complete latent capture and music-domain validation run at the graph's
tentative commit boundary, while its exact entry and random-stream snapshots
are still live. A syntactically reachable but musically invalid candidate,
such as a leading melody hold, is reported as `gckFinalValidation`; ordinary
generation rolls it back, and bounded negotiation may exclude that exact
assignment before trying another round. Unexpected runtime or resource
exceptions are not converted into search alternatives: they propagate after
the same transactional rollback.

The returned `TWfcMusicComposition` is caller-owned and immutable. It exposes
deep copies of all three public cell streams and the rebuilt score, along with
seed, quantum, cell count, latent-capture availability, and a versioned public
signature. Pipeline results retain copied latent state paths. The pipeline's
`Validate` method therefore requires a latent capture and checks its state
paths, projections, caller constraints, public relations, exact score rebuild,
and recomputed signature. A composition reconstructed through the checked
public factory deliberately does not invent latent paths; that factory instead
checks the public token syntax, equal layer lengths, rhythm action equality,
sounding pitch-class harmony, melody attack/hold rules, and exact score
rebuild. The canonical artifact decoder uses that public-only validation path
and independently verifies the stored signature.

For diagnostics, `TryCopyCommittedLayer` returns a detached checked latent
capture and `CopyCommittedTokens` returns detached public tokens. The owner
does not expose its mutable graph or per-pass random streams, so inspection
cannot bypass dirty tracking or perturb replay.

Negotiation remains bounded chronological feasibility search over complete
pass assignments. Public locks preserve an exact motif, while the selective
root controls which provider layers may change. Neither mechanism implies a
coordinate-minimal phrase edit, a conflict-directed repair, or a musical
objective. The self-checking
[NegotiatedVariation fixture](../examples/music/04_NegotiatedVariation/README.md)
publishes that distinction and its exact native/pas2js evidence.

## canonical score text: strict `wfcmusic=1`

`EncodeWfcMusicText` writes a strict ASCII, LF-only document with a final LF.
The fixed order is header, score scalars, tracks, voices, meters, tempos,
spans, and `end`:

```text
wfcmusic=1
tpq=480
steps=12
length=3840
tracks=1
track=0,lead,Pass%20Composition
voices=1
voice=0,0,melody
meters=1
meter=0,0,4,4
tempos=1
tempo=0,0,500000
spans=2
span=0,0,0,960,N,65@88
span=1,0,960,2880,R,
end
```

Track and voice text uses the shared canonical UTF-8 percent codec. Span kinds
are `R`, `N`, or `C`; a tone is `pitch@velocity`, and chord tones are separated
by `;`. Counts and indices are explicit and contiguous.

`DecodeWfcMusicText` rejects reordered or unknown fields, CRLF, missing final
LF, trailing data, malformed UTF-8 or escapes, noncanonical integers, invalid
indices, and every score invariant above. It then re-encodes the score and
requires byte equality. This is a replay artifact, not a forgiving interchange
parser.

The `wfcmusic=1` identity belongs only to the exact music-score artifact
implemented by `wfc_music_text`. The separate `wfcm=1` and `wfcm=2` identities
belong to learned cardinal models documented in
[model learning and priming](learning.md). The names are intentionally
distinct, so dispatch and diagnostics never have to infer an artifact family
from its later fields.

## canonical composition result: strict `wfcmusicpass=1`

`wfc_music_passes_text` serializes one immutable public composition as strict
ASCII with LF line endings and a final LF. Its fixed order is:

```text
wfcmusicpass=1
seed=4045620583
quantum=120
cells=4
harmony=0,wh1%3Ap%3A12%3A0
...
rhythm=0,wr1%3Aa
...
melody=0,wm1%3Aa%3A60%3A90
...
signature=C8541A90
score=wfcmusic%3D1%0Atpq%3D120%0A...
end
```

The excerpt elides the remaining indexed cell lines and score fields; the
conformance suite pins the complete document and signature shown above.

There are exactly `cells` indexed records for each layer. Public tokens and
the complete canonical `wfcmusic=1` score are encoded with the shared UTF-8
percent codec, so the outer document remains one unambiguous line sequence.
The eight uppercase hexadecimal signature is recomputed from the public
configuration, ordered cells, relevant algorithm/format versions, and exact
score semantics.

Decoding validates every canonical field, reconstructs the composition through
the checked public factory, verifies the stored signature, and requires
byte-identical re-encoding. No latent key, private history state, path, graph
domain, or platform path enters the file. This is a result-replay artifact,
not a complete generation replay: reproducing the search also requires the
learned models, constraints, solver and negotiation options, and negotiation
transcript.

## project-owned Standard MIDI Files

`wfc_midi_smf` is a byte-array codec for Standard MIDI Files (SMF). Its format
and event decisions follow the MIDI Association's
[Standard MIDI Files](https://midi.org/standard-midi-files) material and the
[MIDI 1.0 core specifications](https://midi.org/midi-1-0-core-specifications).

Version 1 supports:

- SMF format 0 with exactly one track and format 1 with one or more tracks;
- PPQN division from `1..32767`; SMPTE division and format 2 are rejected;
- all MIDI 1.0 channel status families with their exact one- or two-byte data
  lengths and seven-bit data;
- `F0` and `F7` system-exclusive records as opaque byte arrays;
- opaque meta events, with additional exact validation for tempo (`0x51`) and
  time signature (`0x58`);
- running-status input for channel events; any non-channel event clears it;
- canonical variable-length quantities through `0x0FFFFFFF`;
- exactly one final end-of-track event per track, represented by
  `EndDeltaTicks` in the in-memory model.

The writer always emits explicit channel status, so decoding a file that uses
running status and encoding it again produces the canonical explicit-status
form. It rejects a non-six-byte `MThd`, invalid track counts or chunk order,
truncated records, data after end-of-track, and trailing file bytes. Unknown
chunks and extended headers are not preserved.

Defensive read limits are caller-overridable. The defaults are 64 MiB per
file, 16 MiB per track, 256 tracks, 1,000,000 events, and 16 MiB of event data.

`wfc_music_midi` exports a `TWfcMusicScore` to an SMF format-0 file. It requires
exactly 12 steps per octave, PPQN in `1..32767`, at most 16 voices, pitches in
`0..127`, tempo values in the MIDI three-byte range, and meter values that fit
the MIDI meta event. Voice index maps directly to channel `0..15`; chords emit
one note pair per tone; rests emit no channel event; note-off velocity is zero.
At a shared tick, metadata precedes note-offs, which precede note-ons, while
source order is stable within a priority. Tempo precedes meter at the same
tick. End-of-track reaches the exact score length.

The generic codec can read and write format 1. The score exporter emits only
format 0. The separate [semantic importer](music-import.md) converts supported
format-0/1 streams into `TWfcMusicScore` with explicit event and ending policies;
it does not promise lossless performance-event round-tripping.

## audible portable Music Studio

[Music Audio v1](music-audio.md) renders a validated score to immutable mono
PCM16 with exact tempo-to-frame mapping, bounded fixed-point synthesis, and a
project-owned canonical RIFF/WAVE writer. The same Pascal code produces the
same bytes on native FPC and pas2js; it has no samples or synthesizer library.

The [Music Studio](../examples/music/05_MusicStudio/README.md) joins this
renderer to the persistent three-pass owner. Native hosts inspect
and export composition text, score text, MIDI, and a four-second WAV. A pas2js
browser workbench adds public-cell and motif locks, ordinary/negotiated
selective repair, layer grids, a piano roll, failure reports, downloads, and
user-initiated HTML audio playback. Pending edits and failed attempts unload
old previews and hide stale artifacts.

A separate [arrangement path](music-arrangement.md) generates fresh sections
to a user-defined duration and streams one complete WAVE/RF64 file. The small
phrase editor is not the total composition-length limit. The supplied source
uses explicit bar rounding and rest-separated continuity, not a global
verse/chorus planner. [Semantic MIDI import](music-import.md) now converts
supported event streams into exact scores and selected fixed-grid training
documents with explicit provenance and performance-event policies.

Its four original phrases and fixed two-bar rhythmic form are a deliberately
small, documented model. The [form experiment](research/music-studio-form-v1.md)
publishes all 32 seed/order cases, including finite-budget failures, without
claiming general musical quality or solver superiority.

## dependency and license boundary

The exact score, cell codecs, sequence learning and graph adapters, canonical
text, raw SMF codec, score exporter, PCM/WAVE renderer, tests, and portable examples use
only repository units and the applicable standard FPC/pas2js RTL. They neither
initialize nor reference a submodule.

The original `01_simple_A_major` and `02_simple_song_riffs` studies retain their
authored fluent note rules in shared FPC/pas2js Pascal units. Their native hosts
accept caller-selected seed, note count, and tempo; the riff study also accepts
the `mary`, `bridge`, and `hot-cross` grammar set. A caller may request one new
MIDI or streaming WAVE file, but neither host writes an artifact by default.
The WAVE path uses the project-owned renderer without a preview-duration cap.

The studies no longer use a GUI toolkit, submodule, or third-party playback
runtime. Native hosts write owned MIDI or WAVE artifacts for a player, while
the browser uses standard HTML audio as a host API. The exact score and
generated result remain useful without a playback device.

## present limits and research direction

Music Foundation v1 is a tested vertical slice, not the complete music exit
gate. It currently has:

- monophonic score-to-cell projection; chords exist in scores and MIDI export
  but cannot be represented by one melody cell stream;
- fixed quantization chosen by the caller, with no swing, tuplets, expressive
  timing, or automatic quantizer;
- pitch-class harmony constraints, not chord symbols, keys, scale spelling,
  voice leading, consonance objectives, or functional harmony;
- hard same-coordinate cross-pass constraints and bounded pass-level
  negotiation, without offset look-ahead, coordinate-minimal phrase repair,
  conflict-directed search, or soft scoring;
- caller-selected, fixed-grid training rather than automatic expressive
  quantization; [semantic MIDI import and excerpt training](music-import.md)
  are available in separate project-owned units;
- an SMF format-0 score exporter and explicit-policy format-0/1 score import,
  not lossless interpretation of every MIDI controller or performance detail;
- fixed-corpus browser studios and native file export, not an
  arbitrary score/corpus editor or embedded native playback device;
- a bounded triangle-wave audio preview, not realistic instruments,
  band-limited synthesis, effects, or mastering.

Negotiated Variation v1 adds exact public motif locks, atomic ordinary and
bounded negotiated regeneration, explicit selective provider horizons,
immutable result capture, and strict public replay. It does not change the
fixed-quantum monophonic representation or claim a musical objective.

Promising next passes include meter and phrase structure, rhythm, harmonic
motion, melody, bass, voicing, counterpoint, dynamics, and ornamentation. The
exact IR also makes less conventional experiments testable: constraints over
microtonal step systems, rhythmic tilings, motivic transformation, tension
curves, instrument ranges, and multi-voice dependency DAGs can publish their
models, seeds, artifacts, validation rules, and failures as reproducible
results.

See [sequences](sequences.md) for latent order-N semantics and [passes](passes.md)
for atomic dependency behavior.
