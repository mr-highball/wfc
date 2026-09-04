# music foundation

The music foundation turns a validated, exact score into discrete sequence
cells, composes learned rhythm and harmony passes into a melody pass, rebuilds
the score, and exports a Standard MIDI File. All of that code is project-owned
Pascal shared by native FPC and pas2js. Playback is deliberately outside the
foundation.

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
before the graph commits any pass output.

The lower-level generic API is `TWfcSequenceProjectionRules`,
`MakeWfcSequenceProjectionRule`, and
`ValidateSequenceProjectionMapFromPass` /
`RequireSequenceProjectionMapFromPass`. It can preflight and express other
music relations without adding domain knowledge to the graph core.

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
format 0, and there is no SMF-to-`TWfcMusicScore` semantic importer yet.

## dependency and license boundary

The exact score, cell codecs, sequence learning and graph adapters, canonical
text, raw SMF codec, score exporter, tests, and pass-composition example use
only repository units and the applicable standard FPC/pas2js RTL. They neither
initialize nor reference a submodule.

The older `01_simple_A_major` and `02_simple_song_riffs` experiments remain as
historical examples. Their Lazarus/LCL UI and SDL2 playback route through the
optional SoundShop submodule, which is GPL-3.0. That dependency is isolated
from the MIT runtime package, normal build, tests, portable demo, and canonical
formats. Do not copy its types into the portable API or make it a required
playback path.

Playback is an edge adapter. A future native synthesizer, WebAudio host, or
other renderer should consume validated project-owned events and remain
optional; the score and generated result must stay useful without it.

## present limits and research direction

Music Foundation v1 is a tested vertical slice, not the complete music exit
gate. It currently has:

- monophonic score-to-cell projection; chords exist in scores and MIDI export
  but cannot be represented by one melody cell stream;
- fixed quantization chosen by the caller, with no swing, tuplets, expressive
  timing, or automatic quantizer;
- pitch-class harmony constraints, not chord symbols, keys, scale spelling,
  voice leading, consonance objectives, or functional harmony;
- hard same-coordinate cross-pass constraints, without offset look-ahead,
  phrase-level repair, negotiation, or soft scoring;
- raw token corpora supplied by the caller, without a semantic MIDI learner;
- an SMF format-0 score exporter but no score importer;
- console/Node output only: no browser UI, native playback, or WebAudio
  playback is claimed.

Promising next passes include meter and phrase structure, rhythm, harmonic
motion, melody, bass, voicing, counterpoint, dynamics, and ornamentation. The
exact IR also makes less conventional experiments testable: constraints over
microtonal step systems, rhythmic tilings, motivic transformation, tension
curves, instrument ranges, and multi-voice dependency DAGs can publish their
models, seeds, artifacts, validation rules, and failures as reproducible
results.

See [sequences](sequences.md) for latent order-N semantics and [passes](passes.md)
for atomic dependency behavior.
