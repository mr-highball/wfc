# Music import and training extraction

The music import path converts a deliberately narrow Standard MIDI File (SMF)
subset into the project's exact `TWfcMusicScore`, a deterministic import
receipt, or an editable `wfclearn=1` sequence corpus. The implementation is
project-owned Pascal and has no service, plug-in, sample library, or external
runtime dependency.

This is a semantic import, not a source-file editor:

```text
SMF format 0/1 (PPQ)
        |
        +--> exact wfcmusic=1 score
        +--> deterministic wfcmidiimport=1 receipt
        `--> selected voice ranges --> melody/rhythm/harmony wfclearn=1
```

The MIDI Association describes SMF as an interchange representation for one
or more time-stamped streams, including track, tempo, and time-signature
information; it explicitly distinguishes that interchange role from an
application's normal working format. See its
[Standard MIDI Files overview](https://midi.org/standard-midi-files). The
project follows the official MIDI 1.0 distinction between Note On and Note Off
messages shown in the Association's
[message summary](https://midi.org/summary-of-midi-1-0-messages) and
[status-byte table](https://midi.org/expanded-midi-1-0-messages-list).

## Supported import boundary

`wfc_music_midi_import` accepts synchronous SMF format 0 or 1 with a positive
PPQ division. Format 0 must contain exactly one track. The semantic core is
Note On and Note Off, including velocity-zero Note On as a release. Every
positive-velocity Note On must have one later matching release for its global
channel/pitch key. Missing releases, unmatched releases, zero-duration notes,
and overlapping Note Ons for the same channel/pitch reject instead of being
guessed.

Global event order is `(absolute tick, source track, source event index)`. The
note-on track owns the imported note. Overlapping notes of different pitches
are allocated to deterministic monophonic lanes for that source track and
channel. This retains separate attacks; it does **not** merge overlaps into
chord regions. The resulting exact score can therefore have several voices
even when the source used one channel. An input with no notes gets one
explicit synthetic silent voice so that a positive silent timeline remains a
valid score.

Tempo and meter events are retained when representable. If absent, import uses
500,000 microseconds per quarter note and 4/4, and records each default in the
receipt. Conflicting same-tick timing, inexact measure lengths, or meter
changes away from a complete preceding measure reject. Timing at the exact
source end has no interval to govern and is omitted with a reported count.

Every track's structural End Of Track delta completes that track's duration;
source duration is the maximum completed track duration. Every structural End
Of Track is included in `events-including-eot`.
The default `wmmepPadMeasure` policy extends an incomplete final measure with
explicit score silence. The receipt distinguishes `source-ticks`,
`score-ticks`, and `padding-ticks`. `wmmepRequireMeasure` instead rejects an
incomplete final measure.

### Strict and explicit lossy policies

The default `wmmupReject` policy rejects performance channel events, system
exclusive data, and opaque metadata that the exact score does not represent.
`wmmupIgnoreAndReport` is an explicit lossy choice: those events are omitted
and counted as `ignored-channel`, `ignored-system`, or `ignored-meta`.

Some source details are harmless to the score relation and are always omitted
with explicit receipt fields: text metadata, sequence/key metadata, track
names, a meter's metronome-click setting, and release velocity. Release
velocity is counted in `discarded-release-velocities`; omitted track names are
counted separately. Device name, channel prefix, port, and SMPTE-offset
metadata always reject because silently dropping them could change how later
events are interpreted.

The result is not a byte-round-trip representation of the input. Canonical
score text does not preserve chunk layout, running-status spelling, source
event order among semantically equivalent events, track names, release
velocity, ignored events, or original bytes. Keep the original file when
those facts matter. The receipt describes the accepted semantic conversion;
it is not a reversible copy of the source format.

## Import API

The caller-owned score and detached report are produced by:

```pascal
type
  TWfcMusicMidiUnsupportedPolicy = (
    wmmupReject,
    wmmupIgnoreAndReport
  );
  TWfcMusicMidiEndPolicy = (
    wmmepRequireMeasure,
    wmmepPadMeasure
  );

function DefaultWfcMusicMidiImportOptions:
  TWfcMusicMidiImportOptions;

function ImportWfcMusicMidi(const AFile: TWfcMidiFile;
  const AOptions: TWfcMusicMidiImportOptions;
  out AReport: TWfcMusicMidiImportReport): TWfcMusicScore;

function DecodeWfcMusicMidi(const ABytes: TWfcMidiBytes;
  const AOptions: TWfcMusicMidiImportOptions;
  out AReport: TWfcMusicMidiImportReport): TWfcMusicScore;
```

`ImportWfcMusicMidi` consumes an already parsed value. `DecodeWfcMusicMidi`
first applies the bounded SMF byte parser. Both validate all input before
building the timeline, notes, lanes, or score. The caller owns and must free a
successful score. Input arrays are not retained, and the report is detached.
Any rejection resets the report and returns no partial score.

The version-1 import envelope is finite:

| Quantity | Limit |
| --- | ---: |
| Encoded input or canonical typed-input envelope | 16,777,216 bytes |
| Tracks | 256 |
| Events, including one structural End Of Track per track | 131,072 |
| Notes | 65,536 |
| Monophonic voices/lanes | 256 |
| Score spans | 131,328 |
| Raw tempo events | 4,096 |
| Raw meter events | 4,096 |
| PPQ | 1–32,767 |

Tick accumulation, measure arithmetic, padding, and score fields must also fit
the shared positive `Integer` representation. These are data and work bounds,
not wall-time promises.

## Exact score-to-training bridge

`wfc_music_training` extracts explicitly selected score intervals without
building and then cropping a full-score cell timeline:

```pascal
const
  WFC_MUSIC_TRAINING_VERSION = 1;

type
  EWfcMusicTraining = class(EWfcMusic);
  TWfcMusicTrainingProjection = (
    wmtpMelody,
    wmtpRhythm,
    wmtpHarmony
  );
  TWfcMusicTrainingSelection = record
    Name: TWfcModelToken;
    VoiceIndex: Integer;
    StartTick: Integer;
    LengthTicks: Integer;
  end;
  TWfcMusicTrainingSelections =
    array of TWfcMusicTrainingSelection;

function MakeWfcMusicTrainingSelection(
  const AName: TWfcModelToken;
  const AVoiceIndex, AStartTick,
  ALengthTicks: Integer): TWfcMusicTrainingSelection;

function BuildWfcMusicTrainingDocument(
  const AScore: TWfcMusicScore;
  const ASelections: TWfcMusicTrainingSelections;
  const AProjection: TWfcMusicTrainingProjection;
  const AQuantumTicks, AOrder: Integer;
  const AMetadata: TWfcTrainingMetadata): TWfcTrainingDocument;
```

The returned caller-owned immutable document always uses
`wtkSequence`, `wmbOpen`, `wmsNone`, a `0,0` pattern footprint, the specified
positive order, and sample height 1. Each selection becomes one sample in the
declared order. Sequence history resets at every sample, so there is no learned
transition between different phrases or voices.

Selection start must be nonnegative, length must be positive, the complete
range must fit the score, and both values must align exactly to
`AQuantumTicks`. Every intersecting sounded span
must be monophonic, wholly contained by the selection, and have an aligned
onset and duration. A selection may crop silence at either edge. Internal rest
boundaries must align. A selection that cuts either end of a note rejects;
chords reject for every projection. Repeated attacks remain attacks even when
their pitches match.

The bridge first forms exact melody cells:

- `wmtpMelody` retains attack/hold/rest, pitch, and velocity;
- `wmtpRhythm` retains attack/hold/rest only;
- `wmtpHarmony` maps sounded cells to pitch class and retains explicit rests.

See [music foundation](music.md#fixed-quantum-sequence-cells) for canonical
cell tokens. The document can be serialized with `EncodeWfcTrainingText`,
learned with `LearnWfcTrainingModelText`, or compiled into a recipe with
`LearnWfcTrainingRecipe`.

The bridge preserves `AMetadata` verbatim. In particular, it never supplies or
infers a license. Name, license identifier, source description, and sample
names must be nonempty valid UTF-8 tokens. The caller must declare rights and
provenance appropriate to the selected material.

The shared training limits apply: at most 4,096 selections, 65,536 total
cells, 65,536 cells in one selection, order 1–64, 65,536 canonical characters
per token, and 4,194,304 canonical characters across metadata/sample names and
tokens. Preflight also requires
`score.SpanCount * selection count <= 16,777,216`, checked without overflow,
before allocating sample cells.

## Native command-line importer

The native host is a bounded I/O shell around the portable app unit:

```text
Usage:
  wfc_music_import [--score | --report] [POLICY] [--] INPUT
  wfc_music_import (--melody | --rhythm | --harmony) [POLICY]
    --quantum TICKS --order N --name NAME --license ID --source TEXT
    --sample NAME,VOICE,START,LENGTH [--sample ...] [--] INPUT
  wfc_music_import --help | --version

POLICY:
  --ignore-performance
  --require-measure
```

`INPUT` is one file or `-` for binary standard input. All options precede it;
`--` permits a path beginning with a hyphen. `--score` is the default and emits
canonical `wfcmusic=1`. `--report` emits a canonical receipt. The three
projection modes emit canonical `wfclearn=1`.

Training mode requires every one of `--quantum`, `--order`, `--name`,
`--license`, `--source`, and at least one `--sample`. There is no default or
guessed license. Voice indices are zero-based. A sample record means exactly
`NAME,VOICE,START,LENGTH`; repeated records stay separate and ordered. Name,
license, source, and sample-name fields use the project's canonical
percent-encoded UTF-8 token syntax, so a literal comma in a sample name must be
written as `%2C`.

For example, these commands inspect, import, and extract two independent
melody phrases:

```text
wfc_music_import --report song.mid
wfc_music_import --score song.mid > song.wfcmusic
wfc_music_import --melody --quantum 120 --order 3 --name phrases --license CC0-1.0 --source owned --sample verse,0,0,1920 --sample ending,0,1920,960 song.mid > melody.wfclearn
wfc_learn --model melody.wfclearn > melody.wfcs
```

The tool does not quantize, split/cut notes, write files on its own, or fetch
source data.

### Receipt and provenance

`wfcmidiimport=1` records the source byte count, import policies, source
format, track/event/note counts, source/score/padding ticks, every omission and
default count, and the mapping from imported voice to source track/channel/
lane. Training receipts additionally record projection, quantum, order, and
every ordered sample selection.

Training mode appends the complete receipt to the caller's exact
`SourceDescription`, separated by one LF. It never replaces the caller's
description. Canonical `wfclearn=1` percent-encodes that embedded LF and all
other reserved or non-ASCII UTF-8 bytes.

`source-fnv1a32` is an eight-digit uppercase FNV-1a checksum of the exact input
bytes. It is a deterministic change detector and replay label, not a
cryptographic digest, collision-resistant identity, proof of authorship, or
license verification. Preserve the original source and your own stronger
integrity metadata when those properties matter.

### Failures and output discipline

Successful data is written only to standard output. Failure emits one
LF-terminated diagnostic to standard error and no partial/stale standard
output:

| Exit | Meaning |
| ---: | --- |
| 0 | Success |
| 1 | Invalid SMF, import policy/score constraint, or training selection |
| 2 | Invalid command grammar or option usage |
| 3 | Input/output failure in the native host |
| 70 | Unexpected internal failure |

The native input reader enforces the same 16 MiB maximum before calling the
portable app. Its binary-string compatibility entry point interprets each
character as exactly one byte in `0..255`; browser callers should pass an
`ArrayBuffer`-derived `TWfcMidiBytes` value to `WfcMusicImportExecuteBytes`.
Neither interface treats the binary file as UTF-8 text.
