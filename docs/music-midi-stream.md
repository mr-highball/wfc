# Bounded-memory Standard MIDI Files

The project-owned [SMF stream](../src/wfc_midi_stream.pas) and
[ensemble MIDI adapter](../src/wfc_music_ensemble_midi.pas) write one format-0,
multichannel Standard MIDI File without retaining a score, event timeline, or
complete file. Both are MIT Pascal units for native FPC and pas2js.

They complement, rather than change, the existing finite
`wfc_midi_smf` / `wfc_music_midi` encoders and their checked output.
[Ensemble Studio](../examples/music/06_EnsembleStudio/README.md) supplies native
and browser hosts. [Continuous ensembles](music-ensemble-stream.md) describes
the bounded generation source shared by audio and MIDI.

## Count, replay, publish

An SMF track header contains its byte length before its events. A forward-only
destination therefore needs that length up front. The implementation uses two
passes over the same trusted deterministic source:

1. Count canonical event bytes and record end tick, logical event count,
   synthetic delay-bridge count, and a versioned diagnostic fingerprint.
2. Regenerate the same music and drain bounded MIDI blocks behind the known
   header. Keep the source's models, seed, segmentation, options, constraints,
   and hooks unchanged.
3. Finish and verify the replay, then publish the destination. On any failure
   or cancellation, discard the complete output prefix.

Planning costs generation time, but does not keep the generated timeline.
The immutable plan is a small summary, not an audio buffer, portable model
identity, or resumable solver checkpoint. Replay costs a second generation
pass. The host must apply backpressure rather than queue an entire song.

The FNV32 signature is a diagnostic fingerprint, **not cryptographic proof of
identical content**. Exact byte/event/bridge counts and end tick are checked
as well, but those checks do not authenticate untrusted or adversarial replay.
Use identical immutable inputs and trusted generation behavior. The core
does not open, close, flush, or publish a destination.

The native demo writes an exclusively owned sibling partial and publishes
only a successfully verified new file, never overwriting an existing target.
The browser has separate **Plan MIDI** and **Save planned MIDI** actions:
planning can take time, so opening the file picker belongs to a fresh user
gesture. The browser awaits each bounded write, aborts invalidated/cancelled
work, and closes only verified output. Browser destination replacement and
durability are governed by the browser's file API, not native no-overwrite
semantics.

## Ensemble API

`DefaultWfcMusicEnsembleMidiOptions([0, 1, 2])` declares three ordered voices
mapped to zero-based channels 0, 1, and 2. Defaults are 480 ticks per quarter,
500000 microseconds per quarter, and 4/4 meter. Options and channel arrays are
copied; `Plan.CopyOptions` returns a detached copy.

MIDI channels are unique per voice, in 0..15, with 1..16 voices. This avoids
ambiguous overlapping equal pitches from different voices on one channel.
General MIDI reserves human channel 10 (zero-based 9) for percussion; choosing
it remains an explicit caller mapping, not a universal SMF rule.
No bank, program, instrument, or tuning messages are invented.
See the [official General MIDI Level 1 specification](https://midi.org/general-midi-level-1).

Each sounding voice contains sorted, unique MIDI pitches 0..127, velocities
1..127, and one of the ensemble's attack/hold/rest actions. These twelve-step
MIDI adapter bounds do not restrict the general ensemble token representation.
A hold must match the active voice's pitches **and velocities** exactly. It
emits no new attack, including across generation-segment boundaries.

The counting pass is:

```pascal
Options := DefaultWfcMusicEnsembleMidiOptions([0, 1, 2]);
Counter := TWfcMusicEnsembleMidiCounter.Create(Options);
try
  { For every frame from the first generation pass: }
  Counter.AdmitFrame(Frame, QuantumTicks);
  { Only after the complete source succeeds: }
  Plan := Counter.Finish;
finally
  Counter.Free;
end;
```

This is a lifecycle sketch: `Frame`, `QuantumTicks`, and the loop over the
source belong to the caller. `Plan` is caller-owned and must eventually be
freed. A repeated successful `Finish` returns another detached plan.
Further input is rejected.

Replay creates `TWfcMusicEnsembleMidiStream.Create(Plan)`. It copies the plan,
so the caller may then free it. Initially the header, tempo and meter are
pending. Drain `ReadBytes(MaxBytes, Bytes)` until it returns false before the
first `AdmitFrame`, and after every admission. Admit only while
`NeedsInput=True`. After the entire regenerated source succeeds, call
`EndInput`, drain again, and require `Finished=True` before publication.

Each successful `ReadBytes` returns detached bytes, at most 4096 even for a
larger requested maximum. False always returns nil. It can mean ready for the
next frame, finished, or cancelled; inspect the state properties.
`TickCount` counts admitted duration; `EmittedBytes` counts all returned
file bytes, including the 22-byte file/chunk header. It does not mean a host
has successfully persisted those bytes.

A frame's positive wide tick length may span a long held note or rest.
The cursor retains only active and pending voice frames and event positions.
It emits one logical event at a time. Empty input is valid: initial tempo,
meter, and EOT at tick zero. The Studio's user duration must still be positive.

### Timing and deterministic ordering

The optional third admission argument is `TWfcMusicEnsembleMidiTiming`:

- `TempoMicrosecondsPerQuarter=0` means no tempo change; otherwise 1..16777215.
- `MeterNumerator=0` means no meter change, requiring denominator power zero.
  Otherwise numerator is 1..255 and denominator power is 0..255.
- `Default(TWfcMusicEnsembleMidiTiming)` changes nothing.

Explicit changes are emitted at the frame's starting tick even if the value
equals the preceding one. Tempo precedes meter, then all note-offs precede
all note-ons. Notes are ordered by voice index and then ascending pitch,
independent of channel-number order. The time-signature payload fixes MIDI
clocks per metronome click to 24 and notated 32nds per quarter to 8.
TPQ remains fixed for the file.

A rest closes its active chord; an attack closes and reattacks, even for the
same pitches. `EndInput` closes all remaining tones at the exact accepted end.
It adds no tail and must never be called at an ordinary segment boundary.
These same-tick ordering rules are project policies, not SMF requirements.

### Failure contract

Invalid arguments or admission state reject before modifying the current
frame and are retryable. Processing errors, capacity exhaustion during the
counting pass, or replay divergence set `Failed` and propagate an exception.
A replay that exceeds plan budgets may fail before the end; other mismatches
are found when final EOT is admitted. Already-returned note-offs or other
prefix bytes are not rolled back. The destination must remain unpublished.

`Cancel` is terminal and discards pending data without emitting EOT.
Repeated cancellation is harmless; no later input can turn it into success.
Cancellation after successful completion is a no-op and preserves Finished.
Destruction neither finishes output nor calls a host. Calls are sequential,
non-reentrant, and not thread-safe.

## General event API

The lower-level `TWfcMidiTrackCounter` accepts nondecreasing absolute ticks:

```pascal
Counter.AppendEvent(AbsoluteTick, Event);
TrackPlan := Counter.Finish(EndTick);
Writer := TWfcMidiFileStream.Create(TicksPerQuarter, TrackPlan);
```

`Event` is a `TWfcMidiEvent` with `DeltaTicks=0`. Channel, meta and SysEx
payloads use the existing SMF representation. The stream computes deltas.
The caller cannot submit EOT; `Finish` owns it. Duplicate-tick event order
is exactly caller order. The counter validates and hashes payload bytes
without retaining them; the writer deep-copies one admitted event payload.
A large explicit payload therefore costs its own memory, even though it is
serialized in bounded blocks.

Drain the initial header before `AdmitEvent(Tick, Event)`, and drain each
event before the next. After all events, drain and call `Finish(EndTick)`,
then read through `Finished`. The writer copies plan scalars, allowing early
plan release. The same cancellation, unpublished-prefix, and replay rules apply.

A track plan's `ByteCount` excludes the 22-byte format-0 file/chunk header.
`EventCount` includes owned EOT but excludes generated delay bridges.
`BridgeCount` counts those separately. End tick and signature are immutable.
Unlike the ensemble wrapper, this track-only plan does not bind the file's TPQ.

## Real limits, without a minute policy

There is no arbitrary composition-duration cap. Format and arithmetic limits
remain explicit:

| Quantity | Envelope |
| --- | --- |
| Portable absolute ticks/count arithmetic | Exact nonnegative integers through 9007199254740991, on both native and browser targets |
| Metrical ticks per quarter | 1..32767 |
| Track payload bytes | 0..4294967295, the SMF 32-bit track-length field |
| One encoded delta or meta/SysEx payload length | 0..268435455, a four-byte SMF VLQ |
| One returned byte block | At most 4096 |
| Local arrays | Compiler Integer indexing and available memory |

Long silence or held notes are not rejected merely because one delta exceeds
the VLQ maximum. Let `D` be the gap and `M=268435455`. For positive `D`,
insert `(D-1) div M` empty text events, each with delta `M`, leaving a final
logical-event delta in 1..`M`. Zero gap needs no bridge. Every bridge is seven
bytes: the four-byte delta followed by `FF 01 00`. This applies before EOT
too, preserving terminal silence exactly.

This is a project encoding policy using valid zero-length text metadata,
not a newly claimed MIDI standard. It does not add notes, tempo, or audible
content; a metadata viewer can expose the empty text events. The counter
calculates their number in constant time per logical event. The writer emits
them incrementally. Actual writing still takes time and storage proportional
to output bytes. SMF's format-0, chunk-length, VLQ, text-event and final-EOT
requirements are defined in the
[official Standard MIDI Files specification](https://midi.org/standard-midi-files-specification).

The MIDI duration planner does not pass through PCM sample counts or WAVE's
file envelope. Conversely, a valid MIDI plan does not promise that an audio
render of the same duration will fit its transport. Existing finite-score
export/import resource policies are unchanged. In particular, the bounded
excerpt importer is not a validator for arbitrarily large streamed files.

## Executable contracts and remaining work

Independent conformance programs cover
[general event streaming](../test/wfc_midi_stream_test.lpr) and
[ensemble streaming](../test/wfc_music_ensemble_midi_test.lpr), including
finite-export byte parity, held chords, event ordering, timing changes,
long gaps, malformed numeric inputs, detached ownership and replay failure.
The native and browser build gates include both.
The [shared host suite](../test/wfc_music_ensemble_midi_stream_demo_test.lpr)
checks exact decimal duration, copied plans, generated frame fingerprints,
and count/replay goldens. The
[native process suite](../test/wfc_music_ensemble_midi_render_process_test.lpr)
checks actual output, independent decoding, seed replay, positive short and
longer durations, malformed commands, and preservation of existing files.
Browser file transactions have an additional awaited completion marker;
the synchronous harness alone cannot certify a successful plan/save lifecycle.

Version constants are `WFC_MIDI_STREAM_VERSION=1` and
`WFC_MUSIC_ENSEMBLE_MIDI_VERSION=1`. Existing finite MIDI goldens remain
unchanged. Streaming does not add global musical-form optimization, arbitrary
long-range feasibility, serializable resume state, expressive synthesis,
format-1 multi-track streaming, MIDI 2.0, or infinite storage.
