# Continuous polyphonic ensemble streams

`wfc_music_ensemble_stream` solves a long composition in bounded segments while
carrying the exact sequence context and sounding voices across every seam.
`wfc_music_ensemble_audio` renders that timeline incrementally; the existing
`wfc_music_audio_stream` writes its sample blocks as one WAVE/RF64 file.
These are project-owned [MIT](../LICENSE) Pascal units for native FPC and
pas2js, using repository units and the compiler RTL. Host file and browser
operations stay outside the reusable libraries.

This is not concatenation of independent finite scores or previews. A held
two-tone bass can cross a segment boundary while another voice attacks, rests,
or reattacks the same pitches. The bass retains its original attack, exact
pitch/velocity pairs, oscillator phase, and envelope age.

For frame representation, learning, and the finite-score owner, read
[Synchronized polyphonic music](music-ensemble.md). See the
[music examples](../examples/music/README.md) and
[Ensemble Studio](../examples/music/06_EnsembleStudio/README.md) for host-specific
usage. This document specifies the reusable APIs, not host command-line flags.

## A segment is a continuation, not a new song

Every segment has the same three ordered passes: `harmony`, `rhythm`, and
`ensemble`. Harmony and rhythm are independent providers; ensemble requires
both. Rhythm must equal the ordered voice-action vector. With `wmehmExact`,
the harmony set must equal the union of all sounding pitch classes, including
held notes. With `wmehmAllowed`, that union must be a subset of the provider
set. Silence matches an empty exact set, or any allowed set in the same tuning.

The owner builds on the generic segment contract in
[wfc_sequence_graph.pas](../src/wfc_sequence_graph.pas):

```pascal
TWfcSequenceSegmentBoundary = record
  HasPrevious: Boolean;
  PreviousState: Integer;
  RequireObservedEnd: Boolean;
end;
```

- `MakeWfcSequenceInitialSegmentBoundary(RequireEnd)` sets `HasPrevious=False`
  and `PreviousState=-1`. The first state must have an observed start count.
- `MakeWfcSequenceContinuingSegmentBoundary(PreviousState, RequireEnd)` allows
  exactly those first states for which
  `Model.StatesCompatible(PreviousState, FirstState)` is true.
- A required observed end constrains the last state to have an observed end
  count. It does not insert a rest, cadence, or extra cell.

Beginning-of-sequence (BOS) history is not discarded at the first seam. An
order-four model split into one-cell segments still needs its legitimate
partially populated beginning context in segments two and three. Continuing
boundaries therefore permit BOS-bearing states when the exact predecessor
allows them. They are not the existing BOS-free `wseFragment`/`wseSuffix`
extents, and they do not change those older APIs.

`ApplySequenceModelSegmentToGraph`, `ValidateSequenceSegmentStatePath`, and
`CaptureSolvedSequenceSegment` apply and verify this boundary. The generic
record declares a predecessor; by itself it does not authenticate a caller's
claimed history. The ensemble stream keeps its own private frontier, populated
only from the previously published result: one exact latent state and public
token per layer. Matching only the last public token would lose order-N
context. `CopyFrontier` is a detached diagnostic snapshot, not an import or
resume setter.

Initial ensemble frames must be legal starts. Every later frame, including
the first frame of each continued segment, must legally follow its actual
predecessor in every voice. Holds require identical pitches and velocities;
equal consecutive attacks are still distinct attacks. Model preflight checks
all compatible state edges rather than accepting unsafe holds merely because
the training samples were individually valid. Safe order-one rest/attack
models remain valid.

## Configure and consume

The public configuration is in
[wfc_music_ensemble_stream.pas](../src/wfc_music_ensemble_stream.pas):

```pascal
Config := DefaultWfcMusicEnsembleStreamConfig(
  Models, VoiceCount, StepsPerOctave, QuantumTicks, RequestedTicks, Seed);
Config.SegmentCellCount := 16;
Config.HarmonyMode := wmehmExact;
Config.RequireObservedEnd := False;
Stream := TWfcMusicEnsembleStream.Create(Config);
```

`Models` is a `TWfcMusicEnsembleModels` containing immutable harmony, rhythm,
and ensemble sequence models. Keep all three alive until `Stream` is freed.
The configuration is copied, but the models are borrowed. Tempo and ticks per
quarter belong to the consuming timing/audio application, not to this owner.

Defaults are 16 cells per segment, exact cell-aligned duration, exact harmony,
no observed-end requirement, 256 local backtracks, and 16 pass backtracks.
`Config.Search` is the existing `TGraphNegotiationOptions`; its finite search
and optional trace policies remain explicit.

Call `Next(Segment, Report)`, where `Segment` is a
`TWfcMusicEnsembleSegment` and `Report` is a `TGraphNegotiationReport`.

| Result | Meaning |
| --- | --- |
| `wmaspProduced` | One validated caller-owned segment was published. Consume and free it. |
| `wmaspCompleted` | No more segments; successful completion is idempotent. |
| `wmaspCancelled` | No new segment; cancellation is terminal. |
| `wmaspFailed` | Finite search failed; inspect `Failure` and the returned search report. |

The final segment may be shorter than `SegmentCellCount`. Its successful call
still returns `wmaspProduced`, with `FinalSegment=True` and stream status
`wmasCompleted`; the next call returns `wmaspCompleted`. There is no padding to
a full segment. Non-produced calls return a nil segment. Unexpected hook or
semantic-validation exceptions set the stream to failed and propagate.

Segments expose `Index`, `StartTick`, `CellCount`, `QuantumTicks`, `Seed`,
`FinalSegment`, and `Signature`. `CopyGenerated(Layer)` returns detached state
indices, public tokens, and the exact boundary. `CopyFrames` returns detached
frames, including nested tones. A result remains usable after the stream and
models are freed, although interpreting its state indices still requires the
same model definitions. Neither the stream nor a later call owns or mutates a
previously returned result.

A continued result may begin with a hold, so it deliberately has no
`CopyScore` pretending it is a self-contained finite score. Do not convert
those holds into attacks to satisfy a finite-score constructor. Reconstructing
a complete score requires accumulating the complete valid timeline and thus
does not preserve the streaming memory guarantee.

## Duration and end policy

`RequestedTicks` must be positive. `Rounding` chooses `wmarExact`,
`wmarFloorToCell`, or `wmarCeilToCell`; rounding is to the positive quantum,
not automatically to measures. An empty rounded result and an overflowing
ceiling are rejected. `ActualTicks` exposes the selected duration separately
from `RequestedTicks`.

The default finite cutoff may end at an interior model state. Setting
`RequireObservedEnd=True` requires an observed end in all three models on
the actual final segment only. This is a real constraint and may fail. It
does not make earlier local choices globally aware of a distant required end.

There is no arbitrary song-duration or segment-count policy limit. Portable
tick/frame counters use native `Int64` or browser `NativeInt`, checked through
`9007199254740991` (`2^53-1`) on both targets. Local array counts remain
`Integer`; `SegmentCellCount * QuantumTicks` must fit `Integer`. Existing
sequence-model/training resource limits still apply. Memory, compute time,
storage, and the final output format remain practical limits.

## Constraints, publication, and cancellation

`IntersectAllowedTokens(Layer, Position, Tokens)` constrains an absolute
zero-based cell coordinate, not a segment-local coordinate or tick. Tokens
must belong to the chosen public model; duplicate tokens within one argument
are rejected. Separate calls are conjunctive, including repeated calls at the
same coordinate. An empty set creates a real contradiction.
`ClearAllowedTokens(Layer, Position)` removes the stored caller clauses at
that future coordinate, without removing model or seam constraints.

Only unproduced cells may be edited. Validation and replacement-array
preparation precede publication of an edit. Invalid edits are retryable and
leave stored constraints unchanged. Successful segments retire their past
clauses; unsuccessful segments do not. `CopyConstraints` detaches its arrays.

A subclass may override `ConfigureSegment(Index, StartTick, CellCount, Graph)`
to inspect the borrowed graph and add application restrictions before search.
Do not retain or free that graph, change the derived seed, or call `Next`
recursively. Constraint edits during `Next` are rejected. The hook cannot
erase separately stored caller constraints by clearing a graph domain: the
owner independently checks those clauses, captured model identity and state
paths, voice continuity, rhythm, and harmony before publishing a candidate.

All result preparation and validation precede frontier/progress mutation.
Failed search, validation, or an application exception publishes no segment
and preserves the last committed frontier, produced ticks, index, and future
constraints. Failure is terminal for that stream. This is not rollback of
bytes an external consumer has already written.

`Cancel` is terminal for ready/active streams, preserves the emitted prefix,
and cannot turn completed or failed status into cancellation. Calls are
synchronous and non-reentrant, not thread-safe. Cancellation is cooperative
between calls or from the configuration hook; it does not preempt the finite
solver mid-call. Browser hosts should choose a responsive segment size and
yield to the event loop between bounded units of work.

Once history has been emitted, it cannot be repaired in place. Local
negotiation can revisit providers inside the current segment, not earlier
published segments. A locally valid prefix can dead-end later. There is no
guarantee of a solution for every requested duration, global musical-form
optimization, or arbitrary long-range constraint satisfaction.

## Continuous audio and sequential files

Create one `TWfcMusicEnsembleAudioRenderer` for the entire output, not one per
segment. Its constructor takes `TWfcMusicAudioOptions`, ticks per quarter,
and an array of maximum chord capacities, one per voice including silent
slots. Capacities are copied; their sum, with a minimum of one, fixes mixing
headroom for the whole stream. Admitted chords must fit their declared slot.
Using more headroom than the music needs makes it quieter.

The renderer accepts MIDI pitches `0..127` in twelve-step tuning, velocities
`1..127`, sample rates `32000..48000`, tempo `1..4000000` microseconds per
quarter, and attack/release settings `0..1000` milliseconds. These are this
synthesis adapter's bounds, not restrictions of the ensemble token model.
There is no preview-duration or fixed voice/chord-count policy cap here.

Drain the renderer before admitting the next frame. This helper consumes
bounded blocks synchronously; the caller supplies the sink and owns all three
objects:

```pascal
procedure DrainAudio(const Renderer: TWfcMusicEnsembleAudioRenderer;
  const Writer: TWfcMusicWaveStream);
var
  Samples: TWfcMusicPcm16Samples;
begin
  while Renderer.ReadSamples(2048, Samples) do
    Writer.AppendSamples(Samples);
end;

procedure RenderSegment(const Segment: TWfcMusicEnsembleSegment;
  const Renderer: TWfcMusicEnsembleAudioRenderer;
  const Writer: TWfcMusicWaveStream; const Tempo: Integer);
var
  Frames: TWfcMusicEnsembleFrames;
  I: Integer;
begin
  Frames := Segment.CopyFrames;
  for I := 0 to High(Frames) do
  begin
    Renderer.AdmitFrame(Frames[I], Segment.QuantumTicks, Tempo);
    DrainAudio(Renderer, Writer);
  end;
end;
```

Declare these types through `wfc_music_audio`, `wfc_music_audio_stream`,
`wfc_music_ensemble`, `wfc_music_ensemble_audio`, and
`wfc_music_ensemble_stream`. The sketch uses a constant tempo; callers may
supply a different tempo per admitted interval without resetting the clock.
Keep each segment only until its frames have been consumed.

`ReadSamples` returns at most 2048 detached PCM16 samples regardless of a
larger positive requested maximum. False returns an empty array: inspect
`NeedsInput`, `Finished`, or `Cancelled` rather than assuming end-of-song.
Even an interval that quantizes to zero samples applies its attacks, rests,
and holds. Bad input rejects before mutation; unexpected processing failures
poison the renderer and propagate.

After successful completion of the entire generation stream, and only after
draining its final frame, close the audio timeline exactly once:

```pascal
Renderer.EndInput;
DrainAudio(Renderer, Writer);
Writer.Finish;
```

Release shaping delays output by `LatencyFrames` and adjusts samples still
inside that bounded window when a real note ends. `EndInput` closes remaining
notes and drains the existing duration; it adds no tail. Never call it at an
ordinary segment boundary. `Cancel` discards unreturned samples; cancellation
or generation failure must not be reported as a successful complete file.

`AdvanceWfcMusicEnsembleAudioClock` is the pure timing helper. Start with
`Default(TWfcMusicEnsembleAudioClock)` and keep sample rate and ticks per
quarter fixed. It retains the fractional numerator in units of
`1 / (TicksPerQuarter * 1000000)` sample frames. Therefore accumulated frames
equal the floor of absolute elapsed musical time, even with changing tempos
and unequal interval partitions; they are not the sum of separately rounded
segment durations. Zero ticks is a validating no-op. Invalid timing or
overflow leaves the complete clock unchanged.

`TWfcMusicWaveStream.Create(Sink, SampleRate, ExpectedFrames)` needs the exact
total frame count before it writes the header. Plan that count from the same
timing policy before opening output. `AppendSamples` accepts borrowed PCM16
blocks synchronously and uses at most 4096 buffered bytes. A short `Finish`
is recoverable; an oversized append rejects before writing. A sink exception
poisons the writer and propagates; `FrameCount` counts only blocks whose sink
call returned successfully, since a failed sink may have written a prefix.
Neither writer destruction nor renderer destruction finishes an output or
closes a caller-owned sink.

Files use ordinary 44-byte RIFF/WAVE while its sizes fit, then an 80-byte RF64
header with `ds64`. This is mono PCM16 compatibility output, not a broadcast
metadata implementation. The exact file envelope is
`WFC_MUSIC_AUDIO_STREAM_MAX_FRAMES = (9007199254740991 - 80) div 2` frames.
That format/numeric limit is separate from a song-duration policy.

For an asynchronous browser writable stream, a sink may copy one bounded
block or segment batch, but the host must await its write before generating
more queued output. Yield between blocks/segments, check cancellation, and
abort rather than close a cancelled or failed file transaction. Do not collect
all samples, segments, or bytes into a whole-song Blob. Browser file streaming
availability and destination storage are host concerns, not library promises.

## What stays bounded, and what can grow

Memory is independent of total song duration only while the caller consumes
and releases results and applies bounded backpressure.

| Retained work | What determines its size |
| --- | --- |
| Borrowed models and prepared projection maps | Learned vocabulary, latent order/state count, voice/chord content |
| Current graph, candidate, and optional report/trace | Segment cell count, model size, finite search options |
| Private frontier | Three last state indices and three last public tokens; no accumulated timeline |
| Future caller constraints | Number of stored clauses and their token alternatives, including repeated calls at one coordinate |
| Audio state | Active tones per voice, fixed headroom metadata, and `ReleaseFrames + 1` integer mix slots |
| Audio and file blocks | At most 2048 returned samples and 4096 writer bytes per block |
| Caller-retained copies or queued output | Whatever the caller keeps; accumulating them forfeits bounded-memory streaming |

Chord capacity is an admission ceiling, not a preallocation of every possible
tone. The renderer retains active tone state; changing input is copied before
publication. Large models, chords, future-constraint lists, or segment sizes
still cost memory and CPU even when total duration does not.

## Replay and executable contracts

The new independent versions are `WFC_SEQUENCE_SEGMENT_VERSION=1`,
`WFC_MUSIC_ENSEMBLE_STREAM_VERSION=1`,
`WFC_MUSIC_ENSEMBLE_SEGMENT_SIGNATURE_VERSION=1`, and
`WFC_MUSIC_ENSEMBLE_AUDIO_VERSION=1`. Existing finite composition formats,
sequence extents, and monophonic replay contracts are unchanged.

Segment seeds use `WfcMusicArrangementSectionSeed(BaseSeed, Index)` and
`WFC_MUSIC_ARRANGEMENT_VERSION=1`, including segment zero. This is FNV-1a/32
over the version byte, four little-endian base-seed bytes, then eight
little-endian index bytes. It does not consume a shared random stream or
truncate indices to 32 bits. Independent seed fixtures for base seed 123 are:

| Segment index | Derived seed |
| ---: | --- |
| 0 | `$0EADAA47` |
| 4294967296 | `$BE816B36` |
| 4294967297 | `$01788A37` |
| 9007199254740991 | `$B6CDD64E` |

The stream conformance fixture's first five-cell segment has signature
`$5ED51FA1`: seed zero, 23 cells total, quantum two, two voices, exact harmony,
order-four ensemble/rhythm models, and the order-one harmony model built by
that test. This value identifies that fixture, not arbitrary music with the
same seed. Segment signatures include versioned boundary/state/token witnesses
and configuration fields; they are not cryptographic model identities or
portable authentication. Replay also requires identical immutable models,
construction order, constraints, hook behavior, segment size, and search
options. Changing the segmentation can legitimately change the generated
music. Audio partitioning of the same admitted timeline does not change PCM.

Executable specifications:

- [Sequence segments](../test/wfc_sequence_segment_test.lpr): exact incoming
  boundaries, BOS history, model identity, failed captures, and unchanged
  ordinary extents.
- [Ensemble streams](../test/wfc_music_ensemble_stream_test.lpr): one-cell
  seams across orders two through six, held chords, aggregate path validity,
  genuine allowed supersets and rejection of omitted held pitch classes,
  short final segments, observed-end success/failure, detached ownership,
  constraints, hooks, cancellation, and huge lazy plans.
- [Ensemble projection maps](../test/wfc_music_ensemble_graph_test.lpr) and
  [finite owner](../test/wfc_music_ensemble_passes_test.lpr): distinct exact
  and allowed harmony semantics, including silent and held voices.
- [Incremental ensemble audio](../test/wfc_music_ensemble_audio_test.lpr):
  sample-for-sample finite-renderer parity across rates/envelopes, held and
  reattacked chords, rational clock references, block partitioning, malformed
  input, lazy wide durations, and sequential WAVE failure accounting.
- [Shared stream host](../test/wfc_music_ensemble_stream_demo_test.lpr): exact
  decimal duration planning, short final segments, held seams beyond the
  training excerpt, cancellation, and forged-plan rejection. Browser runs
  additionally require the awaited file-transaction completion marker.
- [Native publication processes](../test/wfc_music_ensemble_render_process_test.lpr):
  real complete files beyond the finite preview adapter's limit, exact RIFF
  extents, deterministic replay, malformed options, existing/racing target
  preservation, and owned partial cleanup. Large RF64 fixtures inspect headers
  without writing an enormous file.

The established MIDI encoder remains a finite-score adapter with its own
limits. The separate [streamed MIDI contract](music-midi-stream.md) now counts
and replays the same continued frame source without retaining a score or
timeline. These APIs do not provide serialized resumable frontiers, whole-song
constraint solving,
independent marginal-voice recombination, expressive performance synthesis,
or automatic musical-form planning. The streamed generation and audio APIs
are reusable foundations for those separate capabilities.
