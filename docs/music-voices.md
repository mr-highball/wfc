# Independently learned musical voices

`wfc_music_voices_training`, `wfc_music_voices_graph`, and
`wfc_music_voices_stream` separate the learned pitch content of each musical
role. They reuse the exact [ensemble frame representation](music-ensemble.md):
a role may rest, attack a complete chord, or hold that chord while another
role attacks. They do not replace the existing joint-frame ensemble APIs or
change any earlier replay format.

The important distinction is vocabulary. Joint-frame training learns observed
vertical combinations as complete tokens. Independent-role training learns one
singleton `wme1` vocabulary per role, then constrains their simultaneous
choices through shared harmony, shared rhythm, pitch ranges, and optional
pair gaps. This can produce vertical combinations absent from the training
examples without constructing a Cartesian vocabulary of complete ensembles.

[Voice Studio](../examples/music/07_VoiceStudio/README.md) demonstrates the
method with two project-authored excerpts, a latest-cell inspector, coverage witnesses,
and incremental WAV/MIDI export. The [research record](research/independent-voices-v1.md)
states the exact representation proof, reproducible fixtures, and limitations.

## Models, tokens, and provenance

`BuildWfcMusicVoicesTrainingBundle` borrows an immutable `TWfcMusicScore`, an
ordered `TWfcMusicVoiceTrainingRoles` vector, common named excerpts, a positive
quantum, provider orders, and ordinary training metadata. A role contains its
stable `Id`, `SourceVoiceIndex`, and sequence `Order`.

The detached result owns ordinary training documents:

| Accessor | Learned public token |
| --- | --- |
| `CopyHarmonyDocument` | Exact union of selected sounding pitch classes: `wmhs1` |
| `CopyRhythmDocument` | Ordered action vector for all selected roles: `wmer1` |
| `CopyVoiceDocument(I)` | One chord-capable voice per `wme1` frame |

Every document has the same ordered, named excerpts. Each excerpt is a separate
sample; no transition is invented between samples. Role order controls both
singleton document identity and rhythm-vector slots, even when source voice
indices are not sorted. Source track/voice identifiers are available through
`RoleAt` provenance and never injected into learned musical tokens.

Every common excerpt must be quantum-aligned and long enough for every
requested order. Its edges may crop silence, but must not cut a sounded span
in **any** selected role. A cut through a held bass is rejected even if the
upper role happens to attack there. Training never repairs such a cut by
inventing a new attack. Unselected score voices do not become role slots.

The bundle checks total samples, tokens, encoded content, and work across all
documents before retaining them. Repeated document metadata and sample names,
role provenance, chord expansion, source/excerpt walks, and sequence histories
participate in the aggregate limits. The limits are the existing
`WFC_TRAINING_MAX_*` constants, not a separate maximum song duration. An
individually valid document does not imply that a whole bundle fits.

The bundle and every returned document are caller-owned and independent of the
input score. Free each document returned by a copy accessor. Generic training
and sequence text formats remain unchanged; use `LearnWfcTrainingModelText`
and `DecodeWfcSequenceText` to obtain caller-owned immutable models.

Valid sample timelines alone do not prove that every learned latent edge is
hold-safe. Graph configuration validates observed starts and **every**
`StatesCompatible` edge with the ensemble continuity predicates. An order-one
rest/attack-only vocabulary can be safe; order one with holds usually admits
invalid edges and is rejected by the graph adapter. The training bundle does
not silently alter the generic learner to hide that distinction.

## Constraints and exact coverage

`TWfcMusicVoicesGraphConfig` borrows `HarmonyModel`, `RhythmModel`, and the
models in `Voices`. Each `TWfcMusicVoiceModel` adds inclusive `MinPitch` and
`MaxPitch`. `StepsPerOctave` is a positive integer, not necessarily twelve.
`HarmonyMode` is explicitly `wmehmExact` or `wmehmAllowed`.

The model/pass order is stable:

| Model index | Public pass label |
| --- | --- |
| 0 | `harmony` |
| 1 | `rhythm` |
| 2 + I | `voice.I` |

Every voice's action must match its rhythm slot. Every sounding pitch must lie
in that role's range, and its pitch class must belong to the selected harmony
set. Holds participate in harmony exactly like attacks. This gives subset
semantics in allowed mode; the selected palette need not be completely sounded.

Exact mode additionally creates a `coverage.C` witness pass for each pitch
class occurring anywhere in the harmony vocabulary. Its value is `absent`
when the current harmony omits that class; otherwise it names the lowest-index
voice that actually sounds it. Earlier voices must omit that class. Together
with per-voice subset constraints this proves
that the complete sounding union equals the selected harmony set. It does
**not** require each voice to play the whole chord. Several voices may supply
the same class; the canonical witness names one supplier, not an exclusive owner.
Allowed mode has no coverage passes or witness records.

Only observed harmony classes receive witness passes. The implementation does
not allocate a pass for every possible tuning step. Lowest-supplier
canonicalization removes equivalent witness assignments for a fixed musical
choice. The initial noncanonical experiment exhausted a pass budget on those
equivalent proofs; the [research record](research/independent-voices-v1.md)
records that negative result. Musical and latent sequence choices can still
produce combinatorial search; avoiding a joint vocabulary does not remove it.

Optional `PairConstraints` use:

```text
gap = lowest pitch of UpperVoice - highest pitch of LowerVoice
MinGap <= gap <= MaxGap
```

`LowerVoice < UpperVoice` sets dependency order, not an automatic musical
ordering. A nonnegative minimum prevents crossing for that pair; zero permits
unisons and negative minima explicitly permit overlap. `wmvprSuspend` skips
the pair when either role rests; `wmvprReject` forbids that rest combination.
Duplicate pairs are rejected: intersect their bounds explicitly. There is no
general melodic-leap, parallel-motion, spelling, or counterpoint rule API in
this version. The small corpus's smooth motion is a tested property of that
corpus, not a general graph guarantee.

## A bounded generation loop

This complete example uses the authored model helper. Compile with `-Fusrc`
and `-Fuexamples/music/07_VoiceStudio`, plus your usual output directories.

```pascal
program IndependentVoices;
{$mode delphi}{$H+}
uses SysUtils, wfc, wfc_sequence, wfc_music_arrangement,
  wfc_music_ensemble, wfc_music_ensemble_graph, wfc_music_voices_training,
  wfc_music_voices_graph, wfc_music_voices_stream, voice_studio_corpus;
var
  Harmony, Rhythm: TWfcSequenceModel;
  Models: TWfcMusicVoiceSequenceModels;
  GraphConfig: TWfcMusicVoicesGraphConfig;
  Config: TWfcMusicVoicesStreamConfig;
  Stream: TWfcMusicVoicesStream;
  Segment: TWfcMusicVoicesSegment;
  Frames: TWfcMusicEnsembleFrames;
  Report: TGraphNegotiationReport;
  Step: TWfcMusicArrangementStep;
  I: Integer;
begin
  BuildVoiceStudioModels(Harmony, Rhythm, Models);
  Stream := nil;
  try
    GraphConfig := Default(TWfcMusicVoicesGraphConfig);
    GraphConfig.HarmonyModel := Harmony;
    GraphConfig.RhythmModel := Rhythm;
    GraphConfig.StepsPerOctave := VOICE_STUDIO_STEPS;
    GraphConfig.HarmonyMode := wmehmExact;
    SetLength(GraphConfig.Voices, Length(Models));
    for I := 0 to High(Models) do
    begin
      GraphConfig.Voices[I].Model := Models[I];
      GraphConfig.Voices[I].MinPitch := VoiceStudioRoleMinimumPitch(I);
      GraphConfig.Voices[I].MaxPitch := VoiceStudioRoleMaximumPitch(I);
    end;
    Config := DefaultWfcMusicVoicesStreamConfig(GraphConfig,
      VOICE_STUDIO_QUANTUM, 16 * VOICE_STUDIO_QUANTUM, 1);
    Config.SegmentCellCount := 5;
    Config.Search.SolveOptions.MaxBacktracks := 1024;
    Config.Search.MaxPassBacktracks := 64;
    Stream := TWfcMusicVoicesStream.Create(Config);
    repeat
      Step := Stream.Next(Segment, Report);
      if Step = wmaspProduced then
      try
        Frames := Segment.CopyFrames;
        WriteLn('segment ', Segment.Index, ': ', Length(Frames), ' cells');
        { Consume these frames before requesting another segment. }
      finally
        Segment.Free;
      end;
    until Step <> wmaspProduced;
    if Step = wmaspFailed then
      raise Exception.Create(Stream.Failure);
  finally
    Stream.Free;
    Harmony.Free;
    Rhythm.Free;
    for I := 0 to High(Models) do Models[I].Free;
  end;
end.
```

The stream copies configuration vectors but borrows immutable models; keep
those models alive until it is freed. Results and their nested copy-accessor
arrays are detached and may outlive both. Retaining every result is a caller
choice that grows memory; the stream itself does not retain emitted history.

For direct finite graph control, `BuildWfcMusicVoicesSegmentGraph` accepts one
`TWfcSequenceSegmentBoundary` per model and returns a caller-owned `TGraph`.
Use ordinary `TrySolve` or bounded `TrySolveNegotiated`, then
`CaptureSolvedWfcMusicVoices`. Capture independently checks layout, original
boundaries, model identities, latent paths and emissions, domains/locks,
rhythm, ranges, pairs, harmonic union, and coverage. Do not bypass capture and
treat private graph keys as musical tokens.

## Frontiers, publication, and failure

An initial segment uses observed start states. A continuing segment's first
domain is precisely the set of states compatible with its authenticated
previous state. The frontier retains a previous state and emitted token for
**harmony, rhythm, and every role**, not just the last merged musical frame.
This preserves order-N context, including legitimate partial BOS histories
when an early segment is shorter than the model order.

A continuing result may start with holds. It is not a standalone score and
must not be rebuilt as though it began a fresh performance. Append its frames
to the existing audio or MIDI state. The incoming voice state proves that
held pitches and velocities match the preceding attack or hold.

`RequireObservedEnd=False` permits a finite cutoff at any valid final state.
Setting it true requires an observed end state in every model on the final
segment. It does not ask the solver to insert rests. Intermediate segments
always use continuation semantics, not independently restarted whole samples.

`IntersectAllowedTokens(ModelIndex, Position, Tokens)` constrains an
unproduced **global cell position**, not a tick. Repeated calls intersect;
an empty list is an explicit contradiction. `ClearAllowedTokens` removes all
stored clauses for that model/cell. Edits reject malformed tokens, produced
positions, terminal streams, and edits during `Next`. Copies and replacement
arrays are prepared before live constraint state changes.

The protected `ConfigureSegment` hook may add restrictions to its borrowed
temporary graph. It must not retain that graph. Reentry is rejected. A changed
derived seed or graph shape is rejected, and independent capture plus stored
global-constraint validation prevents clearing graph rules from bypassing the
original musical requirements. Cancellation observed before publication
returns no candidate. All result/frontier allocations and validation precede
public progress mutation.

Search failure makes the stream terminal failed; an exception marks failure
and propagates. Both preserve its last published frontier and yield no new
segment. `Cancel` never rewinds or repairs the emitted past. Callers choose
whether to discard a partially produced output or keep its validated prefix.
Native and browser file hosts publish only a completed file transaction.

## Capacity, audio, MIDI, and replay

Global ticks and segment indices use the exact portable range through
`2^53-1`; local graph cells/ticks use checked `Integer` indexing. Requested
ticks are resolved against the quantum with an explicit rounding policy.
These are numeric and storage boundaries, not a fixed minute cap.

For fixed models and local horizon, retained generator state is a frontier
linear in model count, one bounded graph/result under construction, and
explicit future constraints. Model vocabularies, chord sizes, coverage
classes, pair relations, search reports, and caller-retained results can all
increase memory. Model validation and graph construction recur per segment.
Large local horizons or exhaustive search budgets can make one synchronous
call expensive even when total-duration memory is bounded.

The existing [incremental ensemble audio renderer](music-ensemble-stream.md)
accepts the merged frames, preserves held oscillator phases, and converts
ticks through its rational clock. Drain its bounded PCM blocks before feeding
more input; call `EndInput`, then drain its retained final samples before completing the
RIFF/RF64 writer. The [MIDI stream](music-midi-stream.md) instead counts and
hashes a deterministic generation pass, then replays it into a forward-only
format-0 file. Its MIDI-specific pitch/channel/track-length limits remain
explicit. Neither path collects a whole-song byte array or browser Blob.

Voice Studio provides the native `VoiceStudioRender` host and the separate
`build-browser-voices.ps1` / `build-browser-voices.sh` browser staging scripts.
For example, after the native build, export to a new destination:

```bash
build/native/bin/VoiceStudioRender --format wave --seconds 8 --seed 1 --output voices.wav
build/native/bin/VoiceStudioRender --format midi --seconds 8 --seed 1 --output voices.mid
```

Use `VoiceStudioRender.exe` on Windows. Duration is caller-selected; eight
seconds is a demonstration, not a limit. The authored host defaults to seed 1,
five-cell segments, 1024 local backtracks and 64 pass backtracks. These finite
search choices are not a guarantee for other seeds or policies.
See its [host guide](../examples/music/07_VoiceStudio/README.md) for actual
command options, cooperative save/cancel behavior, and file-system support.
The [build guide](building.md) and [FPC development tools](development-tools.md)
describe native and real-browser verification.

Training, graph, stream, and segment signatures have separate version-one
constants. Segment seeds use the existing versioned
`WfcMusicArrangementSectionSeed` schedule. Signatures are deterministic
32-bit checks, not cryptographic identity or complete replay specifications.
Replay also requires identical ordered models, constraints, policies, seed,
segment size, and relevant algorithm versions. Changing segmentation can
change output. The exact joint-frame and monophonic API signatures remain
unchanged.

No version-one contract guarantees global long-range feasibility. A valid
published prefix can lead to a later dead end, and limited search can fail
without proving impossibility. This milestone separates roles and proves
their local collective constraints; larger musical form, richer harmony,
general voice leading, and expressive performance remain work.
