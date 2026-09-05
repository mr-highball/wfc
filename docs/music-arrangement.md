# Music arrangements v1

`wfc_music_arrangement` makes a long composition a sequence of freshly solved,
independently validated sections. `wfc_music_audio_stream` writes their PCM
clips into one WAVE/RF64 file without collecting the whole song in memory.
Both are project-owned MIT Pascal units for native FPC and pas2js; host file
and browser operations stay at the edge.

This is a temporal composition boundary, not a claim that local constraints
solve every song-wide objective. The iterator retains an exact bounded context
tail. A specialized section source decides what that context means musically.
The supplied Music Studio source adds cadence and immediate-harmony-repeat
rules; it does not implement verse/chorus planning, long-range thematic memory,
whole-song optimization, or global novelty.

## Duration in Music Studio

The shared example helper `MusicStudioArrangementConfig(seconds, seed)` accepts
positive decimal seconds with at most three fractional places. Signs,
exponents, empty fractions, and zero are invalid. For example, use `180` or
`4.001`, not `3e2`. The helper parses decimal digits using exact integer
arithmetic, rounds up to ticks, then rounds up to complete bars:

| Requested seconds | Actual seconds | Generated sections |
| ---: | ---: | --- |
| `0.001` | 2 | one 8-cell section |
| `4` | 4 | one 16-cell section |
| `4.001` | 6 | one 16-cell section, then one 8-cell section |
| `6` | 6 | one 16-cell section, then one 8-cell section |
| `180` | 180 | 45 fresh 16-cell sections |

The source uses 480 ticks per quarter, 500,000 microseconds per quarter
(120 BPM), and 4/4. Each cell is 240 ticks; a bar is 1,920 ticks/two seconds.
Full sections are two bars/four seconds, and a final half-section is one
complete bar. `MusicStudioRequestedTicks` performs the seconds-to-ticks step;
`MusicStudioArrangementConfig` adds bar rounding, sets a 16-cell continuity
tail, and uses exact iterator rounding. Its `RequestedTicks` is therefore
already bar-rounded; preserve the original seconds string separately when
displaying requested versus actual duration.

There is no arbitrary song-duration or section-count policy cap. Exact integer
arithmetic, the WAVE writer's frame envelope, finite storage, and the host's
ability to keep generating/writing remain real limits. Capacity is checked
before opening a native partial file or invoking a browser file picker.

## Reusable section-source API

The public version is `WFC_MUSIC_ARRANGEMENT_VERSION = 1`. The main types are in
[wfc_music_arrangement.pas](../src/wfc_music_arrangement.pas):

```pascal
TWfcMusicArrangementConfig = record
  RequestedTicks: TWfcMusicArrangementWide;
  QuantumTicks: Integer;
  SectionCellCount: Integer;
  ContinuityCellCount: Integer;
  BaseSeed: TGraphSeed;
  Rounding: TWfcMusicArrangementRounding;
end;

TWfcMusicArrangementSectionSource = class
public
  function GenerateSection(
    const ARequest: TWfcMusicArrangementSectionRequest;
    out AComposition: TWfcMusicComposition;
    out AFailure: String): Boolean; virtual; abstract;
  function ValidateContinuity(
    const ARequest: TWfcMusicArrangementSectionRequest;
    const AComposition: TWfcMusicComposition;
    out AFailure: String): Boolean; virtual; abstract;
end;
```

`DefaultWfcMusicArrangementConfig(requestedTicks, quantumTicks, seed)` uses
16-cell sections, a two-cell tail, and `wmarExact`. The generic iterator does
not impose Music Studio's tempo or bar policy. `wmarExact` rejects a partial
cell, `wmarFloorToCell` rounds down (but cannot yield an empty arrangement),
and `wmarCeilToCell` rounds up with checked overflow. A final section may be
shorter than `SectionCellCount`; the source must be able to produce a valid
score for that length.

A request contains zero-based `Index`, absolute `StartTick`, section-local
`LengthTicks` and `CellCount`, a derived `Seed`, and detached `PriorContext`.
The context records `HasPrevious`, absolute `EndTick`, and harmony/rhythm/
melody public-token tails of at most `ContinuityCellCount` cells each.

`GenerateSection` transfers a new composition on success. An expected finite
search failure returns `False` with a useful message; unexpected exceptions
propagate. The iterator checks the composition signature, requested seed,
quantum, cell count, score length, public-layer/score consistency, and stable
tick/pitch format across sections. Each section must start with an attack or
rest, never a hidden hold from an earlier score. It then calls the source's
`ValidateContinuity` with a fresh authenticated copy of the previous tail.
This check is model-specific; the iterator does not infer musical transitions
from a matching hash alone.

Create `TWfcMusicArrangement(config, source)` and call `Next(out section)`.
Its result is `wmaspProduced`, `wmaspCompleted`, `wmaspCancelled`, or
`wmaspFailed`. Only `Produced` returns a section. Free each section after use;
it owns its `Composition`. The iterator borrows the source, so keep the source
alive until the iterator is freed. Neither retains previously yielded scores.
`CopyContext` returns detached arrays. Progress is available through
`ProducedTicks`, `RemainingTicks`, `SectionCount`, `NextIndex`, and `Status`.

Failure is terminal and returns no partial candidate; already transferred
sections remain caller-owned. `Failure` explains an expected failed step.
`Cancel` is observed between synchronous `Next` calls; it is not an interrupt
inside a solver and the iterator is not a background worker. Hosts must still
decide whether to discard an unfinished output transaction.

The section seed is derived from the version, base seed, and zero-based
64-bit section index through the project's FNV-1a byte mixer. It does not
depend on how often progress was polled or how quickly the host wrote data.
The public arithmetic ceiling is `9,007,199,254,740,991` on both native and
browser targets. Individual section lengths still fit positive `Integer`;
this separates bounded local scores from wide arrangement progress.

## The supplied Studio source

[music_studio_arrangement.pas](../examples/music/05_MusicStudio/music_studio_arrangement.pas)
implements `TMusicStudioSectionSource`. Its four authored corpora are the
same material as the phrase editor, but its model policy is distinct:
harmony and rhythm learn the complete 16-cell or 8-cell form, while melody
uses order-3 contexts. Every section runs a fresh harmony + rhythm -> melody
pass pipeline with a finite local search budget. It never substitutes repeated
PCM, a cached successful phrase, or a new seed after a failed solve.

The source copies any supplied `TWfcMusicStudioLocks` and applies them only to
section index **0** (displayed as section 1). A lock that falls outside a short
first section or contradicts its constraints fails; it is not silently dropped.
Later sections use new derived seeds and their own continuity constraints.

Every section starts with an attack and closes with two melody/rhythm rests.
Pitch classes agree with harmony and actions agree with rhythm. The following
section requires that rest-separated ending and forbids the previous harmony
form key: cell 2 distinguishes complete two-bar forms; cell 0 distinguishes a
final one-bar form from the preceding last bar. These keys are specific to
this authored corpus. They prevent immediate harmonic-form repetition, not
all melodic resemblance, eventual recurrence, or repetition over an entire
composition. New sources can implement stronger policies explicitly.

## Stream a native WAVE file

The native build gate stages `MusicStudioRender` in `build/native/bin`:

```powershell
./build/native/bin/MusicStudioRender.exe --seconds 180 --seed 0 `
  --output build/music-studio-180.wav
```

```bash
./build/native/bin/MusicStudioRender --seconds 180 --seed 0 \
  --output build/music-studio-180.wav
```

`--seconds` and `--output` are required. `--seed` defaults to zero and accepts
unsigned decimal `0..4294967295`. `--help` and `--version` are standalone.
The output's parent must already exist, and the destination must not exist.
The native command supplies no opening locks; it is not an import of the
browser's lock state.

The renderer computes the exact expected frame count at 44,100 Hz, exclusively
creates a unique sibling partial file, and renders/writes one section at a
time. After all frames are written, `Finish` succeeds and the file is flushed
and closed. Only then is it published under the requested name. Windows uses
a non-replacing move; POSIX hosts use non-replacing hard-link publication
followed by removal of the owned partial name. Filesystems without the
required operation fail safely. An existing destination, including one created
while rendering is underway, is preserved.

Caught generation, I/O, or publication failures remove the exact owned partial
file where possible and report failure; no completed output is advertised.
A forced process termination or power loss may leave the uniquely named
partial behind. This is no-overwrite publication, not a promise of crash-proof
storage durability. Progress is printed without retaining the full song.

Each section still uses the [bounded clip renderer](music-audio.md), whose
60-second in-memory preview limit is unchanged. The sequential writer joins
small PCM clips, not multiple WAVE files or one unbounded score. It uses
4,096-byte PCM blocks and switches from a 44-byte RIFF header to an 80-byte
RF64 header when necessary. See [the streaming audio contract](music-audio.md#sequential-wave-and-rf64-streaming)
for exact counts, ownership, and sink-failure rules.

## Browser stream saving

Music Studio's full-composition panel captures the active session seed,
duration, and a detached copy of public opening locks. It asks the browser for
a save location before generating audio. When the required file-picker API is
available, the controller opens a writable file and transfers at most one
section-sized Blob at a time. It awaits each write before generating the next
section; it never falls back to a whole-song Blob in memory.

The browser yields between sections. Cancel or a relevant input edit
invalidates the operation, waits for the current synchronous section or pending
write to settle, and requests abort. Stale picker/write completions cannot
publish success. During final `close`, editing and cancellation are disabled;
the UI says saved only after that promise succeeds. Failures, including a
failed abort, are shown without claiming a completed save.

The browser picker controls destination selection and overwrite confirmation;
it does **not** provide the native command's no-overwrite guarantee. If stream
saving is unavailable, the panel shows a `MusicStudioRender` command instead.
That fallback transfers the seed and duration only, **not browser opening
locks**. Short phrase previews and their ordinary downloads remain separate.

## Evidence and scope

The native process suite checks actual 4-, 6-, and 180-second WAVE files,
byte-identical replay/prefixes, fractional rounding, capacity failures,
no-overwrite behavior, and cleanup after a raced destination. A 180-second
44,100 Hz mono PCM16 RIFF file has 7,938,000 frames and 15,876,044 bytes.
Checked local FPC 3.2.2 and 3.3.1 Windows runs passed all 58 process checks.

Portable iterator, streaming, and Studio-source suites run in the native and
real-browser conformance gates. The Studio `?selftest=1` route additionally
renders 4-, 6-, and 180-second arrangements into a counting sink and exercises
the actual asynchronous save controller with a private fake file backend.
It tests ordered writes, commit, cancellation, input invalidation, write/close
failure, and stale results without touching disk or opening a picker. Its
`data-arrangement-test=passed` is not evidence that a user file was saved or
that audio was played. See [development tools](development-tools.md) and
[the Studio guide](../examples/music/05_MusicStudio/README.md).
