# Music Studio

A music workbench over the portable harmony + rhythm -> melody pipeline.
The bounded phrase editor supports public-cell locks, selective repair, a
piano roll, user-initiated playback, and downloads. A separate arrangement
source generates fresh sections for a requested duration and streams one
complete WAVE/RF64 file, using native FPC or the pas2js browser host.

Everything in this example—including its four short training phrases and
PCM/WAV synthesis—is project-authored under the repository MIT license.
Only repository units, the compiler RTL, and standard host file/DOM/media
APIs are required. No SoundShop, SDL2, samples, soundfonts, CDN, or playback
library is used.

## Run the native phrase editor/exporter

From the repository root, with FPC 3.2.2 or a compatible development compiler:

```powershell
New-Item -ItemType Directory -Force build/music-studio/native/units,
  build/music-studio/native/bin | Out-Null
fpc -B -Mdelphi -Sa -Cr -Co -Ci -Fusrc -Fuexamples/music/05_MusicStudio `
  -FUbuild/music-studio/native/units -FEbuild/music-studio/native/bin `
  examples/music/05_MusicStudio/MusicStudio.lpr
./build/music-studio/native/bin/MusicStudio.exe --selftest
./build/music-studio/native/bin/MusicStudio.exe 0 build/music-studio/my-preview
```

```bash
mkdir -p build/music-studio/native/units build/music-studio/native/bin
fpc -B -Mdelphi -Sa -Cr -Co -Ci -Fusrc -Fuexamples/music/05_MusicStudio \
  -FUbuild/music-studio/native/units -FEbuild/music-studio/native/bin \
  examples/music/05_MusicStudio/MusicStudio.lpr
./build/music-studio/native/bin/MusicStudio --selftest
./build/music-studio/native/bin/MusicStudio 0 build/music-studio/my-preview
```

The output directory must **not exist**; its parent must already exist.
The optional unsigned decimal seed defaults to zero. A failed solve reports
the failure and writes no output directory. A successful export writes:

| File | Meaning |
| --- | --- |
| `composition.wfcmusicpass` | Canonical public harmony, rhythm, melody, score, and seed |
| `score.wfcmusic` | Canonical exact score |
| `score.mid` | Standard MIDI File format 0 |
| `preview.wav` | Mono PCM16 preview, 44,100 Hz, four seconds |

Open the WAV in your normal media player. The native program is a console
inspector/exporter, not an embedded playback device or graphical window.
Without an output directory it prints the report, layers, and artifact
identities without writing files. `--selftest` exercises generation, a failed
ordinary edit, negotiated motif-preserving repair, audio export in memory,
and exact recovery.

Directory creation prevents ordinary overwrite. Export is four sequential
writes, not a transactional directory publisher: an I/O failure can leave a
partial new directory. Review that directory and choose a fresh name before
retrying. Native file creation assumes no adversarial concurrent writer in
the newly created output directory.

## Render a full native composition

The checked repository build also stages `MusicStudioRender`:

```powershell
./build/native/bin/MusicStudioRender.exe --seconds 180 --seed 0 `
  --output build/music-studio-180.wav
```

```bash
./build/native/bin/MusicStudioRender --seconds 180 --seed 0 \
  --output build/music-studio-180.wav
```

To build only this host, use the earlier compiler command with
`MusicStudioRender.lpr` instead of `MusicStudio.lpr`; its executable then
appears in the same chosen output directory.

Seconds must be positive decimal text with at most three fractional places.
The output rounds up to complete two-second 4/4 bars at 120 BPM: `4.001`
produces six seconds. Seed defaults to unsigned decimal zero. There is no
arbitrary song-duration policy cap; exact frame counts and available storage
remain finite. The command takes no opening locks.

The parent directory must exist and the output path must be new. Audio is
written to an exclusively created sibling partial file, one freshly solved
section at a time. Only a finished, flushed, closed file is published, without
replacing an existing destination even if it appeared during generation.
Caught failures remove the exact owned partial where possible; forced
termination can leave that partial behind. POSIX publication requires
filesystem hard-link support. This is a different, transactional single-file
path from the four-file phrase export above.

The renderer uses four-second sections, with a two-second final section when
needed. It writes RIFF or RF64 according to the exact final size and never
allocates a song-sized PCM array. See [Music arrangements v1](../../../docs/music-arrangement.md)
for the source API, continuity rules, capacity limits, and publication details.

## Browser phrase workflow

```powershell
./build-browser-music.ps1 -Compiler 'C:/path/to/pas2js.exe'
```

```bash
PAS2JS=/opt/pas2js/bin/pas2js bash ./build-browser-music.sh
```

Serve `build/browser/music/www` through any static HTTP server. For example,
with the included FPC development server:

```bash
build/native/bin/wfc_serve --root build/browser/music/www --port 8767
```

Build the server with the native gate first; use `wfc_serve.exe` on Windows.
See [development tools](../../../docs/development-tools.md) for its loopback-only serving boundary.

Open `http://127.0.0.1:8767/`. Startup generates seed zero once, but never
starts playback. No runtime request is made beyond loading the local HTML,
CSS, and compiled Pascal script.

1. Keep seed **0** and press **Generate**. Other seeds may legitimately exhaust
   the finite search budget.
2. Inspect the three public rows and monophonic piano roll. Cell indices are
   zero-based; click a cell to select its layer, position, and public token.
3. Press **Lock opening 2-cell motif**, then add a melody lock at cell **2**
   with **G4 attack / 96**.
4. Choose **one-way**, scope **harmony + dependents**, and Generate. This
   fixture contradicts. No prior composition, downloads, or preview remains
   current.
5. Choose **negotiated**, restore pass backtracks to **16**, and Generate.
   Harmony and melody are repaired in two rounds with one pass backtrack;
   rhythm is reused unchanged. The first two melody cells remain locked.
6. Press play to audition the current result or download its artifacts.
   **Clear all locks**, choose full composition, and Generate to recover the
   original seed-zero result.

The token picker lists vocabulary, not globally feasible choices. A listed
token can contradict the corpus, boundary, other locks, or prior passes.
Adding at the same layer/cell replaces the previous Studio lock. Removing a
selected lock and clearing all locks are distinct actions.

Changing seed text, strategy, scope, budgets, or trace capture invalidates
the displayed result immediately. Start a **New session** to apply a changed
seed; that also clears locks and the internal repair baseline. The browser
accepts decimal, `$hex`, and `0xhex` seeds.

The phrase editor creates binary downloads with a `Uint8Array` and a Blob. WAV
playback uses a normal HTML audio element; it never starts automatically.
Dirty edits and failed attempts pause/unload playback, revoke old object
URLs, clear generated cells/artifacts, and retain only the relevant report.
Actual file saving and media output are ultimately host/browser capabilities.

Append `?selftest=1` for an event-driven self-test of this workflow. It ends
at the restored baseline with `data-state="solved"`,
`data-self-test="passed"`, three executed passes, 16 cells, 123 MIDI bytes,
352844 WAV bytes, and zero automatic play events. Body artifact identities
match the table below. The self-test does not claim that a file was saved or
that a speaker emitted sound.

## Browser full-composition workflow

Choose seconds in the full-composition panel, then press **Generate + save
full composition WAV**. It captures the active session seed and a detached
copy of the public locks. Opening locks apply only to section index 0
(displayed as section 1); a lock outside a short first section or incompatible
with its model fails instead of being dropped. The arrangement source uses
its own fixed local search policy, not the phrase editor's strategy/scope
settings. Changing those inputs still cancels a captured save.

When the browser exposes the required save-file picker, select a destination.
The controller generates a section, renders it, awaits that section's write,
and only then proceeds. Its Blob transfers hold at most one section, not the
whole composition. Progress and the current section's pitches remain visible.
The browser yields between sections; Cancel or an input edit invalidates the
operation and requests abort after the current section/write settles. Final
file commit briefly disables editing and cancellation, and saved is reported
only after a successful close. The picker controls overwrite confirmation;
this browser path does not promise native-style no-overwrite publication.

If file streaming is unavailable, the panel shows a native `MusicStudioRender`
command. It carries duration and seed only: **browser opening locks are not
transferred**. It does not silently build a whole-song download in memory.

`?selftest=1` additionally requires `data-arrangement-test="passed"`. It
renders 4-, 6-, and 180-second fixtures into a counting sink and exercises
save, cancel, edit, write/close failure, and stale-result paths with a private
fake file backend. No file picker is opened and no user file is written by
these checks. Phrase and arrangement markers are separate terminal contracts.

## Phrase model and shared API

`music_studio_workbench.pas` owns four 16-cell corpora, their learned models,
the persistent pipeline, explicit caller locks, and the last successful
composition. The corpus notes are C4..C5 at velocity 96; attacks occur at cells
0, 2, 3, 4, 6, 8, 10, 11, and 12, with holds filling gaps and two closing
rests. Each cell is 240 ticks at 480 ticks/quarter and 500000 microseconds/
quarter: two 4/4 bars at 120 BPM.

Harmony and melody use order-3 learned contexts. Rhythm uses order 16,
deliberately fixing the complete two-bar form while shorter contexts allow
structural recombination elsewhere. Harmony here means a pitch-class plan,
not a progression of chord symbols. This is a narrow model, not a broad
compositional style model.

`TWfcMusicStudio.Create(seed)` starts idle. `Run(action, options)` generates
or selectively repairs; defaults are negotiated, 256 local backtracks,
16 pass backtracks, trace disabled. Supported limits are 0..1024 local and
0..32 pass; one-way requires zero pass backtracks. Counts bound search, not
wall time: one phrase solve runs synchronously and has no worker or
mid-solve cancellation API. Full arrangements yield and permit cancellation
between sections; they cannot interrupt a synchronous section solve.

`SetLock`, `ClearLock`, `ClearLocks`, and `LockOpeningMotif` edit public
constraints. `InvalidateCurrent` hides output while retaining a successful
internal baseline. `Reset` discards both. Selective work requires a baseline;
dirty provider layers expand the effective repair scope through their
descendants. Failed attempts keep that baseline only for subsequent repair,
never as current exported output.

`PublicTokens`, `CellTokens`, `MelodyCells`, `CopyLocks`, and `CopyReport`
return detached values. `CopyScore` returns a caller-owned immutable score;
free it. Current-only artifact methods reject stale or failed output.
Reports retain all-round counters, final per-pass outcomes, intended/actual
scope, graph failures, domain capture/validation evidence, and trace/
transcript hashes. A post-commit invariant or report-copy exception drops both
baselines rather than pairing a new graph state with an old composition.

The reusable domain and audio units live in `src`; this fixed-corpus
workbench and its UI are example-owned, not installed runtime units. See
[the music API](../../../docs/music.md) and
[Music Audio v1](../../../docs/music-audio.md) to build another model or host.
The arrangement source uses full-form harmony/rhythm contexts and order-3
melody, runs a fresh three-pass solve for every section, preserves an
attack/rest cadence, and forbids immediate harmonic-form repetition. It does
not replay a cached four-second audio clip. This local policy is described in
[Music arrangements v1](../../../docs/music-arrangement.md#the-supplied-studio-source);
it is not a global verse/chorus or thematic-form planner.

## Replay evidence and limits

| Artifact identity | Seed-zero baseline | Motif-preserving repair |
| --- | --- | --- |
| Public composition signature | `216F6EBB` | `1C1075DB` |
| Score-text FNV-1a | `4167E7E5` | `3690AE9B` |
| MIDI-byte FNV-1a | `86E4DCA3` | `93A9B159` |
| WAV-byte FNV-1a | `64679FF8` | `73A8591A` |

These are non-cryptographic regression identities. The exact four exported
native files and browser hashes match the same artifacts. The focused Studio
suite has 81 checks, and the audio suite has 35, exercised on FPC 3.2.2,
development FPC, and pas2js. These phrase identities do not describe a complete
multi-section arrangement.

The renderer is a bounded fixed-point triangle-wave preview, not instrument
modeling or mastering. The Studio has one monophonic voice and a fixed corpus,
tempo, meter, and section form. This UI does not expose semantic MIDI import,
an arbitrary corpus editor, polyphonic cell generation, phrase-local
optimization, or a soft musical objective. The underlying score/audio layers
support more than this example exposes.

See [the form experiment](../../../docs/research/music-studio-form-v1.md)
for source, raw 32-case results, explicit stopping rules, and failures.
