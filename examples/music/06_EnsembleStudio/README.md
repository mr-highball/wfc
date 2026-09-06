# Ensemble Studio

Ensemble Studio is a dependency-free Pascal example of a genuine three-pass,
three-voice music pipeline. Independent harmony and rhythm providers constrain
the final ensemble pass, which materializes held bass, chord accompaniment,
and an upper line into a canonical score.

The showcase is deliberately structural. It uses a fixed eighth-note quantum,
4/4 meter, 120 BPM, project-authored training phrases, and a simple deterministic
preview timbre. It does not claim instrument realism.

## What it demonstrates

- harmony + rhythm -> ensemble pass lineage;
- a score with three ordered voices, including polyphonic chord spans;
- deterministic seed and whole-bar extent configuration;
- full generation and harmony-, rhythm-, or ensemble-rooted repair;
- exact public token locks with immediate stale-output invalidation;
- terminal pass, validation, failure, and search-count reporting;
- canonical score text and MIDI export; and
- an explicit, user-triggered WAV preview for scores accepted by the existing
  bounded audio adapter.

Bars are user-selected and are not subject to an arbitrary minute ceiling.
The requested score grid and its search state are allocated in memory, so work
and storage grow with that input and checked numeric/platform limits still
apply. The browser displays at most the first 128 cells per lane for
responsiveness; this display window does not shorten the score or its exports.

The WAV preview is separate from generation. A score beyond the adapter's
documented preview limit remains a successful composition with current score
and MIDI outputs; the UI reports that preview is unavailable and never clips
the score. MIDI adapter rejection likewise leaves the canonical score current.

The separate continuous stream is not tied to the finite editor grid. It
accepts positive decimal seconds, rounds upward only to the next eighth-note
cell, and exposes the requested and actual durations in preflight. The browser
shows both before it opens a WAVE destination or begins a MIDI counting pass.
Five-cell working segments
deliberately cross held bass and chord boundaries. The generator carries the
exact learned frontier between segments, and the synthesizer carries note phase
and envelopes between PCM blocks; it does not restart voices or loop a finished
audio fixture at a seam.

## Native showcase

From the repository root, the normal `build.ps1` or `build.sh` native gate
produces `build/native/bin/EnsembleStudio.exe` on Windows or
`build/native/bin/EnsembleStudio` on other supported hosts. A focused current
compiler build is:

```powershell
New-Item -ItemType Directory -Force build/ensemble/native | Out-Null
fpc -B -Mdelphi -Fusrc -Fuexamples/music/06_EnsembleStudio `
  -FUbuild/ensemble/native -FEbuild/ensemble/native `
  examples/music/06_EnsembleStudio/EnsembleStudio.lpr
```

Then run:

```text
EnsembleStudio [--seed UINT32] [--bars POSITIVE] [--output NEW-DIRECTORY]
EnsembleStudio --selftest
```

The output directory must not already exist. The host prepares artifacts before
creating it, refuses replacement, and always writes `score.wfcmusic` when score
export succeeds. It writes `score.mid` and `preview.wav` only when their
respective adapters accept the score, with an explicit diagnostic otherwise.

The default seed-zero, two-bar run has these portable identities:

```text
composition 573E2010
score       33123E67
MIDI        07361333 (227 bytes)
WAV         A273067B (352844 bytes; 176400 frames at 44100 Hz)
transcript  2E106AD9
```

## Browser showcase

Compile and stage the RTL-only browser app with either repository script:

```powershell
.\build-browser-ensemble.ps1 -Compiler 'C:\path\to\pas2js.exe'
```

```sh
PAS2JS=/path/to/pas2js bash ./build-browser-ensemble.sh
```

The staged site is `build/browser/ensemble/www`. Serve that directory with the
project's included FPC server rather than opening `index.html` directly:

```powershell
.\build\native\bin\wfc_serve.exe --root build/browser/ensemble/www --port 4178
```

```sh
build/native/bin/wfc_serve --root build/browser/ensemble/www --port 4178
```

For another device on your trusted Wi-Fi, add `--bind` with this computer's
private IPv4 address and open that address with port `4178`. The default remains
localhost-only. See [LAN serving and firewall scope](../../../docs/development-tools.md#access-from-a-trusted-local-network).
Over LAN HTTP, a browser may not provide the direct streaming file picker;
the preview, ordinary exports, and native streaming fallback are separate.

Seed or bar edits clear all current score/media immediately and require **New
session**, which also clears locks and the hidden repair baseline. Strategy,
scope, allowance, trace, and lock edits also clear visible artifacts, but retain
the current session baseline for an explicit repair. A failed solve exposes its
terminal report and no partial or stale score, MIDI, or audio.

The browser never autoplays. **Render preview** is a user action that creates a
project-owned WAV Blob; playback begins only if the user then activates the
standard HTML audio control.

## Continuous WAVE and MIDI streams

The continuous renderer retains one generated segment and one bounded PCM
block, not a whole-song score or audio buffer. Duration has no arbitrary minute
policy ceiling. Exact tick/frame arithmetic, RIFF/RF64 capacity, available
storage, generation cost, and platform numeric limits still apply.

Enter any positive decimal number of seconds. Supplied fractional precision
is not capped. Planning rounds upward first to an exact tick, then to this
demo's quarter-second musical cell; it never pads to whole bars. Requested and
actual lengths are shown separately. For example, `6.125` seconds produces
`6.25` seconds. Neither an example value nor the initial input is a target.

The normal native build gate produces `build/native/bin/EnsembleStudioRender.exe`
on Windows or `build/native/bin/EnsembleStudioRender` elsewhere. A focused
Windows build from the repository root is:

```powershell
New-Item -ItemType Directory -Force build/ensemble/render/units,build/ensemble/render/bin | Out-Null
fpc -B -Mdelphi -Fusrc -Futools -Fuexamples/music/06_EnsembleStudio `
  -FUbuild/ensemble/render/units -FEbuild/ensemble/render/bin `
  examples/music/06_EnsembleStudio/EnsembleStudioRender.lpr
```

Render to a path that does not yet exist:

```powershell
.\build\ensemble\render\bin\EnsembleStudioRender.exe --seconds 73.12500001 `
  --seed 0 --segment-cells 5 --backtracks 256 --pass-backtracks 16 `
  --output D:\new-ensemble.wav
```

The complete host contract is:

```text
EnsembleStudioRender --seconds DURATION --output NEW-WAVE-PATH
  [--seed UINT32] [--segment-cells POSITIVE]
  [--backtracks NONNEGATIVE] [--pass-backtracks NONNEGATIVE] [--trace]
```

The native host exclusively creates a unique sibling temporary file, publishes
it atomically only after exact frame completion, refuses an existing output
path, and removes only the temporary file it owns after cancellation or
failure. Ctrl+C requests cancellation between bounded generation/write steps.

In the browser, **Stream WAVE with Save As** uses the browser's user-mediated
file picker and awaits every bounded write before producing the next block. A
changed input cancels the captured transaction; a stale picker or write
completion cannot publish success. During the final close, stream controls are
briefly frozen because that commit can no longer be cancelled safely. Browser
Save As may replace a file explicitly selected by the user. Where direct
streaming file access is unavailable, the page displays a native command with
the same captured seed, segment size, search allowances, trace choice, and
duration.

Streaming MIDI uses the same frame generator without allocating PCM. Its
format-0 track maps the bass, chord, and upper voices to channels 0, 1, and 2,
with TPQ 480, constant 120 BPM tempo, and 4/4 meter. The counting pass retains
only configuration, counts, and deterministic fingerprints—not frames or an
event timeline. It must complete before any destination is opened.

The browser therefore separates **Plan MIDI** from **Save planned MIDI**.
Planning is asynchronous and consumes no file-picker activation. Saving must
be a new explicit user click; it opens the picker immediately, regenerates the
same frame stream, awaits every block write, and closes only after tick, byte,
event, and generation fingerprints match. Editing a captured input invalidates
the plan and cancels a pending replay. Browser Save As may replace the file the
user selects.

The normal native gate also produces `EnsembleStudioMidiRender.exe` on Windows
or `EnsembleStudioMidiRender` elsewhere. A focused Windows build is:

```powershell
fpc -B -Mdelphi -Fusrc -Futools -Fuexamples/music/06_EnsembleStudio `
  -FUbuild/ensemble/render/units -FEbuild/ensemble/render/bin `
  examples/music/06_EnsembleStudio/EnsembleStudioMidiRender.lpr
```

The full native gate places this executable in `build/native/bin`; the isolated
build above uses `build/ensemble/render/bin`. Invoke it from that directory or
use its full path, adding `.exe` on Windows. `DURATION` and `NEW-MIDI-PATH`
below are placeholders for your positive decimal seconds and new output path:

```text
EnsembleStudioMidiRender --seconds DURATION --output NEW-MIDI-PATH
  [--seed UINT32] [--segment-cells POSITIVE]
  [--backtracks NONNEGATIVE] [--pass-backtracks NONNEGATIVE] [--trace]
```

It completes the full counting pass before creating an exclusive sibling,
replays and verifies the stream, and atomically publishes without replacing an
existing path. Ctrl+C before or during replay leaves no claimed completed file.

Continuous output is intentionally separate from the finite editor's short
preview and finite-score MIDI adapter. It generates a fresh composition from the current
seed and stream settings; finite editor locks and repair scope are not
transferred. The reusable stream API supports explicit future-cell constraints
for applications that need them.

## Source layout

- `ensemble_studio_workbench.pas` is the portable state, generation, lock,
  validation, reporting, and artifact owner.
- `ensemble_studio_demo.pas` contains the shared deterministic proof used by
  the native and browser conformance programs.
- `ensemble_studio_stream.pas` owns duration preflight, authored stream models,
  the common incremental frame generator, and the stateful PCM pull source.
- `ensemble_studio_midi_stream.pas` owns bounded MIDI counting and verified
  replay over that common frame source.
- `EnsembleStudio.lpr` is the native console and export host.
- `EnsembleStudioRender.lpr` is the no-replace atomic streaming WAVE host.
- `EnsembleStudioMidiRender.lpr` is the two-pass no-replace streaming MIDI host.
- `browser_ensemble_studio_app.pas` contains browser DOM, Blob, and media glue.
- `browser_ensemble_stream.pas` contains the bounded asynchronous browser file
  transaction and its fake-backend conformance checks.
- `BrowserEnsembleStudio.lpr`, `index.html`, and `ensemblestudio.css` form the
  interactive browser entry point and presentation.

All music, training phrases, rendering code, and runtime logic are
project-owned; there are no external samples or synthesis libraries.
