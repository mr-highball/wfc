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

Seed or bar edits clear all current score/media immediately and require **New
session**, which also clears locks and the hidden repair baseline. Strategy,
scope, allowance, trace, and lock edits also clear visible artifacts, but retain
the current session baseline for an explicit repair. A failed solve exposes its
terminal report and no partial or stale score, MIDI, or audio.

The browser never autoplays. **Render preview** is a user action that creates a
project-owned WAV Blob; playback begins only if the user then activates the
standard HTML audio control.

## Source layout

- `ensemble_studio_workbench.pas` is the portable state, generation, lock,
  validation, reporting, and artifact owner.
- `ensemble_studio_demo.pas` contains the shared deterministic proof used by
  the native and browser conformance programs.
- `EnsembleStudio.lpr` is the native console and export host.
- `browser_ensemble_studio_app.pas` contains browser DOM, Blob, and media glue.
- `BrowserEnsembleStudio.lpr`, `index.html`, and `ensemblestudio.css` form the
  interactive browser entry point and presentation.

All music, training phrases, rendering code, and runtime logic are
project-owned; there are no external samples or synthesis libraries.
