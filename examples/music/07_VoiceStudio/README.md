# Voice Studio

Voice Studio is a dependency-free Pascal example of independently learned
bass, chord, and upper-role sequence models constrained by shared harmony and
rhythm passes. The exact-harmony graph can combine role paths into vertical
pitch collections absent from both project-authored excerpts while each role
keeps its own range, rests, attacks, holds, and learned temporal transitions.

The displayed coverage witness chooses the deterministic lowest matching role
for each required pitch class. That choice proves coverage; it does not claim
that no other role sounds the same class.

## Streaming contract

The renderer accepts a positive decimal duration with no example-level minute
ceiling. It rounds upward to the next 120 BPM eighth-note cell (0.25 seconds)
and reports requested and actual duration separately. Numeric representation,
format capacity, available storage, and finite search allowances still apply.

Generation retains the learned frontier plus one caller-selected working
segment. The default five-cell segment deliberately cuts across held bass and
chord spans. WAVE synthesis retains phase and envelopes across those seams and
writes one bounded PCM block at a time. MIDI uses a separate counting pass and
deterministic replay; neither browser transport builds a whole-song Blob.

The browser offers two distinct MIDI actions. **Plan MIDI** counts and
fingerprints the complete generated transaction without opening a file picker
or retaining frames. **Save planned MIDI** is a later user click that opens the
picker immediately, regenerates the same cells, checks the plan, awaits each
bounded write, and commits only after exact tick and byte completion. Editing
any captured option invalidates the plan and cancels pending output. Browser
Save As may replace the destination explicitly selected by the user.

## Native renderer

The normal native gates produce `build/native/bin/VoiceStudioRender.exe` on
Windows or `build/native/bin/VoiceStudioRender` elsewhere. A focused Windows
build from the repository root is:

```powershell
New-Item -ItemType Directory -Force build/voices/render/units,build/voices/render/bin | Out-Null
fpc -B -Mdelphi -Fusrc -Futools -Fuexamples/music/07_VoiceStudio `
  -FUbuild/voices/render/units -FEbuild/voices/render/bin `
  examples/music/07_VoiceStudio/VoiceStudioRender.lpr
```

The command contract is:

```text
VoiceStudioRender --format wave|midi --seconds DURATION --output NEW-PATH
  [--seed UINT32] [--segment-cells POSITIVE]
  [--backtracks NONNEGATIVE] [--pass-backtracks NONNEGATIVE] [--trace]
VoiceStudioRender --selftest
```

Defaults are seed `1`, five cells per segment, 1024 local backtracks, 64 pass
backtracks, and trace capture off. The output path must not exist. The host
creates an exclusive sibling, publishes atomically only after complete
generation/encoding checks, and removes only its owned partial file on failure
or Ctrl+C cancellation.

For example, these commands exercise the same 5.25-second cell plan through
the independent WAVE and MIDI transports:

```powershell
build/native/bin/VoiceStudioRender.exe --format wave --seconds 5.25 --output D:\new-voices.wav
build/native/bin/VoiceStudioRender.exe --format midi --seconds 5.25 --output D:\new-voices.mid
```

## Browser showcase

Stage the RTL-only browser app with either repository script:

```powershell
.\build-browser-voices.ps1 -Compiler 'C:\path\to\pas2js.exe'
```

```sh
PAS2JS=/path/to/pas2js bash ./build-browser-voices.sh
```

The output is `build/browser/voices/www`. Serve it with the included FPC
server rather than opening `index.html` directly:

```powershell
.\build\native\bin\wfc_serve.exe --root build/browser/voices/www --port 4179
```

```sh
build/native/bin/wfc_serve --root build/browser/voices/www --port 4179
```

The page never autoplays. It shows the latest generated cell, provider tokens,
all three human-readable role actions, the exact pitch-class witnesses, novel
vertical count, held-seam count, progress, and bounded native fallback commands.
A terminal generation, encoding, write, or close failure cannot retain or
claim a completed artifact.

## Source layout

- `voice_studio_corpus.pas` owns the two project-authored excerpts and role
  metadata.
- `voice_studio_stream.pas` builds the graph configuration, parses duration,
  and exposes bounded frame and PCM pull sources.
- `voice_studio_midi_stream.pas` owns the detached MIDI count plan and checked
  deterministic replay.
- `voice_studio_demo.pas` contains shared native/browser deterministic checks.
- `VoiceStudioRender.lpr` is the atomic no-replace WAVE/MIDI native host.
- `browser_voice_studio_app.pas` and `wfc_browser_stream_file.pas` provide the
  browser transaction owner and reusable one-block file helper.
- `BrowserVoiceStudio.lpr`, `index.html`, and `voicestudio.css` form the
  browser entry point and responsive presentation.

All music, training excerpts, synthesis, and runtime logic are project-owned;
there are no external samples or third-party runtime libraries.
