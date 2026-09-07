# music examples

The portable music path is project-owned Pascal and has no external runtime
dependency. It learns fixed-quantum rhythm, harmony, and melody sequences,
solves them as named passes, rebuilds an exact score, round-trips canonical
score and public-composition text, performs bounded full/selective negotiated
variation with exact public motif locks, and exports Standard MIDI File and
project-owned PCM/WAVE bytes. Music Studio adds browser playback, editing,
and [user-defined long compositions](../../docs/music-arrangement.md) streamed
to WAVE/RF64. The [MIDI import tool](../../docs/music-import.md) builds exact
scores and explicitly selected training excerpts.

| Example | Status | Targets | Dependencies |
| --- | --- | --- | --- |
| [`07_VoiceStudio`](07_VoiceStudio/README.md) | Independently learned roles, exact harmony coverage, and user-defined WAVE/MIDI streaming | Native FPC, pas2js/browser | Repository units, standard RTL and host APIs only |
| [`06_EnsembleStudio`](06_EnsembleStudio/README.md) | Synchronized polyphony, locks, repair, bounded preview, and user-defined WAVE/MIDI streaming | Native FPC, pas2js/browser | Repository units, standard RTL and host APIs only |
| [`05_MusicStudio`](05_MusicStudio/README.md) | Interactive locks, repair, piano roll, playback and export | Native FPC, pas2js/browser | Repository units, standard RTL and host APIs only |
| [`04_NegotiatedVariation`](04_NegotiatedVariation/README.md) | Current reusable repair and replay proof | Native FPC | Repository units and standard RTL only |
| [`03_PassComposition`](03_PassComposition/README.md) | Current tested vertical slice | Native FPC | Repository units and standard RTL only |
| [`01_simple_A_major`](01_simple_A_major/README.md) | Original A-major adjacency study with explicit export | Native FPC; shared FPC/pas2js unit | Repository units and standard RTL only |
| [`02_simple_song_riffs`](02_simple_song_riffs/README.md) | Original manually authored song-adjacency study with explicit export | Native FPC; shared FPC/pas2js units | Repository units and standard RTL only |

The original studies preserve their explicit pitch-adjacency rules and A-major
fixture in portable Pascal. They now build exact scores and use the same
project-owned MIDI and streaming WAVE implementation as the maintained music
path; no GUI or playback package is involved. Historical notation images and
neighbor notes remain explanatory material. See
[the music foundation](../../docs/music.md) for the exact score, cell,
artifact, MIDI, and audio contracts.

## portable examples

From the repository root, create output directories and compile:

```powershell
New-Item -ItemType Directory -Force `
  build\examples\music\native\units, build\examples\music\native\bin |
  Out-Null
fpc -B -Mdelphi -Sa -Cr -Co -Ci -Fusrc `
  -FUbuild\examples\music\native\units `
  -FEbuild\examples\music\native\bin `
  examples\music\03_PassComposition\PassComposition.lpr
.\build\examples\music\native\bin\PassComposition.exe 0
```

```bash
mkdir -p build/examples/music/native/units build/examples/music/native/bin
fpc -B -Mdelphi -Sa -Cr -Co -Ci -Fusrc \
  -FUbuild/examples/music/native/units \
  -FEbuild/examples/music/native/bin \
  examples/music/03_PassComposition/PassComposition.lpr
./build/examples/music/native/bin/PassComposition 0
```

The optional argument is an unsigned 32-bit seed. The program validates both
cross-pass projections, the strict `wfcmusic=1` score round-trip, and the owned
SMF round-trip before it reports success. It emits no audio and does not write
a `.mid` file; the MIDI artifact is held as a byte array.

The negotiated-variation fixture uses a thin native host over one
self-checking Pascal unit:

```bash
mkdir -p build/examples/music-variation/native/units \
  build/examples/music-variation/native/bin
fpc -B -Mdelphi -Sa -Cr -Co -Ci -Fusrc \
  -Fuexamples/music/04_NegotiatedVariation \
  -FUbuild/examples/music-variation/native/units \
  -FEbuild/examples/music-variation/native/bin \
  examples/music/04_NegotiatedVariation/NegotiatedVariation.lpr
./build/examples/music-variation/native/bin/NegotiatedVariation 0
```

It preserves an exact public melody motif, demonstrates atomic failure before
bounded repair, and validates and replays the final public result without a
playback backend. The focused
[`04_NegotiatedVariation` guide](04_NegotiatedVariation/README.md) records its
budgets, repair scope, signatures, and limitations.

## original note-rule studies

Both native hosts accept `--seed N`, `--notes N`, and `--tempo-us N`. The riff
host additionally accepts a comma-separated
`--songs mary,bridge,hot-cross` selection. Pass `--wave NEW.wav` or
`--midi NEW.mid` to request one new artifact; without either option the hosts
do not write a file. WAVE output is streamed through the owned renderer, so the
caller-selected note count is not constrained by a preview-duration cap.

The focused [A-major](01_simple_A_major/README.md) and
[riff](02_simple_song_riffs/README.md) guides give build commands, validation,
and overwrite behavior.
