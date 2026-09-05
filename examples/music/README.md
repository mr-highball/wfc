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
| [`05_MusicStudio`](05_MusicStudio/README.md) | Interactive locks, repair, piano roll, playback and export | Native FPC, pas2js/browser | Repository units, standard RTL and host APIs only |
| [`04_NegotiatedVariation`](04_NegotiatedVariation/README.md) | Current reusable repair and replay proof | Native FPC | Repository units and standard RTL only |
| [`03_PassComposition`](03_PassComposition/README.md) | Current tested vertical slice | Native FPC | Repository units and standard RTL only |
| `01_simple_A_major` | Legacy playback experiment | Lazarus/LCL | Optional GPL-3.0 SoundShop submodule and SDL2 |
| `02_simple_song_riffs` | Legacy learned-adjacency experiment | Lazarus/LCL | Optional GPL-3.0 SoundShop submodule and SDL2 |

The legacy experiments preserve useful early ideas—explicit pitch adjacency,
duration metadata, pass-oriented composition questions, and an A-major
fixture—but their UI and playback stack is not part of the MIT runtime
foundation. Initializing the
SoundShop submodule is neither necessary nor recommended for normal builds.
See [the music foundation](../../docs/music.md) for the exact score, cell,
artifact, MIDI, and license contracts.

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

## optional legacy playback

Only initialize this dependency when deliberately examining the old examples:

```text
git submodule update --init --recursive examples/music/SoundShop
lazbuild -B --no-write-project examples/music/01_simple_A_major/simple_a_major.lpi
lazbuild -B --no-write-project examples/music/02_simple_song_riffs/simple_song_riffs.lpi
```

They require Lazarus/LCL and an SDL2 shared library at runtime. SoundShop is
GPL-3.0, so this integration stays isolated from the dependency-free MIT build
and cannot define the standard ecosystem API.
