# music examples

The portable music path is project-owned Pascal and has no external runtime
dependency. It learns fixed-quantum rhythm, harmony, and melody sequences,
solves them as named passes, rebuilds an exact score, round-trips canonical
score and public-composition text, performs bounded full/selective negotiated
variation with exact public motif locks, and exports Standard MIDI File bytes.

| Example | Status | Targets | Dependencies |
| --- | --- | --- | --- |
| [`04_NegotiatedVariation`](04_NegotiatedVariation/README.md) | Current reusable repair and replay proof | Native FPC, pas2js/Node | Repository units and standard RTL only |
| [`03_PassComposition`](03_PassComposition/README.md) | Current tested vertical slice | Native FPC, pas2js/Node | Repository units and standard RTL only |
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

For pas2js/Node:

```bash
mkdir -p build/examples/music/pas2js/units build/examples/music/pas2js/bin
pas2js -B -Tnodejs -Mdelphi -Fusrc \
  -FUbuild/examples/music/pas2js/units \
  -FEbuild/examples/music/pas2js/bin \
  examples/music/03_PassComposition/PassComposition.lpr
node build/examples/music/pas2js/bin/PassComposition.js 0
```

The optional argument is an unsigned 32-bit seed. The program validates both
cross-pass projections, the strict `wfcmusic=1` score round-trip, and the owned
SMF round-trip before it reports success. It emits no audio and does not write
a `.mid` file; the MIDI artifact is held as a byte array.

The negotiated-variation fixture uses thin native and Node hosts over one
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

```bash
mkdir -p build/examples/music-variation/pas2js/units \
  build/examples/music-variation/pas2js/bin
pas2js -B -Tnodejs -Mdelphi -Fusrc \
  -Fuexamples/music/04_NegotiatedVariation \
  -FUbuild/examples/music-variation/pas2js/units \
  -FEbuild/examples/music-variation/pas2js/bin \
  examples/music/04_NegotiatedVariation/NegotiatedVariationNode.lpr
node build/examples/music-variation/pas2js/bin/NegotiatedVariationNode.js 0
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
