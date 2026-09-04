# pass-composed music

`PassComposition.lpr` is the first dependency-free music vertical slice. The
same Pascal source runs with native FPC and pas2js/Node.

It learns three order-2 sequence models from small fixed-quantum corpora and
solves an open eight-cell graph in this order:

1. `harmony` solves a latent harmony sequence constrained to the planned
   F-A-C pitch classes;
2. `rhythm` solves a latent attack/hold/rest sequence constrained to the
   planned rhythm;
3. `melody` solves a latent melody sequence while two named projection maps
   require the rhythm action and, for sounding cells, the harmony pitch class.

The rhythm and harmony requirements are ANDed at each cell. Each mapping may
still accept multiple private source states, which preserves the histories
needed by its source sequence model. The program independently captures and
validates all three solved paths, then checks the public relation again without
trusting graph internals.

## score and artifact fixture

Each cell is 480 ticks. The generated melody is rebuilt as one track and one
voice in a two-measure score with:

- 480 ticks per quarter note;
- 12 steps per octave;
- 3840 ticks total;
- 4/4 meter from tick zero;
- 500,000 microseconds per quarter note (120 BPM) from tick zero.

The score constructor verifies its complete partition and meter invariants.
The program then:

- encodes strict `wfcmusic=1`, decodes it, and requires byte-identical
  re-encoding;
- exports project-owned SMF format 0, decodes it with the project-owned raw
  codec, and requires byte-identical canonical re-encoding;
- prints the spans, format versions, MIDI size, and FNV-1a checksum.

The artifact bytes remain in memory. This is a generation and interchange
fixture, not a player, browser UI, or file-writing tool.

## build and run

From the repository root on PowerShell:

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

On a POSIX shell:

```bash
mkdir -p build/examples/music/native/units build/examples/music/native/bin
fpc -B -Mdelphi -Sa -Cr -Co -Ci -Fusrc \
  -FUbuild/examples/music/native/units \
  -FEbuild/examples/music/native/bin \
  examples/music/03_PassComposition/PassComposition.lpr
./build/examples/music/native/bin/PassComposition 0
```

With a configured pas2js compiler and matching RTL:

```bash
mkdir -p build/examples/music/pas2js/units build/examples/music/pas2js/bin
pas2js -B -Tnodejs -Mdelphi -Fusrc \
  -FUbuild/examples/music/pas2js/units \
  -FEbuild/examples/music/pas2js/bin \
  examples/music/03_PassComposition/PassComposition.lpr
node build/examples/music/pas2js/bin/PassComposition.js 0
```

Omit the argument for seed zero, or pass any unsigned 32-bit decimal seed.
Malformed arguments and every validation failure return a nonzero exit code.

The repository-level `build.ps1` and `build.sh` also compile and smoke-test
this example with seed zero after running the four music conformance suites.

## what this proves—and does not

This fixture proves a transactionally atomic, deterministic harmony + rhythm
-> melody -> score -> MIDI route without LCL, SDL2, SoundShop, or another
third-party unit. Atomic means all-or-nothing commit/rollback. The staged v1
solver does not reopen harmony or rhythm decisions when melody fails. The
fixture proves hard pitch-class and action constraints over one quantized
voice.

It does not yet prove polyphonic cell projection, key spelling, functional
harmony, voice leading, MIDI-to-score learning, interactive editing, browser
presentation, or native/WebAudio playback. Those remain explicit research and
ecosystem work in the [music foundation](../../../docs/music.md) and
[roadmap](../../../ROADMAP.md).
