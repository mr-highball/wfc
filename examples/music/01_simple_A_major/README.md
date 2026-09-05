# A-major note study

The original fluent A-major adjacency rules, now using only repository Pascal
and the standard FPC runtime. `TSimpleMusic` in `main.pas` owns generation;
descendants can override its protected `InitWFC` and `NotePitch` hooks. The
authored octave still begins at A = 220 Hz (MIDI 57), reaching A+ at MIDI 69.
Output follows the original cell-index order and legacy neighbor-rule
orientation; the written east-neighbor cycle is heard in reverse, not silently
reoriented into a new grammar.
The old GUI and external playback backend are no longer required or included.

From the repository root, `build.ps1` or `build.sh` builds and tests the native
host. Its executable is `build/native/bin/simple_a_major` (`.exe` on Windows).
The `.lpi` is an optional Lazarus console project with no GUI package dependency.

```text
simple_a_major --notes 37 --seed 55
simple_a_major --notes 37 --seed 55 --midi new-scale.mid
simple_a_major --notes 37 --tempo-us 400000 --seed 55 --wave new-scale.wav
```

`--notes` is any positive count representable by the score and available
memory, not a fixed demo length. Each note is one quarter note; `--tempo-us`
sets microseconds per quarter (default 500000, or 120 BPM). Exact requested
duration is `notes * tempo-us` microseconds. The score uses 1/4 measures so
every positive count ends on a measure boundary; it does not pad to four notes.
PCM ends at the last complete sample frame. The native process tests check
fractional-second timing as well as one-note requests.

With no output option the program only generates and reports the score.
`--midi` or `--wave` explicitly selects a new file. Existing files are never
replaced, and failed publication removes its temporary sibling. WAVE synthesis
uses bounded PCM blocks and RIFF/RF64 output with no preview-duration cap;
the graph and note score still occupy memory. MIDI's own tempo/event/size
representation limits remain checked rather than silently truncated.
The shared synthesis contract currently accepts tempos of 1..4000000
microseconds per quarter; slower score/MIDI tempos are not a WAVE-rendering
capability. This is separate from total composition length.

For user-defined wall-clock arrangements, multiple interacting layers, and
browser playback through the included FPC server, see
[Music Studio](../05_MusicStudio/README.md),
[Ensemble Studio](../06_EnsembleStudio/README.md), and
[Voice Studio](../07_VoiceStudio/README.md).

The [original demonstration video](https://youtu.be/DS09g-GRhv0) records the
historical UI; `a_major.png` preserves the accompanying reference image.
