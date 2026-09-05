# Manually authored riff study

This preserves the original experiment: inspect sheet music and write the
allowed neighboring notes as fluent WFC constraints. The three rule sets come
from Mary Had a Little Lamb, London Bridge Is Falling Down, and Hot Cross Buns.
Selecting several sets unions their note adjacencies; this is not automatic
learning, whole-song reconstruction, or a rhythm model.

`TSimpleRiff` extends the portable [A-major study](../01_simple_A_major/README.md)
and overrides `InitWFC`. Its protected `DoInitializeWFCForSong` hook and public
`Songs` set expose the authored grammar without a GUI or media-library type.
All seven nonempty song selections are covered by the shared native/pas2js
conformance suite. The original sheet-music images remain beside the source.

Run `build.ps1` or `build.sh` from the repository root. The native executable is
`build/native/bin/simple_song_riffs` (`.exe` on Windows); the `.lpi` is an
optional Lazarus console project with no GUI package dependency.

```text
simple_song_riffs --songs mary --notes 37 --seed 55
simple_song_riffs --songs mary,bridge --notes 37 --midi new-riff.mid
simple_song_riffs --songs hot-cross --notes 37 --tempo-us 400000 --wave new-riff.wav
```

All three grammars are selected by default. `--notes`, `--tempo-us`, `--seed`,
and explicit-only, no-overwrite MIDI/WAVE output have the same contract as the
A-major study. Note count is user-defined, each note is one quarter note, and
1/4 measures avoid padding short requests to four notes. The native WAVE sink
streams bounded PCM blocks without an arbitrary minute cap; graph/score memory
and file-format/numeric representation still impose practical limits.

For corpus training, rhythm, harmony, layered repair, and a browser workbench
served by the included FPC tools, continue with
[Music Studio](../05_MusicStudio/README.md). The
[original demonstration video](https://youtu.be/QLTQsu6QMwc) shows the retired UI.
