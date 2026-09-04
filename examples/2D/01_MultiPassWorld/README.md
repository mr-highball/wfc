# MultiPassWorld

`MultiPassWorld.lpr` is the first complete domain demo built on the atomic
reference solver. One Pascal program creates and displays a 32 by 14 world in
three dependent layers:

```text
terrain -> biome -> foliage
```

The demo places a small set of legal caller anchors so water, land, mountain,
ocean, shore, woodland, alpine, reeds, trees, and pine are all visible. Every
other cell is generated. It then runs the independent `wfc_world2d_validate`
checker before rendering the three layers side by side.

## output

The header prints the seed, world model version, portable layer signature,
validation totals, and decisions/propagations/backtracks for each pass. The
glyph legend is:

| Layer | Glyphs |
| --- | --- |
| terrain | `~` water, `.` land, `^` mountain |
| biome | `O` ocean, `s` shore, `p` plains, `w` woodland, `a` alpine |
| foliage | `-` none, `r` reeds, `g` grass, `T` tree, `P` pine |

With no argument, the demo uses seed `$4D505731` (`1297110833`). Model and
signature version 1 produce:

```text
1:81F03F86:9069C5F9:41619106
```

The automated smoke seed `0` produces:

```text
1:5B0DD75D:08022AF1:A40D0955
```

These signatures are identical under native FPC and pas2js/Node. The program
checks both known seeds against these constants and exits with an error if the
showcase model drifts. They cover the final layer values, not console formatting
or solver decision traces.

## build and run

From the repository root, create output directories and compile natively:

```text
fpc -B -Mdelphi -Sa -Cr -Co -Ci -Fusrc -FUbuild/examples/2d/native/units -FEbuild/examples/2d/native/bin examples/2D/01_MultiPassWorld/MultiPassWorld.lpr
build/examples/2d/native/bin/MultiPassWorld 0
```

With a configured pas2js compiler and matching RTL:

```text
pas2js -B -Tnodejs -Mdelphi -Fusrc -FUbuild/examples/2d/pas2js/units -FEbuild/examples/2d/pas2js/bin examples/2D/01_MultiPassWorld/MultiPassWorld.lpr
node build/examples/2d/pas2js/bin/MultiPassWorld.js 0
```

Both root build scripts compile the demo and run noninteractive smoke tests for
seed `0` and the default seed. The hosted pas2js lane does the same with its
pinned compiler and Node.js runtime. This continuously verifies both documented
signatures as well as shared-source transpilation and execution; it is not yet
an interactive browser UI.

## invariants demonstrated

- terrain never places water directly beside mountain;
- ocean refines water, land biomes refine land, and alpine refines mountain;
- reeds, grass, trees, and pine appear only in their permitted biome;
- every pass solves before any generated value is committed;
- the validator checks the output without reading solver rule tables; and
- a seed and versioned model produce stable native/pas2js layer signatures.

See the [2D ecosystem documentation](../../../docs/world2d.md) for the wrapper
API, lock and rollback behavior, validation report, and exact checksum format.
