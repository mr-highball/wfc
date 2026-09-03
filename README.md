# WFC

WFC is a constraint-driven generation library written in Pascal for Free
Pascal Compiler (FPC) and pas2js. It models user-defined values on a 2D or 3D
graph, applies directional constraints, and supports ordered passes whose rules
and results remain separate.

The pass system is the larger idea: generate terrain first, then foliage,
roads, housing, or any other layer while constraining each stage from the
result before it. The same approach can be specialized for world generation,
modular 3D structures, music, text, and other discrete design problems.

> **Project status:** the original API and its greedy traversal solver remain
> available, and the first operational multi-pass contract now works on native
> FPC and pas2js with portable seeded replay. A reference propagating WFC
> solver, richer cross-layer constraints, polished domain libraries, and
> complete native/browser demos are planned and tracked in the
> [roadmap](ROADMAP.md).

## Features

- 2D and 3D graph topology with optional wrapped boundaries
- fluent rules over caller-defined string values
- required directional rules and selection/invalid-state callbacks
- stable, labeled, zero-based passes with isolated values, rules, and outputs
- sequential pipeline execution with selected-pass restoration
- empty-pass copying and same-coordinate constraints on the previous pass
- explicit pipeline seeds with stable, independent per-pass random streams
- matching seeded golden fixtures on native FPC and pas2js/Node
- iterative traversal without a graph-sized call stack
- extension hooks for custom graph and entry behavior
- one Pascal core for native FPC and pas2js

## Basic use

```pascal
uses
  wfc;

var
  Graph: TGraph;
begin
  Graph := TGraph.Create.Reshape(5, 5, 1);
  try
    Graph.AddValue('A')
      .NewRule(AllDirections, ['A', 'B']);

    Graph.AddValue('C')
      .NewRule([gdNorth, gdSouth], ['A', 'B']);

    Graph.Run;
  finally
    Graph.Free;
  end;
end;
```

For a complete terrain-to-foliage pipeline, including `SwitchToPass`,
`PassGraph`, and `RequirePrevious`, see [pass-system semantics](docs/passes.md).
For exact replay behavior, callback requirements, and algorithm versioning,
see [deterministic generation](docs/determinism.md).

## Build and test

Add `src` to the unit search path, then compile the conformance runner:

```text
fpc -B -Mdelphi -Sa -Cr -Co -Ci -Fusrc -FUbuild/native/units -FEbuild/native/bin test/wfc_test.lpr
```

Run the produced `build/native/bin/wfc_test` executable (`wfc_test.exe` on
Windows). It returns a nonzero exit code when a check fails. The same test
source also compiles for a Node.js pas2js target and checks the same seeded
output vectors. A configured pas2js RTL toolchain can use:

```text
pas2js -B -Tnodejs -Mdelphi -Fusrc -FUbuild/pas2js/units -FEbuild/pas2js test/wfc_test.lpr
node build/pas2js/wfc_test.js
```

Create the output directories first. A compiler executable without its
matching pas2js RTL unit paths is not sufficient.

Lazarus can open `test/wfc_test.lpi`. The core, conformance suite, tiled-world
example, and building-kit console need no submodule. The two legacy music
experiments require Lazarus/LCL, SDL2, and the optional GPL-3.0 SoundShop
submodule; they are not part of the dependency-free MIT build path. See the
[examples index](examples/README.md) for exact status and commands.

## Direction

The [roadmap](ROADMAP.md) covers the reference solver, transactional pass
pipeline, model learning and validation tools, 2D/3D/music/text ecosystems,
pas2js playgrounds, reproducible research, documentation, and release
provenance. Current examples are indexed under [examples](examples/README.md).

## License

WFC is released under the [MIT License](LICENSE).

**Tip jar**

- BTC: `bc1q55qh7xptfgkp087sfr5ppfkqe2jpaa59s8u2lz`
- LTC: `LPbvTsFDZ6EdaLRhsvwbxcSfeUv1eZWGP6`
