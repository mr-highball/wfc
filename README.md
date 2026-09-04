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

Run the checked native build and conformance suite from the repository root:

```powershell
.\build.ps1
```

```bash
./build.sh
```

Both entry points compile with checked FPC options, keep all output under
`build/`, run the tests, and preserve failure exit codes. The repository also
includes an FPM package, a runtime-only Lazarus package, and the same
conformance source for pas2js/Node. See [building and testing](docs/building.md)
for compiler overrides, package commands, output paths, and pas2js setup.

The core, conformance suite, tiled-world example, and building-kit console need
no submodule. The two legacy music experiments require Lazarus/LCL, SDL2, and
the optional GPL-3.0 SoundShop
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
