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
> FPC and pas2js. A reference propagating WFC solver, richer cross-layer
> constraints, polished domain libraries, and complete native/browser demos
> are planned and tracked in the [roadmap](ROADMAP.md).

## Features

- 2D and 3D graph topology with optional wrapped boundaries
- fluent rules over caller-defined string values
- required directional rules and selection/invalid-state callbacks
- stable, labeled, zero-based passes with isolated values, rules, and outputs
- sequential pipeline execution with selected-pass restoration
- empty-pass copying and same-coordinate constraints on the previous pass
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

## Build and test

Add `src` to the unit search path, then compile the conformance runner:

```text
fpc -B -Mdelphi -Fusrc test/wfc_test.lpr
```

Run the produced `test/wfc_test` executable (`wfc_test.exe` on Windows). It
returns a nonzero exit code when a check fails. The same test source also
compiles for a Node.js pas2js target.

Lazarus can open `test/wfc_test.lpr` directly. Some older demos have optional
submodule dependencies; clone with `--recursive` only when those demos are
needed.

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
