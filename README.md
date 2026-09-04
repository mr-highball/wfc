# WFC

WFC is a constraint-driven generation library written in Pascal for Free
Pascal Compiler (FPC) and pas2js. It models user-defined values on a 2D or 3D
graph, applies directional constraints, and supports ordered passes whose rules
and results remain separate.

The pass system is the larger idea: generate terrain first, then foliage,
roads, housing, or any other layer while constraining each stage from the
result before it. The same approach can be specialized for world generation,
modular 3D structures, music, text, and other discrete design problems.

> **Project status:** the original API and greedy traversal solver remain
> available. The opt-in reference solver now provides fixed-point propagation,
> deterministic minimum-domain observation, bounded backtracking, structured
> contradiction reports, and an atomic sequential pass pipeline on native FPC
> and pas2js. The first specialized ecosystem now adds a reusable, independently
> validated terrain → biome → foliage world with portable layer signatures.
> Weights, richer cross-layer constraints, additional domain libraries, and
> complete browser demos remain tracked in the
> [roadmap](ROADMAP.md).

## Features

- 2D and 3D graph topology with optional wrapped boundaries
- fluent rules over caller-defined string values
- required directional rules and selection/invalid-state callbacks
- stable, labeled, zero-based passes with isolated values, rules, and outputs
- sequential pipeline execution with selected-pass restoration
- empty-pass copying and same-coordinate constraints on the previous pass
- explicit pipeline seeds with stable, independent per-pass random streams
- an opt-in propagating solver with deterministic MRV and bounded backtracking
- atomic all-pass staging, independent validation, and structured run reports
- a typed 2D terrain/biome/foliage library with an independent semantic checker
- versioned, fixed-token 2D layer signatures shared by native FPC and pas2js
- matching seeded golden fixtures on native FPC and pas2js/Node
- iterative traversal without a graph-sized call stack
- extension hooks for custom graph and entry behavior
- one Pascal core for native FPC and pas2js

## Basic use

```pascal
uses
  SysUtils,
  wfc;

var
  Graph: TGraph;
  Options: TGraphSolveOptions;
  Report: TGraphSolveReport;
begin
  Graph := TGraph.Create.Reshape(5, 5, 1);
  try
    Graph.AddValue('A')
      .NewRule(AllDirections, ['A', 'B']);

    Graph.AddValue('C')
      .NewRule([gdNorth, gdSouth], ['A', 'B']);

    Options := DefaultGraphSolveOptions;
    if not Graph.TrySolve(Options, Report) then
      raise Exception.CreateFmt('WFC failed in pass %d',
        [Report.FailedPassIndex]);
  finally
    Graph.Free;
  end;
end;
```

For a complete terrain-to-foliage pipeline, including `SwitchToPass`,
`PassGraph`, and `RequirePrevious`, see [pass-system semantics](docs/passes.md).
For the reference algorithm, atomicity contract, reports, and exact constraint
semantics, see the [reference solver](docs/solver.md).
For exact replay behavior, callback requirements, and algorithm versioning,
see [deterministic generation](docs/determinism.md).
For the reusable world model, typed locks, validator, signatures, and complete
multi-pass demo, see the [2D ecosystem](docs/world2d.md).

## Build and test

Run the checked native build and conformance suite from the repository root:

```powershell
.\build.ps1
```

```bash
./build.sh
```

Both entry points compile with checked FPC options, keep all output under
`build/`, run the core and 2D ecosystem suites, smoke-test both portable
examples, and preserve failure exit codes. The repository also includes an FPM
package, a runtime-only Lazarus package, and the same conformance sources for
pas2js/Node. See [building and testing](docs/building.md) for compiler
overrides, package commands, output paths, and pas2js setup.

The core, specialized 2D units, conformance suites, portable 2D examples, and
building-kit console need no submodule. The two legacy music experiments
require Lazarus/LCL, SDL2, and the optional GPL-3.0 SoundShop
submodule; they are not part of the dependency-free MIT build path. See the
[examples index](examples/README.md) for exact status and commands.

## Direction

The [roadmap](ROADMAP.md) covers the remaining reference-solver work, richer
pass composition, model learning and validation tools, 2D/3D/music/text
ecosystems, pas2js playgrounds, reproducible research, documentation, and
release provenance. Current examples are indexed under
[examples](examples/README.md).

## License

WFC is released under the [MIT License](LICENSE).

**Tip jar**

- BTC: `bc1q55qh7xptfgkp087sfr5ppfkqe2jpaa59s8u2lz`
- LTC: `LPbvTsFDZ6EdaLRhsvwbxcSfeUv1eZWGP6`
