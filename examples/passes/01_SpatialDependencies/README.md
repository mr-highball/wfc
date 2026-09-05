# spatial pass dependencies

This dependency-free example is the focused Pipeline v2 proof. A shared
Pascal unit builds one terrain provider and two overlay consumers:

```text
terrain -> settlement
        \-> foliage
```

`home` must satisfy two clauses: its terrain at offset `(0,0,0)` is `land` or
`forest`, AND at least one terrain term at offset `(-1,0,0)` or `(1,0,0)` is
`water` or `marsh`. `reeds` use the same finite neighborhood but require
`land` at the current coordinate. `tree` demonstrates a single exact-offset
requirement by requiring `forest` at `(0,0,0)`.

The five terrain cells are fixed to `..F.~`. The bounded solve treats a sample
outside the graph as a non-match. The wrapped solve resolves the west neighbor
of cell zero to cell four, so its anchored home and reeds can see the water.
An additional bounded probe requests those same edge consumers and must return
a structured contradiction naming the terrain dependency.

After solving, the program independently checks all 30 cells across the two
scenarios and every spatial relation, re-runs both scenarios with the same
seed, and requires byte-identical glyph layers and signatures. No solver
internal is trusted as the validator.

## build and run

From the repository root on PowerShell:

```powershell
New-Item -ItemType Directory -Force `
  build\examples\spatial\native\units, build\examples\spatial\native\bin |
  Out-Null
fpc -B -Mdelphi -Sa -Cr -Co -Ci -Fusrc `
  -FUbuild\examples\spatial\native\units `
  -FEbuild\examples\spatial\native\bin `
  examples\passes\01_SpatialDependencies\SpatialDependencies.lpr
.\build\examples\spatial\native\bin\SpatialDependencies.exe 0
```

On a POSIX shell:

```bash
mkdir -p build/examples/spatial/native/units build/examples/spatial/native/bin
fpc -B -Mdelphi -Sa -Cr -Co -Ci -Fusrc \
  -FUbuild/examples/spatial/native/units \
  -FEbuild/examples/spatial/native/bin \
  examples/passes/01_SpatialDependencies/SpatialDependencies.lpr
./build/examples/spatial/native/bin/SpatialDependencies 0
```

The optional argument is an unsigned 32-bit seed. Omit it for seed zero.
Malformed arguments or any failed self-check produce a nonzero process exit.

## seed-zero golden output

Native FPC produce the same seed-zero fixture:

```text
SpatialDependencies: terrain -> settlement + foliage
Seed: 0
Pipeline version: 2
bounded terrain:    ..F.~
bounded settlement: ---H-
bounded foliage:    --T--
bounded signature:  2:B:0570956D:A67D59C8:5E7D8999
wrapped terrain:    ..F.~
wrapped settlement: H--H-
wrapped foliage:    R-T--
wrapped signature:  2:W:3C7EBCBC:80AEDF8E:56C50141
Legend: .=land F=forest ~=water H=home R=reeds T=tree -=none
Bounded out-of-bounds probe: rejected
Independent checks: 30 cells plus spatial clauses
Self-check: passed
```

The signatures include Pipeline v2, the boundary mode, and each solved layer
in stable coordinate order.

## scope

This example proves staged hard reads from already-solved acyclic provider
passes. It does not claim a radius search, distance metric, count threshold,
soft preference, arbitrary predicate callback, cross-shape resampling, cyclic
feedback, negotiation, or repair. Larger neighborhoods are explicit finite
term arrays, so their cost and replay identity remain visible in the model.
