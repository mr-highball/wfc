# LearnPatterns

This dependency-free example learns overlapping `2x2` patterns from two
differently sized token grids, augments them with explicit D4 symmetry, and
round-trips the result through canonical `wfcp=1` text. The graph solves
private latent pattern keys; the example then captures that assignment,
independently checks every overlap, projects it back to the public token
palette, and independently checks every projected contribution.

The training boundary and output boundary are separate choices. Both are
wrapped here, but loading a wrapped model never changes a graph's topology.
The caller explicitly enables `WrapNeighbors` before solving.

From the repository root:

```powershell
New-Item -ItemType Directory -Force `
  build/examples/pattern/native/units, `
  build/examples/pattern/native/bin | Out-Null
fpc -B -Mdelphi -Fusrc `
  -FUbuild/examples/pattern/native/units `
  -FEbuild/examples/pattern/native/bin `
  examples/learning/03_LearnPatterns/LearnPatterns.lpr
.\build\examples\pattern\native\bin\LearnPatterns.exe 0
```

The same Pascal source compiles for Node.js with pas2js:

```bash
mkdir -p build/pas2js/pattern-example-units build/pas2js/pattern-example
pas2js -B -Tnodejs -Mdelphi -Fusrc \
  -FUbuild/pas2js/pattern-example-units \
  -FEbuild/pas2js/pattern-example \
  examples/learning/03_LearnPatterns/LearnPatterns.lpr
node build/pas2js/pattern-example/LearnPatterns.js 0
```

An optional unsigned 32-bit seed selects a replayable solve. Native FPC and
pas2js/Node print the same metadata, counters, projection signature, and token
grid for the same seed. No external library, asset pack, or service is used.

Seed `0` currently learns 17 unique patterns and prints projection signature
`1:6D67D260`. The conformance suite verifies the algorithm and codec more
directly; this signature is the portable end-to-end demo fixture.

The rare rock token is forced through one caller-owned latent anchor so the
rendered fixture cannot degenerate into an all-water solution. That anchor is
an input to replay and does not mutate the learned model.

The demonstrated invariants are:

- palette and pattern identities are deterministic;
- `wfcp=1` decodes and re-encodes byte-for-byte;
- every latent east/south overlap agrees, including wrapped seams;
- every repeated projected footprint contribution agrees;
- the public output contains palette tokens rather than private `@p...` keys;
  and
- source-boundary metadata never silently changes output topology.

If application reports `empty-support-not-representable`, an open training
corpus produced a pattern with no support in at least one direction. This demo
uses wrapped sources so every extracted pattern retains observed directional
support. If solving exhausts the backtrack limit after changing the samples,
increase `MaxBacktracks`, use a compatible output shape, or inspect the
learned relation set; do not replace an empty support row with a wildcard.
