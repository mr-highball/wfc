# LearnedPatternWorld

`LearnedPatternWorld` is a self-checking Pattern-Projected Pass Composition v1
fixture. It learns an overlapping 2D model at runtime, solves the learned
patterns as a private latent layer, exposes only the projected terrain tokens,
and lets two ordinary domain passes consume that public contract:

```text
patterns -> terrain -> foliage
                    -> structure
```

The stable pass labels and indices are `patterns=0`, `terrain=1`, `foliage=2`,
and `structure=3`. Every pass is an overlay on the same wrapped 8 by 6,
depth-one graph. Both thin entry points call the same Pascal demo unit. Native
FPC and pas2js/Node use only project units and their applicable standard RTL.

## runtime learning and canonical artifact

The program learns from two embedded terrain samples:

```text
~~~~~     ~~~~~~~
~...~     ~.....~
~.#.~     ~.###.~
~...~     ~.....~
~~~~~     ~~~~~~~
```

It extracts wrapped 2 by 2 patterns with D4 symmetry. The resulting model has
17 patterns. Before solving, the demo encodes it to the project-owned `wfcp=1`
text format, decodes that text into a fresh model, and requires the second
encoding to be byte-identical. Applications can persist that same canonical
text; this fixture keeps it in memory so every run proves the full learning
path.

## portable recipe, run, and result bundle

The [`pipeline`](pipeline) directory packages the same domain as three strict
portable artifacts:

- [`recipe.wfcpipeline`](pipeline/recipe.wfcpipeline) owns the canonical
  learned `wfcp=1` resource plus the 104-byte foliage and 90-byte structure
  `wfcrules=1` resources;
- [`run.wfcrun`](pipeline/run.wfcrun) pins the wrapped 8 by 6 by 1 shape, seed
  0, one-way strategy, 65,536 local backtracks, no trace, and eight ordered
  public terrain locks;
- [`result.wfcresult`](pipeline/result.wfcresult) is the exact three-layer
  public result. It contains no private `@p...` pattern key.

The recipe is `patterns (private) -> terrain (public)`, followed by public
foliage and structure rule passes. A Pattern2D projection bridge v2 lowers the
terrain locks into intersected private pattern domains before solving. Seven
exact zero-offset public requirements then constrain each foliage or structure
token from terrain; neither downstream resource names a private pattern key.

The example-owned
[`learned_pattern_world_bundle.pas`](learned_pattern_world_bundle.pas) builds
the same recipe and run through public Pascal APIs. The focused native and
pas2js test constructs both programmatic and decoded forms, requires one
semantic identity, executes both, verifies the established rows and layer
hashes, and round-trips every artifact byte exactly. The native side also
compares generated text with all three checked-in files. Repository process
gates run the real native and Node hosts, require validator canonical output,
and compare runner output with the result file.

| Artifact | Bytes | Signature |
| --- | ---: | --- |
| learned Pattern2D resource | `2130` | canonical `wfcp=1` payload |
| foliage rules | `104` | `EC4261C3` |
| structure rules | `90` | `2E18E099` |
| pipeline recipe | `6186` | `DC2030BE` |
| pipeline run | `327` | `AA80D2AA` |
| pipeline result | `3000` | `5329DB78` |

After building the repository tools, validate and replay the committed bundle:

```sh
build/native/bin/wfc_validate recipe examples/2D/05_LearnedPatternWorld/pipeline/recipe.wfcpipeline
build/native/bin/wfc_run examples/2D/05_LearnedPatternWorld/pipeline/recipe.wfcpipeline examples/2D/05_LearnedPatternWorld/pipeline/run.wfcrun
```

The first command reports recipe signature `DC2030BE`; the second writes the
exact canonical result artifact to standard output. All three files are
ASCII-compatible canonical text with LF endings and a final LF.

Learned pattern keys such as `@p...` are deliberately private. The
`wfc_pattern2d_graph` adapter preflights the entire bridge before changing the
target pass: model and source-pass identity, wrapped topology, depth, empty
overlay state, dependency cycles, token conversion, palette values, and every
pattern-footprint alternative. It then installs public terrain values and the
conjunctive source offsets needed to project each 2 by 2 pattern atomically.

The generic adapter calls its conventional target `projection`, but its
lower-level API accepts a domain label. This example names that pass `terrain`
because `~`, `.`, and `#` are its public API. No downstream rule refers to a
latent pattern key.

## downstream semantics

The foliage and structure layers depend only on terrain at the same
coordinate:

| Terrain | Foliage alternatives | Structure |
| --- | --- | --- |
| `~` water | `reeds` | `dock` |
| `.` ground | weighted `grass` or `tree` | `hut` |
| `#` rock | `moss` | `mine` |

The latent pass has deterministic water and rock anchors so all three terrain
tokens occur in the seed-zero fixture. The complete first solve executes
`[0,1,2,3]`.

The graph subclass performs independent validation at the atomic commit
boundary. It captures the solved latent assignment, checks all wrapped overlap
relations, recomputes every footprint contribution and projected token,
compares every public terrain cell, checks both consumer relations, and scans
all three public passes for private pattern keys. The seed-zero totals are 48
patterns, 96 overlaps, 192 footprint contributions, 144 public cells, and 96
consumer relations; the private-key leak count is zero.

## contradiction, rollback, and recovery

After committing the baseline, the fixture asks the structure pass to place a
`dock` at the anchored rock cell. A structure-only regeneration must fail with
a dependency contradiction at pass 3, provider pass 1, row-major entry 28.
The active execution order is exactly `[3]`; patterns, terrain, and foliage are
reported as reused.

The rollback proof compares all 192 pass-cell records, including value,
emptiness, generated ownership, and selected pass, with the baseline and with
an independently constructed same-seed control graph. It also compares the
next draw from every pass's random stream. After clearing the bad constraint,
both graphs regenerate only structure, return to the exact baseline entry
state, retain identical random streams, and reproduce all public hashes. This
is exact transactional recovery, not a claim of minimal repair.

## build and run

From the repository root, compile and run the native entry point on a POSIX
shell:

```sh
mkdir -p build/examples/learned-pattern-world/native/units build/examples/learned-pattern-world/native/bin
fpc -B -Mdelphi -Sa -Cr -Co -Ci -Fusrc -Fuexamples/2D/05_LearnedPatternWorld -FUbuild/examples/learned-pattern-world/native/units -FEbuild/examples/learned-pattern-world/native/bin examples/2D/05_LearnedPatternWorld/LearnedPatternWorld.lpr
./build/examples/learned-pattern-world/native/bin/LearnedPatternWorld 0
```

The equivalent PowerShell commands are:

```powershell
New-Item -ItemType Directory -Force -Path 'build/examples/learned-pattern-world/native/units','build/examples/learned-pattern-world/native/bin' | Out-Null
fpc -B -Mdelphi -Sa -Cr -Co -Ci -Fusrc -Fuexamples/2D/05_LearnedPatternWorld -FUbuild/examples/learned-pattern-world/native/units -FEbuild/examples/learned-pattern-world/native/bin examples/2D/05_LearnedPatternWorld/LearnedPatternWorld.lpr
& '.\build\examples\learned-pattern-world\native\bin\LearnedPatternWorld.exe' 0
```

Compile the Node entry point with a configured pas2js compiler and matching
RTL on a POSIX shell:

```sh
mkdir -p build/examples/learned-pattern-world/pas2js/units build/examples/learned-pattern-world/pas2js/bin
pas2js -B -Tnodejs -Mdelphi -Fusrc -Fuexamples/2D/05_LearnedPatternWorld -FUbuild/examples/learned-pattern-world/pas2js/units -FEbuild/examples/learned-pattern-world/pas2js/bin examples/2D/05_LearnedPatternWorld/LearnedPatternWorldNode.lpr
node build/examples/learned-pattern-world/pas2js/bin/LearnedPatternWorldNode.js 0
```

The equivalent PowerShell commands are:

```powershell
New-Item -ItemType Directory -Force -Path 'build/examples/learned-pattern-world/pas2js/units','build/examples/learned-pattern-world/pas2js/bin' | Out-Null
pas2js -B -Tnodejs -Mdelphi -Fusrc -Fuexamples/2D/05_LearnedPatternWorld -FUbuild/examples/learned-pattern-world/pas2js/units -FEbuild/examples/learned-pattern-world/pas2js/bin examples/2D/05_LearnedPatternWorld/LearnedPatternWorldNode.lpr
node '.\build\examples\learned-pattern-world\pas2js\bin\LearnedPatternWorldNode.js' 0
```

Omit the argument to use seed 0, or pass any unsigned 32-bit seed. Seed-zero
identities are permanent compatibility fixtures:

| Artifact | Identity |
| --- | --- |
| learned pattern count | `17` |
| canonical `wfcp=1` bytes | `2130` |
| canonical model | `9BF802CC` |
| latent assignment | `D800EC4B` |
| public terrain | `EBBC9390` |
| foliage | `92D4BC87` |
| structure | `8FA9D854` |
| complete pipeline | `38FE98C4` |

These values and the complete console output have been measured unchanged on
native FPC 3.2.2, native FPC 3.3.1, and pas2js 3.3.1 running under Node. A
golden change is a model or compatibility decision, not an automatic constant
update.

## seed-zero output

```text
LearnedPatternWorld: patterns -> terrain -> foliage + structure
Seed: 0
Pattern graph adapter version: 1
Runtime corpus: samples=2 footprint=2x2 boundary=wrap symmetry=d4
Learned patterns: 17 canonical-bytes=2130 model-hash=9BF802CC
Execution order: [0,1,2,3]
Latent assignment hash: D800EC4B
Terrain hash: EBBC9390
Foliage hash: 92D4BC87
Structure hash: 8FA9D854
Pipeline hash: 38FE98C4
Independent validation: patterns=48 overlaps=96 contributions=192 public-cells=144 consumer-relations=96
Private pattern keys in public output: 0
Deliberate contradiction: pass=3 provider=1 entry=28 committed-state=rolled-back
Rollback replay: all-entry-state=exact all-pass-rng=exact
Recovery: requested=[3] active=[3] providers=[0,1,2] reused
Self-check: passed

terrain      foliage     structure
~~~~~~~~   rrrrrrrr   DDDDDDDD
~.~...~~   rgrgggrr   DHDHHHDD
~~~.#.~~   rrrgmgrr   DDDHMHDD
~~~.#.~~   rrrgmTrr   DDDHMHDD
~~~...~~   rrrgggrr   DDDHHHDD
.~~~~~~~   grrrrrrr   HDDDDDDD

terrain: ~=water .=ground #=rock
foliage: r=reeds g=grass T=tree m=moss
structure: D=dock H=hut M=mine
```

## current boundary

- The graph bridge currently requires a positive, wrapped, depth-one topology
  and a same-size projection. Open-boundary or resized output is not exposed by
  this composition API.
- The embedded corpus, 2 by 2 footprint, D4 symmetry, and 8 by 6 output are
  deliberate fixture choices. There is no image importer or training CLI in
  this example.
- Terrain is learned, while foliage and structure semantics are intentionally
  hand-authored. Learning downstream cross-pass relations is future work.
- This is an exact discrete constraint pipeline. It does not implement soft
  objectives, learned scoring, conflict-directed repair, or a minimal-change
  repair guarantee.
- The pas2js host targets Node for deterministic parity. It does not include a
  browser UI or require a JavaScript framework.
