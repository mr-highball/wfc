# LearnTiles

`LearnTiles.lpr` is a complete, dependency-free training and replay demo. It
starts with a pretokenized 5 by 5 ASCII island, observes raw token and cardinal
neighbor frequencies, expands those observations through the eight explicit
D4 transforms, serializes the immutable result to canonical model text, reads
that text back, and applies the replayed model to a 24 by 12 `TGraph`.

```text
~~~~~
~...~
~.T.~
~...~
~~~~~
```

The glyphs are `~` water, `.` ground, and `T` tree. The sample uses wrapped
boundaries, and the generated graph enables the same wrapping policy. That
choice gives every observed token at least one neighbor in every cardinal
direction, including tokens on the sample edge.

The output reports the learning, model-text, random, and solver algorithm
versions; each token's raw learned weight; the canonical artifact size; the
seed; solve statistics; and the generated grid. Before printing, an independent
checker visits all 1,152 directed cardinal adjacencies and verifies each one
directly against `TWfcModel.RelationCount`. It does not consult the graph's
installed rule groups.

## build and run

Create the named `units` and `bin` directories first. From the repository root,
compile and run with native FPC:

```text
fpc -B -Mdelphi -Sa -Cr -Co -Ci -Fusrc -FUbuild/examples/learning/native/units -FEbuild/examples/learning/native/bin examples/learning/01_LearnTiles/LearnTiles.lpr
build/examples/learning/native/bin/LearnTiles 0
```

With a configured pas2js compiler and matching RTL, compile the same Pascal
source for Node.js:

```text
pas2js -B -Tnodejs -Mdelphi -Fusrc -FUbuild/examples/learning/pas2js/units -FEbuild/examples/learning/pas2js/bin examples/learning/01_LearnTiles/LearnTiles.lpr
node build/examples/learning/pas2js/bin/LearnTiles.js 0
```

The optional argument is an unsigned 32-bit seed (`0` through `4294967295`;
Pascal-style hexadecimal input such as `$DEADBEEF` is also accepted). Omit it
to use the demo default, `$4C454152`. An invalid seed, failed solve, canonical
round-trip failure, or invalid generated adjacency prints an actionable
`LearnTiles error` message and exits nonzero.

## invariants demonstrated

- token identity follows deterministic first-seen, row-major order;
- weights and directional relation counts are raw positive integer
  observations rather than floating-point probabilities;
- D4 means identity, three rotations, and the mirrored form plus its three
  rotations;
- canonical model text is byte-stable, LF-terminated, and decoded before use;
- the learned boundary policy and graph wrapping policy match;
- the supplied seed replays through the versioned portable random and solver
  algorithms; and
- every generated north, east, south, and west pair has positive support in
  the learned model.

## finite support boundary

An open sample can legitimately observe no neighbor for a particular
token/direction pair. `ApplyModelToGraph` preserves that finite boundary as an
explicit `DenyAll` direction; it does not widen a zero-support row into a
wildcard. Directions outside the model rank remain unconstrained. This demo
uses a wrapped sample so every observed token has cardinal support and applies
the same wrapping policy to its output topology.

This example uses the radius-one, cardinal, single-layer learner with one
sample. Ordered heterogeneous corpora and deterministic model merging are
available in the core and demonstrated by `02_LearnCorpus`; multi-cell 2D
neighborhoods are handled by the separate learner demonstrated by
[`03_LearnPatterns`](../03_LearnPatterns/README.md). Smoothing and learned
cross-pass relations remain explicit future extensions rather than hidden
behavior. See [model learning and priming](../../../docs/learning.md) and
[overlapping 2D patterns](../../../docs/patterns.md) for the model, adapter,
serialization, projection, and replay contracts.
