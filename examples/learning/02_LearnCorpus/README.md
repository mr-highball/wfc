# LearnCorpus

`LearnCorpus.lpr` is a portable, dependency-free corpus-training and replay
demo. It feeds two ordered, heterogeneous 1D samples into
`LearnModel1DCorpus`, preserving both sample shapes and deterministic
first-seen token order:

```text
sea | shore | dune | meadow | forest | meadow | town | shore
hill | forest | hill | meadow | shore | sea | shore | town | meadow | town | road
```

The samples have different lengths and each introduces vocabulary absent from
the other. They also have different endpoints. Both are learned as separate
wrapped loops: the coast closes `shore -> sea`, while the inland sample closes
`road -> hill`. The demo checks those local relations and explicitly rejects
the synthetic `shore -> hill` and `road -> sea` seams that concatenating the
samples would create, including their reciprocal west observations.

The learned model has two sample-shape records, so its canonical artifact uses
the `wfcm=2` format. The demo checks that header, decodes the artifact, checks a
byte-identical re-encoding, applies the replayed model to a wrapped 48-token
`TGraph`, sets an explicit seed, and calls `TrySolve`.

Before printing the result, an independent validator visits every generated
entry's east and west neighbor. All 96 directed pairs must have a positive raw
count in `TWfcModel.RelationCount`; the validator does not consult the graph's
installed rule groups.

## build and run

Create the named `units` and `bin` directories first. From the repository root,
compile and run with native FPC:

```text
fpc -B -Mdelphi -Sa -Cr -Co -Ci -Fusrc -FUbuild/examples/learning-corpus/native/units -FEbuild/examples/learning-corpus/native/bin examples/learning/02_LearnCorpus/LearnCorpus.lpr
build/examples/learning-corpus/native/bin/LearnCorpus 0
```

With a configured pas2js compiler and matching RTL, compile the same Pascal
source for Node.js:

```text
pas2js -B -Tnodejs -Mdelphi -Fusrc -FUbuild/examples/learning-corpus/pas2js/units -FEbuild/examples/learning-corpus/pas2js/bin examples/learning/02_LearnCorpus/LearnCorpus.lpr
node build/examples/learning-corpus/pas2js/bin/LearnCorpus.js 0
```

The optional argument is an unsigned 32-bit seed (`0` through `4294967295`;
Pascal-style hexadecimal input such as `$DEADBEEF` is also accepted). Omit it
to use the explicit demo seed `$434F5250`. A malformed seed, solve failure,
noncanonical artifact, or unsupported generated adjacency prints an actionable
`LearnCorpus error` message and exits nonzero.

## invariants demonstrated

- corpus order and token first-seen order are deterministic;
- sample shapes remain ordered metadata in canonical `wfcm=2` text;
- raw token weights and east/west relation counts are summed across samples;
- wrapped observations close each sample independently, without synthesizing
  cross-sample adjacency;
- exact local-wrap counts are present while both would-be concatenation seams
  remain absent;
- canonical model text is byte-stable, LF-terminated, and decoded before use;
- the learned boundary policy and generated graph topology both wrap;
- the supplied seed replays through versioned portable random and solver
  algorithms; and
- every generated east and west pair has positive support in the replayed
  model.

This example deliberately trains radius-one token adjacency. The core exposes
model merging separately; overlapping 2D patterns are the explicit
higher-level operation demonstrated by
[`03_LearnPatterns`](../03_LearnPatterns/README.md). Smoothing and learned
cross-pass constraints remain future operations rather than hidden corpus
behavior. See [model learning and priming](../../../docs/learning.md) and
[overlapping 2D patterns](../../../docs/patterns.md) for the model, adapter,
serialization, projection, and replay contracts.
