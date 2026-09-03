# deterministic generation

WFC owns its generation randomness. Assigning an explicit `TGraph.Seed` makes
the built-in solver replay the same model and input on native FPC and pas2js.
Random entry identifiers and the host runtime's global random source are not
part of this stream.

```pascal
var
  LGraph: TGraph;
begin
  LGraph := TGraph.Create;
  try
    LGraph.Seed := $DEADBEEF;
    LGraph.Reshape(32, 16, 1);

    LGraph.CurrentPass := 'terrain';
    LGraph.AddValue('land');
    LGraph.AddValue('water');

    LGraph.SwitchToPass('foliage');
    LGraph.AddValue('tree').RequirePrevious('land');
    LGraph.AddValue('none');

    LGraph.Run;
    WriteLn('Replay seed: ', LGraph.Seed);
  finally
    LGraph.Free;
  end;
end;
```

If no seed is assigned, the first `Seed` read or first operation that needs a
pass captures an automatic 32-bit seed once, preserving the historical
stochastic default. Constructing a graph and immediately assigning an explicit
seed does not consume the host's global random stream. Read and persist
`LGraph.Seed` with generated output when that run may need to be reproduced.
The automatic seed is deliberately not a cryptographic random value.

## replay identity

A complete replay identity consists of:

- `WFC_RANDOM_ALGORITHM_VERSION`;
- `Seed`;
- graph dimensions, wrapping, and run mode;
- pass creation order;
- values and rules in their original construction order;
- caller locks and other model input; and
- deterministic custom callback configuration.

Pass labels are not part of random-stream identity. Renaming a pass preserves
its stream because the stable zero-based pass index is used instead. Appending
a later pass cannot perturb an earlier stream.

Value construction order is part of the version-1 model. The built-in
selection callback chooses an index from the valid values in that order. Code
that builds equivalent rules through different value insertion orders has
defined two different replay inputs, even when their sets are equal.

The replay contract assumes values and rules are registered through
`AddValue`, `NewRule`, and `RequirePrevious`. Directly mutating the exposed
rule arrays or dictionary internals bypasses ordered registration and is not a
portable replay input.

Entry GUIDs are intentionally excluded from replay output. Canonical results
should serialize passes by index and entries by index or coordinate.

## version-1 random source

`WFC_RANDOM_ALGORITHM_VERSION = 1` uses xoshiro128++ 1.0 with four 32-bit state
words. The authors' [reference implementation](https://prng.di.unimi.it/xoshiro128plusplus.c)
is dedicated to the public domain; their [generator notes](https://prng.di.unimi.it/)
describe xoshiro128++ as the 32-bit all-purpose member of the family.

The scalar seed expands through MurmurHash3 `fmix32`:

```text
S0 = fmix32(Seed xor A511E9B3)
S1 = fmix32(Seed xor 63D83595)
S2 = fmix32(Seed xor B8D9C6AB)
S3 = fmix32(Seed xor 9E3779B9)
```

Pass zero uses that state. Pass `N` applies the official xoshiro128++ `2^64`
jump `N` times, using this polynomial:

```text
8764000B F542D2D3 6FA035C3 77F2DB5B
```

`RandomIndex(Count)` returns an unbiased integer in `0..Count - 1` through
rejection sampling. `Count <= 0` raises `ERangeError`. `Count = 1` returns zero
without advancing the stream. The implementation uses explicit 32-bit
wrapping helpers so checked native builds and JavaScript use identical
arithmetic.

The conformance suite fixes known-answer vectors for seeds `0`, `1`,
`$DEADBEEF`, and `$FFFFFFFF`, the first two jumped streams, forced rejection
sampling, bottom-up and wrapped top-down 3D grids, and a three-pass
terrain/foliage/copy pipeline. Those output fixtures are checked by both native
FPC and pas2js/Node.

Any incompatible change to seed expansion, stream derivation, bounded
sampling, candidate ordering, traversal order, or the built-in generator must
increment `WFC_RANDOM_ALGORITHM_VERSION`.

## run and reset behavior

Every `Run` rewinds every pass stream from the pipeline seed before solving.
Consequently:

- two consecutive runs of unchanged input replay exactly;
- calls to `RandomIndex` made outside `Run` do not affect the next run;
- unrelated `System.Random` calls and random GUID generation have no effect;
- extra draws in one pass do not advance another pass; and
- restoring an earlier seed restores its stream from the beginning.

`Reset` clears the model and additional passes but preserves `Seed`. The fresh
pass zero begins at the same versioned stream.

A failed `Run` is not yet a transaction over generated cell values. A later
run rewinds randomness and clears solver-owned output, but caller locks or
external callback state changed during the failed attempt remain changed.

## callbacks and extension hooks

Custom `SelectionCallback`, `InvalidStateCallback`, `DoGetStartCoord`, and
`DoGetSelection` implementations must obtain random choices through
`RandomIndex` to participate in replay. Host-language random functions,
timestamps, unordered external data, and mutable callback counters are outside
the guarantee unless the caller resets them identically.

Selection and invalid-state callbacks receive the root graph. During `Run`,
calling `AGraph.RandomIndex` always consumes the stream for the pass being
solved, even if the callback switches `CurrentPass` first. Calling
`AGraph.PassGraph[J].RandomIndex` explicitly consumes pass `J` instead.

Changing `Seed` during pass initialization or `Run` raises
`EInvalidOperation` and leaves the old seed intact. Set it before execution.

## what the seed does not promise

The seed makes the current greedy solver reproducible; it does not make an
impossible model satisfiable, validate output independently, or provide
cryptographic randomness. It also cannot make nondeterministic user callbacks
deterministic on their own.

Future reference solvers may have their own versioned decision algorithms.
Their replay metadata must name both the random algorithm version and the
solver/model version rather than silently reinterpreting version-1 seeds.
