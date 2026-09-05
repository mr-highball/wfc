# deterministic generation

WFC owns its generation randomness. Assigning an explicit `TGraph.Seed` makes
both the legacy `Run` path and the reference `TrySolve` path replay the same
model and input on native FPC and pas2js.
Random entry identifiers and the host runtime's global random source are not
part of this stream.

The full-pipeline `TrySolveNegotiated` path uses the same streams and adds a
separately versioned chronological attempt transcript. Its replay contract is
defined in [bounded pass negotiation](pass-negotiation.md).

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
- `WFC_SOLVER_ALGORITHM_VERSION` and `TGraphSolveOptions` when using
  `TrySolve`;
- `WFC_GRAPH_MODEL_VERSION`, explicit denied directions, and canonical
  pass-local entry domains;
- `WFC_PIPELINE_ALGORITHM_VERSION`, pass modes, dependency edges, named
  requirements, signed offsets, ordered finite any-clauses, and requested
  roots when using dependency planning or selective regeneration;
- `WFC_PASS_NEGOTIATION_ALGORITHM_VERSION`,
  `WFC_PASS_NEGOTIATION_HASH_VERSION`, nested `SolveOptions`, and
  `MaxPassBacktracks` when using `TrySolveNegotiated`;
- `WFC_SELECTIVE_NEGOTIATION_ALGORITHM_VERSION`,
  `WFC_SELECTIVE_NEGOTIATION_HASH_VERSION`, canonical requested-root indices,
  and the active descendant closure when using
  `TryRegenerateNegotiatedFrom`;
- `Seed`;
- graph dimensions, wrapping, and run mode;
- pass creation order;
- values and rules in their original construction order, plus canonical
  pass-local weights for `TrySolve`;
- caller locks and other model input; and
- deterministic custom callback configuration for the legacy `Run` path.

Pass labels are not part of random-stream identity. Renaming a pass preserves
its stream because the stable zero-based pass index is used instead. Appending
a later pass cannot perturb an earlier stream in ordinary `Run`, `TrySolve`, or
selective solving. Negotiation deliberately makes the full later pipeline part
of an earlier pass's retained assignment: a later contradiction can cause that
pass to take another alternative, while its random ticket and candidate order
remain deterministic.

Labels remain part of a human-readable dependency manifest and are used to
select roots, but declared dependency and requirement references bind to stable
indices. Renaming a source therefore does not retarget an existing in-memory
edge or constraint.

Value construction order is part of the model. The legacy built-in selection
chooses uniformly from valid values in that order and deliberately ignores
reference-solver weights. The reference solver keeps the same order, maps one
random ticket through cumulative weights, and tries alternatives cyclically
from the chosen value. Code that builds equivalent rules through different
value insertion orders has defined two different replay inputs, even when their
sets are equal.

Raw weights are positive `Integer` relative frequencies. Each defined pass
divides its complete vector by the greatest common divisor before entropy or
random selection. Multiplying every weight in a pass by the same positive
integer therefore preserves replay exactly, including bounded-sampling
rejection behavior and the following stream state. The canonical normalized
vector is the identity; zero is not a disabled value.

Models produced by the learning layer add their own replay inputs: observation
and corpus algorithm versions, every ordered tokenized sample and shape,
boundary policy, and symmetry policy. Rebuilding from separately learned
models also includes their ordered input list and the merge algorithm version.
A canonical `.wfcm` document captures the resulting immutable shapes, weights,
and relations directly. See [model learning and priming](learning.md) for that
contract and its deliberate tokenization boundary.

Applying a learned or overlapping model to a graph additionally includes
`WFC_MODEL_GRAPH_ADAPTER_VERSION`. Active zero-support rows are explicit
denials under that adapter version; inactive directions remain wildcards.

Overlapping models additionally depend on footprint dimensions and
`WFC_OVERLAPPING_2D_ALGORITHM_VERSION`. Canonical `.wfcp` stores source shapes,
palette, weighted payloads, and the complete recomputable structural relation
set, but not the original corpus. Output replay also includes latent graph
shape and wrapping, locks, and projection version. See
[overlapping 2D patterns](patterns.md).

Sequence models additionally depend on the exact ordered pretokenized UTF-8
corpus, order, and `WFC_SEQUENCE_LEARN_ALGORITHM_VERSION`. Canonical `wfcs=1`
captures ordered sample lengths, public tokens, typed BOS/token histories,
emissions, and raw observation/start/end counts. Graph replay also includes
  `WFC_SEQUENCE_MODEL_VERSION`, `WFC_SEQUENCE_TEXT_VERSION`,
  `WFC_SEQUENCE_GRAPH_MODEL_VERSION`, `WFC_SEQUENCE_GRAPH_ADAPTER_VERSION`,
  output length and extent, public-token domain intersections, and any latent
  or projected cross-pass requirements. Wrapped output is a derived BOS-free
  cycle, not wrapped training evidence. Text completion additionally includes
  the exact raw documents or serialized sequence model, tokenizer kind and
  version, ordered prefix/suffix/locked-span/domain request, completion version,
  and independent validation contract. See [sequence models](sequences.md) and
  [text constraint completion](text.md).

Text Pass Composition additionally includes
`WFC_TEXT_PASS_PIPELINE_VERSION`, `WFC_TEXT_PASS_FRAGMENT_VERSION`, the three
ordered structure/lexical/punctuation models, all complete projection rules
and provider-binding order, token length and extent, every per-layer public
domain or lock, solve options, and seed. Fragment tokens are canonical inputs,
not host-formatted strings. The example signature `1:69ABA6CE` identifies the
version-1 seed-zero showcase fixture; it is not a general serialized
`TWfcTextPassPipeline` format.

The replay contract assumes values and rules are registered through
the public builders or a versioned project adapter such as
`ApplyModelToGraph`. Arbitrary caller mutation of exposed rule arrays or
dictionary internals bypasses ordered registration and is not a portable
replay input. The model adapter's dense installation path validates complete
reciprocity and preserves the historical public rule order before assigning
arrays; it is not a general bulk-rule API.

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
FPC and pas2js.

The specialized 2D suite also freezes a complete terrain → biome → foliage
fixture as `1:F7B994F3:E2E8E15B:21B79A66`. Its versioned CRC-32 stream uses
fixed model tokens rather than compiler string bytes, so native FPC and pas2js
hash exactly the same canonical layer data. This is an output checksum, not a
decision trace and not a replacement for the core replay identity. See the
[2D signature contract](world2d.md#portable-signatures).

Any incompatible change to seed expansion, stream derivation, or bounded
sampling must increment `WFC_RANDOM_ALGORITHM_VERSION`. An incompatible change
to reference-solver propagation, observation, candidate ordering, or
backtracking must increment `WFC_SOLVER_ALGORITHM_VERSION`. Legacy traversal
or built-in-selection changes must likewise receive an explicit compatibility
version rather than silently reinterpreting existing replay inputs. An
incompatible change to deny-all, entry-domain, or other graph-input semantics
must increment `WFC_GRAPH_MODEL_VERSION`.
An incompatible change to dependency planning, spatial clause/boundary
semantics, pass-mode staging, dirty-closure selection, or topological
tie-breaking must increment
`WFC_PIPELINE_ALGORITHM_VERSION` independently.
An incompatible change to negotiation frame selection, exact-assignment
scoping, later-exclusion clearing, stopping rules, or attempt ordering must
increment `WFC_PASS_NEGOTIATION_ALGORITHM_VERSION`. A change to the portable
transcript encoding must increment `WFC_PASS_NEGOTIATION_HASH_VERSION`.
An incompatible change to selective root canonicalization, descendant-closure
construction, clean-pass preservation, active frame eligibility, or scope
reporting must increment `WFC_SELECTIVE_NEGOTIATION_ALGORITHM_VERSION`.
A change to the outer selective transcript encoding must increment
`WFC_SELECTIVE_NEGOTIATION_HASH_VERSION`. These versions are independent so a
scope change does not silently reinterpret full-pipeline Pass Negotiation v1.

The weighted entropy and ticket contract is
`WFC_SOLVER_ALGORITHM_VERSION = 2`. It does not change seed expansion, jumping,
or bounded sampling, so `WFC_RANDOM_ALGORITHM_VERSION` remains `1`. The Q16
logarithm uses only shifts, exact integer-valued `Double` intermediates, and
power-of-two divisions; it does not delegate replay decisions to a host
transcendental math implementation.

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

Every `TrySolve` also rewinds every pass stream before solving. It stages the
complete pass pipeline, so a contradiction or backtrack-limit result leaves
all entry values and `Generated` flags unchanged and restores the random states
that existed before the call. Successful reports record the random, solver,
graph-model, and pipeline versions. Reference observation ties use `Mode` Z
order and then entry index;
non-unit models use deterministic Q16 Shannon entropy, while canonical
unit-weight models retain the exact minimum-domain path. Candidate order follows
`AddValue` order from one weighted first ticket and then a frozen cyclic retry
order. See the [reference solver contract](solver.md) for the full algorithm
identity.

`TryRegenerateFrom` rewinds and executes only the requested roots and their
transitive dependents. Skipped pass entries and random streams remain exactly
unchanged. Dirty pass streams restart from their stable seed/index derivation;
failure restores every entry, ownership flag, selected pass, and pre-call
stream state. `ExecutionOrder` records the actual stable topological plan.

`TrySolveNegotiated` always executes complete-pipeline rounds. Each round
rewinds pass streams from the same seed. A failed round commits nothing and
restores the pre-call streams before the next round; the final success leaves
the streams exactly where that winning ordinary round leaves them. Frozen
local candidate order means rejecting an exact assignment does not draw a new
first ticket, although restoring past that exclusion consumes a local solver
backtrack.

Negotiated replay identity includes the complete model and Pipeline v2 plan,
both negotiation and transcript versions, both budgets, trace-capture setting,
stable chronological choice-frame order, exact exclusions in their earlier
prefix contexts, every rejected ordinary report, and `FinalReport`.
`CalculateGraphNegotiationTranscriptHash` summarizes those numeric fields, but
the copied assignment arrays—not the hash—are the exact nogood identity.

`TryRegenerateNegotiatedFrom` runs the same round protocol over exactly the
canonical requested roots and their transitive descendants. Active streams
follow the round behavior above. Every stream outside the active closure
remains byte-for-byte at its pre-call state even after success. A failed call
restores all streams, entries, ownership flags, and pass selection. The method
never activates a clean provider implicitly; choosing an earlier root is a
caller-visible replay input.

`CalculateGraphSelectiveNegotiationTranscriptHash` adds the two selective
versions, scope algorithm version, complete canonical root array, and active
topological array around a fresh recomputation of the nested negotiation
transcript. Duplicate or reordered labels that resolve to the same stable root
set therefore share one scope identity. With zero outer budget, the sole
`Search.FinalReport` retains exact ordinary `TryRegenerateFrom` parity for the
same roots and solve options.

Every rejected and terminal round can retain a separate ordinary Trace-v1
hash. This preserves contiguous per-pass slices instead of interleaving a
provider revisit into one synthetic trace. See [causal solve
traces](traces.md#negotiated-rounds).

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

Changing `Seed` during pass initialization, `Run`, `TrySolve`, or a negotiated
round raises `EInvalidOperation` and leaves the old seed intact. Set it before
execution.

`TrySolve` and `TrySolveNegotiated` do not invoke selection or invalid-state
callbacks, or the legacy `DoGetStartCoord`, `DoGetSelection`, and `DoValidate`
hooks. Their mutable state therefore cannot influence the reference decision
stream. Conversely, assigning
`Rules[Value].Weight` does not reinterpret legacy `Run`; callers that need a
weighted compatibility traversal must implement that policy in their selection
callback.

## what the seed does not promise

The seed does not make an impossible model satisfiable or provide cryptographic
randomness. It also cannot make nondeterministic legacy callbacks deterministic
on their own. `Run` remains a reproducible greedy compatibility path and does
not gain independent validation merely because it is seeded. `TrySolve` does
validate its staged assignments and names both algorithm versions in its
report; persist those versions, the solve options, and the model identity with
any result intended for long-term replay.
