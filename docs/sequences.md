# sequence models

The sequence foundation learns hard order-N constraints from ordered,
pretokenized UTF-8 samples, adapts the learned latent states to a one-dimensional
`TGraph`, and projects solved states back to caller tokens. It is shared Pascal
for native FPC and pas2js and adds no third-party runtime dependency.

This layer is useful for musical events, words, characters, command streams,
and any other ordered discrete vocabulary. The generic learner accepts token
arrays. The specialized [text foundation](text.md) adds a project-owned,
lossless Unicode-scalar tokenizer plus a portable caller-defined tokenizer
contract; MIDI and other media remain separate domain policies.

## learning contract

`LearnSequenceModel` learns one nonempty sequence. `LearnSequenceModelCorpus`
learns an ordered corpus of nonempty samples. `AOrder` is positive and means
that each latent state contains `Order - 1` history items plus one emitted
public token.

History uses a typed beginning-of-sequence item, `wshBos`, rather than a
reserved string token. Its token index is always `-1`, so every nonempty valid
UTF-8 caller token remains available. Each sample begins with a fresh BOS
history.
History never crosses a sample boundary, and the learner never adds a
last-to-first relation. Learning is therefore bounded/open only in version 1.

Tokens and states are ordered by first appearance across the ordered corpus.
The model retains raw observation, start, and end counts. It does not normalize,
smooth, or add unseen evidence. Sample lengths are also retained and validated
against the aggregate boundary counts.

Corpus isolation prevents accidental training seams, but it does not prevent
intentional structural recombination. If a suffix of one observed latent state
matches the prefix of another, those states are compatible even when that pair
was not adjacent in the corpus. This is the defining order-N behavior: the
model stores observed N-grams, then derives transitions from exact
suffix/prefix overlap. It does not quietly memorize order-N+1 pairs.

## immutable model

`TWfcSequenceModel` exposes ordered public tokens, sample lengths, typed state
history, emitted-token projection, raw counts, and structural compatibility.
Constructor inputs are copied, indexed accessors are read-only, and explicit
copy methods return detached arrays.

For order 1 the history is empty, so every observed token state is compatible
with every other. Higher orders progressively retain more left context. A
state with a fully BOS history is a legal observed start; a positive end count
marks an observed end.

## graph application and boundaries

`ApplySequenceModelToGraph` requires an already shaped, empty `Width x 1 x 1`
pass. It installs one graph value per latent state with raw observation counts
as relative weights and exact structural east/west support.

The extent-aware adapter distinguishes a whole sample, prefix, suffix,
interior fragment, and wrapped cycle. Whole paths require observed start and
end states. Prefixes require only an observed start. Suffixes require a
BOS-free start and observed end. Fragments require a BOS-free start but neither
observed endpoint. The compatibility overload retains the original whole-open
behavior. A width-one whole output must satisfy both endpoints.

`IntersectSequenceAllowedTokens` can constrain any position by one or more
public tokens without replacing an existing endpoint or caller domain.
`IntersectSequenceTokenConstraints`, `IntersectSequenceLockedSpan`,
`IntersectSequencePrefix`, and `IntersectSequenceSuffix` apply checked bulk
masks atomically. Repeated positions intersect.

`SequenceStateSatisfiesEntryConstraints` lets a higher-level validator check a
public state index against the current caller domain and lock without exposing
or reconstructing its model-qualified private graph key. Generated entry
values are outputs and are not mistaken for caller locks.

On a wrapped graph, every position is restricted to states with no BOS history
and the last state must structurally connect to the first. This is a cycle
derived from the learned overlap relation. It is not evidence that the source
was trained as wrapped, and some models have no satisfiable derived cycle.

`CaptureSolvedSequence` returns public token projection and public state
indices, records the selected extent, then independently validates its required
start/end semantics or the closing wrapped transition. Model-qualified graph
keys are private adapter values. They are
collision-safe with caller tokens and never belong in public output,
validation diagnostics, or `wfcs=1` artifacts.

`SequenceStatesSatisfyEntryConstraints` checks a complete captured state path
against current caller locks and allowed domains while proving the applied
model/graph identity once. Its single-position counterpart remains available
for probes. Both surfaces keep private graph keys inside the adapter.

`AnalyzeSequenceTokenDomains` provides solver-independent forward/backward
reachability for the same five extents. Every reported latent state and public
token lies on at least one globally feasible path satisfying the complete
positional mask; wrapped analysis additionally proves the closing edge. Public
tokens stay in first-seen vocabulary order and carry aggregate raw observation
weights. This is the exact inspection surface used by text completion.

## passes and projection

Latent sequence states carry context while ordinary pass values often expose
only public tokens. The adapter supplies both same-coordinate directions:

- `RequireSequenceProjectionFromTokenPass` makes each latent state require its
  emitted public token from a named public-token pass.
- `RequireProjectedSequenceFromPass` lets a value in a downstream pass require
  one or more public tokens projected from a named latent sequence pass.

These helpers keep private state keys out of application code while preserving
the normal dependency-DAG transaction. They express exact token alternatives,
not probabilistic or semantic similarity.

For different public vocabularies, `TWfcSequenceProjectionRules`,
`MakeWfcSequenceProjectionRule`, and
`ValidateSequenceProjectionMapFromPass` /
`RequireSequenceProjectionMapFromPass` define and preflight an exact
target-token to source-token map. The adapter validates complete target
coverage, applied model identity, and dependency acyclicity, then expands every
allowed source token to all of that model's private latent states before adding
the named pass dependency. Multiple named maps are independent requirement
groups and therefore AND together; alternatives inside one map are OR choices.
Preflight failure does not partially mutate the active pass.

When one target must depend on several latent providers, use
`TWfcSequenceProjectionBinding`, `MakeWfcSequenceProjectionBinding`, and
`ValidateSequenceProjectionMapsFromPasses` /
`RequireSequenceProjectionMapsFromPasses`. The bundle preflights every model
identity, pass label, dependency edge, target coverage, and source alternative
before adding the first requirement. It rejects repeated source labels: OR
alternatives belong inside one map, while distinct bindings intentionally form
AND groups. A malformed later binding therefore cannot leave an earlier map
installed. Bundle order is stable and does not change those logical semantics.

The [music foundation](music.md) uses this form to combine two latent source
models. Melody action maps to a rhythm attack/hold/rest token, while a sounding
melody pitch maps to a harmony pitch class. This demonstrates that projection
need not mean identical token text and that source history remains latent.
The [text pass owner](text.md#pass-composition) uses the same generic bundle to
make its punctuation surface depend directly on both lexical and structural
passes.

## canonical `wfcs=1` text

`EncodeWfcSequenceText` writes a strict ASCII, LF-only document with a final LF.
It records, in fixed order, the sequence order, ordered sample lengths, ordered
percent-encoded UTF-8 public tokens, and every latent state's raw observation,
start, and end counts, typed history atoms, and emitted-token index. Private
graph keys and derived relations are omitted.

The order-2 corpus `A B A` and `A C A` encodes as:

```text
wfcs=1
order=2
samples=2
s=0,3
s=1,3
tokens=3
t=0,A
t=1,B
t=2,C
states=5
q=0,2,2,0,B,E0
q=1,1,0,0,T0,E1
q=2,1,0,1,T1,E0
q=3,1,0,0,T0,E2
q=4,1,0,1,T2,E0
end
```

A `q` record contains the state index, observation count, start count, end
count, exactly `Order - 1` history atoms, and one emission atom. `B` is typed
BOS, `Tn` refers to public token index `n`, and `En` emits token index `n`.

`DecodeWfcSequenceText` rejects noncanonical integers or escapes, malformed
UTF-8, reordered or unknown fields, invalid indices or model invariants, CRLF,
missing final LF, and trailing data. A decoded document must re-encode
byte-for-byte. Canonical text therefore identifies the immutable learned model
without making graph-adapter implementation details part of the format.

## replay identity

Relearning the same model requires the exact ordered sample/token arrays,
sequence order, and `WFC_SEQUENCE_LEARN_ALGORITHM_VERSION`. Serialized identity
also includes `WFC_SEQUENCE_MODEL_VERSION` and `WFC_SEQUENCE_TEXT_VERSION`.
Graph output additionally depends on `WFC_SEQUENCE_GRAPH_MODEL_VERSION`,
`WFC_SEQUENCE_GRAPH_ADAPTER_VERSION`, graph length and wrapping, token-domain
intersections, selected extent, pass constraints, solve options,
solver/random versions, and the seed described in
[deterministic generation](determinism.md).

## version-1 scope

Version 1 deliberately does not perform smoothing, train a wrapped corpus, or
implement a probabilistic language model. It learns hard structural constraints
and raw relative counts from caller-supplied tokens. Dedicated extent and bulk
constraint helpers, exact public-domain analysis, and Unicode-scalar text
completion are now available. A standard project-owned word-boundary tokenizer,
variable-length editor, arbitrary-corpus training interface, and browser editor
with global cross-pass domains remain roadmap work. The fixed-length
[three-pass text workbench](../examples/text/03_PassComposition/README.md) now
provides interactive public locks, lineage, contradictions, and exact replay.

The portable foundation remains project-owned Pascal. A tokenizer, event
codec, exporter, or inspector that can reasonably be implemented for FPC and
pas2js belongs in the ecosystem rather than becoming a required library. The
Unicode-scalar tokenizer and exact domain analyzer follow that rule without a
host regex, locale service, or third-party runtime.

See the portable
[LearnSequence example](../examples/sequence/01_LearnSequence/README.md) for
canonical round-trip, seeded open generation, independent validation, and
public/latent/public pass composition from one source on both targets.
See [ConstraintCompletion](../examples/text/02_ConstraintCompletion/README.md)
for anchored infill, prefix-only continuation, exact candidate domains, and
native/pas2js replay.
