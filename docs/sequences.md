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
History never crosses a sample boundary. Open learning does not observe the
last-to-first seam. It remains the default, preserving the version-1 contract.

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

## Circular source training

Both learner functions accept a final `TWfcModelBoundary` argument:

```pascal
Model := LearnSequenceModel(Tokens, Order, wmbWrap);
Model := LearnSequenceModelCorpus(Samples, Order, wmbWrap);
```

The returned models are caller-owned. Omitting the argument, or passing
`wmbOpen`, keeps all existing open-model bytes, counts, ordering and replay
identities unchanged. The immutable model exposes `Boundary` and `ModelVersion`.

With `wmbWrap`, each nonempty sample is an independent circle. The state at
position `p` contains the preceding `Order - 1` tokens taken cyclically from
that same sample, followed by its emitted token. An order greater than sample
length repeats the history as often as necessary; it does not add synthetic
observations. Every original position contributes exactly once. A singleton
at order 1024 therefore contributes one state and one observation, not 1024.
There is no BOS, no observed beginning or end, and every start/end count is
zero. Samples are neither joined nor rotated into extra training examples.

This distinction matters even for `A B` at order 2. Open learning observes
`BOS -> A` and `A -> B`, which cannot form a BOS-free cycle. Circular learning
observes `B -> A` as well as `A -> B`; an even-length output can close, while
an odd-length output cannot. Wrapping is a constraint, not permission to copy
a completed output until its requested length is filled.

Raw weighted context counts must balance: the total number of observations
leaving each history context equals the total entering that context. The
constructor and decoder check this in addition to uniqueness, vocabulary,
sample totals, no BOS/endpoints, and predecessor/successor support. These are
aggregate model consistency checks. They do not reconstruct the original
sample order or prove that the summary can be partitioned into the recorded
individual sample lengths. Retain the source training document for provenance
and exact relearning.

Circular training uses the same resource limits as open training; it does not
increase the state, order, total-history or encoding ceilings. It adds no
tokenizer, normalization, smoothing, external library or runtime dependency.

The [Training Studio](training-studio.md) offers a circular-text preset and an
explicit raw-text boundary choice. Its portable `wfclearn=5` sources retain
the circular policy through learning, recipe export, public locks/quotas,
validation, inspection and exact result replay. The preset's phrase locks are
visible constraints, not changes to the learned vocabulary or weights.

## immutable model

`TWfcSequenceModel` exposes ordered public tokens, sample lengths, typed state
history, emitted-token projection, raw counts, and structural compatibility.
Constructor inputs are copied, indexed accessors are read-only, and explicit
copy methods return detached arrays.

For order 1 the history is empty, so every observed token state is compatible
with every other. Higher orders progressively retain more left context. A
state with a fully BOS history is a legal observed open start; a positive end
count marks an observed open end. Circular models have neither endpoint.

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
derived from the learned overlap relation. For an open-trained model it is
not evidence of circular source observations, and some models have no cycle.
For a circular-trained model the seam was explicitly included in training,
but not every requested output length or lock combination is satisfiable.
Use `wseWrap` to enforce closure, or `wseFragment` for an open finite excerpt
without invented endpoints. `wseWhole`, `wsePrefix`, and `wseSuffix` are
unsatisfiable for a circular model because they require observed endpoints.

`CaptureSolvedSequence` returns public token projection and public state
indices, records the selected extent, then independently validates its required
start/end semantics or the closing wrapped transition. Model-qualified graph
keys are private adapter values. They are
collision-safe with caller tokens and never belong in public output,
validation diagnostics, or saved `wfcs` artifacts.

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

## Canonical sequence text

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

Open models always encode as `wfcs=1`, with the original layout above.
Circular models use `wfcs=2` and a required `boundary=wrap` line immediately
after the header. The remaining record layout is unchanged. For the circular
order-2 sample `A B`:

```text
wfcs=2
boundary=wrap
order=2
samples=1
s=0,2
tokens=2
t=0,A
t=1,B
states=2
q=0,1,0,0,T1,E0
q=1,1,0,0,T0,E1
end
```

Version 2 rejects `boundary=open`, missing/duplicate boundary fields, BOS,
nonzero start/end counts, and unbalanced weighted contexts. Version 1 does
not accept a version-2 boundary line. `WfcSequenceModelTextVersion(Model)`
reports the canonical encoding version rather than silently upgrading old
files. Both versions retain the same strict text envelope limits.

## Exact continued segments

`TWfcSequenceSegmentBoundary` is a separate, versioned boundary contract for
streaming through one immutable model. An initial boundary requires an
observed start and canonical `PreviousState = -1`. A continuing boundary
requires the first state to be a structural successor of the exact previous
latent state. Either can optionally require an observed terminal state.

Use `MakeWfcSequenceInitialSegmentBoundary` or
`MakeWfcSequenceContinuingSegmentBoundary`, then
`ApplySequenceModelSegmentToGraph`. Capture with
`CaptureSolvedSequenceSegment` and independently check with
`ValidateSequenceSegmentStatePath`. State witnesses are model-relative; the
returned arrays are detached, but validating them later still needs the same
model. A public token alone is not enough to identify an order-N frontier.

This is not `wseFragment`: tiny early segments may still carry typed BOS
history. Excluding all BOS-bearing states at every new segment would reject
valid continuations. A circular model has no observed initial/terminal
boundary: obtain a validated fragment or ring witness first, then use its
exact final state as a continuing frontier without requiring an observed end.
Continued segments alone do not guarantee that an eventual last segment
closes onto the first. Existing graph-adapter version constants remain
unchanged. See the
[ensemble stream](music-ensemble-stream.md) for a three-pass application and
its independently checked continuation semantics.

## replay identity

Relearning the same model requires the exact ordered sample/token arrays,
sequence order, source boundary, and learner version. Open learning retains
`WFC_SEQUENCE_LEARN_ALGORITHM_VERSION = 1`; circular learning uses
`WFC_SEQUENCE_WRAPPED_LEARN_ALGORITHM_VERSION = 2`. Open model/text version
constants stay 1; the corresponding wrapped constants are 2. Consult
`Model.ModelVersion` and `WfcSequenceModelTextVersion(Model)` for a particular
model. The graph's private identity also includes circular provenance without
changing existing open-model keys.
Graph output additionally depends on `WFC_SEQUENCE_GRAPH_MODEL_VERSION`,
`WFC_SEQUENCE_GRAPH_ADAPTER_VERSION`, graph length and wrapping, token-domain
intersections, selected extent, pass constraints, solve options,
solver/random versions, and the seed described in
[deterministic generation](determinism.md).

## Scope and conformance

Neither learning mode performs smoothing or implements a probabilistic
language model. The library learns hard structural constraints
and raw relative counts from caller-supplied tokens. Dedicated extent and bulk
constraint helpers, exact public-domain analysis, and Unicode-scalar text
completion are now available. A standard project-owned word-boundary tokenizer,
variable-length editor, and browser editor with global cross-pass domains
remain roadmap work. Editable arbitrary-corpus import is available through
the bounded Training Studio. The fixed-length
[three-pass text workbench](../examples/text/03_PassComposition/README.md) now
provides interactive public locks, lineage, contradictions, and exact replay.

The portable foundation remains project-owned Pascal. A tokenizer, event
codec, exporter, or inspector that can reasonably be implemented for FPC and
pas2js belongs in the ecosystem rather than becoming a required library. The
Unicode-scalar tokenizer and exact domain analyzer follow that rule without a
host regex, locale service, or third-party runtime.

`wfc_sequence_wrap_test` compares the learner against literal repeated-source
window histograms across binary corpora, heterogeneous lengths and orders
larger than samples. It independently enumerates candidate public rings to
check exact feasible domains, and covers strict codec mutations, weighted
balance rejection, locks, closing-edge contradictions and model identity.
The training and circular-studio suites cover the source-to-replayed-result
path on native FPC and pas2js. The native artifact process suite checks the
new saved formats through real command-line file and stdin boundaries.

See the portable
[LearnSequence example](../examples/sequence/01_LearnSequence/README.md) for
canonical round-trip, seeded open generation, independent validation, and
public/latent/public pass composition from one source on both targets.
See [ConstraintCompletion](../examples/text/02_ConstraintCompletion/README.md)
for anchored infill, prefix-only continuation, exact candidate domains, and
native/pas2js replay.
