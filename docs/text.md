# text constraint completion

The text foundation performs hard, constraint-driven completion over the
project's bounded sequence model. It learns exact order-N Unicode-scalar
neighborhoods, preserves caller locks, exposes every globally feasible token
at every position, and solves a fixed-length result with the same deterministic
FPC/pas2js graph kernel used by the other ecosystems.

It is not a probabilistic language model and is not presented as an LLM
replacement. It is useful when a corpus, grammar, template, or upstream pass
defines a finite vocabulary and forbidden local structures matter more than
open-ended semantic invention.

## units

- `wfc_text_tokenize` implements project-owned Unicode-scalar tokenization,
  exact detokenization, raw-document corpus learning, and a portable
  caller-defined tokenizer callback contract.
- `wfc_sequence_analyze` computes exact feasible latent and public domains for
  a constrained path without reading private graph keys.
- `wfc_text_complete` combines prefix, suffix, locked-span, and allowed-token
  domains; solves the latent sequence graph; reconstructs text; and validates
  the result independently.
- `wfc_text_passes` owns the reusable structure -> lexical -> punctuation
  dependency DAG, public constraint surface, exact fragment renderer,
  public-token trace projection, selective regeneration, and independent
  three-layer validation.
- `wfc_sequence_graph` supplies the generic extent-aware adapter and atomic
  bulk token constraints plus atomic N-source projection bundles. Those APIs
  are reusable for music and other ordered domains rather than being text-only
  conveniences.

All maintained runtime code is shared Pascal. It depends on repository units
and the applicable standard FPC or pas2js RTL only.

## Unicode-scalar tokenizer

`TokenizeWfcText(..., wttkUnicodeScalar)` returns exactly one nonempty token for
each Unicode scalar value. Spaces, tabs, line endings, punctuation, combining
marks, and supplementary-plane values are preserved rather than discarded.
`DetokenizeWfcText` requires every input token to contain exactly one scalar and
concatenates them losslessly.

The implementation does not call a host regex, locale service, browser `Intl`
API, or external Unicode library. Native FPC storage is validated and walked as
canonical UTF-8. pas2js storage is validated and walked as UTF-16, including
checked surrogate pairs. Positions in every text API are token/scalar ordinals,
never native byte offsets or JavaScript UTF-16 code-unit offsets.

`LearnWfcTextModel` tokenizes an ordered array of nonempty documents and passes
the resulting isolated samples to `LearnSequenceModelCorpus`. A second overload
accepts `TWfcTextTokenizeCallback`, so an application can own a specialized
tokenization policy while retaining sequence validation and learning. The
standard high-level completion request currently uses the versioned scalar
tokenizer. A standard word tokenizer remains open until the repository carries
its own versioned Unicode word-boundary data; platform-dependent classification
is not an acceptable shortcut.

## path extents

`TWfcSequenceExtent` separates corpus boundary meaning from graph wrapping:

| Extent | Start evidence | End evidence | BOS rule | Closing edge |
| --- | --- | --- | --- | --- |
| `wseWhole` | required | required | permitted at position 0 | no |
| `wsePrefix` | required | not required | permitted at position 0 | no |
| `wseSuffix` | not required | required | forbidden at position 0 | no |
| `wseFragment` | not required | not required | forbidden at position 0 | no |
| `wseWrap` | not required | not required | forbidden everywhere | required |

This distinction matters for ordinary completion. A nine-scalar `wsePrefix`
may validly stop in the middle of a learned sentence. The same path under
`wseWhole` must end at a state observed at a corpus end and is therefore
rejected when truncated. The original `ApplySequenceModelToGraph` and
`ValidateSequenceStatePath` overloads retain their whole/open and wrapped
behavior for existing callers.

## constraints

`TWfcSequenceTokenConstraint` assigns an allowed public-token set to one
position. Multiple constraints at the same position intersect. An empty set is
an explicit contradiction; unknown tokens and out-of-range positions are
malformed input.

`ValidateSequenceTokenConstraints` is the shared non-mutating preflight for a
model, path length, and complete constraint array.

The generic graph helpers are:

- `IntersectSequenceTokenConstraints` for a complete positional mask;
- `IntersectSequenceLockedSpan` for an exact span at a token offset;
- `IntersectSequencePrefix` and `IntersectSequenceSuffix` for exact boundary
  spans.

They preflight every position and public token before mutation. If application
raises unexpectedly, the prior caller domains are restored. They intersect
with endpoint domains and earlier caller domains; they never replace or widen
them.

`TWfcTextCompletionRequest` adds text-oriented sugar:

- `TokenLength` fixes the generated path length;
- `Extent`, `Seed`, `Tokenizer`, and `SolveOptions` define replay behavior;
- `Prefix` and `Suffix` are exact token spans;
- `LockedSpans` retain exact interior islands;
- `Domains` allow one or more tokens at arbitrary positions.

`BuildWfcTextCompletionConstraints` expands those fields in stable order.
Overlapping exact spans and masks are legal when they agree and become a
deterministic unsatisfiable request when their intersection is empty.

## exact domain analysis

`AnalyzeSequenceTokenDomains` performs forward and backward reachability over
the public latent-state indices. A state appears in a position domain only when
it lies on at least one complete path satisfying all locks and the selected
extent. Wrapped analysis additionally proves a compatible last-to-first edge;
it does not confuse bounded training with wrapped evidence.

Each `TWfcSequencePositionDomain` contains:

- feasible latent `StateIndices` in model order;
- projected public `Tokens` in first-seen vocabulary order;
- one aggregate raw observation weight per token.

The weights rank retained learned evidence; they do not add smoothing,
semantic similarity, or unseen neighborhoods. Unsatisfiable analysis returns
`False` with a stable empty-domain, no-path, or no-cycle issue. Malformed input
raises before analysis.

This exact domain surface is intentionally independent of the solver and its
private `@wfcs` graph keys. A CLI or editor can show why a position has one,
several, or no alternatives before collapse.

## completion and validation

`TryCompleteWfcText` follows one explicit pipeline:

1. expand and validate all request constraints;
2. analyze global feasibility;
3. shape an extent-matching sequence graph and intersect the same constraints;
4. solve with the requested seed and solve options;
5. capture public tokens and latent proof indices;
6. detokenize and independently validate the completed text.

Semantic impossibility returns `False` and `wctcsUnsatisfiable` without running
the solver. A solver limit, capture problem, or independent validation failure
has a distinct status. No invalid-state callback invents fallback text.

`ValidateWfcTextCompletion` checks the extent and boundary, every latent
transition, exact state-to-token projection, every expanded lock/domain,
detokenized text identity, and tokenize-detokenize round-trip. The latent state
indices remain in `TWfcTextCompletion.Generated` because the same public token
may be emitted by more than one history-bearing state.

## deterministic replay

Exact replay includes the ordered raw documents, tokenizer and tokenizer
version, sequence order and model versions, output length and extent, every
expanded positional constraint, graph/solver/random versions, solve options,
and seed. Domain ordering comes from immutable state and vocabulary order, not
hash-table iteration or host collation.

The standard fixture uses two project-authored MIT-licensed sentences:

```text
the quick fox rests.
the quiet owl rests.
```

With order 3, prefix `the qui`, a locked separator, suffix ` rests.`, and the
position-7 domain `e | c`, seed zero completes:

```text
the quick fox rests.
```

The same source and request run on native FPC and pas2js/Node in
[`02_ConstraintCompletion`](../examples/text/02_ConstraintCompletion/README.md).

## pass composition

Text models remain ordinary latent sequence passes. The generic
`RequireSequenceProjectionFromTokenPass`,
`RequireProjectedSequenceFromPass`, and complete projection-map APIs can
connect templates, lexical classes, punctuation classes, or other ordered
models through the dependency DAG.

`TWfcTextPassPipeline` supplies one standard three-owner composition:

1. `structure` generates the abstract slot sequence;
2. `lexical` requires a compatible structure token at every position; and
3. `punctuation` requires both a compatible lexical token and a compatible
   structure token at every position, then owns the visible surface.

All three are overlay passes with stable indices `0`, `1`, and `2`. The three
`TWfcSequenceModel` objects remain caller-owned and must outlive the pipeline.
The pipeline deep-copies the caller's complete projection-rule arrays and owns
the configured graph. `TryGenerate` solves all three atomically.
`TryRegenerateFrom` reuses unaffected committed providers and regenerates the
selected dependency closure. A failed solve returns structured evidence
without partially committing an earlier pass.

Every owner-level constraint edit marks its layer as dirty. If a caller then
requests regeneration from a later layer, the owner automatically widens the
root to the earliest edited provider. Failed attempts retain that dirty root;
only a successful solve covering it permits later calls to reuse the pass.

Public constraints can target any layer by position through
`IntersectAllowedTokens`, `IntersectTokenConstraints`, and
`IntersectLockedSpan`. `ClearAllowedTokens` removes caller narrowing while
restoring the endpoint/domain restrictions installed by the sequence adapter;
it does not widen a whole, prefix, suffix, fragment, or wrapped model beyond
its configured extent.

The surface layer emits versioned `@wfctf1:` tokens. Each token canonically
encodes one exact UTF-8 fragment, including leading whitespace or an empty
fragment. `RenderWfcTextPassFragments` validates and concatenates those
fragments without host formatting rules. Construction rejects a punctuation
model unless every public token is a canonical fragment, so rendering cannot
discover an unversioned surface vocabulary only after a successful solve.

After solving, the owner captures all three public-token/state-index paths and
checks them independently against their immutable models, extents, current
caller domains/locks, copied maps, and rendered text. Each layer validates its
model/graph identity once, then checks the complete caller-domain path without
repeating that adapter proof per token. When causal tracing is
enabled, it first validates the core trace, then publishes text-domain events
containing public tokens and numeric state identities. Private model-qualified
`@wfcs` graph keys are
removed from the returned solve report and never become text-domain output.

The portable
[`03_PassComposition`](../examples/text/03_PassComposition/README.md) fixture
runs the same owner on native FPC, pas2js/Node, and an interactive pas2js
browser workbench. The browser can replay seeds, inspect the three aligned
public lineages, apply locks, display contradictions, and run an exact
headless self-test without a JavaScript framework or runtime network service.

## scope and next work

This pipeline is deliberately an acyclic, one-way staged cascade. Downstream
requirements filter a pass using already staged providers. If punctuation
cannot solve, the transaction rolls back; v1 does not negotiate backward and
reopen structure or lexical decisions inside that solve. Bounded inter-pass
repair/negotiation remains an explicit research target.

`AnalyzeSequenceTokenDomains` is exact for one constrained sequence model. It
does not currently compute globally feasible alternatives across all three
models, so the browser does not label its vocabulary list as a global
cross-pass domain.

The current dense state relation and exact domain analysis favor bounded,
inspectable documents and constrained fields. Large corpora will need a
project-owned sparse sequence index/adapter before they become a standard
runtime path. Project-owned word-boundary data, arbitrary-corpus tools,
smoothing/soft objectives, provenance-bearing text artifacts, variable-length
editing, and live causal rejection explanations remain open. They will not be
delegated to required third-party runtime libraries; see the
[dependency policy](dependencies.md).
