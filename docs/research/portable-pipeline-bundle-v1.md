# Portable Pipeline Bundle v1

Status: implementation contract; immutable rule/recipe IR and codecs complete,
runtime compilation, run/result artifacts, and tools pending

## Purpose

WFC already has portable semantic artifacts for learned adjacency models
(`wfcm`), overlapping 2D patterns (`wfcp`), and bounded sequence models
(`wfcs`). Portable Pipeline Bundle v1 also introduces `wfcrules=1` for the
hand-authored pass-local rule surface that a learned model does not represent:
3D directions, explicit denials, and required-direction metadata. WFC also
has a transactional dependency-DAG solver, typed adapters, deterministic pass
streams, and domain-specific commit validators. What it does not yet have is a
portable description of how those pieces form one pipeline.

The implemented Portable Pipeline Bundle foundation fills the declarative part
of that gap. A `wfcpipeline=1` document is an immutable, self-contained recipe
made from known WFC artifact and adapter versions, and it can already be
strictly decoded and validated by native FPC and pas2js without application
callbacks or third-party serialization libraries. Runtime compilation and the
headless tools described below are the next implementation slice.

The bundle is intentionally not a dump of a live `TGraph`. A live graph may
contain procedure pointers, derived pass classes, owner-backed commit hooks,
private dependency roles, mutable dictionaries, and a random-stream cursor.
Writing their visible fragments would create an artifact that looked portable
while silently changing its behavior when loaded. Version 1 instead records
declarative inputs; the pending compiler will regenerate every supported
private rule from a named, versioned adapter.

## Artifact family

The planned pipeline ecosystem separates three contracts. This slice
implements only `wfcpipeline=1`; the run and result contracts remain pending:

- `wfcpipeline=1` describes an immutable pipeline recipe.
- `wfcpipeline-run=1` will describe one execution: dimensions, seed, solver
  options, locks, and domains.
- `wfcpipeline-result=1` will describe one public result and its effective replay
  metadata.

Keeping these separate means one recipe can be run at many sizes and seeds,
while a result can identify the exact recipe and run that produced it. The
first implementation slice builds the recipe IR and codec before the run and
result artifacts are added.

The implemented recipe format uses the project-owned canonical text rules in
`wfc_text_codec`: ASCII syntax, UTF-8 percent-encoded tokens, LF line endings,
one final LF, canonical integers, checked counts, and exact decode/re-encode
identity. The planned run and result formats will use the same rules.

## Bundle identity

A bundle pins every replay-relevant implementation family it uses:

- graph-model version;
- random algorithm version;
- reference-solver version;
- pass-pipeline version;
- bundle graph-adapter version;
- each typed resource format and graph-adapter version;
- each built-in bridge and validator version.

Unknown versions fail closed. A reader never guesses that a newer adapter is
equivalent.

The bundle has a portable 32-bit FNV-1a signature over its complete semantic
IR. This is a deterministic identity and corruption diagnostic, not a
cryptographic authenticity claim. Source fingerprints are opaque provenance
tokens unless their value explicitly names an algorithm.

## Metadata and provenance

The bundle records:

- a nonempty name;
- a nonempty license identifier;
- an optional source description;
- an optional source fingerprint.

Every embedded resource records:

- a unique nonempty resource id;
- its closed resource kind;
- the exact canonical resource document;
- a nonempty source description;
- a nonempty source-license identifier;
- an optional source fingerprint.

Bundled repository fixtures use `MIT` for project-authored definitions and
name the code or sample that produced the resource. No canonical artifact
contains a host path or silently resolves an external file.

## Topology contract

A recipe records a rank, wrapping policy, and plane traversal mode. Dimensions
belong to the run artifact.

- Rank is 1, 2, or 3.
- Rank 1 requires height and depth of one at execution.
- Rank 2 requires depth of one.
- Rank 3 permits all positive dimensions.
- Wrapping is shared by every pass.
- `bottom-up` and `top-down` retain their existing graph meanings.

The initial typed resources describe 1D and 2D local models. Rank 3 is retained
in the IR so empty transforms and public cross-pass offsets do not need a new
pipeline format when a canonical voxel resource is added. A pass adapter must
still reject a rank it cannot represent.

## Typed resources

Version 1 recognizes exactly these resource kinds:

| Kind | Canonical payload | Decoded owner |
| --- | --- | --- |
| `rules` | `wfcrules=1` | `TWfcRuleModel` |
| `model` | `wfcm=1` or `wfcm=2` | `TWfcModel` |
| `pattern2d` | `wfcp=1` | `TWfcOverlappingModel2D` |
| `sequence` | `wfcs=1` | `TWfcSequenceModel` |

The bundle stores the complete payload as one percent-encoded token. During
construction, the payload is decoded with its existing strict reader and
encoded again. Any mismatch is noncanonical and rejected. Decoded resource
objects are owned by the immutable bundle; borrowed accessors never transfer
ownership.

Resource counts, individual payload lengths, aggregate payload length,
aggregate typed-resource relation slots, requirement terms, allowed-token
records, outer-token bytes, encoded document length, and canonical line count
have fixed version-1 ceilings. Outer collection and envelope checks precede
their large allocations. Each nested decoder applies its own limits before
dense allocation; the recipe accounts the decoded resource's relation slots
before accepting it and continuing to the next resource. The exact public
constants are documented in
[Portable rules and pipeline recipes](../pipeline-artifacts.md#version-1-safety-limits)
and are identical on native FPC and pas2js.

## Pass definitions

Passes have stable contiguous indices and unique nonempty labels. Each pass
records:

- visibility: `private` or `public`;
- pass mode: `legacy`, `transform`, or `overlay`;
- an optional transform-source pass index;
- adapter kind;
- resource index when the adapter needs one;
- sequence extent when the adapter is `sequence`.

Version 1 adapter kinds are:

| Adapter | Resource | Meaning |
| --- | --- | --- |
| `empty` | none | Values are supplied by a built-in materializer or transform |
| `rules` | `rules` | Apply an exact pass-local graph rule model |
| `model` | `model` | Apply the complete `TWfcModel` definition |
| `pattern2d` | `pattern2d` | Apply private overlapping-pattern states |
| `sequence` | `sequence` | Apply private bounded-sequence states |

Pattern and sequence adapters are always private because their graph keys are
representation details. Public results never include those keys. A generic
rules and generic-model passes may be public or private. An empty public pass
must obtain a known public vocabulary from exactly one materializing bridge or
a statically resolvable public transform source.

A transform pass must use the empty adapter, name one source, and list that
source as a dependency. Resource-backed passes cannot use transform mode:
once a pass has a local definition, the core solves that definition and does
not perform transform copying. A non-transform pass has no transform source.
A legacy pass after pass zero must list its immediate predecessor because
sequential compatibility remains part of its definition.

## Dependencies

Dependencies are stored as a contiguous ordered list of consumer/provider
index pairs. Self-dependencies, duplicates, missing indices, and cycles are
rejected before any runtime graph exists.

Every edge implied by a transform, bridge, or token requirement must also be
present in this list. The explicit list therefore documents the complete DAG,
and the pending compiler will compare the constructed graph's dependency
surface with the recipe before solving.

The runtime compiler will use stable topological order with the existing
pass-index tie break. Resource and bridge declaration order will not change
that rule.

## Built-in materializing bridges

The implemented recipe validates closed bridge declarations and derives their
target vocabularies. The pending runtime compiler, materializer, and
tentative-commit validator will implement the following semantics. Private
representations may reach a public pass only through a closed bridge kind whose
compiler and independent validator are versioned.

### `pattern2d-projection`

The source uses the `pattern2d` adapter and the target is an empty public
overlay pass. The bridge will regenerate the complete palette and one exact
wrapped offset clause per footprint coordinate by calling the checked
overlapping-pattern projection adapter. It will never write private pattern
keys into the document.

The recipe must be rank 2 and wrapped. One bridge owns the target vocabulary.
Palette tokens using the reserved latent-key grammar, `@p` followed only by
decimal digits, are rejected through the same predicate used by the runtime
adapter. This keeps recipe validation and later materialization in lockstep.
At tentative commit, validation will capture the latent pattern grid and
verify every projected footprint contribution independently.

### `sequence-projection`

The source uses the `sequence` adapter and the target is an empty public
overlay pass. The bridge will install the source model's complete public
vocabulary with neutral weights and will make each public token require every
latent state that emits that token.

The recipe must be rank 1. At tentative commit, validation will capture and
check the latent state path, then compare every target token with the
independently projected public token.

No bridge kind accepts an arbitrary procedure name, class name, expression,
or host function. Unknown kinds fail closed.

## Public token requirements

Ordinary public passes compose through declarative hard requirements. One
requirement identifies:

- a public consumer pass and consumer token;
- a public provider pass;
- `exact` or `any` clause kind;
- one or more signed-offset terms;
- a nonempty ordered set of allowed provider tokens for every term.

An `exact` requirement contains one term and will compile to
`RequireFromPassAt`. An `any` requirement contains one or more terms and will
compile to one `RequireAnyFromPass` clause. Terms in an `any` clause are in
strict signed X/Y/Z order, matching the core's canonical term order. Distinct
requirements are conjunctive. A definitionless transform can expose a static
vocabulary but cannot consume a requirement because it has no rule group;
version 1 rejects that declaration instead of changing transform semantics.

Tokens are resolved against the statically known public vocabulary of both
passes. Private passes cannot appear in these records. Duplicate exact
consumer/provider/offset records are rejected because the core would merge
them as alternatives and erase the apparent conjunction.

Offsets on inactive axes must be zero: rank 1 requires zero Y and Z, while
rank 2 requires zero Z. This prevents wrapped topologies from giving the same
neighbor multiple spellings and rejects structurally impossible open-axis
requirements before graph allocation.

Pattern and sequence projections are expressed by their typed bridges, never
by spelling latent keys in a token requirement.

## Construction transaction

The immutable recipe performs complete static validation before it can be
observed. Runtime compilation will then use a fresh graph owned by a pipeline
runtime:

1. validate the requested dimensions and run inputs;
2. decode or obtain every typed resource;
3. validate all pass/resource, bridge, vocabulary, dependency, and topology
   relationships without a graph;
4. create every pass and assign its stable label;
5. clear compatibility dependencies where the declared mode requires it;
6. install the exact declared DAG and transform sources;
7. apply typed pass adapters;
8. apply materializing bridges;
9. apply public token requirements;
10. apply caller locks and domains from the run artifact;
11. compare the resulting public definition with the recipe;
12. expose the runtime only after all steps succeed.

Any exception will free the unpublished graph and decoded temporary state.
There will be no partially configured caller-owned target.

The runtime graph will be a closed project class whose tentative-commit hook
calls only validators named by the recipe. The immutable bundle will outlive
its runtime; the runtime will own the graph and any pending result capture.

## Run artifact

`wfcpipeline-run=1` will record the invocation rather than changing the
recipe:

- recipe signature;
- positive width, height, and depth compatible with recipe rank;
- unsigned 32-bit seed;
- solve strategy: `one-way` or `negotiated`;
- per-pass local backtrack limit;
- outer pass-backtrack limit;
- trace-capture flag;
- ordered public cell locks;
- ordered public cell allowed-token domains, including an explicit empty
  domain.

Inputs use coordinates and public tokens. Inputs cannot target a private pass.
Pattern selection is constrained through its public projection. Sequence
selection is constrained through its public projection or a future typed
sequence-input record, never through a private state key.

The run manifest and CLI override policy must be explicit. A command must not
quietly prefer an argument over an artifact field.

## Commit validation

Once implemented, pipeline solving will remain
`prepare -> solve -> validate -> commit`. The runtime's commit validator will
check, in stable order:

1. every rules or generic-model pass against its immutable local model;
2. every sequence state path and endpoint/extent rule;
3. every pattern overlap;
4. every typed projection bridge;
5. every public token requirement;
6. every public lock and domain;
7. completeness of every public pass.

The first failure will report its pass and cell through `gckFinalValidation`
while the core can still restore all entries and random streams. Validation
will not be performed after publishing a result.

## Public result artifact

The planned `wfcpipeline-result=1` will contain only public layers. It will
record:

- recipe and run signatures;
- effective algorithm versions, shape, seed, and solve options;
- solve status and stable counters;
- trace or negotiation transcript signature when captured;
- every public pass label and row-major token grid;
- a signature over the complete public result.

Private passes will be represented only by their existence and validator
outcome, not by values. A result encoder will scan public tokens against every
applicable private-key grammar and fail if a private representation escaped.

Failed runs will produce a diagnostic result variant with the structured
contradiction and no partial public layers. A failure will never masquerade as
an empty successful result.

## Planned headless tools

The next implementation slice will add two thin hosts sharing portable Pascal
application logic:

- `wfc-validate` will strictly decode a recipe or run artifact, check resource
  canonicality, references, topology compatibility, the dependency DAG,
  vocabularies, zero-support rows, impossible static inputs, provenance, and
  signatures, then exit with a documented status code.
- `wfc-run` will load a recipe and run artifact, construct a fresh runtime,
  execute it, and write one canonical result artifact to standard output.

Native and pas2js/Node hosts will own only argument, file, standard-input, and
standard-output plumbing. They will not reimplement model or validation rules.
No JSON, YAML, CLI framework, hashing package, filesystem abstraction, or
serializer dependency will be introduced.

## Portable proof

The planned first end-to-end fixture will package the LearnedPatternWorld
recipe:

```text
private patterns -> public terrain -> public foliage
                             `-----> public structure
```

The pattern resource will be the canonical learned `wfcp=1` artifact. Terrain
will be materialized by `pattern2d-projection`; foliage and structure will use
small canonical `wfcrules=1` resources plus public token requirements. The
bundle will record the sample source and MIT license.

The proof must establish:

- programmatic and decoded recipes have the same semantic signature;
- native FPC stable, native FPC development, and pas2js/Node encode identical
  recipe, run, and result bytes;
- seed zero retains the existing model and layer goldens;
- a corrupted resource, unknown version, cycle, private-pass input, missing
  dependency, malformed token, and signature mismatch are rejected;
- a typed commit-validation failure rolls back entries and random streams;
- no public result contains a private pattern or sequence key.

## Deliberate nonclaims

Version 1 does not serialize:

- arbitrary `TGraph` instances;
- procedure or method callbacks;
- derived graph, pass, entry, or rule-group classes;
- arbitrary commit hooks or validators;
- host object identities or random-stream cursor state;
- external resource paths or network locators that are fetched implicitly;
- voxel, building-kit, MIDI, score, text-tokenizer, or UI adapters without a
  canonical typed resource contract;
- soft constraints, cyclic fixed points, conflict-directed search, or
  arbitrary predicate expressions.

Those capabilities require new named resource, adapter, bridge, validator, or
run-artifact versions. They are not approximated with unversioned strings.

## Research questions

The implemented recipe foundation creates a stable experimental boundary for
questions that are difficult to compare in application source alone:

- How much smaller is typed pattern projection than a flattened public rule
  expansion as footprint and palette grow?
- Does separating recipe and run identity make selective regeneration and
  result caching compositional?
- Can static public-vocabulary analysis reject contradictions before graph
  allocation without changing runtime semantics?
- Which validator evidence is sufficient to safely omit private layers from a
  public result?
- Can future voxel and musical resource kinds share the same pass and result
  contracts without adding domain knowledge to the solver?

The implementation and fixtures will publish the exact algorithms, limits,
and negative results rather than treating the file format as self-validating.
