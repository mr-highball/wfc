# Portable Pipeline Bundle v1

Execution note: pas2js figures below are historical measurements. Maintained
execution paths are native FPC and the documented browser demos; those earlier
figures do not establish browser coverage for this experiment.

Status: implemented contract; immutable recipe, run, and result IR; strict
canonical codecs; transactional runtime compiler; native tools and historical pas2js executions

## Purpose

WFC already has portable semantic artifacts for learned adjacency models
(`wfcm`), overlapping 2D patterns (`wfcp`), and bounded sequence models
(`wfcs`). Portable Pipeline Bundle v1 also introduces `wfcrules=1` for the
hand-authored pass-local rule surface that a learned model does not represent:
3D directions, explicit denials, and required-direction metadata. WFC also
has a transactional dependency-DAG solver, typed adapters, deterministic pass
streams, and domain-specific commit validators. Portable Pipeline Bundle v1
gives those pieces one closed, replayable execution boundary.

The implemented bundle fills both the declarative and execution parts of that
gap. A `wfcpipeline=1` document is an immutable, self-contained recipe
made from known WFC artifact and adapter versions, and it can already be
strictly decoded and validated by native FPC and pas2js without application
callbacks or third-party serialization libraries. A recipe can be compiled
into a fresh graph, combined with a canonical run document, and executed into
a canonical public result by either native FPC or historical pas2js runs.

The bundle is intentionally not a dump of a live `TGraph`. A live graph may
contain procedure pointers, derived pass classes, owner-backed commit hooks,
private dependency roles, mutable dictionaries, and a random-stream cursor.
Writing their visible fragments would create an artifact that looked portable
while silently changing its behavior when loaded. Version 1 instead records
declarative inputs; the compiler regenerates every supported private rule from
a named, versioned adapter.

## Artifact family

The pipeline ecosystem separates three implemented contracts:

- `wfcpipeline=1` describes an immutable pipeline recipe.
- `wfcpipeline-run=1` describes one execution: dimensions, seed, solver
  options, locks, and domains.
- `wfcpipeline-result=1` describes one public result and its effective replay
  metadata.

Keeping these separate means one recipe can be run at many sizes and seeds,
while a result identifies the exact recipe and run that produced it.

The implemented recipe format uses the project-owned canonical text rules in
`wfc_text_codec`: ASCII syntax, UTF-8 percent-encoded tokens, LF line endings,
one final LF, canonical integers, checked counts, and exact decode/re-encode
identity. The run and result formats use the same rules.

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
[Portable pipeline artifacts](../pipeline-artifacts.md#version-1-safety-limits)
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
and the compiler compares the constructed graph's dependency
surface with the recipe before solving.

The runtime compiler uses stable topological order with the existing
pass-index tie break. Resource and bridge declaration order does not change
that rule.

## Built-in materializing bridges

The recipe validates closed bridge declarations and derives their target
vocabularies. The runtime compiler, materializer, and tentative-commit
validator implement the following semantics. Private
representations may reach a public pass only through a closed bridge kind whose
compiler and independent validator are versioned.

### `pattern2d-projection`

The source uses the `pattern2d` adapter and the target is an empty public
overlay pass. The bridge regenerates the complete palette and one exact
wrapped offset clause per footprint coordinate by calling the checked
overlapping-pattern projection adapter. It never writes private pattern
keys into the document.

The recipe must be rank 2 and wrapped. One bridge owns the target vocabulary.
Palette tokens using the reserved latent-key grammar, `@p` followed only by
decimal digits, are rejected through the same predicate used by the runtime
adapter. This keeps recipe validation and later materialization in lockstep.
At tentative commit, validation captures the latent pattern grid and
verifies every projected footprint contribution independently.

### `sequence-projection`

The source uses the `sequence` adapter and the target is an empty public
overlay pass. The bridge installs the source model's complete public
vocabulary with neutral weights and makes each public token require every
latent state that emits that token.

The recipe must be rank 1. At tentative commit, validation captures and
checks the latent state path, then compares every target token with the
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

An `exact` requirement contains one term and compiles to
`RequireFromPassAt`. An `any` requirement contains one or more terms and
compiles to one `RequireAnyFromPass` clause. Terms in an `any` clause are in
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
observed. Runtime compilation then uses a fresh graph owned by a pipeline
runtime:

1. validate the requested dimensions and run inputs;
2. decode or obtain every typed resource;
3. validate all pass/resource, bridge, vocabulary, dependency, and topology
   relationships without a graph;
4. resolve public transform aliases, coalesce equal locks, reject conflicting
   locks, and intersect domains in public-vocabulary order;
5. preflight and derive bridge-version-2 private source domains without
   allocating a graph;
6. create every pass and assign its stable label;
7. clear compatibility dependencies where the declared mode requires it;
8. install the exact declared DAG and transform sources;
9. apply typed pass adapters, materializing bridges, and public requirements,
   then verify the complete compiled definition against the recipe;
10. intersect derived private domains with any adapter-installed domains;
11. apply the original effective public domains and locks;
12. select the run seed and expose the runtime only after every step succeeds.

Any exception frees the unpublished graph and temporary state. There is no
partially configured caller-owned target.

The runtime graph is a closed project class whose tentative-commit hook calls
only validators named by the recipe. The immutable recipe and run must outlive
their runtime; the runtime owns the graph and results are detached caller-owned
objects.

## Run artifact

`wfcpipeline-run=1` records the invocation rather than changing the
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

Bridge version 1 preserves forward-only projection. Current recipes use bridge
version 2, which deterministically lowers those same public locks and domains
into the matching private Pattern2D anchors or Sequence states before solving.
The Pattern2D inverse accounts for every wrapped footprint offset; the Sequence
inverse retains every duplicate-emission state and intersects with endpoint or
extent masks. Contributions from aliases and from multiple bridges sharing a
private source are globally intersected. Empty intersections remain ordinary
solve contradictions, while the original public constraints stay in place for
commit-time forward validation.

A lock or domain on a definitionless public transform is resolved to the final
materialized public source. Alias domains at the same effective cell are
intersected in canonical vocabulary-index order; equal locks coalesce, while
conflicting locks or lock/domain exclusions fail before graph publication. A
project-owned open-addressed token lookup keeps this resolution bounded without
introducing a collection dependency.

The command-line runner has no solver-option overrides: every replay field
comes from the run artifact. Its only execution option is output suppression.

## Commit validation

Pipeline solving remains `prepare -> solve -> validate -> commit`. The core
solver first enforces local rules, generic models, caller domains, and
completeness. Before publication, the compiler-owned commit hook then checks,
in stable order:

1. every private pattern assignment and overlap;
2. every private sequence path and endpoint/extent rule;
3. every definitionless transform against its exact source copy;
4. every typed projection bridge;
5. every public token requirement.

The first failure reports its pass and cell through `gckFinalValidation`
while the core can still restore all entries and random streams. Validation
is not deferred until after publishing a result.

## Public result artifact

`wfcpipeline-result=1` contains only public layers. It records:

- recipe and run signatures;
- effective algorithm versions, shape, seed, and solve options;
- solve status and stable counters;
- trace or negotiation transcript signature when captured;
- every public pass label and row-major token grid;
- a signature over the complete public result.

Private passes are represented only by pass outcomes and validator success,
not by values. Capture verifies every published pass index and label against
the recipe and requires every cell token to be an exact member of that pass's
public vocabulary. Private representations therefore have no result-layer
route; typed bridge validators separately protect their projection boundary.

Failed runs produce a diagnostic result variant with the structured
contradiction and no partial public layers. A failure never masquerades as
an empty successful result.

## Headless tools

Two implemented tools use shared portable Pascal application logic:

- `wfc-validate recipe [--quiet | --emit-canonical] [--] INPUT` strictly
  decodes one recipe, including nested resource canonicality, references,
  topology, dependency DAG, vocabularies, provenance, and signatures. The
  optional canonical mode writes its exact normalized recipe.
- `wfc-run [--quiet] [--] RECIPE RUN` strictly decodes the two artifacts,
  constructs a fresh runtime, executes it, and normally writes one canonical
  result artifact even when the solve is not successful. At most one input may
  be `-` for standard input.

Both tools reserve exit `0` for success, `1` for invalid artifacts or an
invalid execution boundary, `2` for command usage, `3` for I/O, and `70` for
an internal failure. `wfc-run` additionally uses `4` for a valid non-solved
result. Quiet mode changes output, not validation or solve semantics.

The native and historical pas2js hosts owned only argument, bounded file, standard-input,
standard-output, and process-exit plumbing. They did not reimplement model or
validation rules. No JSON, YAML, CLI framework, hashing package, filesystem
abstraction, or serializer dependency was introduced.

## Portable proof

The implementation is covered as a layered portable proof on stable FPC,
development FPC, and historical pas2js runs:

- compiler fixtures build empty, rules, generic-model, pattern, and sequence
  passes; install both bridge kinds and exact/any public requirements; and
  exercise independent commit failures;
- recipe, run, and result codecs round-trip exact canonical bytes with pinned
  semantic signatures and reject malformed envelopes, versions, references,
  ordering, and provenance;
- runtime fixtures exercise transform-targeted locks and domains, alias-domain
  intersection, inverse Pattern2D overlap/wrap lowering, Sequence
  duplicate-emission and endpoint intersection, shared private sources,
  version-1 forward compatibility, explicit contradictions, deterministic
  replay, real outer-pass repair, inverse-work and pass-budget exhaustion, and
  detached result lifetime;
- application fixtures execute encoded recipe-plus-run input into decoded
  canonical results and verify non-solved behavior; native and historical pas2js hosts ran a
  committed process matrix covering exact bytes, both legal standard-input
  positions, quiet output, and every ordinary exit class while the shared
  codecs retain pinned bytes and signatures on both targets.

The first domain-scale bundle fixture packages the existing
[`LearnedPatternWorld`](../../examples/2D/05_LearnedPatternWorld/README.md)
pipeline:

```text
private patterns -> public terrain -> public foliage
                             `-----> public structure
```

The pattern resource is the canonical 2,130-byte learned `wfcp=1` artifact.
Terrain is materialized by `pattern2d-projection` bridge version 2; foliage and
structure use 104-byte and 90-byte canonical `wfcrules=1` resources plus seven
exact zero-offset public token requirements. The bundle records the embedded
sample source and MIT license.

Its committed recipe, run, and result artifacts establish:

- programmatic and decoded recipes share semantic signature `DC2030BE`;
- the 8 by 6 by 1, seed-zero run uses eight public terrain locks and retains
  the existing layer hashes `EBBC9390`, `92D4BC87`, and `8FA9D854`;
- the exact result has signature `5329DB78` and contains no private pattern or
  sequence key;
- native FPC and historical pas2js runs executed the same public artifacts, while the
  native suite additionally verifies all three checked-in files byte for byte.

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

The implemented execution bundle creates a stable experimental boundary for
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

The implementation and fixtures publish the exact algorithms, limits, and
negative results rather than treating the file format as self-validating.
