# Portable pipeline artifacts

WFC has four project-owned artifact layers for describing and executing a pass
pipeline without serializing a live `TGraph`:

- `wfcrules=1` stores an immutable hand-authored local rule model;
- `wfcpipeline=1` stores an immutable declarative pipeline recipe containing
  typed resources, passes, dependencies, projection bridges, and public-token
  requirements;
- `wfcpipeline-run=1` stores one recipe-bound invocation, including shape,
  seed, solve policy, public locks, and public allowed-token domains; and
- `wfcpipeline-result=1` stores the terminal status, evidence identity, pass
  outcomes, and complete public output of that invocation.

All four formats are implemented once in portable Pascal and use the same
source on native FPC and pas2js. They need no JSON, YAML, reflection, serializer,
hashing, or schema package.

The executable path is likewise project-owned: `wfc_pipeline_compile` turns a
validated recipe into a fresh closed graph, and `wfc_pipeline_runtime` applies
one run and returns a detached immutable result. Recipe, run, compiler,
runtime, and result versions are explicit and fail closed when unsupported.

To generate a recipe from editable samples, see
[training documents and wfc-learn](training.md). That workflow preserves
source/license labels and ordered corpus fingerprints for cardinal, pattern,
and sequence resources without introducing a file-format dependency.

## Hand-authored rule models

`TWfcRuleModel` in `wfc_rule_model` represents rule semantics that the learned
`TWfcModel` does not cover directly:

- rank 1, 2, or 3;
- an ordered UTF-8 token vocabulary;
- positive integer weights;
- absent direction rows as legacy wildcards;
- explicit directional deny-all rows;
- finite directional allow rows;
- required-direction metadata.

Rows are uniquely and strictly ordered by value index and graph direction.
Targets in an allow row are uniquely and strictly ordered by value index. A
finite edge must already contain the exact reciprocal edge and reciprocal
required metadata that `TGraph.NewRule` would derive. Construction rejects a
partial closure rather than silently broadening or repairing the input.

`ApplyRuleModelToGraph` applies a completely preflighted model to the active
pass. The pass must be empty. Token conversion, weights, finite rule arrays,
explicit denials, and model identity are checked before the first mutation.
This makes failure atomic at the adapter boundary.

The strict codec is in `wfc_rule_text`. A small document has this shape:

```text
wfcrules=1
rank=1
values=2
v=0,2,A
v=1,3,B
rules=4
r=0,0,E,allow,1,1,1
r=1,0,W,deny,0,0
r=2,1,E,deny,0,0
r=3,1,W,allow,1,1,0
signature=5F6B1DDF
end
```

The direction codes are `N`, `E`, `S`, `W`, `U`, and `D`. The required flag
is exactly `0` or `1`. Tokens use the canonical UTF-8 percent encoding shared
with the existing model codecs. The eight-digit FNV-1a signature is a portable
semantic identity and corruption diagnostic, not an authenticity mechanism.

## Pipeline recipe model

`TWfcPipelineModel` in `wfc_pipeline_model` owns a fully validated recipe. A
recipe records:

- name, license identifier, and optional source metadata;
- rank, wrapping, and traversal mode;
- replay-relevant graph, random, solver, pipeline, adapter, and bridge
  versions;
- canonical embedded resources with their own source and license metadata;
- stable pass indices, labels, visibility, modes, and adapters;
- the complete acyclic dependency list;
- closed, typed projection bridges;
- exact-offset and any-of-neighborhood public-token requirements.

Version 1 recognizes four embedded resource kinds:

| Kind | Payload | Pass adapter |
| --- | --- | --- |
| `rules` | `wfcrules=1` | exact hand-authored local rules |
| `model` | `wfcm=1` or `wfcm=2` | learned local adjacency |
| `pattern2d` | `wfcp=1` | private overlapping-pattern states |
| `sequence` | `wfcs=1` | private bounded-sequence states |

Every resource is decoded with its strict existing reader and immediately
re-encoded. The recipe rejects a kind mismatch or any payload that is not
byte-canonical. Resource counts, decoded payload bytes, aggregate typed-resource
relation slots, aggregate requirement terms, aggregate allowed-token records,
and outer-token bytes have public fixed limits. The text codec also bounds
encoded document length and line count before splitting the input.

### Version-1 safety limits

These ceilings are format contract, not tuning suggestions. Changing one
incompatibly requires a new limits or artifact version.

| Artifact or boundary | Version-1 limits |
| --- | --- |
| authored rules | 1,024 values; 6,144 rows; 65,536 aggregate targets; 16 MiB encoded text; 262,144 lines |
| learned model | 65,536 samples; 4,194,304 per dimension, per-sample cells, and aggregate corpus cells; 1,024 values; 4,194,304 relation slots; 16 MiB encoded text; 262,144 lines |
| overlapping 2D pattern | 65,536 sources; 4,194,304 per source dimension, per-source cells, and aggregate source cells; 4,096 per footprint dimension and footprint cells; 4,096 palette tokens; 1,024 patterns; 4,194,304 aggregate pattern cells and relation slots; 16 MiB encoded text; 262,144 lines |
| bounded sequence | order 1,024; 4,096 samples; 1,024 public tokens; 1,024 states; 65,536 aggregate history items; 16 MiB encoded text; 262,144 lines |
| pipeline recipe | 64 resources; 16 MiB per payload and 64 MiB aggregate payload; 256 passes; 4,096 dependencies; 256 bridges; 4,096 requirements; 256 terms per requirement and 8,192 aggregate terms; 1,024 allowed tokens per term and 65,536 aggregate allowed-token records; 1 MiB per encoded outer token and 16 MiB aggregate encoded outer-token text; 16,777,216 aggregate typed-resource relation slots; 256 MiB encoded text; 82,522 lines |
| pipeline run | 4,194,304 per dimension and aggregate cells; 262,144 locks; 262,144 domains; 1,024 tokens per domain and 1,048,576 aggregate domain tokens; 1,000,000 local backtracks; 65,536 pass backtracks; 1 MiB per encoded token and 16 MiB aggregate encoded token text; 64 MiB encoded text; 1,572,878 lines |
| pipeline compiler | 4,194,304 per dimension and aggregate cells for a direct compile request |
| pipeline runtime | 16,777,216 aggregate pass cells in addition to the run and result limits; worst-case public-vocabulary encoding is preflighted before graph compilation; inverse-lowering limits version 1 permits 1,048,576 contributions, 16,777,216 candidate predicate visits, and 4,194,304 stored private indices per invocation |
| pipeline result | 256 public layers; 4,194,304 aggregate public cells; 1 MiB per encoded token and 64 MiB aggregate encoded token text; 256 MiB encoded text; 4,194,850 lines |

Dimensions share the listed cell budgets and are checked before
multiplication. Collection and text-envelope checks happen before their large
outer allocations. Each typed resource decoder applies its own limits before
dense allocation; the recipe then accounts its relation slots before accepting
the resource and proceeding to the next one. Runtime preflight multiplies the
cell count by the complete pass count and separately by the public-layer count.
It also charges every public label and the longest possible token in each
public vocabulary across that pass's entire grid. A request that could exceed
the result encoding budget is rejected before a graph is allocated, even when
one particular random outcome might have used only shorter tokens. These
pass-cell, result-cell, and encoded-byte preflights make the allocation and
output envelope independent of solver luck.

## Pass visibility and bridges

Rule and generic-model passes may be public or private. Pattern and sequence
passes must be private because their graph keys are representation details.
They can reach a public layer only through one of the closed bridge kinds:

- `pattern2d-projection` declares a wrapped rank-2 palette projection;
- `sequence-projection` declares the emitted-token projection of a rank-1
  sequence.

The two bridge-version fields are independent. Versions 1 and 2 are accepted;
newly constructed recipes select version 2. Version 1 retains the original
forward-only materialization contract. Version 2 additionally lowers public
locks and domains through its matching bridge before the private source pass
is solved. Unknown versions fail closed, and changing one bridge field never
changes the other projection kind.

This preparation behavior is the `WFC_PIPELINE_RUNTIME_VERSION = 2`
contract; the outer recipe, run, and result text envelopes remain version 1.

Pattern projection rejects palette tokens in the reserved private-key form
`@p` followed only by decimal digits. The recipe and runtime adapter call the
same project-owned predicate, so a recipe cannot validate and later fail only
because its public vocabulary collides with latent graph keys.

The IR validates bridge topology, endpoints, dependency and vocabulary
ownership, and derives target vocabularies. The compiler materializes those
bridges into a fresh graph. Its tentative-commit hook independently checks
private pattern and sequence captures, exact transform copies, both projection
kinds, and public-token requirements before the core can publish staged
entries. Rule and generic-model passes use the graph's complete local
constraint surface because those adapters do not expose a separate solved
capture validator.

A bridge source must use the matching private adapter. Its target must be an
empty public overlay pass, and the source-to-target dependency must be listed
explicitly. One bridge owns one target vocabulary. An empty public transform
may instead inherit the statically known vocabulary of a public transform
source. Transform passes always use the empty adapter: a local definition
would make the core solve that definition and leave the requested copy
semantics inert. Such a definitionless transform cannot consume a token requirement;
only a pass with a materialized rule definition can do that without changing
copy semantics.

No recipe field names an arbitrary unit, class, callback, validator,
procedure, expression, or external path. Unknown enum values and version
numbers fail closed.

## Public-token requirements

A declarative requirement relates a public consumer token to a public
provider pass. `exact` has one signed-offset term. `any` has one or more terms
in strict X/Y/Z order. Each term contains a nonempty token set in the
provider's vocabulary order.

Both endpoints and every token are resolved during recipe construction. The
dependency must already be present. Duplicate exact
consumer/provider/offset keys are rejected because the graph API would merge
their token sets as alternatives. Rank-1 offsets must have zero Y and Z;
rank-2 offsets must have zero Z. Active axes retain the complete signed
`Integer` range.

## Canonical recipe, run, and result text

`wfc_pipeline_text` encodes and decodes `wfcpipeline=1`. Its line-oriented
format has fixed field order and contiguous indexed records for resources,
passes, dependencies, bridges, requirements, terms, and allowed tokens.
Embedded documents are carried as one canonical percent-encoded field.

`wfc_pipeline_run_text` applies the same rules to `wfcpipeline-run=1`. A run
repeats the recipe signature and records its positive rank-compatible shape,
complete unsigned 32-bit seed, strategy, local and outer backtrack limits,
trace policy, ordered public locks, and ordered public allowed-token domains.
An assigned empty allowed-token array is an explicit contradictory domain; it
is not the same thing as an absent domain. Decoding requires the referenced
recipe and resolves every pass and token against its public vocabulary.

`wfc_pipeline_result_text` encodes `wfcpipeline-result=1`. A result repeats both
the recipe and run signatures, effective algorithm versions, shape, seed, and
solve options. It then records status, stable pass counters, evidence kind and
signature, structured failure fields, one terminal outcome per recipe pass,
and every public result layer. Decoding requires the exact referenced recipe
and run, checks the repeated run fields, reconstructs the immutable result,
verifies its semantic signature, and re-encodes it byte for byte.

All three readers require ASCII syntax, uppercase hexadecimal signatures and
percent escapes, canonical signed and unsigned decimal spelling, exact enum
and Boolean names, LF line endings, exactly one final LF, an exact end marker,
and no trailing data. Unknown artifact or algorithm versions fail closed. The
eight-digit FNV-1a signatures are portable semantic identities and corruption
diagnostics, not cryptographic authenticity claims.

## Recipe to result lifecycle

The normal execution path is:

1. Decode or construct one immutable `TWfcPipelineModel` recipe.
2. Decode or construct one `TWfcPipelineRun` against that recipe. The run's
   signature covers the recipe signature and every effective invocation field.
3. `TWfcPipelineRuntime` resolves transform aliases, consolidates public
   inputs, and preflights any bridge-version-2 inverse work without publishing
   a graph.
4. `CompileWfcPipeline` materializes the recipe into a fresh
   `TWfcCompiledPipeline`, including passes, the dependency DAG, typed adapters,
   projection bridges, requirements, shape, and commit validation.
5. The runtime intersects derived private domains with adapter-installed
   domains, applies the original public domains and locks, and then uses either
   ordinary or negotiated solving.
6. Execution returns a detached `TWfcPipelineResult`. The caller may encode or
   retain it after the runtime, compiled graph, run, and recipe are gone.

Compilation and execution are transactional. A compile error frees the
unpublished graph. During solving, the closed commit hook checks typed private
captures, projections, transforms, and requirements after values are staged
but while the core can still restore entries and versioned random streams. A
validation failure becomes a structured terminal result rather than a
partially published public grid.

### Ownership and borrowed views

| Object | Ownership contract |
| --- | --- |
| `TWfcPipelineModel` | Owns decoded typed resources and deep-copies recipe arrays. `Borrow*Resource` results must not outlive the model; record and array accessors return detached copies. |
| `TWfcPipelineRun` | Deep-copies locks and nested domain tokens. It keeps only the recipe signature, so the recipe need not outlive a completed run construction or decode. |
| `TWfcCompiledPipeline` | Borrows the recipe, which must outlive it. It owns the fresh graph; its `Graph` property is a borrowed view that must not be freed or retained after the compiled owner. |
| `TWfcPipelineRuntime` | Borrows both recipe and run and owns its compiled pipeline. Recipe and run must outlive the runtime. |
| `TWfcPipelineResult` | Owns detached copies of outcomes and public token grids and retains signatures rather than borrowed recipe, run, or graph objects. The caller owns the returned result. |

### Public inputs and transform aliases

Run inputs name only public passes and public vocabulary tokens. A public
definitionless transform is an alias for its materialized public source. Before
compilation, the runtime follows every transform chain to that source and
requires the alias and source vocabularies to match exactly. It then normalizes
all inputs by effective pass and row-major cell:

- identical locks coalesce, while locks selecting different tokens for the
  same effective cell reject the invocation before graph publication;
- multiple domains intersect in recipe vocabulary order, including domains
  supplied through different transform aliases;
- an empty intersection remains an explicit empty domain and produces a
  canonical contradiction result during solving; and
- a lock excluded by the effective domain rejects the invocation before graph
  publication.

This normalization prevents a lock on a definitionless transform from
silently overriding its declared copy semantics. The commit validator checks
every transform cell against its source again inside the rollback-capable
transaction.

For a version-2 Pattern2D projection, every constrained target cell contributes
one predicate for every footprint coordinate. A target `(tx, ty)` and footprint
offset `(ox, oy)` constrain the wrapped private anchor
`(tx - ox, ty - oy)`. Candidate pattern indices remain in ascending model
order and survive only when their palette index at that exact footprint
coordinate belongs to the effective public domain. Folded offsets on small
wrapped grids are separate contributions and therefore intersect even when
they name the same private cell.

For a version-2 Sequence projection, a constrained target position contributes
the ascending set of every private state whose emitted-token index is in the
effective public domain. Duplicate-emission states are all retained. The
derived set is intersected with the sequence adapter's existing start, end,
fragment, or wrapped domain; it never replaces or widens that mask.

Contributions from different inputs and different bridges are sorted by
private source pass and row-major cell, then globally intersected. This also
covers multiple public projections sharing one private source. An empty
intersection is a valid semantic contradiction installed on the graph, not a
construction error. Original public inputs remain installed as well, and the
commit validator independently verifies the forward projection. Version-1
bridges skip only the inverse-lowering step and retain their original
target-side behavior.

The lowering path uses only project-owned Pascal arrays, stable merge sort,
ordered integer-set intersection, and the existing collision-safe token
lookup. No container, serialization, or solver dependency is added; when a
portable operation is practical in FPC and pas2js, the ecosystem owns that
implementation.

### One-way and negotiated outcomes

A one-way run has no outer pass-backtrack budget. With trace capture disabled,
its result has no evidence signature; with capture enabled, it carries the
verified causal trace signature. A negotiated run uses its bounded outer
pass-backtrack budget and always carries the verified negotiation-transcript
signature, which commits to the exact rejected attempts, exclusions, terminal
attempt, solve settings, and any nested traces.

The public result stores the terminal attempt's outcome for every recipe pass.
Private passes therefore retain status and counters but never their latent
values. A solved result contains every public pass label and complete row-major
token grid in recipe order. Contradiction, solver-backtrack-limit, and
pass-backtrack-limit results contain a structured failure and no partial public
layers. A failed run can never be confused with a successful empty output.

The public `TGraph` API does not expose a semantic signature for an already
compiled graph definition. Result capture can verify the graph's shape, seed,
pass count and labels, completeness, and public vocabularies; the closed
runtime supplies the graph produced from the signed recipe instead of
inventing an unverifiable definition identity.

## Headless command contracts

The native hosts contain only bounded byte I/O and process
plumbing. Parsing, validation, compilation, execution, result construction,
and canonical encoding remain in the shared portable Pascal units.

The contracts below use distribution-facing hyphenated command names. Checked
repository builds retain their Pascal source-host basenames as
`wfc_validate[.exe]` and `wfc_run[.exe]`;
packaging may expose the hyphenated names without changing
behavior.

### `wfc-validate`

```text
wfc-validate recipe [--quiet | --emit-canonical] [--] INPUT
wfc-validate --help
wfc-validate --version
```

`INPUT` is one recipe file or `-` for standard input. The default success
output is a one-line recipe summary. `--quiet` suppresses it;
`--emit-canonical` writes the exact input after strict decode and byte-for-byte
canonical verification. The tool validates `wfcpipeline=1` syntax, signatures,
typed resources, provenance, references, topology, vocabularies, and static
limits. It does not compile or solve the recipe.

### `wfc-run`

```text
wfc-run [--quiet] [--] RECIPE RUN
wfc-run --help
wfc-run --version
```

`RECIPE` and `RUN` are canonical artifact files. Either one may be `-` for
standard input, but not both. The tool strictly decodes the recipe first,
decodes the run against it, prepares a fresh runtime, executes once, and writes
one exact canonical `wfcpipeline-result=1` document. `--quiet` suppresses that
document without changing the outcome code. A solver contradiction or either
backtrack limit still forms a valid canonical result and exits `4`; invalid
recipe, run, transform-alias inputs, compile request, or runtime preflight does
not masquerade as a solver result.

| Exit | `wfc-validate` | `wfc-run` |
| ---: | --- | --- |
| `0` | valid recipe, help, or version | solved result, help, or version |
| `1` | invalid recipe artifact | invalid recipe, run, or executable invocation |
| `2` | command-line usage error | command-line usage error |
| `3` | input/output failure | input/output failure |
| `4` | reserved; recipe validation does not solve | valid canonical non-solved result |
| `70` | unexpected internal failure | unexpected internal failure |

Options must precede positional paths. `--` permits a path beginning with a
hyphen. Diagnostics are one line on standard error, while canonical artifacts
are written only to standard output.

The native build drivers run both command-line tools as real child
processes against the committed `test/fixtures/pipeline-cli` artifacts and the
domain-scale `examples/2D/05_LearnedPatternWorld/pipeline` bundle. The gate
compares canonical and summary output byte for byte, exercises file and
anonymous-pipe input, checks quiet, invalid-artifact, usage, I/O, solved, and
non-solved exit behavior, and replays the learned Pattern2D bridge-v2 result
with bounded captures and finite timeouts.

## Scope

A recipe is deliberately not a snapshot of a live graph. Procedure pointers,
derived class state, arbitrary commit hooks, dependency-role internals, mutable
domains, and random-stream cursor state cannot be reconstructed faithfully
from public graph inspection. No artifact field names an arbitrary unit,
class, callback, procedure, expression, or external path.

The design rationale, negative results, and format boundaries are recorded in
the [Portable Pipeline Bundle v1 research
contract](research/portable-pipeline-bundle-v1.md).
