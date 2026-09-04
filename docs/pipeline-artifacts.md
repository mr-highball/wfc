# Portable rules and pipeline recipes

WFC has two project-owned artifact layers for describing a pass pipeline
without serializing a live `TGraph`:

- `wfcrules=1` stores an immutable hand-authored local rule model;
- `wfcpipeline=1` stores an immutable declarative pipeline recipe containing
  typed resources, passes, dependencies, projection bridges, and public-token
  requirements.

Both formats are implemented once in portable Pascal and use the same source
on native FPC and pas2js. They need no JSON, YAML, reflection, serializer,
hashing, or schema package.

This is the recipe foundation. It validates and exchanges complete pipeline
definitions, but it does not yet construct or run a graph. The separately
versioned run and result artifacts, runtime compiler, and headless tools remain
the next layer.

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

| Artifact | Version-1 limits |
| --- | --- |
| authored rules | 1,024 values; 6,144 rows; 65,536 aggregate targets; 16 MiB encoded text; 262,144 lines |
| learned model | 65,536 samples; 4,194,304 per dimension, per-sample cells, and aggregate corpus cells; 1,024 values; 4,194,304 relation slots; 16 MiB encoded text; 262,144 lines |
| overlapping 2D pattern | 65,536 sources; 4,194,304 per source dimension, per-source cells, and aggregate source cells; 4,096 per footprint dimension and footprint cells; 4,096 palette tokens; 1,024 patterns; 4,194,304 aggregate pattern cells and relation slots; 16 MiB encoded text; 262,144 lines |
| bounded sequence | order 1,024; 4,096 samples; 1,024 public tokens; 1,024 states; 65,536 aggregate history items; 16 MiB encoded text; 262,144 lines |
| pipeline recipe | 64 resources; 16 MiB per payload and 64 MiB aggregate payload; 256 passes; 4,096 dependencies; 256 bridges; 4,096 requirements; 256 terms per requirement and 8,192 aggregate terms; 1,024 allowed tokens per term and 65,536 aggregate allowed-token records; 1 MiB per encoded outer token and 16 MiB aggregate encoded outer-token text; 16,777,216 aggregate typed-resource relation slots; 256 MiB encoded text; 82,522 lines |

Dimensions share the listed cell budgets and are checked before
multiplication. Collection and text-envelope checks happen before their large
outer allocations. Each typed resource decoder applies its own limits before
dense allocation; the recipe then accounts its relation slots before accepting
the resource and proceeding to the next one.

## Pass visibility and bridges

Rule and generic-model passes may be public or private. Pattern and sequence
passes must be private because their graph keys are representation details.
They can reach a public layer only through one of the closed version-1 bridge
kinds:

- `pattern2d-projection` declares a wrapped rank-2 palette projection;
- `sequence-projection` declares the emitted-token projection of a rank-1
  sequence.

Pattern projection rejects palette tokens in the reserved private-key form
`@p` followed only by decimal digits. The recipe and runtime adapter call the
same project-owned predicate, so a recipe cannot validate and later fail only
because its public vocabulary collides with latent graph keys.

The current IR validates bridge topology, endpoints, dependency and vocabulary
ownership, and derives target vocabularies. Graph materialization and
tentative-commit validation remain part of the pending runtime compiler.

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

## Canonical recipe text

`wfc_pipeline_text` encodes and decodes `wfcpipeline=1`. Its line-oriented
format has fixed field order and contiguous indexed records for resources,
passes, dependencies, bridges, requirements, terms, and allowed tokens.
Embedded documents are carried as one canonical percent-encoded field.

Readers require ASCII syntax, uppercase percent escapes, canonical signed and
unsigned decimal spelling, LF line endings, exactly one final LF, a matching
semantic signature, an exact end marker, and no trailing data. Decoding ends
with a byte-for-byte re-encode check.

The immutable owner deep-copies all caller arrays, including nested terms and
token lists. Its copy accessors return detached data. Typed resource accessors
are explicitly borrowed: the decoded resource objects remain owned by, and
must not outlive, their pipeline model.

## Scope and next layer

A recipe is deliberately not a snapshot of a live graph. Procedure pointers,
derived class state, commit hooks, dependency-role internals, mutable domains,
and random-stream cursor state cannot be reconstructed faithfully from public
graph inspection.

The full design for compiling recipes, separating execution inputs, capturing
public results, and building `wfc-validate` and `wfc-run` is recorded in the
[Portable Pipeline Bundle v1 research contract](research/portable-pipeline-bundle-v1.md).
Until those pieces land, applications should treat `wfcpipeline=1` as a
validated immutable recipe and continue constructing executable owners with
the existing typed APIs.
