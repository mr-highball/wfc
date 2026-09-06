# Portable connectivity v1: representation and verification record

This is an implementation record for persisting rooted port connectivity and
lowering it across the existing public/private representation boundary. It
does not propose a new connectivity propagator or claim a speed advantage.
The [core record](rooted-connectivity-v1.md) describes the search algorithm.
The [portable contract](../pipeline-connectivity.md) specifies the API and
canonical format.

## Hypothesis and scope

A public connectivity descriptor can constrain a private learned assignment
without exposing private state keys when the existing projection gives exactly
one public token at each private cell coordinate. Persisting only those public
profiles must reproduce the same accepted connectivity relation after decoding.

The supported maps are sequence emission, wrapped Pattern2D anchor projection,
and exact-copy public aliases. Arbitrary resampling, many-public-cells-per-private
cell, or connectivity across several independent passes is outside this result.

## Equivalence argument

Let `f(s)` be the public token emitted by private state `s`. Assign `s` the
participation, opening ports, and required flag of `f(s)`; if `f(s)` has no
profile, `s` is a nonparticipant. Keep root, terminals, coordinates, wrapping,
and the all-participants flag unchanged.

For a complete legal private assignment, every coordinate therefore has the
same participation and ports as its public projection. Private local adjacency
holds for every selected neighboring state pair because the underlying model
has already been satisfied. Public projection passes have neutral local
adjacency. Reciprocal-port edges consequently coincide on that legal assignment,
as do root membership, terminal membership, mandatory values, and reachability.
Exact-copy aliases preserve the same assignment and relation.

For patterns, `f` uses the anchor `(0,0)` of each private cell. Existing exact
overlap clauses establish consistency with the full projected footprint;
counting or joining every footprint occurrence would be a different relation.
Different sequence histories that emit one token remain separate alternatives,
not extra vertices. Root/terminal positions are never remapped on reshape.

On unresolved domains the private adjacency graph may prune earlier than the
neutral public projection graph. That is sound: a complete legal source
assignment must satisfy its own adjacency as well as the public connectivity
relation. This observation is not a generalized arc-consistency claim.

## Independent boundary checks

The compiler installs private search constraints and public materialized
constraints. Publication additionally traverses public entries against the
immutable recipe, so editing the mutable graph registry cannot erase policy.
Immutable solved-result construction repeats that public check without using
the solver or its reachability state. Direct rule/model owners use immutable
local adjacency, including both reciprocal directions; projection owners use
their actual neutral local adjacency.

Public result verification intentionally remains connectivity-specific.
Without latent assignments it does not establish all learned projection
constraints in an arbitrary forged result. No general proof-carrying-artifact
claim follows from this work.

## Reproducible fixtures and stopping rule

The ordinary native and pas2js gates include:

- `wfc_pipeline_connectivity_model_test`: immutable ownership, all descriptor
  fields in semantic identity, public vocabulary order, rank/canonicality,
  shared resource budgets, and unchanged empty-feature/legacy identities.
- `wfc_pipeline_connectivity_text_test`: exact complete documents, UTF-8,
  every byte truncation of a fixture, malformed fields, per-version line
  budgets, aggregate token/terminal limits, and exact-boundary acceptance.
- `wfc_pipeline_connectivity_test`: direct rules and learned models, both
  bridge versions, pattern/sequence lowering, aliases, quotas, wrapped aliases,
  optional versus required islands, full XYZ ports, corruption rejection,
  repair scopes, work limits, shared CLI execution, and result replay.
- Connected Routes native `--portable-selftest` and the actual browser
  `?selftest=1` entry: a saved quota-plus-connectivity route fixture reproduces
  complete public output through recipe/run/result decoding and execution.

All fixtures are finite and project-authored. Seeds, shapes, budgets, and
expected assignments are fixed in the test sources. Any unexpected exception,
different replay, invalid accepted output, or exhausted allowance in a fixture
that asserts success is a test failure; failures are not discarded or replaced
by another seed. Native FPC and pas2js must agree on canonical identities.

The compatibility baseline is the existing quota-free and quota-only fixture
set, whose canonical bytes, signatures, and successful results remain unchanged.
The codec fixture pins legacy `B16949C8` and connectivity `597320BF`; the
quota-plus-connectivity model fixture pins `D72EB9EE`. The demonstration recipe
pins `DB5C9A56` and independently checks both published layers geometrically.

The result is a portable contract and finite conformance evidence, not a
benchmark comparison. Stronger port filtering, automatic profile inference,
regional or chunked connectivity, and relative runtime/storage measurements
remain open. No novelty claim is made for reachability or persistence itself.
