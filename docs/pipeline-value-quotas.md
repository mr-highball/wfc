# Portable whole-pass quotas

A saved pipeline can require exact or bounded output quantities without
embedding Pascal callbacks. A housing pass may require six houses while its
existing terrain requirements exclude water; a public music-token pass can
bound rests or attacks while retaining its sequence model.

This opt-in feature is `WFC_PIPELINE_VALUE_QUOTA_VERSION = 1`, implemented in
project-owned Pascal for native FPC and pas2js. It uses the same
[whole-pass solver](value-quotas.md) as the fluent graph API.

## Authoring

`TWfcPipelineValueQuota` contains `PassIndex`, `LabelText`, `Values`,
`MinimumCount`, and `MaximumCount`. `Values` is a nonempty set in the owning
public pass's vocabulary order. Every `(PassIndex, LabelText)` pair is unique;
descriptor order is retained. Different quotas may overlap and are all hard
constraints. Each physical cell counts once, including locks and wrapped cells.

The constructor overload with a final `TWfcPipelineValueQuotas` argument
copies the descriptors and their token arrays. Existing constructors create
quota-free recipes. `ValueQuotaAt`, `CopyValueQuotas`, and `ValueQuotaCount`
provide detached inspection. `ValueQuotaVersion` is zero for no quotas and
one otherwise. An existing recipe can be extended without changing its other
fields (the example assumes public pass 1 registers `house`):

```pascal
SetLength(Quotas, 1);
Quotas[0] := MakeWfcPipelineValueQuota(1, 'six-houses', ['house'], 6, 6);
Recipe := TWfcPipelineModel.Create(Base.CopyMetadata, Base.CopyVersions,
  Base.Rank, Base.WrapNeighbors, Base.RunMode, Base.CopyResources,
  Base.CopyPasses, Base.CopyDependencies, Base.CopyBridges,
  Base.CopyRequirements, Quotas);
```

Bounds are exact integers satisfying `0 <= minimum <= maximum <= High(Integer)`.
The recipe has no grid shape: an oversized minimum is valid to declare and
makes a smaller invocation unsatisfiable. A maximum larger than the grid is
harmless. No percentage rounding, automatic bounds adjustment, weighted sums,
or frequency inference is performed.

## Public semantics, private search

Quotas name only public tokens, never private pattern keys or sequence states.
The compiler resolves exact-copy transform chains to a materialized public
source. It does not register a local definition on a transform alias.

For a public projection, it also installs an equivalent quota on its private
source **before that source is solved**:

- Sequence projection accepts every state whose emitted token belongs to the
  public set. Multiple histories emitting the same token are alternatives,
  not multiple cell contributions.
- Wrapped 2D pattern projection accepts patterns whose `(0, 0)` anchor palette
  token belongs to the set. It does not count every overlapping footprint.
- Multiple public owners or aliases sharing one source remain conjunctive.
  Internal labels include the immutable recipe quota ordinal, preventing
  collisions between identically named quotas on different owners.

This global lowering is part of the quota contract and works with bridge
versions 1 and 2. It does not change bridge-version-1's forward-only handling
of separate per-cell run locks and domains.

An empty inverse set with minimum zero imposes no additional source constraint.
A positive minimum with no matching source state is unsatisfiable, represented
as an impossible whole-source quota instead of an illegal empty accepted set.
This is defensive handling: the current bridge adapters already reject missing
public-token representations before reaching quota installation.

The compiler's rollback-capable commit hook independently recounts the
immutable recipe against the **declared public owner's entries**. Clearing or
replacing a compiled graph quota cannot bypass this check. Empty or unknown
public entries are rejected even if the quota minimum is zero. Failure restores
the transaction's entries and random state. Selective regeneration still needs
the correct upstream scope; quotas do not enlarge that scope implicitly.

Solved result construction and decoding separately recount declared quotas.
This closes a quota-specific forged-output path; result decoding is not a
general independent proof of every adjacency or cross-pass clause.

## Canonical text and compatibility

Quota-bearing recipes encode as `wfcpipeline=2`. Following the requirement
records, immediately before the signature, the ordered section is:

```text
value-quota-version=1
value-quotas=1
value-quota=0,1,six-houses,6,6,1
quota-token=0,0,house
```

This is an excerpt, not a complete artifact. The encoder supplies its complete
resource/pass context and signature. Labels and tokens use the existing
canonical UTF-8 percent escaping. Count, index, and token order must be exact.
Unknown versions, repeated `(pass, label)` keys, noncanonical numbers, trailing records,
and version-2 documents with no quotas are rejected.

Quota-free recipes keep exact `wfcpipeline=1` bytes and signatures, including
the existing version record and seeded solver behavior. Only nonempty quota
registries append their versioned domain and ordered descriptors to semantic
identity. Run and result envelopes remain version 1 and bind the new recipe
signature. `wfc-validate` reports the actual recipe version; `wfc-run` executes
both recipe versions without an additional option or dependency.

Core failures use `gckValueQuota`; canonical result text spells this
`value-quota`. Its pass identifies where search failed, which may be the
private source after lowering. The existing result failure envelope does not
carry quota ordinals. Native compiler commit diagnostics additionally expose
`wpcvkValueQuota`, the declared public `PassIndex`, and `ValueQuotaIndex`.
A whole-pass bound failure uses `EntryIndex = -1`, not a fabricated coordinate.

## Resource envelope

The version-2 extension permits 4,096 quota descriptors, 1,024 tokens per quota,
and 65,536 aggregate quota tokens. Labels and values share the existing 16 MiB
outer-token budget, including the existing 1 MiB per-token ceiling. The entire
text remains limited to 256 MiB; the version-2 line envelope is 156,252 lines.
Version-1 keeps its original 86,618-line limit. The reader preflights version,
counts, remaining lines, and byte budgets before large allocations.

Compilation preflights an aggregate limit of 16,777,216 latent candidate visits
for quota lowering before allocating the graph. These format/compilation
limits do not promise a bound on solver elapsed time. Finite graph memory,
search allowances, and the existing run/result envelopes still apply.

Training documents currently learn resources and build quota-free recipes.
Hard quotas are authored explicitly on those recipes; observed corpus
frequencies remain weights, not mandatory output proportions.
