# Finite cross-pass count constraints

For paths between arbitrary cells in one pass, use the separate
[rooted port connectivity](connectivity.md) propagator. Local counts do not
imply global reachability.

`RequireCountFromPass` adds an inclusive count range over a caller-defined
neighborhood in an earlier provider pass. It can express requirements such as
"exactly two nearby roads", "at most one flooded neighbor", or "no occupied
support cells" without combining every layer into one value vocabulary.

This opt-in feature is identified by `WFC_PASS_COUNT_VERSION = 1`. Existing
graph, solver, pipeline, negotiation, and trace versions remain unchanged.

## Fluent API

```pascal
Graph.AddValue('market')
  .RequireFromPass('terrain', 'land')
  .RequireCountFromPass('roads', [
    MakeGraphPassMatchTerm(MakeGraphOffset(-1, 0, 0), ['road']),
    MakeGraphPassMatchTerm(MakeGraphOffset(1, 0, 0), ['road']),
    MakeGraphPassMatchTerm(MakeGraphOffset(0, -1, 0), ['road']),
    MakeGraphPassMatchTerm(MakeGraphOffset(0, 1, 0), ['road'])
  ], 2, 2, gpcmDistinctCells);
```

The final mode argument is required; there is no default. The provider label
must already exist and is bound to its stable pass index. The call installs
the same protected dependency role as the other named pass requirements.
Renaming a provider does not retarget the requirement. Self-dependencies,
cycles, and dependency edits during a running pipeline are rejected.

The clause applies whenever this consumer candidate is considered, including
a caller-locked candidate. It is ANDed with directional rules, entry domains,
same-cell requirements, and every other count or any-of clause.

## What is counted?

For each term, resolve the consumer coordinate plus its signed X/Y/Z offset,
using the consumer graph's bounded or wrapped topology. A term matches only
when that provider cell exists, is nonempty, and contains an accepted value.

| Mode | Count |
| --- | --- |
| `gpcmMatchingTerms` | Number of matching canonical declared offsets. Different offsets still count separately if wrapping aliases them to one cell. |
| `gpcmDistinctCells` | Number of different resolved provider cells matched by at least one term. Multiple matching aliases count once. |

A nonmatching alias never hides a later matching alias in distinct-cell mode.
For example, on a wrapped two-cell row, west and east from the first cell
both read the second cell. If that cell is a road, the two road terms count
as **two matching terms** but **one distinct cell**. This is why callers must
choose their interpretation explicitly.

Unresolved bounded offsets and empty provider entries contribute zero. Thus
`0..0` on a nonempty stencil expresses absence and can succeed at a boundary.
Wrapping may make a nonzero declared offset resolve back to the consumer's
own coordinate in the provider layer; it counts according to the selected
mode. It is not silently excluded as "self".

For matching-term mode, `1..N` is equivalent to any-of over the same canonical
terms, and `N..N` requires all those offset predicates. Distinct-cell mode is
appropriate when physical density or support must not double-count aliases.

## Canonicalization and validation

The core validates and copies caller arrays before installing a clause:

- The term array, every accepted-value array, and every value must be nonempty.
- Terms with exactly the same signed offset merge their accepted values as OR.
- Duplicate accepted values are removed; distinct offsets are ordered by
  signed X, then Y, then Z.
- Bounds must satisfy `0 <= minimum <= maximum <= N`, where `N` is the number
  of canonical distinct declared offsets, not the raw input term count.
- The mode must be a valid `TGraphPassCountMode` value.
- Repeating the same provider, term value sets, bounds, and mode is idempotent.
  Reordering accepted alternatives does not create a different count clause.

Different ranges or modes remain separate AND clauses; their intervals are
not implicitly merged. Existing `RequireFromPass`, `RequirePrevious`, and
`RequireAnyFromPass` merging behavior is unchanged. A count clause never joins
the historical previous-pass/same-cell OR merge.

Even a tautological `0..N` clause keeps its validated dependency. In distinct
cell mode, a small wrapped topology can have fewer than `N` physical targets:
a declared-valid minimum may therefore be unsatisfiable at solve time. The
core does not reinterpret the bounds after a reshape or topology change.

Invalid term data raises `EArgumentException`; invalid bounds or mode raises
`ERangeError`. Unknown labels raise `EArgumentException`; ownership, cycle,
and running-pipeline errors raise `EInvalidOperation`. Rejected construction
does not install a partial clause. Mutating an input array afterward does not
change the stored requirement.

## Staging, failure, and repair

Legacy `Run` reads provider output produced earlier in its execution. The
reference solver reads complete staged provider assignments within the atomic
transaction. Both evaluate the same count semantics. Counts are hard filters,
not weights or preferences.

A lower- or upper-bound failure uses the existing named provider dependency
diagnostic. Trace events identify the provider pass; they do not currently
encode the failed count, bounds, or individual clause as extra trace fields.
If several providers fail, existing earliest-provider diagnostic ordering is
preserved. An unsuccessful `TrySolve` rolls back the full transaction.

An ordinary consumer solve cannot revise a completed provider. Use
[selective regeneration](pass-dags.md) for a new explicit descendant
transaction, or [bounded pass negotiation](pass-negotiation.md) and
[selective negotiation](selective-negotiation.md) to reopen permitted complete
provider assignments within their budgets. Neither operation widens the
declared repair horizon automatically.

These are **finite per-candidate cross-pass counts**, not a global cardinality
solver. They do not assert "exactly ten houses anywhere in this output", count
partially solved peer domains, guarantee connectivity, optimize density, or
infer a neighborhood. Callers explicitly supply the stencil; other domain
validators remain necessary for global properties.

## Portable pipeline recipes

`wfc_pipeline_model` represents count clauses with `wprqCount`, `CountMode`,
`MinimumCount`, and `MaximumCount`. Construct them with
`MakeWfcPipelineCountRequirement`; the older generic requirement helper does
not accept the count kind. Immutable recipes require unique X/Y/Z-ordered
terms and provider-vocabulary-ordered accepted values, rather than silently
canonicalizing malformed artifact input.

The strict text spelling is `count-terms-v1` or `count-cells-v1` in the existing
six-field requirement header. A mandatory `count=I,min,max` record follows
immediately, before that requirement's term records. `I` is its parent
requirement index. For example, this header declares requirement zero for
consumer pass one, public token `C`, provider zero, and three terms:

```text
requirement=0,1,C,0,count-terms-v1,3
count=0,1,2
```

This is a fragment, not a complete recipe. Exact/any requirements cannot carry
a count record. Count mode and bounds participate in count-clause signatures;
existing non-count artifact bytes and signatures remain unchanged. Compilation
emits the core fluent clause, and the independent commit validator checks the
range again against public output.

## Runnable proof

[Neighborhood Counts](../examples/passes/04_NeighborhoodCounts/README.md)
shares one Pascal model between a native console host and an interactive
pas2js browser workbench. It composes terrain and roads into a market probe,
independently recounts captured output, contrasts both alias modes, and
demonstrates lower/upper failures, flood rejection, and bounded road repair.

The focused gates are `wfc_pass_count_test`, `wfc_pipeline_count_test`, and
`wfc_count_demo_test`. The native build also runs
`NeighborhoodCounts --selftest`; the browser's `?selftest=1` route additionally
checks that editing inputs invalidates the displayed result.
