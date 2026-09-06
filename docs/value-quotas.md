# Whole-pass value quotas

`TGraph.RequireValueQuota` constrains how many cells in one pass may select a
value from an explicit set. For example, a housing pass can require exactly ten
buildings while its existing terrain requirements exclude water. A rhythm pass
can require between two and four attacks. Quotas propagate over unresolved
domains and participate in ordinary reversible search.

The opt-in contract is `WFC_GRAPH_VALUE_QUOTA_VERSION = 1`. It is project-owned
Pascal, with no additional runtime dependency. Models without quotas keep
their existing random draws, decisions, trace hashes, and algorithm versions.

## A complete example

```pascal
program TenBuildings;
{$mode delphi}{$H+}
uses SysUtils, wfc;
var
  Graph: TGraph;
  Report: TGraphSolveReport;
  I, Buildings: Integer;
begin
  Graph := TGraph.Create;
  try
    Graph.Seed := 42;
    Graph.Reshape(24, 1, 1);
    Graph.WrapNeighbors := False;
    Graph.AddValue('empty');
    Graph.AddValue('house');
    Graph.AddValue('shop');
    Graph.RequireValueQuota(MakeGraphValueQuotaConstraint(
      'ten-buildings', ['house', 'shop'], 10, 10));
    Graph.RequireValueQuota(MakeGraphValueQuotaConstraint(
      'shops', ['shop'], 2, 4));
    if not Graph.TrySolve(DefaultGraphSolveOptions, Report) then
      raise Exception.Create('No composition within the search allowance');
    Buildings := 0;
    for I := 0 to 23 do
      if Graph.Entry[I, 0, 0].Value <> 'empty' then Inc(Buildings);
    WriteLn('Buildings: ', Buildings); // exactly 10, including 2..4 shops
  finally Graph.Free; end;
end.
```

## Contract and ownership

A `TGraphValueQuotaConstraint` contains a nonempty `LabelText`, nonempty
`Values`, and inclusive `MinimumCount` / `MaximumCount` bounds. Values must
already be registered in the active pass. Registration copies the set,
deduplicates it, and stores it in `AddValue` order. The empty/unassigned entry
sentinel is not a selectable quota value; register a normal token such as
`empty` if empty terrain is part of the model.

Bounds satisfy `0 <= minimum <= maximum <= High(Integer)`. Browser-hosted
records must also contain exact finite integers. A maximum larger than the
current cell count is harmless; a minimum larger than it makes the solve
unsatisfiable. Bounds do not silently change on reshape.

Each physical cell is counted once, including locked cells and cells on a
wrapped grid. Neighbor aliases do not multiply a cell's contribution. All
descriptors are hard AND clauses. Their accepted sets may overlap: the example
counts shops toward both quotas. Value weights remain preferences among legal
choices, not desired frequencies or quota substitutes.

The registry belongs to the active pass, just like connectivity. Re-registering
an identical labeled constraint is idempotent. A different definition under
that label raises an exception without changing the registry; remove it first
to replace it. `RemoveValueQuota`, `ClearValueQuotas`, and
`CopyValueQuotaConstraints` provide explicit removal and detached inspection.
`Reset` clears the registry. Mutations are rejected while the graph is running;
normal model authoring inside a derived pass initializer remains supported.

## Search, commit, and repair

For each quota, the solver counts forced members and possible members in
nonempty domains. Too many forced cells or too few possible cells cause an
immediate contradiction. Reaching the maximum removes accepted values from
mixed domains; reaching the minimum possible count removes their nonmembers.
These removals use the same queue and reversible trail as adjacency. Counts
are recomputed after restoration, so no separate mutable count cache can
survive a failed branch.

Quotas compose with adjacency, locks, caller domains, rooted connectivity, and
prior-pass requirements in the `TrySolve` family, including selective solving,
negotiation, and deterministic restarts. The numeric solver independently
recounts complete assignments. The public coordinator separately recounts
public values before commit and after the caller's commit hook. Failure follows
the existing atomic entry/random-state rollback contract.

A changed quota on an upstream pass cannot silently invalidate a preserved
provider. Selective preflight recounts it and rejects the request when its
current assignment no longer satisfies the quota. Include that owning pass in
the regeneration scope. Negotiation still follows its declared pass horizon;
quotas do not authorize broader repair.

Legacy `Run` does not implement quota search and explicitly rejects quota-bearing
pipelines before entry/random-state mutation. Use `TrySolve` or its negotiated,
selective, or restart variants.

## Diagnostics and limits

`gckValueQuota` identifies a search contradiction. Its `ConstraintIndex` is the
ordinal in the failed pass's copied quota registry, not its connectivity
registry. A whole-pass bound failure has `EntryIndex = -1`; it does not invent
an offending coordinate. Trace cause `gtckValueQuota` formats as `value-quota`
and carries that ordinal through removals, causally linked decisions, and
contradictions. Quota ordinals contribute to opt-in event and terminal
negotiation hashes, including when event capture is disabled. Full capture and
bounded live windows share the same event stream.

The implemented bounds propagation is sound, but is not a claim of generalized
arc consistency for overlapping quotas. Some impossible combinations need
search. It does not optimize change count, infer a region, constrain multiple
passes with one quota, or provide percentages, weighted sums, or soft penalties.
Finite grids still require memory and checked numeric capacity. The current
portable pipeline/training recipe format does **not** serialize this registry;
author these constraints through the fluent Pascal API until that format is
extended explicitly.

These differ from [cross-pass neighborhood counts](pass-counts.md), which
inspect complete provider cells while filtering a candidate. See the
[implementation and oracle record](research/value-quotas-v1.md) for the
propagation argument and verification scope.
