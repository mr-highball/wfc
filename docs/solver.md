# reference solver

`TGraph.TrySolve` is the opt-in propagating solver. It maintains a domain for
every cell, propagates constraints to a fixed point, observes by minimum
remaining values, and uses bounded chronological backtracking when a decision
causes a contradiction. The existing `TGraph.Run` traversal remains available
for source and behavior compatibility.

The reference solver coordinates the complete pass pipeline. It stages every
pass in memory and commits entry values only after every pass has solved and
the final assignments have been validated.

## basic use

```pascal
uses
  SysUtils,
  wfc;

var
  Graph: TGraph;
  Options: TGraphSolveOptions;
  Report: TGraphSolveReport;
begin
  Graph := TGraph.Create.Reshape(16, 16, 1);
  try
    Graph.Seed := $DEADBEEF;
    Graph.AddValue('land')
      .NewRule(AllDirections, ['land', 'water']);
    Graph.AddValue('water')
      .NewRule(AllDirections, ['land', 'water']);

    Options := DefaultGraphSolveOptions;
    Options.MaxBacktracks := 1024;
    if not Graph.TrySolve(Options, Report) then
      WriteLn('Unsolved pass: ', Report.FailedPassIndex);
  finally
    Graph.Free;
  end;
end;
```

`DefaultGraphSolveOptions` currently permits 256 backtracks per pass. A value
of zero permits propagation and first choices but stops at the first required
branch restoration. A negative value raises `ERangeError`.

## pipeline transaction

`TrySolve` performs one transaction over the complete linear pipeline:

1. save the selected pass and every pass random-stream state;
2. rewind the versioned per-pass streams from `Seed`;
3. solve each defined pass in index order into a staging buffer;
4. stage a copy of the preceding result for each later definitionless pass;
5. validate each solved assignment independently; and
6. commit every staged value only after the complete pipeline succeeds.

A definitionless pass zero has no input and remains exactly as supplied. In a
later definitionless pass, caller locks override the copied value at that
coordinate. An entry with a nonempty value and `Generated = False` is a caller
lock. Existing generated output is not a lock and is regenerated.

When `TrySolve` returns `False`, entry values, `Generated` flags, selected pass,
and pre-call random-stream states are unchanged. A later-pass contradiction
therefore cannot leave an earlier pass half committed. A successful call marks
new solver output as generated while preserving caller locks.

Entry setters are still used during commit so derived entry behavior remains
available. Each hook observes the pass currently being committed. If a setter
raises, or any entry differs from its exact staged `Value`, `Empty`, and
`Generated` state after all setters finish, those three fields are restored and
the exception is re-raised. A transient rewrite of a later solver-owned entry
can therefore be overwritten by that entry's own commit. Side effects outside
those three fields cannot be undone. Commit hooks must not mutate topology,
rules, pass configuration, or other model state; such mutations are unsupported,
and validation applies to the prepared pre-commit snapshot.

## constraint semantics

For a cell value `C`, a neighbor value `N`, and direction `D` from the cell to
that neighbor, define `Allows(S, D, T)` as the directional rule on source value
`S` permitting target value `T`. An absent directional rule or a rule with an
empty value list is a wildcard. Reference-solver compatibility is the
two-sided conjunction:

```text
Allows(N, D, C) and Allows(C, Inverse(D), N)
```

This makes compatibility independent of which endpoint is assigned first.
Every directional arc is checked separately, even when wrapping makes several
directions point to the same physical entry. A wrapped self-arc supports only
the same candidate value; one candidate cannot use a different candidate as
its own support. A nil neighbor imposes no adjacency constraint.

Required rules retain their force/trigger meaning. A value whose rule group
contains a required rule is required-only. Unless the cell is a caller lock,
that candidate needs at least one physical neighbor whose nonempty required
directional rule explicitly lists it. Support is existential across the six
directions and must also be an adjacency-compatible pair. Nil neighbors cannot
support a candidate, and a self-arc can support only that same candidate.
Caller locks are exempt from required-support eligibility, matching the legacy
behavior, but they still have to satisfy adjacency and previous-pass
constraints.

`RequirePrevious` is an additional unary constraint. It compares a candidate
with the staged value at the same coordinate in the immediately preceding
pass. Thus later passes always read the result of the current transaction, not
stale output left by an earlier run.

The reference solver rejects the empty string as a registered model value. It
is reserved for the public empty-entry state.

## deterministic algorithm

After initial lock and `RequirePrevious` filtering, the solver repeatedly
propagates adjacency and required-support removals through a queue until no
domain changes. It then observes the uncollapsed cell with the smallest domain.
Ties use the pipeline `Mode` Z order and then ascending entry index within each
plane.

Candidates retain `AddValue` order. Each decision draws one unbiased
`RandomIndex` and uses that result as the starting offset of a cyclic candidate
order. Failed alternatives are restored from a removal trail and retried in
that frozen order. Backtracking is chronological and its limit is applied
independently to each pass.

`WFC_SOLVER_ALGORITHM_VERSION = 1` identifies these propagation, observation,
candidate-ordering, and backtracking rules. A replay identity for `TrySolve`
includes both solver and random algorithm versions, the seed, graph topology
and mode, pass order, value/rule construction order, locks, and solve options.
Every call rewinds its streams, so an unchanged graph and seed replay on both
native FPC and pas2js. A failed call restores the stream state that existed
before the call.

That identity guarantees replay of solver decisions. Whole-call completion and
external side effects also require deterministic entry-setter hooks with the
same configuration and external state. Hooks can raise or perform effects that
the solver neither controls nor includes in its algorithm identity.

Legacy callbacks and traversal hooks are deliberately outside this algorithm.
`TrySolve` does not invoke `SelectionCallback`, `InvalidStateCallback`,
`DoGetStartCoord`, `DoGetSelection`, or `DoValidate`.

## reports and failures

`TGraphSolveReport.Status` is one of:

- `gssSolved`: every pass validated and committed;
- `gssContradiction`: the search proved the current bounded search space
  contradictory; or
- `gssBacktrackLimit`: another failed branch needed restoration after the
  configured limit had been reached.

The report records `Seed`, `RandomAlgorithmVersion`,
`SolverAlgorithmVersion`, `FailedPassIndex`, terminal contradiction evidence,
and one `TGraphPassSolveReport` per pass. Terminal contradiction kinds are
`gckInvalidLock`, `gckEmptyDomain`, `gckAdjacency`, `gckPreviousPass`,
`gckRequiredSupport`, and `gckFinalValidation`; `gckNone` is used on success.
Entry and neighbor indices are zero-based. Inspect `HasDirection` before using
the direction field. `FailedPassIndex` and unavailable contradiction pass,
entry, or neighbor indices are `-1`. Report contents are unspecified when
`TrySolve` raises instead of returning.

Per-pass counters have deliberately narrow definitions:

- `Decisions` counts every attempted branch alternative, including retries;
- `Propagations` counts candidate removals caused by adjacency or
  required-support propagation;
- `Contradictions` counts invalid-lock failures, zero-domain discoveries, and
  final-validation failures; and
- `Backtracks` counts completed failed-branch restorations.

Counters saturate at `High(Integer)` so exceptionally large searches retain
the same report behavior under checked native builds and pas2js.

Initial domain construction for locks and `RequirePrevious`, and the direct
restriction to a chosen branch value, are not propagations. A solved pass may
have nonzero contradiction and backtrack counters if it recovered from failed
alternatives; the report's terminal contradiction is cleared on success.

Impossible but structurally valid input returns `False`, including unknown
caller locks on a defined solver pass and unsatisfied constraints. Malformed
model or topology state raises before committing entries. Examples include a
negative backtrack limit, an empty registered symbol, a rule target outside the
value registry, duplicate directional records in a directly replaced public
rule array, or, on a defined pass, a neighbor object outside that pass's own
entry storage. Definitionless passes copy or preserve caller state without
compiling a solver model. The external-neighbor restriction freezes the
topology used by this first reference-solver version; the legacy `Run` path
continues to support public external links.

## implementation unit

`wfc_solver_reference` contains the flattened, string-free kernel used by the
public `wfc` facade. It is installed because package managers must compile it,
but its record layout and helper declarations are implementation details and do
not carry a source-compatibility promise. Applications should use
`TGraph.TrySolve`; direct kernel use is reserved for framework development and
tests until a separately versioned low-level API is deliberately published.

## current scope

Version 1 is intentionally unweighted and has no restart policy, timing data,
stable trace hash, soft constraints, or minimal-unsatisfiable-core analysis.
Pass dependencies are still linear, and `RequirePrevious` still addresses only
the same coordinate in the immediately preceding pass. These remain roadmap
work rather than hidden or partially specified behavior.
