# Prepared pipeline sessions

`wfc_pipeline_session` provides an in-memory edit/repair lifecycle over an
immutable portable recipe. It uses [prepared editable bindings](pipeline-preparation.md)
and the core's existing ordinary/negotiated full/selective solvers. It is MIT
Pascal shared by native FPC and pas2js, with no mutable graph escape or external
dependency. Its purpose is explicit repair permission and honest detached
evidence. The separate [workspace layer](pipeline-workspaces.md) adds saved
history, exact replay, atomic publication and a native CLI above this session;
the session unit itself remains an in-memory owner.

Use [immutable composition](pipeline-composition.md) to assemble definitions,
`TWfcPipelineRun` to choose extents and inputs, then
`TWfcPipelinePreparedSession` to maintain edits. Recipe, seed, format and every
pass layout stay fixed for one session. To change them, create a new session.

## Lifecycle and ownership

Construction takes `Recipe`, `InitialRun`, explicit replacement limits and
explicit outcome limits. The immutable recipe is borrowed and must outlive the
session. The run and caller arrays are needed only during each call: the session
owns copies of its original and applied invocation, an editable binding, pending
causes and a separate last-successful public snapshot. Calls on one session
must not run concurrently. No external graph hooks/observers are installed.

Every returned class is caller-owned and must be freed. Every `Copy*` array and
`LayerAt` nested array is detached. State, edit outcomes, solve outcomes and
repair plans contain no recipe/session borrow and remain inspectable after
those owners are freed. `CopyAppliedRun` returns a caller-owned ordinary typed
run while the live session still has its valid recipe borrow. `CopyInvocation`
on evidence objects returns a detached record, not a new run artifact.

| Operation | Revision and state |
| --- | --- |
| Constructor | Revision0, usable, no successful baseline or current output |
| `ApplyInputs(DesiredRun)` | Every accepted call increments revision, including no-op/options-only edits |
| `ExecuteInitial` | Once only; uses then-applied inputs/options, increments on a normal solved/unsolved outcome |
| `PlanRepair(DesiredRun, Roots)` | Read-only permission preview bound to exact session identity and revision |
| `ExecuteRepair(Plan)` | A normal solved/unsolved repair increments revision; input/preflight refusal does not |

Edits before initial execution are allowed and remain real accepted revisions.
A failed initial solve is inspectable and does not poison a usable session,
but never enables selective repair: retrying initial generation means creating
a new session. This differs deliberately from repeated fresh runtime `Execute`.

`HasSuccessfulBaseline` means a complete successful detached snapshot has been
retained. `HasCurrentOutput` additionally means no unresolved edit/repair region
remains. `CopyLastSuccessfulState` returns `nil` before the first success; later
it is a separate historical preview, never permission to label dirty output
current. No-op/options-only edits can preserve currentness. Authored/effective
input changes revoke it, including locking the same token that was generated.

## Apply complete inputs, then request scope

`ApplyInputs` accepts the complete desired ordered lock/domain lists through a
validated run, not a patch. Omit a record to clear it. Changes are installed
immediately; a lock followed by clear is two ownership transitions, not a
coalesced no-op. Clearing the last caller lock empties its materialized cell.
Domain removal restores the pristine compiler base. Alias and inverse bridge
effects use the shared preparation logic rather than guessed public neighbors.

The edit outcome exposes `CopyImpact`, `CopyAuthoredPassIndices`,
`CopyPendingPassIndices`, `CopyPublicState` and `CopyInvocation`. Pending
requirements accumulate actual changed pass indices across edits. Authored
alias provenance is retained separately. Undoing a later input does not erase
earlier unsettled causes.

`PlanRepair` requires desired inputs to exactly match the currently applied
ordered inputs. Execution strategy, budgets and trace options may differ.
Roots are existing graph pass labels, including private labels when explicitly
authorizing an inverse provider. It calls `TGraph.ResolveRegenerationScope`,
the same DAG helper used for actual selective regeneration:

- Requested roots become unique indices in pass-index order.
- Active passes follow actual topological execution order, not numeric order.
- Only descendants are included; no missing ancestor is silently authorized.
- Planning does not mutate entry values, domains, selected pass or random
  streams. Derived dependency roles may be synchronized by the core.

The detached scope contains `ScopeAlgorithmVersion`, `RequestedRootIndices`,
`ActivePassIndices`, `AuthoredPassIndices`, `RequiredPassIndices` and
`MissingPassIndices`. `MissingBaseline` and `CanExecute` make insufficient scope
inspectable. An alias/leaf root alone cannot authorize a changed provider.
Unknown or malformed roots raise a typed error rather than producing a plan.

`ExecuteRepair` accepts only the exact owner's current revision-bound plan.
A foreign identical session, disposed owner or stale revision cannot acquire
authority through matching hashes. Rejected plans leave the usable session
unchanged. Repair does not assign `Graph.Seed` again or reapply input locks;
each core solve attempt still deterministically resets active-pass random state
from the effective seed and stable pass index.

Normal solver failure keeps the accepted edits, old pending requirements and
the entire authorized active closure pending. A later unrelated repair cannot
hide that failure. A successful sufficient repair settles covered requirements;
currentness returns only when none remain. This is conservative scope, not a
minimum-change or universal-solvability claim.

## A complete session operation

The procedure below accepts an initial and an edited invocation for the same
recipe/seed/layout. Budgets are supplied explicitly by the host. It returns a
detached result only after the requested repair is authorized and executed:

```pascal
uses SysUtils, wfc, wfc_pipeline_model, wfc_pipeline_run,
  wfc_pipeline_prepare, wfc_pipeline_session;

function GenerateThenEdit(const Recipe: TWfcPipelineModel;
  const InitialRun, EditedRun: TWfcPipelineRun;
  const RepairRoots: TGraphPassLabels;
  const ReplacementLimits: TWfcPipelineReplacementLimits;
  const OutcomeLimits: TWfcPipelineSessionOutcomeLimits): TWfcPipelineSessionOutcome;
var Session: TWfcPipelinePreparedSession;
  Initial: TWfcPipelineSessionOutcome;
  Edit: TWfcPipelineSessionEditOutcome;
  Plan: TWfcPipelineSessionRepairPlan;
begin
  Result := nil; Session := nil; Initial := nil; Edit := nil; Plan := nil;
  try
    Session := TWfcPipelinePreparedSession.Create(
      Recipe, InitialRun, ReplacementLimits, OutcomeLimits);
    Initial := Session.ExecuteInitial;
    if not Initial.Solved then Exit;
    Edit := Session.ApplyInputs(EditedRun);
    Plan := Session.PlanRepair(EditedRun, RepairRoots);
    if not Plan.CanExecute then
    begin
      { A persistent editor should retain Session and display CopyScope here;
        this one-shot example deliberately discards its private candidate. }
      Exit;
    end;
    Result := Session.ExecuteRepair(Plan);
    { Inspect Result.Solved and Result.HasCurrentOutput before publication. }
  finally
    Plan.Free; Edit.Free; Initial.Free; Session.Free;
  end;
end;
```

The caller frees the returned outcome, even when `Solved=False`. Build desired
inputs with the existing `MakeWfcPipelineCellLock` and
`MakeWfcPipelineCellDomain` constructors, then create a complete `TWfcPipelineRun`.
Reuse its seed/format/extents for edits. The executable
`test/wfc_package_consumer.lpr` demonstrates same-token alias lock, clear,
insufficient leaf scope, stale-plan refusal and successful provider repair.

## Actual public state and solver reports

`CopyPublicState` returns each public pass's index, label, rank, exact lattice
layout and cells. Cells retain `Token`, `Empty` and `Generated`; empty cells have
no token. Nonempty tokens are checked against the recipe's public vocabulary.
This does not fabricate a complete fresh result from reused or dirty entries.

Outcomes distinguish `wpsokOrdinaryFull`, `wpsokNegotiatedFull`,
`wpsokOrdinarySelective` and `wpsokNegotiatedSelective`. `CopySolveReport` returns
the actual terminal solve. `CopyNegotiationReport` and
`CopySelectiveNegotiationReport` require the matching available report kind;
check `HasNegotiation`/`HasSelectiveNegotiation` before calling them.

Every actual scalar and nested array is retained: pass reports, execution order,
contradiction including `ConstraintIndex`, trace events/delivery fields,
excluded assignments, rejected attempts, terminal reports and hashes.
`LastValidation` exposes the actual independent commit validation state.
The authorized scope can be larger than a failed operation's execution order;
neither is substituted for the other. Capture-disabled runs do not invent traces.
Exactly one report hierarchy is stored; accessors make detached views instead
of retaining duplicate terminal reports.

## Explicit limits and unusable candidates

Initialize every `TWfcPipelineSessionOutcomeLimits` field. `Version=1`; every
count must be a positive finite `Integer`. One owned detached payload charges:

| Limit | Charged inventory |
| --- | --- |
| `MaxPublicCellRecords` | Every captured public cell |
| `MaxEncodedTokenBytes` | Encoded public labels/tokens, invocation lock/domain tokens, actual trace values and delivery messages |
| `MaxReportPassRecords` | All solve `Passes` rows in the one stored hierarchy, including rejected attempts and terminal solve |
| `MaxTraceEvents` | All trace events in that hierarchy |
| `MaxExcludedAssignmentItems` | Every retained rejected-assignment element |

Limits precede detached array allocation/cloning. Reports already exist in the
core by then: these do not cap earlier solver-report allocation. Scope arrays
remain bounded by recipe pass limits; invocation arrays retain existing run
bounds. Native report strings remain actual report fields; encoding conversion
is used for accounting, not a newly promised portable trace wire format.
Caller-retained copies, last-successful copies, input plans and peak aggregate
memory are not collectively covered. These are finite logical/encoded-byte
envelopes, independent of solve budgets and desired application output length.

Session preflight errors raise `EWfcPipelineSession`; lower input/binding errors
retain `EWfcPipelineRuntime` and existing typed validation exceptions. Allocation
failures are not translated into successful outcomes. A normal unsolved report
leaves `Usable=True`. Any exception after mutation starts, including detached
capture failure, makes the candidate unusable and noncurrent. Further mutations
and current-state capture reject. Safe scalar diagnostics, applied invocation
and an already retained last-successful snapshot remain inspectable.

There is no in-place rollback promise or automatic retry after a poisoned
candidate. A host needing atomic live publication must replay accepted operations
into a separate candidate and publish/swap only after complete capture and
encoding. Do not silently continue from partially applied inputs.
`TWfcPipelineWorkspaceSlot` implements that separate-candidate publication
boundary; see [atomic workspace authoring](pipeline-workspaces.md#atomic-authoring-and-ownership).

## Boundaries and verification

The [workspace APIs](pipeline-workspaces.md) separately provide complete
session-evidence encoding, ordered journals, exact replay, atomic authoring
and native file commands. The separate
[Pipeline Workspace demo](../examples/passes/08_PipelineWorkspace/README.md)
uses those APIs for a generic browser editor and native artifact workflow;
it does not add UI responsibilities to this session type or migrate existing
demos. Existing recipe/run/result formats and fresh result validation stay
unchanged.
A saved fresh invocation cannot represent selective reuse, currentness or
accepted lock/clear history; do not export these outcomes as result2 by discarding
that distinction. Workspace journals preserve accepted operation order,
including every lock/clear transition; imported evidence stays unverified until
complete actual replay matches it.

The maintained primary session fixture covers lifecycle, bounds, retained edits,
poisoned candidates and hostile raw handles. The independently authored oracle
compares every report/state field with direct core operations across all four
solve modes, trace on/off, unlike layouts, nonnumeric DAG order, cumulative
pending regions and actual nonzero constraint ordinals. Preparation, replacement,
three learned inverse bridge kinds and scope inspection have separate suites.
Native thread tests remain native-only; portable suites run through the included
FPC browser server/checker. Package consumer checks require installed units,
not a source fallback. These tests support this boundary, not completion of the
broader ecosystem or an assumption that hosted CI has already finished.
