# Preparing and replacing pipeline inputs

`wfc_pipeline_prepare` separates pure input lowering from graph ownership.
Use it when several executions share one immutable recipe, seed and layout, or
when a low-level editor needs to replace caller locks and domains. For an editor
that also needs revisions, pending requirements and explicit repair permission,
start with [prepared sessions](pipeline-sessions.md).

Both APIs are project-owned MIT Pascal for native FPC and pas2js. They use the
same recipe/run types as the [portable artifact workflow](pipeline-artifacts.md)
and are included in the [FPM and Lazarus packages](building.md#fpm-package).
There is no extra dependency, implicit file I/O, or background worker.

## Choose the ownership boundary

| API | Owns | Intended use |
| --- | --- | --- |
| `TWfcPipelinePreparation` | Validated recipe context, reusable vocabularies and lowering lookups | Prepare inputs without allocating a graph |
| `TWfcPipelineInputPlan` | Detached invocation inputs and private lowered constraints | Inspect or share an immutable prepared plan |
| `TWfcPipelineInputBinding.Create` | A fresh compiled graph and retained plan payload | Initial installation and low-level solving |
| `TWfcPipelineInputBinding.CreateEditable` | The same, plus pristine compiler bases and explicit replacement limits | Replace inputs on one graph without recompilation |
| `TWfcPipelinePreparedSession` | Editable binding, revision, pending requirements and detached execution evidence | Controlled edit/repair lifecycle without a graph escape |

`PrepareInputs` checks and lowers public locks/domains, transform aliases and
supported inverse projection constraints before graph allocation. It does not
solve, and successful preparation does not promise that the constraints are
satisfiable. Binding is the graph-allocation boundary: it compiles the recipe,
installs inverse domains, public domains and locks, then sets the initial seed.

## Reusing a preparation

The initial invocation establishes an epoch: the same run format, seed, every
pass topology and every actual extent must be retained by subsequent plans.
Strategy, solve budgets, trace capture, locks and domains may change. To change
the recipe, seed or geometry, construct a new preparation/session. Recipe5 and
run2 retain independent pass extents; this API neither pads grids nor imposes
a uniform root shape.

This complete procedure solves one prepared invocation. `Recipe` and `Run`
are validated objects supplied by the caller:

```pascal
uses SysUtils, wfc, wfc_pipeline_model, wfc_pipeline_run,
  wfc_pipeline_prepare;

procedure SolvePrepared(const Recipe: TWfcPipelineModel;
  const Run: TWfcPipelineRun);
var Preparation: TWfcPipelinePreparation;
  Plan: TWfcPipelineInputPlan;
  Binding: TWfcPipelineInputBinding;
  Options: TGraphSolveOptions;
  Report: TGraphSolveReport;
begin
  Preparation := nil; Plan := nil; Binding := nil;
  try
    Preparation := TWfcPipelinePreparation.Create(Recipe, Run);
    Plan := Preparation.PrepareInputs(Run);
    Binding := TWfcPipelineInputBinding.Create(Preparation, Plan);
    FreeAndNil(Plan);
    FreeAndNil(Preparation);
    Options := DefaultGraphSolveOptions;
    Options.MaxBacktracks := Run.MaxBacktracks;
    Options.CaptureTrace := Run.CaptureTrace;
    { This example deliberately selects ordinary, not negotiated, solving. }
    if not Binding.BorrowCompiled.Graph.TrySolve(Options, Report) then
      WriteLn('No solution within this ordinary solve policy.');
  finally
    Binding.Free; Plan.Free; Preparation.Free;
  end;
end;
```

The immutable recipe must outlive preparation and every binding that borrows
it. The caller's run is needed only during preparation/plan calls; its values
are copied. A binding retains its own plan payload, so caller plan and
preparation wrappers can be freed after binding construction. A plan's
`CopyLocks`, `CopyDomains` and caller-owned `CopyPassLayouts` remain inspectable
after its producer and original run are freed. Returned nested arrays are
detached. The plan cannot bind through a different preparation, even one made
from byte-identical inputs: an exact retained identity lease supplies authority,
not a hash or a recycled object address.

Independent native runtimes and bindings may use shared immutable preparations
and plans concurrently. Keep the recipe and all wrappers alive throughout each
call; reference counting does not make concurrent destruction safe. Do not
mutate one binding concurrently. Native reference ownership uses compiler RTL
interlocked operations, not a process-global mutable registry.

## Explicitly editable bindings

Construct `TWfcPipelineInputBinding.CreateEditable(Preparation, InitialPlan,
Limits)`. All fields of `TWfcPipelineReplacementLimits` must be initialized:

```pascal
Limits.Version := 1;
Limits.MaxRetainedCellRecords := CellRecordBudget;
Limits.MaxRetainedValueItems := ValueItemBudget;
Limits.MaxCandidateVisits := ReplacementWorkBudget;
Binding := TWfcPipelineInputBinding.CreateEditable(Preparation, InitialPlan, Limits);
DesiredPlan := Preparation.PrepareInputs(DesiredRun);
try
  Impact := Binding.ReplaceInputs(DesiredPlan);
finally
  DesiredPlan.Free;
end;
```

The three budgets are positive finite `Integer` values chosen by the caller;
there is intentionally no convenience default pretending to suit every domain.
They are not output lengths or solve budgets. `DesiredRun` contains the complete
desired ordered lock/domain lists, not an incremental patch. Omit a previous
record to remove that input. Calling replacement on a fresh-only binding rejects.

Replacement compares old/new public locks, domains and inverse-source keys,
using actual pass-layout prefix indices. First-touched keys retain pristine
compiler bases, not already overlaid inputs. Removing a domain restores its
compiler base; absent, explicit full and explicit empty domains remain distinct.
Multiple aliases contributing to one effective lock do not erase each other's
ownership. Supported inverse lowering remains Pattern2D bridge v2, Sequence
bridge v2 and Pattern3D bridge v1; old forward-only versions are not upgraded.
Mapped policies are evaluated as policies, not invented inverse projection rules.

`TWfcPipelineInputImpact` contains detached, ascending unique pass indices:

- `AuthoredInputsChanged` and `AuthoredPassIndices` describe changed authored
  records, including alias provenance and ordered-list changes.
- `GraphInputsChanged` and `ChangedPassIndices` describe actual effective lock
  or domain changes on materialized/private passes. These are conservative
  impacts, not a promised minimum repair set.

An identical replacement invokes no graph setters and does not reseed or
recompile. Options-only plans have no input impact; the later solver call must
apply the desired execution options. A lock of a currently generated token
still changes ownership. Removing its last caller-lock contribution clears
the value; it does not resurrect the old generated token. Therefore `lock`
then `clear` is two operations, even when the final authored list equals the
initial list. Do not coalesce accepted edits before applying them.

The editable `BorrowCompiled` reference permits solving, inspection and pass
selection only. Do not externally mutate definitions, dependencies, values,
domains, seed or hooks; doing so invalidates the captured compiler-base contract.
Selection is restored by replacement. Prefer a prepared session when callers
should not receive this mutable low-level reference at all.

## Resource accounting and failure behavior

Replacement preflight charges both old/new retained plan payloads (a shared
payload once), their authored/effective/inverse records and value items, saved
base records/indices, union patches, desired values, detached impact arrays and
temporary vocabulary/current-domain/canonical vectors. Its work estimate covers
registered-value scans, intersections, key lookups and the setter's conservative
quadratic canonicalization/copying work. No registered vocabulary cache survives
a replacement. Counts are logical items, not exact heap bytes; caller-held plans
and whole-process memory are not collectively bounded.

Existing recipe/run/text/result envelopes remain in force. In particular,
runtime total materialized storage is bounded at 16,777,216 cells. Versioned
inverse lowering has aggregate limits of 1,048,576 contributions, 16,777,216
candidate visits and 4,194,304 private indices. Raising editable budgets cannot
waive these existing boundaries. These finite allocation controls are unrelated
to any preferred composition duration or application output-size target.

Input, identity and resource preflight errors raise `EWfcPipelineRuntime` before
writes, leaving the binding usable and unchanged. The runtime unit exposes
the same exception type and constants as aliases for existing callers. A
post-write exception, including allocation failure in a graph setter, makes
the editable binding unusable: borrowing/replacement reject, destruction remains
safe. There is no in-place rollback guarantee. A host requiring atomic published
work must build/replay a separate candidate and swap only after complete success.

Raw pas2js handles and new records/arrays require supported passive shapes;
ordinary getters, sparse arrays and forged/uninitialized wrappers reject.
These checks are not a security sandbox against hostile Proxy traps or direct
concurrent modification of legitimate private state.

## Fresh runtime compatibility and examples

`TWfcPipelineRuntime` still borrows both recipe and run for its lifetime.
Its repeated `Execute` calls replay the same fresh invocation and return the
existing detached result1/result2 objects. The extraction reuses its original
lowering/installation and solver/result path; it is not a second implementation
of aliases or inverse constraints. Retaining prepared payloads does change
memory retention; memory-equivalence with the old initializer is not claimed.

See `test/wfc_pipeline_prepare_test.lpr` and the native-only
`wfc_pipeline_prepare_threads_test.lpr` for identity, lifetime and independent
execution examples. `wfc_pipeline_replace_test.lpr` covers editable transitions;
`wfc_pipeline_replace_inverse_test.lpr` uses all three actual learned inverse
resource kinds. The [installed-package consumer](package-checking.md) exercises
the API without a source-path fallback. [Prepared sessions](pipeline-sessions.md)
add the revision and explicit repair protocol above this low-level owner.
