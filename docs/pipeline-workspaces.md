# Editable pipeline workspaces

The workspace layer saves an ordered edit/solve history, replays it against the
real solver, and publishes a replacement live owner only after that work has
completed. It builds on [prepared sessions](pipeline-sessions.md), not on
importing generated values as locks or restarting only the final invocation.
The five units are shared MIT Pascal for native FPC and pas2js; the included
native `wfc_workspace` tool needs no additional runtime or server.

This is a library and command-line workflow. The Mapped World and Ensemble
Studio browser interfaces have not been migrated to this workspace owner.
Existing [recipe/run/result formats](pipeline-artifacts.md) and the generic
[artifact-family tools](artifact-tools.md) keep their existing contracts.

## From saved claims to verified execution

| Unit | Responsibility |
| --- | --- |
| `wfc_pipeline_workspace_context` | Own complete canonical recipe/run documents, with explicit run-to-recipe indices |
| `wfc_pipeline_session_evidence` | Encode actual detached session edit/solve outcomes as complete canonical evidence |
| `wfc_pipeline_workspace_journal` | Own contexts and ordered actions; validate static history rules without allocating a graph |
| `wfc_pipeline_workspace_journal_text` | Strictly encode/decode the closed journal1 envelope |
| `wfc_pipeline_workspace_replay` | Execute and compare every recorded action; retain verified execution and atomically author/restore a live slot |

`TWfcPipelineWorkspaceJournal.Verification` is always
`wpwvUnverifiedClaims`. Decoding is graph-free. It checks the complete outer
format, bound contexts, action ordering and the syntactic evidence envelope;
it does **not** validate imported report contents or prove a successful solve.
An altered claim, or an inner report truncated at an otherwise valid LF
boundary, can remain syntactically acceptable. It stays explicitly unverified.

`ReplayWfcPipelineWorkspace` reconstructs every epoch and performs every edit,
initial attempt and authorized repair in order. After each action it encodes
the actual outcome and compares the complete evidence text, byte for byte.
Only a complete match returns `TWfcPipelineWorkspaceExecution`. Hash equality,
plausible output cells and a canonical envelope are not substitutes for this
comparison. No imported output is installed as a lock.

A verified execution can legitimately have `HasCurrentOutput=False`: a normal
unsolved attempt is also replayable evidence. Check currentness and baseline
separately before exporting application output. `CopyLastSuccessfulState` is
historical, not proof that dirty cells are current.

## Contexts, epochs and history

`TWfcPipelineWorkspaceContexts.Create(RecipeTexts, RunTexts, Limits)` owns
independent decoded contexts. Each `TWfcPipelineWorkspaceRunText` contains
`RecipeIndex` and complete canonical `Text`. Equal hashes or even equal recipe
texts do not merge rows; resource documents and provenance remain intact.
Context limits explicitly bound recipe rows, run rows and summed text bytes.
Set `TWfcPipelineWorkspaceContextLimits.Version=1`, `MaxRecipes`, `MaxRuns`
and `MaxTextBytes` explicitly. `RecipeTextAt`, `RunTextAt`, `RecipeCount`,
`RunCount` and `TextBytes` inspect the retained context table without solving.
Provenance is data, never an instruction to open a file or URL.

Journal actions are `TWfcPipelineWorkspaceAction` records with `Kind`,
`RunIndex`, `RequestedRootIndices` and `EvidenceText`. Their enum is
`wpwakBeginEpoch`, `wpwakEdit`, `wpwakInitial`, `wpwakRepair`.
`TWfcPipelineWorkspaceJournal.Create(RecipeTexts, RunTexts, Actions, Limits)`
accepts these complete arrays directly; use the slot to author actual claims
without manually assembling reports.

| Action | Static and runtime contract |
| --- | --- |
| Begin epoch | Must be first; later epochs retain all earlier history. Selects an explicit recipe/run and starts a new session without a baseline. |
| Edit | Replaces the complete ordered inputs immediately. Every accepted edit remains a distinct record, including options-only and no-op edits. |
| Initial | At most once per epoch, using exactly the then-applied run. A normal failed attempt is still an attempt. |
| Repair | Follows an initial attempt; uses the same ordered locks/domains as the applied run, with optionally different strategy, budgets or trace settings. Requires actual baseline and sufficient explicit scope during replay/execution. |

Within an epoch, the exact recipe index, seed, run format and every pass layout
and rank are fixed. Change them with an explicit new epoch, not an edit. A
lock followed by clear is two real ownership transitions; it must not be
coalesced into a no-op. Omitting a lock/domain record from the complete next
run clears that input according to [replacement semantics](pipeline-preparation.md).

Only repair actions have roots: nonempty, ascending, unique, zero-based recipe
pass indices. Private pass indices are valid explicit permissions. The actual
closure follows the dependency DAG's topological order, which need not be
numeric order. Descendants are included; missing ancestors are never silently
authorized. Authored, required, active and missing passes remain distinct.
Static journal validation cannot infer a successful baseline or scope
permission from imported reports; replay checks them against actual sessions.

## Atomic authoring and ownership

Start with `TWfcPipelineWorkspaceSlot.Create` and an explicit policy. Its
`PublicationRevision` begins at zero. Each successful publication advances it
once; supply the expected value on every operation. This is a local revision
check, not a cryptographic token or permission for another slot.

```text
BeginEpoch(RecipeText, InitialRunText, Policy, ExpectedRevision): Receipt
ApplyInputs(CompleteRunText, Policy, ExpectedRevision): Receipt
ExecuteInitial(Policy, ExpectedRevision): Receipt
PreviewRepair(CompleteRunText, RootIndices, Policy, ExpectedRevision): Preview
ExecuteRepair(CompleteRunText, RootIndices, Policy, ExpectedRevision): Receipt
Restore(Journal, JournalLimits, ReplacementLimits, OutcomeLimits,
        EvidenceLimits, ReplayLimits, ExpectedRevision)
```

Authoring replays the accepted history into a separate candidate, executes the
new action exactly once, captures/encodes its actual evidence, and constructs
the complete extended journal and receipt. Only then does the slot swap owners.
A thrown input, scope, stale-revision, budget, capture or encoding error leaves
the old slot's journal, current/baseline state and revisions unchanged. There
is no automatic retry, widened scope or in-place rollback claim.

A normal `Solved=False` outcome is different: it is accepted, appended and
published. Inspect the returned receipt and currentness. Failed repair keeps
the appropriate pending scope; it does not erase earlier accepted edits.
`BeginEpoch` always appends recipe/run rows, even if their text is identical;
edit and repair append run rows; initial references the current applied row.

`PreviewRepair` returns `CanExecute`, `MissingBaseline`, the exact `RunText`,
publication/session revisions and detached `CopyScope`. It does not publish
or advance the live revision, but it **does** replay into a temporary candidate:
unlike decoding/inspection, preview is not graph-free. It is an observation,
not an executable authority object; `ExecuteRepair` revalidates its explicit
arguments and expected revision.

Returned execution, receipt, preview, copied model/run and copied public-state
objects are caller-owned. Free them. A receipt's `BorrowPublicState`,
`BorrowEditOutcome` and `BorrowSolveOutcome` are immutable borrows owned by that
receipt; never free them separately. An inapplicable edit/solve borrow is `nil`.
Receipts survive later publications and slot disposal. Copies from their state
or outcome survive receipt disposal too.

Context/journal `BorrowRecipe` and `BorrowRun` similarly last only as long as
their owner; `CopyRecipe`/`CopyRun` return independent caller-owned objects.
`ActionAt`, `CopyActions` and scope arrays are detached, including nested roots.
Replay borrows the input journal only during its call and retains its own
contexts/session recipe lease. The source journal may then be freed.

Slots and executions expose `CopyCanonicalJournal`, `CopyPublicState`,
`CopyLastSuccessfulState` and `CopyAppliedRun`; slots also expose
`CopyCurrentRecipe`. No mutable graph/session borrow escapes. Execution
`Revision` and slot `SessionRevision` describe the current epoch; they are not
the slot's monotonically advancing publication revision. `HasExecution` can be
true after begin while `HasSuccessfulBaseline` and `HasCurrentOutput` are false.

Use typed owners with ordinary caller-controlled lifetimes and synchronous,
single-threaded access to a slot. Raw pas2js records/arrays are checked for
passive fields and valid values, but proxies, forged owners, concurrent
publication and a modified global RTL are not supported security boundaries.

## Explicit limits

Initialize every field of `TWfcPipelineWorkspacePolicy`, including its own
`Version` and every nested `Version`, to supported version1. All other fields
must be positive finite `Integer` values. The library does not silently install
the CLI's defaults. Limits are caller-adjustable logical inventories, not a
fixed composition length, exact peak-memory allowance or wall-clock deadline.

| Policy member | Limits and what they bound |
| --- | --- |
| `Journal` | `MaxRecipes`, `MaxRuns`, `MaxContextTextBytes`, `MaxActions`, `MaxRootReferences`, `MaxEvidenceTextBytes`, `MaxEncodedTextBytes`: the entire retained history and its canonical outer text |
| `Replacement` | `MaxRetainedCellRecords`, `MaxRetainedValueItems`, `MaxCandidateVisits`: editable binding/input replacement storage and work, as specified in preparation |
| `Outcome` | `MaxPublicCellRecords`, `MaxEncodedTokenBytes`, `MaxReportPassRecords`, `MaxTraceEvents`, `MaxExcludedAssignmentItems`: one detached captured payload |
| `Evidence` | `MaxTextBytes`, `MaxLines`: one complete encoded actual edit/solve outcome |
| `Replay` | `MaxEpochs`, `MaxSolveActions`, `MaxInstantiatedCellRecords`, `MaxEvidenceTextBytes`: one whole-history replay's epochs, initial/repair attempts, summed epoch graph cells and claimed/actual evidence bytes |

Every pass's storage counts, including exact-copy aliases; graph cells are
summed once per begin-epoch action, not just for the final epoch. Context and
declared text/count budgets are checked before their corresponding large
decodes/clones where possible. Declared sizes are independently recomputed,
never trusted as allocation permission. Known replay limits precede graph
construction. Actual capture/encoding can still exceed limits after private
solver work and before publication.

Outcome/evidence limits bound captured or encoded output, not solver reports
already allocated by the core. Live and candidate graphs, retained canonical
text, drafts, last-successful copies and caller-held receipts are not one
collectively bounded peak heap. Lowering a policy below retained history refuses
the operation atomically. Raising it remains subject to integer and existing
artifact/solver limits; it does not guarantee feasibility or performance.
Current authoring and preview replay the full prior history on every call.
Checkpointing, compaction and an undo/editor interface are not implemented.

## A complete Pascal example

Save this as `workspace_example.lpr`. It creates a real four-cell rule pipeline,
records a same-token lock and clear separately, repairs, then decodes and
verifies the complete saved journal. The small one-token vocabulary makes the
example predictable; workspaces also accept composed/learned recipes and
independently sized mapped passes.

```pascal
program workspace_example;
{$mode delphi}{$H+}
uses SysUtils, wfc, wfc_model, wfc_rule_model, wfc_rule_text, wfc_sequence,
  wfc_pipeline_model, wfc_pipeline_run, wfc_pipeline_text,
  wfc_pipeline_run_text, wfc_pipeline_workspace_journal,
  wfc_pipeline_workspace_journal_text, wfc_pipeline_workspace_replay;
var
  Tokens: TWfcModelTokens;
  Weights: TWfcModelIntegerArray;
  Resources: TWfcPipelineResources;
  Passes: TWfcPipelinePasses;
  Locks: TWfcPipelineCellLocks;
  Roots: TGraphPassIndices;
  Rules: TWfcRuleModel;
  Recipe: TWfcPipelineModel;
  BaseRun, LockedRun: TWfcPipelineRun;
  Slot: TWfcPipelineWorkspaceSlot;
  Receipt: TWfcPipelineWorkspaceReceipt;
  Preview: TWfcPipelineWorkspaceRepairPreview;
  Journal: TWfcPipelineWorkspaceJournal;
  Verified: TWfcPipelineWorkspaceExecution;
  Policy: TWfcPipelineWorkspacePolicy;
  RecipeText, BaseText, LockedText, Saved: String;
begin
  Rules := nil; Recipe := nil; BaseRun := nil; LockedRun := nil;
  Slot := nil; Receipt := nil; Preview := nil; Journal := nil; Verified := nil;
  Policy.Version := 1;
  with Policy.Journal do begin
    Version := 1; MaxRecipes := 8; MaxRuns := 64;
    MaxContextTextBytes := 1048576; MaxActions := 64;
    MaxRootReferences := 128; MaxEvidenceTextBytes := 4194304;
    MaxEncodedTextBytes := 8388608;
  end;
  with Policy.Replacement do begin
    Version := 1; MaxRetainedCellRecords := 4096;
    MaxRetainedValueItems := 65536; MaxCandidateVisits := 1048576;
  end;
  with Policy.Outcome do begin
    Version := 1; MaxPublicCellRecords := 4096;
    MaxEncodedTokenBytes := 1048576; MaxReportPassRecords := 4096;
    MaxTraceEvents := 65536; MaxExcludedAssignmentItems := 65536;
  end;
  with Policy.Evidence do begin
    Version := 1; MaxTextBytes := 2097152; MaxLines := 65536;
  end;
  with Policy.Replay do begin
    Version := 1; MaxEpochs := 8; MaxSolveActions := 64;
    MaxInstantiatedCellRecords := 65536; MaxEvidenceTextBytes := 4194304;
  end;
  try
    SetLength(Tokens, 1); Tokens[0] := 'land';
    SetLength(Weights, 1); Weights[0] := 1;
    Rules := TWfcRuleModel.Create(1, Tokens, Weights, nil);
    SetLength(Resources, 1);
    Resources[0] := MakeWfcPipelineResource('terrain-rules', wprkRules,
      EncodeWfcRuleText(Rules), 'project-authored one-token example',
      'MIT', 'workspace-example-v1');
    SetLength(Passes, 1);
    Passes[0] := MakeWfcPipelinePass('terrain', wppvPublic, gpmOverlay,
      -1, wpakRules, 0, False, wseWhole);
    Recipe := TWfcPipelineModel.Create(
      MakeWfcPipelineMetadata('workspace example', 'MIT',
        'project-authored', 'workspace-example-v1'),
      1, False, rmBottomUp, Resources, Passes, nil, nil, nil);
    BaseRun := TWfcPipelineRun.Create(Recipe, 4, 1, 1, 7,
      wpssOneWay, 64, 0, True, nil, nil);
    SetLength(Locks, 1);
    Locks[0] := MakeWfcPipelineCellLock(0, 0, 0, 0, 'land');
    LockedRun := TWfcPipelineRun.Create(Recipe, 4, 1, 1, 7,
      wpssOneWay, 64, 0, True, Locks, nil);
    RecipeText := EncodeWfcPipelineModelText(Recipe);
    BaseText := EncodeWfcPipelineRunText(BaseRun);
    LockedText := EncodeWfcPipelineRunText(LockedRun);
    Slot := TWfcPipelineWorkspaceSlot.Create;
    Receipt := Slot.BeginEpoch(RecipeText, BaseText, Policy, 0);
    FreeAndNil(Receipt);
    Receipt := Slot.ExecuteInitial(Policy, Slot.PublicationRevision);
    if not Receipt.BorrowSolveOutcome.Solved then
      raise Exception.Create('Initial attempt did not solve');
    FreeAndNil(Receipt);
    Receipt := Slot.ApplyInputs(LockedText, Policy, Slot.PublicationRevision);
    FreeAndNil(Receipt);
    Receipt := Slot.ApplyInputs(BaseText, Policy, Slot.PublicationRevision);
    FreeAndNil(Receipt); { Clear is a separate accepted edit, not omitted history. }
    SetLength(Roots, 1); Roots[0] := 0;
    Preview := Slot.PreviewRepair(BaseText, Roots, Policy,
      Slot.PublicationRevision);
    if not Preview.CanExecute then raise Exception.Create('Scope refused');
    FreeAndNil(Preview);
    Receipt := Slot.ExecuteRepair(BaseText, Roots, Policy,
      Slot.PublicationRevision);
    if not Receipt.HasCurrentOutput then raise Exception.Create('Not current');
    FreeAndNil(Receipt);
    Saved := Slot.CopyCanonicalJournal;
    Journal := DecodeWfcPipelineWorkspaceJournalText(Saved, Policy.Journal);
    if Journal.Verification <> wpwvUnverifiedClaims then
      raise Exception.Create('Decode must not claim execution');
    Verified := ReplayWfcPipelineWorkspace(Journal, Policy.Journal,
      Policy.Replacement, Policy.Outcome, Policy.Evidence, Policy.Replay);
    FreeAndNil(Journal); FreeAndNil(Slot);
    WriteLn('Verified actions: ', Verified.VerifiedActions);
    WriteLn('Current output: ', Verified.HasCurrentOutput);
  finally
    Verified.Free; Journal.Free; Preview.Free; Receipt.Free; Slot.Free;
    LockedRun.Free; BaseRun.Free; Recipe.Free; Rules.Free;
  end;
end.
```

Compile from the repository root with `fpc -Mdelphi -Sa -Cr -Co -Ci -Fusrc
workspace_example.lpr`, preferably supplying separate `-FU`/`-FE` output
directories as in the [build guide](building.md). Installed consumers instead
use their selected package unit directory, with no source fallback. The program
records five actions; replay retains current output after its source slot and
decoded journal are freed.

## Canonical journal and evidence formats

`EncodeWfcPipelineWorkspaceJournalText` returns the journal's canonical text;
`DecodeWfcPipelineWorkspaceJournalText(Text, Limits)` checks and owns it.
Constructor static errors use `EWfcPipelineWorkspaceJournal`; malformed codec
input uses `EConvertError` with the WFC workspace journal prefix. Allocation
failures are not converted into successful validation.

The closed outer grammar begins with these ordered lines:

```text
wfc-workspace-journal=1
verification=unverified-claims
recipe-count=N
run-count=N
action-count=N
root-reference-count=N
context-text-bytes=N
evidence-text-bytes=N
```

Then come contiguous zero-based recipe rows (`recipe.I.bytes`, `recipe.I.text`),
run rows (`run.I.recipe`, `run.I.bytes`, `run.I.text`), and action rows
(`action.I.kind`, `action.I.run`, `action.I.root-count`, every
`action.I.root.J`, `action.I.evidence-bytes`, `action.I.evidence`), followed by
exactly `end=1` and final LF. `N`, `I` and `J` here describe the grammar, not
literal file contents. Kinds are `begin-epoch`, `edit`, `initial`, `repair`.
Text values use the maintained canonical percent-token codec; byte counts
refer to decoded complete text. Counts/lengths, row order, canonical integers,
remaining input and exact re-encoding are checked. BOM, CR, noncanonical token
encodings and trailing fields are not normalized into acceptance. Policy is
supplied separately, not trusted from the document.

Only begin-epoch evidence is empty. Other evidence must have the exact
`wfc-session-evidence=1` header, matching kind, printable ASCII fields and LF
termination, with its bytes retained unchanged. The writers
`EncodeWfcPipelineSessionEditEvidence` and
`EncodeWfcPipelineSessionOutcomeEvidence` accept actual typed detached outcomes.
Solve kinds distinguish ordinary/negotiated full/selective execution. Complete
reports retain actual failure ordinals, traces, excluded assignments, pending
scope and state/currentness; capture-disabled operations do not invent traces.
These are evidence documents, not fresh result1/result2 or an imported trusted
report model. See the unverified/replay distinction above.

`EWfcPipelineWorkspaceReplay` distinguishes inputs, budget, scope, evidence
mismatch, unverified-owner and stale-publication refusals. `ActionIndex` is
zero-based (`-1` when none applies); `MismatchOffset` is one-based in canonical
ASCII text (`0` when no text mismatch applies). Keep these diagnostics distinct
from a normally returned unsuccessful solve.

## Native CLI

The maintained native build produces `build/native/bin/wfc_workspace[.exe]`.
`--help` or `help` must appear alone; this command has no `--version` option.

```text
wfc_workspace inspect --input JOURNAL
wfc_workspace replay --input JOURNAL
wfc_workspace begin [--input OLD_JOURNAL] --recipe RECIPE --run RUN --output NEW_JOURNAL
wfc_workspace edit --input JOURNAL --run RUN --output NEW_JOURNAL
wfc_workspace initial --input JOURNAL --output NEW_JOURNAL
wfc_workspace preview --input JOURNAL --run RUN --roots 0,2
wfc_workspace repair --input JOURNAL --run RUN --roots 0,2 --output NEW_JOURNAL
```

`inspect` is graph-free and explicitly reports unverified claims. All other
commands with an input journal restore/replay its entire history first, even
`preview` and `begin --input`. `replay` retains/inspects verified execution in
the command process; it does not rewrite a file. These commands do not extend
the families accepted by `wfc_validate`, `wfc_inspect` or `wfc_run`.

All paths are explicit files; there is no stdin/URL mode or special `--`
separator. Unknown, duplicate, empty and command-inapplicable options reject.
Root lists must already be ascending/unique canonical decimal pass indices.
Each authoring output must be a **new file** in an existing parent directory;
no existing destination is overwritten, including an input journal. Publication
uses the included FPC [new-file helper](development-tools.md), not a shell copy.
It protects an existing destination if it appears during publication too.
This is not a crash-durable filesystem transaction or a hostile-filesystem
sandbox; cleanup failures are reported, not hidden.

| Exit | Meaning |
| --- | --- |
| 0 | Help, accepted inspection/authoring or fully verified replay; not a promise that all historical solves succeeded |
| 2 | Invalid command-line usage |
| 3 | Invalid artifact/input/policy or other non-I/O validation failure |
| 4 | Replay/authoring refusal, including evidence mismatch or insufficient preview/repair scope |
| 5 | File/stream I/O failure |
| 10 | A normal unsolved initial/repair attempt was successfully recorded in the new output |

Exit10 is intentionally not the fresh runner's exit4. Inspect the new journal;
do not discard it or treat every nonzero child exit as an accepted solve. A
refused operation writes no new journal. File publication can fail after
private solver work, without changing the original input or existing output.

### Adjustable host policy

Append `--limit NAME=N`, repeatable for different names. Values are positive
canonical decimals fitting `Integer`. These are CLI defaults, not library
constants or application output-length targets:

| Policy | CLI name=default |
| --- | --- |
| Journal | `recipes=1024`, `runs=16384`, `context-bytes=67108864`, `actions=16384`, `roots=65536`, `evidence-bytes=67108864`, `journal-bytes=268435456` |
| Replacement | `input-cells=1048576`, `input-values=4194304`, `input-visits=64000000` |
| Outcome capture | `public-cells=1048576`, `token-bytes=16777216`, `report-passes=65536`, `trace-events=1048576`, `excluded-values=4194304` |
| Evidence encoding | `outcome-bytes=33554432`, `outcome-lines=1048576` |
| Whole-history replay | `epochs=1024`, `solves=16384`, `instantiated-cells=16777216`, `replay-evidence-bytes=67108864` |

For example, `--limit actions=32768 --limit journal-bytes=536870912` raises
those two allowances, not the other limits. Larger histories may require
coordinated changes to several inventories.

### Walk through actual generated artifacts

After the [native build](building.md), run the maintained fixture in a fresh
directory. It authors real rules and learned-sequence contexts, not hand-edited
solver claims. On Windows, from the repository root:

```powershell
New-Item -ItemType Directory build/workspace-demo | Out-Null
.\build\native\bin\wfc_workspace_cli_fixture.exe --make build/workspace-demo
Push-Location build/workspace-demo
try {
  $workspaceTool = '../native/bin/wfc_workspace.exe'
  & $workspaceTool begin --recipe recipe0.wfc --run run0.wfc --output 00.journal
  & $workspaceTool initial --input 00.journal --output 01.journal
  & $workspaceTool edit --input 01.journal --run run1.wfc --output 02.journal
  & $workspaceTool edit --input 02.journal --run run0.wfc --output 03.journal
  & $workspaceTool preview --input 03.journal --run run0.wfc --roots 3
  # Expected exit4: the public alias cannot authorize its private provider.
  & $workspaceTool repair --input 03.journal --run run0.wfc --roots 1 --output 04.journal
  & $workspaceTool inspect --input 04.journal
  & $workspaceTool replay --input 04.journal
} finally { Pop-Location }
```

Use a new directory on a subsequent run; the fixture and authoring commands
will not replace existing files. On Linux/macOS use the same arguments with
`./build/native/bin/wfc_workspace_cli_fixture` and `wfc_workspace`, without
`.exe`. Inside `build/workspace-demo`, the tool path is
`../native/bin/wfc_workspace`.

`run1.wfc` applies a same-token lock to public alias pass3; `run0.wfc` clears
it. They remain separate edits. Root1 explicitly authorizes the private learned
provider and its downstream projection/alias, without touching unrelated pass0.

For a recorded failure, edit `04.journal` with `run2.wfc` into `05.journal`,
then repair `05.journal` with that same run and `--roots 0 --output 06.journal`.
`run2.wfc` contains an explicit empty public domain. The repair returns exit10
and writes `06.journal`; replaying that journal returns exit0 with noncurrent
output and a retained successful baseline. Clear with `run0.wfc` and repair
root0 to recover. Starting `begin --input 04.journal --recipe recipe1.wfc
--run run3.wfc --output epoch2.journal` changes seed/extents in a new epoch
without erasing history. A new initial attempt is required there.

The fixture also writes `direct.journal` and a deliberately changed,
well-enveloped `forged.journal`. Inspecting the latter reports unverified
claims; replay refuses its evidence mismatch. They are test artifacts, not
examples of trusted serialized execution.

## Verification and remaining work

The portable context, evidence, journal and replay suites cover strict shape/
codec boundaries, ownership after producer disposal, actual learned and mapped
contexts, all four solve report kinds, explicit scope, budgets, cumulative
history, failed outcomes, atomic refusal and multiple epochs. A native process
suite invokes the actual seven-command CLI, checking exact statuses and file
preservation. The installed-package consumer exercises the same exported APIs
without a source fallback.

Focused checked FPC verification ran on stable3.2.2 and trunk3.3.1, Win32 and
Win64. Actual maintained FPC-hosted pas2js execution observed 245 journal and
7,069 combined replay/authoring assertions. Thirteen complete journal documents
(276,971 canonical bytes) matched native output byte for byte; a separate
evidence gate compared 53 complete documents. These are functional/portability
checks, not browser visual review, universal solver success, or evidence that
every later full build/hosted CI job has already passed.

General interactive workspace UIs, demo migrations, history compaction,
checkpoint acceleration and concurrent editors remain separate work. This
format deliberately preserves every accepted operation rather than pretending
that a saved final recipe/run can reconstruct edit ownership and selective reuse.
