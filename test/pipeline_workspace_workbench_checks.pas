{ SPDX-License-Identifier: MIT
  Copyright (c) 2021 mr-highball
  Independent controller checks. Expected edits are reconstructed as
  complete typed runs and applied to a separate real prepared session. }
unit pipeline_workspace_workbench_checks;
{$mode delphi}{$H+}
interface
procedure TestPipelineWorkspaceWorkbench;
implementation
uses SysUtils,wfc,wfc_model,wfc_lattice,wfc_rule_model,wfc_rule_text,wfc_sequence,
  wfc_pipeline_layout,wfc_pipeline_model,wfc_pipeline_run,
  wfc_pipeline_run_text,wfc_pipeline_text,wfc_pipeline_prepare,
  wfc_pipeline_session,wfc_pipeline_session_evidence,
  wfc_pipeline_workspace_context,wfc_pipeline_workspace_journal,
  wfc_pipeline_workspace_journal_text,wfc_pipeline_workspace_replay,
  pipeline_workspace_workbench,wfc_workspace_replay_fixture;

var FixtureRecipeText,FixtureRunText: String;

function Policy: TWfcPipelineWorkspacePolicy;
begin
  Result:=Default(TWfcPipelineWorkspacePolicy); Result.Version:=1;
  Result.Journal:=JournalLimits; Result.Replacement:=ReplacementLimits;
  Result.Outcome:=OutcomeLimits; Result.Evidence:=EvidenceLimits;
  Result.Replay.Version:=1; Result.Replay.MaxEpochs:=20;
  Result.Replay.MaxSolveActions:=100; Result.Replay.MaxInstantiatedCellRecords:=100000;
  Result.Replay.MaxEvidenceTextBytes:=8000000;
end;

function Tokens(const Values: array of TWfcModelToken): TWfcModelTokens;
var I: Integer;
begin
  Result:=nil; SetLength(Result,Length(Values));
  for I:=0 to High(Values) do Result[I]:=Values[I];
end;

function Roots(const Values: array of Integer): TGraphPassIndices;
var I: Integer;
begin
  Result:=nil; SetLength(Result,Length(Values));
  for I:=0 to High(Values) do Result[I]:=Values[I];
end;

function AppliedText(const W: TWfcPipelineWorkspaceWorkbench): String;
var R: TWfcPipelineRun;
begin
  R:=W.CopyAppliedRun;
  try Result:=EncodeWfcPipelineRunText(R); finally R.Free; end;
end;

function NewRun(const M: TWfcPipelineModel; const Base: TWfcPipelineRun;
  const Locks: TWfcPipelineCellLocks; const Domains: TWfcPipelineCellDomains): TWfcPipelineRun;
begin
  if Base.FormatVersion=WFC_PIPELINE_RUN_VERSION then
    Result:=TWfcPipelineRun.Create(M,Base.Width,Base.Height,Base.Depth,Base.Seed,
      Base.Strategy,Base.MaxBacktracks,Base.MaxPassBacktracks,Base.CaptureTrace,Locks,Domains)
  else Result:=TWfcPipelineRun.Create(M,Base.CopyPassExtents,Base.Seed,Base.Strategy,
    Base.MaxBacktracks,Base.MaxPassBacktracks,Base.CaptureTrace,Locks,Domains);
end;

procedure Start(const W: TWfcPipelineWorkspaceWorkbench; const RunText: String;
  const Solve: Boolean);
var Receipt: TWfcPipelineWorkspaceReceipt;
begin
  Receipt:=W.BeginEpoch(FixtureRecipeText,RunText,Policy,W.PublicationRevision);
  try Check(Receipt.Kind=wpwakBeginEpoch,'controller begins actual epoch');
  finally Receipt.Free; end;
  if Solve then
  begin
    Receipt:=W.ExecuteInitial(Policy,W.PublicationRevision);
    try Check(Receipt.BorrowSolveOutcome.Solved,'controller actual initial solves');
    finally Receipt.Free; end;
  end;
end;

function CellToken(const W: TWfcPipelineWorkspaceWorkbench;
  const PassIndex,CellIndex: Integer): TWfcModelToken;
var S: TWfcPipelineSessionPublicState; L: TWfcPipelineSessionLayer; I: Integer;
begin
  S:=W.CopyPublicState;
  try
    for I:=0 to S.LayerCount-1 do
    begin
      L:=S.LayerAt(I);
      if L.PassIndex=PassIndex then
      begin
        Check(not L.Cells[CellIndex].Empty,'requested generated fixture cell exists');
        Result:=L.Cells[CellIndex].Token; Exit;
      end;
    end;
    raise Exception.Create('fixture public pass absent');
  finally S.Free; end;
end;

type
  TSnapshot = class
    Journal,RunText: String;
    Publication,Revision,RecipeIndex,RunIndex: Integer;
    Current,BaselinePresent,Execution: Boolean;
    State,Baseline: TWfcPipelineSessionPublicState;
    constructor Create(const W: TWfcPipelineWorkspaceWorkbench);
    destructor Destroy; override;
    procedure CheckUnchanged(const W: TWfcPipelineWorkspaceWorkbench);
  end;

constructor TSnapshot.Create(const W: TWfcPipelineWorkspaceWorkbench);
begin
  inherited Create;
  Journal:=W.CopyCanonicalJournal; RunText:=AppliedText(W);
  Publication:=W.PublicationRevision; Revision:=W.SessionRevision;
  RecipeIndex:=W.CurrentRecipeIndex; RunIndex:=W.CurrentRunIndex;
  Execution:=W.HasExecution; Current:=W.HasCurrentOutput;
  BaselinePresent:=W.HasSuccessfulBaseline;
  State:=W.CopyPublicState; Baseline:=W.CopyLastSuccessfulState;
end;

destructor TSnapshot.Destroy;
begin Baseline.Free; State.Free; inherited Destroy; end;

procedure TSnapshot.CheckUnchanged(const W: TWfcPipelineWorkspaceWorkbench);
var S: TWfcPipelineSessionPublicState;
begin
  Check(W.CopyCanonicalJournal=Journal,'refusal retains every journal byte');
  Check(AppliedText(W)=RunText,'refusal retains complete ordered applied run');
  Check((W.PublicationRevision=Publication) and (W.SessionRevision=Revision),
    'refusal retains publication/session revisions');
  Check((W.CurrentRecipeIndex=RecipeIndex) and (W.CurrentRunIndex=RunIndex),
    'refusal retains exact indexed context bindings');
  Check((W.HasExecution=Execution) and (W.HasCurrentOutput=Current) and
    (W.HasSuccessfulBaseline=BaselinePresent),'refusal retains lifecycle flags');
  S:=W.CopyPublicState; try EqualPublicState(State,S); finally S.Free; end;
  S:=W.CopyLastSuccessfulState; try EqualPublicState(Baseline,S); finally S.Free; end;
end;

procedure CheckEdit(const W: TWfcPipelineWorkspaceWorkbench;
  const Receipt: TWfcPipelineWorkspaceReceipt; const Direct: TWfcPipelinePreparedSession;
  const ExpectedRun: TWfcPipelineRun);
var O: TWfcPipelineSessionEditOutcome; S,B: TWfcPipelineSessionPublicState;
  J: TWfcPipelineWorkspaceJournal; A: TWfcPipelineWorkspaceAction; Text: String;
begin
  O:=nil; S:=nil; B:=nil; J:=nil;
  try
    O:=Direct.ApplyInputs(ExpectedRun); Text:=EncodeWfcPipelineRunText(ExpectedRun);
    Check(AppliedText(W)=Text,'helper exactly reconstructs all ordered input rows/options/extents');
    Check(Receipt.Kind=wpwakEdit,'cell helper records actual edit action');
    Check(Receipt.EvidenceText=EncodeWfcPipelineSessionEditEvidence(O,EvidenceLimits),
      'complete helper edit evidence equals independent actual session');
    Check((Receipt.SessionRevision=Direct.Revision) and (W.SessionRevision=Direct.Revision),
      'helper and direct accepted revision match');
    Check((W.HasCurrentOutput=Direct.HasCurrentOutput) and
      (W.HasSuccessfulBaseline=Direct.HasSuccessfulBaseline),'helper/direct lifecycle match');
    S:=Direct.CopyPublicState; EqualPublicState(S,Receipt.BorrowPublicState); FreeAndNil(S);
    S:=Direct.CopyLastSuccessfulState;
    B:=W.CopyLastSuccessfulState; EqualPublicState(S,B); FreeAndNil(B);
    J:=DecodeWfcPipelineWorkspaceJournalText(W.CopyCanonicalJournal,JournalLimits);
    A:=J.ActionAt(J.ActionCount-1);
    Check((A.Kind=wpwakEdit) and (A.EvidenceText=Receipt.EvidenceText),'journal retains actual edit evidence');
    Check(J.RunTextAt(A.RunIndex).Text=Text,'journal binds entire expected invocation');
    Check(Receipt.ActionIndex=J.ActionCount-1,'receipt identifies exact appended action');
  finally J.Free; B.Free; S.Free; O.Free; end;
end;

procedure TestCompleteCellEdits;
var W: TWfcPipelineWorkspaceWorkbench; M: TWfcPipelineModel;
  Base,Expected: TWfcPipelineRun; Direct: TWfcPipelinePreparedSession;
  Initial: TWfcPipelineSessionOutcome; Receipt: TWfcPipelineWorkspaceReceipt;
  Locks: TWfcPipelineCellLocks; Domains: TWfcPipelineCellDomains;
  Vocabulary: TWfcModelTokens; Token: TWfcModelToken;
  procedure CheckAndRelease;
  begin
    Expected:=NewRun(M,Base,Locks,Domains);
    try CheckEdit(W,Receipt,Direct,Expected);
    finally FreeAndNil(Expected); FreeAndNil(Receipt); end;
  end;
begin
  W:=nil; M:=nil; Base:=nil; Expected:=nil; Direct:=nil; Initial:=nil; Receipt:=nil;
  try
    M:=DecodeWfcPipelineModelText(FixtureRecipeText);
    Base:=DecodeWfcPipelineRunText(FixtureRunText,M); Vocabulary:=M.CopyPublicVocabulary(3);
    Check((M.PassAt(1).Visibility=wppvPrivate) and (M.PassAt(3).Mode=gpmTransform),
      'fixture retains real private learned provider and public alias');
    SetLength(Locks,1); Locks[0]:=MakeWfcPipelineCellLock(0,1,0,0,'U');
    SetLength(Domains,2); Domains[0]:=MakeWfcPipelineCellDomain(0,0,0,0,Tokens(['U','V']));
    Domains[1]:=MakeWfcPipelineCellDomain(3,2,0,0,Vocabulary);
    Expected:=NewRun(M,Base,Locks,Domains); W:=TWfcPipelineWorkspaceWorkbench.Create;
    Start(W,EncodeWfcPipelineRunText(Expected),True);
    Direct:=TWfcPipelinePreparedSession.Create(M,Expected,ReplacementLimits,OutcomeLimits);
    Initial:=Direct.ExecuteInitial; Check(Initial.Solved,'independent custom-input initial solves');
    FreeAndNil(Initial); FreeAndNil(Expected); Token:=CellToken(W,3,0);
    Receipt:=W.SetCellLock(3,0,0,0,Token,Policy,W.PublicationRevision);
    SetLength(Locks,2); Locks[1]:=MakeWfcPipelineCellLock(3,0,0,0,Token); CheckAndRelease;
    Receipt:=W.ClearCellLock(3,0,0,0,Policy,W.PublicationRevision);
    SetLength(Locks,1); CheckAndRelease;
    Receipt:=W.ClearCellLock(3,0,0,0,Policy,W.PublicationRevision); CheckAndRelease;
    Receipt:=W.SetCellLock(0,0,0,0,'U',Policy,W.PublicationRevision);
    SetLength(Locks,2); Locks[1]:=Locks[0]; Locks[0]:=MakeWfcPipelineCellLock(0,0,0,0,'U'); CheckAndRelease;
    Receipt:=W.SetCellLock(0,0,0,0,'V',Policy,W.PublicationRevision);
    Locks[0].Token:='V'; CheckAndRelease;
    Receipt:=W.ClearCellLock(0,0,0,0,Policy,W.PublicationRevision);
    Locks[0]:=Locks[1]; SetLength(Locks,1); CheckAndRelease;
    Receipt:=W.SetCellDomain(0,1,0,0,Tokens(['U','V']),Policy,W.PublicationRevision);
    SetLength(Domains,3); Domains[2]:=Domains[1];
    Domains[1]:=MakeWfcPipelineCellDomain(0,1,0,0,Tokens(['U','V'])); CheckAndRelease;
    Receipt:=W.SetCellDomain(0,1,0,0,Tokens(['U']),Policy,W.PublicationRevision);
    Domains[1].AllowedTokens:=Tokens(['U']); CheckAndRelease;
    Receipt:=W.ClearCellDomain(0,1,0,0,Policy,W.PublicationRevision);
    Domains[1]:=Domains[2]; SetLength(Domains,2); CheckAndRelease;
    Receipt:=W.ClearCellDomain(0,1,0,0,Policy,W.PublicationRevision); CheckAndRelease;
    Receipt:=W.SetCellDomain(0,0,0,0,nil,Policy,W.PublicationRevision);
    Domains[0].AllowedTokens:=nil; CheckAndRelease;
    Expected:=W.CopyAppliedRun;
    Check((Expected.DomainCount=2) and (Length(Expected.DomainAt(0).AllowedTokens)=0),
      'explicit empty domain remains present, not absent'); FreeAndNil(Expected);
    Receipt:=W.ClearCellDomain(0,0,0,0,Policy,W.PublicationRevision);
    Domains[0]:=Domains[1]; SetLength(Domains,1); CheckAndRelease;
    ExportReplayJournal(W.CopyCanonicalJournal);
  finally
    Receipt.Free; Initial.Free; Direct.Free; Expected.Free; Base.Free; M.Free; W.Free;
  end;
end;

type TCellOperation = (coSetLock,coClearLock,coSetDomain,coClearDomain);

procedure ExpectCellFailure(const W: TWfcPipelineWorkspaceWorkbench;
  const Operation: TCellOperation; const PassIndex,X,Y,Z: Integer;
  const Token: TWfcModelToken; const Allowed: TWfcModelTokens;
  const P: TWfcPipelineWorkspacePolicy; const Expected: Integer;
  const Stale: Boolean=False);
var Before: TSnapshot; Receipt: TWfcPipelineWorkspaceReceipt; Rejected: Boolean;
begin
  Before:=TSnapshot.Create(W); Receipt:=nil; Rejected:=False;
  try
    try
      case Operation of
        coSetLock:Receipt:=W.SetCellLock(PassIndex,X,Y,Z,Token,P,Expected);
        coClearLock:Receipt:=W.ClearCellLock(PassIndex,X,Y,Z,P,Expected);
        coSetDomain:Receipt:=W.SetCellDomain(PassIndex,X,Y,Z,Allowed,P,Expected);
        coClearDomain:Receipt:=W.ClearCellDomain(PassIndex,X,Y,Z,P,Expected);
      end;
    except
      on E: EWfcPipelineWorkspaceReplay do
      begin
        Rejected:=True;
        if Stale then Check(E.Kind=wpwrfStalePublication,'stale revision takes precedence');
      end;
      on E: EWfcPipelineWorkspaceWorkbench do begin if Stale then raise; Rejected:=True; end;
      on E: EWfcPipelineRun do begin if Stale then raise; Rejected:=True; end;
      on E: EWfcPipelineWorkspaceJournal do begin if Stale then raise; Rejected:=True; end;
      on E: EWfcPipelineSession do begin if Stale then raise; Rejected:=True; end;
      on E: EWfcPipelineSessionEvidence do begin if Stale then raise; Rejected:=True; end;
      on E: EConvertError do begin if Stale then raise; Rejected:=True; end;
    end;
    Check(Rejected and (Receipt=nil),'invalid helper input produces typed refusal and no receipt');
    Before.CheckUnchanged(W);
  finally Receipt.Free; Before.Free; end;
end;

procedure TestCellRefusals;
var W: TWfcPipelineWorkspaceWorkbench; Receipt: TWfcPipelineWorkspaceReceipt;
  P: TWfcPipelineWorkspacePolicy; Op: TCellOperation;
begin
  W:=TWfcPipelineWorkspaceWorkbench.Create; Receipt:=nil;
  try
    Start(W,FixtureRunText,True); P:=Policy;
    for Op:=Low(TCellOperation) to High(TCellOperation) do
    begin
      ExpectCellFailure(W,Op,-1,0,0,0,'U',Tokens(['U']),P,W.PublicationRevision);
      ExpectCellFailure(W,Op,4,0,0,0,'U',Tokens(['U']),P,W.PublicationRevision);
      ExpectCellFailure(W,Op,1,0,0,0,'U',Tokens(['U']),P,W.PublicationRevision);
      ExpectCellFailure(W,Op,0,-1,0,0,'U',Tokens(['U']),P,W.PublicationRevision);
      ExpectCellFailure(W,Op,0,2,0,0,'U',Tokens(['U']),P,W.PublicationRevision);
      ExpectCellFailure(W,Op,0,0,1,0,'U',Tokens(['U']),P,W.PublicationRevision);
      ExpectCellFailure(W,Op,0,0,0,1,'U',Tokens(['U']),P,W.PublicationRevision);
      ExpectCellFailure(W,Op,-1,-1,0,0,'?',Tokens(['?']),P,W.PublicationRevision-1,True);
    end;
    ExpectCellFailure(W,coSetLock,0,0,0,0,'',nil,P,W.PublicationRevision);
    ExpectCellFailure(W,coSetLock,0,0,0,0,'A',nil,P,W.PublicationRevision);
    ExpectCellFailure(W,coSetDomain,0,0,0,0,'',Tokens(['U','U']),P,W.PublicationRevision);
    ExpectCellFailure(W,coSetDomain,0,0,0,0,'',Tokens(['V','U']),P,W.PublicationRevision);
    ExpectCellFailure(W,coSetDomain,0,0,0,0,'',Tokens(['U','?']),P,W.PublicationRevision);
    ExpectCellFailure(W,coSetDomain,0,0,0,0,'',Tokens(['']),P,W.PublicationRevision);
    Receipt:=W.SetCellLock(0,0,0,0,'U',P,W.PublicationRevision); FreeAndNil(Receipt);
    ExpectCellFailure(W,coSetDomain,0,0,0,0,'',Tokens(['V']),P,W.PublicationRevision);
    ExpectCellFailure(W,coSetDomain,0,0,0,0,'',nil,P,W.PublicationRevision);
    Receipt:=W.SetCellDomain(0,0,0,0,Tokens(['U']),P,W.PublicationRevision); FreeAndNil(Receipt);
    ExpectCellFailure(W,coSetLock,0,0,0,0,'V',nil,P,W.PublicationRevision);
  finally Receipt.Free; W.Free; end;
end;

procedure ExpectRepairFailure(const W: TWfcPipelineWorkspaceWorkbench;
  const Requested: TGraphPassIndices);
var Before: TSnapshot; R: TWfcPipelineWorkspaceReceipt; Rejected: Boolean;
begin
  Before:=TSnapshot.Create(W); R:=nil; Rejected:=False;
  try
    try R:=W.ExecuteRepair(AppliedText(W),Requested,Policy,W.PublicationRevision);
    except on E: EWfcPipelineWorkspaceReplay do
      begin Rejected:=True; Check(E.Kind=wpwrfScope,'repair is refused as insufficient scope/baseline'); end; end;
    Check(Rejected and (R=nil),'refused repair does not author a fake solve'); Before.CheckUnchanged(W);
  finally R.Free; Before.Free; end;
end;

procedure TestHistoryAndFailure;
var A,B,F: TWfcPipelineWorkspaceWorkbench; R,Saved: TWfcPipelineWorkspaceReceipt;
  P: TWfcPipelineWorkspaceRepairPreview; Token: TWfcModelToken;
  J: TWfcPipelineWorkspaceJournal; Before: TSnapshot;
  State,Baseline,SavedState: TWfcPipelineSessionPublicState; SavedEvidence: String;
begin
  A:=nil; B:=nil; F:=nil; R:=nil; Saved:=nil; P:=nil; J:=nil;
  Before:=nil; State:=nil; Baseline:=nil; SavedState:=nil;
  try
    A:=TWfcPipelineWorkspaceWorkbench.Create; B:=TWfcPipelineWorkspaceWorkbench.Create;
    Start(A,FixtureRunText,False); Saved:=A.ExecuteInitial(Policy,A.PublicationRevision);
    Check(Saved.BorrowSolveOutcome.Solved,'retained receipt has actual solved outcome');
    SavedEvidence:=Saved.EvidenceText; SavedState:=A.CopyPublicState;
    Start(B,FixtureRunText,True); Token:=CellToken(A,3,0);
    R:=A.SetCellLock(3,0,0,0,Token,Policy,A.PublicationRevision); FreeAndNil(R);
    R:=A.ClearCellLock(3,0,0,0,Policy,A.PublicationRevision); FreeAndNil(R);
    R:=B.ApplyRun(FixtureRunText,Policy,B.PublicationRevision); FreeAndNil(R);
    Check(AppliedText(A)=AppliedText(B),'separate history and coalesced no-op end at identical complete inputs');
    Check((A.PublicationRevision=4) and (B.PublicationRevision=3),'lock and clear are separate accepted actions');
    J:=DecodeWfcPipelineWorkspaceJournalText(A.CopyCanonicalJournal,JournalLimits);
    Check((J.ActionCount=4) and (J.ActionAt(2).Kind=wpwakEdit) and
      (J.ActionAt(3).Kind=wpwakEdit),'journal does not coalesce accepted lock/clear'); FreeAndNil(J);
    Before:=TSnapshot.Create(A);
    P:=A.PreviewRepair(AppliedText(A),Roots([3]),Policy,A.PublicationRevision);
    Check(not P.CanExecute and not P.MissingBaseline,'alias-only repair cannot cover private provider');
    Check(Length(P.CopyScope.MissingPassIndices)>0,'preview exposes missing impacted passes');
    Before.CheckUnchanged(A); FreeAndNil(Before); FreeAndNil(P);
    ExpectRepairFailure(A,Roots([3]));
    P:=B.PreviewRepair(AppliedText(B),Roots([3]),Policy,B.PublicationRevision);
    Check(P.CanExecute,'coalesced no-op has no private pending history'); FreeAndNil(P);
    R:=B.ExecuteRepair(AppliedText(B),Roots([3]),Policy,B.PublicationRevision);
    Check(R.BorrowSolveOutcome.Solved,'coalesced history leaf repair actually succeeds'); FreeAndNil(R);
    R:=A.ExecuteRepair(AppliedText(A),Roots([1]),Policy,A.PublicationRevision);
    Check(R.BorrowSolveOutcome.Solved and A.HasCurrentOutput,'explicit private provider repair recovers learned projection'); FreeAndNil(R);
    Baseline:=A.CopyLastSuccessfulState;
    R:=A.SetCellDomain(0,0,0,0,nil,Policy,A.PublicationRevision); FreeAndNil(R);
    R:=A.ExecuteRepair(AppliedText(A),Roots([0]),Policy,A.PublicationRevision);
    Check(not R.BorrowSolveOutcome.Solved and not A.HasCurrentOutput and A.HasSuccessfulBaseline,
      'normal failed repair is accepted history with retained baseline'); FreeAndNil(R);
    State:=A.CopyLastSuccessfulState; EqualPublicState(Baseline,State); FreeAndNil(State);
    ExportReplayJournal(A.CopyCanonicalJournal);
    F:=TWfcPipelineWorkspaceWorkbench.Create;
    F.RestoreJournal(A.CopyCanonicalJournal,Policy,F.PublicationRevision);
    Check(not F.HasCurrentOutput and F.HasSuccessfulBaseline,'failed-terminal journal restores as real verified history');
    State:=F.CopyLastSuccessfulState; EqualPublicState(Baseline,State); FreeAndNil(State); FreeAndNil(F);
    F:=TWfcPipelineWorkspaceWorkbench.Create; Start(F,FixtureRunText,False);
    R:=F.SetCellDomain(0,0,0,0,nil,Policy,F.PublicationRevision); FreeAndNil(R);
    R:=F.ExecuteInitial(Policy,F.PublicationRevision);
    Check(not R.BorrowSolveOutcome.Solved and not F.HasSuccessfulBaseline,'failed initial records no invented baseline'); FreeAndNil(R);
    P:=F.PreviewRepair(AppliedText(F),Roots([0]),Policy,F.PublicationRevision);
    Check(not P.CanExecute and P.MissingBaseline,'failed-initial repair remains inspectable but not executable'); FreeAndNil(P);
    ExpectRepairFailure(F,Roots([0])); ExportReplayJournal(F.CopyCanonicalJournal);
    FreeAndNil(F); FreeAndNil(A); FreeAndNil(B);
    Check(Saved.EvidenceText=SavedEvidence,'saved receipt evidence survives controller disposal');
    EqualPublicState(SavedState,Saved.BorrowPublicState);
    State:=Saved.BorrowSolveOutcome.CopyPublicState; EqualPublicState(SavedState,State);
  finally
    State.Free; SavedState.Free; Baseline.Free; Before.Free; J.Free;
    P.Free; Saved.Free; R.Free; F.Free; B.Free; A.Free;
  end;
end;

procedure TestInspectAndRestore;
var W,Restored: TWfcPipelineWorkspaceWorkbench; Before: TSnapshot;
  Contexts: TWfcPipelineWorkspaceContexts; C: TWfcPipelineWorkspaceContextLimits;
  J,Bad,Inspected: TWfcPipelineWorkspaceJournal; Actions: TWfcPipelineWorkspaceActions;
  M: TWfcPipelineModel; Run: TWfcPipelineRun; S: TWfcPipelineSessionPublicState;
  Text: String; Rejected: Boolean;
begin
  W:=nil; Restored:=nil; Before:=nil; Contexts:=nil; J:=nil; Bad:=nil;
  Inspected:=nil; M:=nil; Run:=nil; S:=nil;
  try
    W:=TWfcPipelineWorkspaceWorkbench.Create; Start(W,FixtureRunText,True);
    Before:=TSnapshot.Create(W);
    C.Version:=1; C.MaxRecipes:=1; C.MaxRuns:=1;
    C.MaxTextBytes:=Length(FixtureRecipeText)+Length(FixtureRunText);
    Contexts:=TWfcPipelineWorkspaceWorkbench.InspectDefinition(FixtureRecipeText,FixtureRunText,C);
    Check((Contexts.RecipeCount=1) and (Contexts.RunCount=1) and
      (Contexts.RunTextAt(0).RecipeIndex=0),'definition inspector owns complete explicit row-zero binding');
    Check(Contexts.TextBytes=C.MaxTextBytes,'definition inspector accepts exact aggregate byte bound');
    M:=Contexts.CopyRecipe(0); Run:=Contexts.CopyRun(0); FreeAndNil(Contexts);
    Check((EncodeWfcPipelineModelText(M)=FixtureRecipeText) and
      (EncodeWfcPipelineRunText(Run)=FixtureRunText),'inspector copies survive producer disposal');
    Dec(C.MaxTextBytes); Rejected:=False;
    try Contexts:=TWfcPipelineWorkspaceWorkbench.InspectDefinition(FixtureRecipeText,FixtureRunText,C);
    except on E: EWfcPipelineWorkspaceContext do Rejected:=True; end;
    Check(Rejected and (Contexts=nil),'definition inspector rejects one-less aggregate bound');
    Before.CheckUnchanged(W);
    J:=TWfcPipelineWorkspaceWorkbench.InspectJournal(W.CopyCanonicalJournal,JournalLimits);
    Check(J.Verification=wpwvUnverifiedClaims,'journal inspection makes no executed claim');
    Actions:=J.CopyActions;
    Check(Pos('solved=1'+#10,Actions[1].EvidenceText)>0,'actual solved line available for one-field forgery');
    Actions[1].EvidenceText:=StringReplace(Actions[1].EvidenceText,'solved=1'+#10,'solved=0'+#10,[]);
    Bad:=CopyWithActions(J,Actions); Text:=Bad.CopyCanonicalText;
    Inspected:=TWfcPipelineWorkspaceWorkbench.InspectJournal(Text,JournalLimits);
    Check(Inspected.Verification=wpwvUnverifiedClaims,'well-enveloped false evidence is inspectable, never verified');
    Before.CheckUnchanged(W); Rejected:=False;
    try W.RestoreJournal(Text,Policy,W.PublicationRevision);
    except on E: EWfcPipelineWorkspaceReplay do
      begin Rejected:=True; Check(E.Kind=wpwrfEvidenceMismatch,'restore executes and detects changed complete evidence'); end; end;
    Check(Rejected,'forged restore refused'); Before.CheckUnchanged(W);
    Restored:=TWfcPipelineWorkspaceWorkbench.Create;
    Restored.RestoreJournal(J.CopyCanonicalText,Policy,0);
    Check((Restored.PublicationRevision=1) and Restored.HasCurrentOutput,
      'real restored history creates one atomic publication');
    Check(Restored.CopyCanonicalJournal=J.CopyCanonicalText,'restore retains every input journal byte');
    FreeAndNil(J); FreeAndNil(Bad); FreeAndNil(Inspected); FreeAndNil(W);
    S:=Restored.CopyPublicState; EqualPublicState(Before.State,S);
    Check(EncodeWfcPipelineRunText(Run)=FixtureRunText,'copied invocation survives controllers and imported journals');
    ExportReplayJournal(Restored.CopyCanonicalJournal);
  finally
    S.Free; Run.Free; M.Free; Inspected.Free; Bad.Free; J.Free;
    Contexts.Free; Before.Free; Restored.Free; W.Free;
  end;
end;

procedure TestHelperPolicyBounds;
var Probe,W: TWfcPipelineWorkspaceWorkbench; R: TWfcPipelineWorkspaceReceipt;
  J: TWfcPipelineWorkspaceJournal; Exact,Short: TWfcPipelineWorkspacePolicy;
  I: Integer; ExpectedJournal: String;
begin
  Probe:=nil; W:=nil; R:=nil; J:=nil;
  try
    Probe:=TWfcPipelineWorkspaceWorkbench.Create; Start(Probe,FixtureRunText,True);
    R:=Probe.SetCellLock(0,0,0,0,'U',Policy,Probe.PublicationRevision); FreeAndNil(R);
    ExpectedJournal:=Probe.CopyCanonicalJournal;
    J:=DecodeWfcPipelineWorkspaceJournalText(ExpectedJournal,JournalLimits);
    for I:=0 to 4 do
    begin
      Exact:=Policy;
      case I of
        0:Exact.Journal.MaxActions:=J.ActionCount;
        1:Exact.Journal.MaxRuns:=J.RunCount;
        2:Exact.Journal.MaxContextTextBytes:=J.ContextTextBytes;
        3:Exact.Journal.MaxEvidenceTextBytes:=J.EvidenceTextBytes;
        4:Exact.Journal.MaxEncodedTextBytes:=J.EncodedTextBytes;
      end;
      Short:=Exact;
      case I of
        0:Dec(Short.Journal.MaxActions);
        1:Dec(Short.Journal.MaxRuns);
        2:Dec(Short.Journal.MaxContextTextBytes);
        3:Dec(Short.Journal.MaxEvidenceTextBytes);
        4:Dec(Short.Journal.MaxEncodedTextBytes);
      end;
      W:=TWfcPipelineWorkspaceWorkbench.Create; Start(W,FixtureRunText,True);
      ExpectCellFailure(W,coSetLock,0,0,0,0,'U',nil,Short,W.PublicationRevision);
      R:=W.SetCellLock(0,0,0,0,'U',Exact,W.PublicationRevision);
      Check(R.Kind=wpwakEdit,'exact helper publication budget accepted');
      Check(W.CopyCanonicalJournal=ExpectedJournal,'exact-bound helper retains full deterministic journal, not truncated evidence');
      FreeAndNil(R); FreeAndNil(W);
    end;
    W:=TWfcPipelineWorkspaceWorkbench.Create; Start(W,FixtureRunText,True);
    Short:=Policy; Short.Version:=0;
    ExpectCellFailure(W,coClearLock,0,0,0,0,'',nil,Short,W.PublicationRevision);
    Short:=Policy; Short.Replacement.MaxRetainedValueItems:=0;
    ExpectCellFailure(W,coClearDomain,0,0,0,0,'',nil,Short,W.PublicationRevision);
  finally J.Free; R.Free; W.Free; Probe.Free; end;
end;

procedure TestRunFormats;
var Rules: TWfcRuleModel; Resources: TWfcPipelineResources; Passes: TWfcPipelinePasses;
  Weights: TWfcModelIntegerArray; M: TWfcPipelineModel; Base,Expected,Actual: TWfcPipelineRun;
  W: TWfcPipelineWorkspaceWorkbench; R: TWfcPipelineWorkspaceReceipt;
  Extents: TWfcPipelinePassExtents; Locks: TWfcPipelineCellLocks;
  Domains: TWfcPipelineCellDomains; Version: Integer;
  procedure CheckFormat;
  begin
    Expected:=NewRun(M,Base,Locks,Domains); Actual:=W.CopyAppliedRun;
    try
      Check(Actual.FormatVersion=Version,'helper preserves original run format on legacy recipe');
      Check(EncodeWfcPipelineRunText(Actual)=EncodeWfcPipelineRunText(Expected),
        'helper preserves full legacy/explicit invocation bytes without format promotion');
    finally FreeAndNil(Actual); FreeAndNil(Expected); FreeAndNil(R); end;
  end;
begin
  Rules:=nil; M:=nil; Base:=nil; Expected:=nil; Actual:=nil; W:=nil; R:=nil;
  try
    SetLength(Weights,2); Weights[0]:=1; Weights[1]:=1;
    Rules:=TWfcRuleModel.Create(1,Tokens(['U','V']),Weights,nil);
    SetLength(Resources,1); Resources[0]:=MakeWfcPipelineResource('rules',wprkRules,
      EncodeWfcRuleText(Rules),'literal U/V rules','MIT','uv-run-format');
    SetLength(Passes,1); Passes[0]:=MakeWfcPipelinePass('public',wppvPublic,
      gpmOverlay,-1,wpakRules,0,False,wseWhole);
    M:=TWfcPipelineModel.Create(MakeWfcPipelineMetadata('controller run formats','MIT',
      'project-authored literal rules','run-format'),CurrentWfcPipelineVersions,1,False,
      rmBottomUp,Resources,Passes,nil,nil,nil,nil,nil);
    Check(not M.HasPassMapping,'format compatibility fixture is a legacy recipe');
    SetLength(Extents,1); Extents[0]:=MakeWfcLatticeVector(2,1,1);
    for Version:=1 to 2 do
    begin
      Locks:=nil; Domains:=nil;
      if Version=1 then Base:=TWfcPipelineRun.Create(M,2,1,1,9,wpssOneWay,32,0,False,nil,nil)
      else Base:=TWfcPipelineRun.Create(M,Extents,9,wpssOneWay,32,0,False,nil,nil);
      W:=TWfcPipelineWorkspaceWorkbench.Create;
      R:=W.BeginEpoch(EncodeWfcPipelineModelText(M),EncodeWfcPipelineRunText(Base),Policy,0);
      FreeAndNil(R);
      R:=W.SetCellLock(0,1,0,0,'U',Policy,W.PublicationRevision);
      SetLength(Locks,1); Locks[0]:=MakeWfcPipelineCellLock(0,1,0,0,'U'); CheckFormat;
      R:=W.SetCellDomain(0,0,0,0,Tokens(['U','V']),Policy,W.PublicationRevision);
      SetLength(Domains,1); Domains[0]:=MakeWfcPipelineCellDomain(0,0,0,0,Tokens(['U','V'])); CheckFormat;
      R:=W.ClearCellLock(0,1,0,0,Policy,W.PublicationRevision); Locks:=nil; CheckFormat;
      R:=W.ClearCellDomain(0,0,0,0,Policy,W.PublicationRevision); Domains:=nil; CheckFormat;
      ExportReplayJournal(W.CopyCanonicalJournal); FreeAndNil(W); FreeAndNil(Base);
    end;
  finally R.Free; W.Free; Actual.Free; Expected.Free; Base.Free; M.Free; Rules.Free; end;
end;

{$IFDEF PAS2JS}
procedure RawCellFailure(const W: TWfcPipelineWorkspaceWorkbench; const CaseIndex: Integer);
var P: TWfcPipelineWorkspacePolicy; Allowed: TWfcModelTokens; Token: TWfcModelToken;
  PassIndex,X,Y,Z,Expected,Hits: Integer; Op: TCellOperation; Stale: Boolean;
begin
  { Each case gets fresh typed locals: never $assign into a raw null/object
    left by a previous malformed-record case. }
  P:=Policy; Allowed:=Tokens(['U','V']); Token:='U'; PassIndex:=0;
  X:=0; Y:=0; Z:=0; Expected:=W.PublicationRevision; Hits:=0;
  Op:=coSetDomain; Stale:=False;
  case CaseIndex of
    0:asm PassIndex=NaN; end;
    1:asm PassIndex='0'; end;
    2:asm X=Infinity; end;
    3:asm Y=false; end;
    4:asm Z=null; end;
    5:begin Op:=coSetLock; asm Token=42; end; end;
    6:begin Op:=coSetLock; asm Token=new String('U'); end; end;
    7:begin Op:=coSetLock; asm Token={toString:function(){Hits++;return 'U';}}; end; end;
    8:asm Allowed=null; end;
    9:asm delete Allowed[0]; end;
    10:asm Object.defineProperty(Allowed,'0',{get:function(){Hits++;throw new Error('token getter');}}); end;
    11:asm delete Allowed[0];Object.setPrototypeOf(Allowed,{'0':'U'}); end;
    12:asm Allowed[0]=true; end;
    13:asm Allowed[0]=new String('U'); end;
    14:asm P=null; end;
    15:asm Object.defineProperty(P,'Replacement',{get:function(){Hits++;throw new Error('policy getter');}}); end;
    16:asm P.Outcome.MaxPublicCellRecords='10000'; end;
    17:asm Object.defineProperty(P.Journal,'MaxActions',{get:function(){Hits++;throw new Error('limit getter');}}); end;
    18:begin Stale:=True; asm Expected=NaN; end; end;
    19:begin
      Stale:=True;
      asm Expected--;Object.defineProperty(P,'Journal',{get:function(){Hits++;throw new Error('stale-before-policy');}}); end;
    end;
    20:begin Op:=coClearLock; asm X='0'; end; end;
    21:begin Op:=coClearDomain; asm PassIndex=true; end; end;
  end;
  ExpectCellFailure(W,Op,PassIndex,X,Y,Z,Token,Allowed,P,Expected,Stale);
  Check(Hits=0,'controller raw guard invokes no caller coercion/accessor');
end;
{$ENDIF}

procedure RawArrayPositive(const W: TWfcPipelineWorkspaceWorkbench; const CaseIndex: Integer);
var P: TWfcPipelineWorkspacePolicy; Allowed: TWfcModelTokens;
  R: TWfcPipelineWorkspaceReceipt; Run: TWfcPipelineRun; Hits,I: Integer;
  Text: String; D: TWfcPipelineCellDomain; Found: Boolean;
begin
  P:=Policy; Allowed:=Tokens(['U','V']); R:=nil; Run:=nil; Hits:=0;
  { Every target authors the same four valid domain intentions. Only the
    browser-specific passive array representation is conditional. }
  {$IFDEF PAS2JS}
  case CaseIndex of
    0:asm Object.defineProperty(Allowed,'slice',{get:function(){Hits++;throw new Error('slice getter');}}); end;
    1:asm Allowed.slice=function(){Hits++;return this;}; end;
    2:asm Allowed.slice=null; end;
    3:asm
      Object.freeze(Allowed); Object.freeze(P.Journal); Object.freeze(P.Replacement);
      Object.freeze(P.Outcome); Object.freeze(P.Evidence); Object.freeze(P.Replay); Object.freeze(P);
    end;
  end;
  {$ENDIF}
  try
    R:=W.SetCellDomain(0,1,0,0,Allowed,P,W.PublicationRevision);
    Check((R.Kind=wpwakEdit) and (Hits=0),'valid passive array edits do not invoke caller slice');
    Text:=W.CopyCanonicalJournal; if CaseIndex<>3 then Allowed[0]:='V';
    Check(W.CopyCanonicalJournal=Text,'caller token mutation cannot rewrite accepted journal');
    Run:=W.CopyAppliedRun; Found:=False;
    for I:=0 to Run.DomainCount-1 do
    begin
      D:=Run.DomainAt(I);
      if (D.PassIndex=0) and (D.X=1) then
      begin
        Found:=True;
        Check((Length(D.AllowedTokens)=2) and (D.AllowedTokens[0]='U') and
          (D.AllowedTokens[1]='V'),'accepted domain is independently retained in vocabulary order');
        D.AllowedTokens[0]:='V';
        Check(Run.DomainAt(I).AllowedTokens[0]='U','copied domain cannot alter immutable run owner');
      end;
    end;
    Check(Found,'positive raw domain remains explicitly authored');
  finally Run.Free; R.Free; end;
end;

procedure TestCellArrayBoundaries;
var W: TWfcPipelineWorkspaceWorkbench; I: Integer;
begin
  W:=TWfcPipelineWorkspaceWorkbench.Create;
  try
    Start(W,FixtureRunText,True);
    {$IFDEF PAS2JS}
    for I:=0 to 21 do RawCellFailure(W,I);
    {$ENDIF}
    for I:=0 to 3 do RawArrayPositive(W,I);
    ExportReplayJournal(W.CopyCanonicalJournal);
  finally W.Free; end;
end;

procedure TestPipelineWorkspaceWorkbench;
var J: TWfcPipelineWorkspaceJournal; Expected,Baseline: TWfcPipelineSessionPublicState;
  Revision: Integer;
begin
  J:=nil; Expected:=nil; Baseline:=nil;
  try
    J:=FixtureJournal(False,Expected,Baseline,Revision);
    FixtureRecipeText:=J.RecipeTextAt(0); FixtureRunText:=J.RunTextAt(0).Text;
  finally J.Free; Expected.Free; Baseline.Free; end;
  TestCompleteCellEdits; TestCellRefusals; TestHistoryAndFailure;
  TestInspectAndRestore; TestHelperPolicyBounds; TestRunFormats;
  TestCellArrayBoundaries;
  FixtureRecipeText:=''; FixtureRunText:='';
end;
end.
