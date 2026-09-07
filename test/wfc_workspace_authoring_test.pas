{ SPDX-License-Identifier: MIT
  Copyright (c) 2021 mr-highball
  Authoring versus independently captured session history. }
unit wfc_workspace_authoring_test;
{$mode delphi}{$H+}
interface
procedure TestWorkspaceAuthoring;
implementation
uses SysUtils,wfc,wfc_pipeline_model,wfc_pipeline_run,wfc_pipeline_run_text,
  wfc_pipeline_session,wfc_pipeline_session_evidence,
  wfc_pipeline_workspace_journal,wfc_pipeline_workspace_journal_text,
  wfc_pipeline_workspace_replay,wfc_workspace_replay_fixture;
function Policy: TWfcPipelineWorkspacePolicy;
begin
  Result:=Default(TWfcPipelineWorkspacePolicy); Result.Version:=1;
  Result.Journal:=JournalLimits; Result.Replacement:=ReplacementLimits;
  Result.Outcome:=OutcomeLimits; Result.Evidence:=EvidenceLimits;
  Result.Replay.Version:=1; Result.Replay.MaxEpochs:=20;
  Result.Replay.MaxSolveActions:=100; Result.Replay.MaxInstantiatedCellRecords:=100000;
  Result.Replay.MaxEvidenceTextBytes:=8000000;
end;
function Roots(const Values: array of Integer): TGraphPassIndices;
var I: Integer;
begin Result:=nil; SetLength(Result,Length(Values)); for I:=0 to High(Values) do Result[I]:=Values[I]; end;
function AuthorAction(const Slot: TWfcPipelineWorkspaceSlot;
  const History: TWfcPipelineWorkspaceJournal; const Index: Integer): TWfcPipelineWorkspaceReceipt;
var A: TWfcPipelineWorkspaceAction; Text: String; RecipeIndex: Integer;
begin
  A:=History.ActionAt(Index); Text:=History.RunTextAt(A.RunIndex).Text;
  RecipeIndex:=History.RunTextAt(A.RunIndex).RecipeIndex;
  case A.Kind of
    wpwakBeginEpoch:Result:=Slot.BeginEpoch(History.RecipeTextAt(RecipeIndex),Text,Policy,Slot.PublicationRevision);
    wpwakEdit:Result:=Slot.ApplyInputs(Text,Policy,Slot.PublicationRevision);
    wpwakInitial:Result:=Slot.ExecuteInitial(Policy,Slot.PublicationRevision);
    wpwakRepair:Result:=Slot.ExecuteRepair(Text,A.RequestedRootIndices,Policy,Slot.PublicationRevision);
  else raise Exception.Create('unexpected fixture action'); end;
end;
procedure TestAuthoredHistory(const EndWithFailure: Boolean);
var History,Authored: TWfcPipelineWorkspaceJournal; Slot: TWfcPipelineWorkspaceSlot;
  Receipt,Saved: TWfcPipelineWorkspaceReceipt; Execution: TWfcPipelineWorkspaceExecution;
  Expected,Baseline,State,SavedState: TWfcPipelineSessionPublicState;
  Revision,I,RunCount,RecipeCount,OldRunIndex: Integer; A,B: TWfcPipelineWorkspaceAction;
  P: TWfcPipelineWorkspacePolicy; Text,SavedEvidence: String; Run: TWfcPipelineRun;
  Preview: TWfcPipelineWorkspaceRepairPreview; Scope: TWfcPipelineSessionScope;
begin
  History:=nil; Authored:=nil; Slot:=nil; Receipt:=nil; Saved:=nil; Execution:=nil;
  Expected:=nil; Baseline:=nil; State:=nil; SavedState:=nil; Run:=nil; Preview:=nil;
  try
    P:=Policy; History:=FixtureJournal(EndWithFailure,Expected,Baseline,Revision);
    Slot:=TWfcPipelineWorkspaceSlot.Create; RunCount:=0; RecipeCount:=0; OldRunIndex:=-1;
    Check(not Slot.HasExecution and not Slot.HasCurrentOutput and not Slot.HasSuccessfulBaseline,'authoring starts empty');
    Check((Slot.CurrentRecipeIndex=-1) and (Slot.CurrentRunIndex=-1),'empty context indices are absent');
    for I:=0 to History.ActionCount-1 do
    begin
      A:=History.ActionAt(I);
      if A.Kind=wpwakRepair then
      begin
        Text:=Slot.CopyCanonicalJournal;
        Preview:=Slot.PreviewRepair(History.RunTextAt(A.RunIndex).Text,A.RequestedRootIndices,P,Slot.PublicationRevision);
        Check(Preview.CanExecute and not Preview.MissingBaseline,'valid explicit repair preview');
        Check((Preview.PublicationRevision=I) and (Preview.SessionRevision=Slot.SessionRevision),'preview uses both revisions');
        Check(Preview.RunText=History.RunTextAt(A.RunIndex).Text,'preview retains complete requested run');
        Scope:=Preview.CopyScope; Scope.RequestedRootIndices[0]:=0;
        Check(Preview.CopyScope.RequestedRootIndices[0]=A.RequestedRootIndices[0],'preview scope copies detached');
        Check((Slot.PublicationRevision=I) and (Slot.CopyCanonicalJournal=Text),'preview does not publish');
        FreeAndNil(Preview);
      end;
      Receipt:=AuthorAction(Slot,History,I);
      Check(Receipt.Kind=A.Kind,'receipt retains exact action kind');
      Check((Receipt.ActionIndex=I) and (Receipt.PublicationRevision=I+1),'exact action and publication indices');
      Check(Receipt.EvidenceText=A.EvidenceText,'authoring evidence equals independently executed complete evidence');
      Check((Slot.PublicationRevision=I+1) and (Receipt.SessionRevision=Slot.SessionRevision),'receipt revision equals published session');
      Check(Receipt.HasCurrentOutput=Slot.HasCurrentOutput,'receipt currentness equals published currentness');
      Check(Receipt.HasSuccessfulBaseline=Slot.HasSuccessfulBaseline,'receipt baseline state equals publication');
      if A.Kind=wpwakBeginEpoch then
      begin
        Inc(RecipeCount); Check(Receipt.SessionRevision=0,'new epoch resets only session revision');
        Check((Receipt.EvidenceText='') and (Receipt.BorrowEditOutcome=nil) and (Receipt.BorrowSolveOutcome=nil),'begin has no invented outcome');
      end;
      if A.Kind<>wpwakInitial then Inc(RunCount)
      else Check(Receipt.RunIndex=OldRunIndex,'initial references exactly the applied row');
      if A.Kind=wpwakEdit then
        Check((Receipt.BorrowEditOutcome<>nil) and (Receipt.BorrowSolveOutcome=nil),'edit receipt owns actual edit only');
      if A.Kind in [wpwakInitial,wpwakRepair] then
        Check((Receipt.BorrowSolveOutcome<>nil) and (Receipt.BorrowEditOutcome=nil),'solve receipt owns actual complete outcome');
      State:=Slot.CopyPublicState; EqualPublicState(Receipt.BorrowPublicState,State); FreeAndNil(State);
      Check((Receipt.RecipeIndex=RecipeCount-1) and (Receipt.EpochCount=RecipeCount),'explicit recipe and epoch indices');
      Check((Receipt.RunIndex=Slot.CurrentRunIndex) and (Receipt.RecipeIndex=Slot.CurrentRecipeIndex),'receipt contexts identify publication');
      OldRunIndex:=Receipt.RunIndex;
      if (Saved=nil) and (A.Kind=wpwakInitial) then
      begin
        Saved:=Receipt; Receipt:=nil; SavedEvidence:=Saved.EvidenceText;
        SavedState:=Saved.BorrowSolveOutcome.CopyPublicState;
      end;
      FreeAndNil(Receipt);
    end;
    Check((Slot.SessionRevision=Revision) and (Slot.HasCurrentOutput=not EndWithFailure),'independent final revision and currentness');
    State:=Slot.CopyPublicState; EqualPublicState(Expected,State); FreeAndNil(State);
    State:=Slot.CopyLastSuccessfulState; EqualPublicState(Baseline,State); FreeAndNil(State);
    Run:=Slot.CopyAppliedRun; Check(Run.LockCount=0,'authoring never injects generated values as locks'); FreeAndNil(Run);
    Text:=Slot.CopyCanonicalJournal; ExportReplayJournal(Text);
    Authored:=DecodeWfcPipelineWorkspaceJournalText(Text,P.Journal);
    Check(Authored.Verification=wpwvUnverifiedClaims,'exported authored document does not pretend to verify imported claims');
    Check((Authored.ActionCount=History.ActionCount) and (Authored.RunCount=RunCount) and
      (Authored.RecipeCount=RecipeCount),'all actions and explicit appended contexts retained');
    for I:=0 to Authored.ActionCount-1 do
    begin
      A:=History.ActionAt(I); B:=Authored.ActionAt(I);
      Check((A.Kind=B.Kind) and (A.EvidenceText=B.EvidenceText),'no coalesced or rewritten accepted history');
      Check(History.RunTextAt(A.RunIndex).Text=Authored.RunTextAt(B.RunIndex).Text,'each explicit run edge retains full input context');
    end;
    Execution:=ReplayWfcPipelineWorkspace(Authored,P.Journal,P.Replacement,P.Outcome,P.Evidence,P.Replay);
    State:=Execution.CopyPublicState; EqualPublicState(Expected,State); FreeAndNil(State);
    State:=Execution.CopyLastSuccessfulState; EqualPublicState(Baseline,State); FreeAndNil(State);
    FreeAndNil(Execution); FreeAndNil(History); FreeAndNil(Authored); FreeAndNil(Slot);
    Check(Saved.EvidenceText=SavedEvidence,'actual outcome evidence survives all producing owners');
    EqualPublicState(SavedState,Saved.BorrowPublicState);
    State:=Saved.BorrowSolveOutcome.CopyPublicState; EqualPublicState(SavedState,State);
  finally
    Preview.Free; Run.Free; State.Free; SavedState.Free; Baseline.Free; Expected.Free;
    Execution.Free; Receipt.Free; Saved.Free; Slot.Free; Authored.Free; History.Free;
  end;
end;
type TAuthorFailureAction = (afaBegin,afaEdit,afaInitial,afaRepair);
procedure ExpectFailure(const Slot: TWfcPipelineWorkspaceSlot;
  const Which: TAuthorFailureAction; const RecipeText,RunText: String;
  const RootValues: TGraphPassIndices; const P: TWfcPipelineWorkspacePolicy;
  const ExpectedRevision,ExpectedKind: Integer);
var Receipt: TWfcPipelineWorkspaceReceipt; State,Baseline,AfterState: TWfcPipelineSessionPublicState;
  Text,RunBefore: String; Revision,SessionRevision,RI,UI: Integer;
  Current,HasBaseline,Rejected: Boolean; Run: TWfcPipelineRun;
begin
  Receipt:=nil; State:=nil; Baseline:=nil; AfterState:=nil; Run:=nil;
  try
    Text:=Slot.CopyCanonicalJournal; Revision:=Slot.PublicationRevision; SessionRevision:=Slot.SessionRevision;
    RI:=Slot.CurrentRecipeIndex; UI:=Slot.CurrentRunIndex; Current:=Slot.HasCurrentOutput; HasBaseline:=Slot.HasSuccessfulBaseline;
    State:=Slot.CopyPublicState; Baseline:=Slot.CopyLastSuccessfulState;
    Run:=Slot.CopyAppliedRun; RunBefore:=EncodeWfcPipelineRunText(Run); FreeAndNil(Run); Rejected:=False;
    try
      case Which of
        afaBegin:Receipt:=Slot.BeginEpoch(RecipeText,RunText,P,ExpectedRevision);
        afaEdit:Receipt:=Slot.ApplyInputs(RunText,P,ExpectedRevision);
        afaInitial:Receipt:=Slot.ExecuteInitial(P,ExpectedRevision);
        afaRepair:Receipt:=Slot.ExecuteRepair(RunText,RootValues,P,ExpectedRevision);
      end;
    except
      on E: EWfcPipelineWorkspaceReplay do begin Rejected:=True; if ExpectedKind>=0 then Check(Ord(E.Kind)=ExpectedKind,'authoring expected typed replay rejection'); end;
      on E: EWfcPipelineWorkspaceJournal do begin if ExpectedKind>=0 then raise; Rejected:=True; end;
      on E: EWfcPipelineSession do begin if ExpectedKind>=0 then raise; Rejected:=True; end;
      on E: EWfcPipelineSessionEvidence do begin if ExpectedKind>=0 then raise; Rejected:=True; end;
      on E: EConvertError do begin if ExpectedKind>=0 then raise; Rejected:=True; end;
    end;
    Check(Rejected and (Receipt=nil),'rejected action returns no partial receipt');
    Check((Slot.CopyCanonicalJournal=Text) and (Slot.PublicationRevision=Revision),'authoring rejection retains every journal byte/publication revision');
    Check((Slot.SessionRevision=SessionRevision) and (Slot.CurrentRecipeIndex=RI) and (Slot.CurrentRunIndex=UI),'rejected action preserves session and context indices');
    Check((Slot.HasCurrentOutput=Current) and (Slot.HasSuccessfulBaseline=HasBaseline),'rejection preserves current/baseline flags');
    AfterState:=Slot.CopyPublicState; EqualPublicState(State,AfterState); FreeAndNil(AfterState);
    AfterState:=Slot.CopyLastSuccessfulState; EqualPublicState(Baseline,AfterState);
    Run:=Slot.CopyAppliedRun; Check(EncodeWfcPipelineRunText(Run)=RunBefore,'rejection preserves complete applied inputs/options');
  finally Run.Free; AfterState.Free; Baseline.Free; State.Free; Receipt.Free; end;
end;
procedure TestAuthoringRefusals;
var History: TWfcPipelineWorkspaceJournal; Slot: TWfcPipelineWorkspaceSlot;
  Expected,Baseline: TWfcPipelineSessionPublicState; Revision,I: Integer;
  Receipt: TWfcPipelineWorkspaceReceipt; Preview: TWfcPipelineWorkspaceRepairPreview;
  P,Short: TWfcPipelineWorkspacePolicy; BaseText,LockText,RecipeText,Before: String;
  A: TWfcPipelineWorkspaceAction; Scope: TWfcPipelineSessionScope;
  Probe: TWfcPipelineWorkspaceSlot; Published,ProbeJournal: TWfcPipelineWorkspaceJournal;
begin
  History:=nil; Slot:=nil; Expected:=nil; Baseline:=nil; Receipt:=nil; Preview:=nil;
  Probe:=nil; Published:=nil; ProbeJournal:=nil;
  try
    P:=Policy; History:=FixtureJournal(False,Expected,Baseline,Revision); Slot:=TWfcPipelineWorkspaceSlot.Create;
    A:=History.ActionAt(0); BaseText:=History.RunTextAt(A.RunIndex).Text;
    RecipeText:=History.RecipeTextAt(0); A:=History.ActionAt(2); LockText:=History.RunTextAt(A.RunIndex).Text;
    Receipt:=AuthorAction(Slot,History,0); FreeAndNil(Receipt);
    Preview:=Slot.PreviewRepair(BaseText,Roots([1]),P,Slot.PublicationRevision);
    Check(not Preview.CanExecute and Preview.MissingBaseline,'begin-only preview explains missing baseline'); FreeAndNil(Preview);
    ExpectFailure(Slot,afaRepair,'',BaseText,Roots([1]),P,Slot.PublicationRevision,-1);
    Receipt:=AuthorAction(Slot,History,1); FreeAndNil(Receipt);
    ExpectFailure(Slot,afaInitial,'','',nil,P,Slot.PublicationRevision,-1);
    ExpectFailure(Slot,afaEdit,'','bad',nil,P,Slot.PublicationRevision,Ord(wpwrfInputs));
    Short:=P; Short.Version:=2;
    ExpectFailure(Slot,afaEdit,'',BaseText,nil,Short,Slot.PublicationRevision-1,Ord(wpwrfStalePublication));
    ExpectFailure(Slot,afaEdit,'',BaseText,nil,Short,Slot.PublicationRevision,Ord(wpwrfInputs));
    ExpectFailure(Slot,afaRepair,'',LockText,Roots([1]),P,Slot.PublicationRevision,-1);
    ExpectFailure(Slot,afaRepair,'',BaseText,Roots([]),P,Slot.PublicationRevision,Ord(wpwrfInputs));
    ExpectFailure(Slot,afaRepair,'',BaseText,Roots([1,1]),P,Slot.PublicationRevision,Ord(wpwrfInputs));
    ExpectFailure(Slot,afaRepair,'',BaseText,Roots([3,1]),P,Slot.PublicationRevision,Ord(wpwrfInputs));
    ExpectFailure(Slot,afaRepair,'',BaseText,Roots([4]),P,Slot.PublicationRevision,Ord(wpwrfInputs));
    for I:=2 to 3 do begin Receipt:=AuthorAction(Slot,History,I); FreeAndNil(Receipt); end;
    Preview:=Slot.PreviewRepair(BaseText,Roots([3]),P,Slot.PublicationRevision);
    Scope:=Preview.CopyScope;
    Check(not Preview.CanExecute and not Preview.MissingBaseline and (Length(Scope.MissingPassIndices)>0),'leaf-only preview exposes pending backing source');
    FreeAndNil(Preview);
    ExpectFailure(Slot,afaRepair,'',BaseText,Roots([3]),P,Slot.PublicationRevision,Ord(wpwrfScope));
    Short:=P; Short.Journal.MaxActions:=Slot.PublicationRevision;
    ExpectFailure(Slot,afaEdit,'',BaseText,nil,Short,Slot.PublicationRevision,Ord(wpwrfBudget));
    Short:=P; Short.Journal.MaxRuns:=3;
    ExpectFailure(Slot,afaEdit,'',BaseText,nil,Short,Slot.PublicationRevision,Ord(wpwrfBudget));
    Short:=P; Short.Journal.MaxRecipes:=1;
    ExpectFailure(Slot,afaBegin,RecipeText,BaseText,nil,Short,Slot.PublicationRevision,Ord(wpwrfBudget));
    Short:=P; Short.Replay.MaxEpochs:=1;
    ExpectFailure(Slot,afaBegin,RecipeText,BaseText,nil,Short,Slot.PublicationRevision,Ord(wpwrfBudget));
    Short:=P; Short.Replay.MaxSolveActions:=1;
    ExpectFailure(Slot,afaRepair,'',BaseText,Roots([1]),Short,Slot.PublicationRevision,Ord(wpwrfBudget));
    Short:=P; Short.Replay.MaxInstantiatedCellRecords:=History.BorrowRun(0).TotalCellCount;
    ExpectFailure(Slot,afaBegin,RecipeText,BaseText,nil,Short,Slot.PublicationRevision,Ord(wpwrfBudget));
    { Fits old outcomes and the draft header, but not the new complete edit.
      Thus failure occurs only after privately applying the input transition. }
    Short:=P; Short.Journal.MaxEvidenceTextBytes:=
      Length(History.ActionAt(1).EvidenceText)+Length(History.ActionAt(2).EvidenceText)+
      Length(History.ActionAt(3).EvidenceText)+128;
    ExpectFailure(Slot,afaEdit,'',LockText,nil,Short,Slot.PublicationRevision,-1);
    Short:=P;
    { Reset independently: the replay aggregate is now deliberately tight. }
    Short.Replay.MaxEvidenceTextBytes:=Length(History.ActionAt(1).EvidenceText)+
      Length(History.ActionAt(2).EvidenceText)+Length(History.ActionAt(3).EvidenceText)+128;
    ExpectFailure(Slot,afaEdit,'',LockText,nil,Short,Slot.PublicationRevision,-1);
    { Find the exact final encoded size independently, then permit one byte
      less. The shorter internal draft must never be publishable in its place. }
    Published:=DecodeWfcPipelineWorkspaceJournalText(Slot.CopyCanonicalJournal,P.Journal);
    Probe:=TWfcPipelineWorkspaceSlot.Create;
    Probe.Restore(Published,P.Journal,P.Replacement,P.Outcome,P.Evidence,P.Replay,0);
    Receipt:=Probe.ApplyInputs(LockText,P,1); FreeAndNil(Receipt);
    ProbeJournal:=DecodeWfcPipelineWorkspaceJournalText(Probe.CopyCanonicalJournal,P.Journal);
    Short:=P; Short.Journal.MaxEncodedTextBytes:=ProbeJournal.EncodedTextBytes-1;
    Check(Short.Journal.MaxEncodedTextBytes>Published.EncodedTextBytes,'old journal fits final encoded failure policy');
    ExpectFailure(Slot,afaEdit,'',LockText,nil,Short,Slot.PublicationRevision,-1);
    FreeAndNil(ProbeJournal); FreeAndNil(Probe); FreeAndNil(Published);
    Before:=Slot.CopyCanonicalJournal;
    Receipt:=Slot.ApplyInputs(BaseText,P,Slot.PublicationRevision); FreeAndNil(Receipt);
    Check((Slot.PublicationRevision=5) and (Slot.CopyCanonicalJournal<>Before),'even byte-identical full input edit remains a distinct accepted action');
    Receipt:=Slot.ExecuteRepair(BaseText,Roots([1]),P,Slot.PublicationRevision);
    Check(Receipt.BorrowSolveOutcome.Solved and Receipt.HasCurrentOutput,'explicit broader repair recovers after retained edits'); FreeAndNil(Receipt);
    Receipt:=Slot.BeginEpoch(RecipeText,BaseText,P,Slot.PublicationRevision);
    Check((Receipt.RecipeIndex=1) and (Receipt.EpochCount=2) and (Receipt.SessionRevision=0),'identical recipe/run still create explicit new context and epoch');
    Check(not Slot.HasSuccessfulBaseline and not Slot.HasCurrentOutput,'begin clears only current-epoch baseline'); FreeAndNil(Receipt);
    ExportReplayJournal(Slot.CopyCanonicalJournal);
  finally ProbeJournal.Free; Probe.Free; Published.Free; Preview.Free; Receipt.Free; Slot.Free; History.Free; Baseline.Free; Expected.Free; end;
end;
procedure TestFailedInitial;
var History: TWfcPipelineWorkspaceJournal; Slot: TWfcPipelineWorkspaceSlot;
  Expected,Baseline: TWfcPipelineSessionPublicState; Revision: Integer;
  Receipt: TWfcPipelineWorkspaceReceipt; P,Short: TWfcPipelineWorkspacePolicy;
  RunText: String; A: TWfcPipelineWorkspaceAction;
begin
  History:=nil; Slot:=nil; Expected:=nil; Baseline:=nil; Receipt:=nil;
  try
    P:=Policy; History:=FixtureJournal(True,Expected,Baseline,Revision);
    A:=History.ActionAt(5); RunText:=History.RunTextAt(A.RunIndex).Text;
    Slot:=TWfcPipelineWorkspaceSlot.Create;
    Receipt:=Slot.BeginEpoch(History.RecipeTextAt(0),RunText,P,0); FreeAndNil(Receipt);
    Short:=P; Short.Outcome.MaxReportPassRecords:=1;
    ExpectFailure(Slot,afaInitial,'','',nil,Short,1,-1);
    Receipt:=Slot.ExecuteInitial(P,1);
    Check(not Receipt.BorrowSolveOutcome.Solved and not Receipt.HasSuccessfulBaseline and not Receipt.HasCurrentOutput,'ordinary failed initial is a fully published outcome');
    Check((Slot.PublicationRevision=2) and (Slot.SessionRevision=1),'failed initial increments both actual revisions'); FreeAndNil(Receipt);
    ExpectFailure(Slot,afaInitial,'','',nil,P,2,-1);
    ExpectFailure(Slot,afaRepair,'',RunText,Roots([0]),P,2,Ord(wpwrfScope));
    ExportReplayJournal(Slot.CopyCanonicalJournal);
  finally Receipt.Free; Slot.Free; History.Free; Baseline.Free; Expected.Free; end;
end;
{$IFDEF PAS2JS}
procedure HostileFailure(const Slot: TWfcPipelineWorkspaceSlot; const BaseText: String;
  const CaseIndex: Integer);
var P: TWfcPipelineWorkspacePolicy; R: TGraphPassIndices; Text: String;
  Hits,Expected,FailureKind: Integer;
begin
  { Every case has fresh typed locals. Do not assign a Pascal record into a
    variable that an earlier raw-JS fixture replaced with null/plain data. }
  P:=Policy; R:=Roots([1]); Text:=BaseText; Hits:=0;
  Expected:=Slot.PublicationRevision; FailureKind:=Ord(wpwrfInputs);
  case CaseIndex of
    0:asm P=null; end;
    1:asm Object.defineProperty(P,'Replay',{get:function(){Hits++;throw new Error('outer policy getter');}}); end;
    2:asm P.Replay.MaxEpochs=NaN; end;
    3:asm Object.defineProperty(P.Outcome,'MaxTraceEvents',{get:function(){Hits++;throw new Error('nested policy getter');}}); end;
    4:asm R=null; end;
    5:asm R[0]=NaN; end;
    6:asm delete R[0]; end;
    7:asm Object.defineProperty(R,'0',{get:function(){Hits++;throw new Error('root getter');}}); end;
    8:asm delete R[0]; Object.setPrototypeOf(R,{'0':1}); end;
    9:asm Text=new String(Text); end;
    10:asm Text=null; end;
    11:begin asm Expected=NaN; end; FailureKind:=Ord(wpwrfStalePublication); end;
    12:begin
      asm Expected=Expected-1; Object.defineProperty(P,'Replay',{get:function(){Hits++;throw new Error('stale must precede policy');}}); end;
      FailureKind:=Ord(wpwrfStalePublication);
    end;
  end;
  ExpectFailure(Slot,afaRepair,'',Text,R,P,Expected,FailureKind);
  Check(Hits=0,'authoring raw boundary never invokes getter/coercion');
end;
procedure HostileSlicePositive(const Slot: TWfcPipelineWorkspaceSlot; const BaseText: String;
  const CaseIndex: Integer);
var R: TGraphPassIndices; P: TWfcPipelineWorkspacePolicy;
  Receipt: TWfcPipelineWorkspaceReceipt; Preview: TWfcPipelineWorkspaceRepairPreview;
  Hits,BeforeRevision: Integer; Text: String; Scope: TWfcPipelineSessionScope;
begin
  R:=Roots([1]); P:=Policy; Receipt:=nil; Preview:=nil; Hits:=0;
  case CaseIndex of
    0:asm Object.defineProperty(R,'slice',{get:function(){Hits++;throw new Error('slice getter');}}); end;
    1:asm R.slice=function(){Hits++;return this;}; end;
    2:asm R.slice=null; end;
    3:asm
      Object.freeze(R);
      Object.freeze(P.Journal); Object.freeze(P.Replacement); Object.freeze(P.Outcome);
      Object.freeze(P.Evidence); Object.freeze(P.Replay); Object.freeze(P);
    end;
  end;
  try
    BeforeRevision:=Slot.PublicationRevision;
    Preview:=Slot.PreviewRepair(BaseText,R,P,BeforeRevision);
    Check(Preview.CanExecute and (Hits=0),'preview ignores caller slice override');
    Receipt:=Slot.ExecuteRepair(BaseText,R,P,BeforeRevision);
    Check((Hits=0) and Receipt.BorrowSolveOutcome.Solved,'actual authoring ignores caller slice override');
    Text:=Slot.CopyCanonicalJournal; if CaseIndex<>3 then R[0]:=0;
    Check(Preview.CopyScope.RequestedRootIndices[0]=1,'preview cannot alias caller roots');
    Check(Receipt.BorrowSolveOutcome.CopyScope.RequestedRootIndices[0]=1,'actual receipt cannot alias caller roots');
    Check(Slot.CopyCanonicalJournal=Text,'caller root mutation cannot change history');
    Scope:=Preview.CopyScope; Scope.RequestedRootIndices[0]:=0;
    Check(Preview.CopyScope.RequestedRootIndices[0]=1,'preview copies cannot alter retained scope');
  finally Preview.Free; Receipt.Free; end;
end;
procedure TestHostileAuthoring;
var History: TWfcPipelineWorkspaceJournal; Slot: TWfcPipelineWorkspaceSlot;
  Expected,Baseline: TWfcPipelineSessionPublicState; Revision,I: Integer;
  Receipt: TWfcPipelineWorkspaceReceipt; Text: String;
begin
  History:=nil; Slot:=nil; Expected:=nil; Baseline:=nil; Receipt:=nil;
  try
    History:=FixtureJournal(False,Expected,Baseline,Revision); Slot:=TWfcPipelineWorkspaceSlot.Create;
    for I:=0 to 1 do begin Receipt:=AuthorAction(Slot,History,I); FreeAndNil(Receipt); end;
    Text:=History.RunTextAt(History.ActionAt(0).RunIndex).Text;
    for I:=0 to 12 do HostileFailure(Slot,Text,I);
    for I:=0 to 3 do HostileSlicePositive(Slot,Text,I);
  finally Receipt.Free; Slot.Free; History.Free; Baseline.Free; Expected.Free; end;
end;
{$ENDIF}
procedure TestWorkspaceAuthoring;
begin
  TestAuthoredHistory(False); TestAuthoredHistory(True);
  TestAuthoringRefusals; TestFailedInitial;
  {$IFDEF PAS2JS}TestHostileAuthoring;{$ENDIF}
end;
end.
