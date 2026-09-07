{ SPDX-License-Identifier: MIT
  Copyright (c) 2021 mr-highball
  Additional replay histories, captured from actual prepared sessions.
  Deliberately forged negative claims are labeled and never treated as reports. }
unit wfc_workspace_replay_extra;
{$mode delphi}{$H+}
interface
procedure TestAdditionalReplayHistories;
implementation
uses SysUtils,wfc,wfc_model,wfc_rule_model,wfc_rule_text,wfc_sequence,
  wfc_pipeline_model,wfc_pipeline_text,wfc_pipeline_run,wfc_pipeline_run_text,
  wfc_pipeline_prepare,wfc_pipeline_session,wfc_pipeline_session_evidence,
  wfc_pipeline_workspace_context,wfc_pipeline_workspace_journal,
  wfc_pipeline_workspace_replay,wfc_workspace_replay_fixture;

function Tokens(const Values: array of TWfcModelToken): TWfcModelTokens;
var I: Integer;
begin
  Result:=nil; SetLength(Result,Length(Values));
  for I:=0 to High(Values) do Result[I]:=Values[I];
end;

function RulesResource(const Name: TWfcModelToken;
  const Values: array of TWfcModelToken): TWfcPipelineResource;
var Rules: TWfcRuleModel; Weights: TWfcModelIntegerArray; I: Integer;
begin
  SetLength(Weights,Length(Values));
  for I:=0 to High(Weights) do Weights[I]:=1;
  Rules:=TWfcRuleModel.Create(1,Tokens(Values),Weights,nil);
  try
    Result:=MakeWfcPipelineResource(Name,wprkRules,EncodeWfcRuleText(Rules),
      'literal independent replay fixture','MIT','actual-session-history');
  finally Rules.Free; end;
end;

function Position(const X: Integer): TGraphPosition;
begin Result.X:=X; Result.Y:=0; Result.Z:=0; end;

function RootLabels(const Name: String): TGraphPassLabels;
begin Result:=nil; SetLength(Result,1); Result[0]:=Name; end;

function LimitsFor(const J: TWfcPipelineWorkspaceJournal): TWfcPipelineWorkspaceReplayLimits;
var I: Integer; A: TWfcPipelineWorkspaceAction;
begin
  Result:=Default(TWfcPipelineWorkspaceReplayLimits); Result.Version:=1;
  Result.MaxEvidenceTextBytes:=J.EvidenceTextBytes;
  for I:=0 to J.ActionCount-1 do
  begin
    A:=J.ActionAt(I);
    if A.Kind=wpwakBeginEpoch then
    begin
      Inc(Result.MaxEpochs);
      Inc(Result.MaxInstantiatedCellRecords,J.BorrowRun(A.RunIndex).TotalCellCount);
    end;
    if A.Kind in [wpwakInitial,wpwakRepair] then Inc(Result.MaxSolveActions);
  end;
  if Result.MaxEvidenceTextBytes=0 then Result.MaxEvidenceTextBytes:=1;
  if Result.MaxSolveActions=0 then Result.MaxSolveActions:=1;
end;

function InitialJournal(const M: TWfcPipelineModel; const R: TWfcPipelineRun;
  const InitialEvidence: String): TWfcPipelineWorkspaceJournal;
var Recipes: TWfcPipelineWorkspaceRecipeTexts; Runs: TWfcPipelineWorkspaceRunTexts;
  Actions: TWfcPipelineWorkspaceActions;
begin
  SetLength(Recipes,1); Recipes[0]:=EncodeWfcPipelineModelText(M);
  SetLength(Runs,1); Runs[0].RecipeIndex:=0; Runs[0].Text:=EncodeWfcPipelineRunText(R);
  SetLength(Actions,2);
  Actions[0].Kind:=wpwakBeginEpoch; Actions[0].RunIndex:=0;
  Actions[0].RequestedRootIndices:=nil; Actions[0].EvidenceText:='';
  Actions[1].Kind:=wpwakInitial; Actions[1].RunIndex:=0;
  Actions[1].RequestedRootIndices:=nil; Actions[1].EvidenceText:=InitialEvidence;
  Result:=TWfcPipelineWorkspaceJournal.Create(Recipes,Runs,Actions,JournalLimits);
end;

function HasLine(const Text,Line: String): Boolean;
begin Result:=Pos(#10+Line+#10,#10+Text)>0; end;

function ChangeOneLine(const Text,OldLine,NewLine: String): String;
var P: Integer; Needle: String;
begin
  Needle:=OldLine+#10; P:=Pos(Needle,Text);
  Check((P>0) and ((P=1) or (Text[P-1]=#10)),'tamper targets a complete actual evidence line');
  Check(Pos(Needle,Copy(Text,P+Length(Needle),Length(Text)))=0,'tamper line occurs exactly once');
  Result:=Copy(Text,1,P-1)+NewLine+#10+Copy(Text,P+Length(Needle),Length(Text));
end;

procedure CheckRestoredState(const J: TWfcPipelineWorkspaceJournal;
  const Expected,Baseline: TWfcPipelineSessionPublicState; const Revision: Integer;
  const Current,HasBaseline: Boolean);
var E: TWfcPipelineWorkspaceExecution; S: TWfcPipelineSessionPublicState;
begin
  E:=nil; S:=nil;
  try
    E:=ReplayWfcPipelineWorkspace(J,JournalLimits,ReplacementLimits,OutcomeLimits,EvidenceLimits,LimitsFor(J));
    Check((E.Revision=Revision) and (E.VerifiedActions=J.ActionCount),'extra history replays every revision/action');
    Check((E.HasCurrentOutput=Current) and (E.HasSuccessfulBaseline=HasBaseline),'extra actual output/baseline flags');
    Check((E.CurrentRecipeIndex=0) and (E.CurrentRunIndex=0),'extra retained context indices');
    Check(E.CopyCanonicalJournal=J.CopyCanonicalText,'extra complete history retained');
    S:=E.CopyPublicState; EqualPublicState(Expected,S); FreeAndNil(S);
    S:=E.CopyLastSuccessfulState; EqualPublicState(Baseline,S);
    FreeAndNil(E); EqualPublicState(Baseline,S);
    ExportReplayJournal(J.CopyCanonicalText);
  finally S.Free; E.Free; end;
end;

procedure ExpectAtomicRejection(const Slot: TWfcPipelineWorkspaceSlot;
  const Bad: TWfcPipelineWorkspaceJournal;
  const ExpectedKind: TWfcPipelineWorkspaceReplayFailureKind; const ActionIndex: Integer);
var State,Baseline,After: TWfcPipelineSessionPublicState;
  BeforeText: String; BeforeRevision: Integer; Rejected: Boolean;
begin
  State:=nil; Baseline:=nil; After:=nil;
  try
    State:=Slot.CopyPublicState; Baseline:=Slot.CopyLastSuccessfulState;
    BeforeText:=Slot.CopyCanonicalJournal; BeforeRevision:=Slot.PublicationRevision;
    Rejected:=False;
    try
      Slot.Restore(Bad,JournalLimits,ReplacementLimits,OutcomeLimits,EvidenceLimits,
        LimitsFor(Bad),BeforeRevision);
    except on E: EWfcPipelineWorkspaceReplay do
      begin
        Rejected:=True;
        Check((E.Kind=ExpectedKind) and (E.ActionIndex=ActionIndex),'extra precise rejection kind and action');
        if ExpectedKind=wpwrfEvidenceMismatch then Check(E.MismatchOffset>0,'extra complete mismatch offset');
      end;
    end;
    Check(Rejected,'extra invalid restore actually rejects');
    Check(Slot.HasExecution and (Slot.PublicationRevision=BeforeRevision),'extra rejected restore retains publication');
    Check(Slot.CopyCanonicalJournal=BeforeText,'extra rejected restore retains complete journal');
    After:=Slot.CopyPublicState; EqualPublicState(State,After); FreeAndNil(After);
    After:=Slot.CopyLastSuccessfulState; EqualPublicState(Baseline,After);
  finally After.Free; Baseline.Free; State.Free; end;
end;

function ImpossibleQuotaRecipe: TWfcPipelineModel;
var Resources: TWfcPipelineResources; Passes: TWfcPipelinePasses;
  Quotas: TWfcPipelineValueQuotas;
begin
  { Same actual second-quota failure as the independently checked session oracle. }
  SetLength(Resources,1); Resources[0]:=RulesResource('quota-rules',['A','B']);
  SetLength(Passes,1);
  Passes[0]:=MakeWfcPipelinePass('quota-pass',wppvPublic,gpmOverlay,-1,wpakRules,0,False,wseWhole);
  SetLength(Quotas,2);
  Quotas[0]:=MakeWfcPipelineValueQuota(0,'first-valid',Tokens(['A']),0,1);
  Quotas[1]:=MakeWfcPipelineValueQuota(0,'second-impossible',Tokens(['B']),2,2);
  Result:=TWfcPipelineModel.Create(MakeWfcPipelineMetadata('failed initial replay','MIT','',''),
    1,False,rmBottomUp,Resources,Passes,nil,nil,nil,Quotas);
end;

procedure TestFailedInitialAndPublication;
var M: TWfcPipelineModel; R: TWfcPipelineRun; Session: TWfcPipelinePreparedSession;
  O,Unexpected: TWfcPipelineSessionOutcome; Plan: TWfcPipelineSessionRepairPlan;
  Report: TGraphSolveReport; J,Bad,Good,FailedRepair: TWfcPipelineWorkspaceJournal;
  Slot: TWfcPipelineWorkspaceSlot; Actions: TWfcPipelineWorkspaceActions;
  Expected,GoodState,GoodBaseline,FailedState,FailedBaseline,Actual: TWfcPipelineSessionPublicState;
  Text: String; Revision,OtherRevision,Count: Integer; Rejected: Boolean;
begin
  M:=nil; R:=nil; Session:=nil; O:=nil; Unexpected:=nil; Plan:=nil;
  J:=nil; Bad:=nil; Good:=nil; FailedRepair:=nil; Slot:=nil;
  Expected:=nil; GoodState:=nil; GoodBaseline:=nil; FailedState:=nil; FailedBaseline:=nil; Actual:=nil;
  try
    M:=ImpossibleQuotaRecipe;
    R:=TWfcPipelineRun.Create(M,1,1,1,0,wpssOneWay,32,0,False,nil,nil);
    Session:=TWfcPipelinePreparedSession.Create(M,R,ReplacementLimits,OutcomeLimits);
    O:=Session.ExecuteInitial; Report:=O.CopySolveReport;
    Check(not O.Solved and not O.HasCurrentOutput and not O.HasSuccessfulBaseline,'actual failed initial has no baseline');
    Check(not Session.HasSuccessfulBaseline and not Session.HasCurrentOutput,'failed initial owner agrees with outcome');
    Check((Report.Contradiction.Kind=gckValueQuota) and (Report.Contradiction.ConstraintIndex=1),'actual second quota ordinal1 survives failed initial');
    Check(not Report.TraceCaptured and (Length(Report.Trace)=0) and (Report.TraceHash=0),'capture-off actual failure invents no trace');
    Text:=EncodeWfcPipelineSessionOutcomeEvidence(O,EvidenceLimits);
    Check(HasLine(Text,'invocation.capture=0') and HasLine(Text,'solve.trace-captured=0') and
      HasLine(Text,'solve.trace-count=0'),'capture-off fields retained in complete evidence');
    Expected:=Session.CopyPublicState; Revision:=Session.Revision;
    Plan:=Session.PlanRepair(R,RootLabels('quota-pass'));
    Check(Plan.MissingBaseline and not Plan.CanExecute,'failed initial repair plan is inspectable but refused');
    Rejected:=False;
    try Unexpected:=Session.ExecuteRepair(Plan);
    except on E: EWfcPipelineSession do Rejected:=True; end;
    Check(Rejected and (Unexpected=nil) and Session.Usable and (Session.Revision=Revision),'refused direct repair is nonmutating');
    Actual:=Session.CopyPublicState; EqualPublicState(Expected,Actual); FreeAndNil(Actual);
    J:=InitialJournal(M,R,Text);
    CheckRestoredState(J,Expected,nil,Revision,False,False);

    { This deliberately forged, structurally admissible repair claim comes from
      an actual full outcome with only its kind mislabeled. It is not a report
      produced by a solver, and replay must refuse it before comparison. }
    Actions:=J.CopyActions; Count:=Length(Actions); SetLength(Actions,Count+1);
    Actions[Count].Kind:=wpwakRepair; Actions[Count].RunIndex:=0;
    SetLength(Actions[Count].RequestedRootIndices,1); Actions[Count].RequestedRootIndices[0]:=0;
    Actions[Count].EvidenceText:=ChangeOneLine(Text,'kind=ordinary-full','kind=ordinary-selective');
    Bad:=CopyWithActions(J,Actions);
    Check(Bad.Verification=wpwvUnverifiedClaims,'forged repair remains only a syntactically admissible claim');

    Good:=FixtureJournal(False,GoodState,GoodBaseline,OtherRevision);
    Slot:=TWfcPipelineWorkspaceSlot.Create;
    Slot.Restore(Good,JournalLimits,ReplacementLimits,OutcomeLimits,EvidenceLimits,LimitsFor(Good),0);
    ExpectAtomicRejection(Slot,Bad,wpwrfScope,Count);
    Slot.Restore(J,JournalLimits,ReplacementLimits,OutcomeLimits,EvidenceLimits,LimitsFor(J),1);
    Check(Slot.PublicationRevision=2,'valid failed initial publishes over an existing successful owner');
    Actual:=Slot.CopyPublicState; EqualPublicState(Expected,Actual); FreeAndNil(Actual);
    Actual:=Slot.CopyLastSuccessfulState; Check(Actual=nil,'failed initial publication does not inherit old baseline');
    ExpectAtomicRejection(Slot,Bad,wpwrfScope,Count);
    FreeAndNil(Bad);

    FailedRepair:=FixtureJournal(True,FailedState,FailedBaseline,OtherRevision);
    Check(FailedBaseline<>nil,'failed repair fixture retains a real successful preview');
    Slot.Restore(FailedRepair,JournalLimits,ReplacementLimits,OutcomeLimits,EvidenceLimits,LimitsFor(FailedRepair),2);
    Check(Slot.PublicationRevision=3,'valid failed repair also publishes over an existing owner');
    Actual:=Slot.CopyPublicState; EqualPublicState(FailedState,Actual); FreeAndNil(Actual);
    Actual:=Slot.CopyLastSuccessfulState; EqualPublicState(FailedBaseline,Actual); FreeAndNil(Actual);
    ExportReplayJournal(Slot.CopyCanonicalJournal);
    Actions:=FailedRepair.CopyActions; Count:=High(Actions);
    Check(Actions[Count].Kind=wpwakRepair,'late mismatch targets actual failed terminal repair');
    Actions[Count].EvidenceText:=ChangeOneLine(Actions[Count].EvidenceText,'solved=0','solved=1');
    Bad:=CopyWithActions(FailedRepair,Actions);
    ExpectAtomicRejection(Slot,Bad,wpwrfEvidenceMismatch,Count);
  finally
    Actual.Free; FailedBaseline.Free; FailedState.Free; GoodBaseline.Free; GoodState.Free; Expected.Free;
    Slot.Free; FailedRepair.Free; Good.Free; Bad.Free; J.Free; Plan.Free; Unexpected.Free;
    O.Free; Session.Free; R.Free; M.Free;
  end;
end;

function NegotiationRecipe: TWfcPipelineModel;
var Resources: TWfcPipelineResources; Passes: TWfcPipelinePasses;
  Dependencies: TWfcPipelineDependencies; Requirements: TWfcPipelineRequirements;
  Terms: TWfcPipelineRequirementTerms;
begin
  { Deterministic seed0 provider rejection from the checked session oracle. }
  SetLength(Resources,2); Resources[0]:=RulesResource('land',['marsh','meadow']);
  Resources[1]:=RulesResource('building',['cottage']);
  SetLength(Passes,2);
  Passes[0]:=MakeWfcPipelinePass('terrain',wppvPublic,gpmOverlay,-1,wpakRules,0,False,wseWhole);
  Passes[1]:=MakeWfcPipelinePass('housing',wppvPublic,gpmOverlay,-1,wpakRules,1,False,wseWhole);
  SetLength(Dependencies,1); Dependencies[0]:=MakeWfcPipelineDependency(1,0);
  SetLength(Terms,1); Terms[0]:=MakeWfcPipelineRequirementTerm(0,0,0,Tokens(['meadow']));
  SetLength(Requirements,1); Requirements[0]:=MakeWfcPipelineRequirement(1,'cottage',0,wprqExact,Terms);
  Result:=TWfcPipelineModel.Create(MakeWfcPipelineMetadata('actual rejected replay assignments','MIT','',''),
    1,False,rmBottomUp,Resources,Passes,Dependencies,nil,Requirements);
end;

procedure CheckCapturedSolve(const R: TGraphSolveReport; const Capture: Boolean;
  const Text,Prefix: String);
begin
  Check(R.TraceCaptured=Capture,'actual attempt preserves capture choice');
  if Capture then Check(Length(R.Trace)>0,'trace-enabled actual attempt contains events')
  else Check((Length(R.Trace)=0) and (R.TraceHash=0),'trace-disabled actual attempt has no synthesized events');
  Check(HasLine(Text,Prefix+'.trace-captured='+IntToStr(Ord(Capture))),'attempt capture flag explicitly encoded');
  Check(HasLine(Text,Prefix+'.trace-count='+IntToStr(Length(R.Trace))),'complete actual attempt trace count encoded');
end;

procedure TestNegotiatedHistory(const Capture: Boolean);
var M: TWfcPipelineModel; R: TWfcPipelineRun; Session: TWfcPipelinePreparedSession;
  O: TWfcPipelineSessionOutcome; N: TGraphNegotiationReport;
  Plan: TWfcPipelineSessionRepairPlan; J,FullJ,Bad: TWfcPipelineWorkspaceJournal;
  Actions: TWfcPipelineWorkspaceActions; Slot: TWfcPipelineWorkspaceSlot;
  State,Baseline: TWfcPipelineSessionPublicState; Text,Prefix,Line: String;
  I,K,Revision: Integer;
begin
  M:=nil; R:=nil; Session:=nil; O:=nil; Plan:=nil; J:=nil; FullJ:=nil; Bad:=nil;
  Slot:=nil; State:=nil; Baseline:=nil;
  try
    M:=NegotiationRecipe;
    R:=TWfcPipelineRun.Create(M,1,1,1,0,wpssNegotiated,32,8,Capture,nil,nil);
    Session:=TWfcPipelinePreparedSession.Create(M,R,ReplacementLimits,OutcomeLimits);
    O:=Session.ExecuteInitial; N:=O.CopyNegotiationReport;
    Check(O.Solved and (O.Kind=wpsokNegotiatedFull),'actual negotiated initial succeeds');
    Check((Length(N.Attempts)>0) and (N.PassBacktracks>0),'actual negotiation contains rejected rounds');
    Text:=EncodeWfcPipelineSessionOutcomeEvidence(O,EvidenceLimits);
    Check(HasLine(Text,'search.attempt-count='+IntToStr(Length(N.Attempts))),'every rejected round count encoded');
    for I:=0 to High(N.Attempts) do
    begin
      Check(N.Attempts[I].SolveReport.Status<>gssSolved,'actual excluded round was not solved');
      Check((N.Attempts[I].BacktrackedPassIndex=0) and (Length(N.Attempts[I].ExcludedAssignment)=1),'actual provider assignment fully retained');
      Prefix:='search.attempt.'+IntToStr(I);
      Check(HasLine(Text,Prefix+'.backtracked='+IntToStr(N.Attempts[I].BacktrackedPassIndex)+','+
        IntToStr(N.Attempts[I].BacktrackedExecutionOrdinal)),'provider index and execution ordinal encoded');
      Check(HasLine(Text,Prefix+'.assignment-count='+IntToStr(Length(N.Attempts[I].ExcludedAssignment))),'complete excluded assignment length encoded');
      for K:=0 to High(N.Attempts[I].ExcludedAssignment) do
        Check(HasLine(Text,Prefix+'.assignment.'+IntToStr(K)+'='+IntToStr(N.Attempts[I].ExcludedAssignment[K])),
          'every actual excluded assignment value encoded');
      CheckCapturedSolve(N.Attempts[I].SolveReport,Capture,Text,Prefix+'.solve');
    end;
    CheckCapturedSolve(N.FinalReport,Capture,Text,'search.final');
    J:=InitialJournal(M,R,Text); FreeAndNil(O);
    Plan:=Session.PlanRepair(R,RootLabels('terrain'));
    Check(Plan.CanExecute,'actual negotiated selective repair authorized');
    O:=Session.ExecuteRepair(Plan);
    Check(O.Solved and (O.Kind=wpsokNegotiatedSelective),'actual negotiated selective repair succeeds');
    Actions:=J.CopyActions; SetLength(Actions,3); Actions[2].Kind:=wpwakRepair;
    Actions[2].RunIndex:=0; Actions[2].RequestedRootIndices:=Plan.CopyScope.RequestedRootIndices;
    Actions[2].EvidenceText:=EncodeWfcPipelineSessionOutcomeEvidence(O,EvidenceLimits);
    FullJ:=CopyWithActions(J,Actions);
    State:=Session.CopyPublicState; Baseline:=Session.CopyLastSuccessfulState; Revision:=Session.Revision;
    FreeAndNil(O); FreeAndNil(Plan); FreeAndNil(Session); FreeAndNil(R); FreeAndNil(M);
    CheckRestoredState(FullJ,State,Baseline,Revision,True,True);
    Slot:=TWfcPipelineWorkspaceSlot.Create;
    Slot.Restore(FullJ,JournalLimits,ReplacementLimits,OutcomeLimits,EvidenceLimits,LimitsFor(FullJ),0);

    { Change a real excluded assignment while retaining every report/hash and
      context. Complete replay comparison, not an identity hash, must reject. }
    Actions:=FullJ.CopyActions;
    Line:='search.attempt.0.assignment.0='+IntToStr(N.Attempts[0].ExcludedAssignment[0]);
    Actions[1].EvidenceText:=ChangeOneLine(Actions[1].EvidenceText,Line,
      'search.attempt.0.assignment.0='+IntToStr(N.Attempts[0].ExcludedAssignment[0]+1));
    Bad:=CopyWithActions(FullJ,Actions);
    ExpectAtomicRejection(Slot,Bad,wpwrfEvidenceMismatch,1);
  finally
    Baseline.Free; State.Free; Slot.Free; Bad.Free; FullJ.Free; J.Free;
    Plan.Free; O.Free; Session.Free; R.Free; M.Free;
  end;
end;

procedure TestConnectivityOrdinal;
var M: TWfcPipelineModel; R: TWfcPipelineRun; Resources: TWfcPipelineResources;
  Passes: TWfcPipelinePasses; Connections: TWfcPipelineConnectivities;
  Profiles: TWfcPipelineConnectivityValues; Required: TGraphPositions;
  Domains: TWfcPipelineCellDomains; Session: TWfcPipelinePreparedSession;
  O: TWfcPipelineSessionOutcome; Report: TGraphSolveReport;
  State: TWfcPipelineSessionPublicState; J: TWfcPipelineWorkspaceJournal;
  Text: String;
begin
  M:=nil; R:=nil; Session:=nil; O:=nil; State:=nil; J:=nil;
  try
    SetLength(Resources,1); Resources[0]:=RulesResource('path-rules',['A','B']);
    SetLength(Passes,1);
    Passes[0]:=MakeWfcPipelinePass('path',wppvPublic,gpmOverlay,-1,wpakRules,0,False,wseWhole);
    SetLength(Profiles,1); Profiles[0]:=MakeWfcPipelineConnectivityValue('A',[gdEast,gdWest]);
    SetLength(Connections,2);
    Connections[0]:=MakeWfcPipelineConnectivity(0,'first-trivial',Position(0),nil,Profiles,False);
    SetLength(Required,1); Required[0]:=Position(2);
    Connections[1]:=MakeWfcPipelineConnectivity(0,'second-disconnected',Position(0),Required,Profiles,False);
    M:=TWfcPipelineModel.Create(MakeWfcPipelineMetadata('actual second disconnected path','MIT','',''),
      1,False,rmBottomUp,Resources,Passes,nil,nil,nil,nil,Connections);
    SetLength(Domains,3);
    Domains[0]:=MakeWfcPipelineCellDomain(0,0,0,0,Tokens(['A']));
    Domains[1]:=MakeWfcPipelineCellDomain(0,1,0,0,Tokens(['B']));
    Domains[2]:=MakeWfcPipelineCellDomain(0,2,0,0,Tokens(['A']));
    R:=TWfcPipelineRun.Create(M,3,1,1,17,wpssOneWay,32,0,True,nil,Domains);
    Session:=TWfcPipelinePreparedSession.Create(M,R,ReplacementLimits,OutcomeLimits);
    O:=Session.ExecuteInitial; Report:=O.CopySolveReport;
    Check(not O.Solved and not Session.HasSuccessfulBaseline,'actual disconnected initial fails without baseline');
    Check((Report.Contradiction.Kind=gckConnectivity) and (Report.Contradiction.ConstraintIndex=1),
      'actual second connectivity descriptor retains registry ordinal1');
    Text:=EncodeWfcPipelineSessionOutcomeEvidence(O,EvidenceLimits);
    CheckCapturedSolve(Report,True,Text,'solve');
    J:=InitialJournal(M,R,Text); State:=Session.CopyPublicState;
    CheckRestoredState(J,State,nil,Session.Revision,False,False);
  finally J.Free; State.Free; O.Free; Session.Free; R.Free; M.Free; end;
end;

procedure TestAdditionalReplayHistories;
begin
  TestFailedInitialAndPublication;
  TestNegotiatedHistory(False); TestNegotiatedHistory(True);
  TestConnectivityOrdinal;
end;
end.
