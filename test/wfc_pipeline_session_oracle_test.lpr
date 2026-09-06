{ SPDX-License-Identifier: MIT
  Copyright (c) 2021 mr-highball
  Independent prepared-session integration against real direct-core calls. }
program wfc_pipeline_session_oracle_test;
{$mode delphi}{$H+}
uses {$IFDEF PAS2JS}wfc_browser_test_host,{$ENDIF}
  SysUtils,wfc,wfc_model,wfc_lattice,wfc_rule_model,wfc_rule_text,wfc_sequence,
  wfc_pipeline_model,wfc_pipeline_layout,wfc_pipeline_run,wfc_pipeline_prepare,
  wfc_pipeline_session,wfc_session_oracle_helpers;

function Tokens(const A: array of TWfcModelToken): TWfcModelTokens;
var I: Integer;
begin Result:=nil; SetLength(Result,Length(A)); for I:=0 to High(A) do Result[I]:=A[I]; end;
function RulesResource(const Id: TWfcModelToken;
  const A: array of TWfcModelToken): TWfcPipelineResource;
var R: TWfcRuleModel; W: TWfcModelIntegerArray; I: Integer;
begin
  SetLength(W,Length(A)); for I:=0 to High(W) do W[I]:=1;
  R:=TWfcRuleModel.Create(1,Tokens(A),W,nil);
  try Result:=MakeWfcPipelineResource(Id,wprkRules,EncodeWfcRuleText(R),
    'independent session fixture','MIT','literal'); finally R.Free; end;
end;
function ReplacementLimits: TWfcPipelineReplacementLimits;
begin
  Result.Version:=1; Result.MaxRetainedCellRecords:=10000;
  Result.MaxRetainedValueItems:=10000; Result.MaxCandidateVisits:=1000000;
end;
function OutcomeLimits: TWfcPipelineSessionOutcomeLimits;
begin
  Result.Version:=1; Result.MaxPublicCellRecords:=10000;
  Result.MaxEncodedTokenBytes:=1000000; Result.MaxReportPassRecords:=10000;
  Result.MaxTraceEvents:=100000; Result.MaxExcludedAssignmentItems:=10000;
end;
function BranchRecipe: TWfcPipelineModel;
var R: TWfcPipelineResources; P: TWfcPipelinePasses;
  D: TWfcPipelineDependencies; T: TWfcPipelinePassTopologies; I: Integer;
begin
  SetLength(R,1); R[0]:=RulesResource('letters',['A','B']);
  SetLength(P,4); SetLength(T,4);
  for I:=0 to 3 do
  begin
    P[I]:=MakeWfcPipelinePass('unused',wppvPublic,gpmOverlay,-1,wpakRules,0,False,wseWhole);
    T[I]:=LegacyWfcPipelinePassTopology(1,False);
    T[I].Origin:=MakeWfcLatticeVector(-7,0,0); T[I].Pitch:=MakeWfcLatticeVector(3,1,1);
  end;
  P[0].LabelName:='root'; P[2].LabelName:='branch'; P[3].LabelName:='provider';
  P[1]:=MakeWfcPipelinePass('alias',wppvPublic,gpmTransform,3,wpakEmpty,-1,False,wseWhole);
  SetLength(D,1); D[0]:=MakeWfcPipelineDependency(1,3);
  Result:=TWfcPipelineModel.Create(MakeWfcPipelineMetadata('session oracle','MIT','',''),
    CurrentWfcPipelineVersions,1,False,rmBottomUp,R,P,D,nil,nil,nil,nil,WFC_PASS_MAPPING_VERSION,T);
end;
function BranchRun(const M: TWfcPipelineModel; const Strategy: TWfcPipelineSolveStrategy;
  const Capture: Boolean; const L: TWfcPipelineCellLocks;
  const D: TWfcPipelineCellDomains): TWfcPipelineRun;
var Extents: TWfcPipelinePassExtents; PassBudget: Integer;
begin
  SetLength(Extents,4); Extents[0]:=MakeWfcLatticeVector(1,1,1);
  Extents[1]:=MakeWfcLatticeVector(2,1,1); Extents[2]:=MakeWfcLatticeVector(3,1,1);
  Extents[3]:=Extents[1];
  PassBudget:=0; if Strategy=wpssNegotiated then PassBudget:=8;
  Result:=TWfcPipelineRun.Create(M,Extents,0,Strategy,32,PassBudget,Capture,L,D);
end;
procedure CheckOutcome(const O: TWfcPipelineSessionOutcome;
  const Session: TWfcPipelinePreparedSession;
  const B: TWfcPipelineInputBinding; const M: TWfcPipelineModel; const R: TWfcPipelineRun;
  const Roots: TGraphPassLabels; const Full: Boolean);
var S: TGraphSolveReport; N: TGraphNegotiationReport; Q: TGraphSelectiveNegotiationReport;
  SO: TGraphSolveOptions; NO: TGraphNegotiationOptions;
  State: TWfcPipelineSessionPublicState; Scope: TWfcPipelineSessionScope;
  ExpectedRoots,ExpectedActive: TGraphPassIndices; Solved: Boolean;
  CopyReport: TGraphSolveReport; AllRoots: TGraphPassLabels; I: Integer;
begin
  Check(O.Revision=Session.Revision,'detached outcome actual revision');
  Check(O.HasCurrentOutput=Session.HasCurrentOutput,'detached outcome actual currentness');
  Check(O.HasSuccessfulBaseline=Session.HasSuccessfulBaseline,'detached outcome actual baseline flag');
  EqualIndices(O.CopyPendingPassIndices,Session.CopyPendingPassIndices,'detached outcome pending');
  SO:=DefaultGraphSolveOptions; SO.MaxBacktracks:=R.MaxBacktracks; SO.CaptureTrace:=R.CaptureTrace;
  NO:=DefaultGraphNegotiationOptions; NO.SolveOptions:=SO; NO.MaxPassBacktracks:=R.MaxPassBacktracks;
  if R.Strategy=wpssOneWay then
  begin
    if Full then Solved:=B.BorrowCompiled.Graph.TrySolve(SO,S)
    else Solved:=B.BorrowCompiled.Graph.TryRegenerateFrom(Roots,SO,S);
    EqualSolve(O.CopySolveReport,S);
    Check(not O.HasNegotiation and not O.HasSelectiveNegotiation,'ordinary report tag');
    if Full then Check(O.Kind=wpsokOrdinaryFull,'ordinary full tag')
    else Check(O.Kind=wpsokOrdinarySelective,'ordinary selective tag');
  end
  else if Full then
  begin
    Solved:=B.BorrowCompiled.Graph.TrySolveNegotiated(NO,N); S:=N.FinalReport;
    EqualNegotiation(O.CopyNegotiationReport,N); EqualSolve(O.CopySolveReport,S);
    Check(O.HasNegotiation and not O.HasSelectiveNegotiation,'full negotiated report availability');
    Check(O.Kind=wpsokNegotiatedFull,'negotiated full tag');
  end
  else
  begin
    Solved:=B.BorrowCompiled.Graph.TryRegenerateNegotiatedFrom(Roots,NO,Q); S:=Q.Search.FinalReport;
    EqualSelective(O.CopySelectiveNegotiationReport,Q);
    EqualNegotiation(O.CopyNegotiationReport,Q.Search); EqualSolve(O.CopySolveReport,S);
    Check(O.HasNegotiation and O.HasSelectiveNegotiation,'selective negotiated report availability');
    Check(O.Kind=wpsokNegotiatedSelective,'negotiated selective tag');
  end;
  Check(O.Solved=Solved,'actual solved result'); EqualValidation(O.LastValidation,B.BorrowCompiled.LastValidation);
  EqualInvocation(O.CopyInvocation,R);
  Scope:=O.CopyScope;
  Check(Scope.ScopeAlgorithmVersion=WFC_SELECTIVE_NEGOTIATION_ALGORITHM_VERSION,'outcome scope algorithm');
  if Full then
  begin
    SetLength(AllRoots,M.PassCount);
    for I:=0 to M.PassCount-1 do AllRoots[I]:=String(M.PassAt(I).LabelName);
    B.BorrowCompiled.Graph.ResolveRegenerationScope(AllRoots,ExpectedRoots,ExpectedActive);
    Check(Length(Scope.RequestedRootIndices)=0,'full outcome invents no selective roots');
    EqualIndices(Scope.ActivePassIndices,ExpectedActive,'full actual topological active set');
  end
  else
  begin
    B.BorrowCompiled.Graph.ResolveRegenerationScope(Roots,ExpectedRoots,ExpectedActive);
    Scope:=O.CopyScope; EqualIndices(Scope.RequestedRootIndices,ExpectedRoots,'outcome requested roots');
    EqualIndices(Scope.ActivePassIndices,ExpectedActive,'outcome complete authorized closure');
  end;
  State:=O.CopyPublicState;
  try EqualState(State,M,R,B.BorrowCompiled.Graph); finally State.Free; end;
  { Modify every managed family in a returned terminal copy, then request it
    again. The direct-core report above must remain a separate actual oracle. }
  CopyReport:=O.CopySolveReport;
  if Length(CopyReport.Passes)>0 then CopyReport.Passes[0].Decisions:=99999;
  if Length(CopyReport.ExecutionOrder)>0 then CopyReport.ExecutionOrder[0]:=99999;
  if Length(CopyReport.Trace)>0 then CopyReport.Trace[0].Value:='not-the-retained-value';
  EqualSolve(O.CopySolveReport,S);
end;
procedure ApplyBoth(const Session: TWfcPipelinePreparedSession;
  const P: TWfcPipelinePreparation; const B: TWfcPipelineInputBinding;
  const M: TWfcPipelineModel; const R: TWfcPipelineRun);
var Edit: TWfcPipelineSessionEditOutcome; Plan: TWfcPipelineInputPlan;
  Expected,Actual: TWfcPipelineInputImpact; State: TWfcPipelineSessionPublicState; Revision: Integer;
begin
  Revision:=Session.Revision; Edit:=nil; Plan:=P.PrepareInputs(R);
  try
    Edit:=Session.ApplyInputs(R); Expected:=B.ReplaceInputs(Plan); Actual:=Edit.CopyImpact;
    Check(Session.Revision=Revision+1,'one revision per accepted edit');
    Check(Edit.Revision=Session.Revision,'edit revision matches owner');
    Check(Edit.HasCurrentOutput=Session.HasCurrentOutput,'edit detached currentness');
    Check(Edit.HasSuccessfulBaseline=Session.HasSuccessfulBaseline,'edit detached baseline');
    EqualIndices(Edit.CopyPendingPassIndices,Session.CopyPendingPassIndices,'edit detached pending');
    Check(Actual.AuthoredInputsChanged=Expected.AuthoredInputsChanged,'edit authored change truth');
    Check(Actual.GraphInputsChanged=Expected.GraphInputsChanged,'edit graph change truth');
    EqualIndices(Actual.AuthoredPassIndices,Expected.AuthoredPassIndices,'edit authors');
    EqualIndices(Actual.ChangedPassIndices,Expected.ChangedPassIndices,'edit materialized impacts');
    EqualInvocation(Edit.CopyInvocation,R);
    State:=Edit.CopyPublicState;
    try EqualState(State,M,R,B.BorrowCompiled.Graph); finally State.Free; end;
  finally Edit.Free; Plan.Free; end;
end;
procedure RepairBoth(const Session: TWfcPipelinePreparedSession;
  const B: TWfcPipelineInputBinding; const M: TWfcPipelineModel;
  const R: TWfcPipelineRun; const Roots: TGraphPassLabels; const ExpectedSolved: Boolean);
var Plan: TWfcPipelineSessionRepairPlan; O: TWfcPipelineSessionOutcome; Revision: Integer;
  BeforeScope,AfterScope: TWfcPipelineSessionScope;
begin
  Revision:=Session.Revision; Plan:=Session.PlanRepair(R,Roots); O:=nil;
  try
    Check(Plan.CanExecute and not Plan.MissingBaseline,'explicit sufficient repair plan');
    Check(Plan.BaseRevision=Revision,'plan exact base revision');
    BeforeScope:=Plan.CopyScope;
    O:=Session.ExecuteRepair(Plan); CheckOutcome(O,Session,B,M,R,Roots,False);
    AfterScope:=O.CopyScope;
    EqualIndices(AfterScope.RequiredPassIndices,BeforeScope.RequiredPassIndices,'outcome retains planned requirements');
    EqualIndices(AfterScope.AuthoredPassIndices,BeforeScope.AuthoredPassIndices,'outcome retains planned authors');
    EqualIndices(AfterScope.MissingPassIndices,BeforeScope.MissingPassIndices,'outcome retains planned missing set');
    Check(O.Solved=ExpectedSolved,'fixture repair expected semantic result');
    Check(Session.Revision=Revision+1,'one revision per normal repair result');
  finally O.Free; Plan.Free; end;
end;
function Labels(const A: array of String): TGraphPassLabels;
var I: Integer;
begin Result:=nil; SetLength(Result,Length(A)); for I:=0 to High(A) do Result[I]:=A[I]; end;
procedure TestHistory(const Strategy: TWfcPipelineSolveStrategy; const Capture: Boolean);
var M: TWfcPipelineModel; R,Desired: TWfcPipelineRun; P: TWfcPipelinePreparation;
  Input: TWfcPipelineInputPlan; B: TWfcPipelineInputBinding; Session: TWfcPipelinePreparedSession;
  Initial: TWfcPipelineSessionOutcome; Plan,Stale: TWfcPipelineSessionRepairPlan;
  L: TWfcPipelineCellLocks; D: TWfcPipelineCellDomains; Pending: TGraphPassIndices;
  State,Baseline: TWfcPipelineSessionPublicState; Layer: TWfcPipelineSessionLayer;
  Token: TWfcModelToken; Rejected: Boolean; Rev: Integer; Scope: TWfcPipelineSessionScope;
begin
  M:=BranchRecipe; R:=BranchRun(M,Strategy,Capture,nil,nil); Desired:=nil;
  P:=nil; Input:=nil; B:=nil; Session:=nil; Initial:=nil; Plan:=nil; Stale:=nil; Baseline:=nil;
  try
    P:=TWfcPipelinePreparation.Create(M,R); Input:=P.PrepareInputs(R);
    B:=TWfcPipelineInputBinding.CreateEditable(P,Input,ReplacementLimits);
    Session:=TWfcPipelinePreparedSession.Create(M,R,ReplacementLimits,OutcomeLimits);
    Check(Session.Revision=0,'new owner starts revision zero');
    Plan:=Session.PlanRepair(R,Labels(['provider']));
    Check(not Plan.CanExecute and Plan.MissingBaseline,'baseline required before selective solve');
    FreeAndNil(Plan);
    Initial:=Session.ExecuteInitial; CheckOutcome(Initial,Session,B,M,R,nil,True);
    Check(Initial.Solved and Session.HasCurrentOutput,'initial branch fixture solved/current');
    Check(Session.Revision=1,'initial increments revision');
    Baseline:=Session.CopyLastSuccessfulState;
    EqualState(Baseline,M,R,B.BorrowCompiled.Graph);
    Stale:=Session.PlanRepair(R,Labels(['alias']));
    Layer:=Baseline.LayerAt(3); Token:=Layer.Cells[1].Token;
    SetLength(L,1); L[0]:=MakeWfcPipelineCellLock(1,1,0,0,Token);
    Desired:=BranchRun(M,Strategy,Capture,L,nil); ApplyBoth(Session,P,B,M,Desired); FreeAndNil(Desired);
    Check(not Session.HasCurrentOutput,'accepted lock invalidates current output');
    ApplyBoth(Session,P,B,M,R);
    State:=Session.CopyPublicState;
    try
      Layer:=State.LayerAt(3);
      Check(Layer.Cells[1].Empty and not Layer.Cells[1].Generated,'lock then unlock is not a coalesced no-op');
    finally State.Free; end;
    Layer:=Baseline.LayerAt(3); Check(not Layer.Cells[1].Empty and Layer.Cells[1].Generated,'old success is separately retained generated preview');
    Pending:=Session.CopyPendingPassIndices;
    Check((Length(Pending)=1) and (Pending[0]=3),'provider pending survives net-zero authored lock map');
    Rev:=Session.Revision; Rejected:=False;
    FreeAndNil(Initial);
    try Initial:=Session.ExecuteRepair(Stale);
    except on E: EWfcPipelineSession do Rejected:=True; end;
    Check(Rejected and Session.Usable and (Session.Revision=Rev),'stale plan rejects without revision/mutation');
    Plan:=Session.PlanRepair(R,Labels(['alias'])); Scope:=Plan.CopyScope;
    Check(not Plan.CanExecute and not Plan.MissingBaseline,'leaf cannot silently authorize backing provider');
    Check((Length(Scope.MissingPassIndices)=1) and (Scope.MissingPassIndices[0]=3),'precise missing provider');
    Check((Length(Scope.AuthoredPassIndices)=1) and (Scope.AuthoredPassIndices[0]=1),'authored alias provenance survives both edits');
    FreeAndNil(Plan);
    RepairBoth(Session,B,M,R,Labels(['provider','provider']),True);
    Check(Session.HasCurrentOutput and (Length(Session.CopyPendingPassIndices)=0),'explicit backing repair clears pending');

    SetLength(D,1); D[0]:=MakeWfcPipelineCellDomain(2,2,0,0,nil);
    Desired:=BranchRun(M,Strategy,Capture,nil,D); ApplyBoth(Session,P,B,M,Desired);
    RepairBoth(Session,B,M,Desired,Labels(['branch']),False); FreeAndNil(Desired);
    Check(not Session.HasCurrentOutput and Session.HasSuccessfulBaseline,'normal failed repair is dirty, not no-baseline');
    ApplyBoth(Session,P,B,M,R);
    D[0]:=MakeWfcPipelineCellDomain(0,0,0,0,Tokens(['A']));
    Desired:=BranchRun(M,Strategy,Capture,nil,D); ApplyBoth(Session,P,B,M,Desired);
    Plan:=Session.PlanRepair(Desired,Labels(['root'])); Scope:=Plan.CopyScope;
    Check(not Plan.CanExecute,'later unrelated edit cannot forget prior failed region');
    Check((Length(Scope.MissingPassIndices)=1) and (Scope.MissingPassIndices[0]=2),'old failed branch remains missing');
    FreeAndNil(Plan);
    RepairBoth(Session,B,M,Desired,Labels(['branch','root','branch']),True);
    Check(Session.HasCurrentOutput,'explicit both-region repair restores currentness');
    State:=Session.CopyPublicState;
    try EqualState(State,M,Desired,B.BorrowCompiled.Graph); finally State.Free; end;
    FreeAndNil(Desired); ApplyBoth(Session,P,B,M,R);
    RepairBoth(Session,B,M,R,Labels(['root']),True);
    { Old detached state remains readable after every producing owner is gone. }
    FreeAndNil(Session); FreeAndNil(B); FreeAndNil(Input); FreeAndNil(P); FreeAndNil(R); FreeAndNil(M);
    Layer:=Baseline.LayerAt(3); Check(Layer.Cells[1].Token=Token,'detached state outlives recipe and session');
  finally
    Baseline.Free; Stale.Free; Plan.Free; Initial.Free; Session.Free; B.Free;
    Input.Free; P.Free; Desired.Free; R.Free; M.Free;
  end;
end;
function NegotiationRecipe: TWfcPipelineModel;
var R: TWfcPipelineResources; P: TWfcPipelinePasses; D: TWfcPipelineDependencies;
  Q: TWfcPipelineRequirements; T: TWfcPipelineRequirementTerms;
begin
  SetLength(R,2); R[0]:=RulesResource('land',['marsh','meadow']); R[1]:=RulesResource('building',['cottage']);
  SetLength(P,2); P[0]:=MakeWfcPipelinePass('terrain',wppvPublic,gpmOverlay,-1,wpakRules,0,False,wseWhole);
  P[1]:=MakeWfcPipelinePass('housing',wppvPublic,gpmOverlay,-1,wpakRules,1,False,wseWhole);
  SetLength(D,1); D[0]:=MakeWfcPipelineDependency(1,0);
  SetLength(T,1); T[0]:=MakeWfcPipelineRequirementTerm(0,0,0,Tokens(['meadow']));
  SetLength(Q,1); Q[0]:=MakeWfcPipelineRequirement(1,'cottage',0,wprqExact,T);
  Result:=TWfcPipelineModel.Create(MakeWfcPipelineMetadata('real negotiation','MIT','',''),
    1,False,rmBottomUp,R,P,D,nil,Q);
end;
procedure TestRejectedAttempts(const Capture: Boolean);
var M: TWfcPipelineModel; R: TWfcPipelineRun; P: TWfcPipelinePreparation;
  Input: TWfcPipelineInputPlan; B: TWfcPipelineInputBinding;
  Session: TWfcPipelinePreparedSession; O: TWfcPipelineSessionOutcome;
  N,Again: TGraphNegotiationReport; Plan: TWfcPipelineSessionRepairPlan;
begin
  M:=NegotiationRecipe;
  R:=TWfcPipelineRun.Create(M,1,1,1,0,wpssNegotiated,32,8,Capture,nil,nil);
  P:=nil; Input:=nil; B:=nil; Session:=nil; O:=nil; Plan:=nil;
  try
    P:=TWfcPipelinePreparation.Create(M,R); Input:=P.PrepareInputs(R);
    B:=TWfcPipelineInputBinding.CreateEditable(P,Input,ReplacementLimits);
    Session:=TWfcPipelinePreparedSession.Create(M,R,ReplacementLimits,OutcomeLimits);
    O:=Session.ExecuteInitial; CheckOutcome(O,Session,B,M,R,nil,True);
    N:=O.CopyNegotiationReport;
    Check(Length(N.Attempts)>0,'fixture actually retains rejected assignments');
    Check(Length(N.Attempts[0].ExcludedAssignment)=1,'actual one-cell excluded assignment');
    N.Attempts[0].ExcludedAssignment[0]:=9999;
    N.Attempts[0].SolveReport.Passes[0].Backtracks:=9999;
    Again:=O.CopyNegotiationReport;
    Check(Again.Attempts[0].ExcludedAssignment[0]<>9999,'nested assignment copy detached');
    Check(Again.Attempts[0].SolveReport.Passes[0].Backtracks<>9999,'nested attempt report copy detached');
    FreeAndNil(O);
    RepairBoth(Session,B,M,R,Labels(['terrain']),True);
  finally Plan.Free; O.Free; Session.Free; B.Free; Input.Free; P.Free; R.Free; M.Free; end;
end;
procedure TestConstraintOrdinal;
var M: TWfcPipelineModel; R: TWfcPipelineRun; Resources: TWfcPipelineResources;
  Passes: TWfcPipelinePasses; Quotas: TWfcPipelineValueQuotas;
  P: TWfcPipelinePreparation; Input: TWfcPipelineInputPlan; B: TWfcPipelineInputBinding;
  Session: TWfcPipelinePreparedSession; O: TWfcPipelineSessionOutcome;
  Report: TGraphSolveReport; Plan: TWfcPipelineSessionRepairPlan;
begin
  SetLength(Resources,1); Resources[0]:=RulesResource('quota-rules',['A','B']);
  SetLength(Passes,1); Passes[0]:=MakeWfcPipelinePass('quota-pass',wppvPublic,gpmOverlay,-1,wpakRules,0,False,wseWhole);
  SetLength(Quotas,2);
  Quotas[0]:=MakeWfcPipelineValueQuota(0,'first-valid',Tokens(['A']),0,1);
  Quotas[1]:=MakeWfcPipelineValueQuota(0,'second-impossible',Tokens(['B']),2,2);
  M:=TWfcPipelineModel.Create(MakeWfcPipelineMetadata('quota ordinal oracle','MIT','',''),
    1,False,rmBottomUp,Resources,Passes,nil,nil,nil,Quotas);
  R:=TWfcPipelineRun.Create(M,1,1,1,0,wpssOneWay,32,0,True,nil,nil);
  P:=nil; Input:=nil; B:=nil; Session:=nil; O:=nil; Plan:=nil;
  try
    P:=TWfcPipelinePreparation.Create(M,R); Input:=P.PrepareInputs(R);
    B:=TWfcPipelineInputBinding.CreateEditable(P,Input,ReplacementLimits);
    Session:=TWfcPipelinePreparedSession.Create(M,R,ReplacementLimits,OutcomeLimits);
    O:=Session.ExecuteInitial; CheckOutcome(O,Session,B,M,R,nil,True); Report:=O.CopySolveReport;
    Check(not O.Solved and not Session.HasSuccessfulBaseline,'impossible quota never enables baseline');
    Check((Report.Contradiction.Kind=gckValueQuota) and (Report.Contradiction.ConstraintIndex=1),
      'real failed second quota keeps nonzero registry ordinal');
    Plan:=Session.PlanRepair(R,Labels(['quota-pass']));
    Check(not Plan.CanExecute and Plan.MissingBaseline,'failed initial cannot enable selective repair');
  finally Plan.Free; O.Free; Session.Free; B.Free; Input.Free; P.Free; R.Free; M.Free; end;
end;
var Strategy: TWfcPipelineSolveStrategy; Capture: Boolean;
begin
  for Strategy:=Low(TWfcPipelineSolveStrategy) to High(TWfcPipelineSolveStrategy) do
    for Capture:=False to True do TestHistory(Strategy,Capture);
  TestRejectedAttempts(False); TestRejectedAttempts(True);
  TestConstraintOrdinal;
  WriteLn('Prepared session independent oracle: ',OracleChecks,' checks passed.');
end.
