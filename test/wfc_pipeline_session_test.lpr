{ SPDX-License-Identifier: MIT
  Copyright (c) 2021 mr-highball
  Session lifecycle, capture bounds and ownership regression. }
program wfc_pipeline_session_test;
{$mode delphi}{$H+}
uses {$IFDEF PAS2JS}wfc_browser_test_host,{$ENDIF}
  SysUtils,wfc,wfc_lattice,wfc_model,wfc_sequence,wfc_rule_model,wfc_rule_text,
  wfc_pipeline_model,wfc_pipeline_layout,wfc_pipeline_run,wfc_pipeline_prepare,
  wfc_pipeline_session;
var Checks: Integer;
procedure Check(const OK: Boolean; const Detail: String);
begin Inc(Checks); if not OK then raise Exception.Create(Detail); end;
function Tokens(const A: array of TWfcModelToken): TWfcModelTokens;
var I: Integer;
begin Result:=nil; SetLength(Result,Length(A)); for I:=0 to High(A) do Result[I]:=A[I]; end;
function Roots(const A: array of String): TGraphPassLabels;
var I: Integer;
begin Result:=nil; SetLength(Result,Length(A)); for I:=0 to High(A) do Result[I]:=A[I]; end;
function Limits: TWfcPipelineReplacementLimits;
begin Result.Version:=1; Result.MaxRetainedCellRecords:=10000;
  Result.MaxRetainedValueItems:=10000; Result.MaxCandidateVisits:=1000000; end;
function CaptureLimits: TWfcPipelineSessionOutcomeLimits;
begin
  Result.Version:=1; Result.MaxPublicCellRecords:=10000; Result.MaxEncodedTokenBytes:=1000000;
  Result.MaxReportPassRecords:=1000; Result.MaxTraceEvents:=100000; Result.MaxExcludedAssignmentItems:=100000;
end;
function Recipe: TWfcPipelineModel;
var Rules: TWfcRuleModel; Weights: TWfcModelIntegerArray;
  R: TWfcPipelineResources; P: TWfcPipelinePasses; D: TWfcPipelineDependencies;
  T: TWfcPipelinePassTopologies; I: Integer;
begin
  SetLength(Weights,2); Weights[0]:=1; Weights[1]:=1;
  Rules:=TWfcRuleModel.Create(1,Tokens(['A','B']),Weights,nil);
  try SetLength(R,1); R[0]:=MakeWfcPipelineResource('rules',wprkRules,
    EncodeWfcRuleText(Rules),'literal values','MIT','fixture'); finally Rules.Free; end;
  SetLength(P,5); SetLength(T,5); SetLength(D,2);
  for I:=0 to 4 do
  begin
    P[I]:=MakeWfcPipelinePass(TWfcModelToken('pass-'+IntToStr(I)),wppvPublic,gpmOverlay,-1,wpakRules,0,False,wseWhole);
    T[I]:=LegacyWfcPipelinePassTopology(1,False);
  end;
  P[1]:=MakeWfcPipelinePass('alias-before',wppvPublic,gpmTransform,2,wpakEmpty,-1,False,wseWhole);
  P[3]:=MakeWfcPipelinePass('alias-after',wppvPublic,gpmTransform,2,wpakEmpty,-1,False,wseWhole);
  D[0]:=MakeWfcPipelineDependency(1,2); D[1]:=MakeWfcPipelineDependency(3,2);
  Result:=TWfcPipelineModel.Create(MakeWfcPipelineMetadata('session','MIT','private tests',''),
    CurrentWfcPipelineVersions,1,False,rmBottomUp,R,P,D,nil,nil,nil,nil,WFC_PASS_MAPPING_VERSION,T);
end;
function RunFor(const M: TWfcPipelineModel; const L: TWfcPipelineCellLocks;
  const D: TWfcPipelineCellDomains; const Negotiated: Boolean=False;
  const Trace: Boolean=True): TWfcPipelineRun;
var E: TWfcPipelinePassExtents; Strategy: TWfcPipelineSolveStrategy; PassBudget: Integer;
begin
  SetLength(E,5); E[0]:=MakeWfcLatticeVector(2,1,1); E[1]:=MakeWfcLatticeVector(5,1,1);
  E[2]:=E[1]; E[3]:=E[1]; E[4]:=MakeWfcLatticeVector(3,1,1);
  if Negotiated then begin Strategy:=wpssNegotiated; PassBudget:=4; end
  else begin Strategy:=wpssOneWay; PassBudget:=0; end;
  Result:=TWfcPipelineRun.Create(M,E,7,Strategy,64,PassBudget,Trace,L,D);
end;
function StateText(const State: TWfcPipelineSessionPublicState): String;
var I,J: Integer; L: TWfcPipelineSessionLayer;
begin
  Result:=''; for I:=0 to State.LayerCount-1 do
  begin
    L:=State.LayerAt(I); Result:=Result+IntToStr(L.PassIndex)+':'+String(L.LabelName)+':';
    for J:=0 to High(L.Cells) do Result:=Result+String(L.Cells[J].Token)+BoolToStr(L.Cells[J].Empty)+BoolToStr(L.Cells[J].Generated)+';';
  end;
end;
function SessionText(const S: TWfcPipelinePreparedSession): String;
var State: TWfcPipelineSessionPublicState;
begin State:=S.CopyPublicState; try Result:=StateText(State); finally State.Free; end; end;
procedure TestLifecycle(const Negotiated: Boolean);
var M: TWfcPipelineModel; Run,Changed,CopyRun: TWfcPipelineRun;
  S,Other: TWfcPipelinePreparedSession; O,Retained: TWfcPipelineSessionOutcome;
  Edit: TWfcPipelineSessionEditOutcome; Plan,Stale,Foreign: TWfcPipelineSessionRepairPlan;
  State,Last: TWfcPipelineSessionPublicState; Layer: TWfcPipelineSessionLayer;
  Scope: TWfcPipelineSessionScope; Locks: TWfcPipelineCellLocks; Domains: TWfcPipelineCellDomains;
  Report,R2: TGraphSolveReport; Selection: TGraphSelectiveNegotiationReport;
  Before: String; Saw: Boolean; Token: TWfcModelToken; Inv: TWfcPipelineSessionInvocation;
begin
  M:=Recipe; Run:=RunFor(M,nil,nil,Negotiated); Changed:=nil; S:=nil; Other:=nil;
  O:=nil; Retained:=nil; Edit:=nil; Plan:=nil; Stale:=nil; Foreign:=nil; State:=nil; Last:=nil; CopyRun:=nil;
  try
    S:=TWfcPipelinePreparedSession.Create(M,Run,Limits,CaptureLimits);
    Check((S.Revision=0) and S.Usable and not S.HasCurrentOutput and not S.HasSuccessfulBaseline,'fresh lifecycle');
    Last:=S.CopyLastSuccessfulState; Check(Last=nil,'no fabricated successful baseline');
    Plan:=S.PlanRepair(Run,Roots(['pass-2'])); Check(not Plan.CanExecute and Plan.MissingBaseline,'baseline refusal is inspectable');
    Saw:=False; try O:=S.ExecuteRepair(Plan); except on E:EWfcPipelineSession do Saw:=True; end;
    Check(Saw and S.Usable and (S.Revision=0),'baseline refusal does not mutate'); FreeAndNil(Plan);
    O:=S.ExecuteInitial; Check(O.Solved and O.HasCurrentOutput and O.HasSuccessfulBaseline,'initial output current');
    Check((O.Revision=1) and (S.Revision=1),'initial revision increments once');
    if Negotiated then Check(O.Kind=wpsokNegotiatedFull,'negotiated full tag')
    else Check(O.Kind=wpsokOrdinaryFull,'ordinary full tag');
    Scope:=O.CopyScope; Check((Length(Scope.RequestedRootIndices)=0) and (Length(Scope.ActivePassIndices)=5),'full scope all actual passes, no requested roots');
    Check((Scope.ActivePassIndices[1]=2) and (Scope.ActivePassIndices[2]=1),'non-numeric actual topology retained');
    Report:=O.CopySolveReport; Check(Report.TraceCaptured and (Length(Report.Trace)>0),'actual trace captured');
    Report.Passes[0].Decisions:=-777; Report.Trace[0].EventId:=-777;
    R2:=O.CopySolveReport; Check((R2.Passes[0].Decisions>=0) and (R2.Trace[0].EventId=0),'nested actual report copy detached');
    State:=O.CopyPublicState; Layer:=State.LayerAt(2); Token:=Layer.Cells[4].Token;
    Check(Layer.Cells[4].Generated and not Layer.Cells[4].Empty,'actual generated ownership captured');
    Layer.Cells[4].Token:='tampered'; Layer:=State.LayerAt(2); Check(Layer.Cells[4].Token=Token,'layer cells detached');
    FreeAndNil(State); Before:=SessionText(S); Retained:=O; O:=nil;
    Stale:=S.PlanRepair(Run,Roots(['pass-2']));
    SetLength(Locks,1); Locks[0]:=MakeWfcPipelineCellLock(1,4,0,0,Token);
    Changed:=RunFor(M,Locks,nil,Negotiated); Edit:=S.ApplyInputs(Changed);
    Check(Edit.Revision=2,'accepted edit increments revision');
    Check(not Edit.HasCurrentOutput and Edit.HasSuccessfulBaseline,'edit revokes currentness, retains baseline');
    Check((Length(Edit.CopyPendingPassIndices)=1) and (Edit.CopyPendingPassIndices[0]=2),'alias lock impacts actual provider');
    State:=Edit.CopyPublicState; Layer:=State.LayerAt(2); Check(not Layer.Cells[4].Generated,'same-token edit records caller ownership'); FreeAndNil(State);
    Last:=S.CopyLastSuccessfulState; Check(StateText(Last)=Before,'last success is detached old preview'); FreeAndNil(Last);
    Saw:=False; try O:=S.ExecuteRepair(Stale); except on E:EWfcPipelineSession do Saw:=True; end;
    Check(Saw and S.Usable and (S.Revision=2),'stale plan refused before mutation'); FreeAndNil(Stale);
    Plan:=S.PlanRepair(Changed,Roots(['alias-before'])); Scope:=Plan.CopyScope;
    Check(not Plan.CanExecute and not Plan.MissingBaseline and (Length(Scope.MissingPassIndices)=1) and (Scope.MissingPassIndices[0]=2),'leaf cannot authorize inverse/materialized provider');
    FreeAndNil(Plan); FreeAndNil(Edit); FreeAndNil(Changed);
    Edit:=S.ApplyInputs(Run); Check((S.Revision=3) and not S.HasCurrentOutput,'clear is distinct accepted edit');
    State:=Edit.CopyPublicState; Layer:=State.LayerAt(2); Check(Layer.Cells[4].Empty and not Layer.Cells[4].Generated,'clear empties provider instead of resurrecting generated token'); FreeAndNil(State); FreeAndNil(Edit);
    Check(Length(S.CopyPendingPassIndices)=1,'undo does not erase pending impact');
    SetLength(Domains,1); Domains[0]:=MakeWfcPipelineCellDomain(2,0,0,0,nil);
    Changed:=RunFor(M,nil,Domains,Negotiated); Edit:=S.ApplyInputs(Changed); FreeAndNil(Edit);
    Plan:=S.PlanRepair(Changed,Roots(['pass-2','pass-2'])); Check(Plan.CanExecute,'explicit provider closure is sufficient');
    O:=S.ExecuteRepair(Plan); Check(not O.Solved and S.Usable and not S.HasCurrentOutput,'normal solver failure is retained, not poisoned');
    Scope:=O.CopyScope; Check((Length(Scope.RequestedRootIndices)=1) and (Length(Scope.ActivePassIndices)=3),'canonical duplicate roots and real closure');
    Check(Length(O.CopyPendingPassIndices)=3,'failed repair retains entire authorized active closure');
    Report:=O.CopySolveReport; Check((Report.Status<>gssSolved) and (Length(Report.ExecutionOrder)<=3),'actual terminal failed report retained');
    if Negotiated then begin Selection:=O.CopySelectiveNegotiationReport;
      Check((O.Kind=wpsokNegotiatedSelective) and (Length(Selection.ActivePassIndices)=3),'actual selective negotiation wrapper'); end
    else Check(O.Kind=wpsokOrdinarySelective,'ordinary selective tag');
    FreeAndNil(O); FreeAndNil(Plan); FreeAndNil(Changed);
    Edit:=S.ApplyInputs(Run); FreeAndNil(Edit);
    Plan:=S.PlanRepair(Run,Roots(['pass-4'])); Check(not Plan.CanExecute,'unrelated later repair cannot bypass older failed region'); FreeAndNil(Plan);
    Plan:=S.PlanRepair(Run,Roots(['pass-2'])); O:=S.ExecuteRepair(Plan);
    Check(O.Solved and S.HasCurrentOutput and (Length(S.CopyPendingPassIndices)=0),'explicit sufficient repair settles pending');
    FreeAndNil(O); FreeAndNil(Plan);
    Before:=SessionText(S); Edit:=S.ApplyInputs(Run);
    Check(Edit.HasCurrentOutput and not Edit.CopyImpact.GraphInputsChanged,'accepted identical edit preserves currentness without graph writes');
    Check(SessionText(S)=Before,'identical edit preserves actual ownership'); FreeAndNil(Edit);
    Other:=TWfcPipelinePreparedSession.Create(M,Run,Limits,CaptureLimits); O:=Other.ExecuteInitial; FreeAndNil(O);
    Foreign:=Other.PlanRepair(Run,Roots(['pass-2'])); Saw:=False;
    try O:=S.ExecuteRepair(Foreign); except on E:EWfcPipelineSession do Saw:=True; end;
    Check(Saw and S.Usable,'foreign identical session plan rejected');
    FreeAndNil(Other); Saw:=False; try O:=S.ExecuteRepair(Foreign); except on E:EWfcPipelineSession do Saw:=True; end;
    Check(Saw,'foreign plan lease survives owner disposal without gaining authority');
    FreeAndNil(Foreign); CopyRun:=S.CopyAppliedRun; Check(CopyRun.Seed=Run.Seed,'copied applied invocation retained'); FreeAndNil(CopyRun);
    FreeAndNil(S); FreeAndNil(Run); FreeAndNil(M);
    Inv:=Retained.CopyInvocation; Check((Inv.Seed=7) and (Length(Inv.Extents)=5),'outcome invocation survives session and recipe disposal');
    State:=Retained.CopyPublicState; Check(State.LayerCount=5,'detached public outcome survives all source owners'); FreeAndNil(State);
  finally CopyRun.Free; Last.Free; State.Free; Foreign.Free; Stale.Free; Plan.Free; Edit.Free;
    Retained.Free; O.Free; Other.Free; S.Free; Changed.Free; Run.Free; M.Free; end;
end;
procedure TestInitialAndLimits;
var M: TWfcPipelineModel; Run,Changed: TWfcPipelineRun; S: TWfcPipelinePreparedSession;
  O: TWfcPipelineSessionOutcome; E: TWfcPipelineSessionEditOutcome; P: TWfcPipelineSessionRepairPlan;
  D: TWfcPipelineCellDomains; C: TWfcPipelineSessionOutcomeLimits; Saw: Boolean;
  R: TGraphSolveReport;
begin
  M:=Recipe; Run:=RunFor(M,nil,nil,False,False); Changed:=nil; S:=nil; O:=nil; E:=nil; P:=nil;
  try
    S:=TWfcPipelinePreparedSession.Create(M,Run,Limits,CaptureLimits);
    E:=S.ApplyInputs(Run); Check((S.Revision=1) and not E.HasCurrentOutput,'pre-initial accepted edit revision'); FreeAndNil(E);
    O:=S.ExecuteInitial; R:=O.CopySolveReport;
    Check(O.Solved and (S.Revision=2) and not R.TraceCaptured and (Length(R.Trace)=0),'initial follows pre-edit, capture-off preserved'); FreeAndNil(O);
    Saw:=False; try O:=S.ExecuteInitial; except on Ex:EWfcPipelineSession do Saw:=True; end;
    Check(Saw and S.Usable and (S.Revision=2),'initial is once-only'); FreeAndNil(S);
    SetLength(D,1); D[0]:=MakeWfcPipelineCellDomain(2,0,0,0,nil); Changed:=RunFor(M,nil,D);
    S:=TWfcPipelinePreparedSession.Create(M,Changed,Limits,CaptureLimits); O:=S.ExecuteInitial;
    Check(not O.Solved and not S.HasSuccessfulBaseline and S.Usable,'failed initial is honest usable nonbaseline'); FreeAndNil(O);
    P:=S.PlanRepair(Changed,Roots(['pass-2'])); Check(P.MissingBaseline and not P.CanExecute,'failed initial never enables repair'); FreeAndNil(P); FreeAndNil(S); FreeAndNil(Changed);
    C:=CaptureLimits; C.MaxReportPassRecords:=1; S:=TWfcPipelinePreparedSession.Create(M,Run,Limits,C);
    Saw:=False; try O:=S.ExecuteInitial; except on Ex:EWfcPipelineSession do Saw:=Pos('capture limit',Ex.Message)>0; end;
    Check(Saw and not S.Usable and not S.HasCurrentOutput and (O=nil),'post-solve report capture limit poisons private candidate');
    Check(not S.HasSuccessfulBaseline,'failed first capture does not fabricate an empty successful snapshot');
    Saw:=False; try E:=S.ApplyInputs(Run); except on Ex:EWfcPipelineSession do Saw:=True; end;
    Check(Saw and (E=nil),'poisoned session rejects later mutation'); FreeAndNil(S);
    C:=CaptureLimits; C.Version:=0; Saw:=False;
    try S:=TWfcPipelinePreparedSession.Create(M,Run,Limits,C); except on Ex:EWfcPipelineSession do Saw:=True; end;
    Check(Saw and (S=nil),'invalid capture version refuses construction');
    C:=CaptureLimits; C.MaxPublicCellRecords:=1; Saw:=False;
    try S:=TWfcPipelinePreparedSession.Create(M,Run,Limits,C); except on Ex:EWfcPipelineSession do Saw:=True; end;
    Check(Saw and (S=nil),'explicit public-cell cap enforced');
  finally P.Free; E.Free; O.Free; S.Free; Changed.Free; Run.Free; M.Free; end;
end;
procedure TestUninitializedOwners;
var S: TWfcPipelinePreparedSession; P: TWfcPipelineSessionRepairPlan;
  O: TWfcPipelineSessionOutcome; E: TWfcPipelineSessionEditOutcome;
  State: TWfcPipelineSessionPublicState; Saw: Boolean; Dummy: Integer;
begin
  S:=TWfcPipelinePreparedSession(TClass(TWfcPipelinePreparedSession).Create);
  P:=TWfcPipelineSessionRepairPlan(TClass(TWfcPipelineSessionRepairPlan).Create);
  O:=TWfcPipelineSessionOutcome(TClass(TWfcPipelineSessionOutcome).Create);
  E:=TWfcPipelineSessionEditOutcome(TClass(TWfcPipelineSessionEditOutcome).Create);
  State:=TWfcPipelineSessionPublicState(TClass(TWfcPipelineSessionPublicState).Create);
  try
    Saw:=False; try Dummy:=S.Revision; except on Ex:EWfcPipelineSession do Saw:=True; end;
    Check(Saw,'inherited uninitialized session constructor rejected');
    Saw:=False; try Dummy:=P.BaseRevision; except on Ex:EWfcPipelineSession do Saw:=True; end;
    Check(Saw,'inherited uninitialized plan constructor rejected');
    Saw:=False; try Dummy:=O.Revision; except on Ex:EWfcPipelineSession do Saw:=True; end;
    Check(Saw,'inherited uninitialized solve outcome constructor rejected');
    Saw:=False; try Dummy:=E.Revision; except on Ex:EWfcPipelineSession do Saw:=True; end;
    Check(Saw,'inherited uninitialized edit outcome constructor rejected');
    Saw:=False; try Dummy:=State.LayerCount; except on Ex:EWfcPipelineSession do Saw:=True; end;
    Check(Saw,'inherited uninitialized public-state constructor rejected');
  finally State.Free; E.Free; O.Free; P.Free; S.Free; end;
end;
procedure TestCaptureAccounting;
var M: TWfcPipelineModel; Run: TWfcPipelineRun; S: TWfcPipelinePreparedSession;
  O: TWfcPipelineSessionOutcome; C: TWfcPipelineSessionOutcomeLimits;
  Saw: Boolean; I,Assignments,PassRows: Integer; N: TGraphNegotiationReport;
  Resources: TWfcPipelineResources; Passes: TWfcPipelinePasses;
  Dependencies: TWfcPipelineDependencies; Requirements: TWfcPipelineRequirements;
  Terms: TWfcPipelineRequirementTerms;
  function RulesResource(const Name: TWfcModelToken; const Values: array of TWfcModelToken): TWfcPipelineResource;
  var R: TWfcRuleModel; W: TWfcModelIntegerArray; J: Integer;
  begin
    SetLength(W,Length(Values)); for J:=0 to High(W) do W[J]:=1;
    R:=TWfcRuleModel.Create(1,Tokens(Values),W,nil);
    try Result:=MakeWfcPipelineResource(Name,wprkRules,EncodeWfcRuleText(R),'accounting','MIT',''); finally R.Free; end;
  end;
  procedure RejectAfterSolve(const LimitsValue: TWfcPipelineSessionOutcomeLimits; const LabelText: String);
  begin
    S:=TWfcPipelinePreparedSession.Create(M,Run,Limits,LimitsValue); Saw:=False;
    try O:=S.ExecuteInitial; except on E:EWfcPipelineSession do Saw:=Pos('capture limit',E.Message)>0; end;
    Check(Saw and not S.Usable and (O=nil),LabelText);
    FreeAndNil(S);
  end;
begin
  M:=Recipe; Run:=RunFor(M,nil,nil,False,False); S:=nil; O:=nil;
  try
    C:=CaptureLimits; C.MaxEncodedTokenBytes:=0;
    for I:=0 to M.PassCount-1 do C.MaxEncodedTokenBytes:=C.MaxEncodedTokenBytes+Length(M.PassAt(I).LabelName);
    RejectAfterSolve(C,'actual cell token bytes charged beyond empty-state labels');
    FreeAndNil(Run); Run:=RunFor(M,nil,nil,False,True); C:=CaptureLimits; C.MaxTraceEvents:=1;
    RejectAfterSolve(C,'actual captured trace event limit poisons after solve');
    FreeAndNil(Run); FreeAndNil(M);
    SetLength(Resources,2); Resources[0]:=RulesResource('land',['marsh','meadow']);
    Resources[1]:=RulesResource('building',['cottage']); SetLength(Passes,2);
    Passes[0]:=MakeWfcPipelinePass('terrain',wppvPublic,gpmOverlay,-1,wpakRules,0,False,wseWhole);
    Passes[1]:=MakeWfcPipelinePass('housing',wppvPublic,gpmOverlay,-1,wpakRules,1,False,wseWhole);
    SetLength(Dependencies,1); Dependencies[0]:=MakeWfcPipelineDependency(1,0);
    SetLength(Terms,1); Terms[0]:=MakeWfcPipelineRequirementTerm(0,0,0,Tokens(['meadow']));
    SetLength(Requirements,1); Requirements[0]:=MakeWfcPipelineRequirement(1,'cottage',0,wprqExact,Terms);
    M:=TWfcPipelineModel.Create(MakeWfcPipelineMetadata('negotiation accounting','MIT','',''),
      1,False,rmBottomUp,Resources,Passes,Dependencies,nil,Requirements);
    Run:=TWfcPipelineRun.Create(M,2,1,1,0,wpssNegotiated,32,8,True,nil,nil);
    S:=TWfcPipelinePreparedSession.Create(M,Run,Limits,CaptureLimits); O:=S.ExecuteInitial;
    N:=O.CopyNegotiationReport; Check(O.Solved and (Length(N.Attempts)>0),'accounting uses actual rejected attempts');
    Assignments:=0; PassRows:=Length(N.FinalReport.Passes);
    for I:=0 to High(N.Attempts) do
    begin Inc(Assignments,Length(N.Attempts[I].ExcludedAssignment)); Inc(PassRows,Length(N.Attempts[I].SolveReport.Passes)); end;
    Check(Assignments>1,'fixture actually stores multiple excluded items'); FreeAndNil(O); FreeAndNil(S);
    C:=CaptureLimits; C.MaxExcludedAssignmentItems:=Assignments-1;
    RejectAfterSolve(C,'all rejected assignments count toward capture limit');
    C:=CaptureLimits; C.MaxReportPassRecords:=PassRows-1;
    RejectAfterSolve(C,'all rejected and final pass reports count toward capture limit');
    C:=CaptureLimits; C.MaxReportPassRecords:=PassRows; C.MaxExcludedAssignmentItems:=Assignments;
    S:=TWfcPipelinePreparedSession.Create(M,Run,Limits,C); O:=S.ExecuteInitial;
    Check(O.Solved and S.Usable,'exact single-representation nested report budgets succeed');
  finally O.Free; S.Free; Run.Free; M.Free; end;
end;
{$IFDEF PAS2JS}
procedure TestHostileHandles;
var M: TWfcPipelineModel; Run: TWfcPipelineRun; S,Bad: TWfcPipelinePreparedSession;
  O: TWfcPipelineSessionOutcome; P,BadPlan: TWfcPipelineSessionRepairPlan;
  C: TWfcPipelineSessionOutcomeLimits; RootsValue: TGraphPassLabels;
  Saw: Boolean; Calls,Choice: Integer;
begin
  M:=Recipe; Run:=RunFor(M,nil,nil); S:=nil; O:=nil; P:=nil;
  try
    S:=TWfcPipelinePreparedSession.Create(M,Run,Limits,CaptureLimits); O:=S.ExecuteInitial; FreeAndNil(O);
    for Choice:=0 to 3 do
    begin
      Calls:=0;
      asm
        if(Choice===0) BadPlan=null;
        if(Choice===1) BadPlan={};
        if(Choice===2) BadPlan=Object.create(pas.wfc_pipeline_session.TWfcPipelineSessionRepairPlan);
        if(Choice===3) { BadPlan={};Object.defineProperty(BadPlan,'FData',{get:function(){Calls++;throw new Error('getter');}}); }
      end;
      Saw:=False; try O:=S.ExecuteRepair(BadPlan); except on Ex:EWfcPipelineSession do Saw:=True; end;
      Check(Saw and (Calls=0) and S.Usable and (S.Revision=1),'hostile plan rejected without getters or mutation');
    end;
    asm Bad=Object.create(pas.wfc_pipeline_session.TWfcPipelinePreparedSession);Object.defineProperty(Bad,'FData',{get:function(){Calls++;throw new Error('getter');}}); end;
    Calls:=0; Saw:=False; try P:=Bad.PlanRepair(Run,Roots(['pass-2'])); except on Ex:EWfcPipelineSession do Saw:=True; end;
    Check(Saw and (Calls=0),'forged session receiver rejected passively');
    RootsValue:=Roots(['pass-2']); asm delete RootsValue[0]; end;
    Saw:=False; try P:=S.PlanRepair(Run,RootsValue); except on Ex:EWfcPipelineSession do Saw:=True; end;
    Check(Saw and S.Usable and (S.Revision=1),'sparse roots reject without mutation');
    for Choice:=0 to 5 do
    begin
      Calls:=0;
      asm
        if(Choice===0) RootsValue=null;
        if(Choice===1) RootsValue=undefined;
        if(Choice===2) RootsValue=7;
        if(Choice===3) RootsValue='pass-2';
        if(Choice===4) RootsValue={};
        if(Choice===5) {RootsValue=['pass-2'];Object.defineProperty(RootsValue,'0',{get:function(){Calls++;throw new Error('getter');}});}
      end;
      Saw:=False; try P:=S.PlanRepair(Run,RootsValue); except on Ex:EWfcPipelineSession do Saw:=True; end;
      Check(Saw and (Calls=0) and S.Usable and (S.Revision=1),'raw roots reach typed passive core validation before array copying');
    end;
    asm RootsValue=Object.freeze(['pass-2']); end;
    P:=S.PlanRepair(Run,RootsValue); Check(P.CanExecute,'frozen dense roots are read-only valid input'); FreeAndNil(P);
    C:=CaptureLimits; Calls:=0;
    asm Object.defineProperty(C,'MaxTraceEvents',{get:function(){Calls++;throw new Error('getter');}}); end;
    Bad:=nil; Saw:=False; try Bad:=TWfcPipelinePreparedSession.Create(M,Run,Limits,C); except on Ex:EWfcPipelineSession do Saw:=True; end;
    Check(Saw and (Calls=0) and (Bad=nil),'getter-backed limits reject before graph construction');
  finally P.Free; O.Free; S.Free; Run.Free; M.Free; end;
end;
{$ENDIF}
begin
  try
    TestLifecycle(False); TestLifecycle(True); TestInitialAndLimits; TestUninitializedOwners; TestCaptureAccounting;
    {$IFDEF PAS2JS}TestHostileHandles;{$ENDIF}
    WriteLn('Private prepared session checks: ',Checks);
  except on E:Exception do begin WriteLn('FAIL: ',E.ClassName,': ',E.Message); Halt(1); end; end;
end.
