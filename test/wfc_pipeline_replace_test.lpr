{ SPDX-License-Identifier: MIT
  Copyright (c) 2021 mr-highball
  Real editable binding ownership and resource boundary regression. }
program wfc_pipeline_replace_test;
{$mode delphi}{$H+}
uses {$IFDEF PAS2JS}wfc_browser_test_host,{$ENDIF}
  SysUtils,wfc,wfc_lattice,wfc_model,wfc_sequence,wfc_rule_model,wfc_rule_text,
  wfc_pipeline_model,wfc_pipeline_layout,wfc_pipeline_run,wfc_pipeline_prepare;
var Checks: Integer;
procedure Check(const OK: Boolean; const Detail: String);
begin Inc(Checks); if not OK then raise Exception.Create(Detail); end;
function Tokens(const A: array of TWfcModelToken): TWfcModelTokens;
var I: Integer;
begin Result:=nil; SetLength(Result,Length(A)); for I:=0 to High(A) do Result[I]:=A[I]; end;
function Limits: TWfcPipelineReplacementLimits;
begin Result.Version:=1; Result.MaxRetainedCellRecords:=10000;
  Result.MaxRetainedValueItems:=10000; Result.MaxCandidateVisits:=1000000; end;
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
  { Earlier alias refers to later owner: closure is not numeric pass order. }
  P[1]:=MakeWfcPipelinePass('alias-before',wppvPublic,gpmTransform,2,wpakEmpty,-1,False,wseWhole);
  P[3]:=MakeWfcPipelinePass('alias-after',wppvPublic,gpmTransform,2,wpakEmpty,-1,False,wseWhole);
  D[0]:=MakeWfcPipelineDependency(1,2); D[1]:=MakeWfcPipelineDependency(3,2);
  Result:=TWfcPipelineModel.Create(MakeWfcPipelineMetadata('replacement','MIT','private tests',''),
    CurrentWfcPipelineVersions,1,False,rmBottomUp,R,P,D,nil,nil,nil,nil,WFC_PASS_MAPPING_VERSION,T);
end;
function RunFor(const M: TWfcPipelineModel; const L: TWfcPipelineCellLocks;
  const D: TWfcPipelineCellDomains; const Budget: Integer=64): TWfcPipelineRun;
var E: TWfcPipelinePassExtents;
begin
  SetLength(E,5); E[0]:=MakeWfcLatticeVector(2,1,1); E[1]:=MakeWfcLatticeVector(5,1,1);
  E[2]:=E[1]; E[3]:=E[1]; E[4]:=MakeWfcLatticeVector(3,1,1);
  Result:=TWfcPipelineRun.Create(M,E,7,wpssOneWay,Budget,0,True,L,D);
end;
function PlanFor(const P: TWfcPipelinePreparation; const M: TWfcPipelineModel;
  const L: TWfcPipelineCellLocks; const D: TWfcPipelineCellDomains; const Budget: Integer=64): TWfcPipelineInputPlan;
var Run: TWfcPipelineRun;
begin Run:=RunFor(M,L,D,Budget); try Result:=P.PrepareInputs(Run); finally Run.Free; end; end;
function Change(const B: TWfcPipelineInputBinding; const P: TWfcPipelinePreparation;
  const M: TWfcPipelineModel; const L: TWfcPipelineCellLocks; const D: TWfcPipelineCellDomains;
  const Budget: Integer=64): TWfcPipelineInputImpact;
var Plan: TWfcPipelineInputPlan;
begin Plan:=PlanFor(P,M,L,D,Budget); try Result:=B.ReplaceInputs(Plan); finally Plan.Free; end; end;
function State(const G: TGraph): String;
var Saved,I,J,K: Integer; P: TGraph; V: TGraphValues; E: TGraphEntry;
begin
  Saved:=G.CurrentPassIndex; G.SwitchToPass(0); Result:=IntToStr(G.Seed)+'|';
  try
    for I:=0 to G.TotalPassCount-1 do
    begin
      P:=G.PassGraph[I];
      for J:=0 to P.PassLayout.Cells.X-1 do
      begin
        E:=P.Entry[J,0,0];
        Result:=Result+IntToStr(I)+','+IntToStr(J)+':'+String(E.Value)+':'+BoolToStr(E.Empty)+':'+BoolToStr(E.Generated)+':'+BoolToStr(P.HasAllowedValues(J,0,0));
        V:=P.CopyAllowedValues(J,0,0); for K:=0 to High(V) do Result:=Result+','+String(V[K]);
        Result:=Result+';';
      end;
    end;
  finally G.SwitchToPass(Saved); end;
end;
procedure Solve(const G: TGraph);
var O: TGraphSolveOptions; R: TGraphSolveReport;
begin O:=Default(TGraphSolveOptions); O.MaxBacktracks:=64; Check(G.TrySolve(O,R),'real graph solve succeeds'); end;
procedure TestEdits;
var M: TWfcPipelineModel; Run: TWfcPipelineRun; P: TWfcPipelinePreparation;
  Plan: TWfcPipelineInputPlan; B,Fresh: TWfcPipelineInputBinding; G: TGraph;
  L: TWfcPipelineCellLocks; D: TWfcPipelineCellDomains; I: TWfcPipelineInputImpact;
  Before,Branch: String; Token: TWfcModelToken; Saw: Boolean; V: TGraphValues;
begin
  M:=Recipe; Run:=RunFor(M,nil,nil); P:=nil; Plan:=nil; B:=nil; Fresh:=nil;
  try
    P:=TWfcPipelinePreparation.Create(M,Run); Plan:=P.PrepareInputs(Run);
    B:=TWfcPipelineInputBinding.CreateEditable(P,Plan,Limits); G:=B.BorrowCompiled.Graph;
    Fresh:=TWfcPipelineInputBinding.Create(P,Plan); Saw:=False;
    try Fresh.ReplaceInputs(Plan); except on E: EWfcPipelineRuntime do Saw:=Pos('fresh-only',E.Message)>0; end;
    Check(Saw and Fresh.Usable,'fresh-only replacement rejects without poisoning');
    Solve(G); G.SwitchToPass(4); Before:=State(G); Branch:=String(G.Entry[2,0,0].Value);
    I:=B.ReplaceInputs(Plan);
    Check(not I.AuthoredInputsChanged and not I.GraphInputsChanged,'identical plan is no-op');
    Check((Before=State(G)) and (G.CurrentPassIndex=4),'no-op preserves full graph/seed/selection');
    I:=Change(B,P,M,nil,nil,17);
    Check(not I.AuthoredInputsChanged and not I.GraphInputsChanged,'options-only change not graph or author edit');
    Check(Before=State(G),'options-only change does not clear generated output');

    Token:=TWfcModelToken(G.PassGraph[2].Entry[4,0,0].Value);
    SetLength(L,2); L[0]:=MakeWfcPipelineCellLock(1,4,0,0,Token); L[1]:=MakeWfcPipelineCellLock(3,4,0,0,Token);
    I:=Change(B,P,M,L,nil);
    Check(I.AuthoredInputsChanged and I.GraphInputsChanged,'two aliases install one effective lock');
    Check((Length(I.AuthoredPassIndices)=2) and (I.AuthoredPassIndices[0]=1) and (I.AuthoredPassIndices[1]=3),'all alias authors reported ascending');
    Check((Length(I.ChangedPassIndices)=1) and (I.ChangedPassIndices[0]=2),'actual later owner impacted, not alias or ancestor');
    Check(not G.PassGraph[2].Entry[4,0,0].Generated,'same-token assignment changes generated to caller ownership');
    Check(G.CurrentPassIndex=4,'changed edit restores unrelated active pass');
    I.ChangedPassIndices[0]:=99;
    Before:=State(G); SetLength(L,1); I:=Change(B,P,M,L,nil);
    Check(I.AuthoredInputsChanged and not I.GraphInputsChanged,'remove one alias is provenance-only');
    Check(Before=State(G),'surviving contributor keeps lock without any rewrite');
    L[0]:=MakeWfcPipelineCellLock(2,4,0,0,Token); I:=Change(B,P,M,L,nil);
    Check(I.AuthoredInputsChanged and not I.GraphInputsChanged,'relocate alias lock to direct owner without rewriting');
    I:=Change(B,P,M,nil,nil);
    Check(I.GraphInputsChanged and (I.ChangedPassIndices[0]=2),'last lock removal impacts materialized owner');
    Check(G.PassGraph[2].Entry[4,0,0].Empty and not G.PassGraph[2].Entry[4,0,0].Generated,'lock then clear remains a real empty cell');
    Check(G.PassGraph[1].Entry[4,0,0].Generated,'clearing owner does not pretend stale alias was repaired');
    Check((String(G.PassGraph[4].Entry[2,0,0].Value)=Branch) and G.PassGraph[4].Entry[2,0,0].Generated,'independent branch untouched');
    Solve(G);

    SetLength(D,2); D[0]:=MakeWfcPipelineCellDomain(1,4,0,0,Tokens(['A','B']));
    D[1]:=MakeWfcPipelineCellDomain(3,4,0,0,Tokens(['A']));
    I:=Change(B,P,M,nil,D);
    V:=G.PassGraph[2].CopyAllowedValues(4,0,0);
    Check((Length(V)=1) and (V[0]='A'),'alias domain contributors intersect');
    Check(G.PassGraph[2].Entry[4,0,0].Generated,'domain-only narrowing preserves generated ownership');
    SetLength(L,1); L[0]:=MakeWfcPipelineCellLock(1,4,0,0,'A'); Change(B,P,M,L,D);
    SetLength(D,1); I:=Change(B,P,M,L,D);
    Check(I.GraphInputsChanged and (Length(G.PassGraph[2].CopyAllowedValues(4,0,0))=2),'broaden weaker domain under unchanged lock');
    Change(B,P,M,nil,D);
    Check(G.PassGraph[2].Entry[4,0,0].Empty and G.PassGraph[2].HasAllowedValues(4,0,0),'unlock reveals surviving full domain');
    D[0]:=MakeWfcPipelineCellDomain(1,4,0,0,nil); I:=Change(B,P,M,nil,D);
    Check(G.PassGraph[2].HasAllowedValues(4,0,0) and (Length(G.PassGraph[2].CopyAllowedValues(4,0,0))=0),'explicit empty domain distinct from absent');
    I:=Change(B,P,M,nil,nil);
    Check(I.GraphInputsChanged and not G.PassGraph[2].HasAllowedValues(4,0,0),'clear restores absent compiler base');

    SetLength(D,2); D[0]:=MakeWfcPipelineCellDomain(0,1,0,0,Tokens(['B']));
    D[1]:=MakeWfcPipelineCellDomain(4,2,0,0,Tokens(['A'])); G.SwitchToPass(4);
    I:=Change(B,P,M,nil,D);
    Check((G.CurrentPassIndex=4) and (Length(I.ChangedPassIndices)=2) and (I.ChangedPassIndices[0]=0) and (I.ChangedPassIndices[1]=4),'unlike root and late pass edits have separate keys and stable selection');
    G.SwitchToPass(0); V:=G.CopyAllowedValues(1,0,0); Check((Length(V)=1) and (V[0]='B'),'root zero edit reaches root, not late facade');
    V:=G.PassGraph[4].CopyAllowedValues(2,0,0); Check((Length(V)=1) and (V[0]='A'),'late unlike pass key retained');
    Change(B,P,M,nil,nil); Check(not G.HasAllowedValues(1,0,0),'later first-touched root base restores absent');

    SetLength(L,2); L[0]:=MakeWfcPipelineCellLock(1,0,0,0,'A'); L[1]:=MakeWfcPipelineCellLock(3,0,0,0,'B');
    Before:=State(G); Saw:=False;
    try Change(B,P,M,L,nil); except on E: EWfcPipelineRuntime do Saw:=True; end;
    Check(Saw and B.Usable and (Before=State(G)),'conflicting aliases reject during pure lowering before graph writes');
  finally Fresh.Free; B.Free; Plan.Free; P.Free; Run.Free; M.Free; end;
end;

procedure TestLifetimeAndLimits;
var M: TWfcPipelineModel; Run: TWfcPipelineRun; P,Q: TWfcPipelinePreparation;
  Plan,Desired,Foreign: TWfcPipelineInputPlan; B: TWfcPipelineInputBinding;
  Bound: TWfcPipelineReplacementLimits; L: TWfcPipelineCellLocks; D: TWfcPipelineCellDomains;
  Before: String; Saw: Boolean; Mode,I: Integer; Impact: TWfcPipelineInputImpact;
begin
  M:=Recipe; Run:=RunFor(M,nil,nil); P:=nil; Q:=nil; Plan:=nil; Desired:=nil; Foreign:=nil; B:=nil;
  try
    P:=TWfcPipelinePreparation.Create(M,Run); Plan:=P.PrepareInputs(Run);
    SetLength(L,1); L[0]:=MakeWfcPipelineCellLock(1,4,0,0,'A');
    SetLength(D,1); D[0]:=MakeWfcPipelineCellDomain(0,1,0,0,Tokens(['A']));
    Desired:=PlanFor(P,M,L,D);
    for Mode:=0 to 2 do
    begin
      Bound:=Limits;
      case Mode of 0:Bound.MaxRetainedCellRecords:=22; 1:Bound.MaxRetainedValueItems:=1; 2:Bound.MaxCandidateVisits:=32; end;
      B:=TWfcPipelineInputBinding.CreateEditable(P,Plan,Bound); B.BorrowCompiled.Graph.SwitchToPass(4);
      Before:=State(B.BorrowCompiled.Graph); Saw:=False;
      try B.ReplaceInputs(Desired); except on E: EWfcPipelineRuntime do Saw:=Pos('limit exceeded',E.Message)>0; end;
      Check(Saw and B.Usable,'custom replacement resource bound rejects typed, usable');
      Check((Before=State(B.BorrowCompiled.Graph)) and (B.BorrowCompiled.Graph.CurrentPassIndex=4),'budget rejection leaves graph/cache/selection unchanged');
      Impact:=B.ReplaceInputs(Plan); Check(not Impact.GraphInputsChanged,'old applied payload survives rejected replacement');
      FreeAndNil(B);
    end;
    Bound:=Limits; B:=TWfcPipelineInputBinding.CreateEditable(P,Plan,Bound);
    Impact:=B.ReplaceInputs(Desired); Check(Impact.GraphInputsChanged,'caller may explicitly raise all three resource limits');
    Q:=TWfcPipelinePreparation.Create(M,Run); Foreign:=Q.PrepareInputs(Run); Before:=State(B.BorrowCompiled.Graph);
    Saw:=False; try B.ReplaceInputs(Foreign); except on E: EWfcPipelineRuntime do Saw:=Pos('different preparation lifetime',E.Message)>0; end;
    Check(Saw and B.Usable and (Before=State(B.BorrowCompiled.Graph)),'identical different producer lacks replacement authority');
    FreeAndNil(Q); Saw:=False;
    try B.ReplaceInputs(Foreign); except on E: EWfcPipelineRuntime do Saw:=True; end;
    Check(Saw and B.Usable,'foreign retained identity still rejects after its producer dies');
    FreeAndNil(P);
    Impact:=B.ReplaceInputs(Plan); Check(Impact.GraphInputsChanged,'retained own plan usable after producer destroyed');
    Check(Plan.CopyLocks=nil,'plan inspection still detached after producer destroyed');
    FreeAndNil(B);
    P:=TWfcPipelinePreparation.Create(M,Run); FreeAndNil(Plan); Plan:=P.PrepareInputs(Run);
    Bound:=Limits; Bound.MaxRetainedCellRecords:=High(Integer); Bound.MaxRetainedValueItems:=High(Integer); Bound.MaxCandidateVisits:=High(Integer);
    B:=TWfcPipelineInputBinding.CreateEditable(P,Plan,Bound);
    Check(B.Usable,'all positive logical limits accept full compiler-neutral Integer range');
    FreeAndNil(B);
    for I:=0 to 4 do
    begin
      Bound:=Limits; case I of 0:Bound.Version:=2; 1:Bound.MaxRetainedCellRecords:=0; 2:Bound.MaxRetainedValueItems:=-1; 3:Bound.MaxCandidateVisits:=0; 4:Bound.Version:=0; end;
      Saw:=False; try B:=TWfcPipelineInputBinding.CreateEditable(nil,nil,Bound);
      except on E: EWfcPipelineRuntime do Saw:=Pos('replacement limits',E.Message)>0; end;
      Check(Saw and (B=nil),'typed malformed limits reject before owner or graph access');
    end;
  finally B.Free; Foreign.Free; Desired.Free; Plan.Free; Q.Free; P.Free; Run.Free; M.Free; end;
end;

{$IFDEF PAS2JS}
procedure TestRawBoundaries;
var M: TWfcPipelineModel; Run: TWfcPipelineRun; P: TWfcPipelinePreparation;
  Plan,BadPlan,EmptyPlan: TWfcPipelineInputPlan; B,Made,BadBinding: TWfcPipelineInputBinding;
  Bound: TWfcPipelineReplacementLimits; I,J,Reads: Integer; Saw: Boolean; Before: String;
begin
  M:=Recipe; Run:=RunFor(M,nil,nil); P:=nil; Plan:=nil; B:=nil; Made:=nil; EmptyPlan:=nil;
  try
    P:=TWfcPipelinePreparation.Create(M,Run); Plan:=P.PrepareInputs(Run);
    B:=TWfcPipelineInputBinding.CreateEditable(P,Plan,Limits); Before:=State(B.BorrowCompiled.Graph);
    for I:=0 to 4 do
    begin
      Reads:=0; Saw:=False;
      asm Bound=[null,undefined,1,'limits',[]][I]; end;
      try Made:=TWfcPipelineInputBinding.CreateEditable(P,Plan,Bound);
      except on E: EWfcPipelineRuntime do Saw:=Pos('replacement limits',E.Message)>0; end;
      Check(Saw and (Made=nil),'raw outer limit shape rejects typed');
    end;
    for I:=0 to 3 do for J:=0 to 9 do
    begin
      Reads:=0; Saw:=False;
      asm
        Bound={Version:1,MaxRetainedCellRecords:10000,MaxRetainedValueItems:10000,MaxCandidateVisits:1000000};
        let key=['Version','MaxRetainedCellRecords','MaxRetainedValueItems','MaxCandidateVisits'][I];
        if(J===0)delete Bound[key];
        else if(J===1)Object.defineProperty(Bound,key,{get:function(){Reads++;throw new Error('limit getter');}});
        else Bound[key]=[null,null,NaN,Infinity,0.5,'2',0,-1,new Number(1),2147483648][J];
      end;
      try Made:=TWfcPipelineInputBinding.CreateEditable(P,Plan,Bound);
      except on E: EWfcPipelineRuntime do Saw:=Pos('replacement limits',E.Message)>0; end;
      Check(Saw and (Made=nil),'invalid passive/integer limit field rejects typed');
      Check(Reads=0,'limit getter never executes');
    end;
    for I:=0 to 8 do
    begin
      Reads:=0; Saw:=False;
      asm
        BadPlan=[null,undefined,1,'plan',[],{},{$class:pas.wfc_pipeline_prepare.TWfcPipelineInputPlan},
          Object.create(pas.wfc_pipeline_prepare.TWfcPipelineInputPlan),Object.create(null)][I];
        if(BadPlan!==null && typeof BadPlan==='object')Object.defineProperty(BadPlan,'FData',
          {get:function(){Reads++;throw new Error('plan getter');}});
      end;
      try B.ReplaceInputs(BadPlan); except on E: EWfcPipelineRuntime do Saw:=True; end;
      Check(Saw and (Reads=0),'foreign desired plan rejects before getter');
      Check(B.Usable and (State(B.BorrowCompiled.Graph)=Before),'foreign desired plan rejection does not poison/change graph');
    end;
    Reads:=0; Saw:=False;
    asm BadBinding=Object.create(pas.wfc_pipeline_prepare.TWfcPipelineInputBinding);
      Object.defineProperty(BadBinding,'FCompiled',{get:function(){Reads++;throw new Error('binding getter');}}); end;
    try BadBinding.ReplaceInputs(Plan); except on E: EWfcPipelineRuntime do Saw:=True; end;
    Check(Saw and (Reads=0),'foreign replacement receiver rejects before graph getter');
    EmptyPlan:=TWfcPipelineInputPlan.Create; Saw:=False;
    try B.ReplaceInputs(EmptyPlan); except on E: EWfcPipelineRuntime do Saw:=True; end;
    Check(Saw and B.Usable,'real uninitialized plan cannot replace inputs');
    asm Bound=Object.create({Version:1,MaxRetainedCellRecords:10000,MaxRetainedValueItems:10000,MaxCandidateVisits:1000000}); end;
    Made:=TWfcPipelineInputBinding.CreateEditable(P,Plan,Bound);
    Check(Made.Usable,'passive inherited record defaults accepted');
  finally EmptyPlan.Free; Made.Free; B.Free; Plan.Free; P.Free; Run.Free; M.Free; end;
end;
{$ENDIF}
begin
  TestEdits; TestLifetimeAndLimits;
  {$IFDEF PAS2JS}TestRawBoundaries;{$ENDIF}
  WriteLn('Private pipeline replacement: ',Checks,' checks passed.');
end.
