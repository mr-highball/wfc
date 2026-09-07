{ SPDX-License-Identifier: MIT
  Copyright (c) 2021 mr-highball
  Detached preparation and initial binding ownership regression. }
program wfc_pipeline_prepare_test;
{$mode delphi}{$H+}
uses {$IFDEF PAS2JS}wfc_browser_test_host,{$ENDIF}
  SysUtils,wfc,wfc_model,wfc_sequence,wfc_rule_model,wfc_rule_text,
  wfc_pipeline_model,wfc_pipeline_layout,wfc_pipeline_run,wfc_pipeline_result,
  wfc_pipeline_result_text,wfc_pipeline_prepare,wfc_pipeline_runtime;
type TPreparationAncestorClass = class of TObject;
var Checks: Integer;
procedure Check(const OK: Boolean; const Detail: String);
begin Inc(Checks); if not OK then raise Exception.Create(Detail); end;
function Tokens(const A: array of TWfcModelToken): TWfcModelTokens;
var I: Integer;
begin Result:=nil; SetLength(Result,Length(A)); for I:=0 to High(A) do Result[I]:=A[I]; end;

function Recipe: TWfcPipelineModel;
var Rules: TWfcRuleModel; Weights: TWfcModelIntegerArray;
  R: TWfcPipelineResources; P: TWfcPipelinePasses; D: TWfcPipelineDependencies;
begin
  SetLength(Weights,2); Weights[0]:=1; Weights[1]:=1;
  Rules:=TWfcRuleModel.Create(1,Tokens(['A','B']),Weights,nil);
  try SetLength(R,1); R[0]:=MakeWfcPipelineResource('rules',wprkRules,
    EncodeWfcRuleText(Rules),'literal values','MIT','fixture'); finally Rules.Free; end;
  SetLength(P,3); SetLength(D,2);
  P[0]:=MakeWfcPipelinePass('public',wppvPublic,gpmOverlay,-1,wpakRules,0,False,wseWhole);
  P[1]:=MakeWfcPipelinePass('alias1',wppvPublic,gpmTransform,0,wpakEmpty,-1,False,wseWhole);
  P[2]:=MakeWfcPipelinePass('alias2',wppvPublic,gpmTransform,0,wpakEmpty,-1,False,wseWhole);
  D[0]:=MakeWfcPipelineDependency(1,0); D[1]:=MakeWfcPipelineDependency(2,0);
  Result:=TWfcPipelineModel.Create(MakeWfcPipelineMetadata('preparation','MIT','pure lowering',''),
    1,False,rmBottomUp,R,P,D,nil,nil);
end;

function Invocation(const M: TWfcPipelineModel; const Seed: Cardinal=7;
  const Width: Integer=3; const MaxBacktracks: Integer=32): TWfcPipelineRun;
var Locks: TWfcPipelineCellLocks; Domains: TWfcPipelineCellDomains;
begin
  SetLength(Locks,1); Locks[0]:=MakeWfcPipelineCellLock(1,1,0,0,'A');
  SetLength(Domains,1); Domains[0]:=MakeWfcPipelineCellDomain(2,1,0,0,Tokens(['A','B']));
  Result:=TWfcPipelineRun.Create(M,Width,1,1,Seed,wpssOneWay,MaxBacktracks,0,True,Locks,Domains);
end;

procedure TestDetachedLifetime;
var M: TWfcPipelineModel; Run,Changed: TWfcPipelineRun;
  P,Other: TWfcPipelinePreparation; Plan,NewPlan: TWfcPipelineInputPlan;
  B: TWfcPipelineInputBinding; Locks: TWfcPipelineCellLocks;
  Domains: TWfcPipelineCellDomains; Layout: TWfcPipelineLayoutTable;
  Saw: Boolean; I: Integer; Options: TGraphSolveOptions; Report: TGraphSolveReport;
begin
  M:=Recipe; Run:=Invocation(M); P:=nil; Other:=nil; Plan:=nil; NewPlan:=nil; B:=nil; Changed:=nil;
  try
    P:=TWfcPipelinePreparation.Create(M,Run); Plan:=P.PrepareInputs(Run);
    Locks:=Plan.CopyLocks; Domains:=Plan.CopyDomains;
    Check((Length(Locks)=1) and (Locks[0].PassIndex=1),'authored alias lock preserved');
    Check((Length(Domains)=1) and (Domains[0].PassIndex=2),'authored alias domain preserved');
    Locks[0].Token:='B'; Domains[0].AllowedTokens[0]:='mutated';
    Check(Plan.CopyLocks[0].Token='A','copied locks detached');
    Check(Plan.CopyDomains[0].AllowedTokens[0]='A','copied nested domain tokens detached');
    Layout:=Plan.CopyPassLayouts;
    try Check((Layout.PassCount=3) and (Layout.TotalCellCount=9),'owned actual-pass layout table'); finally Layout.Free; end;
    Changed:=Invocation(M,7,3,17); NewPlan:=P.PrepareInputs(Changed);
    Check(NewPlan.CopyLocks[0].Token='A','search options may change within epoch');
    FreeAndNil(NewPlan); FreeAndNil(Changed);
    Changed:=Invocation(M,8); Saw:=False;
    try NewPlan:=P.PrepareInputs(Changed); except on E: EWfcPipelineRuntime do Saw:=Pos('epoch',E.Message)>0; end;
    Check(Saw and (NewPlan=nil),'changed seed rejected without returned plan'); FreeAndNil(Changed);
    Changed:=Invocation(M,7,4); Saw:=False;
    try NewPlan:=P.PrepareInputs(Changed); except on E: EWfcPipelineRuntime do Saw:=Pos('epoch',E.Message)>0; end;
    Check(Saw and (NewPlan=nil),'changed geometry rejected without returned plan'); FreeAndNil(Changed);
    Other:=TWfcPipelinePreparation.Create(M,Run); Saw:=False;
    try B:=TWfcPipelineInputBinding.Create(Other,Plan); except on E: EWfcPipelineRuntime do Saw:=Pos('different preparation lifetime',E.Message)>0; end;
    Check(Saw and (B=nil),'identical recipe and run do not grant producer authority'); FreeAndNil(Other);
    B:=TWfcPipelineInputBinding.Create(P,Plan);
    Check(not B.BorrowCompiled.Graph.PassGraph[0].Entry[1,0,0].Empty,'alias lock materialized on public owner');
    Check(not B.BorrowCompiled.Graph.PassGraph[0].Entry[1,0,0].Generated,'authored lock is not generated');
    Check(B.BorrowCompiled.Graph.PassGraph[1].Entry[1,0,0].Empty,'alias itself is not prefilled as a caller lock');
    FreeAndNil(P); FreeAndNil(Run);
    Check(Plan.CopyLocks[0].Token='A','plan inspection survives producer and run release');
    Run:=Invocation(M);
    for I:=0 to 63 do
    begin
      Other:=TWfcPipelinePreparation.Create(M,Run); Saw:=False;
      try
        try with TWfcPipelineInputBinding.Create(Other,Plan) do Free;
        except on E: EWfcPipelineRuntime do Saw:=Pos('different preparation lifetime',E.Message)>0; end;
        Check(Saw,'retained identity lease prevents producer allocator-reuse acceptance');
      finally FreeAndNil(Other); end;
    end;
    FreeAndNil(Plan); FreeAndNil(Run);
    Options:=Default(TGraphSolveOptions); Options.MaxBacktracks:=32;
    Check(B.BorrowCompiled.Graph.TrySolve(Options,Report),'binding survives caller plan/preparation/run release');
    Check(not B.BorrowCompiled.Graph.PassGraph[1].Entry[1,0,0].Empty,'real solve generates projection alias');
    Check(B.BorrowCompiled.Graph.PassGraph[1].Entry[1,0,0].Generated,'projection retains generated ownership');
  finally B.Free; NewPlan.Free; Plan.Free; Other.Free; P.Free; Changed.Free; Run.Free; M.Free; end;
end;

procedure TestFreshRuntimeParity;
var M: TWfcPipelineModel; Run: TWfcPipelineRun; P: TWfcPipelinePreparation;
  Plan: TWfcPipelineInputPlan; B: TWfcPipelineInputBinding;
  R,S: TWfcPipelineResult; Options: TGraphSolveOptions; Report: TGraphSolveReport;
  Runtime: TWfcPipelineRuntime;
begin
  M:=Recipe; Run:=Invocation(M); P:=nil; Plan:=nil; B:=nil; R:=nil; S:=nil; Runtime:=nil;
  try
    P:=TWfcPipelinePreparation.Create(M,Run); Plan:=P.PrepareInputs(Run);
    B:=TWfcPipelineInputBinding.Create(P,Plan);
    Options:=Default(TGraphSolveOptions); Options.MaxBacktracks:=Run.MaxBacktracks; Options.CaptureTrace:=Run.CaptureTrace;
    B.BorrowCompiled.Graph.TrySolve(Options,Report);
    R:=CreateWfcPipelineResultFromSolveReport(M,Run,B.BorrowCompiled.Graph,Report);
    Runtime:=TWfcPipelineRuntime.Create(M,Run);
    Check((Runtime.Recipe=M) and (Runtime.Run=Run),'legacy recipe/run properties remain exact borrowed objects');
    S:=Runtime.Execute;
    Check(EncodeWfcPipelineResultText(R)=EncodeWfcPipelineResultText(S),'binding and fresh runtime complete result bytes equal');
    FreeAndNil(S); S:=Runtime.Execute;
    Check(EncodeWfcPipelineResultText(R)=EncodeWfcPipelineResultText(S),'repeated existing Execute retains full fresh invocation bytes');
  finally Runtime.Free; S.Free; R.Free; B.Free; Plan.Free; P.Free; Run.Free; M.Free; end;
end;

procedure TestEmptyConstruction;
var M: TWfcPipelineModel; Run: TWfcPipelineRun; P,EmptyP: TWfcPipelinePreparation;
  Plan,EmptyPlan: TWfcPipelineInputPlan; B,EmptyB: TWfcPipelineInputBinding;
  I: Integer; Saw: Boolean;
begin
  M:=Recipe; Run:=Invocation(M); P:=nil; Plan:=nil; B:=nil;
  EmptyPlan:=nil; EmptyP:=nil; EmptyB:=nil;
  try
    P:=TWfcPipelinePreparation.Create(M,Run); Plan:=P.PrepareInputs(Run);
    { InputPlan inherits parameterless TObject.Create because its owning
      constructor has a distinct private name. It must fail safely on use. }
    EmptyPlan:=TWfcPipelineInputPlan.Create;
    for I:=0 to 2 do
    begin
      Saw:=False;
      try case I of 0:EmptyPlan.CopyLocks; 1:EmptyPlan.CopyDomains; 2:EmptyPlan.CopyPassLayouts; end;
      except on E: EWfcPipelineRuntime do Saw:=E.Message<>''; end;
      Check(Saw,'inherited empty plan access is a typed input error');
    end;
    Saw:=False;
    try B:=TWfcPipelineInputBinding.Create(P,EmptyPlan);
    except on E: EWfcPipelineRuntime do Saw:=E.Message<>''; end;
    Check(Saw and (B=nil),'empty plan cannot allocate a binding');
    {$IFNDEF PAS2JS}
    { Their parameterized Create hides a direct no-argument call, but an
      ordinary ancestor-metaclass factory can still make empty native owners. }
    EmptyP:=TWfcPipelinePreparation(TPreparationAncestorClass(TWfcPipelinePreparation).Create);
    Saw:=False;
    try EmptyP.PrepareInputs(Run);
    except on E: EWfcPipelineRuntime do Saw:=E.Message<>''; end;
    Check(Saw,'empty native preparation receiver rejects safely');
    Saw:=False;
    try B:=TWfcPipelineInputBinding.Create(EmptyP,Plan);
    except on E: EWfcPipelineRuntime do Saw:=E.Message<>''; end;
    Check(Saw and (B=nil),'empty native preparation cannot allocate binding');
    EmptyB:=TWfcPipelineInputBinding(TPreparationAncestorClass(TWfcPipelineInputBinding).Create);
    Saw:=False;
    try EmptyB.BorrowCompiled;
    except on E: EWfcPipelineRuntime do Saw:=E.Message<>''; end;
    Check(Saw,'empty native binding cannot expose a graph');
    {$ENDIF}
    Saw:=False;
    try EmptyP:=TWfcPipelinePreparation.Create(nil,nil);
    except on E: EWfcPipelineRuntime do Saw:=E.Message<>''; end;
    Check(Saw,'preparation nil constructor arguments reject typed');
    Saw:=False;
    try B:=TWfcPipelineInputBinding.Create(nil,nil);
    except on E: EWfcPipelineRuntime do Saw:=E.Message<>''; end;
    Check(Saw and (B=nil),'binding nil constructor arguments reject typed');
  finally EmptyB.Free; EmptyP.Free; EmptyPlan.Free; B.Free; Plan.Free; P.Free; Run.Free; M.Free; end;
end;

{$IFDEF PAS2JS}
procedure TestUntrustedHandles;
var M: TWfcPipelineModel; Run: TWfcPipelineRun; P,BadP: TWfcPipelinePreparation;
  Plan,BadPlan: TWfcPipelineInputPlan; B,BadB: TWfcPipelineInputBinding;
  I,J,Reads: Integer; Saw: Boolean;
begin
  M:=Recipe; Run:=Invocation(M); P:=nil; Plan:=nil; B:=nil;
  try
    P:=TWfcPipelinePreparation.Create(M,Run); Plan:=P.PrepareInputs(Run);
    for I:=0 to 9 do
      for J:=0 to 1 do
      begin
        Reads:=0; Saw:=False; B:=nil;
        asm
          let fake=[null,undefined,1,'owner',[],{},
            {$class:pas.wfc_pipeline_prepare.TWfcPipelinePreparation},
            {$class:pas.wfc_pipeline_prepare.TWfcPipelineInputPlan},
            Object.create(pas.wfc_pipeline_prepare.TWfcPipelinePreparation),
            Object.create(pas.wfc_pipeline_prepare.TWfcPipelineInputPlan)][I];
          if(fake!==null && typeof fake==='object')
            Object.defineProperty(fake,'FData',{get:function(){Reads++;throw new Error('foreign handle getter');}});
          BadP=J===0?fake:P; BadPlan=J===1?fake:Plan;
        end;
        try B:=TWfcPipelineInputBinding.Create(BadP,BadPlan);
        except on E: EWfcPipelineRuntime do Saw:=Pos('not a live preparation owner',E.Message)>0; end;
        Check(Saw,'typed foreign handle rejected'); Check(B=nil,'foreign handle produces no binding');
        Check(Reads=0,'foreign handle fields never read');
      end;
    Reads:=0; Saw:=False;
    asm BadP=Object.create(pas.wfc_pipeline_prepare.TWfcPipelinePreparation);
      Object.defineProperty(BadP,'FData',{get:function(){Reads++;throw new Error('receiver getter');}}); end;
    try BadP.PrepareInputs(Run);
    except on E: EWfcPipelineRuntime do Saw:=Pos('not a live preparation owner',E.Message)>0; end;
    Check(Saw and (Reads=0),'foreign preparation receiver rejected without field access');
    asm BadPlan=Object.create(pas.wfc_pipeline_prepare.TWfcPipelineInputPlan);
      Object.defineProperty(BadPlan,'FData',{get:function(){Reads++;throw new Error('receiver getter');}}); end;
    for I:=0 to 2 do
    begin
      Saw:=False;
      try case I of 0:BadPlan.CopyLocks; 1:BadPlan.CopyDomains; 2:BadPlan.CopyPassLayouts; end;
      except on E: EWfcPipelineRuntime do Saw:=Pos('not a live preparation owner',E.Message)>0; end;
      Check(Saw and (Reads=0),'foreign plan receiver rejected without field access');
    end;
    Saw:=False;
    asm BadB=Object.create(pas.wfc_pipeline_prepare.TWfcPipelineInputBinding);
      Object.defineProperty(BadB,'FCompiled',{get:function(){Reads++;throw new Error('receiver getter');}}); end;
    try BadB.BorrowCompiled;
    except on E: EWfcPipelineRuntime do Saw:=Pos('not a live preparation owner',E.Message)>0; end;
    Check(Saw and (Reads=0),'foreign binding receiver rejected without graph access');
  finally B.Free; Plan.Free; P.Free; Run.Free; M.Free; end;
end;
{$ENDIF}

begin
  TestDetachedLifetime; TestFreshRuntimeParity; TestEmptyConstruction;
  {$IFDEF PAS2JS}TestUntrustedHandles;{$ENDIF}
  WriteLn('Private pipeline preparation: ',Checks,' checks passed.');
end.
