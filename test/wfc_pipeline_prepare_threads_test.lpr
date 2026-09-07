{ SPDX-License-Identifier: MIT
  Copyright (c) 2021 mr-highball
  Native-only regression: independent runtimes and shared immutable plans. }
program wfc_pipeline_prepare_threads_test;
{$mode delphi}{$H+}
{$IFDEF PAS2JS}{$FATAL Native thread regression only}{$ENDIF}
uses {$IFDEF UNIX}cthreads,{$ENDIF} Classes,SysUtils,SyncObjs,wfc,wfc_model,
  wfc_sequence,wfc_rule_model,wfc_rule_text,wfc_pipeline_model,wfc_pipeline_run,
  wfc_pipeline_result,wfc_pipeline_result_text,wfc_pipeline_prepare,wfc_pipeline_runtime;

function Tokens(const A: array of TWfcModelToken): TWfcModelTokens;
var I: Integer;
begin Result:=nil; SetLength(Result,Length(A)); for I:=0 to High(A) do Result[I]:=A[I]; end;
function Recipe: TWfcPipelineModel;
var Rules: TWfcRuleModel; Weights: TWfcModelIntegerArray;
  R: TWfcPipelineResources; P: TWfcPipelinePasses; D: TWfcPipelineDependencies;
begin
  SetLength(Weights,2); Weights[0]:=1; Weights[1]:=2;
  Rules:=TWfcRuleModel.Create(1,Tokens(['A','B']),Weights,nil);
  try SetLength(R,1); R[0]:=MakeWfcPipelineResource('rules',wprkRules,
    EncodeWfcRuleText(Rules),'literal source','MIT','thread-fixture'); finally Rules.Free; end;
  SetLength(P,2); SetLength(D,1);
  P[0]:=MakeWfcPipelinePass('public',wppvPublic,gpmOverlay,-1,wpakRules,0,False,wseWhole);
  P[1]:=MakeWfcPipelinePass('alias',wppvPublic,gpmTransform,0,wpakEmpty,-1,False,wseWhole);
  D[0]:=MakeWfcPipelineDependency(1,0);
  Result:=TWfcPipelineModel.Create(MakeWfcPipelineMetadata('thread fixture','MIT','parallel owners',''),
    1,False,rmBottomUp,R,P,D,nil,nil);
end;
function Invocation(const M: TWfcPipelineModel): TWfcPipelineRun;
var Locks: TWfcPipelineCellLocks; Domains: TWfcPipelineCellDomains;
begin
  SetLength(Locks,1); Locks[0]:=MakeWfcPipelineCellLock(1,1,0,0,'A');
  SetLength(Domains,1); Domains[0]:=MakeWfcPipelineCellDomain(0,0,0,0,Tokens(['A','B']));
  Result:=TWfcPipelineRun.Create(M,3,1,1,77,wpssOneWay,32,0,True,Locks,Domains);
end;

type
  TWorker = class(TThread)
  public
    StartGate: TEvent;
    SharedRecipe: TWfcPipelineModel;
    SharedRun: TWfcPipelineRun;
    SharedPreparation: TWfcPipelinePreparation;
    SharedPlan: TWfcPipelineInputPlan;
    Expected: String;
    Failure: String;
    Checks: Integer;
    procedure Assert(const OK: Boolean; const MessageText: String);
    procedure Execute; override;
  end;

procedure TWorker.Assert(const OK: Boolean; const MessageText: String);
begin Inc(Checks); if not OK then raise Exception.Create(MessageText); end;
procedure TWorker.Execute;
var I: Integer; M: TWfcPipelineModel; Run: TWfcPipelineRun;
  R: TWfcPipelineResult; B: TWfcPipelineInputBinding;
  Options: TGraphSolveOptions; Report: TGraphSolveReport;
begin
  try
    Assert(StartGate.WaitFor(10000)=wrSignaled,'worker start deadline');
    for I:=0 to 15 do
    begin
      M:=nil; Run:=nil; R:=nil;
      try
        M:=Recipe; Run:=Invocation(M); R:=ExecuteWfcPipeline(M,Run);
        Assert(EncodeWfcPipelineResultText(R)=Expected,'independent runtime full result bytes');
      finally R.Free; Run.Free; M.Free; end;
    end;
    for I:=0 to 63 do
    begin
      B:=nil; R:=nil;
      try
        B:=TWfcPipelineInputBinding.Create(SharedPreparation,SharedPlan);
        Options:=Default(TGraphSolveOptions); Options.MaxBacktracks:=SharedRun.MaxBacktracks;
        Options.CaptureTrace:=SharedRun.CaptureTrace;
        Assert(B.BorrowCompiled.Graph.TrySolve(Options,Report),'shared immutable-plan binding solves');
        R:=CreateWfcPipelineResultFromSolveReport(SharedRecipe,SharedRun,B.BorrowCompiled.Graph,Report);
        Assert(EncodeWfcPipelineResultText(R)=Expected,'shared-plan independent graph full result bytes');
      finally R.Free; B.Free; end;
    end;
  except on E: Exception do Failure:=E.ClassName+': '+E.Message; end;
end;

var M: TWfcPipelineModel; Run: TWfcPipelineRun; P: TWfcPipelinePreparation;
  Plan: TWfcPipelineInputPlan; ExpectedResult: TWfcPipelineResult;
  Workers: array[0..3] of TWorker; Gate: TEvent; Expected: String;
  I,Checks: Integer; Failure: String;
begin
  M:=nil; Run:=nil; P:=nil; Plan:=nil; ExpectedResult:=nil; Gate:=nil;
  for I:=0 to High(Workers) do Workers[I]:=nil;
  try
    M:=Recipe; Run:=Invocation(M); P:=TWfcPipelinePreparation.Create(M,Run);
    Plan:=P.PrepareInputs(Run); ExpectedResult:=ExecuteWfcPipeline(M,Run);
    Expected:=EncodeWfcPipelineResultText(ExpectedResult); FreeAndNil(ExpectedResult);
    Gate:=TEvent.Create(nil,True,False,'');
    for I:=0 to High(Workers) do
    begin
      Workers[I]:=TWorker.Create(True); Workers[I].FreeOnTerminate:=False;
      Workers[I].StartGate:=Gate; Workers[I].SharedRecipe:=M; Workers[I].SharedRun:=Run;
      Workers[I].SharedPreparation:=P; Workers[I].SharedPlan:=Plan;
      Workers[I].Expected:=Expected; Workers[I].Start;
    end;
    Gate.SetEvent;
    Checks:=0; Failure:='';
    for I:=0 to High(Workers) do
    begin
      Workers[I].WaitFor; Inc(Checks,Workers[I].Checks);
      if Workers[I].Failure<>'' then Failure:=Failure+'worker'+IntToStr(I)+': '+Workers[I].Failure+'; ';
    end;
    if Failure<>'' then raise Exception.Create(Failure);
    Inc(Checks);
    if Plan.CopyLocks[0].Token<>'A' then raise Exception.Create('shared owner changed after parallel release');
    WriteLn('Native preparation threads: ',Checks,' checks passed; four threads,64 independent runtimes,256 shared-plan bindings.');
  finally
    if Gate<>nil then Gate.SetEvent;
    for I:=0 to High(Workers) do Workers[I].Free;
    Gate.Free; ExpectedResult.Free; Plan.Free; P.Free; Run.Free; M.Free;
  end;
end.
