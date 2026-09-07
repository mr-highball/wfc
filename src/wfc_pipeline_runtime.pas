(*
MIT License

Copyright (c) 2021 mr-highball

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
*)
unit wfc_pipeline_runtime;

{$mode delphi}{$H+}

interface

uses
  SysUtils,
  wfc,
  wfc_model,
  wfc_pipeline_model,
  wfc_pipeline_run,
  wfc_pipeline_compile,
  wfc_pipeline_result,
  wfc_pipeline_prepare;

const
  WFC_PIPELINE_RUNTIME_VERSION = wfc_pipeline_prepare.WFC_PIPELINE_RUNTIME_VERSION;
  WFC_PIPELINE_RUNTIME_INVERSE_LIMITS_VERSION = wfc_pipeline_prepare.WFC_PIPELINE_RUNTIME_INVERSE_LIMITS_VERSION;
  WFC_PIPELINE_RUNTIME_MAX_TOTAL_PASS_CELL_COUNT = wfc_pipeline_prepare.WFC_PIPELINE_RUNTIME_MAX_TOTAL_PASS_CELL_COUNT;
  WFC_PIPELINE_RUNTIME_MAX_INVERSE_CONTRIBUTION_COUNT = wfc_pipeline_prepare.WFC_PIPELINE_RUNTIME_MAX_INVERSE_CONTRIBUTION_COUNT;
  WFC_PIPELINE_RUNTIME_MAX_INVERSE_CANDIDATE_VISIT_COUNT = wfc_pipeline_prepare.WFC_PIPELINE_RUNTIME_MAX_INVERSE_CANDIDATE_VISIT_COUNT;
  WFC_PIPELINE_RUNTIME_MAX_INVERSE_PRIVATE_INDEX_COUNT = wfc_pipeline_prepare.WFC_PIPELINE_RUNTIME_MAX_INVERSE_PRIVATE_INDEX_COUNT;

type
  EWfcPipelineRuntime = wfc_pipeline_prepare.EWfcPipelineRuntime;

  { Owns one prepared executable graph. Recipe and Run are immutable borrowed
    inputs and must outlive this object. Execute returns a detached result
    owned by the caller. Repeated execution rewinds the core's versioned random
    streams and therefore replays the same invocation. }
  TWfcPipelineRuntime = class
  strict private
    FRecipe: TWfcPipelineModel;
    FRun: TWfcPipelineRun;
    FCompiled: TWfcCompiledPipeline;
    FBinding: TWfcPipelineInputBinding;
    procedure Initialize(const ARecipe: TWfcPipelineModel;
      const ARun: TWfcPipelineRun);
  public
    constructor Create(const ARecipe: TWfcPipelineModel;
      const ARun: TWfcPipelineRun);
    destructor Destroy; override;

    function Execute: TWfcPipelineResult;

    property Recipe: TWfcPipelineModel read FRecipe;
    property Run: TWfcPipelineRun read FRun;
  end;

function ExecuteWfcPipeline(const ARecipe: TWfcPipelineModel;
  const ARun: TWfcPipelineRun): TWfcPipelineResult;

implementation


constructor TWfcPipelineRuntime.Create(const ARecipe: TWfcPipelineModel;
  const ARun: TWfcPipelineRun);
begin
  inherited Create;
  Initialize(ARecipe, ARun);
end;

destructor TWfcPipelineRuntime.Destroy;
begin
  FBinding.Free;
  FBinding := nil;
  FCompiled := nil;
  FRun := nil;
  FRecipe := nil;
  inherited Destroy;
end;

procedure TWfcPipelineRuntime.Initialize(const ARecipe: TWfcPipelineModel;
  const ARun: TWfcPipelineRun);
var
  LPreparation: TWfcPipelinePreparation;
  LPlan: TWfcPipelineInputPlan;
  LBinding: TWfcPipelineInputBinding;
  LCompiled: TWfcCompiledPipeline;
begin
  LPreparation:=nil; LPlan:=nil; LBinding:=nil;
  try
    LPreparation:=TWfcPipelinePreparation.Create(ARecipe,ARun);
    LPlan:=LPreparation.PrepareInputs(ARun);
    LBinding:=TWfcPipelineInputBinding.Create(LPreparation,LPlan);
    LCompiled:=LBinding.BorrowCompiled;
    FRecipe:=ARecipe; FRun:=ARun;
    FCompiled:=LCompiled; FBinding:=LBinding; LBinding:=nil;
  finally
    LBinding.Free; LPlan.Free; LPreparation.Free;
  end;
end;

function TWfcPipelineRuntime.Execute: TWfcPipelineResult;
var
  LNegotiationOptions: TGraphNegotiationOptions;
  LNegotiationReport: TGraphNegotiationReport;
  LSolveOptions: TGraphSolveOptions;
  LSolveReport: TGraphSolveReport;
begin
  Result := nil;
  case Ord(FRun.Strategy) of
    Ord(wpssOneWay):
      begin
        LSolveOptions := Default(TGraphSolveOptions);
        LSolveOptions.MaxBacktracks := FRun.MaxBacktracks;
        LSolveOptions.CaptureTrace := FRun.CaptureTrace;
        FCompiled.Graph.TrySolve(LSolveOptions, LSolveReport);
        Result := CreateWfcPipelineResultFromSolveReport(
          FRecipe, FRun, FCompiled.Graph, LSolveReport);
      end;
    Ord(wpssNegotiated):
      begin
        LNegotiationOptions := Default(TGraphNegotiationOptions);
        LNegotiationOptions.SolveOptions.MaxBacktracks :=
          FRun.MaxBacktracks;
        LNegotiationOptions.SolveOptions.CaptureTrace :=
          FRun.CaptureTrace;
        LNegotiationOptions.MaxPassBacktracks :=
          FRun.MaxPassBacktracks;
        FCompiled.Graph.TrySolveNegotiated(
          LNegotiationOptions, LNegotiationReport);
        Result := CreateWfcPipelineResultFromNegotiationReport(
          FRecipe, FRun, FCompiled.Graph, LNegotiationReport);
      end;
  else
    raise EWfcPipelineRuntime.Create('runtime solve strategy is unknown');
  end;
end;

function ExecuteWfcPipeline(const ARecipe: TWfcPipelineModel;
  const ARun: TWfcPipelineRun): TWfcPipelineResult;
var
  LRuntime: TWfcPipelineRuntime;
begin
  Result := nil;
  LRuntime := TWfcPipelineRuntime.Create(ARecipe, ARun);
  try
    Result := LRuntime.Execute;
  finally
    LRuntime.Free;
  end;
end;

end.
