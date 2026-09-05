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
unit wfc_training_workspace;

{$mode delphi}{$H+}

interface

uses
  SysUtils, wfc, wfc_model, wfc_training, wfc_pipeline_model,
  wfc_pipeline_run, wfc_pipeline_result;

const
  WFC_TRAINING_WORKSPACE_VERSION = 1;

type
  EWfcTrainingWorkspace = class(Exception);

  TWfcTrainingWorkspaceLimits = record
    MaxSourceTextLength: Integer;
    MaxSourceTokens: Integer;
    MaxSourceSamples: Integer;
    MaxModelItems: Integer;
    MaxOutputCells: Integer;
    MaxBacktracks: Integer;
    MaxPassBacktracks: Integer;
  end;

  TWfcTrainingSolveOptions = record
    Width: Integer;
    Height: Integer;
    Seed: TGraphSeed;
    Strategy: TWfcPipelineSolveStrategy;
    MaxBacktracks: Integer;
    MaxPassBacktracks: Integer;
    CaptureTrace: Boolean;
  end;

  { One editable source and its derived artifacts. Editing source invalidates
    every derived object. Editing run inputs invalidates the previous result.
    Methods raise on invalid input; a failed mutation never exposes artifacts
    from the preceding draft. No graph or owned object escapes the workspace. }
  TWfcTrainingWorkspace = class
  strict private
    FLimits: TWfcTrainingWorkspaceLimits;
    FSourceText: String;
    FDocument: TWfcTrainingDocument;
    FRecipe: TWfcPipelineModel;
    FRun: TWfcPipelineRun;
    FResult: TWfcPipelineResult;
    FModelItemCount: Integer;
    procedure Initialize(const ALimits: TWfcTrainingWorkspaceLimits);
    procedure ClearTraining;
    procedure RequireRecipe;
    procedure RequireRun;
    procedure RequireResult;
    function GetHasRecipe: Boolean;
    function GetHasRun: Boolean;
    function GetHasResult: Boolean;
    function GetPublicPassIndex: Integer;
    function GetRank: Integer;
    function GetWrapNeighbors: Boolean;
    function GetSampleCount: Integer;
    function GetSourceTokenCount: Integer;
    function GetModelItemCount: Integer;
  public
    constructor Create; overload;
    constructor Create(const ALimits: TWfcTrainingWorkspaceLimits); overload;
    destructor Destroy; override;

    procedure SetSourceText(const AText: String);
    procedure Train;
    procedure ClearRun;
    procedure ConfigureRun(const AOptions: TWfcTrainingSolveOptions;
      const ALocks: TWfcPipelineCellLocks;
      const ADomains: TWfcPipelineCellDomains);
    procedure Solve;

    function CopyLimits: TWfcTrainingWorkspaceLimits;
    function SourceOptions: TWfcTrainingOptions;
    function CopyMetadata: TWfcTrainingMetadata;
    function CopyPasses: TWfcPipelinePasses;
    function PublicVocabulary: TWfcModelTokens;
    function OutputTokens: TWfcModelTokens;
    function CopyFailure: TWfcPipelineFailure;
    function CopyPassOutcomes: TWfcPipelinePassOutcomes;
    function ResultStatus: TWfcPipelineResultStatus;
    function TrainingSignatureText: String;
    function RecipeSignatureText: String;
    function ResultSignatureText: String;
    function RecipeText: String;
    function ModelText: String;
    function RunText: String;
    function ResultText: String;

    property SourceText: String read FSourceText;
    property HasRecipe: Boolean read GetHasRecipe;
    property HasRun: Boolean read GetHasRun;
    property HasResult: Boolean read GetHasResult;
    property PublicPassIndex: Integer read GetPublicPassIndex;
    property Rank: Integer read GetRank;
    property WrapNeighbors: Boolean read GetWrapNeighbors;
    property SampleCount: Integer read GetSampleCount;
    property SourceTokenCount: Integer read GetSourceTokenCount;
    property ModelItemCount: Integer read GetModelItemCount;
  end;

function DefaultWfcTrainingWorkspaceLimits: TWfcTrainingWorkspaceLimits;
function InteractiveWfcTrainingWorkspaceLimits: TWfcTrainingWorkspaceLimits;
function DefaultWfcTrainingSolveOptions: TWfcTrainingSolveOptions;

implementation

uses
  wfc_training_text, wfc_pipeline_text, wfc_pipeline_run_text,
  wfc_pipeline_result_text, wfc_pipeline_runtime;

function DefaultWfcTrainingWorkspaceLimits: TWfcTrainingWorkspaceLimits;
begin
  Result.MaxSourceTextLength := WFC_TRAINING_MAX_ENCODED_TEXT_LENGTH;
  Result.MaxSourceTokens := WFC_TRAINING_MAX_TOTAL_TOKEN_COUNT;
  Result.MaxSourceSamples := WFC_TRAINING_MAX_SAMPLE_COUNT;
  Result.MaxModelItems := 1024;
  Result.MaxOutputCells := WFC_PIPELINE_RUN_MAX_CELL_COUNT;
  Result.MaxBacktracks := WFC_PIPELINE_RUN_MAX_BACKTRACKS;
  Result.MaxPassBacktracks := WFC_PIPELINE_RUN_MAX_PASS_BACKTRACKS;
end;

function InteractiveWfcTrainingWorkspaceLimits: TWfcTrainingWorkspaceLimits;
begin
  Result.MaxSourceTextLength := 262144;
  Result.MaxSourceTokens := 512;
  Result.MaxSourceSamples := 64;
  Result.MaxModelItems := 128;
  Result.MaxOutputCells := 512;
  Result.MaxBacktracks := 4096;
  Result.MaxPassBacktracks := 64;
end;

function DefaultWfcTrainingSolveOptions: TWfcTrainingSolveOptions;
begin
  Result.Width := 4;
  Result.Height := 4;
  Result.Seed := 0;
  Result.Strategy := wpssOneWay;
  Result.MaxBacktracks := 1024;
  Result.MaxPassBacktracks := 0;
  Result.CaptureTrace := False;
end;

procedure CheckLimit(const AValue, AMaximum: Integer;
  const AAllowZero: Boolean; const AName: String);
begin
  if (AValue < 0) or ((not AAllowZero) and (AValue = 0)) or
      (AValue > AMaximum) then
    raise EWfcTrainingWorkspace.Create(AName + ' is outside workspace limits');
end;

procedure TWfcTrainingWorkspace.Initialize(
  const ALimits: TWfcTrainingWorkspaceLimits);
var
  LMaximum: TWfcTrainingWorkspaceLimits;
begin
  LMaximum := DefaultWfcTrainingWorkspaceLimits;
  CheckLimit(ALimits.MaxSourceTextLength, LMaximum.MaxSourceTextLength,
    False, 'source text limit');
  CheckLimit(ALimits.MaxSourceTokens, LMaximum.MaxSourceTokens,
    False, 'source token limit');
  CheckLimit(ALimits.MaxSourceSamples, LMaximum.MaxSourceSamples,
    False, 'source sample limit');
  CheckLimit(ALimits.MaxModelItems, LMaximum.MaxModelItems,
    False, 'model item limit');
  CheckLimit(ALimits.MaxOutputCells, LMaximum.MaxOutputCells,
    False, 'output cell limit');
  CheckLimit(ALimits.MaxBacktracks, LMaximum.MaxBacktracks,
    True, 'local search limit');
  CheckLimit(ALimits.MaxPassBacktracks, LMaximum.MaxPassBacktracks,
    True, 'pass search limit');
  FLimits := ALimits;
end;

constructor TWfcTrainingWorkspace.Create;
begin
  inherited Create;
  Initialize(DefaultWfcTrainingWorkspaceLimits);
end;

constructor TWfcTrainingWorkspace.Create(
  const ALimits: TWfcTrainingWorkspaceLimits);
begin
  inherited Create;
  Initialize(ALimits);
end;

destructor TWfcTrainingWorkspace.Destroy;
begin
  ClearTraining;
  inherited Destroy;
end;

procedure TWfcTrainingWorkspace.ClearRun;
begin
  FreeAndNil(FResult);
  FreeAndNil(FRun);
end;

procedure TWfcTrainingWorkspace.ClearTraining;
begin
  ClearRun;
  FreeAndNil(FRecipe);
  FreeAndNil(FDocument);
  FModelItemCount := 0;
end;

procedure TWfcTrainingWorkspace.SetSourceText(const AText: String);
begin
  ClearTraining;
  FSourceText := '';
  CheckLimit(Length(AText), FLimits.MaxSourceTextLength, True, 'source text');
  FSourceText := AText;
end;

procedure TWfcTrainingWorkspace.Train;
var
  LDocument: TWfcTrainingDocument;
  LRecipe: TWfcPipelineModel;
  LItems: Integer;
  LResource: TWfcPipelineResource;
begin
  ClearTraining;
  LDocument := nil;
  LRecipe := nil;
  try
    LDocument := DecodeWfcTrainingText(FSourceText);
    CheckLimit(LDocument.SampleCount, FLimits.MaxSourceSamples,
      False, 'source sample count');
    CheckLimit(LDocument.TotalTokenCount, FLimits.MaxSourceTokens,
      False, 'source token count');
    LRecipe := LearnWfcTrainingRecipe(LDocument);
    LResource := LRecipe.ResourceAt(0);
    case LResource.Kind of
      wprkModel: LItems := LRecipe.BorrowModelResource(0).ValueCount;
      wprkPattern2D: LItems := LRecipe.BorrowPattern2DResource(0).PatternCount;
      wprkSequence: LItems := LRecipe.BorrowSequenceResource(0).StateCount;
    else
      raise EWfcTrainingWorkspace.Create('unsupported trained resource kind');
    end;
    CheckLimit(LItems, FLimits.MaxModelItems, False, 'learned model item count');
    FDocument := LDocument;
    LDocument := nil;
    FRecipe := LRecipe;
    LRecipe := nil;
    FModelItemCount := LItems;
  finally
    LRecipe.Free;
    LDocument.Free;
  end;
end;

procedure TWfcTrainingWorkspace.RequireRecipe;
begin
  if FRecipe = nil then
    raise EWfcTrainingWorkspace.Create('train the current source first');
end;

procedure TWfcTrainingWorkspace.RequireRun;
begin
  RequireRecipe;
  if FRun = nil then
    raise EWfcTrainingWorkspace.Create('configure a run for the current recipe first');
end;

procedure TWfcTrainingWorkspace.RequireResult;
begin
  RequireRun;
  if FResult = nil then
    raise EWfcTrainingWorkspace.Create('solve the current run first');
end;

procedure TWfcTrainingWorkspace.ConfigureRun(
  const AOptions: TWfcTrainingSolveOptions;
  const ALocks: TWfcPipelineCellLocks;
  const ADomains: TWfcPipelineCellDomains);
begin
  ClearRun;
  RequireRecipe;
  CheckLimit(AOptions.Width, FLimits.MaxOutputCells, False, 'output width');
  CheckLimit(AOptions.Height, FLimits.MaxOutputCells, False, 'output height');
  if AOptions.Width > FLimits.MaxOutputCells div AOptions.Height then
    raise EWfcTrainingWorkspace.Create('output cell count exceeds workspace limit');
  CheckLimit(AOptions.MaxBacktracks, FLimits.MaxBacktracks,
    True, 'local backtrack budget');
  CheckLimit(AOptions.MaxPassBacktracks, FLimits.MaxPassBacktracks,
    True, 'pass backtrack budget');
  FRun := TWfcPipelineRun.Create(FRecipe, AOptions.Width, AOptions.Height,
    1, AOptions.Seed, AOptions.Strategy, AOptions.MaxBacktracks,
    AOptions.MaxPassBacktracks, AOptions.CaptureTrace, ALocks, ADomains);
end;

procedure TWfcTrainingWorkspace.Solve;
begin
  FreeAndNil(FResult);
  RequireRun;
  FResult := ExecuteWfcPipeline(FRecipe, FRun);
end;

function TWfcTrainingWorkspace.GetHasRecipe: Boolean;
begin
  Result := FRecipe <> nil;
end;

function TWfcTrainingWorkspace.GetHasRun: Boolean;
begin
  Result := FRun <> nil;
end;

function TWfcTrainingWorkspace.GetHasResult: Boolean;
begin
  Result := FResult <> nil;
end;

function TWfcTrainingWorkspace.GetPublicPassIndex: Integer;
begin
  RequireRecipe;
  Result := FRecipe.FindPass('output');
end;

function TWfcTrainingWorkspace.GetRank: Integer;
begin
  RequireRecipe;
  Result := FRecipe.Rank;
end;

function TWfcTrainingWorkspace.GetWrapNeighbors: Boolean;
begin
  RequireRecipe;
  Result := FRecipe.WrapNeighbors;
end;

function TWfcTrainingWorkspace.GetSampleCount: Integer;
begin
  RequireRecipe;
  Result := FDocument.SampleCount;
end;

function TWfcTrainingWorkspace.GetSourceTokenCount: Integer;
begin
  RequireRecipe;
  Result := FDocument.TotalTokenCount;
end;

function TWfcTrainingWorkspace.GetModelItemCount: Integer;
begin
  RequireRecipe;
  Result := FModelItemCount;
end;

function TWfcTrainingWorkspace.CopyLimits: TWfcTrainingWorkspaceLimits;
begin
  Result := FLimits;
end;

function TWfcTrainingWorkspace.SourceOptions: TWfcTrainingOptions;
begin
  RequireRecipe;
  Result := FDocument.CopyOptions;
end;

function TWfcTrainingWorkspace.CopyMetadata: TWfcTrainingMetadata;
begin
  RequireRecipe;
  Result := FDocument.CopyMetadata;
end;

function TWfcTrainingWorkspace.CopyPasses: TWfcPipelinePasses;
begin
  RequireRecipe;
  Result := FRecipe.CopyPasses;
end;

function TWfcTrainingWorkspace.PublicVocabulary: TWfcModelTokens;
begin
  RequireRecipe;
  Result := FRecipe.CopyPublicVocabulary(PublicPassIndex);
end;

function TWfcTrainingWorkspace.OutputTokens: TWfcModelTokens;
var
  LLayer: TWfcPipelineResultLayer;
begin
  RequireResult;
  Result := nil;
  if FResult.Status <> wprsSolved then
    Exit;
  LLayer := FResult.LayerAt(0);
  Result := LLayer.Tokens;
end;

function TWfcTrainingWorkspace.CopyFailure: TWfcPipelineFailure;
begin
  RequireResult;
  Result := FResult.CopyFailure;
end;

function TWfcTrainingWorkspace.CopyPassOutcomes: TWfcPipelinePassOutcomes;
begin
  RequireResult;
  Result := FResult.CopyPassOutcomes;
end;

function TWfcTrainingWorkspace.ResultStatus: TWfcPipelineResultStatus;
begin
  RequireResult;
  Result := FResult.Status;
end;

function TWfcTrainingWorkspace.TrainingSignatureText: String;
begin
  RequireRecipe;
  Result := WfcTrainingSignatureHex(FDocument.Signature);
end;

function TWfcTrainingWorkspace.RecipeSignatureText: String;
begin
  RequireRecipe;
  Result := WfcPipelineSignatureHex(FRecipe.Signature);
end;

function TWfcTrainingWorkspace.ResultSignatureText: String;
begin
  RequireResult;
  Result := WfcPipelineResultSignatureHex(FResult.Signature);
end;

function TWfcTrainingWorkspace.RecipeText: String;
begin
  RequireRecipe;
  Result := EncodeWfcPipelineModelText(FRecipe);
end;

function TWfcTrainingWorkspace.ModelText: String;
var
  LResource: TWfcPipelineResource;
begin
  RequireRecipe;
  LResource := FRecipe.ResourceAt(0);
  Result := LResource.Document;
end;

function TWfcTrainingWorkspace.RunText: String;
begin
  RequireRun;
  Result := EncodeWfcPipelineRunText(FRun);
end;

function TWfcTrainingWorkspace.ResultText: String;
begin
  RequireResult;
  Result := EncodeWfcPipelineResultText(FResult);
end;

end.
