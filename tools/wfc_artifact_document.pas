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
unit wfc_artifact_document;

{$mode delphi}{$H+}

interface

uses
  SysUtils, wfc_rule_model, wfc_model, wfc_pattern2d, wfc_sequence,
  wfc_training, wfc_pipeline_model, wfc_pipeline_run, wfc_pipeline_result;

type
  TWfcArtifactKind = (wakRules, wakModel, wakPattern2D, wakSequence,
    wakTraining, wakRecipe, wakRun, wakResult);

  EWfcArtifactDocument = class(Exception);
  EWfcArtifactInvalid = class(EWfcArtifactDocument);
  EWfcArtifactReplayInvocation = class(EWfcArtifactDocument);
  EWfcArtifactReplayMismatch = class(EWfcArtifactDocument);

  { A closed, strict-codec artifact family. Dependencies are decoded from the
    supplied text only: no paths or URLs in an artifact are followed.
    All object properties are borrowed, read-only views owned by this object;
    callers must not free them. CanonicalText retains the accepted input bytes.
    Construction validates the existing schema and declared binding, NOT a
    universal solution proof. Only explicit RequireReplay executes a solver. }
  TWfcArtifactDocument = class
  strict private
    FKind: TWfcArtifactKind;
    FCanonicalText: String;
    FRules: TWfcRuleModel;
    FModel: TWfcModel;
    FPattern2D: TWfcOverlappingModel2D;
    FSequence: TWfcSequenceModel;
    FTraining: TWfcTrainingDocument;
    FRecipe: TWfcPipelineModel;
    FRun: TWfcPipelineRun;
    FStoredResult: TWfcPipelineResult;
  public
    constructor Create(const AKind: TWfcArtifactKind;
      const AInputText, ARecipeText, ARunText: String);
    destructor Destroy; override;
    function Summary: String;
    { A fresh deterministic execution must reproduce every canonical result
      byte, including diagnostics/counters and exact non-solved outcomes.
      Search uses the recorded budgets; this is not a wall-clock deadline. }
    procedure RequireReplay;
    property Kind: TWfcArtifactKind read FKind;
    property CanonicalText: String read FCanonicalText;
    property Rules: TWfcRuleModel read FRules;
    property Model: TWfcModel read FModel;
    property Pattern2D: TWfcOverlappingModel2D read FPattern2D;
    property Sequence: TWfcSequenceModel read FSequence;
    property Training: TWfcTrainingDocument read FTraining;
    property Recipe: TWfcPipelineModel read FRecipe;
    property Run: TWfcPipelineRun read FRun;
    property StoredResult: TWfcPipelineResult read FStoredResult;
  end;

function WfcArtifactKindName(const AKind: TWfcArtifactKind): String;
function WfcArtifactInputLimit(const AKind: TWfcArtifactKind): Integer;

implementation

uses
  wfc_rule_text, wfc_model_text, wfc_pattern2d_text, wfc_sequence_text,
  wfc_training_text, wfc_pipeline_text, wfc_pipeline_run_text,
  wfc_pipeline_result_text, wfc_pipeline_runtime, wfc_pipeline_compile;

function WfcArtifactKindName(const AKind: TWfcArtifactKind): String;
begin
  case AKind of
    wakRules: Result := 'rules';
    wakModel: Result := 'model';
    wakPattern2D: Result := 'pattern2d';
    wakSequence: Result := 'sequence';
    wakTraining: Result := 'training';
    wakRecipe: Result := 'recipe';
    wakRun: Result := 'run';
    wakResult: Result := 'result';
  else
    raise EWfcArtifactInvalid.Create('artifact kind is unknown');
  end;
end;

function WfcArtifactInputLimit(const AKind: TWfcArtifactKind): Integer;
begin
  case AKind of
    wakRules: Result := WFC_RULE_MAX_ENCODED_TEXT_LENGTH;
    wakModel: Result := WFC_MODEL_MAX_ENCODED_TEXT_LENGTH;
    wakPattern2D: Result := WFC_PATTERN_2D_MAX_ENCODED_TEXT_LENGTH;
    wakSequence: Result := WFC_SEQUENCE_MAX_ENCODED_TEXT_LENGTH;
    wakTraining: Result := WFC_TRAINING_MAX_ENCODED_TEXT_LENGTH;
    wakRecipe: Result := WFC_PIPELINE_MAX_ENCODED_TEXT_LENGTH;
    wakRun: Result := WFC_PIPELINE_RUN_MAX_ENCODED_TEXT_LENGTH;
    wakResult: Result := WFC_PIPELINE_RESULT_MAX_ENCODED_TEXT_LENGTH;
  else
    raise EWfcArtifactInvalid.Create('artifact kind is unknown');
  end;
end;

procedure CheckText(const AText, ARole: String; const ALimit: Integer);
var I: Integer;
begin
  if Length(AText) > ALimit then
    raise EWfcArtifactInvalid.Create(ARole + ' input exceeds its byte limit');
  for I := 1 to Length(AText) do
    if Ord(AText[I]) > 127 then
      raise EWfcArtifactInvalid.Create(ARole + ' input must be ASCII');
end;

constructor TWfcArtifactDocument.Create(const AKind: TWfcArtifactKind;
  const AInputText, ARecipeText, ARunText: String);
var LName: String;
begin
  inherited Create;
  LName := WfcArtifactKindName(AKind);
  FKind := AKind;
  case AKind of
    wakRun:
      begin
        if ARecipeText = '' then
          raise EWfcArtifactInvalid.Create('run artifact requires recipe context');
        if ARunText <> '' then
          raise EWfcArtifactInvalid.Create('run artifact does not accept run context');
      end;
    wakResult:
      begin
        if ARecipeText = '' then
          raise EWfcArtifactInvalid.Create('result artifact requires recipe context');
        if ARunText = '' then
          raise EWfcArtifactInvalid.Create('result artifact requires run context');
      end;
  else
    if (ARecipeText <> '') or (ARunText <> '') then
      raise EWfcArtifactInvalid.Create(LName + ' artifact does not accept context');
  end;

  { Preflight every supplied role before any decoder builds owned objects.
    Existing codecs retain their own canonical and aggregate resource limits. }
  CheckText(AInputText, LName, WfcArtifactInputLimit(AKind));
  if ARecipeText <> '' then
    CheckText(ARecipeText, 'recipe context', WfcArtifactInputLimit(wakRecipe));
  if ARunText <> '' then
    CheckText(ARunText, 'run context', WfcArtifactInputLimit(wakRun));
  try
    if ARecipeText <> '' then
      FRecipe := DecodeWfcPipelineModelText(ARecipeText);
    if ARunText <> '' then
      FRun := DecodeWfcPipelineRunText(ARunText, FRecipe);
    case AKind of
      wakRules: FRules := DecodeWfcRuleText(AInputText);
      wakModel: FModel := DecodeWfcModelText(AInputText);
      wakPattern2D: FPattern2D := DecodeWfcPattern2DText(AInputText);
      wakSequence: FSequence := DecodeWfcSequenceText(AInputText);
      wakTraining: FTraining := DecodeWfcTrainingText(AInputText);
      wakRecipe: FRecipe := DecodeWfcPipelineModelText(AInputText);
      wakRun: FRun := DecodeWfcPipelineRunText(AInputText, FRecipe);
      wakResult: FStoredResult := DecodeWfcPipelineResultText(
        AInputText, FRecipe, FRun);
    end;
  except
    { The strict text decoders translate their expected typed model errors to
      EConvertError. Do not catch unexpected runtime/allocation exceptions. }
    on E: EConvertError do
      raise EWfcArtifactInvalid.Create(E.Message);
  end;
  FCanonicalText := AInputText;
end;

destructor TWfcArtifactDocument.Destroy;
begin
  FStoredResult.Free;
  FRun.Free;
  FRecipe.Free;
  FTraining.Free;
  FSequence.Free;
  FPattern2D.Free;
  FModel.Free;
  FRules.Free;
  inherited Destroy;
end;

function ResultStatusName(const AStatus: TWfcPipelineResultStatus): String;
begin
  case AStatus of
    wprsSolved: Result := 'solved';
    wprsContradiction: Result := 'contradiction';
    wprsSolverBacktrackLimit: Result := 'solver-backtrack-limit';
    wprsPassBacktrackLimit: Result := 'pass-backtrack-limit';
  else
    raise EInvalidOp.Create('decoded result status is unknown');
  end;
end;

function TWfcArtifactDocument.Summary: String;
begin
  { Keep the original recipe CLI's summary bytes, including old v1 goldens. }
  if FKind = wakRecipe then
  begin
    Result := 'valid canonical wfcpipeline=' +
      IntToStr(WfcPipelineModelTextVersion(FRecipe)) + ' signature=' +
      WfcPipelineSignatureHex(FRecipe.Signature) +
      ' resources=' + IntToStr(FRecipe.ResourceCount) +
      ' passes=' + IntToStr(FRecipe.PassCount) +
      ' dependencies=' + IntToStr(FRecipe.DependencyCount) +
      ' bridges=' + IntToStr(FRecipe.BridgeCount) +
      ' requirements=' + IntToStr(FRecipe.RequirementCount);
    if FRecipe.ValueQuotaCount <> 0 then
      Result := Result + ' value-quotas=' + IntToStr(FRecipe.ValueQuotaCount);
    if FRecipe.ConnectivityCount <> 0 then
      Result := Result + ' connectivities=' + IntToStr(FRecipe.ConnectivityCount);
    Exit(Result + #10);
  end;

  Result := 'valid canonical ' + Copy(FCanonicalText, 1,
    Pos(#10, FCanonicalText) - 1);
  case FKind of
    wakRules: Result := Result + ' values=' + IntToStr(FRules.ValueCount) +
      ' rules=' + IntToStr(FRules.RuleCount) + ' scope=schema';
    wakModel: Result := Result + ' values=' + IntToStr(FModel.ValueCount) +
      ' samples=' + IntToStr(FModel.SampleCount) + ' scope=schema';
    wakPattern2D: Result := Result + ' patterns=' + IntToStr(FPattern2D.PatternCount) +
      ' palette=' + IntToStr(FPattern2D.PaletteCount) + ' scope=schema';
    wakSequence: Result := Result + ' states=' + IntToStr(FSequence.StateCount) +
      ' tokens=' + IntToStr(FSequence.PublicTokenCount) + ' scope=schema';
    wakTraining: Result := Result + ' samples=' + IntToStr(FTraining.SampleCount) +
      ' value-quotas=' + IntToStr(FTraining.ValueQuotaCount) +
      ' connectivities=' + IntToStr(FTraining.ConnectivityCount) +
      ' scope=authoring-schema';
    wakRun: Result := Result + ' signature=' +
      WfcPipelineRunSignatureHex(FRun.Signature) + ' recipe=' +
      WfcPipelineSignatureHex(FRecipe.Signature) + ' scope=schema-and-binding';
    wakResult: Result := Result + ' signature=' +
      WfcPipelineResultSignatureHex(FStoredResult.Signature) + ' recipe=' +
      WfcPipelineSignatureHex(FRecipe.Signature) + ' run=' +
      WfcPipelineRunSignatureHex(FRun.Signature) + ' status=' +
      ResultStatusName(FStoredResult.Status) + ' scope=schema-and-binding';
  end;
  Result := Result + #10;
end;

procedure TWfcArtifactDocument.RequireReplay;
var LActual: TWfcPipelineResult; LText: String;
begin
  if FKind <> wakResult then
    raise EWfcArtifactReplayInvocation.Create('replay requires a result artifact');
  LActual := nil;
  try
    try
      LActual := ExecuteWfcPipeline(FRecipe, FRun);
    except
      on E: EWfcPipelineRuntime do
        raise EWfcArtifactReplayInvocation.Create(E.Message);
      on E: EWfcPipelineCompile do
        raise EWfcArtifactReplayInvocation.Create(E.Message);
    end;
    LText := EncodeWfcPipelineResultText(LActual);
    if LText <> FCanonicalText then
      raise EWfcArtifactReplayMismatch.Create(
        'fresh replay does not match the complete canonical result');
  finally
    LActual.Free;
  end;
end;

end.
