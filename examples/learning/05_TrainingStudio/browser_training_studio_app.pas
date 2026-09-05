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
unit browser_training_studio_app;

{$mode delphi}{$H+}

interface

uses
  JS,
  Web,
  SysUtils,
  wfc,
  wfc_model,
  wfc_pipeline_model,
  wfc_pipeline_run,
  wfc_pipeline_result,
  wfc_training,
  wfc_training_workspace;

type
  TBrowserTrainingStudioApplication = class
  strict private
    FWorkspace: TWfcTrainingWorkspace;
    FLimits: TWfcTrainingWorkspaceLimits;
    FLocks: TWfcPipelineCellLocks;
    FVocabulary: TWfcModelTokens;
    FConfiguredOptions: TWfcTrainingSolveOptions;
    FSelectedCell: Integer;
    FFileReader: TJSFileReader;
    FSourceDownloadUrl: String;
    FArtifactDownloadUrl: String;

    FPresetSelect: TJSHTMLSelectElement;
    FLoadPresetButton: TJSHTMLButtonElement;
    FSourceFileInput: TJSHTMLInputElement;
    FSourceInput: TJSHTMLTextAreaElement;
    FTrainButton: TJSHTMLButtonElement;
    FSourceDownloadLink: TJSHTMLAnchorElement;

    FRawNameInput: TJSHTMLInputElement;
    FRawSampleNameInput: TJSHTMLInputElement;
    FRawLicenseInput: TJSHTMLInputElement;
    FRawSourceInput: TJSHTMLInputElement;
    FRawOrderInput: TJSHTMLInputElement;
    FRawTextInput: TJSHTMLTextAreaElement;
    FConvertRawButton: TJSHTMLButtonElement;

    FStatusElement: TJSElement;
    FProfileElement: TJSElement;
    FSampleCountElement: TJSElement;
    FSourceTokenCountElement: TJSElement;
    FLockCountElement: TJSElement;
    FTrainingSignatureElement: TJSElement;
    FRecipeSignatureElement: TJSElement;
    FResultSignatureElement: TJSElement;
    FStatusDetailElement: TJSElement;

    FSolveButton: TJSHTMLButtonElement;
    FWidthInput: TJSHTMLInputElement;
    FHeightInput: TJSHTMLInputElement;
    FSeedInput: TJSHTMLInputElement;
    FStrategySelect: TJSHTMLSelectElement;
    FBacktracksInput: TJSHTMLInputElement;
    FPassBacktracksInput: TJSHTMLInputElement;
    FTraceInput: TJSHTMLInputElement;

    FLockXInput: TJSHTMLInputElement;
    FLockYInput: TJSHTMLInputElement;
    FLockTokenSelect: TJSHTMLSelectElement;
    FAddLockButton: TJSHTMLButtonElement;
    FLockList: TJSHTMLSelectElement;
    FRemoveLockButton: TJSHTMLButtonElement;
    FClearLocksButton: TJSHTMLButtonElement;

    FResultStatusElement: TJSElement;
    FOutputGrid: TJSElement;
    FOutputPlaceholder: TJSElement;
    FPassReportElement: TJSElement;
    FFailureReportElement: TJSElement;

    FArtifactSelect: TJSHTMLSelectElement;
    FArtifactDownloadLink: TJSHTMLAnchorElement;
    FArtifactOutput: TJSHTMLTextAreaElement;
    FArtifactStatus: TJSElement;

    function RequireElement(const AId: String): TJSElement;
    procedure BindDocument;
    procedure BindEvents;
    procedure PopulatePresets;
    procedure WriteOptions(const AOptions: TWfcTrainingSolveOptions);
    procedure LoadPreset(const AIndex: Integer);
    procedure ApplySourceText(const AText: String);
    procedure TrainWorkspace;
    procedure SolveWorkspace;
    procedure InvalidateRun(const AReason: String);

    function ReadBoundedInteger(const AInput: TJSHTMLInputElement;
      const AName: String; const AMinimum, AMaximum: Integer): Integer;
    function TryParseSeed(const AText: String; out ASeed: TGraphSeed): Boolean;
    function ReadOptions: TWfcTrainingSolveOptions;
    function ReadLockCoordinate(const AInput: TJSHTMLInputElement;
      const AName: String; const AMaximum: Integer): Integer;
    function SelectedVocabularyIndex: Integer;
    function FindVocabularyToken(const AToken: TWfcModelToken): Integer;
    procedure AddOrReplaceLock(const AX, AY: Integer;
      const AToken: TWfcModelToken);
    procedure SortLocks;

    procedure SetState(const AState, AStatus, ADetail: String);
    procedure ShowError(const AMessage: String);
    procedure RefreshAll;
    procedure RefreshMetrics;
    procedure RefreshVocabulary;
    procedure RefreshLocks;
    procedure RefreshResult;
    procedure RefreshReports;
    procedure RefreshArtifact;
    procedure RefreshDownloads;
    procedure ClearOutput;
    procedure SetDownloadLink(const ALink: TJSHTMLAnchorElement;
      const AText, AFileName: String; var AObjectUrl: String);
    function SelectedArtifactText: String;
    function SelectedArtifactFileName: String;
    function ProfileName(const AKind: TWfcTrainingKind): String;
    function ResultStatusName(const AStatus: TWfcPipelineResultStatus): String;
    function VisibilityName(const AVisibility: TWfcPipelinePassVisibility): String;
    function AdapterName(const AAdapter: TWfcPipelineAdapterKind): String;
    function DispositionName(const ADisposition: TGraphPassDisposition): String;
    function ContradictionName(const AKind: TGraphContradictionKind): String;
    function DisplayToken(const AToken: TWfcModelToken): String;

    procedure ConvertRawText;
    procedure BeginSourceFileRead;
    procedure CancelSourceFileRead;
    procedure CommitSourceFileText(const AReader: TJSFileReader;
      const AText: String);
    procedure DiscardSourceForImportError;
    procedure AssertTest(const ACondition: Boolean; const AMessage: String);
    procedure DispatchDomEvent(const AElement: TJSElement;
      const AEventName: String);
    procedure RunSelfTest;

    function HandleLoadPreset(AEvent: TJSMouseEvent): Boolean;
    function HandleSourceInput(AEvent: TJSEvent): Boolean;
    function HandleSourceFile(AEvent: TJSEvent): Boolean;
    function HandleSourceFileLoaded(AEvent: TJSEvent): Boolean;
    function HandleSourceFileError(AEvent: TJSEvent): Boolean;
    function HandleTrain(AEvent: TJSMouseEvent): Boolean;
    function HandleConvertRaw(AEvent: TJSMouseEvent): Boolean;
    function HandleRunInput(AEvent: TJSEvent): Boolean;
    function HandleSolve(AEvent: TJSMouseEvent): Boolean;
    function HandleAddLock(AEvent: TJSMouseEvent): Boolean;
    function HandleRemoveLock(AEvent: TJSMouseEvent): Boolean;
    function HandleClearLocks(AEvent: TJSMouseEvent): Boolean;
    function HandleOutputClick(AEvent: TJSMouseEvent): Boolean;
    function HandleArtifactChange(AEvent: TJSEvent): Boolean;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Run;
  end;

implementation

uses
  wfc_text_codec,
  wfc_training_text,
  wfc_text_training,
  training_studio_presets;

const
  MAX_SEED = Cardinal($FFFFFFFF);
  INITIAL_PRESET = 2;
  RAW_TEXT_STORAGE_LIMIT = 2048;
  BASELINE_SOURCE_SIGNATURE = '0FA2C5EA';
  BASELINE_RECIPE_SIGNATURE = 'DBCBA621';
  BASELINE_RESULT_SIGNATURE = '947C4AFD';

constructor TBrowserTrainingStudioApplication.Create;
begin
  inherited Create;
  FWorkspace := TWfcTrainingWorkspace.Create(
    InteractiveWfcTrainingWorkspaceLimits);
  FLimits := FWorkspace.CopyLimits;
  FLocks := nil;
  FVocabulary := nil;
  FConfiguredOptions := DefaultWfcTrainingSolveOptions;
  FSelectedCell := -1;
  FFileReader := nil;
  FSourceDownloadUrl := '';
  FArtifactDownloadUrl := '';
end;

destructor TBrowserTrainingStudioApplication.Destroy;
begin
  if FSourceDownloadUrl <> '' then
    TJSURL.revokeObjectURL(FSourceDownloadUrl);
  if FArtifactDownloadUrl <> '' then
    TJSURL.revokeObjectURL(FArtifactDownloadUrl);
  FWorkspace.Free;
  inherited Destroy;
end;

function TBrowserTrainingStudioApplication.RequireElement(
  const AId: String): TJSElement;
begin
  Result := document.getElementById(AId);
  if not Assigned(Result) then
    raise EWfcTrainingWorkspace.Create('training studio is missing #' + AId);
end;

procedure TBrowserTrainingStudioApplication.BindDocument;
begin
  FPresetSelect := TJSHTMLSelectElement(RequireElement('preset-select'));
  FLoadPresetButton := TJSHTMLButtonElement(RequireElement('load-preset-button'));
  FSourceFileInput := TJSHTMLInputElement(RequireElement('source-file-input'));
  FSourceInput := TJSHTMLTextAreaElement(RequireElement('source-input'));
  FTrainButton := TJSHTMLButtonElement(RequireElement('train-button'));
  FSourceDownloadLink := TJSHTMLAnchorElement(
    RequireElement('download-source-link'));

  FRawNameInput := TJSHTMLInputElement(RequireElement('raw-name-input'));
  FRawSampleNameInput := TJSHTMLInputElement(
    RequireElement('raw-sample-name-input'));
  FRawLicenseInput := TJSHTMLInputElement(RequireElement('raw-license-input'));
  FRawSourceInput := TJSHTMLInputElement(RequireElement('raw-source-input'));
  FRawOrderInput := TJSHTMLInputElement(RequireElement('raw-order-input'));
  FRawTextInput := TJSHTMLTextAreaElement(RequireElement('raw-text-input'));
  FConvertRawButton := TJSHTMLButtonElement(RequireElement('convert-raw-button'));

  FStatusElement := RequireElement('status');
  FProfileElement := RequireElement('profile-output');
  FSampleCountElement := RequireElement('sample-count');
  FSourceTokenCountElement := RequireElement('source-token-count');
  FLockCountElement := RequireElement('lock-count');
  FTrainingSignatureElement := RequireElement('training-signature');
  FRecipeSignatureElement := RequireElement('recipe-signature');
  FResultSignatureElement := RequireElement('result-signature');
  FStatusDetailElement := RequireElement('status-detail');

  FSolveButton := TJSHTMLButtonElement(RequireElement('solve-button'));
  FWidthInput := TJSHTMLInputElement(RequireElement('width-input'));
  FHeightInput := TJSHTMLInputElement(RequireElement('height-input'));
  FSeedInput := TJSHTMLInputElement(RequireElement('seed-input'));
  FStrategySelect := TJSHTMLSelectElement(RequireElement('strategy-select'));
  FBacktracksInput := TJSHTMLInputElement(RequireElement('backtracks-input'));
  FPassBacktracksInput := TJSHTMLInputElement(
    RequireElement('pass-backtracks-input'));
  FTraceInput := TJSHTMLInputElement(RequireElement('trace-input'));

  FLockXInput := TJSHTMLInputElement(RequireElement('lock-x-input'));
  FLockYInput := TJSHTMLInputElement(RequireElement('lock-y-input'));
  FLockTokenSelect := TJSHTMLSelectElement(
    RequireElement('lock-token-select'));
  FAddLockButton := TJSHTMLButtonElement(RequireElement('add-lock-button'));
  FLockList := TJSHTMLSelectElement(RequireElement('lock-list'));
  FRemoveLockButton := TJSHTMLButtonElement(
    RequireElement('remove-lock-button'));
  FClearLocksButton := TJSHTMLButtonElement(
    RequireElement('clear-locks-button'));

  FResultStatusElement := RequireElement('result-status');
  FOutputGrid := RequireElement('output-grid');
  FOutputPlaceholder := RequireElement('output-placeholder');
  FPassReportElement := RequireElement('pass-report');
  FFailureReportElement := RequireElement('failure-report');

  FArtifactSelect := TJSHTMLSelectElement(RequireElement('artifact-select'));
  FArtifactDownloadLink := TJSHTMLAnchorElement(
    RequireElement('download-artifact-link'));
  FArtifactOutput := TJSHTMLTextAreaElement(RequireElement('artifact-output'));
  FArtifactStatus := RequireElement('artifact-status');
end;

procedure TBrowserTrainingStudioApplication.BindEvents;
begin
  FLoadPresetButton.onclick := @HandleLoadPreset;
  FSourceInput.oninput := @HandleSourceInput;
  FSourceFileInput.onchange := @HandleSourceFile;
  FTrainButton.onclick := @HandleTrain;
  FConvertRawButton.onclick := @HandleConvertRaw;

  FWidthInput.oninput := @HandleRunInput;
  FHeightInput.oninput := @HandleRunInput;
  FSeedInput.oninput := @HandleRunInput;
  FStrategySelect.onchange := @HandleRunInput;
  FBacktracksInput.oninput := @HandleRunInput;
  FPassBacktracksInput.oninput := @HandleRunInput;
  FTraceInput.onchange := @HandleRunInput;
  FSolveButton.onclick := @HandleSolve;

  FAddLockButton.onclick := @HandleAddLock;
  FRemoveLockButton.onclick := @HandleRemoveLock;
  FClearLocksButton.onclick := @HandleClearLocks;
  FArtifactSelect.onchange := @HandleArtifactChange;
end;

procedure TBrowserTrainingStudioApplication.PopulatePresets;
var
  I: Integer;
  LOption: TJSHTMLOptionElement;
begin
  FPresetSelect.textContent := '';
  for I := 0 to TRAINING_STUDIO_PRESET_COUNT - 1 do
  begin
    LOption := TJSHTMLOptionElement(document.createElement('option'));
    LOption.value := IntToStr(I);
    LOption.textContent := IntToStr(I) + ' · ' + TrainingStudioPresetName(I);
    FPresetSelect.appendChild(LOption);
  end;
  FPresetSelect.value := IntToStr(INITIAL_PRESET);
end;

procedure TBrowserTrainingStudioApplication.WriteOptions(
  const AOptions: TWfcTrainingSolveOptions);
begin
  FWidthInput.value := IntToStr(AOptions.Width);
  FHeightInput.value := IntToStr(AOptions.Height);
  FSeedInput.value := UIntToStr(AOptions.Seed);
  if AOptions.Strategy = wpssNegotiated then
    FStrategySelect.value := 'negotiated'
  else
    FStrategySelect.value := 'one-way';
  FBacktracksInput.value := IntToStr(AOptions.MaxBacktracks);
  FPassBacktracksInput.value := IntToStr(AOptions.MaxPassBacktracks);
  FTraceInput.checked := AOptions.CaptureTrace;
end;

procedure TBrowserTrainingStudioApplication.LoadPreset(const AIndex: Integer);
begin
  FPresetSelect.value := IntToStr(AIndex);
  WriteOptions(TrainingStudioPresetOptions(AIndex));
  ApplySourceText(TrainingStudioPresetText(AIndex));
  SetState('source-dirty', 'Preset loaded; train to continue.',
    TrainingStudioPresetName(AIndex) + ' is the current editable source.');
end;

procedure TBrowserTrainingStudioApplication.ApplySourceText(
  const AText: String);
begin
  CancelSourceFileRead;
  FLocks := nil;
  FVocabulary := nil;
  FSelectedCell := -1;
  FSourceInput.value := AText;
  FWorkspace.SetSourceText(AText);
  RefreshAll;
  SetState('source-dirty', 'Source changed; derived artifacts cleared.',
    'Train the current source before configuring another run.');
end;

procedure TBrowserTrainingStudioApplication.TrainWorkspace;
begin
  CancelSourceFileRead;
  FLocks := nil;
  FSelectedCell := -1;
  FWorkspace.SetSourceText(FSourceInput.value);
  FWorkspace.Train;
  RefreshAll;
  SetState('trained', 'Recipe trained.',
    'The model and recipe are current; configure and solve a bounded run.');
end;

procedure TBrowserTrainingStudioApplication.SolveWorkspace;
var
  LOptions: TWfcTrainingSolveOptions;
  LStatus: TWfcPipelineResultStatus;
begin
  { Clearing first is deliberate: malformed edited options cannot leave an
    older run or result looking current. }
  FWorkspace.ClearRun;
  FSelectedCell := -1;
  RefreshAll;
  LOptions := ReadOptions;
  FWorkspace.ConfigureRun(LOptions, FLocks, nil);
  FConfiguredOptions := LOptions;
  FWorkspace.Solve;
  RefreshAll;
  LStatus := FWorkspace.ResultStatus;
  case LStatus of
    wprsSolved:
      SetState('solved', 'Solved public output.',
        'The terminal result and every replay artifact are current.');
    wprsContradiction:
      SetState('contradiction', 'Constraints contradict.',
        'The terminal failure is exportable; no partial public output is shown.');
    wprsSolverBacktrackLimit:
      SetState('solver-limit', 'Local search budget exhausted.',
        'The terminal limit result is exportable; increase the local budget to retry.');
    wprsPassBacktrackLimit:
      SetState('pass-limit', 'Pass search budget exhausted.',
        'The terminal limit result is exportable; increase the pass budget to retry.');
  end;
end;

procedure TBrowserTrainingStudioApplication.InvalidateRun(
  const AReason: String);
begin
  FWorkspace.ClearRun;
  FSelectedCell := -1;
  RefreshAll;
  if FWorkspace.HasRecipe then
    SetState('run-dirty', 'Run inputs changed; old result cleared.', AReason)
  else
    SetState('source-dirty', 'Train the source before solving.', AReason);
end;

function TBrowserTrainingStudioApplication.ReadBoundedInteger(
  const AInput: TJSHTMLInputElement; const AName: String;
  const AMinimum, AMaximum: Integer): Integer;
begin
  if not TryStrToInt(Trim(AInput.value), Result) or
      (Result < AMinimum) or (Result > AMaximum) then
    raise EConvertError.CreateFmt('%s must be from %d through %d',
      [AName, AMinimum, AMaximum]);
end;

function TBrowserTrainingStudioApplication.TryParseSeed(
  const AText: String; out ASeed: TGraphSeed): Boolean;
var
  LBase: Cardinal;
  LCharacter: Char;
  LDigit: Cardinal;
  LIndex: Integer;
  LStart: Integer;
  LText: String;
  LValue: Cardinal;
begin
  Result := False;
  ASeed := 0;
  LText := Trim(AText);
  if LText = '' then Exit;
  LBase := 10;
  LStart := 1;
  if LText[1] = '$' then
  begin
    LBase := 16;
    LStart := 2;
  end
  else if (Length(LText) >= 2) and (LText[1] = '0') and
      ((LText[2] = 'x') or (LText[2] = 'X')) then
  begin
    LBase := 16;
    LStart := 3;
  end;
  if LStart > Length(LText) then Exit;
  LValue := 0;
  for LIndex := LStart to Length(LText) do
  begin
    LCharacter := LText[LIndex];
    if LCharacter in ['0'..'9'] then
      LDigit := Cardinal(Ord(LCharacter) - Ord('0'))
    else if (LBase = 16) and (LCharacter in ['a'..'f']) then
      LDigit := Cardinal(Ord(LCharacter) - Ord('a') + 10)
    else if (LBase = 16) and (LCharacter in ['A'..'F']) then
      LDigit := Cardinal(Ord(LCharacter) - Ord('A') + 10)
    else
      Exit;
    if LValue > (MAX_SEED - LDigit) div LBase then Exit;
    LValue := (LValue * LBase) + LDigit;
  end;
  ASeed := TGraphSeed(LValue);
  Result := True;
end;

function TBrowserTrainingStudioApplication.ReadOptions:
  TWfcTrainingSolveOptions;
begin
  Result := DefaultWfcTrainingSolveOptions;
  Result.Width := ReadBoundedInteger(FWidthInput, 'width', 1,
    FLimits.MaxOutputCells);
  Result.Height := ReadBoundedInteger(FHeightInput, 'height', 1,
    FLimits.MaxOutputCells);
  if not TryParseSeed(FSeedInput.value, Result.Seed) then
    raise EConvertError.Create('seed must be decimal, $hex, or 0xhex');
  if FStrategySelect.value = 'one-way' then
    Result.Strategy := wpssOneWay
  else if FStrategySelect.value = 'negotiated' then
    Result.Strategy := wpssNegotiated
  else
    raise EConvertError.Create('unknown solve strategy');
  Result.MaxBacktracks := ReadBoundedInteger(FBacktracksInput,
    'local backtrack budget', 0, FLimits.MaxBacktracks);
  Result.MaxPassBacktracks := ReadBoundedInteger(FPassBacktracksInput,
    'pass backtrack budget', 0, FLimits.MaxPassBacktracks);
  if (Result.Strategy = wpssOneWay) and
      (Result.MaxPassBacktracks <> 0) then
    raise EConvertError.Create(
      'one-way strategy requires a zero pass-backtrack budget');
  Result.CaptureTrace := FTraceInput.checked;
end;

function TBrowserTrainingStudioApplication.ReadLockCoordinate(
  const AInput: TJSHTMLInputElement; const AName: String;
  const AMaximum: Integer): Integer;
begin
  Result := ReadBoundedInteger(AInput, AName, 0, AMaximum - 1);
end;

function TBrowserTrainingStudioApplication.SelectedVocabularyIndex: Integer;
begin
  if not TryStrToInt(FLockTokenSelect.value, Result) or
      (Result < 0) or (Result >= Length(FVocabulary)) then
    raise EConvertError.Create('select a public output token');
end;

function TBrowserTrainingStudioApplication.FindVocabularyToken(
  const AToken: TWfcModelToken): Integer;
begin
  for Result := 0 to Length(FVocabulary) - 1 do
    if FVocabulary[Result] = AToken then Exit;
  Result := -1;
end;

procedure TBrowserTrainingStudioApplication.AddOrReplaceLock(
  const AX, AY: Integer; const AToken: TWfcModelToken);
var
  I: Integer;
begin
  for I := 0 to Length(FLocks) - 1 do
    if (FLocks[I].X = AX) and (FLocks[I].Y = AY) and
        (FLocks[I].Z = 0) then
    begin
      FLocks[I] := MakeWfcPipelineCellLock(FWorkspace.PublicPassIndex,
        AX, AY, 0, AToken);
      SortLocks;
      Exit;
    end;
  SetLength(FLocks, Length(FLocks) + 1);
  FLocks[High(FLocks)] := MakeWfcPipelineCellLock(
    FWorkspace.PublicPassIndex, AX, AY, 0, AToken);
  SortLocks;
end;

procedure TBrowserTrainingStudioApplication.SortLocks;
var
  I: Integer;
  J: Integer;
  LValue: TWfcPipelineCellLock;
begin
  for I := 1 to Length(FLocks) - 1 do
  begin
    LValue := FLocks[I];
    J := I - 1;
    while (J >= 0) and
        ((FLocks[J].PassIndex > LValue.PassIndex) or
         ((FLocks[J].PassIndex = LValue.PassIndex) and
          ((FLocks[J].Z > LValue.Z) or
           ((FLocks[J].Z = LValue.Z) and
            ((FLocks[J].Y > LValue.Y) or
             ((FLocks[J].Y = LValue.Y) and
              (FLocks[J].X > LValue.X))))))) do
    begin
      FLocks[J + 1] := FLocks[J];
      Dec(J);
    end;
    FLocks[J + 1] := LValue;
  end;
end;

procedure TBrowserTrainingStudioApplication.SetState(
  const AState, AStatus, ADetail: String);
begin
  document.body.setAttribute('data-state', AState);
  FStatusElement.textContent := AStatus;
  FStatusDetailElement.textContent := ADetail;
end;

procedure TBrowserTrainingStudioApplication.ShowError(
  const AMessage: String);
begin
  try
    RefreshAll;
  except
    { Preserve the original error if rendering also encounters bad state. }
  end;
  SetState('error', 'Error: ' + AMessage,
    'No artifact invalidated by this operation is displayed as current.');
end;

procedure TBrowserTrainingStudioApplication.RefreshAll;
begin
  RefreshVocabulary;
  RefreshLocks;
  RefreshMetrics;
  RefreshResult;
  RefreshReports;
  RefreshDownloads;
  RefreshArtifact;
end;

procedure TBrowserTrainingStudioApplication.RefreshMetrics;
var
  LOptions: TWfcTrainingOptions;
  LPasses: TWfcPipelinePasses;
  LProfile: String;
  LResultStatus: String;
  LSourceSignature: String;
  LRecipeSignature: String;
  LResultSignature: String;
  LCellCount: Integer;
  LTokens: TWfcModelTokens;
begin
  LProfile := '';
  LResultStatus := 'none';
  LSourceSignature := '';
  LRecipeSignature := '';
  LResultSignature := '';
  LCellCount := 0;
  LPasses := nil;

  if FWorkspace.HasRecipe then
  begin
    LOptions := FWorkspace.SourceOptions;
    LProfile := ProfileName(LOptions.Kind);
    LSourceSignature := FWorkspace.TrainingSignatureText;
    LRecipeSignature := FWorkspace.RecipeSignatureText;
    FProfileElement.textContent := LProfile;
    FSampleCountElement.textContent := IntToStr(FWorkspace.SampleCount);
    FSourceTokenCountElement.textContent :=
      IntToStr(FWorkspace.SourceTokenCount);
    FTrainingSignatureElement.textContent := LSourceSignature;
    FRecipeSignatureElement.textContent := LRecipeSignature;
    LPasses := FWorkspace.CopyPasses;
  end
  else
  begin
    FProfileElement.textContent := '—';
    FSampleCountElement.textContent := '0';
    FSourceTokenCountElement.textContent := '0';
    FTrainingSignatureElement.textContent := '—';
    FRecipeSignatureElement.textContent := '—';
  end;

  if FWorkspace.HasResult then
  begin
    LResultStatus := ResultStatusName(FWorkspace.ResultStatus);
    LResultSignature := FWorkspace.ResultSignatureText;
    FResultSignatureElement.textContent := LResultSignature;
    if FWorkspace.ResultStatus = wprsSolved then
    begin
      LTokens := FWorkspace.OutputTokens;
      LCellCount := Length(LTokens);
    end;
  end
  else
    FResultSignatureElement.textContent := '—';

  FLockCountElement.textContent := IntToStr(Length(FLocks));
  document.body.setAttribute('data-profile', LProfile);
  document.body.setAttribute('data-source-signature', LSourceSignature);
  document.body.setAttribute('data-training-signature', LSourceSignature);
  document.body.setAttribute('data-recipe-signature', LRecipeSignature);
  document.body.setAttribute('data-result-signature', LResultSignature);
  document.body.setAttribute('data-result-status', LResultStatus);
  document.body.setAttribute('data-output-count', IntToStr(LCellCount));
  document.body.setAttribute('data-cell-count', IntToStr(LCellCount));
  document.body.setAttribute('data-pass-count', IntToStr(Length(LPasses)));
  document.body.setAttribute('data-lock-count', IntToStr(Length(FLocks)));
end;

procedure TBrowserTrainingStudioApplication.RefreshVocabulary;
var
  I: Integer;
  LHasSelection: Boolean;
  LOption: TJSHTMLOptionElement;
  LSelectedIndex: Integer;
  LSelectedToken: TWfcModelToken;
begin
  LHasSelection := TryStrToInt(FLockTokenSelect.value, LSelectedIndex) and
    (LSelectedIndex >= 0) and (LSelectedIndex < Length(FVocabulary));
  if LHasSelection then LSelectedToken := FVocabulary[LSelectedIndex];
  FVocabulary := nil;
  FLockTokenSelect.textContent := '';
  if not FWorkspace.HasRecipe then
  begin
    FLockTokenSelect.disabled := True;
    Exit;
  end;
  FVocabulary := FWorkspace.PublicVocabulary;
  for I := 0 to Length(FVocabulary) - 1 do
  begin
    LOption := TJSHTMLOptionElement(document.createElement('option'));
    LOption.value := IntToStr(I);
    LOption.textContent := DisplayToken(FVocabulary[I]);
    FLockTokenSelect.appendChild(LOption);
  end;
  FLockTokenSelect.disabled := Length(FVocabulary) = 0;
  if LHasSelection then
  begin
    LSelectedIndex := FindVocabularyToken(LSelectedToken);
    if LSelectedIndex >= 0 then
      FLockTokenSelect.value := IntToStr(LSelectedIndex);
  end;
end;

procedure TBrowserTrainingStudioApplication.RefreshLocks;
var
  I: Integer;
  LOption: TJSHTMLOptionElement;
  LSelectedIndex: Integer;
begin
  LSelectedIndex := FLockList.selectedIndex;
  FLockList.textContent := '';
  for I := 0 to Length(FLocks) - 1 do
  begin
    LOption := TJSHTMLOptionElement(document.createElement('option'));
    LOption.value := IntToStr(I);
    LOption.textContent := 'x=' + IntToStr(FLocks[I].X) +
      ' y=' + IntToStr(FLocks[I].Y) + ' token=' +
      DisplayToken(FLocks[I].Token);
    FLockList.appendChild(LOption);
  end;
  FRemoveLockButton.disabled := Length(FLocks) = 0;
  FClearLocksButton.disabled := Length(FLocks) = 0;
  FAddLockButton.disabled := not FWorkspace.HasRecipe;
  if (LSelectedIndex >= 0) and (LSelectedIndex < Length(FLocks)) then
    FLockList.selectedIndex := LSelectedIndex;
end;

procedure TBrowserTrainingStudioApplication.ClearOutput;
begin
  FOutputGrid.textContent := '';
  FOutputGrid.setAttribute('style', '--grid-columns:1');
  FOutputPlaceholder.removeAttribute('hidden');
  FSelectedCell := -1;
end;

procedure TBrowserTrainingStudioApplication.RefreshResult;
var
  I: Integer;
  LX: Integer;
  LY: Integer;
  LButton: TJSHTMLButtonElement;
  LStatus: TWfcPipelineResultStatus;
  LTokens: TWfcModelTokens;
begin
  ClearOutput;
  if not FWorkspace.HasResult then
  begin
    FResultStatusElement.textContent := 'not run';
    FOutputPlaceholder.textContent :=
      'Train and solve to inspect public cells.';
    Exit;
  end;

  LStatus := FWorkspace.ResultStatus;
  FResultStatusElement.textContent := ResultStatusName(LStatus);
  if LStatus <> wprsSolved then
  begin
    FOutputPlaceholder.textContent :=
      'Terminal ' + ResultStatusName(LStatus) +
      ' result contains no partial public output.';
    Exit;
  end;

  LTokens := FWorkspace.OutputTokens;
  FOutputGrid.setAttribute('style', '--grid-columns:' +
    IntToStr(FConfiguredOptions.Width));
  for I := 0 to Length(LTokens) - 1 do
  begin
    LX := I mod FConfiguredOptions.Width;
    LY := I div FConfiguredOptions.Width;
    LButton := TJSHTMLButtonElement(document.createElement('button'));
    LButton._type := 'button';
    LButton.className := 'output-cell';
    LButton.textContent := DisplayToken(LTokens[I]);
    LButton.setAttribute('data-index', IntToStr(I));
    LButton.setAttribute('data-x', IntToStr(LX));
    LButton.setAttribute('data-y', IntToStr(LY));
    LButton.setAttribute('role', 'gridcell');
    LButton.setAttribute('aria-label', 'x ' + IntToStr(LX) +
      ', y ' + IntToStr(LY) + ', token ' + DisplayToken(LTokens[I]));
    LButton.onclick := @HandleOutputClick;
    FOutputGrid.appendChild(LButton);
  end;
  FOutputPlaceholder.setAttribute('hidden', '');
end;

procedure TBrowserTrainingStudioApplication.RefreshReports;
var
  I: Integer;
  LFailure: TWfcPipelineFailure;
  LOutcomes: TWfcPipelinePassOutcomes;
  LPasses: TWfcPipelinePasses;
  LText: String;
begin
  if not FWorkspace.HasRecipe then
  begin
    FPassReportElement.textContent := 'No recipe trained.';
    FFailureReportElement.textContent := 'No terminal result.';
    Exit;
  end;

  LPasses := FWorkspace.CopyPasses;
  LOutcomes := nil;
  if FWorkspace.HasResult then
    LOutcomes := FWorkspace.CopyPassOutcomes;
  LText := 'rank=' + IntToStr(FWorkspace.Rank) +
    ' wrap=' + LowerCase(BoolToStr(FWorkspace.WrapNeighbors, True)) +
    ' passes=' + IntToStr(Length(LPasses));
  for I := 0 to Length(LPasses) - 1 do
  begin
    LText := LText + LineEnding + IntToStr(I) + ' ' +
      String(LPasses[I].LabelName) + ' [' +
      VisibilityName(LPasses[I].Visibility) + '/' +
      AdapterName(LPasses[I].AdapterKind) + ']';
    if I < Length(LOutcomes) then
      LText := LText + ' ' + DispositionName(LOutcomes[I].Disposition) +
        ' decisions=' + IntToStr(LOutcomes[I].Decisions) +
        ' propagations=' + IntToStr(LOutcomes[I].Propagations) +
        ' contradictions=' + IntToStr(LOutcomes[I].Contradictions) +
        ' backtracks=' + IntToStr(LOutcomes[I].Backtracks) +
        ' excluded=' + IntToStr(LOutcomes[I].ExcludedAssignments) +
        ' executed=' + LowerCase(BoolToStr(LOutcomes[I].Executed, True)) +
        ' ordinal=' + IntToStr(LOutcomes[I].ExecutionOrdinal);
  end;
  FPassReportElement.textContent := LText;

  if not FWorkspace.HasResult then
  begin
    FFailureReportElement.textContent := 'No terminal result.';
    Exit;
  end;
  if FWorkspace.ResultStatus = wprsSolved then
  begin
    FFailureReportElement.textContent :=
      'none' + LineEnding + 'The terminal result is solved.';
    Exit;
  end;
  LFailure := FWorkspace.CopyFailure;
  LText := 'kind=' + ContradictionName(LFailure.Kind) +
    LineEnding + 'pass=' + IntToStr(LFailure.PassIndex) +
    LineEnding + 'entry=' + IntToStr(LFailure.EntryIndex) +
    LineEnding + 'neighbor=' + IntToStr(LFailure.NeighborIndex) +
    LineEnding + 'dependency-pass=' +
      IntToStr(LFailure.DependencyPassIndex);
  if LFailure.HasDirection then
    LText := LText + LineEnding + 'direction=' +
      IntToStr(Ord(LFailure.Direction));
  FFailureReportElement.textContent := LText;
end;

procedure TBrowserTrainingStudioApplication.SetDownloadLink(
  const ALink: TJSHTMLAnchorElement; const AText, AFileName: String;
  var AObjectUrl: String);
var
  LBlob: TJSBlob;
  LParts: TJSArray;
begin
  if AObjectUrl <> '' then
  begin
    TJSURL.revokeObjectURL(AObjectUrl);
    AObjectUrl := '';
  end;
  if AText = '' then
  begin
    ALink.removeAttribute('href');
    ALink.removeAttribute('download');
    ALink.setAttribute('aria-disabled', 'true');
    ALink.className := 'button-link disabled';
    Exit;
  end;
  LParts := TJSArray.new;
  LParts.push(AText);
  LBlob := TJSBlob.new(LParts);
  AObjectUrl := TJSURL.createObjectURL(LBlob);
  ALink.href := AObjectUrl;
  ALink.download := AFileName;
  ALink.setAttribute('aria-disabled', 'false');
  ALink.className := 'button-link';
end;

procedure TBrowserTrainingStudioApplication.RefreshDownloads;
begin
  SetDownloadLink(FSourceDownloadLink, FWorkspace.SourceText,
    'training-source.wfclearn', FSourceDownloadUrl);
end;

function TBrowserTrainingStudioApplication.SelectedArtifactText: String;
begin
  Result := '';
  if FArtifactSelect.value = 'source' then
    Result := FWorkspace.SourceText
  else if FArtifactSelect.value = 'model' then
  begin
    if FWorkspace.HasRecipe then Result := FWorkspace.ModelText;
  end
  else if FArtifactSelect.value = 'recipe' then
  begin
    if FWorkspace.HasRecipe then Result := FWorkspace.RecipeText;
  end
  else if FArtifactSelect.value = 'run' then
  begin
    if FWorkspace.HasRun then Result := FWorkspace.RunText;
  end
  else if FArtifactSelect.value = 'result' then
  begin
    if FWorkspace.HasResult then Result := FWorkspace.ResultText;
  end
  else
    raise ERangeError.Create('unknown artifact selection');
end;

function TBrowserTrainingStudioApplication.SelectedArtifactFileName: String;
begin
  if FArtifactSelect.value = 'source' then
    Result := 'training-source.wfclearn'
  else if FArtifactSelect.value = 'model' then
    Result := 'learned.model'
  else if FArtifactSelect.value = 'recipe' then
    Result := 'recipe.wfcpipeline'
  else if FArtifactSelect.value = 'run' then
    Result := 'run.wfcrun'
  else if FArtifactSelect.value = 'result' then
    Result := 'result.wfcresult'
  else
    raise ERangeError.Create('unknown artifact selection');
end;

procedure TBrowserTrainingStudioApplication.RefreshArtifact;
var
  LText: String;
begin
  LText := SelectedArtifactText;
  FArtifactOutput.value := LText;
  SetDownloadLink(FArtifactDownloadLink, LText,
    SelectedArtifactFileName, FArtifactDownloadUrl);
  if LText = '' then
    FArtifactStatus.textContent :=
      'This artifact is unavailable for the current workspace state.'
  else
    FArtifactStatus.textContent := IntToStr(Length(LText)) +
      ' characters · canonical LF text ready to download.';
end;

function TBrowserTrainingStudioApplication.ProfileName(
  const AKind: TWfcTrainingKind): String;
begin
  case AKind of
    wtkAdjacency1D: Result := 'adjacency1d';
    wtkAdjacency2D: Result := 'adjacency2d';
    wtkPattern2D: Result := 'pattern2d';
    wtkSequence: Result := 'sequence';
  else
    Result := 'unknown';
  end;
end;

function TBrowserTrainingStudioApplication.ResultStatusName(
  const AStatus: TWfcPipelineResultStatus): String;
begin
  case AStatus of
    wprsSolved: Result := 'solved';
    wprsContradiction: Result := 'contradiction';
    wprsSolverBacktrackLimit: Result := 'solver-backtrack-limit';
    wprsPassBacktrackLimit: Result := 'pass-backtrack-limit';
  else
    Result := 'unknown';
  end;
end;

function TBrowserTrainingStudioApplication.VisibilityName(
  const AVisibility: TWfcPipelinePassVisibility): String;
begin
  if AVisibility = wppvPublic then Result := 'public' else Result := 'private';
end;

function TBrowserTrainingStudioApplication.AdapterName(
  const AAdapter: TWfcPipelineAdapterKind): String;
begin
  case AAdapter of
    wpakEmpty: Result := 'projection';
    wpakModel: Result := 'adjacency';
    wpakRules: Result := 'rules';
    wpakPattern2D: Result := 'pattern2d';
    wpakSequence: Result := 'sequence';
  else
    Result := 'unknown';
  end;
end;

function TBrowserTrainingStudioApplication.DispositionName(
  const ADisposition: TGraphPassDisposition): String;
begin
  case ADisposition of
    gpdNotRun: Result := 'not-run';
    gpdReused: Result := 'reused';
    gpdCleared: Result := 'cleared';
    gpdCopied: Result := 'copied';
    gpdSolved: Result := 'solved';
    gpdFailed: Result := 'failed';
  else
    Result := 'unknown';
  end;
end;

function TBrowserTrainingStudioApplication.ContradictionName(
  const AKind: TGraphContradictionKind): String;
begin
  case AKind of
    gckNone: Result := 'none';
    gckEmptyDomain: Result := 'empty-domain';
    gckInvalidLock: Result := 'invalid-lock';
    gckAdjacency: Result := 'adjacency';
    gckPreviousPass: Result := 'previous-pass';
    gckRequiredSupport: Result := 'required-support';
    gckFinalValidation: Result := 'final-validation';
    gckPassDependency: Result := 'pass-dependency';
    gckEntryDomain: Result := 'entry-domain';
    gckExcludedAssignment: Result := 'excluded-assignment';
    gckConnectivity: Result := 'connectivity';
  else
    Result := 'unknown';
  end;
end;

function TBrowserTrainingStudioApplication.DisplayToken(
  const AToken: TWfcModelToken): String;
begin
  Result := WfcTextEncodeToken(AToken, 'training studio token');
end;

procedure TBrowserTrainingStudioApplication.ConvertRawText;
var
  LDocument: TWfcTrainingDocument;
  LMetadata: TWfcTrainingMetadata;
  LOrder: Integer;
  LSample: TWfcTrainingSample;
  LSamples: TWfcTextTrainingSamples;
  LText: String;
  LOptions: TWfcTrainingSolveOptions;
begin
  CancelSourceFileRead;
  if Trim(FRawNameInput.value) = '' then
    raise EConvertError.Create('raw document name is required');
  if Trim(FRawSampleNameInput.value) = '' then
    raise EConvertError.Create('raw sample name is required');
  if Trim(FRawLicenseInput.value) = '' then
    raise EConvertError.Create('license declaration is required');
  if Trim(FRawSourceInput.value) = '' then
    raise EConvertError.Create('source description is required');
  LText := FRawTextInput.value;
  if LText = '' then
    raise EConvertError.Create('raw text sample is required');
  if Length(LText) > RAW_TEXT_STORAGE_LIMIT then
    raise EConvertError.CreateFmt(
      'raw text exceeds the browser storage limit [%d > %d]',
      [Length(LText), RAW_TEXT_STORAGE_LIMIT]);
  LOrder := ReadBoundedInteger(FRawOrderInput, 'sequence order', 1, 64);
  LMetadata := MakeWfcTrainingMetadata(FRawNameInput.value,
    FRawLicenseInput.value, FRawSourceInput.value);
  SetLength(LSamples, 1);
  LSamples[0] := MakeWfcTextTrainingSample(
    FRawSampleNameInput.value, LText);
  LDocument := BuildWfcTextTrainingDocument(LMetadata, LSamples, LOrder);
  try
    LSample := LDocument.SampleAt(0);
    if LSample.Width > FLimits.MaxOutputCells then
      raise EWfcTrainingWorkspace.CreateFmt(
        'raw sample exceeds the interactive output limit [%d > %d scalars]',
        [LSample.Width, FLimits.MaxOutputCells]);
    ApplySourceText(EncodeWfcTrainingText(LDocument));
    LOptions := DefaultWfcTrainingSolveOptions;
    LOptions.Width := LSample.Width;
    LOptions.Height := 1;
    WriteOptions(LOptions);
  finally
    LDocument.Free;
  end;
  SetState('source-dirty', 'Raw text converted; train to continue.',
    'Metadata and the explicit sample boundary are preserved in wfclearn text.');
end;

procedure TBrowserTrainingStudioApplication.DiscardSourceForImportError;
begin
  FLocks := nil;
  FVocabulary := nil;
  FSelectedCell := -1;
  FSourceInput.value := '';
  FWorkspace.SetSourceText('');
  RefreshAll;
end;

procedure TBrowserTrainingStudioApplication.CancelSourceFileRead;
var
  LReader: TJSFileReader;
begin
  LReader := FFileReader;
  FFileReader := nil;
  if Assigned(LReader) and (LReader.readyState = TJSFileReader.LOADING) then
    LReader.abort;
end;

procedure TBrowserTrainingStudioApplication.CommitSourceFileText(
  const AReader: TJSFileReader; const AText: String);
begin
  if (not Assigned(AReader)) or (AReader <> FFileReader) then Exit;
  FFileReader := nil;
  ApplySourceText(AText);
end;

procedure TBrowserTrainingStudioApplication.BeginSourceFileRead;
var
  LFile: TJSHTMLFile;
begin
  if (not Assigned(FSourceFileInput.files)) or
      (FSourceFileInput.files.length = 0) then Exit;
  CancelSourceFileRead;
  { Accepting a new import selection immediately invalidates the previous
    source lineage. A slow or failed read cannot leave it looking current. }
  DiscardSourceForImportError;
  SetState('source-dirty', 'Reading selected source file…',
    'The previous source and all of its derived artifacts are cleared.');
  LFile := FSourceFileInput.files[0];
  { The retained File object remains readable after the picker is reset. This
    lets a cancelled import select the same path again and receive change. }
  FSourceFileInput.value := '';
  if LFile.size > FLimits.MaxSourceTextLength then
  begin
    raise EWfcTrainingWorkspace.CreateFmt(
      'source file exceeds the interactive limit [%d > %d bytes]',
      [LFile.size, FLimits.MaxSourceTextLength]);
  end;
  FFileReader := TJSFileReader.new;
  FFileReader.onload := @HandleSourceFileLoaded;
  FFileReader.onerror := @HandleSourceFileError;
  FFileReader.readAsText(LFile);
end;

procedure TBrowserTrainingStudioApplication.AssertTest(
  const ACondition: Boolean; const AMessage: String);
begin
  if not ACondition then
    raise EWfcTrainingWorkspace.Create('self-test: ' + AMessage);
end;

procedure TBrowserTrainingStudioApplication.DispatchDomEvent(
  const AElement: TJSElement; const AEventName: String);
begin
  AElement.dispatchEvent(TJSEvent.new(AEventName));
end;

procedure TBrowserTrainingStudioApplication.RunSelfTest;
var
  I: Integer;
  LBaselineRecipe: String;
  LBaselineResult: String;
  LResultText: String;
  LStaleReader: TJSFileReader;
  LTokenIndex: Integer;
  LTokens: TWfcModelTokens;
begin
  document.body.setAttribute('data-self-test', 'pending');
  document.body.removeAttribute('data-self-test-message');
  document.body.setAttribute('data-source-invalidation', 'pending');
  document.body.setAttribute('data-run-invalidation', 'pending');
  document.body.setAttribute('data-contradiction', 'pending');
  document.body.setAttribute('data-recovery', 'pending');
  document.body.setAttribute('data-preset-sweep', 'pending');
  document.body.setAttribute('data-import-race', 'pending');
  try
    AssertTest(FWorkspace.HasResult and
      (FWorkspace.ResultStatus = wprsSolved),
      'initial preset did not solve');
    AssertTest(FWorkspace.TrainingSignatureText = BASELINE_SOURCE_SIGNATURE,
      'initial source signature changed');
    AssertTest(FWorkspace.RecipeSignatureText = BASELINE_RECIPE_SIGNATURE,
      'initial recipe signature changed');
    AssertTest(FWorkspace.ResultSignatureText = BASELINE_RESULT_SIGNATURE,
      'initial result signature changed');
    LBaselineRecipe := FWorkspace.RecipeSignatureText;
    LBaselineResult := FWorkspace.ResultSignatureText;
    LTokens := FWorkspace.OutputTokens;
    AssertTest(Length(LTokens) = 16, 'initial output cell count changed');
    for I := 0 to Length(LTokens) - 1 do
      if ((I mod 4 + I div 4) mod 2) = 0 then
        AssertTest(LTokens[I] = 'A', 'initial even checker cell changed')
      else
        AssertTest(LTokens[I] = 'B', 'initial odd checker cell changed');

    for I := 0 to TRAINING_STUDIO_PRESET_COUNT - 1 do
    begin
      FPresetSelect.value := IntToStr(I);
      DispatchDomEvent(FLoadPresetButton, 'click');
      DispatchDomEvent(FTrainButton, 'click');
      DispatchDomEvent(FSolveButton, 'click');
      AssertTest(FWorkspace.HasResult and
        (FWorkspace.ResultStatus = wprsSolved),
        'preset ' + IntToStr(I) + ' did not solve through browser events');
    end;
    document.body.setAttribute('data-preset-sweep', 'passed');
    FPresetSelect.value := IntToStr(INITIAL_PRESET);
    DispatchDomEvent(FLoadPresetButton, 'click');
    DispatchDomEvent(FTrainButton, 'click');
    DispatchDomEvent(FSolveButton, 'click');
    AssertTest(FWorkspace.ResultSignatureText = LBaselineResult,
      'preset sweep did not restore the baseline');

    FArtifactSelect.value := 'result';
    DispatchDomEvent(FArtifactSelect, 'change');
    AssertTest(FArtifactOutput.value <> '', 'result artifact was not rendered');
    FSourceInput.value := FSourceInput.value + #10;
    DispatchDomEvent(FSourceInput, 'input');
    AssertTest((not FWorkspace.HasRecipe) and (not FWorkspace.HasRun) and
      (not FWorkspace.HasResult), 'source edit retained derived artifacts');
    AssertTest((FArtifactOutput.value = '') and
      (document.body.getAttribute('data-cell-count') = '0'),
      'source edit retained stale output');
    document.body.setAttribute('data-source-invalidation', 'passed');

    FPresetSelect.value := IntToStr(INITIAL_PRESET);
    DispatchDomEvent(FLoadPresetButton, 'click');
    DispatchDomEvent(FTrainButton, 'click');
    DispatchDomEvent(FSolveButton, 'click');
    AssertTest(FWorkspace.HasResult and
      (FWorkspace.ResultSignatureText = LBaselineResult),
      'baseline replay after source edit changed');

    FArtifactSelect.value := 'result';
    DispatchDomEvent(FArtifactSelect, 'change');
    FSeedInput.value := '1';
    DispatchDomEvent(FSeedInput, 'input');
    AssertTest(FWorkspace.HasRecipe and (not FWorkspace.HasRun) and
      (not FWorkspace.HasResult), 'run edit invalidation scope changed');
    AssertTest(FWorkspace.RecipeSignatureText = LBaselineRecipe,
      'run edit changed recipe');
    AssertTest((FArtifactOutput.value = '') and
      (document.body.getAttribute('data-cell-count') = '0'),
      'run edit retained stale result output');
    document.body.setAttribute('data-run-invalidation', 'passed');
    FSeedInput.value := '0';
    DispatchDomEvent(FSeedInput, 'input');

    LStaleReader := TJSFileReader.new;
    FFileReader := TJSFileReader.new;
    CommitSourceFileText(LStaleReader, 'obsolete asynchronous import');
    AssertTest(FWorkspace.SourceText = TrainingStudioPresetText(INITIAL_PRESET),
      'stale file callback replaced the newer source');
    CancelSourceFileRead;
    document.body.setAttribute('data-import-race', 'passed');

    LTokenIndex := FindVocabularyToken('B');
    AssertTest(LTokenIndex >= 0, 'public token B is unavailable');
    FLockTokenSelect.value := IntToStr(LTokenIndex);
    FLockXInput.value := '0';
    FLockYInput.value := '0';
    DispatchDomEvent(FAddLockButton, 'click');
    AssertTest((Length(FLocks) = 1) and (FLocks[0].Token = 'B'),
      'nonfirst vocabulary selection changed while adding a lock');
    DispatchDomEvent(FSolveButton, 'click');
    LTokens := FWorkspace.OutputTokens;
    AssertTest((Length(LTokens) = 16) and (LTokens[0] = 'B'),
      'selected B lock was not enforced in public output');
    DispatchDomEvent(FClearLocksButton, 'click');

    LTokenIndex := FindVocabularyToken('A');
    AssertTest(LTokenIndex >= 0, 'public token A is unavailable');
    FLockTokenSelect.value := IntToStr(LTokenIndex);
    FLockXInput.value := '0';
    FLockYInput.value := '0';
    DispatchDomEvent(FAddLockButton, 'click');
    FLockTokenSelect.value := IntToStr(LTokenIndex);
    FLockXInput.value := '1';
    FLockYInput.value := '0';
    DispatchDomEvent(FAddLockButton, 'click');
    DispatchDomEvent(FSolveButton, 'click');
    AssertTest(FWorkspace.HasResult and
      (FWorkspace.ResultStatus = wprsContradiction),
      'adjacent equal locks did not contradict');
    LResultText := FWorkspace.ResultText;
    AssertTest((LResultText <> '') and
      (Length(FWorkspace.OutputTokens) = 0),
      'contradiction exposed partial output or no terminal artifact');
    document.body.setAttribute('data-contradiction', 'passed');

    FLockList.selectedIndex := 1;
    DispatchDomEvent(FRemoveLockButton, 'click');
    DispatchDomEvent(FSolveButton, 'click');
    AssertTest(FWorkspace.HasResult and
      (FWorkspace.ResultStatus = wprsSolved),
      'removing contradictory lock did not recover');
    DispatchDomEvent(FClearLocksButton, 'click');
    DispatchDomEvent(FSolveButton, 'click');
    AssertTest(FWorkspace.HasResult and
      (FWorkspace.ResultStatus = wprsSolved) and
      (FWorkspace.TrainingSignatureText = BASELINE_SOURCE_SIGNATURE) and
      (FWorkspace.RecipeSignatureText = BASELINE_RECIPE_SIGNATURE) and
      (FWorkspace.ResultSignatureText = BASELINE_RESULT_SIGNATURE),
      'clearing locks did not restore exact baseline');
    AssertTest((Length(FLocks) = 0) and
      (document.body.getAttribute('data-cell-count') = '16'),
      'final baseline state has stale locks or cells');
    document.body.setAttribute('data-recovery', 'passed');
    document.body.setAttribute('data-self-test', 'passed');
  except
    on E: Exception do
    begin
      document.body.setAttribute('data-self-test', 'failed');
      document.body.setAttribute('data-self-test-message', E.Message);
      ShowError(E.Message);
    end;
  end;
end;

function TBrowserTrainingStudioApplication.HandleLoadPreset(
  AEvent: TJSMouseEvent): Boolean;
var
  LIndex: Integer;
begin
  Result := False;
  try
    if not TryStrToInt(FPresetSelect.value, LIndex) then
      raise EConvertError.Create('select a known training preset');
    LoadPreset(LIndex);
  except
    on E: Exception do ShowError(E.Message);
  end;
end;

function TBrowserTrainingStudioApplication.HandleSourceInput(
  AEvent: TJSEvent): Boolean;
begin
  Result := False;
  try
    ApplySourceText(FSourceInput.value);
  except
    on E: Exception do ShowError(E.Message);
  end;
end;

function TBrowserTrainingStudioApplication.HandleSourceFile(
  AEvent: TJSEvent): Boolean;
begin
  Result := False;
  try
    BeginSourceFileRead;
  except
    on E: Exception do ShowError(E.Message);
  end;
end;

function TBrowserTrainingStudioApplication.HandleSourceFileLoaded(
  AEvent: TJSEvent): Boolean;
var
  LReader: TJSFileReader;
begin
  Result := False;
  try
    LReader := TJSFileReader(AEvent.target);
    if (not Assigned(LReader)) or (LReader <> FFileReader) then Exit;
    CommitSourceFileText(LReader, String(LReader.Result));
    FSourceFileInput.value := '';
    SetState('source-dirty', 'Source file imported; train to continue.',
      'The imported text is now the editable current source.');
  except
    on E: Exception do ShowError(E.Message);
  end;
end;

function TBrowserTrainingStudioApplication.HandleSourceFileError(
  AEvent: TJSEvent): Boolean;
var
  LReader: TJSFileReader;
begin
  Result := False;
  try
    LReader := TJSFileReader(AEvent.target);
    if (not Assigned(LReader)) or (LReader <> FFileReader) then Exit;
    FFileReader := nil;
    FSourceFileInput.value := '';
    DiscardSourceForImportError;
    raise EWfcTrainingWorkspace.Create('the selected source file could not be read');
  except
    on E: Exception do ShowError(E.Message);
  end;
end;

function TBrowserTrainingStudioApplication.HandleTrain(
  AEvent: TJSMouseEvent): Boolean;
begin
  Result := False;
  try
    TrainWorkspace;
  except
    on E: Exception do ShowError(E.Message);
  end;
end;

function TBrowserTrainingStudioApplication.HandleConvertRaw(
  AEvent: TJSMouseEvent): Boolean;
begin
  Result := False;
  try
    ConvertRawText;
  except
    on E: Exception do ShowError(E.Message);
  end;
end;

function TBrowserTrainingStudioApplication.HandleRunInput(
  AEvent: TJSEvent): Boolean;
begin
  Result := False;
  try
    InvalidateRun('Reconfigure and solve when the edited inputs are ready.');
  except
    on E: Exception do ShowError(E.Message);
  end;
end;

function TBrowserTrainingStudioApplication.HandleSolve(
  AEvent: TJSMouseEvent): Boolean;
begin
  Result := False;
  try
    SolveWorkspace;
  except
    on E: Exception do ShowError(E.Message);
  end;
end;

function TBrowserTrainingStudioApplication.HandleAddLock(
  AEvent: TJSMouseEvent): Boolean;
var
  I: Integer;
  LOptions: TWfcTrainingSolveOptions;
  LTokenIndex: Integer;
  LX: Integer;
  LY: Integer;
begin
  Result := False;
  try
    FWorkspace.ClearRun;
    FSelectedCell := -1;
    if not FWorkspace.HasRecipe then
      raise EWfcTrainingWorkspace.Create('train the current source first');
    LOptions := ReadOptions;
    LX := ReadLockCoordinate(FLockXInput, 'lock x', LOptions.Width);
    LY := ReadLockCoordinate(FLockYInput, 'lock y', LOptions.Height);
    LTokenIndex := SelectedVocabularyIndex;
    AddOrReplaceLock(LX, LY, FVocabulary[LTokenIndex]);
    RefreshAll;
    for I := 0 to Length(FLocks) - 1 do
      if (FLocks[I].X = LX) and (FLocks[I].Y = LY) and
          (FLocks[I].Z = 0) then
      begin
        FLockList.selectedIndex := I;
        Break;
      end;
    SetState('run-dirty', 'Public lock updated; old result cleared.',
      'Configure and solve to apply the current sorted lock set.');
  except
    on E: Exception do ShowError(E.Message);
  end;
end;

function TBrowserTrainingStudioApplication.HandleRemoveLock(
  AEvent: TJSMouseEvent): Boolean;
var
  I: Integer;
  LIndex: Integer;
begin
  Result := False;
  try
    FWorkspace.ClearRun;
    FSelectedCell := -1;
    LIndex := FLockList.selectedIndex;
    if (LIndex < 0) or (LIndex >= Length(FLocks)) then
      raise ERangeError.Create('select a public lock to remove');
    for I := LIndex to Length(FLocks) - 2 do
      FLocks[I] := FLocks[I + 1];
    SetLength(FLocks, Length(FLocks) - 1);
    RefreshAll;
    SetState('run-dirty', 'Public lock removed; old result cleared.',
      'Configure and solve to apply the remaining locks.');
  except
    on E: Exception do ShowError(E.Message);
  end;
end;

function TBrowserTrainingStudioApplication.HandleClearLocks(
  AEvent: TJSMouseEvent): Boolean;
begin
  Result := False;
  try
    FWorkspace.ClearRun;
    FSelectedCell := -1;
    FLocks := nil;
    RefreshAll;
    SetState('run-dirty', 'Public locks cleared; old result cleared.',
      'Configure and solve for an unconstrained exact replay.');
  except
    on E: Exception do ShowError(E.Message);
  end;
end;

function TBrowserTrainingStudioApplication.HandleOutputClick(
  AEvent: TJSMouseEvent): Boolean;
var
  LElement: TJSElement;
  LPrevious: TJSElement;
begin
  Result := False;
  try
    LElement := TJSElement(AEvent.currentTarget);
    if not TryStrToInt(LElement.getAttribute('data-index'), FSelectedCell) then
      raise EConvertError.Create('selected output cell index is invalid');
    FLockXInput.value := LElement.getAttribute('data-x');
    FLockYInput.value := LElement.getAttribute('data-y');
    LPrevious := document.querySelector('.output-cell.selected');
    if Assigned(LPrevious) then LPrevious.className := 'output-cell';
    LElement.className := 'output-cell selected';
    FStatusDetailElement.textContent := 'Selected public cell x=' +
      FLockXInput.value + ', y=' + FLockYInput.value + '.';
  except
    on E: Exception do ShowError(E.Message);
  end;
end;

function TBrowserTrainingStudioApplication.HandleArtifactChange(
  AEvent: TJSEvent): Boolean;
begin
  Result := False;
  try
    RefreshArtifact;
  except
    on E: Exception do ShowError(E.Message);
  end;
end;

procedure TBrowserTrainingStudioApplication.Run;
begin
  try
    BindDocument;
    PopulatePresets;
    BindEvents;
    LoadPreset(INITIAL_PRESET);
    TrainWorkspace;
    SolveWorkspace;
    if Pos('selftest=1', window.location.search) > 0 then
      RunSelfTest
    else
      document.body.setAttribute('data-self-test', 'not-requested');
  except
    on E: Exception do ShowError(E.Message);
  end;
end;

end.
