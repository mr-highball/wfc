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
  TTrainingConnectivityRow = record
    Value: TWfcModelToken;
    Participant, Required: TJSHTMLInputElement;
    Ports: array[TGraphDirection] of TJSHTMLInputElement;
  end;

  TBrowserTrainingStudioApplication = class
  strict private
    FWorkspace: TWfcTrainingWorkspace;
    FLimits: TWfcTrainingWorkspaceLimits;
    FLocks: TWfcPipelineCellLocks;
    FVocabulary: TWfcModelTokens;
    FConfiguredOptions: TWfcTrainingSolveOptions;
    FConfiguredDepth: Integer;
    FSelectedCell: Integer;
    FFileReader: TJSFileReader;
    FSourceDownloadUrl: String;
    FArtifactDownloadUrl: String;
    FQuotaDraftDirty: Boolean;
    FEditingQuotaIndex: Integer;
    FConnectivityDraftDirty: Boolean;
    FEditingConnectivityIndex: Integer;
    FConnectivityProfileOrder: TWfcModelTokens;
    FConnectivityRows: array of TTrainingConnectivityRow;
    FConnectivityLabelInput: TJSHTMLInputElement;
    FConnectivityRoot: array[0..2] of TJSHTMLInputElement;
    FConnectivityTerminals: TJSHTMLTextAreaElement;
    FConnectivityAll: TJSHTMLInputElement;
    FConnectivityProfiles: TJSElement;
    FConnectivityList: TJSHTMLSelectElement;
    FConnectivityApplyButton, FConnectivityNewButton,
      FConnectivityRemoveButton, FConnectivityClearButton,
      FConnectivityDiscardButton, FConnectivityDemoButton: TJSHTMLButtonElement;
    FConnectivityStatus: TJSElement;

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
    FDepthInput: TJSHTMLInputElement;
    FSeedInput: TJSHTMLInputElement;
    FStrategySelect: TJSHTMLSelectElement;
    FBacktracksInput: TJSHTMLInputElement;
    FPassBacktracksInput: TJSHTMLInputElement;
    FTraceInput: TJSHTMLInputElement;

    FLockXInput: TJSHTMLInputElement;
    FLockYInput: TJSHTMLInputElement;
    FLockZInput: TJSHTMLInputElement;
    FLockTokenSelect: TJSHTMLSelectElement;
    FAddLockButton: TJSHTMLButtonElement;
    FLockList: TJSHTMLSelectElement;
    FRemoveLockButton: TJSHTMLButtonElement;
    FClearLocksButton: TJSHTMLButtonElement;

    FQuotaLabelInput: TJSHTMLInputElement;
    FQuotaTokensSelect: TJSHTMLSelectElement;
    FQuotaMinimumInput: TJSHTMLInputElement;
    FQuotaMaximumInput: TJSHTMLInputElement;
    FQuotaList: TJSHTMLSelectElement;
    FQuotaApplyButton: TJSHTMLButtonElement;
    FQuotaNewButton: TJSHTMLButtonElement;
    FQuotaRemoveButton: TJSHTMLButtonElement;
    FQuotaClearButton: TJSHTMLButtonElement;
    FQuotaDiscardButton: TJSHTMLButtonElement;
    FQuotaStatusElement: TJSElement;

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
    procedure WriteOptions(const AOptions: TWfcTrainingSolveOptions;
      const ADepth: Integer);
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
    procedure AddOrReplaceLock(const AX, AY, AZ: Integer;
      const AToken: TWfcModelToken);
    procedure SortLocks;
    procedure ReloadQuotaEditor;
    procedure LoadQuotaFields(const AIndex: Integer);
    procedure BeginQuotaDraft;
    procedure CommitValueQuotas(const AQuotas: TWfcTrainingValueQuotas);
    procedure ApplyQuotaDraft;
    procedure RefreshQuotaState;
    procedure RunQuotaSelfTest;
    function PolicyDraftDirty: Boolean;
    procedure RequireNoPolicyDraft;
    procedure ReloadConnectivityEditor;
    procedure LoadConnectivityFields(const AIndex: Integer);
    procedure BeginConnectivityDraft;
    procedure ApplyConnectivityDraft;
    procedure CommitConnectivities(const AValues: TWfcTrainingConnectivities);
    procedure RefreshConnectivityState;
    procedure RunConnectivitySelfTest;

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
    function HandleQuotaInput(AEvent: TJSEvent): Boolean;
    function HandleQuotaSelect(AEvent: TJSEvent): Boolean;
    function HandleQuotaApply(AEvent: TJSMouseEvent): Boolean;
    function HandleQuotaNew(AEvent: TJSMouseEvent): Boolean;
    function HandleQuotaRemove(AEvent: TJSMouseEvent): Boolean;
    function HandleQuotaClear(AEvent: TJSMouseEvent): Boolean;
    function HandleQuotaDiscard(AEvent: TJSMouseEvent): Boolean;
    function HandleConnectivityInput(AEvent: TJSEvent): Boolean;
    function HandleConnectivitySelect(AEvent: TJSEvent): Boolean;
    function HandleConnectivityApply(AEvent: TJSMouseEvent): Boolean;
    function HandleConnectivityNew(AEvent: TJSMouseEvent): Boolean;
    function HandleConnectivityRemove(AEvent: TJSMouseEvent): Boolean;
    function HandleConnectivityClear(AEvent: TJSMouseEvent): Boolean;
    function HandleConnectivityDiscard(AEvent: TJSMouseEvent): Boolean;
    function HandleConnectivityDemo(AEvent: TJSMouseEvent): Boolean;
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
  training_studio_presets,
  training_studio_connectivity;

const
  MAX_SEED = Cardinal($FFFFFFFF);
  INITIAL_PRESET = 2;
  VOLUME_PRESET = 5;
  RAW_TEXT_STORAGE_LIMIT = 2048;
  BASELINE_SOURCE_SIGNATURE = '0FA2C5EA';
  BASELINE_RECIPE_SIGNATURE = 'DBCBA621';
  BASELINE_RESULT_SIGNATURE = '947C4AFD';
  VOLUME_SOURCE_SIGNATURE = 'C6E52736';
  VOLUME_RECIPE_SIGNATURE = '4B8C29E4';
  VOLUME_RESULT_SIGNATURE = 'CBDC737A';

constructor TBrowserTrainingStudioApplication.Create;
begin
  inherited Create;
  FWorkspace := TWfcTrainingWorkspace.Create(
    InteractiveWfcTrainingWorkspaceLimits);
  FLimits := FWorkspace.CopyLimits;
  FLocks := nil;
  FVocabulary := nil;
  FConfiguredOptions := DefaultWfcTrainingSolveOptions;
  FConfiguredDepth := 1;
  FSelectedCell := -1;
  FFileReader := nil;
  FSourceDownloadUrl := '';
  FArtifactDownloadUrl := '';
  FQuotaDraftDirty := False;
  FEditingQuotaIndex := -1;
  FEditingConnectivityIndex := -1;
  FConnectivityDraftDirty := False;
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
  FDepthInput := TJSHTMLInputElement(RequireElement('depth-input'));
  FSeedInput := TJSHTMLInputElement(RequireElement('seed-input'));
  FStrategySelect := TJSHTMLSelectElement(RequireElement('strategy-select'));
  FBacktracksInput := TJSHTMLInputElement(RequireElement('backtracks-input'));
  FPassBacktracksInput := TJSHTMLInputElement(
    RequireElement('pass-backtracks-input'));
  FTraceInput := TJSHTMLInputElement(RequireElement('trace-input'));

  FLockXInput := TJSHTMLInputElement(RequireElement('lock-x-input'));
  FLockYInput := TJSHTMLInputElement(RequireElement('lock-y-input'));
  FLockZInput := TJSHTMLInputElement(RequireElement('lock-z-input'));
  FLockTokenSelect := TJSHTMLSelectElement(
    RequireElement('lock-token-select'));
  FAddLockButton := TJSHTMLButtonElement(RequireElement('add-lock-button'));
  FLockList := TJSHTMLSelectElement(RequireElement('lock-list'));
  FRemoveLockButton := TJSHTMLButtonElement(
    RequireElement('remove-lock-button'));
  FClearLocksButton := TJSHTMLButtonElement(
    RequireElement('clear-locks-button'));

  FQuotaLabelInput := TJSHTMLInputElement(RequireElement('quota-label-input'));
  FQuotaTokensSelect := TJSHTMLSelectElement(RequireElement('quota-tokens-select'));
  FQuotaMinimumInput := TJSHTMLInputElement(RequireElement('quota-minimum-input'));
  FQuotaMaximumInput := TJSHTMLInputElement(RequireElement('quota-maximum-input'));
  FQuotaList := TJSHTMLSelectElement(RequireElement('quota-list'));
  FQuotaApplyButton := TJSHTMLButtonElement(RequireElement('quota-apply-button'));
  FQuotaNewButton := TJSHTMLButtonElement(RequireElement('quota-new-button'));
  FQuotaRemoveButton := TJSHTMLButtonElement(RequireElement('quota-remove-button'));
  FQuotaClearButton := TJSHTMLButtonElement(RequireElement('quota-clear-button'));
  FQuotaDiscardButton := TJSHTMLButtonElement(RequireElement('quota-discard-button'));
  FQuotaStatusElement := RequireElement('quota-status');
  FConnectivityLabelInput := TJSHTMLInputElement(RequireElement('connectivity-label'));
  FConnectivityRoot[0] := TJSHTMLInputElement(RequireElement('connectivity-root-x'));
  FConnectivityRoot[1] := TJSHTMLInputElement(RequireElement('connectivity-root-y'));
  FConnectivityRoot[2] := TJSHTMLInputElement(RequireElement('connectivity-root-z'));
  FConnectivityTerminals := TJSHTMLTextAreaElement(RequireElement('connectivity-terminals'));
  FConnectivityAll := TJSHTMLInputElement(RequireElement('connectivity-all'));
  FConnectivityProfiles := RequireElement('connectivity-profiles');
  FConnectivityList := TJSHTMLSelectElement(RequireElement('connectivity-list'));
  FConnectivityApplyButton := TJSHTMLButtonElement(RequireElement('connectivity-apply'));
  FConnectivityNewButton := TJSHTMLButtonElement(RequireElement('connectivity-new'));
  FConnectivityRemoveButton := TJSHTMLButtonElement(RequireElement('connectivity-remove'));
  FConnectivityClearButton := TJSHTMLButtonElement(RequireElement('connectivity-clear'));
  FConnectivityDiscardButton := TJSHTMLButtonElement(RequireElement('connectivity-discard'));
  FConnectivityDemoButton := TJSHTMLButtonElement(RequireElement('connectivity-demo'));
  FConnectivityStatus := RequireElement('connectivity-status');

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
  FDepthInput.oninput := @HandleRunInput;
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
  FQuotaLabelInput.oninput := @HandleQuotaInput;
  FQuotaMinimumInput.oninput := @HandleQuotaInput;
  FQuotaMaximumInput.oninput := @HandleQuotaInput;
  FQuotaTokensSelect.onchange := @HandleQuotaInput;
  FQuotaList.onchange := @HandleQuotaSelect;
  FQuotaApplyButton.onclick := @HandleQuotaApply;
  FQuotaNewButton.onclick := @HandleQuotaNew;
  FQuotaRemoveButton.onclick := @HandleQuotaRemove;
  FQuotaClearButton.onclick := @HandleQuotaClear;
  FQuotaDiscardButton.onclick := @HandleQuotaDiscard;
  FConnectivityLabelInput.oninput := @HandleConnectivityInput;
  FConnectivityRoot[0].oninput := @HandleConnectivityInput;
  FConnectivityRoot[1].oninput := @HandleConnectivityInput;
  FConnectivityRoot[2].oninput := @HandleConnectivityInput;
  FConnectivityTerminals.oninput := @HandleConnectivityInput;
  FConnectivityAll.onchange := @HandleConnectivityInput;
  FConnectivityList.onchange := @HandleConnectivitySelect;
  FConnectivityApplyButton.onclick := @HandleConnectivityApply;
  FConnectivityNewButton.onclick := @HandleConnectivityNew;
  FConnectivityRemoveButton.onclick := @HandleConnectivityRemove;
  FConnectivityClearButton.onclick := @HandleConnectivityClear;
  FConnectivityDiscardButton.onclick := @HandleConnectivityDiscard;
  FConnectivityDemoButton.onclick := @HandleConnectivityDemo;
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
  const AOptions: TWfcTrainingSolveOptions; const ADepth: Integer);
begin
  FWidthInput.value := IntToStr(AOptions.Width);
  FHeightInput.value := IntToStr(AOptions.Height);
  FDepthInput.value := IntToStr(ADepth);
  FLockZInput.value := '0';
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
  RequireNoPolicyDraft;
  FPresetSelect.value := IntToStr(AIndex);
  WriteOptions(TrainingStudioPresetOptions(AIndex),
    TrainingStudioPresetDepth(AIndex));
  ApplySourceText(TrainingStudioPresetText(AIndex));
  SetState('source-dirty', 'Preset loaded; train to continue.',
    TrainingStudioPresetName(AIndex) + ' is the current editable source.');
end;

procedure TBrowserTrainingStudioApplication.ApplySourceText(
  const AText: String);
begin
  RequireNoPolicyDraft;
  CancelSourceFileRead;
  FQuotaDraftDirty := False;
  FLocks := nil;
  FVocabulary := nil;
  FSelectedCell := -1;
  FSourceInput.value := AText;
  FWorkspace.SetSourceText(AText);
  ReloadQuotaEditor;
  ReloadConnectivityEditor;
  RefreshAll;
  SetState('source-dirty', 'Source changed; derived artifacts cleared.',
    'Train the current source before configuring another run.');
end;

procedure TBrowserTrainingStudioApplication.TrainWorkspace;
begin
  RequireNoPolicyDraft;
  CancelSourceFileRead;
  FLocks := nil;
  FSelectedCell := -1;
  FWorkspace.SetSourceText(FSourceInput.value);
  FWorkspace.Train;
  ReloadQuotaEditor;
  ReloadConnectivityEditor;
  RefreshAll;
  SetState('trained', 'Recipe trained.',
    'The model and recipe are current; configure and solve a bounded run.');
end;

procedure TBrowserTrainingStudioApplication.SolveWorkspace;
var
  LDepth: Integer;
  LOptions: TWfcTrainingSolveOptions;
  LStatus: TWfcPipelineResultStatus;
begin
  RequireNoPolicyDraft;
  { Clearing first is deliberate: malformed edited options cannot leave an
    older run or result looking current. }
  FWorkspace.ClearRun;
  FSelectedCell := -1;
  RefreshAll;
  LOptions := ReadOptions;
  LDepth := ReadBoundedInteger(FDepthInput, 'depth', 1,
    FLimits.MaxOutputCells);
  if FWorkspace.Rank = 3 then
    FWorkspace.ConfigureVolumeRun(LOptions, LDepth, FLocks, nil)
  else
  begin
    if LDepth <> 1 then
      raise EConvertError.Create(
        'depth must be 1 unless the trained recipe has rank 3');
    FWorkspace.ConfigureRun(LOptions, FLocks, nil);
  end;
  FConfiguredOptions := LOptions;
  FConfiguredDepth := LDepth;
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
  const AX, AY, AZ: Integer; const AToken: TWfcModelToken);
var
  I: Integer;
begin
  for I := 0 to Length(FLocks) - 1 do
    if (FLocks[I].X = AX) and (FLocks[I].Y = AY) and
        (FLocks[I].Z = AZ) then
    begin
      FLocks[I] := MakeWfcPipelineCellLock(FWorkspace.PublicPassIndex,
        AX, AY, AZ, AToken);
      SortLocks;
      Exit;
    end;
  SetLength(FLocks, Length(FLocks) + 1);
  FLocks[High(FLocks)] := MakeWfcPipelineCellLock(
    FWorkspace.PublicPassIndex, AX, AY, AZ, AToken);
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

procedure TBrowserTrainingStudioApplication.LoadQuotaFields(const AIndex: Integer);
var I, J: Integer; Q: TWfcTrainingValueQuota;
begin
  FEditingQuotaIndex := AIndex;
  FQuotaList.selectedIndex := AIndex;
  FQuotaLabelInput.value := '';
  FQuotaMinimumInput.value := '0';
  FQuotaMaximumInput.value := '0';
  for I := 0 to FQuotaTokensSelect.options.length - 1 do
    TJSHTMLOptionElement(FQuotaTokensSelect.options[I]).selected := False;
  if AIndex < 0 then Exit;
  Q := FWorkspace.CopyValueQuotas[AIndex];
  FQuotaLabelInput.value := String(Q.LabelText);
  FQuotaMinimumInput.value := IntToStr(Q.MinimumCount);
  FQuotaMaximumInput.value := IntToStr(Q.MaximumCount);
  for I := 0 to Length(FVocabulary) - 1 do
    for J := 0 to Length(Q.Values) - 1 do
      if FVocabulary[I] = Q.Values[J] then
        TJSHTMLOptionElement(FQuotaTokensSelect.options[I]).selected := True;
end;

procedure TBrowserTrainingStudioApplication.ReloadQuotaEditor;
var I, J: Integer; LOption: TJSHTMLOptionElement;
  Q: TWfcTrainingValueQuotas; LText: String;
begin
  FEditingQuotaIndex := -1;
  FQuotaList.textContent := '';
  FQuotaTokensSelect.textContent := '';
  if FWorkspace.HasRecipe then
  begin
    FVocabulary := FWorkspace.PublicVocabulary;
    for I := 0 to Length(FVocabulary) - 1 do
    begin
      LOption := TJSHTMLOptionElement(document.createElement('option'));
      LOption.value := IntToStr(I);
      LOption.textContent := DisplayToken(FVocabulary[I]);
      FQuotaTokensSelect.appendChild(LOption);
    end;
    Q := FWorkspace.CopyValueQuotas;
    for I := 0 to Length(Q) - 1 do
    begin
      LText := DisplayToken(Q[I].LabelText) + ' : ' +
        IntToStr(Q[I].MinimumCount) + '..' + IntToStr(Q[I].MaximumCount) + ' {';
      for J := 0 to Length(Q[I].Values) - 1 do
      begin
        if J > 0 then LText := LText + ', ';
        LText := LText + DisplayToken(Q[I].Values[J]);
      end;
      LOption := TJSHTMLOptionElement(document.createElement('option'));
      LOption.value := IntToStr(I);
      LOption.textContent := LText + '}';
      FQuotaList.appendChild(LOption);
    end;
  end;
  LoadQuotaFields(-1);
end;

function TBrowserTrainingStudioApplication.PolicyDraftDirty: Boolean;
begin
  Result := FQuotaDraftDirty or FConnectivityDraftDirty;
end;

procedure TBrowserTrainingStudioApplication.RequireNoPolicyDraft;
begin
  if PolicyDraftDirty then
    raise EWfcTrainingWorkspace.Create('apply or discard the policy draft before this action');
end;

procedure TBrowserTrainingStudioApplication.LoadConnectivityFields(const AIndex: Integer);
var I, J: Integer; D: TGraphDirection; C: TWfcTrainingConnectivity;
begin
  FEditingConnectivityIndex := AIndex;
  FConnectivityList.selectedIndex := AIndex;
  FConnectivityLabelInput.value := '';
  for I := 0 to 2 do FConnectivityRoot[I].value := '0';
  FConnectivityTerminals.value := '';
  FConnectivityAll.checked := False;
  FConnectivityProfileOrder := nil;
  for I := 0 to High(FConnectivityRows) do
  begin
    FConnectivityRows[I].Participant.checked := False;
    FConnectivityRows[I].Required.checked := False;
    for D := Low(TGraphDirection) to High(TGraphDirection) do
      FConnectivityRows[I].Ports[D].checked := False;
  end;
  if AIndex < 0 then Exit;
  C := FWorkspace.CopyConnectivities[AIndex];
  FConnectivityLabelInput.value := String(C.LabelText);
  FConnectivityRoot[0].value := IntToStr(C.Root.X);
  FConnectivityRoot[1].value := IntToStr(C.Root.Y);
  FConnectivityRoot[2].value := IntToStr(C.Root.Z);
  FConnectivityAll.checked := C.RequireAllParticipants;
  for I := 0 to High(C.RequiredPositions) do
  begin
    if I > 0 then FConnectivityTerminals.value := FConnectivityTerminals.value + #10;
    FConnectivityTerminals.value := FConnectivityTerminals.value +
      IntToStr(C.RequiredPositions[I].X) + ',' + IntToStr(C.RequiredPositions[I].Y) +
      ',' + IntToStr(C.RequiredPositions[I].Z);
  end;
  SetLength(FConnectivityProfileOrder, Length(C.Values));
  for I := 0 to High(C.Values) do
  begin
    FConnectivityProfileOrder[I] := C.Values[I].Value;
    for J := 0 to High(FConnectivityRows) do
      if FConnectivityRows[J].Value = C.Values[I].Value then
      begin
        FConnectivityRows[J].Participant.checked := True;
        FConnectivityRows[J].Required.checked := C.Values[I].RequiredByValue;
        for D := Low(TGraphDirection) to High(TGraphDirection) do
          FConnectivityRows[J].Ports[D].checked := D in C.Values[I].Openings;
      end;
  end;
end;

procedure TBrowserTrainingStudioApplication.ReloadConnectivityEditor;
const Names: array[TGraphDirection] of String = ('North', 'East', 'South', 'West', 'Up', 'Down');
var I: Integer; D: TGraphDirection; Row, Cell: TJSElement;
  Option: TJSHTMLOptionElement; C: TWfcTrainingConnectivities;
  function AddCheck(const ADescription: String): TJSHTMLInputElement;
  begin
    Cell := document.createElement('td');
    Result := TJSHTMLInputElement(document.createElement('input'));
    Result.setAttribute('type', 'checkbox');
    Result.setAttribute('aria-label', ADescription);
    Result.onchange := @HandleConnectivityInput;
    Cell.appendChild(Result); Row.appendChild(Cell);
  end;
begin
  FConnectivityProfiles.textContent := '';
  FConnectivityList.textContent := '';
  FConnectivityRows := nil;
  if FWorkspace.HasRecipe then
  begin
    FVocabulary := FWorkspace.PublicVocabulary;
    SetLength(FConnectivityRows, Length(FVocabulary));
    for I := 0 to High(FVocabulary) do
    begin
      Row := document.createElement('tr');
      Cell := document.createElement('th');
      Cell.textContent := DisplayToken(FVocabulary[I]); Row.appendChild(Cell);
      FConnectivityRows[I].Value := FVocabulary[I];
      FConnectivityRows[I].Participant := AddCheck(DisplayToken(FVocabulary[I]) + ' participates');
      for D := Low(TGraphDirection) to High(TGraphDirection) do
        FConnectivityRows[I].Ports[D] := AddCheck(DisplayToken(FVocabulary[I]) + ' ' + Names[D]);
      FConnectivityRows[I].Required := AddCheck(DisplayToken(FVocabulary[I]) + ' required by value');
      FConnectivityProfiles.appendChild(Row);
    end;
    C := FWorkspace.CopyConnectivities;
    for I := 0 to High(C) do
    begin
      Option := TJSHTMLOptionElement(document.createElement('option'));
      Option.value := IntToStr(I);
      Option.textContent := DisplayToken(C[I].LabelText) + ' : root ' +
        IntToStr(C[I].Root.X) + ',' + IntToStr(C[I].Root.Y) + ',' +
        IntToStr(C[I].Root.Z) + ' · ' + IntToStr(Length(C[I].Values)) +
        ' profiles · ' + IntToStr(Length(C[I].RequiredPositions)) + ' terminals';
      FConnectivityList.appendChild(Option);
    end;
  end;
  LoadConnectivityFields(-1);
end;

procedure TBrowserTrainingStudioApplication.BeginConnectivityDraft;
begin
  if FQuotaDraftDirty then
    raise EWfcTrainingWorkspace.Create('apply or discard the quota draft before editing connectivity');
  CancelSourceFileRead;
  if not FWorkspace.HasRecipe then
    raise EWfcTrainingWorkspace.Create('train the current source before editing connectivity');
  FConnectivityDraftDirty := True;
  FWorkspace.ClearRun;
  FSelectedCell := -1;
  RefreshAll;
  SetState('connectivity-dirty', 'Connectivity draft changed; apply or discard it.',
    'No old output or derived download is current. Ports are explicit, never inferred.');
end;

procedure TBrowserTrainingStudioApplication.CommitConnectivities(
  const AValues: TWfcTrainingConnectivities);
begin
  BeginConnectivityDraft;
  try
    FWorkspace.ReplaceConnectivities(AValues);
    FConnectivityDraftDirty := False;
    ReloadQuotaEditor;
    ReloadConnectivityEditor;
  finally
    FSourceInput.value := FWorkspace.SourceText;
    RefreshAll;
  end;
  SetState('trained', 'Connectivity saved in the training source; recipe rebuilt.',
    'Configure and solve again. Saved quotas are retained.');
end;

procedure TBrowserTrainingStudioApplication.ApplyConnectivityDraft;
var C: TWfcTrainingConnectivities; V: TWfcTrainingConnectivityValues;
  P: TGraphPosition; T: TGraphPositions; I, J, N: Integer; Found: Boolean;
  procedure AppendRow(const Index: Integer);
  var D: TGraphDirection; Ports: TGraphDirections;
  begin
    if not FConnectivityRows[Index].Participant.checked then Exit;
    Ports := [];
    for D := Low(TGraphDirection) to High(TGraphDirection) do
      if FConnectivityRows[Index].Ports[D].checked then Include(Ports, D);
    N := Length(V); SetLength(V, N + 1);
    V[N] := MakeWfcTrainingConnectivityValue(FConnectivityRows[Index].Value,
      Ports, FConnectivityRows[Index].Required.checked);
  end;
begin
  BeginConnectivityDraft;
  P.X := WfcTextParseCanonicalInteger(FConnectivityRoot[0].value, 'root X', 'studio');
  P.Y := WfcTextParseCanonicalInteger(FConnectivityRoot[1].value, 'root Y', 'studio');
  P.Z := WfcTextParseCanonicalInteger(FConnectivityRoot[2].value, 'root Z', 'studio');
  T := ParseTrainingStudioTerminals(FConnectivityTerminals.value);
  V := nil;
  { Keep existing authored order even when the learner's vocabulary changes.
    New participants append in current public order; displayed escapes never
    become token values. No participation, port or required flag is inferred. }
  for I := 0 to High(FConnectivityProfileOrder) do
    for J := 0 to High(FConnectivityRows) do
      if FConnectivityRows[J].Value = FConnectivityProfileOrder[I] then AppendRow(J);
  for I := 0 to High(FConnectivityRows) do
  begin
    Found := False;
    for J := 0 to High(FConnectivityProfileOrder) do
      if FConnectivityRows[I].Value = FConnectivityProfileOrder[J] then Found := True;
    if not Found then AppendRow(I);
  end;
  C := FWorkspace.CopyConnectivities;
  I := FEditingConnectivityIndex;
  if I < 0 then begin I := Length(C); SetLength(C, I + 1); end
  else if I >= Length(C) then
    raise EWfcTrainingWorkspace.Create('selected network is stale');
  C[I] := MakeWfcTrainingConnectivity(FConnectivityLabelInput.value, P, T, V,
    FConnectivityAll.checked);
  CommitConnectivities(C);
end;

procedure TBrowserTrainingStudioApplication.RefreshConnectivityState;
var Available: Boolean; Count, I: Integer; D: TGraphDirection;
begin
  Available := FWorkspace.HasRecipe and not FQuotaDraftDirty;
  Count := 0;
  if FWorkspace.HasRecipe then Count := FWorkspace.ConnectivityCount
  else begin FConnectivityList.textContent := ''; FEditingConnectivityIndex := -1; end;
  FConnectivityLabelInput.disabled := not Available;
  for I := 0 to 2 do FConnectivityRoot[I].disabled := not Available;
  FConnectivityTerminals.disabled := not Available;
  FConnectivityAll.disabled := not Available;
  for I := 0 to High(FConnectivityRows) do
  begin
    FConnectivityRows[I].Participant.disabled := not Available;
    FConnectivityRows[I].Required.disabled := not Available;
    for D := Low(TGraphDirection) to High(TGraphDirection) do
      FConnectivityRows[I].Ports[D].disabled := not Available;
  end;
  FConnectivityApplyButton.disabled := not Available;
  if FEditingConnectivityIndex < 0 then FConnectivityApplyButton.textContent := 'Apply new network'
  else FConnectivityApplyButton.textContent := 'Apply selected network';
  FConnectivityNewButton.disabled := not Available or PolicyDraftDirty;
  FConnectivityList.disabled := not Available or PolicyDraftDirty;
  FConnectivityRemoveButton.disabled := not Available or PolicyDraftDirty or
    (FEditingConnectivityIndex < 0);
  FConnectivityClearButton.disabled := not Available or PolicyDraftDirty or (Count = 0);
  FConnectivityDiscardButton.disabled := not FConnectivityDraftDirty;
  FConnectivityDemoButton.disabled := PolicyDraftDirty;
  FSourceInput.disabled := PolicyDraftDirty;
  FLoadPresetButton.disabled := PolicyDraftDirty;
  FSourceFileInput.disabled := PolicyDraftDirty;
  FConvertRawButton.disabled := PolicyDraftDirty;
  document.body.setAttribute('data-connectivity-count', IntToStr(Count));
  document.body.setAttribute('data-connectivity-draft',
    LowerCase(BoolToStr(FConnectivityDraftDirty, True)));
  if FConnectivityDraftDirty then
    FConnectivityStatus.textContent := 'Unapplied network draft. Apply or discard before training, solving or exporting. If rebuilding failed, discard and train the retained source again.'
  else
    FConnectivityStatus.textContent := IntToStr(Count) +
      ' saved networks. Root and terminals are absolute XYZ positions; resizing never moves them.';
end;

procedure TBrowserTrainingStudioApplication.BeginQuotaDraft;
begin
  if FConnectivityDraftDirty then
    raise EWfcTrainingWorkspace.Create('apply or discard the connectivity draft before editing quotas');
  CancelSourceFileRead;
  if not FWorkspace.HasRecipe then
    raise EWfcTrainingWorkspace.Create('train the current source before editing quotas');
  FQuotaDraftDirty := True;
  FWorkspace.ClearRun;
  FSelectedCell := -1;
  RefreshAll;
  SetState('quota-dirty', 'Quota draft changed; apply or discard it.',
    'No old output or derived download is current. Source download waits for the draft too.');
end;

procedure TBrowserTrainingStudioApplication.CommitValueQuotas(
  const AQuotas: TWfcTrainingValueQuotas);
begin
  BeginQuotaDraft;
  try
    FWorkspace.ReplaceValueQuotas(AQuotas);
    FQuotaDraftDirty := False;
    ReloadQuotaEditor;
    ReloadConnectivityEditor;
  finally
    { Successful edits are canonical source changes. On a failed rebuild the
      retained source is the only recoverable artifact, never an old run. }
    FSourceInput.value := FWorkspace.SourceText;
    RefreshAll;
  end;
  SetState('trained', 'Quotas saved in the training source; recipe rebuilt.',
    'Configure and solve again. Download source to preserve these exact hard bounds.');
end;

procedure TBrowserTrainingStudioApplication.ApplyQuotaDraft;
var Q: TWfcTrainingValueQuotas; V: TWfcModelTokens;
  I, N, LMinimum, LMaximum: Integer; LQuota: TWfcTrainingValueQuota;
begin
  BeginQuotaDraft;
  LMinimum := WfcTextParseCanonicalInteger(FQuotaMinimumInput.value,
    'quota minimum', 'training studio');
  LMaximum := WfcTextParseCanonicalInteger(FQuotaMaximumInput.value,
    'quota maximum', 'training studio');
  V := nil;
  { DOM option positions refer only to the current detached public vocabulary.
    Tokens, not their indices or displayed percent-escaped labels, are saved. }
  for I := 0 to FQuotaTokensSelect.options.length - 1 do
    if TJSHTMLOptionElement(FQuotaTokensSelect.options[I]).selected then
    begin
      if I >= Length(FVocabulary) then
        raise EWfcTrainingWorkspace.Create('quota vocabulary selection is stale');
      N := Length(V); SetLength(V, N + 1); V[N] := FVocabulary[I];
    end;
  LQuota := MakeWfcTrainingValueQuota(FQuotaLabelInput.value, V, LMinimum, LMaximum);
  Q := FWorkspace.CopyValueQuotas;
  if FEditingQuotaIndex >= 0 then
  begin
    if FEditingQuotaIndex >= Length(Q) then
      raise EWfcTrainingWorkspace.Create('selected quota is stale');
    Q[FEditingQuotaIndex] := LQuota;
  end
  else
  begin
    N := Length(Q); SetLength(Q, N + 1); Q[N] := LQuota;
  end;
  CommitValueQuotas(Q);
end;

procedure TBrowserTrainingStudioApplication.RefreshQuotaState;
var LHasRecipe, Available: Boolean; LCount: Integer;
begin
  LHasRecipe := FWorkspace.HasRecipe;
  Available := LHasRecipe and not FConnectivityDraftDirty;
  LCount := 0;
  if LHasRecipe then LCount := FWorkspace.ValueQuotaCount
  else
  begin
    FQuotaList.textContent := '';
    FEditingQuotaIndex := -1;
  end;
  FQuotaLabelInput.disabled := not Available;
  FQuotaTokensSelect.disabled := not Available;
  FQuotaMinimumInput.disabled := not Available;
  FQuotaMaximumInput.disabled := not Available;
  FQuotaApplyButton.disabled := not Available;
  if FEditingQuotaIndex < 0 then FQuotaApplyButton.textContent := 'Apply new quota'
  else FQuotaApplyButton.textContent := 'Apply selected quota';
  FQuotaNewButton.disabled := (not Available) or PolicyDraftDirty;
  FQuotaList.disabled := (not Available) or PolicyDraftDirty;
  FQuotaRemoveButton.disabled := (not Available) or PolicyDraftDirty or
    (FEditingQuotaIndex < 0);
  FQuotaClearButton.disabled := (not Available) or PolicyDraftDirty or (LCount = 0);
  FQuotaDiscardButton.disabled := not FQuotaDraftDirty;
  FSolveButton.disabled := (not FWorkspace.HasRecipe) or PolicyDraftDirty;
  FTrainButton.disabled := PolicyDraftDirty;
  document.body.setAttribute('data-quota-count', IntToStr(LCount));
  document.body.setAttribute('data-quota-draft', LowerCase(BoolToStr(FQuotaDraftDirty, True)));
  if FQuotaDraftDirty then
    FQuotaStatusElement.textContent := 'Unapplied draft. Apply or discard before solving or downloading. ' +
      'If rebuilding failed, discard the draft and train the retained source again.'
  else if not LHasRecipe then
    FQuotaStatusElement.textContent := 'Train the current source to choose public tokens.'
  else
    FQuotaStatusElement.textContent := IntToStr(LCount) +
      ' saved whole-output quotas. Bounds count the complete XYZ output once, not each row or slice.';
end;

procedure TBrowserTrainingStudioApplication.SetState(
  const AState, AStatus, ADetail: String);
begin
  if FConnectivityDraftDirty and (AState = 'run-dirty') then
  begin
    document.body.setAttribute('data-state', 'connectivity-dirty');
    FStatusElement.textContent := 'Run edited; connectivity draft still needs apply or discard.';
    FStatusDetailElement.textContent := 'Resolve the network draft before configuring another solve.';
  end
  else if FQuotaDraftDirty and (AState = 'run-dirty') then
  begin
    document.body.setAttribute('data-state', 'quota-dirty');
    FStatusElement.textContent := 'Run edited; quota draft still needs apply or discard.';
    FStatusDetailElement.textContent := 'Both edits are pending. Resolve the quota draft before configuring another solve.';
  end
  else
  begin
    document.body.setAttribute('data-state', AState);
    FStatusElement.textContent := AStatus;
    FStatusDetailElement.textContent := ADetail;
  end;
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
  RefreshQuotaState;
  RefreshConnectivityState;
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
  LOutputDepth: Integer;
  LTokens: TWfcModelTokens;
begin
  LProfile := '';
  LResultStatus := 'none';
  LSourceSignature := '';
  LRecipeSignature := '';
  LResultSignature := '';
  LCellCount := 0;
  LOutputDepth := 0;
  LPasses := nil;

  if FWorkspace.HasRecipe then
  begin
    LOptions := FWorkspace.SourceOptions;
    LProfile := ProfileName(LOptions.Kind);
    LSourceSignature := FWorkspace.TrainingSignatureText;
    if not PolicyDraftDirty then LRecipeSignature := FWorkspace.RecipeSignatureText;
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
      LOutputDepth := FConfiguredDepth;
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
  document.body.setAttribute('data-output-depth', IntToStr(LOutputDepth));
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
      ' y=' + IntToStr(FLocks[I].Y) +
      ' z=' + IntToStr(FLocks[I].Z) + ' token=' +
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
  FOutputPlaceholder.removeAttribute('hidden');
  FSelectedCell := -1;
end;

procedure TBrowserTrainingStudioApplication.RefreshResult;
var
  I: Integer;
  LX: Integer;
  LY: Integer;
  LZ: Integer;
  LButton: TJSHTMLButtonElement;
  LHeading: TJSElement;
  LSlice: TJSElement;
  LSliceGrid: TJSElement;
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
  if Length(LTokens) <> FConfiguredOptions.Width *
      FConfiguredOptions.Height * FConfiguredDepth then
    raise EWfcTrainingWorkspace.Create(
      'terminal output shape does not match its configured volume');
  for LZ := 0 to FConfiguredDepth - 1 do
  begin
    LSlice := document.createElement('section');
    LSlice.className := 'output-slice';
    LSlice.setAttribute('data-z', IntToStr(LZ));
    LSlice.setAttribute('aria-label', 'Output slice Z ' + IntToStr(LZ));
    LHeading := document.createElement('h3');
    LHeading.textContent := 'Z = ' + IntToStr(LZ);
    LSlice.appendChild(LHeading);
    LSliceGrid := document.createElement('div');
    LSliceGrid.className := 'output-slice-grid';
    LSliceGrid.setAttribute('style', '--grid-columns:' +
      IntToStr(FConfiguredOptions.Width));
    LSliceGrid.setAttribute('role', 'rowgroup');
    for LY := 0 to FConfiguredOptions.Height - 1 do
      for LX := 0 to FConfiguredOptions.Width - 1 do
      begin
        I := (LZ * FConfiguredOptions.Height + LY) *
          FConfiguredOptions.Width + LX;
        LButton := TJSHTMLButtonElement(document.createElement('button'));
        LButton._type := 'button';
        LButton.id := 'output-cell-' + IntToStr(LX) + '-' +
          IntToStr(LY) + '-' + IntToStr(LZ);
        LButton.className := 'output-cell';
        LButton.textContent := DisplayToken(LTokens[I]);
        LButton.setAttribute('data-index', IntToStr(I));
        LButton.setAttribute('data-x', IntToStr(LX));
        LButton.setAttribute('data-y', IntToStr(LY));
        LButton.setAttribute('data-z', IntToStr(LZ));
        LButton.setAttribute('data-xyz', IntToStr(LX) + ',' +
          IntToStr(LY) + ',' + IntToStr(LZ));
        LButton.setAttribute('role', 'gridcell');
        LButton.setAttribute('aria-label', 'x ' + IntToStr(LX) +
          ', y ' + IntToStr(LY) + ', z ' + IntToStr(LZ) +
          ', token ' + DisplayToken(LTokens[I]));
        LButton.onclick := @HandleOutputClick;
        LSliceGrid.appendChild(LButton);
      end;
    LSlice.appendChild(LSliceGrid);
    FOutputGrid.appendChild(LSlice);
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
  if PolicyDraftDirty then
    SetDownloadLink(FSourceDownloadLink, '', 'training-source.wfclearn', FSourceDownloadUrl)
  else SetDownloadLink(FSourceDownloadLink, FWorkspace.SourceText,
    'training-source.wfclearn', FSourceDownloadUrl);
end;

function TBrowserTrainingStudioApplication.SelectedArtifactText: String;
begin
  Result := '';
  if PolicyDraftDirty then Exit;
  if FArtifactSelect.value = 'source' then
    Result := FWorkspace.SourceText
  else if FArtifactSelect.value = 'model' then
  begin
    if FWorkspace.HasRecipe and (FWorkspace.ValueQuotaCount = 0) and
        (FWorkspace.ConnectivityCount = 0) then
      Result := FWorkspace.ModelText;
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
  if PolicyDraftDirty then
    FArtifactStatus.textContent := 'Apply or discard the policy draft before exporting.'
  else if (FArtifactSelect.value = 'model') and FWorkspace.HasRecipe and
      ((FWorkspace.ValueQuotaCount > 0) or (FWorkspace.ConnectivityCount > 0)) then
    FArtifactStatus.textContent := 'Standalone models cannot retain authored quotas or connectivity. Download the source or pipeline recipe instead.'
  else if LText = '' then
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
    wtkAdjacency3D: Result := 'adjacency3d';
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
    gckValueQuota: Result := 'value-quota';
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
  RequireNoPolicyDraft;
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
    WriteOptions(LOptions, 1);
  finally
    LDocument.Free;
  end;
  SetState('source-dirty', 'Raw text converted; train to continue.',
    'Metadata and the explicit sample boundary are preserved in wfclearn text.');
end;

procedure TBrowserTrainingStudioApplication.DiscardSourceForImportError;
begin
  RequireNoPolicyDraft;
  FQuotaDraftDirty := False;
  FLocks := nil;
  FVocabulary := nil;
  FSelectedCell := -1;
  FSourceInput.value := '';
  FWorkspace.SetSourceText('');
  ReloadQuotaEditor;
  ReloadConnectivityEditor;
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
  RequireNoPolicyDraft;
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

procedure TBrowserTrainingStudioApplication.RunQuotaSelfTest;
var LCafe: TWfcModelToken; S, P, R, V: String;
  LTokens: TWfcModelTokens; I, LCount: Integer; LStaleReader: TJSFileReader;

  procedure SelectPreset(const AIndex: Integer);
  begin
    FPresetSelect.value := IntToStr(AIndex);
    DispatchDomEvent(FLoadPresetButton, 'click');
    DispatchDomEvent(FTrainButton, 'click');
    DispatchDomEvent(FSolveButton, 'click');
    AssertTest(FWorkspace.HasResult and (FWorkspace.ResultStatus = wprsSolved),
      'quota fixture preset did not solve');
  end;

  procedure Draft(const ALabel, AToken: TWfcModelToken;
    const AMinimum, AMaximum: String);
  var J: Integer;
  begin
    FQuotaLabelInput.value := String(ALabel);
    FQuotaMinimumInput.value := AMinimum;
    FQuotaMaximumInput.value := AMaximum;
    for J := 0 to Length(FVocabulary) - 1 do
      TJSHTMLOptionElement(FQuotaTokensSelect.options[J]).selected := FVocabulary[J] = AToken;
    DispatchDomEvent(FQuotaLabelInput, 'input');
  end;

  procedure ApplyAndSolve;
  begin
    DispatchDomEvent(FQuotaApplyButton, 'click');
    AssertTest(not FQuotaDraftDirty and FWorkspace.HasRecipe,
      'quota edit failed to publish a current recipe');
    AssertTest(FSourceInput.value = FWorkspace.SourceText,
      'applied quota source textarea is stale');
    DispatchDomEvent(FSolveButton, 'click');
    AssertTest(FWorkspace.HasResult, 'quota solve did not produce a terminal result');
  end;
begin
  document.body.setAttribute('data-quota-edit', 'pending');
  document.body.setAttribute('data-quota-replay', 'pending');
  document.body.setAttribute('data-quota-contradiction', 'pending');
  document.body.setAttribute('data-quota-invalidation', 'pending');
  document.body.setAttribute('data-quota-volume', 'pending');
  LCafe := 'caf' + Chr($E9);
  SelectPreset(3);
  AssertTest(FWorkspace.OutputTokens[0] = 'red', 'seed-zero phrase baseline changed');
  FArtifactSelect.value := 'result';
  DispatchDomEvent(FArtifactSelect, 'change');
  LStaleReader := TJSFileReader.new;
  FFileReader := LStaleReader;
  Draft('prefer ' + LCafe, LCafe, '1', '1');
  CommitSourceFileText(LStaleReader, 'obsolete quota-era import');
  AssertTest((FWorkspace.SourceText = TrainingStudioPresetText(3)) and
    (FFileReader = nil), 'quota editing did not cancel stale source reads');
  AssertTest(FQuotaDraftDirty and FSolveButton.disabled and FTrainButton.disabled and
    not FWorkspace.HasRun and not FWorkspace.HasResult and
    (FArtifactOutput.value = '') and (not FArtifactDownloadLink.hasAttribute('href')) and
    (not FSourceDownloadLink.hasAttribute('href')) and
    (document.body.getAttribute('data-recipe-signature') = ''),
    'quota draft exposed stale solving, signatures or downloads');
  FArtifactSelect.value := 'recipe'; DispatchDomEvent(FArtifactSelect, 'change');
  AssertTest(FArtifactOutput.value = '', 'quota draft exposed old recipe export');
  DispatchDomEvent(FSeedInput, 'input');
  AssertTest((document.body.getAttribute('data-state') = 'quota-dirty') and
    FSolveButton.disabled and FQuotaDraftDirty,
    'editing a run field hid the unresolved quota draft');
  DispatchDomEvent(FSolveButton, 'click');
  AssertTest(not FWorkspace.HasResult, 'programmatic solve bypassed draft guard');
  ApplyAndSolve;
  LTokens := FWorkspace.OutputTokens;
  AssertTest((FWorkspace.ResultStatus = wprsSolved) and (Length(LTokens) = 3) and
    (LTokens[0] = LCafe) and (LTokens[1] = 'fox') and (LTokens[2] = '.'),
    'public Unicode quota did not guide private sequence state selection');
  AssertTest((FWorkspace.ValueQuotaCount = 1) and
    (Pos('wfclearn=3'#10, FWorkspace.SourceText) = 1) and
    (Pos('caf%C3%A9', FWorkspace.SourceText) > 0) and
    (Pos('wfcpipeline=2'#10, FWorkspace.RecipeText) = 1),
    'quota source/recipe did not retain canonical versioned Unicode policy');
  document.body.setAttribute('data-quota-edit', 'passed');
  S := FWorkspace.SourceText; P := FWorkspace.RecipeText;
  R := FWorkspace.RunText; V := FWorkspace.ResultText;
  FArtifactSelect.value := 'model'; DispatchDomEvent(FArtifactSelect, 'change');
  AssertTest((FArtifactOutput.value = '') and (not FArtifactDownloadLink.hasAttribute('href')) and
    (Pos('cannot retain', FArtifactStatus.textContent) > 0), 'model-only export silently lost quotas');
  FArtifactSelect.value := 'source'; DispatchDomEvent(FArtifactSelect, 'change');
  AssertTest((FArtifactOutput.value = S) and FArtifactDownloadLink.hasAttribute('href'),
    'canonical quota source cannot be saved');
  FSourceInput.value := S; DispatchDomEvent(FSourceInput, 'input');
  AssertTest(not FWorkspace.HasRecipe, 'reimport did not invalidate its old lineage');
  DispatchDomEvent(FTrainButton, 'click'); DispatchDomEvent(FSolveButton, 'click');
  AssertTest((FWorkspace.SourceText = S) and (FWorkspace.RecipeText = P) and
    (FWorkspace.RunText = R) and (FWorkspace.ResultText = V) and
    (FQuotaList.options.length = 1), 'saved quota source did not retrain and replay exactly');
  document.body.setAttribute('data-quota-replay', 'passed');

  { An additional multi-token quota counts set membership once, not one
    contribution per accepted token. It remains conjunctive with cafe=1. }
  Draft('opening alternatives', 'red', '1', '1');
  for I := 0 to Length(FVocabulary) - 1 do
    if FVocabulary[I] = LCafe then
      TJSHTMLOptionElement(FQuotaTokensSelect.options[I]).selected := True;
  DispatchDomEvent(FQuotaTokensSelect, 'change');
  ApplyAndSolve;
  AssertTest((FWorkspace.ValueQuotaCount = 2) and (FWorkspace.OutputTokens[0] = LCafe),
    'multi-token quota did not retain conjunctive public semantics');

  FQuotaList.selectedIndex := 0; DispatchDomEvent(FQuotaList, 'change');
  FQuotaMinimumInput.value := '1.5'; DispatchDomEvent(FQuotaMinimumInput, 'input');
  DispatchDomEvent(FQuotaApplyButton, 'click');
  AssertTest(FQuotaDraftDirty and not FWorkspace.HasResult and FSolveButton.disabled,
    'invalid numeric draft exposed old result');
  DispatchDomEvent(FQuotaDiscardButton, 'click');
  AssertTest(not FQuotaDraftDirty and not FWorkspace.HasRun and not FWorkspace.HasResult,
    'discarding draft resurrected an old invocation');
  { Valid numbers but reversed bounds enter the shared destructive editor
    mutation: only the retained source remains after rejection. }
  FQuotaList.selectedIndex := 0; DispatchDomEvent(FQuotaList, 'change');
  Draft('invalid bounds', LCafe, '2', '1');
  DispatchDomEvent(FQuotaApplyButton, 'click');
  AssertTest(FQuotaDraftDirty and not FWorkspace.HasRecipe and
    not FWorkspace.HasRun and not FWorkspace.HasResult and
    (FQuotaList.options.length = 0) and
    (FSourceInput.value = FWorkspace.SourceText), 'failed rebuild kept stale derived artifacts');
  DispatchDomEvent(FQuotaDiscardButton, 'click');
  DispatchDomEvent(FTrainButton, 'click'); DispatchDomEvent(FSolveButton, 'click');
  AssertTest(FWorkspace.HasResult and (FWorkspace.OutputTokens[0] = LCafe),
    'discard/retrain could not recover the retained quota source');
  document.body.setAttribute('data-quota-invalidation', 'passed');

  SelectPreset(INITIAL_PRESET);
  Draft('half A', 'A', '8', '8'); ApplyAndSolve;
  AssertTest(FWorkspace.ResultStatus = wprsSolved, 'legal pattern quota did not solve');
  LTokens := FWorkspace.OutputTokens; LCount := 0;
  for I := 0 to Length(LTokens) - 1 do if LTokens[I] = 'A' then Inc(LCount);
  AssertTest(LCount = 8, 'pattern quota count mismatch');
  FQuotaList.selectedIndex := 0; DispatchDomEvent(FQuotaList, 'change');
  Draft('half A', 'A', '7', '7'); ApplyAndSolve;
  AssertTest((FWorkspace.ResultStatus = wprsContradiction) and
    (Length(FWorkspace.OutputTokens) = 0) and
    (document.querySelectorAll('#output-grid .output-cell').length = 0),
    'impossible pattern quota showed old output');
  AssertTest(ContradictionName(gckValueQuota) = 'value-quota', 'quota diagnostic label is missing');
  FQuotaList.selectedIndex := 0; DispatchDomEvent(FQuotaList, 'change');
  DispatchDomEvent(FQuotaRemoveButton, 'click'); DispatchDomEvent(FSolveButton, 'click');
  AssertTest((FWorkspace.ValueQuotaCount = 0) and
    (FWorkspace.ResultSignatureText = BASELINE_RESULT_SIGNATURE),
    'removing final quota did not restore old version-one replay');
  document.body.setAttribute('data-quota-contradiction', 'passed');

  SelectPreset(VOLUME_PRESET);
  Draft('whole volume A', 'A', '32', '32'); ApplyAndSolve;
  AssertTest((FWorkspace.ResultStatus = wprsSolved) and
    (Length(FWorkspace.OutputTokens) = 64), 'volume quota counted a slice instead of all XYZ cells');
  FDepthInput.value := '2'; DispatchDomEvent(FDepthInput, 'input');
  AssertTest(not FWorkspace.HasResult and
    (FWorkspace.CopyValueQuotas[0].MinimumCount = 32), 'shape edit changed absolute quota bounds');
  DispatchDomEvent(FSolveButton, 'click');
  AssertTest(FWorkspace.HasResult and (FWorkspace.ResultStatus = wprsContradiction) and
    (Length(FWorkspace.OutputTokens) = 0), 'smaller volume silently clamped its quota');
  DispatchDomEvent(FQuotaClearButton, 'click');
  FDepthInput.value := '4'; DispatchDomEvent(FDepthInput, 'input');
  DispatchDomEvent(FSolveButton, 'click');
  AssertTest(FWorkspace.ResultSignatureText = VOLUME_RESULT_SIGNATURE,
    'clear quotas did not restore unchanged volume replay');
  document.body.setAttribute('data-quota-volume', 'passed');
  SelectPreset(INITIAL_PRESET);
  AssertTest((FWorkspace.TrainingSignatureText = BASELINE_SOURCE_SIGNATURE) and
    (FWorkspace.RecipeSignatureText = BASELINE_RECIPE_SIGNATURE) and
    (FWorkspace.ResultSignatureText = BASELINE_RESULT_SIGNATURE) and
    (FWorkspace.ValueQuotaCount = 0) and not FQuotaDraftDirty,
    'quota self-test did not restore the untouched baseline');
end;

procedure TBrowserTrainingStudioApplication.RunConnectivitySelfTest;
var S, R: String; I: Integer; D: TGraphDirection; Reader: TJSFileReader;
  C: TWfcTrainingConnectivities;
  procedure SelectNetwork;
  begin
    FConnectivityList.selectedIndex := 0;
    DispatchDomEvent(FConnectivityList, 'change');
  end;
  procedure ApplyAndSolve;
  begin
    DispatchDomEvent(FConnectivityApplyButton, 'click');
    AssertTest(not FConnectivityDraftDirty and FWorkspace.HasRecipe,
      'network editor failed to save: ' + FStatusElement.textContent);
    DispatchDomEvent(FSolveButton, 'click');
    AssertTest(FWorkspace.HasResult, 'network solve has no terminal result');
  end;
begin
  document.body.setAttribute('data-connectivity-edit', 'pending');
  document.body.setAttribute('data-connectivity-replay', 'pending');
  document.body.setAttribute('data-connectivity-contradiction', 'pending');
  document.body.setAttribute('data-connectivity-invalidation', 'pending');
  document.body.setAttribute('data-connectivity-volume', 'pending');
  DispatchDomEvent(FConnectivityDemoButton, 'click');
  AssertTest((FWorkspace.ConnectivityCount = 1) and (FWorkspace.ValueQuotaCount = 1) and
    TrainingStudioRouteIsValid(FWorkspace.OutputTokens), 'route demonstration button');
  FArtifactSelect.value := 'model'; DispatchDomEvent(FArtifactSelect, 'change');
  AssertTest((FArtifactOutput.value = '') and
    (FArtifactDownloadLink.getAttribute('aria-disabled') = 'true'),
    'model export silently omitted network');
  FQuotaLabelInput.value := 'unsaved quota';
  DispatchDomEvent(FQuotaLabelInput, 'input');
  HandleConnectivityNew(nil); HandleConnectivityApply(nil); HandleConnectivityClear(nil);
  HandleLoadPreset(nil); HandleTrain(nil); HandleSolve(nil);
  AssertTest(FQuotaDraftDirty and (FQuotaLabelInput.value = 'unsaved quota') and
    not FConnectivityDraftDirty and (FWorkspace.ConnectivityCount = 1) and
    not FWorkspace.HasResult, 'opposite handlers lost a quota draft');
  DispatchDomEvent(FQuotaDiscardButton, 'click');
  SelectNetwork;
  Reader := TJSFileReader.new; FFileReader := Reader;
  FConnectivityRoot[0].value := '0.5';
  DispatchDomEvent(FConnectivityRoot[0], 'input');
  CommitSourceFileText(Reader, 'stale source');
  HandleQuotaNew(nil); HandleQuotaApply(nil); HandleQuotaClear(nil);
  HandleLoadPreset(nil); HandleTrain(nil); HandleSolve(nil);
  AssertTest(FConnectivityDraftDirty and not FQuotaDraftDirty and (FFileReader = nil) and
    not FWorkspace.HasRun and not FWorkspace.HasResult and FTrainButton.disabled and
    FSolveButton.disabled and (FArtifactOutput.value = '') and
    (FSourceDownloadLink.getAttribute('aria-disabled') = 'true') and
    (FConnectivityRoot[0].value = '0.5'), 'network draft stale action guards');
  FWidthInput.value := '4'; DispatchDomEvent(FWidthInput, 'input');
  AssertTest(document.body.getAttribute('data-state') = 'connectivity-dirty',
    'run edit hid a network draft');
  DispatchDomEvent(FConnectivityApplyButton, 'click');
  AssertTest(FConnectivityDraftDirty and not FWorkspace.HasResult,
    'fractional root silently accepted');
  FConnectivityRoot[0].value := '0';
  FConnectivityLabelInput.value := 'roads % / ' + String(WfcTextDecodeToken('caf%C3%A9', 'test'));
  FConnectivityTerminals.value := '3,2,0'#10'0,1,0';
  DispatchDomEvent(FConnectivityTerminals, 'input');
  ApplyAndSolve;
  AssertTest(TrainingStudioRouteIsValid(FWorkspace.OutputTokens) and
    (FWorkspace.CopyConnectivities[0].RequiredPositions[0].Y = 1),
    'network terminal sort or route validation');
  AssertTest(FWorkspace.CopyConnectivities[0].LabelText =
    'roads % / ' + WfcTextDecodeToken('caf%C3%A9', 'test'), 'network label escaped identity');
  document.body.setAttribute('data-connectivity-edit', 'passed');
  document.body.setAttribute('data-connectivity-invalidation', 'passed');
  S := FWorkspace.SourceText; R := FWorkspace.ResultText;
  AssertTest(Pos('wfclearn=4'#10, S) = 1, 'network source v4');
  ApplySourceText(S); DispatchDomEvent(FTrainButton, 'click');
  DispatchDomEvent(FSolveButton, 'click');
  AssertTest((FWorkspace.ResultText = R) and (FWorkspace.ValueQuotaCount = 1),
    'network source reload exact replay and quota coexistence');
  SelectNetwork;
  for I := 0 to High(FConnectivityRows) do
    FConnectivityRows[I].Participant.checked := False;
  DispatchDomEvent(FConnectivityAll, 'change');
  DispatchDomEvent(FConnectivityApplyButton, 'click');
  AssertTest(FConnectivityDraftDirty and not FWorkspace.HasRecipe and
    not FWorkspace.HasRun and not FWorkspace.HasResult and
    (FWorkspace.SourceText = S) and (FSourceInput.value = S) and
    (FConnectivityList.options.length = 0), 'failed network rebuild kept stale artifacts');
  DispatchDomEvent(FConnectivityDiscardButton, 'click');
  DispatchDomEvent(FTrainButton, 'click'); DispatchDomEvent(FSolveButton, 'click');
  AssertTest(not PolicyDraftDirty and (FWorkspace.ResultText = R),
    'failed network draft did not recover from retained source');
  document.body.setAttribute('data-connectivity-replay', 'passed');
  SelectNetwork;
  for I := 0 to High(FConnectivityRows) do
    if FConnectivityRows[I].Value = 'road' then
      for D := Low(TGraphDirection) to High(TGraphDirection) do
        FConnectivityRows[I].Ports[D].checked := D in [gdEast, gdWest];
  DispatchDomEvent(FConnectivityAll, 'change'); ApplyAndSolve;
  AssertTest((FWorkspace.ResultStatus = wprsContradiction) and
    (Length(FWorkspace.OutputTokens) = 0), 'missing planar ports did not contradict');
  SelectNetwork;
  for I := 0 to High(FConnectivityRows) do
    if FConnectivityRows[I].Value = 'road' then
      for D := Low(TGraphDirection) to High(TGraphDirection) do
        FConnectivityRows[I].Ports[D].checked := D in [gdNorth, gdEast, gdSouth, gdWest];
  DispatchDomEvent(FConnectivityAll, 'change'); ApplyAndSolve;
  AssertTest(FWorkspace.ResultText = R, 'port correction exact recovery');
  SelectNetwork; DispatchDomEvent(FConnectivityRemoveButton, 'click');
  AssertTest((FWorkspace.ConnectivityCount = 0) and (FWorkspace.ValueQuotaCount = 1) and
    (Pos('wfclearn=3'#10, FWorkspace.SourceText) = 1), 'network removal lost saved quota');
  DispatchDomEvent(FQuotaClearButton, 'click');
  AssertTest(FWorkspace.SourceText = TrainingStudioRouteSource, 'remove policies legacy restoration');
  document.body.setAttribute('data-connectivity-contradiction', 'passed');
  LoadPreset(VOLUME_PRESET); DispatchDomEvent(FTrainButton, 'click');
  FWidthInput.value := '2'; FHeightInput.value := '2'; FDepthInput.value := '2';
  C := TrainingStudioRouteNetwork;
  C[0].LabelText := 'XYZ column'; C[0].RequiredPositions[0] := TrainingStudioPosition(0, 0, 1);
  C[0].RequireAllParticipants := False;
  SetLength(C[0].Values, 2);
  C[0].Values[0] := MakeWfcTrainingConnectivityValue('B', [gdUp, gdDown], False);
  C[0].Values[1] := MakeWfcTrainingConnectivityValue('A', [gdUp, gdDown], False);
  { Imported authored order is deliberately opposite the learner's order. }
  CommitConnectivities(C); SelectNetwork;
  FConnectivityLabelInput.value := 'XYZ column saved';
  DispatchDomEvent(FConnectivityLabelInput, 'input'); ApplyAndSolve;
  AssertTest((FWorkspace.ResultStatus = wprsSolved) and (Length(FWorkspace.OutputTokens) = 8) and
    (FWorkspace.CopyConnectivities[0].Values[0].Value = 'B') and
    (FWorkspace.CopyConnectivities[0].Values[1].Value = 'A') and
    (FWorkspace.CopyConnectivities[0].RequiredPositions[0].Z = 1), 'XYZ and profile authored order');
  SelectNetwork; FConnectivityAll.checked := True;
  DispatchDomEvent(FConnectivityAll, 'change'); ApplyAndSolve;
  AssertTest(FWorkspace.ResultStatus = wprsContradiction, 'all-participant islands allowed');
  SelectNetwork; FConnectivityAll.checked := False;
  for I := 0 to High(FConnectivityRows) do
    if FConnectivityRows[I].Value = 'B' then FConnectivityRows[I].Required.checked := True;
  DispatchDomEvent(FConnectivityAll, 'change'); ApplyAndSolve;
  AssertTest(FWorkspace.ResultStatus = wprsContradiction, 'required-by-value islands allowed');
  DispatchDomEvent(FConnectivityClearButton, 'click');
  AssertTest(FWorkspace.SourceText = TrainingStudioPresetText(VOLUME_PRESET),
    'XYZ connectivity clear did not restore version two source');
  document.body.setAttribute('data-connectivity-volume', 'passed');
  LoadPreset(INITIAL_PRESET); DispatchDomEvent(FTrainButton, 'click');
  DispatchDomEvent(FSolveButton, 'click');
  AssertTest((FWorkspace.TrainingSignatureText = BASELINE_SOURCE_SIGNATURE) and
    (FWorkspace.RecipeSignatureText = BASELINE_RECIPE_SIGNATURE) and
    (FWorkspace.ResultSignatureText = BASELINE_RESULT_SIGNATURE) and
    not PolicyDraftDirty and (FWorkspace.ConnectivityCount = 0),
    'connectivity self-test failed to restore legacy baseline');
end;

procedure TBrowserTrainingStudioApplication.RunSelfTest;
var
  I: Integer;
  LBaselineRecipe: String;
  LBaselineResult: String;
  LCellElement: TJSElement;
  LNodes: TJSNodeList;
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
  document.body.setAttribute('data-volume-dimensions', 'pending');
  document.body.setAttribute('data-volume-lock', 'pending');
  document.body.setAttribute('data-volume-contradiction', 'pending');
  document.body.setAttribute('data-volume-recovery', 'pending');
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

    AssertTest((VOLUME_PRESET < TRAINING_STUDIO_PRESET_COUNT) and
      (TrainingStudioPresetDepth(VOLUME_PRESET) = 4),
      'volume preset depth contract changed');
    FPresetSelect.value := IntToStr(VOLUME_PRESET);
    DispatchDomEvent(FLoadPresetButton, 'click');
    DispatchDomEvent(FTrainButton, 'click');
    DispatchDomEvent(FSolveButton, 'click');
    LTokens := FWorkspace.OutputTokens;
    LNodes := document.querySelectorAll('#output-grid .output-slice');
    AssertTest((FWorkspace.Rank = 3) and FWorkspace.HasResult and
      (FWorkspace.ResultStatus = wprsSolved) and
      (FWorkspace.TrainingSignatureText = VOLUME_SOURCE_SIGNATURE) and
      (FWorkspace.RecipeSignatureText = VOLUME_RECIPE_SIGNATURE) and
      (FWorkspace.ResultSignatureText = VOLUME_RESULT_SIGNATURE) and
      (FDepthInput.value = '4') and (FConfiguredDepth = 4) and
      (Length(LTokens) = 64) and (LNodes.length = 4) and
      (document.querySelectorAll('#output-grid .output-cell').length = 64) and
      (document.body.getAttribute('data-output-depth') = '4') and
      Assigned(document.getElementById('output-cell-0-0-1')),
      'volume preset did not render four labeled, unique XYZ slices');
    document.body.setAttribute('data-volume-dimensions', 'passed');

    LTokenIndex := FindVocabularyToken('A');
    AssertTest(LTokenIndex >= 0, 'volume token A is unavailable');
    FLockTokenSelect.value := IntToStr(LTokenIndex);
    FLockXInput.value := '0';
    FLockYInput.value := '0';
    FLockZInput.value := '1';
    DispatchDomEvent(FAddLockButton, 'click');
    AssertTest((Length(FLocks) = 1) and (FLocks[0].X = 0) and
      (FLocks[0].Y = 0) and (FLocks[0].Z = 1) and
      (FLocks[0].Token = 'A'),
      'volume lock did not retain its nonzero Z coordinate');
    DispatchDomEvent(FSolveButton, 'click');
    LTokens := FWorkspace.OutputTokens;
    AssertTest((Length(LTokens) = 64) and (LTokens[16] = 'A'),
      'nonzero-Z public lock was not enforced');
    LCellElement := document.getElementById('output-cell-0-0-1');
    AssertTest(Assigned(LCellElement),
      'nonzero-Z output cell is missing from the DOM');
    DispatchDomEvent(LCellElement, 'click');
    AssertTest((FLockXInput.value = '0') and (FLockYInput.value = '0') and
      (FLockZInput.value = '1'),
      'output click did not copy all three coordinates');

    LTokenIndex := FindVocabularyToken('B');
    AssertTest(LTokenIndex >= 0, 'volume token B is unavailable');
    FLockTokenSelect.value := IntToStr(LTokenIndex);
    FLockXInput.value := '0';
    FLockYInput.value := '0';
    FLockZInput.value := '0';
    DispatchDomEvent(FAddLockButton, 'click');
    AssertTest((Length(FLocks) = 2) and (FLocks[0].Z = 0) and
      (FLocks[0].Token = 'B') and (FLocks[1].Z = 1) and
      (FLocks[1].Token = 'A'),
      'lock sorting did not preserve distinct Z coordinates');

    LTokenIndex := FindVocabularyToken('A');
    FLockTokenSelect.value := IntToStr(LTokenIndex);
    FLockXInput.value := '1';
    FLockYInput.value := '0';
    FLockZInput.value := '1';
    DispatchDomEvent(FAddLockButton, 'click');
    AssertTest((Length(FLocks) = 3) and (FLocks[2].Z = 1) and
      (FLocks[2].X = 1) and (FLocks[2].Token = 'A'),
      'adjacent volume lock was not retained after sorted insertion');
    document.body.setAttribute('data-volume-lock', 'passed');
    DispatchDomEvent(FSolveButton, 'click');
    AssertTest(FWorkspace.HasResult and
      (FWorkspace.ResultStatus = wprsContradiction) and
      (Length(FWorkspace.OutputTokens) = 0) and
      (document.querySelectorAll('#output-grid .output-cell').length = 0),
      'adjacent equal locks did not produce a clean volume contradiction');
    document.body.setAttribute('data-volume-contradiction', 'passed');

    FLockList.selectedIndex := 2;
    DispatchDomEvent(FRemoveLockButton, 'click');
    DispatchDomEvent(FSolveButton, 'click');
    LTokens := FWorkspace.OutputTokens;
    AssertTest(FWorkspace.HasResult and
      (FWorkspace.ResultStatus = wprsSolved) and
      (Length(LTokens) = 64) and (LTokens[16] = 'A') and
      (Length(FLocks) = 2) and (FLocks[0].Z = 0) and (FLocks[1].Z = 1),
      'removing the contradictory volume lock did not recover XYZ state');
    DispatchDomEvent(FClearLocksButton, 'click');
    DispatchDomEvent(FSolveButton, 'click');
    AssertTest(FWorkspace.HasResult and
      (FWorkspace.ResultStatus = wprsSolved) and
      (Length(FWorkspace.OutputTokens) = 64),
      'clearing volume locks did not restore an unconstrained volume');
    document.body.setAttribute('data-volume-recovery', 'passed');

    FPresetSelect.value := IntToStr(INITIAL_PRESET);
    DispatchDomEvent(FLoadPresetButton, 'click');
    DispatchDomEvent(FTrainButton, 'click');
    DispatchDomEvent(FSolveButton, 'click');
    AssertTest(FWorkspace.ResultSignatureText = LBaselineResult,
      'volume scenario did not restore the unchanged baseline');

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
    RunQuotaSelfTest;
    RunConnectivitySelfTest;
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

function TBrowserTrainingStudioApplication.HandleQuotaInput(AEvent: TJSEvent): Boolean;
begin
  Result := False;
  try BeginQuotaDraft; except on E: Exception do ShowError(E.Message); end;
end;

function TBrowserTrainingStudioApplication.HandleQuotaSelect(AEvent: TJSEvent): Boolean;
var LIndex: Integer;
begin
  Result := False;
  try
    if PolicyDraftDirty then
    begin
      FQuotaList.selectedIndex := FEditingQuotaIndex;
      raise EWfcTrainingWorkspace.Create('apply or discard the quota draft before selecting another');
    end;
    LIndex := FQuotaList.selectedIndex;
    if (LIndex < 0) or (LIndex >= FWorkspace.ValueQuotaCount) then Exit;
    LoadQuotaFields(LIndex);
    RefreshQuotaState;
  except on E: Exception do ShowError(E.Message); end;
end;

function TBrowserTrainingStudioApplication.HandleQuotaApply(AEvent: TJSMouseEvent): Boolean;
begin
  Result := False;
  try ApplyQuotaDraft; except on E: Exception do ShowError(E.Message); end;
end;

function TBrowserTrainingStudioApplication.HandleQuotaNew(AEvent: TJSMouseEvent): Boolean;
begin
  Result := False;
  try
    if PolicyDraftDirty then
      raise EWfcTrainingWorkspace.Create('apply or discard the quota draft before starting another');
    LoadQuotaFields(-1);
    RefreshQuotaState;
  except on E: Exception do ShowError(E.Message); end;
end;

function TBrowserTrainingStudioApplication.HandleQuotaRemove(AEvent: TJSMouseEvent): Boolean;
var Q: TWfcTrainingValueQuotas; I: Integer;
begin
  Result := False;
  try
    if PolicyDraftDirty then
      raise EWfcTrainingWorkspace.Create('apply or discard the quota draft before removing a saved quota');
    Q := FWorkspace.CopyValueQuotas;
    if (FEditingQuotaIndex < 0) or (FEditingQuotaIndex >= Length(Q)) then
      raise EWfcTrainingWorkspace.Create('select a saved quota to remove');
    for I := FEditingQuotaIndex to Length(Q) - 2 do Q[I] := Q[I + 1];
    SetLength(Q, Length(Q) - 1);
    CommitValueQuotas(Q);
  except on E: Exception do ShowError(E.Message); end;
end;

function TBrowserTrainingStudioApplication.HandleQuotaClear(AEvent: TJSMouseEvent): Boolean;
begin
  Result := False;
  try
    if PolicyDraftDirty then
      raise EWfcTrainingWorkspace.Create('apply or discard the quota draft before clearing saved quotas');
    CommitValueQuotas(nil);
  except on E: Exception do ShowError(E.Message); end;
end;

function TBrowserTrainingStudioApplication.HandleQuotaDiscard(AEvent: TJSMouseEvent): Boolean;
begin
  Result := False;
  try
    if FConnectivityDraftDirty then
      raise EWfcTrainingWorkspace.Create('discard the connectivity draft in its own editor');
    CancelSourceFileRead;
    FQuotaDraftDirty := False;
    ReloadQuotaEditor;
    RefreshAll;
    if FWorkspace.HasRecipe then
      SetState('trained', 'Quota draft discarded; saved source is unchanged.',
        'The old run and result stay cleared. Configure and solve again.')
    else
      SetState('source-dirty', 'Quota draft discarded; train the retained source.',
        'The failed rebuild left no recipe, run, or result.');
  except on E: Exception do ShowError(E.Message); end;
end;

function TBrowserTrainingStudioApplication.HandleConnectivityInput(AEvent: TJSEvent): Boolean;
begin
  Result := False;
  try BeginConnectivityDraft; except on E: Exception do ShowError(E.Message); end;
end;

function TBrowserTrainingStudioApplication.HandleConnectivitySelect(AEvent: TJSEvent): Boolean;
begin
  Result := False;
  try
    if PolicyDraftDirty then
    begin
      FConnectivityList.selectedIndex := FEditingConnectivityIndex;
      RequireNoPolicyDraft;
    end;
    if (FConnectivityList.selectedIndex < 0) or
        (FConnectivityList.selectedIndex >= FWorkspace.ConnectivityCount) then Exit;
    LoadConnectivityFields(FConnectivityList.selectedIndex);
    RefreshConnectivityState;
  except on E: Exception do ShowError(E.Message); end;
end;

function TBrowserTrainingStudioApplication.HandleConnectivityApply(AEvent: TJSMouseEvent): Boolean;
begin
  Result := False;
  try ApplyConnectivityDraft; except on E: Exception do ShowError(E.Message); end;
end;

function TBrowserTrainingStudioApplication.HandleConnectivityNew(AEvent: TJSMouseEvent): Boolean;
begin
  Result := False;
  try RequireNoPolicyDraft; LoadConnectivityFields(-1); RefreshConnectivityState;
  except on E: Exception do ShowError(E.Message); end;
end;

function TBrowserTrainingStudioApplication.HandleConnectivityRemove(AEvent: TJSMouseEvent): Boolean;
var C: TWfcTrainingConnectivities; I: Integer;
begin
  Result := False;
  try
    RequireNoPolicyDraft;
    C := FWorkspace.CopyConnectivities;
    if (FEditingConnectivityIndex < 0) or (FEditingConnectivityIndex >= Length(C)) then
      raise EWfcTrainingWorkspace.Create('select a saved network to remove');
    for I := FEditingConnectivityIndex to Length(C) - 2 do C[I] := C[I + 1];
    SetLength(C, Length(C) - 1); CommitConnectivities(C);
  except on E: Exception do ShowError(E.Message); end;
end;

function TBrowserTrainingStudioApplication.HandleConnectivityClear(AEvent: TJSMouseEvent): Boolean;
begin
  Result := False;
  try RequireNoPolicyDraft; CommitConnectivities(nil);
  except on E: Exception do ShowError(E.Message); end;
end;

function TBrowserTrainingStudioApplication.HandleConnectivityDiscard(AEvent: TJSMouseEvent): Boolean;
begin
  Result := False;
  try
    if FQuotaDraftDirty then
      raise EWfcTrainingWorkspace.Create('discard the quota draft in its own editor');
    CancelSourceFileRead;
    FConnectivityDraftDirty := False;
    ReloadQuotaEditor; ReloadConnectivityEditor; RefreshAll;
    if FWorkspace.HasRecipe then
      SetState('trained', 'Network draft discarded; saved source is unchanged.',
        'The previous run stays cleared; configure and solve again.')
    else SetState('source-dirty', 'Network draft discarded; train the retained source.',
      'The failed rebuild left no recipe, run, or result.');
  except on E: Exception do ShowError(E.Message); end;
end;

function TBrowserTrainingStudioApplication.HandleConnectivityDemo(AEvent: TJSMouseEvent): Boolean;
begin
  Result := False;
  try
    RequireNoPolicyDraft;
    ApplySourceText(TrainingStudioRouteSource);
    WriteOptions(TrainingStudioRouteOptions, 1);
    TrainWorkspace;
    CommitConnectivities(TrainingStudioRouteNetwork);
    CommitValueQuotas(TrainingStudioRouteQuota);
    SolveWorkspace;
    AssertTest(TrainingStudioRouteIsValid(FWorkspace.OutputTokens),
      'route demonstration independent path/count validation');
  except on E: Exception do ShowError(E.Message); end;
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
  LDepth: Integer;
  LX: Integer;
  LY: Integer;
  LZ: Integer;
begin
  Result := False;
  try
    FWorkspace.ClearRun;
    FSelectedCell := -1;
    if not FWorkspace.HasRecipe then
      raise EWfcTrainingWorkspace.Create('train the current source first');
    LOptions := ReadOptions;
    LDepth := ReadBoundedInteger(FDepthInput, 'depth', 1,
      FLimits.MaxOutputCells);
    if (FWorkspace.Rank <> 3) and (LDepth <> 1) then
      raise EConvertError.Create(
        'depth must be 1 unless the trained recipe has rank 3');
    LX := ReadLockCoordinate(FLockXInput, 'lock x', LOptions.Width);
    LY := ReadLockCoordinate(FLockYInput, 'lock y', LOptions.Height);
    LZ := ReadLockCoordinate(FLockZInput, 'lock z', LDepth);
    LTokenIndex := SelectedVocabularyIndex;
    AddOrReplaceLock(LX, LY, LZ, FVocabulary[LTokenIndex]);
    RefreshAll;
    for I := 0 to Length(FLocks) - 1 do
      if (FLocks[I].X = LX) and (FLocks[I].Y = LY) and
          (FLocks[I].Z = LZ) then
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
    FLockZInput.value := LElement.getAttribute('data-z');
    LPrevious := document.querySelector('.output-cell.selected');
    if Assigned(LPrevious) then LPrevious.className := 'output-cell';
    LElement.className := 'output-cell selected';
    FStatusDetailElement.textContent := 'Selected public cell x=' +
      FLockXInput.value + ', y=' + FLockYInput.value + ', z=' +
      FLockZInput.value + '.';
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
