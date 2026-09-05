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
unit browser_ensemble_studio_app;

{$mode delphi}{$H+}

interface

uses
  JS,
  Web,
  SysUtils,
  wfc,
  wfc_model,
  wfc_music,
  wfc_music_audio,
  wfc_music_sequence,
  wfc_music_ensemble,
  wfc_music_ensemble_passes,
  wfc_midi_smf,
  wfc_music_midi,
  ensemble_studio_workbench,
  browser_ensemble_stream;

type
  { Browser-only interaction and Blob glue. The portable workbench owns all
    generation, constraints, validation, reports, and canonical artifacts. }
  TBrowserEnsembleStudioApplication = class
  strict private
    FStudio: TEnsembleStudio;
    FStreamController: TBrowserEnsembleStreamController;
    FOptions: TEnsembleStudioOptions;
    FAction: TEnsembleStudioAction;
    FVocabulary: TWfcModelTokens;
    FSessionConfigDirty: Boolean;
    FAudioPlayEvents: Integer;
    FScoreUrl, FMidiUrl, FWaveUrl: String;

    FSeedInput, FBarsInput: TJSHTMLInputElement;
    FNewSessionButton, FGenerateButton, FRenderPreviewButton:
      TJSHTMLButtonElement;
    FStrategySelect, FScopeSelect: TJSHTMLSelectElement;
    FBacktracksInput, FPassBacktracksInput, FTraceInput:
      TJSHTMLInputElement;
    FStatusElement, FStatusDetailElement, FSessionSummary,
      FSessionOutput, FResultStatusOutput, FStrategyOutput,
      FScopeOutput, FLocalBacktracksOutput, FPassBacktracksOutput,
      FPassCountOutput, FLockCountOutput, FSignatureOutput: TJSElement;
    FHarmonyGrid, FRhythmGrid, FEnsembleGrid, FLineagePlaceholder,
      FDisplayWindowNote, FBassGrid, FChordsGrid, FUpperGrid,
      FScorePlaceholder: TJSElement;
    FLockLayerSelect: TJSHTMLSelectElement;
    FLockCellInput: TJSHTMLInputElement;
    FLockTokenSelect, FLockList: TJSHTMLSelectElement;
    FAddLockButton, FRemoveLockButton, FClearLocksButton:
      TJSHTMLButtonElement;
    FScopeReport, FPassReport, FFailureReport, FAudioStatus,
      FAudioDetail: TJSElement;
    FPreviewAudio: TJSHTMLAudioElement;
    FScoreLink, FMidiLink, FWaveLink: TJSHTMLAnchorElement;
    FArtifactOutput: TJSHTMLTextAreaElement;

    function RequireElement(const AId: String): TJSElement;
    procedure BindDocument;
    procedure BindEvents;
    procedure WriteDefaultOptions;
    procedure SetState(const AState, AStatus, ADetail: String);
    procedure ShowError(const AMessage: String);
    function TryParseSeed(const AText: String;
      out ASeed: TGraphSeed): Boolean;
    function TryParseNonnegativeInteger(const AText: String;
      out AValue: Integer): Boolean;
    function ReadBoundedInteger(const AInput: TJSHTMLInputElement;
      const AName: String; const AMinimum, AMaximum: Integer): Integer;
    function ReadOptions: TEnsembleStudioOptions;
    function SelectedAction: TEnsembleStudioAction;
    function SelectedLayer: TWfcMusicEnsembleLayer;
    function SelectedVocabularyIndex: Integer;
    function FindVocabularyToken(const AToken: TWfcModelToken): Integer;
    function ActionName(const AAction: TEnsembleStudioAction): String;
    function StrategyName(const AOptions: TEnsembleStudioOptions): String;
    function DispositionName(
      const ADisposition: TGraphPassDisposition): String;
    function ContradictionName(
      const AKind: TGraphContradictionKind): String;
    function PassIndicesText(const AIndices: TGraphPassIndices;
      const AEmptyText: String): String;

    procedure RevokeUrl(var AUrl: String);
    procedure DisableDownload(const ALink: TJSHTMLAnchorElement;
      var AUrl: String);
    procedure SetTextDownload(const ALink: TJSHTMLAnchorElement;
      const AText, AFileName, AMime: String; var AUrl: String);
    procedure SetBinaryDownload(const ALink: TJSHTMLAnchorElement;
      const ABytes: array of Byte; const AFileName, AMime: String;
      var AUrl: String; out AByteCount: Integer;
      out ASignature: String);
    procedure ResetPreviewAudio;
    procedure ClearCurrentPresentation;
    procedure ClearReports;
    procedure RefreshAll;
    procedure RefreshMetrics;
    procedure RefreshVocabulary;
    procedure RefreshLocks;
    procedure RefreshLineage;
    procedure RefreshLayer(const ALayer: TWfcMusicEnsembleLayer;
      const AGrid: TJSElement);
    procedure RefreshVoices;
    procedure RefreshVoice(const AVoiceIndex: Integer;
      const AGrid: TJSElement;
      const AFrames: TWfcMusicEnsembleFrames);
    procedure RefreshReports;
    procedure RefreshArtifacts;
    procedure BuildPreview;
    procedure UpdateSessionSummary;
    function LockIndex(const ALayer: TWfcMusicEnsembleLayer;
      const APosition: Integer): Integer;
    function ReportPassCount(const AReport: TEnsembleStudioReport): Integer;

    procedure InvalidateForPendingEdit(const AReason: String;
      const ASessionConfig: Boolean);
    procedure StartNewSession;
    procedure Generate;
    procedure AssertTest(const ACondition: Boolean;
      const AMessage: String);
    procedure DispatchDomEvent(const AElement: TJSElement;
      const AEventName: String);

    function HandleSessionInput(AEvent: TJSEvent): Boolean;
    function HandleNewSession(AEvent: TJSMouseEvent): Boolean;
    function HandleGenerate(AEvent: TJSMouseEvent): Boolean;
    function HandleRunInput(AEvent: TJSEvent): Boolean;
    function HandleStrategyChange(AEvent: TJSEvent): Boolean;
    function HandleLockLayerChange(AEvent: TJSEvent): Boolean;
    function HandleAddLock(AEvent: TJSMouseEvent): Boolean;
    function HandleRemoveLock(AEvent: TJSMouseEvent): Boolean;
    function HandleClearLocks(AEvent: TJSMouseEvent): Boolean;
    function HandleCellClick(AEvent: TJSMouseEvent): Boolean;
    function HandleRenderPreview(AEvent: TJSMouseEvent): Boolean;
    function HandleAudioPlay(AEvent: TJSPointerEvent): Boolean;
    function HandleAudioEnded(AEvent: TJSEvent): Boolean;
    function HandleAudioError(AEvent: TJSErrorEvent): Boolean;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Run;
    procedure RunSelfTest;
  end;

{ Used by the portable browser conformance wrapper. It creates only the DOM
  contract required by this controller; the showcase itself uses index.html. }
procedure InstallEnsembleStudioBrowserTestFixture;

implementation

const
  MAX_SEED = Cardinal($FFFFFFFF);
  DISPLAY_CELL_LIMIT = 128;

constructor TBrowserEnsembleStudioApplication.Create;
begin
  inherited Create;
  FStudio := TEnsembleStudio.Create(0, ENSEMBLE_STUDIO_DEFAULT_BARS);
  FOptions := DefaultEnsembleStudioOptions;
  FAction := esaGenerate;
  FVocabulary := nil;
  FSessionConfigDirty := False;
  FAudioPlayEvents := 0;
  FScoreUrl := '';
  FMidiUrl := '';
  FWaveUrl := '';
end;

destructor TBrowserEnsembleStudioApplication.Destroy;
begin
  if FStreamController <> nil then FStreamController.Release;
  RevokeUrl(FScoreUrl);
  RevokeUrl(FMidiUrl);
  RevokeUrl(FWaveUrl);
  FStudio.Free;
  inherited Destroy;
end;

function TBrowserEnsembleStudioApplication.RequireElement(
  const AId: String): TJSElement;
begin
  Result := document.getElementById(AId);
  if not Assigned(Result) then
    raise EEnsembleStudio.Create('Ensemble Studio is missing #' + AId);
end;

procedure TBrowserEnsembleStudioApplication.BindDocument;
begin
  FSeedInput := TJSHTMLInputElement(RequireElement('seed-input'));
  FBarsInput := TJSHTMLInputElement(RequireElement('bars-input'));
  FNewSessionButton := TJSHTMLButtonElement(
    RequireElement('new-session-button'));
  FGenerateButton := TJSHTMLButtonElement(RequireElement('generate-button'));
  FRenderPreviewButton := TJSHTMLButtonElement(
    RequireElement('render-preview-button'));
  FStrategySelect := TJSHTMLSelectElement(RequireElement('strategy-select'));
  FScopeSelect := TJSHTMLSelectElement(RequireElement('scope-select'));
  FBacktracksInput := TJSHTMLInputElement(
    RequireElement('backtracks-input'));
  FPassBacktracksInput := TJSHTMLInputElement(
    RequireElement('pass-backtracks-input'));
  FTraceInput := TJSHTMLInputElement(RequireElement('trace-input'));
  FStatusElement := RequireElement('status');
  FStatusDetailElement := RequireElement('status-detail');
  FSessionSummary := RequireElement('session-summary');
  FSessionOutput := RequireElement('session-output');
  FResultStatusOutput := RequireElement('result-status');
  FStrategyOutput := RequireElement('strategy-output');
  FScopeOutput := RequireElement('scope-output');
  FLocalBacktracksOutput := RequireElement('local-backtracks-output');
  FPassBacktracksOutput := RequireElement('pass-backtracks-output');
  FPassCountOutput := RequireElement('pass-count-output');
  FLockCountOutput := RequireElement('lock-count-output');
  FSignatureOutput := RequireElement('composition-signature');
  FHarmonyGrid := RequireElement('harmony-grid');
  FRhythmGrid := RequireElement('rhythm-grid');
  FEnsembleGrid := RequireElement('ensemble-grid');
  FLineagePlaceholder := RequireElement('lineage-placeholder');
  FDisplayWindowNote := RequireElement('display-window-note');
  FBassGrid := RequireElement('bass-grid');
  FChordsGrid := RequireElement('chords-grid');
  FUpperGrid := RequireElement('upper-grid');
  FScorePlaceholder := RequireElement('score-placeholder');
  FLockLayerSelect := TJSHTMLSelectElement(
    RequireElement('lock-layer-select'));
  FLockCellInput := TJSHTMLInputElement(RequireElement('lock-cell-input'));
  FLockTokenSelect := TJSHTMLSelectElement(
    RequireElement('lock-token-select'));
  FLockList := TJSHTMLSelectElement(RequireElement('lock-list'));
  FAddLockButton := TJSHTMLButtonElement(RequireElement('add-lock-button'));
  FRemoveLockButton := TJSHTMLButtonElement(
    RequireElement('remove-lock-button'));
  FClearLocksButton := TJSHTMLButtonElement(
    RequireElement('clear-locks-button'));
  FScopeReport := RequireElement('scope-report');
  FPassReport := RequireElement('pass-report');
  FFailureReport := RequireElement('failure-report');
  FAudioStatus := RequireElement('audio-status');
  FAudioDetail := RequireElement('audio-detail');
  FPreviewAudio := TJSHTMLAudioElement(RequireElement('preview-audio'));
  FScoreLink := TJSHTMLAnchorElement(RequireElement('download-score-link'));
  FMidiLink := TJSHTMLAnchorElement(RequireElement('download-midi-link'));
  FWaveLink := TJSHTMLAnchorElement(RequireElement('download-wav-link'));
  FArtifactOutput := TJSHTMLTextAreaElement(
    RequireElement('artifact-output'));
end;

procedure TBrowserEnsembleStudioApplication.BindEvents;
begin
  FSeedInput.oninput := @HandleSessionInput;
  FBarsInput.oninput := @HandleSessionInput;
  FNewSessionButton.onclick := @HandleNewSession;
  FGenerateButton.onclick := @HandleGenerate;
  FStrategySelect.onchange := @HandleStrategyChange;
  FScopeSelect.onchange := @HandleRunInput;
  FBacktracksInput.oninput := @HandleRunInput;
  FPassBacktracksInput.oninput := @HandleRunInput;
  FTraceInput.onchange := @HandleRunInput;
  FLockLayerSelect.onchange := @HandleLockLayerChange;
  FAddLockButton.onclick := @HandleAddLock;
  FRemoveLockButton.onclick := @HandleRemoveLock;
  FClearLocksButton.onclick := @HandleClearLocks;
  FRenderPreviewButton.onclick := @HandleRenderPreview;
  FPreviewAudio.onplay := @HandleAudioPlay;
  FPreviewAudio.onended := @HandleAudioEnded;
  FPreviewAudio.onerror := @HandleAudioError;
end;

procedure TBrowserEnsembleStudioApplication.WriteDefaultOptions;
begin
  FOptions := DefaultEnsembleStudioOptions;
  if FOptions.Negotiated then FStrategySelect.value := 'negotiated'
  else FStrategySelect.value := 'one-way';
  FScopeSelect.value := 'full';
  FBacktracksInput.value := IntToStr(FOptions.MaxBacktracks);
  FPassBacktracksInput.value := IntToStr(FOptions.MaxPassBacktracks);
  FTraceInput.checked := FOptions.CaptureTrace;
end;

procedure TBrowserEnsembleStudioApplication.SetState(
  const AState, AStatus, ADetail: String);
begin
  document.body.setAttribute('data-state', AState);
  if AState <> 'error' then document.body.removeAttribute('data-error');
  FStatusElement.textContent := AStatus;
  FStatusDetailElement.textContent := ADetail;
end;

procedure TBrowserEnsembleStudioApplication.ShowError(
  const AMessage: String);
begin
  document.body.setAttribute('data-error', AMessage);
  SetState('error', 'Error: ' + AMessage,
    'Output invalidated by this operation remains unavailable.');
end;

function TBrowserEnsembleStudioApplication.TryParseSeed(
  const AText: String; out ASeed: TGraphSeed): Boolean;
var
  LBase, LDigit, LValue: Cardinal;
  LCharacter: Char;
  LIndex, LStart: Integer;
  LText: String;
begin
  Result := False;
  ASeed := 0;
  LText := Trim(AText);
  if LText = '' then Exit;
  LBase := 10;
  LStart := 1;
  if LText[1] = '$' then begin LBase := 16; LStart := 2; end
  else if (Length(LText) >= 2) and (LText[1] = '0') and
      ((LText[2] = 'x') or (LText[2] = 'X')) then
  begin LBase := 16; LStart := 3; end;
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
    else Exit;
    if LValue > (MAX_SEED - LDigit) div LBase then Exit;
    LValue := LValue * LBase + LDigit;
  end;
  ASeed := TGraphSeed(LValue);
  Result := True;
end;

function TBrowserEnsembleStudioApplication.ReadBoundedInteger(
  const AInput: TJSHTMLInputElement; const AName: String;
  const AMinimum, AMaximum: Integer): Integer;
begin
  if not TryParseNonnegativeInteger(AInput.value, Result) or
      (Result < AMinimum) or (Result > AMaximum) then
    raise EConvertError.CreateFmt('%s must be from %d through %d',
      [AName, AMinimum, AMaximum]);
end;

function TBrowserEnsembleStudioApplication.TryParseNonnegativeInteger(
  const AText: String; out AValue: Integer): Boolean;
var
  I, LDigit: Integer;
  LText: String;
begin
  Result := False;
  AValue := 0;
  LText := Trim(AText);
  if LText = '' then Exit;
  for I := 1 to Length(LText) do
  begin
    if not (LText[I] in ['0'..'9']) then Exit;
    LDigit := Ord(LText[I]) - Ord('0');
    if AValue > (High(Integer) - LDigit) div 10 then Exit;
    AValue := AValue * 10 + LDigit;
  end;
  Result := True;
end;

function TBrowserEnsembleStudioApplication.ReadOptions:
  TEnsembleStudioOptions;
begin
  Result := DefaultEnsembleStudioOptions;
  if FStrategySelect.value = 'negotiated' then Result.Negotiated := True
  else if FStrategySelect.value = 'one-way' then Result.Negotiated := False
  else raise EConvertError.Create('select a known generation strategy');
  Result.MaxBacktracks := ReadBoundedInteger(FBacktracksInput,
    'local backtrack allowance', 0, High(Integer));
  Result.MaxPassBacktracks := ReadBoundedInteger(FPassBacktracksInput,
    'pass backtrack allowance', 0, High(Integer));
  if (not Result.Negotiated) and (Result.MaxPassBacktracks <> 0) then
    raise EConvertError.Create(
      'one-way generation requires zero pass backtracks');
  Result.CaptureTrace := FTraceInput.checked;
end;

function TBrowserEnsembleStudioApplication.SelectedAction:
  TEnsembleStudioAction;
begin
  if FScopeSelect.value = 'full' then Result := esaGenerate
  else if FScopeSelect.value = 'harmony' then Result := esaHarmony
  else if FScopeSelect.value = 'rhythm' then Result := esaRhythm
  else if FScopeSelect.value = 'ensemble' then Result := esaEnsemble
  else raise EConvertError.Create('select a known regeneration scope');
end;

function TBrowserEnsembleStudioApplication.SelectedLayer:
  TWfcMusicEnsembleLayer;
begin
  if FLockLayerSelect.value = 'harmony' then Result := wmelHarmony
  else if FLockLayerSelect.value = 'rhythm' then Result := wmelRhythm
  else if FLockLayerSelect.value = 'ensemble' then Result := wmelEnsemble
  else raise EConvertError.Create('select a known public layer');
end;

function TBrowserEnsembleStudioApplication.SelectedVocabularyIndex: Integer;
begin
  if not TryStrToInt(FLockTokenSelect.value, Result) or
      (Result < 0) or (Result >= Length(FVocabulary)) then
    raise EConvertError.Create('select a public token');
end;

function TBrowserEnsembleStudioApplication.FindVocabularyToken(
  const AToken: TWfcModelToken): Integer;
begin
  for Result := 0 to High(FVocabulary) do
    if FVocabulary[Result] = AToken then Exit;
  Result := -1;
end;

function TBrowserEnsembleStudioApplication.ActionName(
  const AAction: TEnsembleStudioAction): String;
begin
  case AAction of
    esaGenerate: Result := 'full';
    esaHarmony: Result := 'harmony';
    esaRhythm: Result := 'rhythm';
    esaEnsemble: Result := 'ensemble';
  else Result := 'unknown';
  end;
end;

function TBrowserEnsembleStudioApplication.StrategyName(
  const AOptions: TEnsembleStudioOptions): String;
begin
  if AOptions.Negotiated then Result := 'negotiated'
  else Result := 'one-way';
end;

function TBrowserEnsembleStudioApplication.DispositionName(
  const ADisposition: TGraphPassDisposition): String;
begin
  case ADisposition of
    gpdNotRun: Result := 'not-run';
    gpdReused: Result := 'reused';
    gpdCleared: Result := 'cleared';
    gpdCopied: Result := 'copied';
    gpdSolved: Result := 'solved';
    gpdFailed: Result := 'failed';
  else Result := 'unknown';
  end;
end;

function TBrowserEnsembleStudioApplication.ContradictionName(
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
  else Result := 'unknown';
  end;
end;

function TBrowserEnsembleStudioApplication.PassIndicesText(
  const AIndices: TGraphPassIndices; const AEmptyText: String): String;
var
  I: Integer;
begin
  if Length(AIndices) = 0 then Exit(AEmptyText);
  Result := '';
  for I := 0 to High(AIndices) do
  begin
    if I > 0 then Result := Result + ', ';
    if (AIndices[I] >= Ord(Low(TWfcMusicEnsembleLayer))) and
        (AIndices[I] <= Ord(High(TWfcMusicEnsembleLayer))) then
      Result := Result + WfcMusicEnsembleLayerName(
        TWfcMusicEnsembleLayer(AIndices[I]))
    else Result := Result + 'pass ' + IntToStr(AIndices[I]);
  end;
end;

procedure TBrowserEnsembleStudioApplication.RevokeUrl(var AUrl: String);
begin
  if AUrl = '' then Exit;
  TJSURL.revokeObjectURL(AUrl);
  AUrl := '';
end;

procedure TBrowserEnsembleStudioApplication.DisableDownload(
  const ALink: TJSHTMLAnchorElement; var AUrl: String);
begin
  RevokeUrl(AUrl);
  ALink.removeAttribute('href');
  ALink.removeAttribute('download');
  ALink.setAttribute('aria-disabled', 'true');
  ALink.className := 'button-link disabled';
end;

procedure TBrowserEnsembleStudioApplication.SetTextDownload(
  const ALink: TJSHTMLAnchorElement;
  const AText, AFileName, AMime: String; var AUrl: String);
var
  LBlob: TJSBlob;
  LOptions: TJSBlobInit;
  LParts: TJSArray;
begin
  DisableDownload(ALink, AUrl);
  LParts := TJSArray.new;
  LParts.push(AText);
  LOptions := TJSBlobInit.new;
  LOptions.type_ := AMime;
  LBlob := TJSBlob.new(LParts, LOptions);
  AUrl := TJSURL.createObjectURL(LBlob);
  ALink.href := AUrl;
  ALink.download := AFileName;
  ALink.setAttribute('aria-disabled', 'false');
  ALink.className := 'button-link';
end;

procedure TBrowserEnsembleStudioApplication.SetBinaryDownload(
  const ALink: TJSHTMLAnchorElement; const ABytes: array of Byte;
  const AFileName, AMime: String; var AUrl: String;
  out AByteCount: Integer; out ASignature: String);
var
  I: Integer;
  LBlob: TJSBlob;
  LOptions: TJSBlobInit;
  LParts: TJSArray;
  LTyped: TJSUint8Array;
begin
  DisableDownload(ALink, AUrl);
  AByteCount := Length(ABytes);
  ASignature := EnsembleStudioByteSignature(ABytes);
  LTyped := TJSUint8Array.new(AByteCount);
  for I := 0 to AByteCount - 1 do LTyped[I] := ABytes[I];
  LParts := TJSArray.new;
  LParts.push(LTyped);
  LOptions := TJSBlobInit.new;
  LOptions.type_ := AMime;
  LBlob := TJSBlob.new(LParts, LOptions);
  AUrl := TJSURL.createObjectURL(LBlob);
  ALink.href := AUrl;
  ALink.download := AFileName;
  ALink.setAttribute('aria-disabled', 'false');
  ALink.className := 'button-link';
end;

procedure TBrowserEnsembleStudioApplication.ResetPreviewAudio;
var
  LReplacement: TJSHTMLAudioElement;
begin
  FPreviewAudio.onplay := nil;
  FPreviewAudio.onended := nil;
  FPreviewAudio.onerror := nil;
  FPreviewAudio.pause;
  FPreviewAudio.removeAttribute('src');
  FPreviewAudio.load;
  LReplacement := TJSHTMLAudioElement(FPreviewAudio.cloneNode(False));
  LReplacement.removeAttribute('src');
  LReplacement.autoplay := False;
  LReplacement.preload := 'none';
  LReplacement.onplay := @HandleAudioPlay;
  LReplacement.onended := @HandleAudioEnded;
  LReplacement.onerror := @HandleAudioError;
  FPreviewAudio.parentNode.replaceChild(LReplacement, FPreviewAudio);
  FPreviewAudio := LReplacement;
end;

procedure TBrowserEnsembleStudioApplication.ClearCurrentPresentation;
begin
  FHarmonyGrid.textContent := '';
  FRhythmGrid.textContent := '';
  FEnsembleGrid.textContent := '';
  FLineagePlaceholder.removeAttribute('hidden');
  FDisplayWindowNote.setAttribute('hidden', '');
  FBassGrid.textContent := '';
  FChordsGrid.textContent := '';
  FUpperGrid.textContent := '';
  FScorePlaceholder.removeAttribute('hidden');
  FArtifactOutput.value := '';
  DisableDownload(FScoreLink, FScoreUrl);
  DisableDownload(FMidiLink, FMidiUrl);
  ResetPreviewAudio;
  DisableDownload(FWaveLink, FWaveUrl);
  FRenderPreviewButton.disabled := True;
  FAudioStatus.textContent := 'no preview';
  FAudioDetail.textContent :=
    'Generate a current score before requesting a short WAV preview.';
  document.body.setAttribute('data-cell-count', '0');
  document.body.setAttribute('data-displayed-cell-count', '0');
  document.body.setAttribute('data-voice-count', '0');
  document.body.setAttribute('data-midi-bytes', '0');
  document.body.setAttribute('data-midi-status', 'none');
  document.body.removeAttribute('data-midi-error');
  document.body.setAttribute('data-wave-bytes', '0');
  document.body.setAttribute('data-audio-ready', 'false');
  document.body.setAttribute('data-composition-signature', '');
  document.body.setAttribute('data-score-signature', '');
  document.body.setAttribute('data-midi-signature', '');
  document.body.setAttribute('data-wave-signature', '');
end;

procedure TBrowserEnsembleStudioApplication.ClearReports;
begin
  FScopeReport.textContent := 'No generation attempt.';
  FPassReport.textContent := 'No generation attempt.';
  FFailureReport.textContent := 'No terminal failure.';
end;

function TBrowserEnsembleStudioApplication.LockIndex(
  const ALayer: TWfcMusicEnsembleLayer;
  const APosition: Integer): Integer;
var
  LLocks: TEnsembleStudioLocks;
begin
  LLocks := FStudio.CopyLocks;
  for Result := 0 to High(LLocks) do
    if (LLocks[Result].Layer = ALayer) and
        (LLocks[Result].Position = APosition) then Exit;
  Result := -1;
end;

function TBrowserEnsembleStudioApplication.ReportPassCount(
  const AReport: TEnsembleStudioReport): Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to High(AReport.Passes) do
    if AReport.Passes[I].Executed then Inc(Result);
end;

procedure TBrowserEnsembleStudioApplication.UpdateSessionSummary;
var
  LBars: Integer;
begin
  if TryParseNonnegativeInteger(FBarsInput.value, LBars) and
      (LBars > 0) and (LBars <= ENSEMBLE_STUDIO_MAX_BARS) then
    FSessionSummary.textContent := IntToStr(LBars) + ' bars -> ' +
      IntToStr(EnsembleStudioBarsToCellCount(LBars)) + ' cells -> ' +
      IntToStr(EnsembleStudioDurationSeconds(LBars)) +
      ' seconds at the fixed showcase tempo.'
  else
    FSessionSummary.textContent :=
      'Enter a positive whole-bar count within the numeric format bounds.';
end;

procedure TBrowserEnsembleStudioApplication.RefreshMetrics;
var
  LLocks: TEnsembleStudioLocks;
  LReport: TEnsembleStudioReport;
  LSignature, LStatus: String;
begin
  LLocks := FStudio.CopyLocks;
  LReport := FStudio.CopyReport;
  LStatus := EnsembleStudioStatusName(FStudio.Status);
  FScopeSelect.disabled := FSessionConfigDirty or not FStudio.HasBaseline;
  if not FStudio.HasBaseline then FScopeSelect.value := 'full';
  if FSessionConfigDirty then FGenerateButton.textContent := 'New session required'
  else if FStudio.HasBaseline then FGenerateButton.textContent := 'Generate / repair'
  else FGenerateButton.textContent := 'Generate baseline';
  FSessionOutput.textContent := 'seed ' + UIntToStr(FStudio.Seed) +
    ' / ' + IntToStr(FStudio.Bars) + ' bars';
  FResultStatusOutput.textContent := LStatus;
  FStrategyOutput.textContent := StrategyName(FOptions);
  FScopeOutput.textContent := ActionName(FAction);
  FLocalBacktracksOutput.textContent := IntToStr(LReport.Backtracks) +
    ' / ' + IntToStr(FOptions.MaxBacktracks);
  FPassBacktracksOutput.textContent := IntToStr(LReport.PassBacktracks) +
    ' / ' + IntToStr(FOptions.MaxPassBacktracks);
  FPassCountOutput.textContent := IntToStr(ReportPassCount(LReport));
  FLockCountOutput.textContent := IntToStr(Length(LLocks));
  LSignature := '';
  if FStudio.HasCurrent then LSignature := FStudio.SignatureText;
  if LSignature = '' then FSignatureOutput.textContent := '-'
  else FSignatureOutput.textContent := LSignature;
  document.body.setAttribute('data-seed', UIntToStr(FStudio.Seed));
  document.body.setAttribute('data-bars', IntToStr(FStudio.Bars));
  document.body.setAttribute('data-result-status', LStatus);
  document.body.setAttribute('data-lock-count', IntToStr(Length(LLocks)));
  document.body.setAttribute('data-pass-count',
    IntToStr(ReportPassCount(LReport)));
  document.body.setAttribute('data-composition-signature', LSignature);
  UpdateSessionSummary;
end;

procedure TBrowserEnsembleStudioApplication.RefreshVocabulary;
var
  I, LOldIndex, LSelectedIndex: Integer;
  LLayer: TWfcMusicEnsembleLayer;
  LOldToken: TWfcModelToken;
  LOption: TJSHTMLOptionElement;
begin
  LOldToken := '';
  if TryStrToInt(FLockTokenSelect.value, LOldIndex) and
      (LOldIndex >= 0) and (LOldIndex < Length(FVocabulary)) then
    LOldToken := FVocabulary[LOldIndex];
  LLayer := SelectedLayer;
  FVocabulary := FStudio.PublicTokens(LLayer);
  FLockTokenSelect.textContent := '';
  for I := 0 to High(FVocabulary) do
  begin
    LOption := TJSHTMLOptionElement(document.createElement('option'));
    LOption.value := IntToStr(I);
    LOption.textContent := EnsembleStudioTokenLabel(LLayer, FVocabulary[I]);
    FLockTokenSelect.appendChild(LOption);
  end;
  LSelectedIndex := FindVocabularyToken(LOldToken);
  if (LSelectedIndex < 0) and (Length(FVocabulary) > 0) then
    LSelectedIndex := 0;
  if LSelectedIndex >= 0 then
    FLockTokenSelect.value := IntToStr(LSelectedIndex);
end;

procedure TBrowserEnsembleStudioApplication.RefreshLocks;
var
  I, LOldIndex, LSelectedIndex: Integer;
  LLocks: TEnsembleStudioLocks;
  LOldLayer: TWfcMusicEnsembleLayer;
  LOldPosition: Integer;
  LOption: TJSHTMLOptionElement;
begin
  LLocks := FStudio.CopyLocks;
  LOldIndex := FLockList.selectedIndex;
  LOldPosition := -1;
  LOldLayer := wmelHarmony;
  if (LOldIndex >= 0) and (LOldIndex < Length(LLocks)) then
  begin
    LOldLayer := LLocks[LOldIndex].Layer;
    LOldPosition := LLocks[LOldIndex].Position;
  end;
  FLockList.textContent := '';
  LSelectedIndex := -1;
  for I := 0 to High(LLocks) do
  begin
    LOption := TJSHTMLOptionElement(document.createElement('option'));
    LOption.value := IntToStr(I);
    LOption.textContent := WfcMusicEnsembleLayerName(LLocks[I].Layer) +
      ' / cell ' + IntToStr(LLocks[I].Position) + ' / ' +
      EnsembleStudioTokenLabel(LLocks[I].Layer, LLocks[I].Token);
    FLockList.appendChild(LOption);
    if (LLocks[I].Layer = LOldLayer) and
        (LLocks[I].Position = LOldPosition) then LSelectedIndex := I;
  end;
  if LSelectedIndex >= 0 then FLockList.selectedIndex := LSelectedIndex;
end;

procedure TBrowserEnsembleStudioApplication.RefreshLayer(
  const ALayer: TWfcMusicEnsembleLayer; const AGrid: TJSElement);
var
  I, LDisplayCount: Integer;
  LButton: TJSHTMLButtonElement;
  LSmall: TJSElement;
  LTokens: TWfcModelTokens;
begin
  AGrid.textContent := '';
  if not FStudio.HasCurrent then Exit;
  LTokens := FStudio.CellTokens(ALayer);
  if Length(LTokens) <> FStudio.CellCount then
    raise EEnsembleStudio.Create('public layer length differs from session');
  LDisplayCount := Length(LTokens);
  if LDisplayCount > DISPLAY_CELL_LIMIT then LDisplayCount := DISPLAY_CELL_LIMIT;
  for I := 0 to LDisplayCount - 1 do
  begin
    LButton := TJSHTMLButtonElement(document.createElement('button'));
    LButton.setAttribute('type', 'button');
    LButton.className := 'music-cell';
    if LockIndex(ALayer, I) >= 0 then
      LButton.className := LButton.className + ' locked';
    LButton.setAttribute('data-layer',
      WfcMusicEnsembleLayerName(ALayer));
    LButton.setAttribute('data-index', IntToStr(I));
    LButton.setAttribute('title',
      EnsembleStudioTokenLabel(ALayer, LTokens[I]));
    LButton.textContent := EnsembleStudioTokenLabel(ALayer, LTokens[I]);
    LSmall := document.createElement('small');
    LSmall.textContent := 'cell ' + IntToStr(I);
    LButton.appendChild(LSmall);
    LButton.onclick := @HandleCellClick;
    AGrid.appendChild(LButton);
  end;
end;

procedure TBrowserEnsembleStudioApplication.RefreshLineage;
var
  LDisplayCount: Integer;
begin
  if not FStudio.HasCurrent then
  begin
    FHarmonyGrid.textContent := '';
    FRhythmGrid.textContent := '';
    FEnsembleGrid.textContent := '';
    FLineagePlaceholder.removeAttribute('hidden');
    FDisplayWindowNote.setAttribute('hidden', '');
    Exit;
  end;
  RefreshLayer(wmelHarmony, FHarmonyGrid);
  RefreshLayer(wmelRhythm, FRhythmGrid);
  RefreshLayer(wmelEnsemble, FEnsembleGrid);
  FLineagePlaceholder.setAttribute('hidden', '');
  LDisplayCount := FStudio.CellCount;
  if LDisplayCount > DISPLAY_CELL_LIMIT then LDisplayCount := DISPLAY_CELL_LIMIT;
  document.body.setAttribute('data-cell-count', IntToStr(FStudio.CellCount));
  document.body.setAttribute('data-displayed-cell-count',
    IntToStr(LDisplayCount));
  if FStudio.CellCount > DISPLAY_CELL_LIMIT then
  begin
    FDisplayWindowNote.textContent := 'Showing the first ' +
      IntToStr(DISPLAY_CELL_LIMIT) + ' of ' +
      IntToStr(FStudio.CellCount) +
      ' cells in each lane. The canonical score and MIDI exports contain the full requested grid.';
    FDisplayWindowNote.removeAttribute('hidden');
  end
  else
    FDisplayWindowNote.setAttribute('hidden', '');
end;

procedure TBrowserEnsembleStudioApplication.RefreshVoice(
  const AVoiceIndex: Integer; const AGrid: TJSElement;
  const AFrames: TWfcMusicEnsembleFrames);
var
  I, LDisplayCount: Integer;
  LCell: TWfcMusicVoiceCell;
  LElement, LSmall: TJSElement;
begin
  AGrid.textContent := '';
  LDisplayCount := Length(AFrames);
  if LDisplayCount > DISPLAY_CELL_LIMIT then LDisplayCount := DISPLAY_CELL_LIMIT;
  for I := 0 to LDisplayCount - 1 do
  begin
    if (AVoiceIndex < 0) or
        (AVoiceIndex >= Length(AFrames[I].Voices)) then
      raise EEnsembleStudio.Create('ensemble voice layout changed');
    LCell := AFrames[I].Voices[AVoiceIndex];
    LElement := document.createElement('div');
    LElement.className := 'voice-cell';
    case LCell.Action of
      wmcaAttack: LElement.className := LElement.className + ' attack';
      wmcaHold: LElement.className := LElement.className + ' hold';
      wmcaRest: LElement.className := LElement.className + ' rest';
    end;
    LElement.textContent := EnsembleStudioVoiceCellLabel(LCell);
    LSmall := document.createElement('small');
    LSmall.textContent := 'cell ' + IntToStr(I);
    LElement.appendChild(LSmall);
    AGrid.appendChild(LElement);
  end;
end;

procedure TBrowserEnsembleStudioApplication.RefreshVoices;
var
  LFrames: TWfcMusicEnsembleFrames;
begin
  if not FStudio.HasCurrent then
  begin
    FBassGrid.textContent := '';
    FChordsGrid.textContent := '';
    FUpperGrid.textContent := '';
    FScorePlaceholder.removeAttribute('hidden');
    document.body.setAttribute('data-voice-count', '0');
    Exit;
  end;
  LFrames := FStudio.EnsembleFrames;
  RefreshVoice(0, FBassGrid, LFrames);
  RefreshVoice(1, FChordsGrid, LFrames);
  RefreshVoice(2, FUpperGrid, LFrames);
  FScorePlaceholder.setAttribute('hidden', '');
  document.body.setAttribute('data-voice-count',
    IntToStr(ENSEMBLE_STUDIO_VOICE_COUNT));
end;

procedure TBrowserEnsembleStudioApplication.RefreshReports;
var
  I: Integer;
  LPass: TGraphPassSolveReport;
  LReport: TEnsembleStudioReport;
  LText: String;
begin
  LReport := FStudio.CopyReport;
  if LReport.Status in [essIdle, essDirty] then
  begin
    ClearReports;
    Exit;
  end;
  FScopeReport.textContent :=
    'requested: ' + PassIndicesText(LReport.RequestedRootIndices,
      'full ensemble') + #10 +
    'active: ' + PassIndicesText(LReport.ActivePassIndices, 'none') + #10 +
    'strategy: ' + StrategyName(FOptions) + #10 +
    'rounds: ' + IntToStr(LReport.Rounds) + #10 +
    'all-round decisions: ' + IntToStr(LReport.Decisions) + #10 +
    'all-round propagations: ' + IntToStr(LReport.Propagations) + #10 +
    'all-round contradictions: ' + IntToStr(LReport.Contradictions) + #10 +
    'all-round backtracks: ' + IntToStr(LReport.Backtracks) + #10 +
    'pass backtracks: ' + IntToStr(LReport.PassBacktracks);
  LText := '';
  for I := 0 to High(LReport.Passes) do
  begin
    LPass := LReport.Passes[I];
    if LText <> '' then LText := LText + #10;
    if I <= Ord(High(TWfcMusicEnsembleLayer)) then
      LText := LText + WfcMusicEnsembleLayerName(
        TWfcMusicEnsembleLayer(I))
    else LText := LText + 'pass ' + IntToStr(I);
    LText := LText + ': ' + DispositionName(LPass.Disposition) +
      ' executed=' + LowerCase(BoolToStr(LPass.Executed, True)) +
      ' decisions=' + IntToStr(LPass.Decisions) +
      ' propagations=' + IntToStr(LPass.Propagations) +
      ' contradictions=' + IntToStr(LPass.Contradictions) +
      ' backtracks=' + IntToStr(LPass.Backtracks) +
      ' trace-events=' + IntToStr(LPass.TraceCount);
  end;
  if LText = '' then LText := 'No pass outcomes were produced.';
  FPassReport.textContent := LText + #10#10 + FStudio.RunReportText;
  if LReport.Status = essSolved then
    FFailureReport.textContent := 'No terminal failure.'
  else
    FFailureReport.textContent :=
      'status=' + EnsembleStudioStatusName(LReport.Status) + #10 +
      'kind=' + ContradictionName(LReport.FailureKind) + #10 +
      'failed-pass=' + IntToStr(LReport.FailedPass) + #10 +
      'cell=' + IntToStr(LReport.FailedCell) + #10 +
      'dependency-pass=' + IntToStr(LReport.DependencyPass) + #10 +
      'trace=' + UpperCase(IntToHex(LReport.TraceHash, 8)) + #10 +
      'transcript=' + UpperCase(IntToHex(LReport.TranscriptHash, 8));
end;

procedure TBrowserEnsembleStudioApplication.RefreshArtifacts;
var
  LMidi: TWfcMidiBytes;
  LMidiByteCount: Integer;
  LMidiFailure, LMidiSignature, LScoreText: String;
begin
  DisableDownload(FScoreLink, FScoreUrl);
  DisableDownload(FMidiLink, FMidiUrl);
  ResetPreviewAudio;
  DisableDownload(FWaveLink, FWaveUrl);
  FRenderPreviewButton.disabled := not FStudio.HasCurrent;
  FAudioStatus.textContent := 'no preview';
  FAudioDetail.textContent :=
    'WAV rendering is user initiated. Long score and MIDI artifacts remain available when the bounded preview adapter declines.';
  document.body.setAttribute('data-midi-bytes', '0');
  document.body.setAttribute('data-midi-status', 'none');
  document.body.removeAttribute('data-midi-error');
  document.body.setAttribute('data-wave-bytes', '0');
  document.body.setAttribute('data-audio-ready', 'false');
  document.body.setAttribute('data-score-signature', '');
  document.body.setAttribute('data-midi-signature', '');
  document.body.setAttribute('data-wave-signature', '');
  FArtifactOutput.value := '';
  if not FStudio.HasCurrent then Exit;
  LScoreText := FStudio.ScoreText;
  SetTextDownload(FScoreLink, LScoreText, 'ensemble-score.wfcmusic',
    'text/plain;charset=utf-8', FScoreUrl);
  FArtifactOutput.value := LScoreText;
  document.body.setAttribute('data-score-signature',
    EnsembleStudioTextSignature(LScoreText));
  LMidiFailure := '';
  try
    LMidi := FStudio.MidiBytes;
    SetBinaryDownload(FMidiLink, LMidi, 'ensemble.mid', 'audio/midi',
      FMidiUrl, LMidiByteCount, LMidiSignature);
    document.body.setAttribute('data-midi-bytes', IntToStr(LMidiByteCount));
    document.body.setAttribute('data-midi-signature', LMidiSignature);
    document.body.setAttribute('data-midi-status', 'ready');
  except
    on E: EWfcMusicMidi do LMidiFailure := E.Message;
    on E: EWfcMidiSmf do LMidiFailure := E.Message;
  end;
  if LMidiFailure <> '' then
  begin
    DisableDownload(FMidiLink, FMidiUrl);
    document.body.setAttribute('data-midi-status', 'unavailable');
    document.body.setAttribute('data-midi-error', LMidiFailure);
    FAudioDetail.textContent := 'Canonical score is current. MIDI adapter unavailable: ' +
      LMidiFailure + ' WAV preview remains an independent, user-initiated action.';
  end;
end;

procedure TBrowserEnsembleStudioApplication.BuildPreview;
var
  LBytes: TWfcMusicAudioBytes;
  LByteCount, LFrameCount: Integer;
  LFailure, LSignature: String;
begin
  ResetPreviewAudio;
  DisableDownload(FWaveLink, FWaveUrl);
  document.body.setAttribute('data-wave-bytes', '0');
  document.body.setAttribute('data-wave-signature', '');
  document.body.setAttribute('data-audio-ready', 'false');
  if not FStudio.HasCurrent then
    raise EEnsembleStudio.Create('generate a current score before preview');
  if not FStudio.TryWavePreview(LBytes, LFrameCount, LFailure) then
  begin
    FAudioStatus.textContent := 'preview unavailable';
    FAudioDetail.textContent := LFailure +
      ' The current canonical score and MIDI remain available without clipping.';
    Exit;
  end;
  SetBinaryDownload(FWaveLink, LBytes, 'ensemble-preview.wav',
    'audio/wav', FWaveUrl, LByteCount, LSignature);
  FPreviewAudio.autoplay := False;
  FPreviewAudio.preload := 'none';
  FPreviewAudio.src := FWaveUrl;
  FAudioStatus.textContent := 'preview ready - press play';
  FAudioDetail.textContent := IntToStr(LFrameCount) +
    ' mono PCM16 frames at 44100 Hz; deterministic structural preview timbre.';
  document.body.setAttribute('data-wave-bytes', IntToStr(LByteCount));
  document.body.setAttribute('data-wave-signature', LSignature);
  document.body.setAttribute('data-audio-ready', 'true');
end;

procedure TBrowserEnsembleStudioApplication.RefreshAll;
begin
  RefreshVocabulary;
  RefreshLocks;
  RefreshMetrics;
  RefreshLineage;
  RefreshVoices;
  RefreshReports;
  RefreshArtifacts;
end;

procedure TBrowserEnsembleStudioApplication.InvalidateForPendingEdit(
  const AReason: String; const ASessionConfig: Boolean);
begin
  FStudio.InvalidateCurrent;
  if ASessionConfig then FSessionConfigDirty := True;
  ClearCurrentPresentation;
  ClearReports;
  RefreshVocabulary;
  RefreshLocks;
  RefreshMetrics;
  SetState('dirty', 'Inputs changed; old output cleared.', AReason);
end;

procedure TBrowserEnsembleStudioApplication.StartNewSession;
var
  LBars: Integer;
  LSeed: TGraphSeed;
begin
  FStudio.InvalidateCurrent;
  ClearCurrentPresentation;
  ClearReports;
  FSessionConfigDirty := True;
  if not TryParseSeed(FSeedInput.value, LSeed) then
    raise EConvertError.Create('seed must be decimal, $hex, or 0xhex');
  LBars := ReadBoundedInteger(FBarsInput, 'bars', 1,
    ENSEMBLE_STUDIO_MAX_BARS);
  FStudio.Reset(LSeed, LBars);
  FSessionConfigDirty := False;
  FAction := esaGenerate;
  FScopeSelect.value := 'full';
  RefreshAll;
  SetState('ready', 'New session ready.',
    'Locks and the previous generation baseline were cleared.');
end;

procedure TBrowserEnsembleStudioApplication.Generate;
var
  LAction: TEnsembleStudioAction;
  LBars: Integer;
  LOptions: TEnsembleStudioOptions;
  LSeed: TGraphSeed;
  LSolved: Boolean;
begin
  FStudio.InvalidateCurrent;
  ClearCurrentPresentation;
  ClearReports;
  if FSessionConfigDirty then
    raise EConvertError.Create(
      'seed or bars changed; start a new session before generating');
  if not TryParseSeed(FSeedInput.value, LSeed) then
    raise EConvertError.Create('seed must be decimal, $hex, or 0xhex');
  LBars := ReadBoundedInteger(FBarsInput, 'bars', 1,
    ENSEMBLE_STUDIO_MAX_BARS);
  if (LSeed <> FStudio.Seed) or (LBars <> FStudio.Bars) then
    raise EConvertError.Create(
      'seed or bars differ from this session; start a new session');
  LOptions := ReadOptions;
  LAction := SelectedAction;
  if not FStudio.HasBaseline then
  begin
    LAction := esaGenerate;
    FScopeSelect.value := 'full';
  end;
  FOptions := LOptions;
  FAction := LAction;
  LSolved := FStudio.Run(LAction, LOptions);
  try
    RefreshAll;
  except
    FStudio.InvalidateCurrent;
    ClearCurrentPresentation;
    ClearReports;
    RefreshMetrics;
    raise;
  end;
  if LSolved then
  begin
    if document.body.getAttribute('data-midi-status') = 'ready' then
      SetState('solved', 'Ensemble solved.',
        'The three public layers, canonical score, and MIDI are current. WAV preview remains user initiated.')
    else
      SetState('solved', 'Ensemble solved; MIDI unavailable.',
        'The canonical score remains current. ' +
        document.body.getAttribute('data-midi-error'));
  end
  else
    case FStudio.Status of
      essContradiction:
        SetState('failed', 'Constraints contradict.',
          'The terminal report is current; no partial score or media is exposed.');
      essSolverLimit:
        SetState('failed', 'Local search allowance exhausted.',
          'Increase the local allowance or change constraints before retrying.');
      essPassLimit:
        SetState('failed', 'Pass search allowance exhausted.',
          'Increase the pass allowance or change constraints before retrying.');
    else
      SetState('failed', 'Generation did not solve.',
        'The terminal report is current; no partial output is exposed.');
    end;
end;

procedure TBrowserEnsembleStudioApplication.AssertTest(
  const ACondition: Boolean; const AMessage: String);
begin
  if not ACondition then raise EEnsembleStudio.Create(AMessage);
end;

procedure TBrowserEnsembleStudioApplication.DispatchDomEvent(
  const AElement: TJSElement; const AEventName: String);
begin
  AElement.dispatchEvent(TJSEvent.new(AEventName));
end;

procedure TBrowserEnsembleStudioApplication.RunSelfTest;
var
  I, LAlternativeIndex, LRestIndex: Integer;
  LBaselineEnsemble, LBaselineHarmony, LBaselineRhythm,
    LPublic: TWfcModelTokens;
  LFrame: TWfcMusicEnsembleFrame;
  LLocks: TEnsembleStudioLocks;
  LReport: TEnsembleStudioReport;
begin
  document.body.setAttribute('data-self-test', 'pending');
  document.body.setAttribute('data-new-session-invalidation', 'pending');
  document.body.setAttribute('data-run-invalidation', 'pending');
  document.body.setAttribute('data-failure-clears-output', 'pending');
  document.body.setAttribute('data-selective-regeneration', 'pending');
  document.body.setAttribute('data-long-score-preserved', 'pending');
  document.body.setAttribute('data-download-metadata', 'pending');
  document.body.setAttribute('data-recovery', 'pending');
  try
    AssertTest(FStudio.HasCurrent and
      (FStudio.SignatureText =
       ENSEMBLE_STUDIO_BASELINE_COMPOSITION_SIGNATURE),
      'initial seed-zero composition changed');
    AssertTest((document.body.getAttribute('data-score-signature') =
      ENSEMBLE_STUDIO_BASELINE_SCORE_SIGNATURE) and
      (document.body.getAttribute('data-midi-signature') =
       ENSEMBLE_STUDIO_BASELINE_MIDI_SIGNATURE) and
      (document.body.getAttribute('data-midi-bytes') =
       IntToStr(ENSEMBLE_STUDIO_BASELINE_MIDI_BYTES)) and
      (document.body.getAttribute('data-wave-bytes') = '0') and
      (document.body.getAttribute('data-audio-ready') = 'false') and
      FScoreLink.hasAttribute('href') and FMidiLink.hasAttribute('href') and
      (FScoreLink.download = 'ensemble-score.wfcmusic') and
      (FMidiLink.download = 'ensemble.mid'),
      'initial artifact metadata or deferred preview changed');
    AssertTest(FAudioPlayEvents = 0,
      'preview started without a user playback action');
    document.body.setAttribute('data-download-metadata', 'passed');

    FScopeSelect.value := 'ensemble';
    DispatchDomEvent(FScopeSelect, 'change');
    FSeedInput.value := '1';
    DispatchDomEvent(FSeedInput, 'input');
    AssertTest((not FStudio.HasCurrent) and FStudio.HasBaseline and
      (document.body.getAttribute('data-cell-count') = '0') and
      (not FScoreLink.hasAttribute('href')) and
      (not FMidiLink.hasAttribute('href')) and
      (document.body.getAttribute('data-audio-ready') = 'false'),
      'seed edit retained stale output or erased the hidden baseline');
    DispatchDomEvent(FNewSessionButton, 'click');
    AssertTest((FStudio.Seed = 1) and (FStudio.Bars = 2) and
      not FStudio.HasBaseline and (Length(FStudio.CopyLocks) = 0),
      'new session did not clear baseline and locks');
    FSeedInput.value := '0';
    DispatchDomEvent(FSeedInput, 'input');
    DispatchDomEvent(FNewSessionButton, 'click');
    DispatchDomEvent(FGenerateButton, 'click');
    AssertTest(FStudio.HasCurrent and (FScopeSelect.value = 'full') and
      (FStudio.SignatureText =
       ENSEMBLE_STUDIO_BASELINE_COMPOSITION_SIGNATURE),
      'new-session event path retained stale repair scope or lost baseline');
    document.body.setAttribute('data-new-session-invalidation', 'passed');

    FSeedInput.value := '0xZZ';
    DispatchDomEvent(FSeedInput, 'input');
    DispatchDomEvent(FNewSessionButton, 'click');
    AssertTest((not FStudio.HasCurrent) and
      (document.body.getAttribute('data-state') = 'error'),
      'malformed seed did not fail without current output');
    FSeedInput.value := '0';
    FBarsInput.value := '2x';
    DispatchDomEvent(FBarsInput, 'input');
    DispatchDomEvent(FNewSessionButton, 'click');
    AssertTest((not FStudio.HasCurrent) and
      (document.body.getAttribute('data-state') = 'error'),
      'trailing junk in bars was accepted');
    FBarsInput.value := '2';
    DispatchDomEvent(FBarsInput, 'input');
    DispatchDomEvent(FNewSessionButton, 'click');
    DispatchDomEvent(FGenerateButton, 'click');
    AssertTest(FStudio.HasCurrent and
      (FStudio.SignatureText =
       ENSEMBLE_STUDIO_BASELINE_COMPOSITION_SIGNATURE),
      'session did not recover after malformed input');

    FBarsInput.value := '31';
    DispatchDomEvent(FBarsInput, 'input');
    DispatchDomEvent(FNewSessionButton, 'click');
    DispatchDomEvent(FGenerateButton, 'click');
    AssertTest(FStudio.HasCurrent and (FStudio.CellCount = 248) and
      (document.body.getAttribute('data-cell-count') = '248') and
      (document.body.getAttribute('data-displayed-cell-count') =
       IntToStr(DISPLAY_CELL_LIMIT)) and
      FScoreLink.hasAttribute('href') and FMidiLink.hasAttribute('href'),
      'long requested score was capped or lost before export');
    DispatchDomEvent(FRenderPreviewButton, 'click');
    AssertTest(FStudio.HasCurrent and FScoreLink.hasAttribute('href') and
      FMidiLink.hasAttribute('href') and
      (document.body.getAttribute('data-audio-ready') = 'false') and
      (document.body.getAttribute('data-wave-bytes') = '0') and
      (not FWaveLink.hasAttribute('href')),
      'bounded preview failure damaged the valid long score');
    document.body.setAttribute('data-long-score-preserved', 'passed');

    FBarsInput.value := '2';
    DispatchDomEvent(FBarsInput, 'input');
    DispatchDomEvent(FNewSessionButton, 'click');
    DispatchDomEvent(FGenerateButton, 'click');
    FBacktracksInput.value := '255';
    DispatchDomEvent(FBacktracksInput, 'input');
    AssertTest((not FStudio.HasCurrent) and FStudio.HasBaseline and
      (not FScoreLink.hasAttribute('href')) and
      (not FMidiLink.hasAttribute('href')),
      'run edit retained stale artifacts or erased repair baseline');
    DispatchDomEvent(FGenerateButton, 'click');
    AssertTest(FStudio.HasCurrent, 'run edit did not recover');
    document.body.setAttribute('data-run-invalidation', 'passed');

    LBaselineEnsemble := FStudio.CellTokens(wmelEnsemble);
    LBaselineHarmony := FStudio.CellTokens(wmelHarmony);
    LBaselineRhythm := FStudio.CellTokens(wmelRhythm);
    LPublic := FStudio.PublicTokens(wmelEnsemble);
    LAlternativeIndex := -1;
    for I := 0 to High(LPublic) do
    begin
      LFrame := DecodeWfcMusicEnsembleFrame(LPublic[I]);
      if (LPublic[I] <> LBaselineEnsemble[0]) and
          WfcMusicEnsembleFrameCanStart(LFrame) and
          (EncodeWfcMusicRhythmFrame(
            ProjectWfcMusicEnsembleFrameToRhythm(LFrame)) =
           LBaselineRhythm[0]) and
          (EncodeWfcMusicPitchClassSet(
            ProjectWfcMusicEnsembleFrameToPitchClassSet(LFrame, 12)) =
           LBaselineHarmony[0]) then
      begin
        LAlternativeIndex := I;
        Break;
      end;
    end;
    AssertTest(LAlternativeIndex >= 0,
      'compatible alternate ensemble opening is unavailable');
    FLockLayerSelect.value := 'ensemble';
    DispatchDomEvent(FLockLayerSelect, 'change');
    FLockCellInput.value := '0';
    FLockTokenSelect.value := IntToStr(
      FindVocabularyToken(LPublic[LAlternativeIndex]));
    DispatchDomEvent(FAddLockButton, 'click');
    LLocks := FStudio.CopyLocks;
    AssertTest((Length(LLocks) = 1) and
      (LLocks[0].Token = LPublic[LAlternativeIndex]) and
      (not FStudio.HasCurrent),
      'selected nonfirst token changed during lock refresh');
    FScopeSelect.value := 'ensemble';
    DispatchDomEvent(FScopeSelect, 'change');
    DispatchDomEvent(FGenerateButton, 'click');
    LReport := FStudio.CopyReport;
    AssertTest(FStudio.HasCurrent and
      (FStudio.CellTokens(wmelEnsemble)[0] = LPublic[LAlternativeIndex]) and
      (FStudio.SignatureText <>
       ENSEMBLE_STUDIO_BASELINE_COMPOSITION_SIGNATURE) and
      (Length(LReport.ActivePassIndices) = 1) and
      (LReport.ActivePassIndices[0] = Ord(wmelEnsemble)),
      'ensemble-only repair did not retain the explicit public lock');
    document.body.setAttribute('data-selective-regeneration', 'passed');

    LPublic := FStudio.PublicTokens(wmelEnsemble);
    LRestIndex := -1;
    for I := 0 to High(LPublic) do
      if DecodeWfcMusicEnsembleFrame(LPublic[I]).Voices[0].Action =
          wmcaRest then
      begin
        LRestIndex := I;
        Break;
      end;
    AssertTest(LRestIndex >= 0,
      'rest ensemble token is unavailable for failure test');
    FLockTokenSelect.value := IntToStr(
      FindVocabularyToken(LPublic[LRestIndex]));
    DispatchDomEvent(FAddLockButton, 'click');
    FScopeSelect.value := 'full';
    DispatchDomEvent(FScopeSelect, 'change');
    DispatchDomEvent(FGenerateButton, 'click');
    AssertTest((not FStudio.HasCurrent) and FStudio.HasBaseline and
      (document.body.getAttribute('data-state') = 'failed') and
      (document.body.getAttribute('data-cell-count') = '0') and
      (not FScoreLink.hasAttribute('href')) and
      (not FMidiLink.hasAttribute('href')) and
      (not FWaveLink.hasAttribute('href')) and
      (document.body.getAttribute('data-audio-ready') = 'false'),
      'failed solve exposed stale score, MIDI, or preview');
    document.body.setAttribute('data-failure-clears-output', 'passed');

    DispatchDomEvent(FClearLocksButton, 'click');
    FScopeSelect.value := 'full';
    DispatchDomEvent(FScopeSelect, 'change');
    DispatchDomEvent(FGenerateButton, 'click');
    AssertTest(FStudio.HasCurrent and
      (FStudio.SignatureText =
       ENSEMBLE_STUDIO_BASELINE_COMPOSITION_SIGNATURE) and
      (Length(FStudio.CopyLocks) = 0),
      'clearing locks did not restore the exact baseline');
    DispatchDomEvent(FRenderPreviewButton, 'click');
    AssertTest((document.body.getAttribute('data-wave-signature') =
      ENSEMBLE_STUDIO_BASELINE_WAVE_SIGNATURE) and
      (document.body.getAttribute('data-wave-bytes') =
       IntToStr(ENSEMBLE_STUDIO_BASELINE_WAVE_BYTES)) and
      (document.body.getAttribute('data-audio-ready') = 'true') and
      FWaveLink.hasAttribute('href') and
      (FWaveLink.download = 'ensemble-preview.wav') and
      (not FPreviewAudio.autoplay) and (FAudioPlayEvents = 0),
      'explicit baseline preview metadata or no-autoplay state changed');
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

function TBrowserEnsembleStudioApplication.HandleSessionInput(
  AEvent: TJSEvent): Boolean;
begin
  Result := False;
  try
    InvalidateForPendingEdit(
      'Start a new session to apply the edited seed and bar count.', True);
  except on E: Exception do ShowError(E.Message); end;
end;

function TBrowserEnsembleStudioApplication.HandleNewSession(
  AEvent: TJSMouseEvent): Boolean;
begin
  Result := False;
  try StartNewSession; except on E: Exception do ShowError(E.Message); end;
end;

function TBrowserEnsembleStudioApplication.HandleGenerate(
  AEvent: TJSMouseEvent): Boolean;
begin
  Result := False;
  try
    Generate;
  except
    on E: Exception do
    begin
      if (FStudio.Status = essDirty) and not FStudio.HasBaseline then
        FSessionConfigDirty := True;
      RefreshMetrics;
      ShowError(E.Message);
    end;
  end;
end;

function TBrowserEnsembleStudioApplication.HandleRunInput(
  AEvent: TJSEvent): Boolean;
begin
  Result := False;
  try
    InvalidateForPendingEdit(
      'Generate again when the edited strategy, scope, allowances, and trace setting are ready.',
      False);
  except on E: Exception do ShowError(E.Message); end;
end;

function TBrowserEnsembleStudioApplication.HandleStrategyChange(
  AEvent: TJSEvent): Boolean;
begin
  Result := False;
  try
    if FStrategySelect.value = 'one-way' then
      FPassBacktracksInput.value := '0'
    else if (FStrategySelect.value = 'negotiated') and
        (Trim(FPassBacktracksInput.value) = '0') then
      FPassBacktracksInput.value := IntToStr(
        DefaultEnsembleStudioOptions.MaxPassBacktracks);
    InvalidateForPendingEdit(
      'Generate again with the selected pass strategy and allowances.', False);
  except on E: Exception do ShowError(E.Message); end;
end;

function TBrowserEnsembleStudioApplication.HandleLockLayerChange(
  AEvent: TJSEvent): Boolean;
begin
  Result := False;
  try RefreshVocabulary; except on E: Exception do ShowError(E.Message); end;
end;

function TBrowserEnsembleStudioApplication.HandleAddLock(
  AEvent: TJSMouseEvent): Boolean;
var
  I, LPosition, LTokenIndex: Integer;
  LLayer: TWfcMusicEnsembleLayer;
  LLocks: TEnsembleStudioLocks;
  LToken: TWfcModelToken;
begin
  Result := False;
  try
    if FSessionConfigDirty then
      raise EEnsembleStudio.Create(
        'start a new session before editing locks for the new grid');
    LLayer := SelectedLayer;
    LPosition := ReadBoundedInteger(FLockCellInput, 'lock cell', 0,
      FStudio.CellCount - 1);
    LTokenIndex := SelectedVocabularyIndex;
    LToken := FVocabulary[LTokenIndex];
    FStudio.SetLock(LLayer, LPosition, LToken);
    ClearCurrentPresentation;
    ClearReports;
    RefreshVocabulary;
    RefreshLocks;
    RefreshMetrics;
    LLocks := FStudio.CopyLocks;
    for I := 0 to High(LLocks) do
      if (LLocks[I].Layer = LLayer) and
          (LLocks[I].Position = LPosition) then
      begin FLockList.selectedIndex := I; Break; end;
    SetState('dirty', 'Public lock updated; old output cleared.',
      'Regenerate the selected scope to apply the sorted lock set.');
  except on E: Exception do ShowError(E.Message); end;
end;

function TBrowserEnsembleStudioApplication.HandleRemoveLock(
  AEvent: TJSMouseEvent): Boolean;
var
  LIndex: Integer;
  LLocks: TEnsembleStudioLocks;
begin
  Result := False;
  try
    if FSessionConfigDirty then
      raise EEnsembleStudio.Create('start a new session before editing locks');
    LLocks := FStudio.CopyLocks;
    LIndex := FLockList.selectedIndex;
    if (LIndex < 0) or (LIndex >= Length(LLocks)) then
      raise ERangeError.Create('select a public lock to remove');
    FStudio.ClearLock(LLocks[LIndex].Layer, LLocks[LIndex].Position);
    ClearCurrentPresentation;
    ClearReports;
    RefreshVocabulary;
    RefreshLocks;
    RefreshMetrics;
    SetState('dirty', 'Public lock removed; old output cleared.',
      'Regenerate to apply the remaining locks.');
  except on E: Exception do ShowError(E.Message); end;
end;

function TBrowserEnsembleStudioApplication.HandleClearLocks(
  AEvent: TJSMouseEvent): Boolean;
begin
  Result := False;
  try
    if FSessionConfigDirty then
      raise EEnsembleStudio.Create('start a new session before editing locks');
    FStudio.ClearLocks;
    ClearCurrentPresentation;
    ClearReports;
    RefreshVocabulary;
    RefreshLocks;
    RefreshMetrics;
    SetState('dirty', 'All public locks cleared; old output cleared.',
      'Regenerate for an unconstrained ensemble.');
  except on E: Exception do ShowError(E.Message); end;
end;

function TBrowserEnsembleStudioApplication.HandleCellClick(
  AEvent: TJSMouseEvent): Boolean;
var
  LElement: TJSElement;
  LIndex, LTokenIndex: Integer;
  LLayer: TWfcMusicEnsembleLayer;
  LTokens: TWfcModelTokens;
begin
  Result := False;
  try
    LElement := TJSElement(AEvent.currentTarget);
    if not TryStrToInt(LElement.getAttribute('data-index'), LIndex) or
        (LIndex < 0) or (LIndex >= FStudio.CellCount) then
      raise ERangeError.Create('selected ensemble cell is invalid');
    FLockLayerSelect.value := LElement.getAttribute('data-layer');
    LLayer := SelectedLayer;
    FLockCellInput.value := IntToStr(LIndex);
    RefreshVocabulary;
    LTokens := FStudio.CellTokens(LLayer);
    LTokenIndex := FindVocabularyToken(LTokens[LIndex]);
    if LTokenIndex >= 0 then
      FLockTokenSelect.value := IntToStr(LTokenIndex);
    FStatusDetailElement.textContent := 'Selected ' +
      WfcMusicEnsembleLayerName(LLayer) + ' cell ' + IntToStr(LIndex) +
      ' for an optional public lock.';
  except on E: Exception do ShowError(E.Message); end;
end;

function TBrowserEnsembleStudioApplication.HandleRenderPreview(
  AEvent: TJSMouseEvent): Boolean;
begin
  Result := False;
  try BuildPreview; except on E: Exception do ShowError(E.Message); end;
end;

function TBrowserEnsembleStudioApplication.HandleAudioPlay(
  AEvent: TJSPointerEvent): Boolean;
begin
  Result := False;
  Inc(FAudioPlayEvents);
  document.body.setAttribute('data-audio-play-events',
    IntToStr(FAudioPlayEvents));
  FAudioStatus.textContent := 'playing project-owned preview';
end;

function TBrowserEnsembleStudioApplication.HandleAudioEnded(
  AEvent: TJSEvent): Boolean;
begin
  Result := False;
  if FStudio.HasCurrent then
    FAudioStatus.textContent := 'preview ready - press play';
end;

function TBrowserEnsembleStudioApplication.HandleAudioError(
  AEvent: TJSErrorEvent): Boolean;
begin
  Result := False;
  FAudioStatus.textContent := 'browser could not decode preview';
  document.body.setAttribute('data-audio-ready', 'false');
end;

procedure TBrowserEnsembleStudioApplication.Run;
begin
  try
    BindDocument;
    BindEvents;
    WriteDefaultOptions;
    document.body.setAttribute('data-audio-play-events', '0');
    StartNewSession;
    Generate;
    FStreamController := TBrowserEnsembleStreamController.Create;
    FStreamController.Run;
    if Pos('selftest=1', window.location.search) > 0 then
    begin
      RunSelfTest;
      FStreamController.RunSelfTest;
    end
    else document.body.setAttribute('data-self-test', 'not-requested');
  except on E: Exception do ShowError(E.Message); end;
end;

procedure InstallEnsembleStudioBrowserTestFixture;
begin
  TJSHTMLElement(document.body).innerHTML :=
    '<input id="seed-input" inputmode="text" value="0">' +
    '<input id="bars-input" type="number" min="1" step="1" value="2">' +
    '<button id="new-session-button" type="button"></button>' +
    '<button id="generate-button" type="button"></button>' +
    '<select id="strategy-select"><option value="negotiated">n</option>' +
      '<option value="one-way">o</option></select>' +
    '<select id="scope-select"><option value="full">f</option>' +
      '<option value="harmony">h</option><option value="rhythm">r</option>' +
      '<option value="ensemble">e</option></select>' +
    '<input id="backtracks-input" type="number" min="0" step="1" value="256">' +
    '<input id="pass-backtracks-input" type="number" min="0" step="1" value="16">' +
    '<input id="trace-input" type="checkbox">' +
    '<span id="status"></span><span id="status-detail"></span>' +
    '<span id="session-summary"></span><span id="session-output"></span>' +
    '<span id="result-status"></span><span id="strategy-output"></span>' +
    '<span id="scope-output"></span><span id="local-backtracks-output"></span>' +
    '<span id="pass-backtracks-output"></span><span id="pass-count-output"></span>' +
    '<span id="lock-count-output"></span><span id="composition-signature"></span>' +
    '<div id="harmony-grid"></div><div id="rhythm-grid"></div>' +
    '<div id="ensemble-grid"></div><p id="display-window-note" hidden></p>' +
    '<p id="lineage-placeholder"></p>' +
    '<div id="bass-grid"></div><div id="chords-grid"></div>' +
    '<div id="upper-grid"></div><p id="score-placeholder"></p>' +
    '<select id="lock-layer-select"><option value="harmony">h</option>' +
      '<option value="rhythm">r</option><option value="ensemble">e</option></select>' +
    '<input id="lock-cell-input" type="number" min="0" step="1" value="0">' +
    '<select id="lock-token-select"></select><select id="lock-list"></select>' +
    '<button id="add-lock-button"></button><button id="remove-lock-button"></button>' +
    '<button id="clear-locks-button"></button>' +
    '<pre id="scope-report"></pre><pre id="pass-report"></pre>' +
    '<pre id="failure-report"></pre><span id="audio-status"></span>' +
    '<span id="audio-detail"></span><div><button id="render-preview-button" type="button"></button>' +
    '<audio id="preview-audio" controls preload="none"></audio></div>' +
    '<a id="download-score-link"></a><a id="download-midi-link"></a>' +
    '<a id="download-wav-link"></a><textarea id="artifact-output"></textarea>' +
    '<input id="stream-seconds-input" inputmode="decimal" autocomplete="off" value="6.125">' +
    '<input id="stream-segment-cells-input" type="number" min="1" step="1" value="5">' +
    '<input id="stream-backtracks-input" type="number" min="0" step="1" value="256">' +
    '<input id="stream-pass-backtracks-input" type="number" min="0" step="1" value="16">' +
    '<input id="stream-trace-input" type="checkbox">' +
    '<button id="stream-start-button" type="button"></button>' +
    '<button id="stream-midi-plan-button" type="button"></button>' +
    '<button id="stream-midi-save-button" type="button" disabled></button>' +
    '<button id="stream-cancel-button" type="button"></button>' +
    '<progress id="stream-progress" max="1" value="0"></progress>' +
    '<span id="stream-status"></span><span id="stream-detail"></span>' +
    '<span id="stream-plan"></span><pre id="stream-fallback"></pre>' +
    '<pre id="failure"></pre>';
end;

end.
