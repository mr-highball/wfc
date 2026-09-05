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
unit browser_music_studio_app;

{$mode delphi}{$H+}

interface

uses
  JS,
  Web,
  SysUtils,
  wfc,
  wfc_model,
  wfc_music,
  wfc_music_sequence,
  wfc_music_passes,
  music_studio_workbench;

type
  { Browser-only DOM, Blob, and interaction glue. The workbench remains the
    sole generation, lock, report, and canonical-artifact authority. }
  TBrowserMusicStudioApplication = class
  strict private
    FStudio: TWfcMusicStudio;
    FOptions: TWfcMusicStudioOptions;
    FAction: TWfcMusicStudioAction;
    FVocabulary: TWfcModelTokens;
    FAudioPlayEvents: Integer;
    FCompositionUrl, FScoreUrl, FMidiUrl, FWaveUrl: String;

    FSeedInput: TJSHTMLInputElement;
    FNewSessionButton, FGenerateButton: TJSHTMLButtonElement;
    FStrategySelect, FScopeSelect: TJSHTMLSelectElement;
    FBacktracksInput, FPassBacktracksInput, FTraceInput: TJSHTMLInputElement;
    FMotifButton: TJSHTMLButtonElement;

    FStatusElement, FStatusDetailElement, FSeedOutput,
      FResultStatusOutput, FStrategyOutput, FScopeOutput,
      FLocalBacktracksOutput, FPassBacktracksOutput,
      FPassCountOutput, FLockCountOutput, FSignatureOutput: TJSElement;
    FHarmonyGrid, FRhythmGrid, FMelodyGrid, FLayersPlaceholder,
      FPianoRoll, FPitchSummary: TJSElement;

    FLockLayerSelect: TJSHTMLSelectElement;
    FLockCellInput: TJSHTMLInputElement;
    FLockTokenSelect, FLockList: TJSHTMLSelectElement;
    FAddLockButton, FRemoveLockButton, FClearLocksButton: TJSHTMLButtonElement;

    FRepairReport, FPassReport, FFailureReport, FAudioStatus: TJSElement;
    FPreviewAudio: TJSHTMLAudioElement;
    FCompositionLink, FScoreLink, FMidiLink, FWaveLink: TJSHTMLAnchorElement;
    FArtifactSelect: TJSHTMLSelectElement;
    FArtifactOutput: TJSHTMLTextAreaElement;

    function RequireElement(const AId: String): TJSElement;
    procedure BindDocument;
    procedure BindEvents;
    procedure WriteDefaultOptions;
    procedure SetState(const AState, AStatus, ADetail: String);
    procedure ShowError(const AMessage: String);

    function TryParseSeed(const AText: String; out ASeed: TGraphSeed): Boolean;
    function ReadBoundedInteger(const AInput: TJSHTMLInputElement;
      const AName: String; const AMinimum, AMaximum: Integer): Integer;
    function ReadOptions: TWfcMusicStudioOptions;
    function SelectedAction: TWfcMusicStudioAction;
    function SelectedLayer: TWfcMusicPassLayer;
    function SelectedVocabularyIndex: Integer;
    function FindVocabularyToken(const AToken: TWfcModelToken): Integer;
    function LayerName(const ALayer: TWfcMusicPassLayer): String;
    function ActionName(const AAction: TWfcMusicStudioAction): String;
    function StrategyName(const AOptions: TWfcMusicStudioOptions): String;
    function DispositionName(const ADisposition: TGraphPassDisposition): String;
    function ContradictionName(const AKind: TGraphContradictionKind): String;
    function PassIndicesText(const AIndices: TGraphPassIndices;
      const AEmptyText: String): String;

    procedure RevokeUrl(var AUrl: String);
    procedure DisableDownload(const ALink: TJSHTMLAnchorElement;
      var AUrl: String);
    procedure SetTextDownload(const ALink: TJSHTMLAnchorElement;
      const AText, AFileName, AMime: String; var AUrl: String);
    procedure SetBinaryDownload(const ALink: TJSHTMLAnchorElement;
      const ABytes: array of Byte; const AFileName, AMime: String;
      var AUrl: String; out AByteCount: Integer; out ASignature: String);
    procedure ClearCurrentPresentation;
    procedure ClearReports;
    procedure ResetPreviewAudio;
    procedure RefreshAll;
    procedure RefreshMetrics;
    procedure RefreshVocabulary;
    procedure RefreshLocks;
    procedure RefreshLayers;
    procedure RefreshLayer(const ALayer: TWfcMusicPassLayer;
      const AGrid: TJSElement);
    procedure RefreshPianoRoll;
    procedure RefreshReports;
    procedure RefreshArtifacts;
    procedure RefreshArtifactInspector;
    function LockIndex(const ALayer: TWfcMusicPassLayer;
      const APosition: Integer): Integer;
    function ReportPassCount(const AReport: TWfcMusicStudioReport): Integer;

    procedure InvalidateForPendingEdit(const AReason: String);
    procedure StartNewSession;
    procedure Generate;
    procedure AssertTest(const ACondition: Boolean; const AMessage: String);
    procedure DispatchDomEvent(const AElement: TJSElement;
      const AEventName: String);
    procedure RunSelfTest;

    function HandleSeedInput(AEvent: TJSEvent): Boolean;
    function HandleNewSession(AEvent: TJSMouseEvent): Boolean;
    function HandleGenerate(AEvent: TJSMouseEvent): Boolean;
    function HandleRunInput(AEvent: TJSEvent): Boolean;
    function HandleStrategyChange(AEvent: TJSEvent): Boolean;
    function HandleMotif(AEvent: TJSMouseEvent): Boolean;
    function HandleLockLayerChange(AEvent: TJSEvent): Boolean;
    function HandleAddLock(AEvent: TJSMouseEvent): Boolean;
    function HandleRemoveLock(AEvent: TJSMouseEvent): Boolean;
    function HandleClearLocks(AEvent: TJSMouseEvent): Boolean;
    function HandleCellClick(AEvent: TJSMouseEvent): Boolean;
    function HandleArtifactChange(AEvent: TJSEvent): Boolean;
    function HandleAudioPlay(AEvent: TJSPointerEvent): Boolean;
    function HandleAudioEnded(AEvent: TJSEvent): Boolean;
    function HandleAudioError(AEvent: TJSErrorEvent): Boolean;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Run;
  end;

implementation

uses
  wfc_music_audio;

const
  MAX_SEED = Cardinal($FFFFFFFF);
  OPENING_MOTIF_CELLS = 2;
  BASELINE_SIGNATURE = '216F6EBB';
  BASELINE_SCORE_SIGNATURE = '4167E7E5';
  BASELINE_MIDI_SIGNATURE = '86E4DCA3';
  BASELINE_WAVE_SIGNATURE = '64679FF8';
  REPAIR_SIGNATURE = '1C1075DB';
  REPAIR_SCORE_SIGNATURE = '3690AE9B';
  REPAIR_MIDI_SIGNATURE = '93A9B159';
  REPAIR_WAVE_SIGNATURE = '73A8591A';
  REPAIR_TRANSCRIPT = '4A9D9975';
  CONFLICTING_MELODY_TOKEN = 'wm1:a:67:96';

constructor TBrowserMusicStudioApplication.Create;
begin
  inherited Create;
  FStudio := TWfcMusicStudio.Create(0);
  FOptions := DefaultMusicStudioOptions;
  FAction := msaGenerate;
  FVocabulary := nil;
  FAudioPlayEvents := 0;
  FCompositionUrl := '';
  FScoreUrl := '';
  FMidiUrl := '';
  FWaveUrl := '';
end;

destructor TBrowserMusicStudioApplication.Destroy;
begin
  RevokeUrl(FCompositionUrl);
  RevokeUrl(FScoreUrl);
  RevokeUrl(FMidiUrl);
  RevokeUrl(FWaveUrl);
  FStudio.Free;
  inherited Destroy;
end;

function TBrowserMusicStudioApplication.RequireElement(
  const AId: String): TJSElement;
begin
  Result := document.getElementById(AId);
  if not Assigned(Result) then
    raise EMusicStudio.Create('music studio is missing #' + AId);
end;

procedure TBrowserMusicStudioApplication.BindDocument;
begin
  FSeedInput := TJSHTMLInputElement(RequireElement('seed-input'));
  FNewSessionButton := TJSHTMLButtonElement(RequireElement('new-session-button'));
  FGenerateButton := TJSHTMLButtonElement(RequireElement('generate-button'));
  FStrategySelect := TJSHTMLSelectElement(RequireElement('strategy-select'));
  FScopeSelect := TJSHTMLSelectElement(RequireElement('scope-select'));
  FBacktracksInput := TJSHTMLInputElement(RequireElement('backtracks-input'));
  FPassBacktracksInput := TJSHTMLInputElement(
    RequireElement('pass-backtracks-input'));
  FTraceInput := TJSHTMLInputElement(RequireElement('trace-input'));
  FMotifButton := TJSHTMLButtonElement(RequireElement('motif-button'));

  FStatusElement := RequireElement('status');
  FStatusDetailElement := RequireElement('status-detail');
  FSeedOutput := RequireElement('seed-output');
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
  FMelodyGrid := RequireElement('melody-grid');
  FLayersPlaceholder := RequireElement('layers-placeholder');
  FPianoRoll := RequireElement('piano-roll');
  FPitchSummary := RequireElement('pitch-summary');
  FLockLayerSelect := TJSHTMLSelectElement(RequireElement('lock-layer-select'));
  FLockCellInput := TJSHTMLInputElement(RequireElement('lock-cell-input'));
  FLockTokenSelect := TJSHTMLSelectElement(RequireElement('lock-token-select'));
  FAddLockButton := TJSHTMLButtonElement(RequireElement('add-lock-button'));
  FLockList := TJSHTMLSelectElement(RequireElement('lock-list'));
  FRemoveLockButton := TJSHTMLButtonElement(RequireElement('remove-lock-button'));
  FClearLocksButton := TJSHTMLButtonElement(RequireElement('clear-locks-button'));
  FRepairReport := RequireElement('repair-report');
  FPassReport := RequireElement('pass-report');
  FFailureReport := RequireElement('failure-report');
  FAudioStatus := RequireElement('audio-status');
  FPreviewAudio := TJSHTMLAudioElement(RequireElement('preview-audio'));
  FCompositionLink := TJSHTMLAnchorElement(
    RequireElement('download-composition-link'));
  FScoreLink := TJSHTMLAnchorElement(RequireElement('download-score-link'));
  FMidiLink := TJSHTMLAnchorElement(RequireElement('download-midi-link'));
  FWaveLink := TJSHTMLAnchorElement(RequireElement('download-wav-link'));
  FArtifactSelect := TJSHTMLSelectElement(RequireElement('artifact-select'));
  FArtifactOutput := TJSHTMLTextAreaElement(RequireElement('artifact-output'));
end;

procedure TBrowserMusicStudioApplication.BindEvents;
begin
  FSeedInput.oninput := @HandleSeedInput;
  FNewSessionButton.onclick := @HandleNewSession;
  FGenerateButton.onclick := @HandleGenerate;
  FStrategySelect.onchange := @HandleStrategyChange;
  FScopeSelect.onchange := @HandleRunInput;
  FBacktracksInput.oninput := @HandleRunInput;
  FPassBacktracksInput.oninput := @HandleRunInput;
  FTraceInput.onchange := @HandleRunInput;
  FMotifButton.onclick := @HandleMotif;
  FLockLayerSelect.onchange := @HandleLockLayerChange;
  FAddLockButton.onclick := @HandleAddLock;
  FRemoveLockButton.onclick := @HandleRemoveLock;
  FClearLocksButton.onclick := @HandleClearLocks;
  FArtifactSelect.onchange := @HandleArtifactChange;
  FPreviewAudio.onplay := @HandleAudioPlay;
  FPreviewAudio.onended := @HandleAudioEnded;
  FPreviewAudio.onerror := @HandleAudioError;
end;

procedure TBrowserMusicStudioApplication.WriteDefaultOptions;
begin
  FOptions := DefaultMusicStudioOptions;
  if FOptions.Negotiated then FStrategySelect.value := 'negotiated'
  else FStrategySelect.value := 'one-way';
  FScopeSelect.value := 'full';
  FBacktracksInput.value := IntToStr(FOptions.MaxBacktracks);
  FPassBacktracksInput.value := IntToStr(FOptions.MaxPassBacktracks);
  FTraceInput.checked := FOptions.CaptureTrace;
end;

procedure TBrowserMusicStudioApplication.SetState(
  const AState, AStatus, ADetail: String);
begin
  document.body.setAttribute('data-state', AState);
  if AState <> 'error' then document.body.removeAttribute('data-error');
  FStatusElement.textContent := AStatus;
  FStatusDetailElement.textContent := ADetail;
end;

procedure TBrowserMusicStudioApplication.ShowError(const AMessage: String);
begin
  try RefreshAll; except end;
  document.body.setAttribute('data-error', AMessage);
  SetState('error', 'Error: ' + AMessage,
    'Any output invalidated by this operation remains unavailable.');
end;

function TBrowserMusicStudioApplication.TryParseSeed(
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

function TBrowserMusicStudioApplication.ReadBoundedInteger(
  const AInput: TJSHTMLInputElement; const AName: String;
  const AMinimum, AMaximum: Integer): Integer;
begin
  if not TryStrToInt(Trim(AInput.value), Result) or
      (Result < AMinimum) or (Result > AMaximum) then
    raise EConvertError.CreateFmt('%s must be from %d through %d',
      [AName, AMinimum, AMaximum]);
end;

function TBrowserMusicStudioApplication.ReadOptions: TWfcMusicStudioOptions;
begin
  Result := DefaultMusicStudioOptions;
  if FStrategySelect.value = 'negotiated' then Result.Negotiated := True
  else if FStrategySelect.value = 'one-way' then Result.Negotiated := False
  else raise EConvertError.Create('select a known generation strategy');
  Result.MaxBacktracks := ReadBoundedInteger(FBacktracksInput,
    'local backtrack budget', 0, 1024);
  Result.MaxPassBacktracks := ReadBoundedInteger(FPassBacktracksInput,
    'pass backtrack budget', 0, 32);
  if (not Result.Negotiated) and (Result.MaxPassBacktracks <> 0) then
    raise EConvertError.Create(
      'one-way generation requires a zero pass-backtrack budget');
  Result.CaptureTrace := FTraceInput.checked;
end;

function TBrowserMusicStudioApplication.SelectedAction: TWfcMusicStudioAction;
begin
  if FScopeSelect.value = 'full' then Result := msaGenerate
  else if FScopeSelect.value = 'harmony' then Result := msaHarmony
  else if FScopeSelect.value = 'rhythm' then Result := msaRhythm
  else if FScopeSelect.value = 'melody' then Result := msaMelody
  else raise EConvertError.Create('select a known regeneration scope');
end;

function TBrowserMusicStudioApplication.SelectedLayer: TWfcMusicPassLayer;
begin
  if FLockLayerSelect.value = 'harmony' then Result := wmplHarmony
  else if FLockLayerSelect.value = 'rhythm' then Result := wmplRhythm
  else if FLockLayerSelect.value = 'melody' then Result := wmplMelody
  else raise EConvertError.Create('select a known public layer');
end;

function TBrowserMusicStudioApplication.SelectedVocabularyIndex: Integer;
begin
  if not TryStrToInt(FLockTokenSelect.value, Result) or
      (Result < 0) or (Result >= Length(FVocabulary)) then
    raise EConvertError.Create('select a public token');
end;

function TBrowserMusicStudioApplication.FindVocabularyToken(
  const AToken: TWfcModelToken): Integer;
begin
  for Result := 0 to High(FVocabulary) do
    if FVocabulary[Result] = AToken then Exit;
  Result := -1;
end;

function TBrowserMusicStudioApplication.LayerName(
  const ALayer: TWfcMusicPassLayer): String;
begin Result := WfcMusicPassLayerName(ALayer); end;

function TBrowserMusicStudioApplication.ActionName(
  const AAction: TWfcMusicStudioAction): String;
begin
  case AAction of
    msaGenerate: Result := 'full';
    msaHarmony: Result := 'harmony';
    msaRhythm: Result := 'rhythm';
    msaMelody: Result := 'melody';
  else Result := 'unknown';
  end;
end;

function TBrowserMusicStudioApplication.StrategyName(
  const AOptions: TWfcMusicStudioOptions): String;
begin
  if AOptions.Negotiated then Result := 'negotiated' else Result := 'one-way';
end;

function TBrowserMusicStudioApplication.DispositionName(
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

function TBrowserMusicStudioApplication.ContradictionName(
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
  else Result := 'unknown';
  end;
end;

function TBrowserMusicStudioApplication.PassIndicesText(
  const AIndices: TGraphPassIndices; const AEmptyText: String): String;
var I: Integer;
begin
  if Length(AIndices) = 0 then Exit(AEmptyText);
  Result := '';
  for I := 0 to High(AIndices) do
  begin
    if I > 0 then Result := Result + ', ';
    if (AIndices[I] >= Ord(Low(TWfcMusicPassLayer))) and
        (AIndices[I] <= Ord(High(TWfcMusicPassLayer))) then
      Result := Result + LayerName(TWfcMusicPassLayer(AIndices[I]))
    else Result := Result + 'pass ' + IntToStr(AIndices[I]);
  end;
end;

procedure TBrowserMusicStudioApplication.RevokeUrl(var AUrl: String);
begin
  if AUrl = '' then Exit;
  TJSURL.revokeObjectURL(AUrl);
  AUrl := '';
end;

procedure TBrowserMusicStudioApplication.DisableDownload(
  const ALink: TJSHTMLAnchorElement; var AUrl: String);
begin
  RevokeUrl(AUrl);
  ALink.removeAttribute('href');
  ALink.removeAttribute('download');
  ALink.setAttribute('aria-disabled', 'true');
  ALink.className := 'button-link disabled';
end;

procedure TBrowserMusicStudioApplication.SetTextDownload(
  const ALink: TJSHTMLAnchorElement; const AText, AFileName, AMime: String;
  var AUrl: String);
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

procedure TBrowserMusicStudioApplication.SetBinaryDownload(
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
  ASignature := MusicStudioByteSignature(ABytes);
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

procedure TBrowserMusicStudioApplication.ClearCurrentPresentation;
begin
  FHarmonyGrid.textContent := '';
  FRhythmGrid.textContent := '';
  FMelodyGrid.textContent := '';
  FLayersPlaceholder.removeAttribute('hidden');
  FPianoRoll.textContent := '';
  FPitchSummary.textContent := 'No melody pitches to display.';
  FArtifactOutput.value := '';
  DisableDownload(FCompositionLink, FCompositionUrl);
  DisableDownload(FScoreLink, FScoreUrl);
  DisableDownload(FMidiLink, FMidiUrl);
  ResetPreviewAudio;
  DisableDownload(FWaveLink, FWaveUrl);
  FAudioStatus.textContent := 'no preview';
  document.body.setAttribute('data-cell-count', '0');
  document.body.setAttribute('data-midi-bytes', '0');
  document.body.setAttribute('data-wav-bytes', '0');
  document.body.setAttribute('data-audio-ready', 'false');
  document.body.setAttribute('data-composition-signature', '');
  document.body.setAttribute('data-score-signature', '');
  document.body.setAttribute('data-midi-signature', '');
  document.body.setAttribute('data-wave-signature', '');
end;

procedure TBrowserMusicStudioApplication.ResetPreviewAudio;
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

procedure TBrowserMusicStudioApplication.ClearReports;
begin
  FRepairReport.textContent := 'No current generation attempt.';
  FPassReport.textContent := 'No current generation attempt.';
  FFailureReport.textContent := 'No terminal failure.';
end;

function TBrowserMusicStudioApplication.LockIndex(
  const ALayer: TWfcMusicPassLayer; const APosition: Integer): Integer;
var LLocks: TWfcMusicStudioLocks;
begin
  LLocks := FStudio.CopyLocks;
  for Result := 0 to High(LLocks) do
    if (LLocks[Result].Layer = ALayer) and
        (LLocks[Result].Position = APosition) then Exit;
  Result := -1;
end;

function TBrowserMusicStudioApplication.ReportPassCount(
  const AReport: TWfcMusicStudioReport): Integer;
var I: Integer;
begin
  Result := 0;
  for I := 0 to High(AReport.Passes) do
    if AReport.Passes[I].Executed then Inc(Result);
end;

procedure TBrowserMusicStudioApplication.RefreshMetrics;
var
  LLocks: TWfcMusicStudioLocks;
  LPassCount: Integer;
  LReport: TWfcMusicStudioReport;
  LSignature, LStatus: String;
begin
  LLocks := FStudio.CopyLocks;
  LReport := FStudio.CopyReport;
  LPassCount := ReportPassCount(LReport);
  LStatus := MusicStudioStatusName(FStudio.Status);
  FSeedOutput.textContent := UIntToStr(FStudio.Seed);
  FResultStatusOutput.textContent := LStatus;
  FStrategyOutput.textContent := StrategyName(FOptions);
  FScopeOutput.textContent := ActionName(FAction);
  FLocalBacktracksOutput.textContent := IntToStr(LReport.Backtracks) +
    ' / ' + IntToStr(FOptions.MaxBacktracks);
  FPassBacktracksOutput.textContent := IntToStr(LReport.PassBacktracks) +
    ' / ' + IntToStr(FOptions.MaxPassBacktracks);
  FPassCountOutput.textContent := IntToStr(LPassCount);
  FLockCountOutput.textContent := IntToStr(Length(LLocks));
  LSignature := '';
  if FStudio.HasCurrent then LSignature := FStudio.SignatureText;
  if LSignature = '' then FSignatureOutput.textContent := '—'
  else FSignatureOutput.textContent := LSignature;
  document.body.setAttribute('data-seed', UIntToStr(FStudio.Seed));
  document.body.setAttribute('data-result-status', LStatus);
  document.body.setAttribute('data-strategy', StrategyName(FOptions));
  document.body.setAttribute('data-scope', ActionName(FAction));
  document.body.setAttribute('data-lock-count', IntToStr(Length(LLocks)));
  document.body.setAttribute('data-pass-count', IntToStr(LPassCount));
  document.body.setAttribute('data-composition-signature', LSignature);
end;

procedure TBrowserMusicStudioApplication.RefreshVocabulary;
var
  I, LOldIndex, LSelectedIndex: Integer;
  LLayer: TWfcMusicPassLayer;
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
    LOption.textContent := MusicStudioTokenLabel(LLayer, FVocabulary[I]);
    FLockTokenSelect.appendChild(LOption);
  end;
  LSelectedIndex := FindVocabularyToken(LOldToken);
  if (LSelectedIndex < 0) and (Length(FVocabulary) > 0) then LSelectedIndex := 0;
  if LSelectedIndex >= 0 then FLockTokenSelect.value := IntToStr(LSelectedIndex);
end;

procedure TBrowserMusicStudioApplication.RefreshLocks;
var
  I, LOldIndex, LSelectedIndex: Integer;
  LLocks: TWfcMusicStudioLocks;
  LOldLayer: TWfcMusicPassLayer;
  LOldPosition: Integer;
  LOption: TJSHTMLOptionElement;
begin
  LLocks := FStudio.CopyLocks;
  LOldIndex := FLockList.selectedIndex;
  LOldPosition := -1;
  LOldLayer := wmplHarmony;
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
    LOption.textContent := LayerName(LLocks[I].Layer) + ' · cell ' +
      IntToStr(LLocks[I].Position) + ' · ' +
      MusicStudioTokenLabel(LLocks[I].Layer, LLocks[I].Token);
    FLockList.appendChild(LOption);
    if (LLocks[I].Layer = LOldLayer) and
        (LLocks[I].Position = LOldPosition) then LSelectedIndex := I;
  end;
  if LSelectedIndex >= 0 then FLockList.selectedIndex := LSelectedIndex;
end;

procedure TBrowserMusicStudioApplication.RefreshLayer(
  const ALayer: TWfcMusicPassLayer; const AGrid: TJSElement);
var
  I: Integer;
  LButton: TJSHTMLButtonElement;
  LLabel: String;
  LSmall: TJSElement;
  LTokens: TWfcModelTokens;
begin
  AGrid.textContent := '';
  if not FStudio.HasCurrent then Exit;
  LTokens := FStudio.CellTokens(ALayer);
  if Length(LTokens) <> MUSIC_STUDIO_CELL_COUNT then
    raise EMusicStudio.Create('current public layer does not contain 16 cells');
  for I := 0 to High(LTokens) do
  begin
    LButton := TJSHTMLButtonElement(document.createElement('button'));
    LButton.setAttribute('type', 'button');
    LButton.setAttribute('data-layer', LayerName(ALayer));
    LButton.setAttribute('data-index', IntToStr(I));
    LLabel := MusicStudioTokenLabel(ALayer, LTokens[I]);
    LButton.setAttribute('aria-label', LayerName(ALayer) + ' cell ' +
      IntToStr(I) + ': ' + LLabel);
    LButton.setAttribute('title', LLabel);
    LButton.className := 'music-cell';
    if LockIndex(ALayer, I) >= 0 then
      LButton.className := LButton.className + ' locked';
    LButton.textContent := LLabel;
    LSmall := document.createElement('small');
    LSmall.textContent := 'cell ' + IntToStr(I);
    LButton.appendChild(LSmall);
    LButton.onclick := @HandleCellClick;
    AGrid.appendChild(LButton);
  end;
end;

procedure TBrowserMusicStudioApplication.RefreshLayers;
begin
  if not FStudio.HasCurrent then
  begin
    FHarmonyGrid.textContent := '';
    FRhythmGrid.textContent := '';
    FMelodyGrid.textContent := '';
    FLayersPlaceholder.removeAttribute('hidden');
    document.body.setAttribute('data-cell-count', '0');
    RefreshPianoRoll;
    Exit;
  end;
  RefreshLayer(wmplHarmony, FHarmonyGrid);
  RefreshLayer(wmplRhythm, FRhythmGrid);
  RefreshLayer(wmplMelody, FMelodyGrid);
  FLayersPlaceholder.setAttribute('hidden', '');
  document.body.setAttribute('data-cell-count',
    IntToStr(MUSIC_STUDIO_CELL_COUNT));
  RefreshPianoRoll;
end;

procedure TBrowserMusicStudioApplication.RefreshPianoRoll;
var
  I, LAttackCount, LHoldCount, LMaximumPitch, LMinimumPitch, LPitch,
    LRestCount: Integer;
  LCell: TWfcMusicMelodyCell;
  LCells: TWfcMusicMelodyCells;
  LElement: TJSElement;
  LHasPitch: Boolean;
begin
  FPianoRoll.textContent := '';
  if not FStudio.HasCurrent then
  begin
    FPitchSummary.textContent := 'No melody pitches to display.';
    Exit;
  end;
  LCells := FStudio.MelodyCells;
  if Length(LCells) <> MUSIC_STUDIO_CELL_COUNT then
    raise EMusicStudio.Create('current melody does not contain 16 cells');
  LElement := document.createElement('span');
  LElement.className := 'piano-label piano-corner';
  LElement.textContent := 'pitch';
  FPianoRoll.appendChild(LElement);
  for I := 0 to High(LCells) do
  begin
    LElement := document.createElement('span');
    LElement.className := 'piano-label piano-cell-label';
    LElement.textContent := IntToStr(I);
    FPianoRoll.appendChild(LElement);
  end;
  LAttackCount := 0;
  LHoldCount := 0;
  LRestCount := 0;
  LHasPitch := False;
  LMinimumPitch := 127;
  LMaximumPitch := 0;
  for I := 0 to High(LCells) do
  begin
    case LCells[I].Action of
      wmcaRest: Inc(LRestCount);
      wmcaAttack: Inc(LAttackCount);
      wmcaHold: Inc(LHoldCount);
    end;
    if LCells[I].Action <> wmcaRest then
    begin
      LHasPitch := True;
      if LCells[I].Pitch < LMinimumPitch then LMinimumPitch := LCells[I].Pitch;
      if LCells[I].Pitch > LMaximumPitch then LMaximumPitch := LCells[I].Pitch;
    end;
  end;
  for LPitch := 72 downto 60 do
  begin
    LElement := document.createElement('span');
    LElement.className := 'piano-label';
    LElement.textContent := MusicStudioPitchName(LPitch);
    FPianoRoll.appendChild(LElement);
    for I := 0 to High(LCells) do
    begin
      LCell := LCells[I];
      LElement := document.createElement('span');
      LElement.className := 'piano-slot';
      if (LCell.Action <> wmcaRest) and (LCell.Pitch = LPitch) then
      begin
        if LCell.Action = wmcaAttack then
          LElement.className := LElement.className + ' attack'
        else LElement.className := LElement.className + ' hold';
        LElement.setAttribute('title', MusicStudioPitchName(LPitch) + ' ' +
          MusicStudioTokenLabel(wmplMelody, EncodeWfcMusicMelodyCell(LCell)));
      end;
      FPianoRoll.appendChild(LElement);
    end;
  end;
  LElement := document.createElement('span');
  LElement.className := 'piano-label';
  LElement.textContent := 'rest';
  FPianoRoll.appendChild(LElement);
  for I := 0 to High(LCells) do
  begin
    LElement := document.createElement('span');
    LElement.className := 'piano-slot';
    if LCells[I].Action = wmcaRest then
      LElement.className := LElement.className + ' rest';
    FPianoRoll.appendChild(LElement);
  end;
  FPitchSummary.textContent := 'attacks=' + IntToStr(LAttackCount) +
    ' · holds=' + IntToStr(LHoldCount) + ' · rests=' + IntToStr(LRestCount);
  if LHasPitch then FPitchSummary.textContent := FPitchSummary.textContent +
    ' · range=' + MusicStudioPitchName(LMinimumPitch) + '..' +
    MusicStudioPitchName(LMaximumPitch);
end;

procedure TBrowserMusicStudioApplication.RefreshReports;
var
  I: Integer;
  LPass: TGraphPassSolveReport;
  LReport: TWfcMusicStudioReport;
  LText: String;
begin
  LReport := FStudio.CopyReport;
  if LReport.Status in [mssIdle, mssDirty] then
  begin
    ClearReports;
    Exit;
  end;
  FRepairReport.textContent :=
    'requested: ' + PassIndicesText(LReport.RequestedRootIndices,
      'full composition') + #10 +
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
    if (I >= Ord(Low(TWfcMusicPassLayer))) and
        (I <= Ord(High(TWfcMusicPassLayer))) then
      LText := LText + LayerName(TWfcMusicPassLayer(I))
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
  if LReport.Status = mssSolved then
    FFailureReport.textContent := 'No terminal failure.'
  else
    FFailureReport.textContent :=
      'status=' + MusicStudioStatusName(LReport.Status) + #10 +
      'kind=' + ContradictionName(LReport.FailureKind) + #10 +
      'failed-pass=' + IntToStr(LReport.FailedPass) + #10 +
      'cell=' + IntToStr(LReport.FailedCell) + #10 +
      'dependency-pass=' + IntToStr(LReport.DependencyPass) + #10 +
      'trace=' + UpperCase(IntToHex(LReport.TraceHash, 8)) + #10 +
      'transcript=' + UpperCase(IntToHex(LReport.TranscriptHash, 8));
end;

procedure TBrowserMusicStudioApplication.RefreshArtifactInspector;
begin
  if not FStudio.HasCurrent then
  begin
    FArtifactOutput.value := '';
    Exit;
  end;
  if FArtifactSelect.value = 'composition' then
    FArtifactOutput.value := FStudio.CompositionText
  else if FArtifactSelect.value = 'score' then
    FArtifactOutput.value := FStudio.ScoreText
  else raise EConvertError.Create('select a known text artifact');
end;

procedure TBrowserMusicStudioApplication.RefreshArtifacts;
var
  LMidiByteCount, LWaveByteCount: Integer;
  LClip: TWfcMusicPcm16Clip;
  LScore: TWfcMusicScore;
  LCompositionText, LMidiSignature, LScoreText, LWaveSignature: String;
begin
  DisableDownload(FCompositionLink, FCompositionUrl);
  DisableDownload(FScoreLink, FScoreUrl);
  DisableDownload(FMidiLink, FMidiUrl);
  ResetPreviewAudio;
  DisableDownload(FWaveLink, FWaveUrl);
  FAudioStatus.textContent := 'no preview';
  document.body.setAttribute('data-midi-bytes', '0');
  document.body.setAttribute('data-wav-bytes', '0');
  document.body.setAttribute('data-audio-ready', 'false');
  document.body.setAttribute('data-score-signature', '');
  document.body.setAttribute('data-midi-signature', '');
  document.body.setAttribute('data-wave-signature', '');
  if not FStudio.HasCurrent then
  begin
    RefreshArtifactInspector;
    Exit;
  end;
  LCompositionText := FStudio.CompositionText;
  LScoreText := FStudio.ScoreText;
  SetTextDownload(FCompositionLink, LCompositionText,
    'composition.wfcmusicpass', 'text/plain;charset=utf-8', FCompositionUrl);
  SetTextDownload(FScoreLink, LScoreText,
    'score.wfcmusic', 'text/plain;charset=utf-8', FScoreUrl);
  SetBinaryDownload(FMidiLink, FStudio.MidiBytes, 'composition.mid',
    'audio/midi', FMidiUrl, LMidiByteCount, LMidiSignature);
  LScore := FStudio.CopyScore;
  try
    LClip := RenderWfcMusicAudio(LScore, DefaultWfcMusicAudioOptions);
    try
      SetBinaryDownload(FWaveLink, EncodeWfcMusicWave(LClip), 'preview.wav',
        'audio/wav', FWaveUrl, LWaveByteCount, LWaveSignature);
    finally
      LClip.Free;
    end;
  finally
    LScore.Free;
  end;
  FPreviewAudio.autoplay := False;
  FPreviewAudio.preload := 'none';
  FPreviewAudio.src := FWaveUrl;
  FAudioStatus.textContent := 'preview ready · press play';
  document.body.setAttribute('data-midi-bytes', IntToStr(LMidiByteCount));
  document.body.setAttribute('data-wav-bytes', IntToStr(LWaveByteCount));
  document.body.setAttribute('data-score-signature',
    MusicStudioTextSignature(LScoreText));
  document.body.setAttribute('data-midi-signature', LMidiSignature);
  document.body.setAttribute('data-wave-signature', LWaveSignature);
  document.body.setAttribute('data-audio-ready', 'true');
  RefreshArtifactInspector;
end;

procedure TBrowserMusicStudioApplication.RefreshAll;
begin
  RefreshVocabulary;
  RefreshLocks;
  RefreshMetrics;
  RefreshLayers;
  RefreshReports;
  RefreshArtifacts;
end;

procedure TBrowserMusicStudioApplication.InvalidateForPendingEdit(
  const AReason: String);
begin
  FStudio.InvalidateCurrent;
  ClearCurrentPresentation;
  ClearReports;
  RefreshVocabulary;
  RefreshLocks;
  RefreshMetrics;
  FStrategyOutput.textContent := 'pending run';
  FScopeOutput.textContent := 'pending run';
  FLocalBacktracksOutput.textContent := '—';
  FPassBacktracksOutput.textContent := '—';
  FPassCountOutput.textContent := '0';
  document.body.setAttribute('data-strategy', 'pending');
  document.body.setAttribute('data-scope', 'pending');
  document.body.setAttribute('data-pass-count', '0');
  SetState('dirty', 'Generation inputs changed; old output cleared.', AReason);
end;

procedure TBrowserMusicStudioApplication.StartNewSession;
var LSeed: TGraphSeed;
begin
  FStudio.InvalidateCurrent;
  ClearCurrentPresentation;
  ClearReports;
  if not TryParseSeed(FSeedInput.value, LSeed) then
    raise EConvertError.Create('seed must be decimal, $hex, or 0xhex');
  FStudio.Reset(LSeed);
  FAction := msaGenerate;
  FScopeSelect.value := 'full';
  RefreshAll;
  SetState('ready', 'New session ready.',
    'Locks and the prior engine baseline were cleared; generate to compose.');
end;

procedure TBrowserMusicStudioApplication.Generate;
var
  LAction: TWfcMusicStudioAction;
  LOptions: TWfcMusicStudioOptions;
  LSeed: TGraphSeed;
  LSolved: Boolean;
begin
  FStudio.InvalidateCurrent;
  ClearCurrentPresentation;
  ClearReports;
  if not TryParseSeed(FSeedInput.value, LSeed) then
    raise EConvertError.Create('seed must be decimal, $hex, or 0xhex');
  if LSeed <> FStudio.Seed then
    raise EConvertError.Create(
      'seed differs from this session; start a new session before generating');
  LOptions := ReadOptions;
  LAction := SelectedAction;
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
    SetState('solved', 'Composition solved.',
      'Public layers, canonical artifacts, MIDI, and the user-initiated WAV preview are current.')
  else
    case FStudio.Status of
      mssContradiction:
        SetState('contradiction', 'Constraints contradict.',
          'The terminal report is current; no partial composition or audio is exposed.');
      mssSolverLimit:
        SetState('limit', 'Local search budget exhausted.',
          'The terminal report is current; increase the local budget to retry.');
      mssPassLimit:
        SetState('limit', 'Pass search budget exhausted.',
          'The terminal report is current; increase the pass budget to retry.');
    else
      SetState('failed', 'Generation did not solve.',
        'The terminal report is current; no partial output is exposed.');
    end;
end;

procedure TBrowserMusicStudioApplication.AssertTest(
  const ACondition: Boolean; const AMessage: String);
begin
  if not ACondition then raise EMusicStudio.Create(AMessage);
end;

procedure TBrowserMusicStudioApplication.DispatchDomEvent(
  const AElement: TJSElement; const AEventName: String);
begin
  AElement.dispatchEvent(TJSEvent.new(AEventName));
end;

procedure TBrowserMusicStudioApplication.RunSelfTest;
var
  I, LTokenIndex: Integer;
  LLocks: TWfcMusicStudioLocks;
  LReport: TWfcMusicStudioReport;
begin
  document.body.setAttribute('data-self-test', 'pending');
  document.body.removeAttribute('data-self-test-message');
  document.body.setAttribute('data-new-session-invalidation', 'pending');
  document.body.setAttribute('data-run-invalidation', 'pending');
  document.body.setAttribute('data-failure-clears-output', 'pending');
  document.body.setAttribute('data-motif-lock', 'pending');
  document.body.setAttribute('data-selective-regeneration', 'pending');
  document.body.setAttribute('data-recovery', 'pending');
  try
    AssertTest(FStudio.HasCurrent and (FStudio.Status = mssSolved) and
      (FStudio.SignatureText = BASELINE_SIGNATURE),
      'initial seed-zero composition changed');
    AssertTest((Length(FStudio.CellTokens(wmplHarmony)) = MUSIC_STUDIO_CELL_COUNT) and
      (Length(FStudio.CellTokens(wmplRhythm)) = MUSIC_STUDIO_CELL_COUNT) and
      (Length(FStudio.CellTokens(wmplMelody)) = MUSIC_STUDIO_CELL_COUNT),
      'initial public layer cell counts changed');
    AssertTest((StrToInt(document.body.getAttribute('data-midi-bytes')) > 0) and
      (document.body.getAttribute('data-wav-bytes') = '352844') and
      (document.body.getAttribute('data-score-signature') =
        BASELINE_SCORE_SIGNATURE) and
      (document.body.getAttribute('data-midi-signature') =
        BASELINE_MIDI_SIGNATURE) and
      (document.body.getAttribute('data-wave-signature') =
        BASELINE_WAVE_SIGNATURE) and
      (document.body.getAttribute('data-audio-ready') = 'true'),
      'initial binary artifacts were not prepared');
    AssertTest(FAudioPlayEvents = 0, 'preview started without a user play action');

    FSeedInput.value := '1';
    DispatchDomEvent(FSeedInput, 'input');
    AssertTest((not FStudio.HasCurrent) and
      (document.body.getAttribute('data-cell-count') = '0') and
      (document.body.getAttribute('data-audio-ready') = 'false') and
      (FPreviewAudio.currentSrc = '') and (FPreviewAudio.readyState = 0),
      'seed edit retained stale output or audio');
    DispatchDomEvent(FNewSessionButton, 'click');
    AssertTest((FStudio.Seed = 1) and (not FStudio.HasBaseline) and
      (Length(FStudio.CopyLocks) = 0),
      'new session retained seed, locks, or baseline');
    document.body.setAttribute('data-new-session-invalidation', 'passed');

    FSeedInput.value := '0';
    DispatchDomEvent(FSeedInput, 'input');
    DispatchDomEvent(FNewSessionButton, 'click');
    DispatchDomEvent(FGenerateButton, 'click');
    AssertTest(FStudio.HasCurrent and
      (FStudio.SignatureText = BASELINE_SIGNATURE),
      'same-seed new session did not restore the deterministic baseline');

    FBacktracksInput.value := '255';
    DispatchDomEvent(FBacktracksInput, 'input');
    AssertTest((not FStudio.HasCurrent) and FStudio.HasBaseline and
      (document.body.getAttribute('data-midi-bytes') = '0') and
      (document.body.getAttribute('data-wav-bytes') = '0'),
      'run edit retained stale artifacts or erased the repair baseline');
    FBacktracksInput.value := '1025';
    DispatchDomEvent(FBacktracksInput, 'input');
    DispatchDomEvent(FGenerateButton, 'click');
    AssertTest((not FStudio.HasCurrent) and
      (document.body.getAttribute('data-state') = 'error') and
      (document.body.getAttribute('data-audio-ready') = 'false'),
      'invalid budget retained a current output or audio');
    document.body.setAttribute('data-run-invalidation', 'passed');
    FBacktracksInput.value := '256';
    DispatchDomEvent(FBacktracksInput, 'input');
    DispatchDomEvent(FGenerateButton, 'click');
    AssertTest(FStudio.HasCurrent and
      (FStudio.SignatureText = BASELINE_SIGNATURE),
      'generation did not recover after a run edit');

    DispatchDomEvent(FMotifButton, 'click');
    LLocks := FStudio.CopyLocks;
    AssertTest((Length(LLocks) = OPENING_MOTIF_CELLS) and
      (not FStudio.HasCurrent), 'opening motif did not become two locks');
    for I := 0 to High(LLocks) do
      AssertTest((LLocks[I].Layer = wmplMelody) and
        (LLocks[I].Position = I), 'opening motif lock coordinates changed');
    document.body.setAttribute('data-motif-lock', 'passed');

    FLockLayerSelect.value := 'melody';
    DispatchDomEvent(FLockLayerSelect, 'change');
    LTokenIndex := FindVocabularyToken(CONFLICTING_MELODY_TOKEN);
    AssertTest(LTokenIndex >= 0, 'required public melody token is unavailable');
    FLockCellInput.value := '2';
    FLockTokenSelect.value := IntToStr(LTokenIndex);
    DispatchDomEvent(FAddLockButton, 'click');
    LLocks := FStudio.CopyLocks;
    AssertTest((Length(LLocks) = 3) and
      (LLocks[2].Token = CONFLICTING_MELODY_TOKEN),
      'explicit nonfirst melody lock changed while refreshing controls');

    FScopeSelect.value := 'harmony';
    DispatchDomEvent(FScopeSelect, 'change');
    FStrategySelect.value := 'one-way';
    DispatchDomEvent(FStrategySelect, 'change');
    AssertTest(FPassBacktracksInput.value = '0',
      'one-way UI did not zero the pass budget');
    DispatchDomEvent(FGenerateButton, 'click');
    AssertTest((not FStudio.HasCurrent) and FStudio.HasBaseline and
      (FStudio.Status <> mssSolved) and
      (document.body.getAttribute('data-cell-count') = '0') and
      (document.body.getAttribute('data-midi-bytes') = '0') and
      (document.body.getAttribute('data-wav-bytes') = '0') and
      (document.body.getAttribute('data-audio-ready') = 'false'),
      'one-way failure exposed stale output or erased the baseline');
    document.body.setAttribute('data-failure-clears-output', 'passed');

    FStrategySelect.value := 'negotiated';
    DispatchDomEvent(FStrategySelect, 'change');
    DispatchDomEvent(FGenerateButton, 'click');
    LReport := FStudio.CopyReport;
    AssertTest(FStudio.HasCurrent and
      (FStudio.SignatureText = REPAIR_SIGNATURE) and
      (LReport.Action = msaHarmony) and (LReport.Rounds = 2) and
      (LReport.PassBacktracks = 1) and
      (Length(LReport.RequestedRootIndices) = 1) and
      (LReport.RequestedRootIndices[0] = Ord(wmplHarmony)) and
      (Length(LReport.ActivePassIndices) = 2) and
      (LReport.ActivePassIndices[0] = Ord(wmplHarmony)) and
      (LReport.ActivePassIndices[1] = Ord(wmplMelody)) and
      (Length(LReport.Passes) > Ord(wmplRhythm)) and
      (LReport.Passes[Ord(wmplRhythm)].Disposition = gpdReused) and
      (UpperCase(IntToHex(LReport.TranscriptHash, 8)) = REPAIR_TRANSCRIPT) and
      (document.body.getAttribute('data-score-signature') =
        REPAIR_SCORE_SIGNATURE) and
      (document.body.getAttribute('data-midi-signature') =
        REPAIR_MIDI_SIGNATURE) and
      (document.body.getAttribute('data-wave-signature') =
        REPAIR_WAVE_SIGNATURE),
      'negotiated selective repair evidence changed');
    document.body.setAttribute('data-selective-regeneration', 'passed');

    DispatchDomEvent(FClearLocksButton, 'click');
    FScopeSelect.value := 'full';
    DispatchDomEvent(FScopeSelect, 'change');
    DispatchDomEvent(FGenerateButton, 'click');
    AssertTest(FStudio.HasCurrent and
      (FStudio.SignatureText = BASELINE_SIGNATURE) and
      (Length(FStudio.CopyLocks) = 0),
      'clearing locks did not recover the exact baseline');
    document.body.setAttribute('data-recovery', 'passed');

    WriteDefaultOptions;
    FSeedInput.value := '0';
    FStudio.Reset(0);
    FAction := msaGenerate;
    Generate;
    AssertTest(FStudio.HasCurrent and
      (FStudio.SignatureText = BASELINE_SIGNATURE) and
      (document.body.getAttribute('data-cell-count') = '16') and
      (document.body.getAttribute('data-score-signature') =
        BASELINE_SCORE_SIGNATURE) and
      (document.body.getAttribute('data-midi-signature') =
        BASELINE_MIDI_SIGNATURE) and
      (document.body.getAttribute('data-wave-signature') =
        BASELINE_WAVE_SIGNATURE) and
      (document.body.getAttribute('data-audio-ready') = 'true') and
      (FAudioPlayEvents = 0), 'final baseline or no-autoplay state changed');
    document.body.setAttribute('data-document-width',
      IntToStr(document.documentElement.scrollWidth));
    document.body.setAttribute('data-viewport-width',
      IntToStr(document.documentElement.clientWidth));
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

function TBrowserMusicStudioApplication.HandleSeedInput(
  AEvent: TJSEvent): Boolean;
begin
  Result := False;
  try
    InvalidateForPendingEdit(
      'Start a new session to apply the edited seed and clear all locks.');
  except on E: Exception do ShowError(E.Message); end;
end;

function TBrowserMusicStudioApplication.HandleNewSession(
  AEvent: TJSMouseEvent): Boolean;
begin
  Result := False;
  try StartNewSession; except on E: Exception do ShowError(E.Message); end;
end;

function TBrowserMusicStudioApplication.HandleGenerate(
  AEvent: TJSMouseEvent): Boolean;
begin
  Result := False;
  try Generate; except on E: Exception do ShowError(E.Message); end;
end;

function TBrowserMusicStudioApplication.HandleRunInput(
  AEvent: TJSEvent): Boolean;
begin
  Result := False;
  try
    InvalidateForPendingEdit(
      'Generate again when the edited strategy, scope, budget, and trace settings are ready.');
  except on E: Exception do ShowError(E.Message); end;
end;

function TBrowserMusicStudioApplication.HandleStrategyChange(
  AEvent: TJSEvent): Boolean;
begin
  Result := False;
  try
    if FStrategySelect.value = 'one-way' then
      FPassBacktracksInput.value := '0'
    else if (FStrategySelect.value = 'negotiated') and
        (Trim(FPassBacktracksInput.value) = '0') then
      FPassBacktracksInput.value := IntToStr(
        DefaultMusicStudioOptions.MaxPassBacktracks);
    InvalidateForPendingEdit(
      'Generate again with the selected pass strategy and its current budgets.');
  except on E: Exception do ShowError(E.Message); end;
end;

function TBrowserMusicStudioApplication.HandleMotif(
  AEvent: TJSMouseEvent): Boolean;
begin
  Result := False;
  try
    FStudio.LockOpeningMotif(OPENING_MOTIF_CELLS);
    ClearCurrentPresentation;
    RefreshVocabulary;
    RefreshLocks;
    RefreshMetrics;
    ClearReports;
    SetState('dirty', 'Opening motif locked; old output cleared.',
      'Regenerate melody or a wider scope to apply the two public locks.');
  except on E: Exception do ShowError(E.Message); end;
end;

function TBrowserMusicStudioApplication.HandleLockLayerChange(
  AEvent: TJSEvent): Boolean;
begin
  Result := False;
  try RefreshVocabulary; except on E: Exception do ShowError(E.Message); end;
end;

function TBrowserMusicStudioApplication.HandleAddLock(
  AEvent: TJSMouseEvent): Boolean;
var
  I, LPosition, LTokenIndex: Integer;
  LLayer: TWfcMusicPassLayer;
  LLocks: TWfcMusicStudioLocks;
  LToken: TWfcModelToken;
begin
  Result := False;
  try
    FStudio.InvalidateCurrent;
    ClearCurrentPresentation;
    ClearReports;
    LLayer := SelectedLayer;
    LPosition := ReadBoundedInteger(FLockCellInput, 'lock cell', 0,
      MUSIC_STUDIO_CELL_COUNT - 1);
    LTokenIndex := SelectedVocabularyIndex;
    LToken := FVocabulary[LTokenIndex];
    FStudio.SetLock(LLayer, LPosition, LToken);
    RefreshVocabulary;
    RefreshLocks;
    RefreshMetrics;
    LLocks := FStudio.CopyLocks;
    for I := 0 to High(LLocks) do
      if (LLocks[I].Layer = LLayer) and (LLocks[I].Position = LPosition) then
      begin FLockList.selectedIndex := I; Break; end;
    SetState('dirty', 'Public lock updated; old output cleared.',
      'Regenerate the selected scope to apply the sorted lock set.');
  except on E: Exception do ShowError(E.Message); end;
end;

function TBrowserMusicStudioApplication.HandleRemoveLock(
  AEvent: TJSMouseEvent): Boolean;
var
  LIndex: Integer;
  LLocks: TWfcMusicStudioLocks;
begin
  Result := False;
  try
    LLocks := FStudio.CopyLocks;
    LIndex := FLockList.selectedIndex;
    if (LIndex < 0) or (LIndex >= Length(LLocks)) then
      raise ERangeError.Create('select a public lock to remove');
    FStudio.ClearLock(LLocks[LIndex].Layer, LLocks[LIndex].Position);
    ClearCurrentPresentation;
    RefreshVocabulary;
    RefreshLocks;
    RefreshMetrics;
    ClearReports;
    SetState('dirty', 'Public lock removed; old output cleared.',
      'Regenerate to apply the remaining locks.');
  except on E: Exception do ShowError(E.Message); end;
end;

function TBrowserMusicStudioApplication.HandleClearLocks(
  AEvent: TJSMouseEvent): Boolean;
begin
  Result := False;
  try
    FStudio.ClearLocks;
    ClearCurrentPresentation;
    RefreshVocabulary;
    RefreshLocks;
    RefreshMetrics;
    ClearReports;
    SetState('dirty', 'All public locks cleared; old output cleared.',
      'Regenerate for an unconstrained composition.');
  except on E: Exception do ShowError(E.Message); end;
end;

function TBrowserMusicStudioApplication.HandleCellClick(
  AEvent: TJSMouseEvent): Boolean;
var
  LElement: TJSElement;
  LIndex, LTokenIndex: Integer;
  LLayer: TWfcMusicPassLayer;
  LTokens: TWfcModelTokens;
begin
  Result := False;
  try
    LElement := TJSElement(AEvent.currentTarget);
    if not TryStrToInt(LElement.getAttribute('data-index'), LIndex) or
        (LIndex < 0) or (LIndex >= MUSIC_STUDIO_CELL_COUNT) then
      raise ERangeError.Create('selected music cell is invalid');
    FLockLayerSelect.value := LElement.getAttribute('data-layer');
    LLayer := SelectedLayer;
    FLockCellInput.value := IntToStr(LIndex);
    RefreshVocabulary;
    LTokens := FStudio.CellTokens(LLayer);
    LTokenIndex := FindVocabularyToken(LTokens[LIndex]);
    if LTokenIndex >= 0 then FLockTokenSelect.value := IntToStr(LTokenIndex);
    FStatusDetailElement.textContent := 'Selected ' + LayerName(LLayer) +
      ' cell ' + IntToStr(LIndex) + ' for an optional public lock.';
  except on E: Exception do ShowError(E.Message); end;
end;

function TBrowserMusicStudioApplication.HandleArtifactChange(
  AEvent: TJSEvent): Boolean;
begin
  Result := False;
  try RefreshArtifactInspector; except on E: Exception do ShowError(E.Message); end;
end;

function TBrowserMusicStudioApplication.HandleAudioPlay(
  AEvent: TJSPointerEvent): Boolean;
begin
  Result := False;
  Inc(FAudioPlayEvents);
  document.body.setAttribute('data-audio-play-events', IntToStr(FAudioPlayEvents));
  FAudioStatus.textContent := 'playing project-owned preview';
end;

function TBrowserMusicStudioApplication.HandleAudioEnded(
  AEvent: TJSEvent): Boolean;
begin
  Result := False;
  if FStudio.HasCurrent then FAudioStatus.textContent := 'preview ready · press play';
end;

function TBrowserMusicStudioApplication.HandleAudioError(
  AEvent: TJSErrorEvent): Boolean;
begin
  Result := False;
  FAudioStatus.textContent := 'browser could not decode preview';
  document.body.setAttribute('data-audio-ready', 'false');
end;

procedure TBrowserMusicStudioApplication.Run;
begin
  try
    BindDocument;
    BindEvents;
    WriteDefaultOptions;
    document.body.setAttribute('data-audio-play-events', '0');
    RefreshAll;
    SetState('ready', 'Session ready.', 'Generating deterministic seed 0.');
    Generate;
    if Pos('selftest=1', window.location.search) > 0 then RunSelfTest
    else document.body.setAttribute('data-self-test', 'not-requested');
  except on E: Exception do ShowError(E.Message); end;
end;

end.
