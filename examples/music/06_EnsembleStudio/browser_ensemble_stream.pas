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
unit browser_ensemble_stream;

{$mode delphi}{$H+}
{$modeswitch externalclass}

interface

uses
  JS,
  Web,
  SysUtils,
  wfc,
  wfc_music_audio,
  wfc_music_audio_stream,
  wfc_music_arrangement,
  wfc_midi_smf,
  wfc_midi_stream,
  ensemble_studio_stream,
  ensemble_studio_midi_stream;

type
  TBrowserEnsembleStreamOperationKind = (
    besokWave, besokMidiPlan, besokMidiSave);

  TBrowserEnsembleStreamOperation = class
  public
    Kind: TBrowserEnsembleStreamOperationKind;
    Cancelled, Committing: Boolean;
    MidiPlan: TEnsembleStudioMidiPlan;
    destructor Destroy; override;
  end;

  { Browser transport for the independent long-form stream. It owns no finite
    editor state. Release detaches listeners immediately and delays destruction
    until outstanding picker/write promises settle. }
  TBrowserEnsembleStreamController = class
  strict private
    FSeed, FSeconds, FSegmentCells, FBacktracks, FPassBacktracks:
      TJSHTMLInputElement;
    FTrace: TJSHTMLInputElement;
    FStart, FMidiPlanButton, FMidiSaveButton, FCancel: TJSHTMLButtonElement;
    FProgress: TJSHTMLProgressElement;
    FStatus, FDetail, FPlan, FFallback: TJSElement;
    FActive: TBrowserEnsembleStreamOperation;
    FMidiPlan: TEnsembleStudioMidiPlan;
    FBusy, FReleased, FBound, FSelfTestRunning: Boolean;
    FAsyncCount: Integer;
    FRefreshTimer: NativeInt;
    function RequireElement(const AId: String): TJSElement;
    function ReadSeed: TGraphSeed;
    function ReadNonnegative(const AInput: TJSHTMLInputElement;
      const AName: String): Integer;
    function ReadOptions: TEnsembleStudioStreamOptions;
    procedure SetStatus(const AState, AMessage: String);
    procedure SetBusy(const AValue: Boolean);
    procedure RefreshPlan;
    procedure ClearMidiPlan;
    procedure Cancel(const AReason: String);
    procedure AsyncFinished;
    function HandleStart(AEvent: TJSMouseEvent): Boolean;
    function HandleMidiPlan(AEvent: TJSMouseEvent): Boolean;
    function HandleMidiSave(AEvent: TJSMouseEvent): Boolean;
    function HandleCancel(AEvent: TJSMouseEvent): Boolean;
    function HandleMutation(AEvent: TJSEvent): Boolean;
    procedure SaveStream(const ATestPicker: TJSPromise = nil); async;
    procedure PlanMidi; async;
    procedure SaveMidi(const ATestPicker: TJSPromise = nil); async;
    procedure CheckSaveFixture(const AKind: String); async;
    procedure CheckMidiSaveFixture(const AKind: String); async;
  public
    constructor Create;
    procedure Run;
    procedure Release;
    procedure RunSelfTest; async;
  end;

procedure InstallEnsembleStreamBrowserTestFixture;

implementation

type
  EBrowserEnsembleStreamCancelled = class(EEnsembleStudioStream);

  TEnsembleWritableFile = class external name 'Object' (TJSObject)
    function write(const AData: TJSBlob): TJSPromise;
    function close: TJSPromise;
    function abort: TJSPromise;
  end;
  TEnsembleFileHandle = class external name 'Object' (TJSObject)
    function createWritable: TJSPromise;
  end;
  TEnsembleSaveWindow = class external name 'Window' (TJSWindow)
    function showSaveFilePicker(const AOptions: TJSObject): TJSPromise;
      reintroduce;
  end;
  TEnsembleEventDocument = class external name 'Document' (TJSObject)
    procedure removeEventListener(const AName: String;
      const AListener: TJSEventHandler; const AUseCapture: Boolean);
  end;

  TOneBlockSink = class(TWfcMusicAudioByteSink)
  strict private
    FBlock: TJSUint8Array;
    FSize, FPeak: Integer;
    FTotal: TWfcMusicAudioStreamCount;
  public
    procedure WriteBytes(const ABytes: array of Byte); override;
    function DetachBlob: TJSBlob;
    procedure Discard;
    property Size: Integer read FSize;
    property Peak: Integer read FPeak;
    property Total: TWfcMusicAudioStreamCount read FTotal;
  end;

  TControlState = record
    Element: TJSElement;
    WasDisabled: Boolean;
  end;
  TControlStates = array of TControlState;

function BrowserYield: TJSPromise;
begin
  Result := TJSPromise.new(
    procedure(AResolve, AReject: TJSPromiseResolver)
    begin
      window.setTimeout(procedure begin AResolve(Null); end, 0);
    end);
end;

function FailureText(const AValue: JSValue): String;
begin
  if isObject(AValue) and (TObject(AValue) is Exception) then
    Result := Exception(TObject(AValue)).Message
  else Result := String(AValue);
  if Result = '' then Result := 'unknown browser failure';
end;

function FailureName(const AValue: JSValue): String;
begin
  Result := '';
  if isObject(AValue) and isString(TJSObject(AValue)['name']) then
    Result := String(TJSObject(AValue)['name']);
end;

function IsCancelledFailure(const AValue: JSValue): Boolean;
begin
  Result := isObject(AValue) and
    (TObject(AValue) is EBrowserEnsembleStreamCancelled);
end;

destructor TBrowserEnsembleStreamOperation.Destroy;
begin
  MidiPlan.Free;
  inherited Destroy;
end;

function OperationCurrent(const AActive, ACandidate:
  TBrowserEnsembleStreamOperation): Boolean;
begin
  Result := (ACandidate <> nil) and (AActive = ACandidate) and
    not ACandidate.Cancelled;
end;

procedure CheckOperation(const AActive, ACandidate:
  TBrowserEnsembleStreamOperation);
begin
  if not OperationCurrent(AActive, ACandidate) then
    raise EBrowserEnsembleStreamCancelled.Create(
      'stream save was cancelled or superseded');
end;

procedure FreezeControls(out AStates: TControlStates);
var
  I: Integer;
  LControls: TJSNodeList;
  LElement: TJSElement;
begin
  LControls := document.querySelectorAll('main input, main select, main button');
  SetLength(AStates, LControls.length);
  for I := 0 to LControls.length - 1 do
  begin
    LElement := TJSElement(LControls[I]);
    AStates[I].Element := LElement;
    AStates[I].WasDisabled := LElement.hasAttribute('disabled');
    LElement.setAttribute('disabled', '');
  end;
end;

procedure RestoreControls(const AStates: TControlStates);
var
  I: Integer;
begin
  for I := 0 to High(AStates) do
    if not AStates[I].WasDisabled then
      AStates[I].Element.removeAttribute('disabled');
end;

procedure TOneBlockSink.WriteBytes(const ABytes: array of Byte);
var
  I: Integer;
begin
  if FBlock <> nil then
    raise EEnsembleStudioStream.Create(
      'browser transport requested another block before the prior write');
  if Length(ABytes) > WFC_MUSIC_AUDIO_STREAM_BLOCK_BYTES then
    raise EEnsembleStudioStream.Create('browser byte block exceeded 4096 bytes');
  FBlock := TJSUint8Array.new(Length(ABytes));
  for I := 0 to High(ABytes) do FBlock[I] := ABytes[I];
  FSize := Length(ABytes);
  if FSize > FPeak then FPeak := FSize;
  FTotal := FTotal + FSize;
end;

function TOneBlockSink.DetachBlob: TJSBlob;
var
  LOptions: TJSBlobInit;
  LParts: TJSArray;
begin
  if FBlock = nil then
    raise EEnsembleStudioStream.Create('browser byte block is empty');
  LParts := TJSArray.new;
  LParts.push(FBlock);
  LOptions := TJSBlobInit.new;
  LOptions.type_ := 'audio/wav';
  Result := TJSBlob.new(LParts, LOptions);
  FBlock := nil;
  FSize := 0;
end;

procedure TOneBlockSink.Discard;
begin
  FBlock := nil;
  FSize := 0;
end;

function MidiBlockBlob(const ABytes: TWfcMidiBytes): TJSBlob;
var
  I: Integer;
  LBlock: TJSUint8Array;
  LOptions: TJSBlobInit;
  LParts: TJSArray;
begin
  if (Length(ABytes) < 1) or
      (Length(ABytes) > WFC_MIDI_STREAM_BLOCK_BYTES) then
    raise EEnsembleStudioMidiStream.Create(
      'browser MIDI block is outside the 1..4096 byte transport bound');
  LBlock := TJSUint8Array.new(Length(ABytes));
  for I := 0 to High(ABytes) do LBlock[I] := ABytes[I];
  LParts := TJSArray.new;
  LParts.push(LBlock);
  LOptions := TJSBlobInit.new;
  LOptions.type_ := 'audio/midi';
  Result := TJSBlob.new(LParts, LOptions);
end;

procedure AppendMidiBytes(var ADestination: TWfcMidiBytes;
  const ASource: TWfcMidiBytes);
var
  I, LOld: Integer;
begin
  if Length(ASource) > High(Integer) - Length(ADestination) then
    raise EEnsembleStudioMidiStream.Create(
      'browser MIDI self-test byte fixture exceeds Integer');
  LOld := Length(ADestination);
  SetLength(ADestination, LOld + Length(ASource));
  for I := 0 to High(ASource) do ADestination[LOld + I] := ASource[I];
end;

procedure AppendTypedMidiBytes(var ADestination: TWfcMidiBytes;
  const ASource: TJSUint8Array);
var
  I, LOld: Integer;
begin
  if ASource.length > High(Integer) - Length(ADestination) then
    raise EEnsembleStudioMidiStream.Create(
      'browser MIDI Blob fixture exceeds Integer');
  LOld := Length(ADestination);
  SetLength(ADestination, LOld + ASource.length);
  for I := 0 to ASource.length - 1 do
    ADestination[LOld + I] := ASource[I];
end;

function MidiBytesEqual(const ALeft, ARight: TWfcMidiBytes): Boolean;
var
  I: Integer;
begin
  if Length(ALeft) <> Length(ARight) then Exit(False);
  for I := 0 to High(ALeft) do
    if ALeft[I] <> ARight[I] then Exit(False);
  Result := True;
end;

function CollectPlannedMidiBytes(
  const APlan: TEnsembleStudioMidiPlan): TWfcMidiBytes;
var
  LBlock: TWfcMidiBytes;
  LReplay: TEnsembleStudioMidiStream;
  LStep: TWfcMusicArrangementStep;
begin
  Result := nil;
  LReplay := TEnsembleStudioMidiStream.Create(APlan);
  try
    repeat
      LStep := LReplay.NextBytes(LBlock);
      case LStep of
        wmaspProduced: AppendMidiBytes(Result, LBlock);
        wmaspCompleted: ;
        wmaspCancelled:
          raise EEnsembleStudioMidiStream.Create(
            'browser MIDI self-test replay was cancelled');
        wmaspFailed:
          raise EEnsembleStudioMidiStream.Create(LReplay.Failure);
      end;
    until LStep = wmaspCompleted;
  finally
    LReplay.Free;
  end;
end;

constructor TBrowserEnsembleStreamController.Create;
begin
  inherited Create;
end;

function TBrowserEnsembleStreamController.RequireElement(
  const AId: String): TJSElement;
begin
  Result := document.getElementById(AId);
  if Result = nil then
    raise EEnsembleStudioStream.Create('stream UI is missing #' + AId);
end;

function TryReadUnsigned(const AText: String; const AMaximum: Cardinal;
  out AValue: Cardinal): Boolean;
var
  I, LBase, LDigit, LStart: Integer;
  LValue: Cardinal;
begin
  Result := False;
  AValue := 0;
  if AText = '' then Exit;
  LBase := 10;
  LStart := 1;
  if AText[1] = '$' then
  begin
    LBase := 16;
    LStart := 2;
  end
  else if (Length(AText) >= 2) and (AText[1] = '0') and
      ((AText[2] = 'x') or (AText[2] = 'X')) then
  begin
    LBase := 16;
    LStart := 3;
  end;
  if LStart > Length(AText) then Exit;
  LValue := 0;
  for I := LStart to Length(AText) do
  begin
    if AText[I] in ['0'..'9'] then LDigit := Ord(AText[I]) - Ord('0')
    else if AText[I] in ['a'..'f'] then LDigit := Ord(AText[I]) - Ord('a') + 10
    else if AText[I] in ['A'..'F'] then LDigit := Ord(AText[I]) - Ord('A') + 10
    else Exit;
    if LDigit >= LBase then Exit;
    if LValue > (AMaximum - Cardinal(LDigit)) div Cardinal(LBase) then Exit;
    LValue := LValue * Cardinal(LBase) + Cardinal(LDigit);
  end;
  AValue := LValue;
  Result := True;
end;

function TryReadDecimalUnsigned(const AText: String;
  const AMaximum: Cardinal; out AValue: Cardinal): Boolean;
var
  I: Integer;
  LDigit: Cardinal;
begin
  Result := False;
  AValue := 0;
  if AText = '' then Exit;
  for I := 1 to Length(AText) do
  begin
    if not (AText[I] in ['0'..'9']) then Exit;
    LDigit := Ord(AText[I]) - Ord('0');
    if AValue > (AMaximum - LDigit) div 10 then Exit;
    AValue := AValue * 10 + LDigit;
  end;
  Result := True;
end;

function TBrowserEnsembleStreamController.ReadSeed: TGraphSeed;
var
  LValue: Cardinal;
begin
  if not TryReadUnsigned(Trim(FSeed.value), Cardinal($FFFFFFFF), LValue) then
    raise EEnsembleStudioStream.Create(
      'stream seed must be decimal, $hex, or 0xhex unsigned 32-bit');
  Result := TGraphSeed(LValue);
end;

function TBrowserEnsembleStreamController.ReadNonnegative(
  const AInput: TJSHTMLInputElement; const AName: String): Integer;
var
  LValue: Cardinal;
begin
  if not TryReadDecimalUnsigned(Trim(AInput.value),
      Cardinal(High(Integer)), LValue) then
    raise EEnsembleStudioStream.Create(AName +
      ' must be a whole decimal integer from 0 through ' + IntToStr(High(Integer)));
  Result := Integer(LValue);
end;

function TBrowserEnsembleStreamController.ReadOptions:
  TEnsembleStudioStreamOptions;
begin
  Result := DefaultEnsembleStudioStreamOptions;
  Result.Seed := ReadSeed;
  Result.SegmentCellCount := ReadNonnegative(FSegmentCells,
    'stream segment cells');
  if (Result.SegmentCellCount < 1) or
      (Result.SegmentCellCount > High(Integer) div
        ENSEMBLE_STUDIO_STREAM_QUANTUM) then
    raise EEnsembleStudioStream.Create('stream segment cells must be from 1 through ' +
      IntToStr(High(Integer) div ENSEMBLE_STUDIO_STREAM_QUANTUM));
  Result.MaxBacktracks := ReadNonnegative(FBacktracks,
    'stream local search allowance');
  Result.MaxPassBacktracks := ReadNonnegative(FPassBacktracks,
    'stream pass search allowance');
  Result.CaptureTrace := FTrace.checked;
end;

procedure TBrowserEnsembleStreamController.SetStatus(
  const AState, AMessage: String);
begin
  if FReleased then Exit;
  document.body.setAttribute('data-stream-state', AState);
  FStatus.textContent := AMessage;
end;

procedure TBrowserEnsembleStreamController.SetBusy(const AValue: Boolean);
begin
  FBusy := AValue;
  if FReleased then Exit;
  FStart.disabled := AValue;
  FMidiPlanButton.disabled := AValue;
  FMidiSaveButton.disabled := AValue or (FMidiPlan = nil);
  FCancel.disabled := not AValue or (FActive = nil);
end;

procedure TBrowserEnsembleStreamController.AsyncFinished;
begin
  Dec(FAsyncCount);
  if FReleased and (FAsyncCount = 0) then Free;
end;

procedure TBrowserEnsembleStreamController.RefreshPlan;
var
  LFrames: TEnsembleStudioFramePlan;
  LOptions: TEnsembleStudioStreamOptions;
  LPlan: TEnsembleStudioStreamPlan;
  LMidiCommand, LTraceOption, LWaveCommand, LWaveFailure: String;
begin
  if FReleased then Exit;
  try
    LOptions := ReadOptions;
    LFrames := PlanEnsembleStudioFrames(FSeconds.value);
    if LOptions.CaptureTrace then LTraceOption := ' --trace'
    else LTraceOption := '';
    LMidiCommand := 'EnsembleStudioMidiRender --seconds ' +
      LFrames.RequestedText + ' --seed ' + IntToStr(LOptions.Seed) +
      ' --segment-cells ' + IntToStr(LOptions.SegmentCellCount) +
      ' --backtracks ' + IntToStr(LOptions.MaxBacktracks) +
      ' --pass-backtracks ' + IntToStr(LOptions.MaxPassBacktracks) +
      LTraceOption + ' --output NEW.mid';
    LWaveCommand := '';
    LWaveFailure := '';
    try
      LPlan := PlanEnsembleStudioStream(FSeconds.value);
      FPlan.textContent := 'Requested ' + LFrames.RequestedText +
        ' s → actual ' + EnsembleStudioStreamSecondsText(LFrames.ActualTicks) +
        ' s (' + IntToStr(LFrames.CellCount) + ' eighth-note cells, ' +
        IntToStr(LPlan.ExpectedFrames) + ' PCM frames). Working segment ' +
        IntToStr(LOptions.SegmentCellCount) + ' cells; seed ' +
        IntToStr(LOptions.Seed) + '.';
      FPlan.setAttribute('data-valid', 'true');
      LWaveCommand := 'EnsembleStudioRender --seconds ' +
        LPlan.RequestedText + ' --seed ' +
      IntToStr(LOptions.Seed) + ' --segment-cells ' +
      IntToStr(LOptions.SegmentCellCount) + ' --backtracks ' +
      IntToStr(LOptions.MaxBacktracks) + ' --pass-backtracks ' +
      IntToStr(LOptions.MaxPassBacktracks) + LTraceOption +
        ' --output NEW.wav';
    except
      LWaveFailure := FailureText(JSExceptValue);
      FPlan.textContent := 'Requested ' + LFrames.RequestedText +
        ' s → actual ' + EnsembleStudioStreamSecondsText(LFrames.ActualTicks) +
        ' s (' + IntToStr(LFrames.CellCount) +
        ' eighth-note cells). MIDI planning is valid; WAVE is unavailable: ' +
        LWaveFailure + '.';
      FPlan.setAttribute('data-valid', 'false');
    end;
    FPlan.setAttribute('data-midi-valid', 'true');
    if LWaveCommand <> '' then
      FFallback.textContent := 'Native WAVE (new output): ' + LWaveCommand +
        #10 + 'Native MIDI (new output): ' + LMidiCommand
    else
      FFallback.textContent := 'Native WAVE unavailable: ' + LWaveFailure +
        #10 + 'Native MIDI (new output): ' + LMidiCommand;
  except
    FPlan.textContent := 'Stream cannot start: ' + FailureText(JSExceptValue) + '.';
    FPlan.setAttribute('data-valid', 'false');
    FPlan.setAttribute('data-midi-valid', 'false');
    FFallback.textContent := 'Correct the stream inputs to show the native command.';
  end;
end;

procedure TBrowserEnsembleStreamController.ClearMidiPlan;
begin
  FreeAndNil(FMidiPlan);
  if FReleased then Exit;
  document.body.setAttribute('data-midi-stream-plan-state', 'none');
  document.body.removeAttribute('data-midi-stream-plan-bytes');
  document.body.removeAttribute('data-midi-stream-plan-signature');
  document.body.removeAttribute('data-midi-stream-frame-signature');
  FMidiSaveButton.disabled := True;
end;

procedure TBrowserEnsembleStreamController.Cancel(const AReason: String);
begin
  if (FActive = nil) or FActive.Committing then Exit;
  FActive.Cancelled := True;
  FCancel.disabled := True;
  SetStatus('cancelling', AReason +
    ' Waiting for the current bounded generation/write step to settle.');
end;

function TBrowserEnsembleStreamController.HandleStart(
  AEvent: TJSMouseEvent): Boolean;
begin
  Result := False;
  if not FBusy then SaveStream;
end;

function TBrowserEnsembleStreamController.HandleMidiPlan(
  AEvent: TJSMouseEvent): Boolean;
begin
  Result := False;
  if not FBusy then PlanMidi;
end;

function TBrowserEnsembleStreamController.HandleMidiSave(
  AEvent: TJSMouseEvent): Boolean;
begin
  Result := False;
  if not FBusy and (FMidiPlan <> nil) then SaveMidi;
end;

function TBrowserEnsembleStreamController.HandleCancel(
  AEvent: TJSMouseEvent): Boolean;
begin
  Result := False;
  Cancel('Cancellation requested.');
end;

function TBrowserEnsembleStreamController.HandleMutation(
  AEvent: TJSEvent): Boolean;
var
  LId: String;
begin
  Result := True;
  if FReleased or not (AEvent.target is TJSElement) then Exit;
  LId := TJSElement(AEvent.target).id;
  if (LId = 'stream-start-button') or
      (LId = 'stream-midi-plan-button') or
      (LId = 'stream-midi-save-button') or
      (LId = 'stream-cancel-button') then Exit;
  if (AEvent._type = 'click') and (LId <> 'new-session-button') then Exit;
  if (AEvent._type <> 'click') and
      (LId <> 'seed-input') and (LId <> 'stream-seconds-input') and
      (LId <> 'stream-segment-cells-input') and
      (LId <> 'stream-backtracks-input') and
      (LId <> 'stream-pass-backtracks-input') and
      (LId <> 'stream-trace-input') then Exit;
  if (FActive <> nil) and FActive.Committing then
  begin
    AEvent.preventDefault;
    AEvent.stopImmediatePropagation;
    Exit(False);
  end;
  ClearMidiPlan;
  Cancel('Stream inputs changed; the captured transaction is stale.');
  if FRefreshTimer <> 0 then window.clearTimeout(FRefreshTimer);
  FRefreshTimer := window.setTimeout(
    procedure
    begin
      FRefreshTimer := 0;
      RefreshPlan;
      if not FBusy and not FReleased then
      begin
        FProgress.value := 0;
        FDetail.textContent := 'No stream save is active.';
        if (FPlan.getAttribute('data-valid') = 'true') or
            (FPlan.getAttribute('data-midi-valid') = 'true') then
          if isFunction(TJSObject(window)['showSaveFilePicker']) then
            SetStatus('ready', 'Plan updated. Save As may replace a file selected by the user.')
          else SetStatus('unavailable',
            'Plan updated. This browser needs the native host for streaming output.')
        else SetStatus('invalid', 'Correct the plan before starting a stream.');
      end;
    end, 0);
end;

procedure TBrowserEnsembleStreamController.SaveStream(
  const ATestPicker: TJSPromise); async;
var
  LBlob: TJSBlob;
  LCommitted, LCancelled: Boolean;
  LControls: TControlStates;
  LFile: TEnsembleFileHandle;
  LOptions, LType, LAccept: TJSObject;
  LOperation: TBrowserEnsembleStreamOperation;
  LPcm: TEnsembleStudioPcmStream;
  LPlan: TEnsembleStudioStreamPlan;
  LSamples: TWfcMusicPcm16Samples;
  LSink: TOneBlockSink;
  LStep: TWfcMusicArrangementStep;
  LStreamOptions: TEnsembleStudioStreamOptions;
  LWritable: TEnsembleWritableFile;
  LWriter: TWfcMusicWaveStream;
  LFailure: String;
begin
  if FReleased or FBusy then Exit;
  Inc(FAsyncCount);
  LOperation := nil;
  LPcm := nil;
  LSink := nil;
  LWriter := nil;
  LWritable := nil;
  LBlob := nil;
  LControls := nil;
  LCommitted := False;
  try
    try
      { Capacity and all numeric input are checked before the picker or any
        writable destination exists. }
      LPlan := PlanEnsembleStudioStream(FSeconds.value);
      LStreamOptions := ReadOptions;
      RefreshPlan;
      if (ATestPicker = nil) and
          not isFunction(TJSObject(window)['showSaveFilePicker']) then
      begin
        SetStatus('unavailable',
          'This browser cannot stream to a chosen file. Use the native command.');
        Exit;
      end;
      LOperation := TBrowserEnsembleStreamOperation.Create;
      LOperation.Kind := besokWave;
      FActive := LOperation;
      SetBusy(True);
      FProgress.value := 0;
      document.body.setAttribute('data-stream-written-frames', '0');
      document.body.setAttribute('data-stream-written-bytes', '0');
      document.body.setAttribute('data-stream-seam-holds', '0');
      document.body.removeAttribute('data-stream-peak-bytes');
      document.body.setAttribute('data-stream-actual-seconds',
        EnsembleStudioStreamSecondsText(LPlan.ActualTicks));
      FDetail.textContent := 'Captured seed ' + IntToStr(LStreamOptions.Seed) +
        ', requested ' + LPlan.RequestedText + ' s, actual ' +
        EnsembleStudioStreamSecondsText(LPlan.ActualTicks) + ' s. No PCM has been generated.';
      SetStatus('choosing',
        'Choose a WAVE file. Save As is user-mediated and may replace the selected file.');
      LOptions := TJSObject.new;
      LOptions['suggestedName'] := 'ensemble-stream-seed-' +
        IntToStr(LStreamOptions.Seed) + '.wav';
      LType := TJSObject.new;
      LType['description'] := 'PCM16 WAVE / RF64 audio';
      LAccept := TJSObject.new;
      LAccept['audio/wav'] := TJSArray.new('.wav');
      LType['accept'] := LAccept;
      LOptions['types'] := TJSArray.new(LType);
      { Picker call remains in the original user activation turn. }
      if ATestPicker <> nil then
        LFile := await(TEnsembleFileHandle, ATestPicker)
      else
        LFile := await(TEnsembleFileHandle,
          TEnsembleSaveWindow(window).showSaveFilePicker(LOptions));
      CheckOperation(FActive, LOperation);
      LWritable := await(TEnsembleWritableFile, LFile.createWritable);
      CheckOperation(FActive, LOperation);

      LPcm := TEnsembleStudioPcmStream.Create(LPlan, LStreamOptions);
      LSink := TOneBlockSink.Create;
      LWriter := TWfcMusicWaveStream.Create(LSink,
        ENSEMBLE_STUDIO_STREAM_SAMPLE_RATE, LPlan.ExpectedFrames);
      { Construction emits exactly one bounded header block. }
      LBlob := LSink.DetachBlob;
      await(LWritable.write(LBlob));
      LBlob := nil;
      CheckOperation(FActive, LOperation);

      repeat
        await(BrowserYield);
        CheckOperation(FActive, LOperation);
        LStep := LPcm.NextSamples(LSamples);
        case LStep of
          wmaspProduced:
            begin
              LWriter.AppendSamples(LSamples);
              LBlob := LSink.DetachBlob;
              SetStatus('writing', 'Writing bounded PCM block; segment ' +
                IntToStr(LPcm.SegmentsProduced) + '.');
              await(LWritable.write(LBlob));
              LBlob := nil;
              CheckOperation(FActive, LOperation);
              FProgress.value := LWriter.FrameCount / LPlan.ExpectedFrames;
              document.body.setAttribute('data-stream-written-frames',
                IntToStr(LWriter.FrameCount));
              document.body.setAttribute('data-stream-written-bytes',
                IntToStr(LSink.Total));
              document.body.setAttribute('data-stream-seam-holds',
                IntToStr(LPcm.SeamHoldCount));
              FDetail.textContent := IntToStr(LWriter.FrameCount) + ' / ' +
                IntToStr(LPlan.ExpectedFrames) + ' frames written; ' +
                IntToStr(LPcm.SegmentsProduced) +
                ' incrementally solved working segments; one PCM block in flight.';
            end;
          wmaspCompleted: ;
          wmaspCancelled:
            raise EBrowserEnsembleStreamCancelled.Create('stream source was cancelled');
          wmaspFailed:
            raise EEnsembleStudioStream.Create(LPcm.Failure);
        end;
      until LStep = wmaspCompleted;
      CheckOperation(FActive, LOperation);
      LWriter.Finish;
      if (LWriter.FrameCount <> LPlan.ExpectedFrames) or
          (LPcm.EmittedFrames <> LPlan.ExpectedFrames) then
        raise EEnsembleStudioStream.Create('final frame count differs from preflight');

      LOperation.Committing := True;
      FreezeControls(LControls);
      SetStatus('committing',
        'Committing the chosen file; editing and cancellation are briefly locked.');
      await(LWritable.close);
      LCommitted := True;
      if not FReleased then
      begin
        FProgress.value := 1;
        document.body.setAttribute('data-stream-peak-bytes', IntToStr(LSink.Peak));
        SetStatus('saved', 'Saved ' +
          EnsembleStudioStreamSecondsText(LPlan.ActualTicks) +
          ' seconds of continuous three-voice PCM.');
      end;
    except
      LFailure := FailureText(JSExceptValue);
      LCancelled := ((LOperation <> nil) and LOperation.Cancelled) or
        (FailureName(JSExceptValue) = 'AbortError') or
        IsCancelledFailure(JSExceptValue);
      if LPcm <> nil then LPcm.Cancel;
      if (LWritable <> nil) and not LCommitted then
      begin
        try
          await(LWritable.abort);
        except
          LFailure := LFailure + ' Abort also failed: ' + FailureText(JSExceptValue);
          LCancelled := False;
        end;
      end;
      if LCancelled then
        SetStatus('cancelled',
          'Save cancelled; no completed stream is claimed.')
      else
        SetStatus('failed', 'Save failed: ' + LFailure +
          '. No completed stream is claimed.');
    end;
  finally
    { This is transaction cleanup, not result publication. Restoring the
      captured element states is safe after Release; replacement DOM nodes
      are not referenced, and an unchanged page must not retain frozen
      unrelated editor controls. }
    RestoreControls(LControls);
    if FActive = LOperation then FActive := nil;
    LWriter.Free;
    LSink.Free;
    LPcm.Free;
    LOperation.Free;
    SetBusy(False);
    if FReleased then
    begin
      FStart.disabled := True;
      FMidiPlanButton.disabled := True;
      FMidiSaveButton.disabled := True;
      FCancel.disabled := True;
    end;
    AsyncFinished;
  end;
end;

procedure TBrowserEnsembleStreamController.PlanMidi; async;
var
  LFramePlan: TEnsembleStudioFramePlan;
  LNewPlan: TEnsembleStudioMidiPlan;
  LOperation: TBrowserEnsembleStreamOperation;
  LPlanner: TEnsembleStudioMidiPlanner;
  LStep: TWfcMusicArrangementStep;
  LStreamOptions: TEnsembleStudioStreamOptions;
  LFailure: String;
begin
  if FReleased or FBusy then Exit;
  ClearMidiPlan;
  Inc(FAsyncCount);
  LNewPlan := nil;
  LOperation := nil;
  LPlanner := nil;
  try
    try
      { MIDI has its own format capacity. Do not route this preflight through
        the WAVE/RF64 frame-count envelope. }
      LFramePlan := PlanEnsembleStudioFrames(FSeconds.value);
      LStreamOptions := ReadOptions;
      RefreshPlan;
      LOperation := TBrowserEnsembleStreamOperation.Create;
      LOperation.Kind := besokMidiPlan;
      FActive := LOperation;
      SetBusy(True);
      FProgress.value := 0;
      document.body.setAttribute('data-midi-stream-plan-state', 'planning');
      document.body.setAttribute('data-midi-stream-planned-frames', '0');
      SetStatus('midi-planning',
        'Planning MIDI before any file picker is opened.');
      FDetail.textContent := 'Counting format-0 events and bytes from a ' +
        'deterministic frame pass; no frames or event timeline are retained.';
      LPlanner := TEnsembleStudioMidiPlanner.Create(
        LFramePlan, LStreamOptions);
      repeat
        await(BrowserYield);
        CheckOperation(FActive, LOperation);
        LStep := LPlanner.Next;
        case LStep of
          wmaspProduced:
            begin
              FProgress.value := LPlanner.FramesProcessed /
                LFramePlan.CellCount;
              document.body.setAttribute('data-midi-stream-planned-frames',
                IntToStr(LPlanner.FramesProcessed));
              FDetail.textContent := 'Planning frame ' +
                IntToStr(LPlanner.FramesProcessed) + ' / ' +
                IntToStr(LFramePlan.CellCount) +
                '; no destination has been requested.';
            end;
          wmaspCompleted: ;
          wmaspCancelled:
            raise EBrowserEnsembleStreamCancelled.Create(
              'MIDI planning was cancelled');
          wmaspFailed:
            raise EEnsembleStudioMidiStream.Create(LPlanner.Failure);
        end;
      until LStep = wmaspCompleted;
      CheckOperation(FActive, LOperation);
      LNewPlan := LPlanner.DetachPlan;
      FMidiPlan := LNewPlan;
      LNewPlan := nil;
      FProgress.value := 1;
      document.body.setAttribute('data-midi-stream-plan-state', 'planned');
      document.body.setAttribute('data-midi-stream-plan-bytes',
        IntToStr(FMidiPlan.FileByteCount));
      document.body.setAttribute('data-midi-stream-plan-signature',
        IntToHex(FMidiPlan.MidiSignature, 8));
      document.body.setAttribute('data-midi-stream-frame-signature',
        IntToHex(FMidiPlan.FrameSignature, 8));
      SetStatus('midi-planned',
        'MIDI plan ready. Use Save planned MIDI in a new user click.');
      FDetail.textContent := IntToStr(FMidiPlan.FrameCount) + ' frames, ' +
        IntToStr(FMidiPlan.EventCount) + ' events, ' +
        IntToStr(FMidiPlan.FileByteCount) +
        ' file bytes. The plan retains counts and fingerprints, not music frames.';
    except
      LFailure := FailureText(JSExceptValue);
      if LPlanner <> nil then LPlanner.Cancel;
      ClearMidiPlan;
      if ((LOperation <> nil) and LOperation.Cancelled) or
          IsCancelledFailure(JSExceptValue) then
        SetStatus('cancelled',
          'MIDI planning cancelled; there is no savable plan.')
      else
        SetStatus('failed', 'MIDI planning failed: ' + LFailure +
          '. There is no savable plan.');
    end;
  finally
    if FActive = LOperation then FActive := nil;
    LNewPlan.Free;
    LPlanner.Free;
    LOperation.Free;
    SetBusy(False);
    if FReleased then
    begin
      FStart.disabled := True;
      FMidiPlanButton.disabled := True;
      FMidiSaveButton.disabled := True;
      FCancel.disabled := True;
    end;
    AsyncFinished;
  end;
end;

procedure TBrowserEnsembleStreamController.SaveMidi(
  const ATestPicker: TJSPromise); async;
var
  LBlob: TJSBlob;
  LBytes: TWfcMidiBytes;
  LCancelled, LCommitted: Boolean;
  LControls: TControlStates;
  LFile: TEnsembleFileHandle;
  LOptions, LType, LAccept: TJSObject;
  LOperation: TBrowserEnsembleStreamOperation;
  LPicker: TJSPromise;
  LReplay: TEnsembleStudioMidiStream;
  LStep: TWfcMusicArrangementStep;
  LWritable: TEnsembleWritableFile;
  LFailure: String;
  LPeak: Integer;
begin
  if FReleased or FBusy then Exit;
  if FMidiPlan = nil then
  begin
    SetStatus('invalid',
      'Plan MIDI first; saving never performs a hidden planning pass.');
    Exit;
  end;
  Inc(FAsyncCount);
  LBlob := nil;
  LControls := nil;
  LCommitted := False;
  LFile := nil;
  LOperation := nil;
  LReplay := nil;
  LWritable := nil;
  LPeak := 0;
  try
    try
      if (ATestPicker = nil) and
          not isFunction(TJSObject(window)['showSaveFilePicker']) then
      begin
        SetStatus('unavailable',
          'This browser cannot save the planned MIDI stream. Use the native command.');
        Exit;
      end;
      LOperation := TBrowserEnsembleStreamOperation.Create;
      LOperation.Kind := besokMidiSave;
      { Consume the immutable plan into this operation. Input edits can now
        invalidate the controller without freeing a plan still in use. }
      LOperation.MidiPlan := FMidiPlan;
      FMidiPlan := nil;
      FActive := LOperation;
      SetBusy(True);
      FProgress.value := 0;
      document.body.setAttribute('data-midi-stream-plan-state', 'saving');
      document.body.setAttribute('data-midi-stream-written-bytes', '0');
      document.body.removeAttribute('data-midi-stream-peak-bytes');
      SetStatus('midi-choosing',
        'Choose a MIDI file. Save As is user-mediated and may replace the selected file.');
      FDetail.textContent := 'Replaying seed ' +
        IntToStr(LOperation.MidiPlan.Options.Seed) + ' for ' +
        EnsembleStudioStreamSecondsText(
          LOperation.MidiPlan.FramePlan.ActualTicks) +
        ' seconds; the stored plan contains no frame timeline.';
      LOptions := TJSObject.new;
      LOptions['suggestedName'] := 'ensemble-stream-seed-' +
        IntToStr(LOperation.MidiPlan.Options.Seed) + '.mid';
      LType := TJSObject.new;
      LType['description'] := 'Standard MIDI File';
      LAccept := TJSObject.new;
      LAccept['audio/midi'] := TJSArray.new('.mid');
      LType['accept'] := LAccept;
      LOptions['types'] := TJSArray.new(LType);
      { This call is evaluated before the first await and therefore remains in
        the explicit Save planned MIDI user-activation turn. }
      if ATestPicker <> nil then LPicker := ATestPicker
      else LPicker := TEnsembleSaveWindow(window).showSaveFilePicker(LOptions);
      LFile := await(TEnsembleFileHandle, LPicker);
      CheckOperation(FActive, LOperation);
      LWritable := await(TEnsembleWritableFile, LFile.createWritable);
      CheckOperation(FActive, LOperation);
      LReplay := TEnsembleStudioMidiStream.Create(LOperation.MidiPlan);

      repeat
        await(BrowserYield);
        CheckOperation(FActive, LOperation);
        LStep := LReplay.NextBytes(LBytes);
        case LStep of
          wmaspProduced:
            begin
              LBlob := MidiBlockBlob(LBytes);
              if LBlob.size > LPeak then LPeak := LBlob.size;
              SetStatus('midi-writing',
                'Writing one bounded MIDI block from deterministic replay.');
              await(LWritable.write(LBlob));
              LBlob := nil;
              CheckOperation(FActive, LOperation);
              FProgress.value := LReplay.EmittedBytes /
                LOperation.MidiPlan.FileByteCount;
              document.body.setAttribute('data-midi-stream-written-bytes',
                IntToStr(LReplay.EmittedBytes));
              FDetail.textContent := IntToStr(LReplay.EmittedBytes) + ' / ' +
                IntToStr(LOperation.MidiPlan.FileByteCount) +
                ' bytes written; one byte block is in flight.';
            end;
          wmaspCompleted: ;
          wmaspCancelled:
            raise EBrowserEnsembleStreamCancelled.Create(
              'MIDI replay was cancelled');
          wmaspFailed:
            raise EEnsembleStudioMidiStream.Create(LReplay.Failure);
        end;
      until LStep = wmaspCompleted;
      CheckOperation(FActive, LOperation);
      if (LReplay.TickCount <> LOperation.MidiPlan.EndTick) or
          (LReplay.EmittedBytes <> LOperation.MidiPlan.FileByteCount) then
        raise EEnsembleStudioMidiStream.Create(
          'MIDI replay differs from the stored plan');

      LOperation.Committing := True;
      FreezeControls(LControls);
      SetStatus('midi-committing',
        'Committing the chosen MIDI file; edits and cancellation are briefly locked.');
      await(LWritable.close);
      LCommitted := True;
      if not FReleased then
      begin
        FProgress.value := 1;
        document.body.setAttribute('data-midi-stream-peak-bytes',
          IntToStr(LPeak));
        document.body.setAttribute('data-midi-stream-plan-state', 'saved');
        SetStatus('midi-saved', 'Saved verified format-0 MIDI for ' +
          EnsembleStudioStreamSecondsText(
            LOperation.MidiPlan.FramePlan.ActualTicks) + ' seconds.');
      end;
    except
      LFailure := FailureText(JSExceptValue);
      LCancelled := ((LOperation <> nil) and LOperation.Cancelled) or
        (FailureName(JSExceptValue) = 'AbortError') or
        IsCancelledFailure(JSExceptValue);
      if LReplay <> nil then LReplay.Cancel;
      if (LWritable <> nil) and not LCommitted then
      begin
        try
          await(LWritable.abort);
        except
          LFailure := LFailure + ' Abort also failed: ' +
            FailureText(JSExceptValue);
          LCancelled := False;
        end;
      end;
      if not FReleased then ClearMidiPlan;
      if LCancelled then
        SetStatus('cancelled',
          'MIDI save cancelled; no completed stream is claimed.')
      else
        SetStatus('failed', 'MIDI save failed: ' + LFailure +
          '. No completed stream is claimed.');
    end;
  finally
    RestoreControls(LControls);
    if FActive = LOperation then FActive := nil;
    LReplay.Free;
    LOperation.Free;
    SetBusy(False);
    if FReleased then
    begin
      FStart.disabled := True;
      FMidiPlanButton.disabled := True;
      FMidiSaveButton.disabled := True;
      FCancel.disabled := True;
    end;
    AsyncFinished;
  end;
end;

procedure BrowserAssert(const ACondition: Boolean; const AMessage: String);
begin
  if not ACondition then
    raise EEnsembleStudioStream.Create('stream browser self-test: ' + AMessage);
end;

procedure TBrowserEnsembleStreamController.CheckSaveFixture(
  const AKind: String); async;
var
  LAbortCalls, LCloseCalls, LOpenCalls, LWriteCalls: Integer;
  LFile, LWritable: TJSObject;
  LInFlight: Boolean;
  LOriginalSeconds, LOriginalSegmentCells, LOriginalBacktracks: String;
  LPicker: TJSPromise;
  LStateAtRelease: String;
  LWritten: NativeInt;
begin
  LAbortCalls := 0;
  LCloseCalls := 0;
  LOpenCalls := 0;
  LWriteCalls := 0;
  LInFlight := False;
  LWritten := 0;
  LStateAtRelease := '';
  LWritable := TJSObject.new;
  LWritable['write'] := function(AData: JSValue): JSValue
    begin
      BrowserAssert(not LInFlight, 'writes cannot overlap');
      BrowserAssert(TJSBlob(AData).size <= WFC_MUSIC_AUDIO_STREAM_BLOCK_BYTES,
        'actual Blob exceeds one PCM block');
      LInFlight := True;
      Inc(LWriteCalls);
      Inc(LWritten, TJSBlob(AData).size);
      if (AKind = 'cancel') then FCancel.click;
      if AKind = 'edit' then FSeconds.dispatchEvent(TJSEvent.new('input'));
      Result := BrowserYield._then(function(AValue: JSValue): JSValue
        begin
          LInFlight := False;
          if AKind = 'write-failure' then
            Result := TJSPromise.reject('fixture write failure')
          else Result := Null;
        end);
    end;
  LWritable['close'] := function: JSValue
    begin
      BrowserAssert(not LInFlight, 'close waits for the final write');
      BrowserAssert(FSeed.disabled and FSeconds.disabled and FCancel.disabled,
        'irreversible commit freezes edits and cancellation');
      Inc(LCloseCalls);
      if AKind = 'release-close' then
      begin
        LStateAtRelease := document.body.getAttribute('data-stream-state');
        Release;
        Result := BrowserYield;
      end
      else if AKind = 'close-failure' then
        Result := TJSPromise.reject('fixture close failure')
      else Result := BrowserYield;
    end;
  LWritable['abort'] := function: JSValue
    begin
      BrowserAssert(not LInFlight, 'abort waits for the pending write');
      Inc(LAbortCalls);
      Result := BrowserYield;
    end;
  LFile := TJSObject.new;
  LFile['createWritable'] := function: JSValue
    begin
      Inc(LOpenCalls);
      Result := TJSPromise.resolve(LWritable);
    end;
  LPicker := TJSPromise.resolve(LFile);
  if AKind = 'stale' then
    LPicker := LPicker._then(function(AValue: JSValue): JSValue
      begin
        FActive := nil;
        Result := AValue;
      end);
  LOriginalSeconds := FSeconds.value;
  LOriginalSegmentCells := FSegmentCells.value;
  LOriginalBacktracks := FBacktracks.value;
  try
    if AKind = 'invalid' then FSeconds.value := '0'
    else FSeconds.value := '1.5';
    if AKind = 'invalid-segment' then
      FSegmentCells.value := IntToStr(
        High(Integer) div ENSEMBLE_STUDIO_STREAM_QUANTUM + 1);
    if AKind = 'invalid-budget' then FBacktracks.value := '2x';
    SetBusy(False);
    await(SaveStream(LPicker));
    if AKind = 'saved' then
    begin
      BrowserAssert((LOpenCalls = 1) and (LWriteCalls > 2) and
        (LWritten = 44 + 66150 * 2) and (LCloseCalls = 1) and
        (LAbortCalls = 0), 'successful bounded stream commits exact bytes once');
      BrowserAssert(document.body.getAttribute('data-stream-state') = 'saved',
        'saved state requires successful close');
      BrowserAssert(document.body.getAttribute('data-stream-seam-holds') <> '0',
        'five-cell segments preserve a real held seam');
    end
    else if (AKind = 'cancel') or (AKind = 'edit') then
    begin
      BrowserAssert((LOpenCalls = 1) and (LWriteCalls = 1) and
        (LCloseCalls = 0) and (LAbortCalls = 1),
        'cancel/edit aborts after the pending header write');
      BrowserAssert(document.body.getAttribute('data-stream-state') = 'cancelled',
        'cancelled transaction cannot claim completion');
    end
    else if (AKind = 'write-failure') or (AKind = 'close-failure') then
    begin
      BrowserAssert((LOpenCalls = 1) and (LAbortCalls = 1),
        'write/close failure aborts the writable transaction');
      BrowserAssert(document.body.getAttribute('data-stream-state') = 'failed',
        'failed transaction cannot claim completion');
    end
    else if AKind = 'release-close' then
    begin
      BrowserAssert((LOpenCalls = 1) and (LWriteCalls > 2) and
        (LCloseCalls = 1) and (LAbortCalls = 0),
        'release during close lets the irreversible commit settle once');
      BrowserAssert(FReleased,
        'release-close fixture releases the temporary controller');
      BrowserAssert(document.body.getAttribute('data-stream-state') =
        LStateAtRelease,
        'released controller publishes no post-close status');
      BrowserAssert(not document.body.hasAttribute('data-stream-peak-bytes'),
        'released controller publishes no post-close peak marker');
      BrowserAssert((FStart.onclick = nil) and (FCancel.onclick = nil) and
        not FSeed.disabled and FStart.disabled and FCancel.disabled,
        'release detaches handlers and transaction cleanup restores controls');
    end
    else if AKind = 'stale' then
    begin
      BrowserAssert((LOpenCalls = 0) and (LWriteCalls = 0) and
        (LCloseCalls = 0) and (LAbortCalls = 0),
        'stale picker completion cannot open a destination');
    end
    else
      BrowserAssert((LOpenCalls = 0) and (LWriteCalls = 0) and
        (LCloseCalls = 0) and (LAbortCalls = 0),
        'invalid preflight cannot reach the picker or writable');
    BrowserAssert(not FBusy and (FActive = nil),
      'async operation releases active state');
  finally
    FSeconds.value := LOriginalSeconds;
    FSegmentCells.value := LOriginalSegmentCells;
    FBacktracks.value := LOriginalBacktracks;
    SetBusy(False);
  end;
end;

procedure TBrowserEnsembleStreamController.CheckMidiSaveFixture(
  const AKind: String); async;
var
  LAbortCalls, LCloseCalls, LOpenCalls, LWriteCalls: Integer;
  LExpectedData, LObservedData: TWfcMidiBytes;
  LExpectedBytes: TWfcMidiStreamCount;
  LFile, LWritable: TJSObject;
  LInFlight: Boolean;
  LOriginalSeconds: String;
  LPicker: TJSPromise;
  LPlanSignature, LStateAtRelease: String;
  LWritten: NativeInt;
begin
  LAbortCalls := 0;
  LCloseCalls := 0;
  LOpenCalls := 0;
  LWriteCalls := 0;
  LExpectedBytes := 0;
  LExpectedData := nil;
  LInFlight := False;
  LObservedData := nil;
  LWritten := 0;
  LPlanSignature := '';
  LStateAtRelease := '';
  LOriginalSeconds := FSeconds.value;
  try
    FSeconds.value := '1.5';
    ClearMidiPlan;
    SetBusy(False);
    RefreshPlan;
    if AKind = 'no-plan' then
    begin
      await(SaveMidi(TJSPromise.resolve(Null)));
      BrowserAssert(document.body.getAttribute('data-stream-state') = 'invalid',
        'MIDI save without an explicit plan is rejected before the picker');
      Exit;
    end;
    if AKind = 'plan-pending-edit' then
    begin
      PlanMidi;
      FSeconds.value := '1.75';
      FSeconds.dispatchEvent(TJSEvent.new('input'));
      while FBusy do await(BrowserYield);
      BrowserAssert((FMidiPlan = nil) and FMidiSaveButton.disabled and
        (document.body.getAttribute('data-midi-stream-plan-state') = 'none'),
        'input mutation cancels a pending MIDI counting pass');
      Exit;
    end;
    await(PlanMidi);
    BrowserAssert((FMidiPlan <> nil) and
      (document.body.getAttribute('data-midi-stream-plan-state') = 'planned'),
      'MIDI counting pass produces an explicit savable plan');
    BrowserAssert(FMidiPlan.FrameCount = 6,
      'MIDI test plan uses the exact six-cell duration');
    LExpectedBytes := FMidiPlan.FileByteCount;
    LPlanSignature := IntToHex(FMidiPlan.MidiSignature, 8);
    if AKind = 'plan-stale' then
    begin
      FSeconds.value := '1.75';
      FSeconds.dispatchEvent(TJSEvent.new('input'));
      await(BrowserYield);
      BrowserAssert((FMidiPlan = nil) and FMidiSaveButton.disabled and
        (document.body.getAttribute('data-midi-stream-plan-state') = 'none'),
        'input mutation immediately invalidates the counted MIDI plan');
      Exit;
    end;
    if AKind = 'saved' then
      LExpectedData := CollectPlannedMidiBytes(FMidiPlan);

    LWritable := TJSObject.new;
    LWritable['write'] := function(AData: JSValue): JSValue
      begin
        BrowserAssert(not LInFlight, 'MIDI writes cannot overlap');
        BrowserAssert((TJSBlob(AData).size > 0) and
          (TJSBlob(AData).size <= WFC_MIDI_STREAM_BLOCK_BYTES),
          'MIDI Blob stays within one encoded block');
        LInFlight := True;
        Inc(LWriteCalls);
        Inc(LWritten, TJSBlob(AData).size);
        if AKind = 'cancel' then FCancel.click;
        if AKind = 'edit' then
        begin
          FSeconds.value := '1.75';
          FSeconds.dispatchEvent(TJSEvent.new('input'));
        end;
        if AKind = 'saved' then
          Result := TJSBlob(AData).arrayBuffer._then(
            function(AValue: JSValue): JSValue
            var
              LTyped: TJSUint8Array;
            begin
              LTyped := TJSUint8Array.new(TJSArrayBuffer(AValue));
              AppendTypedMidiBytes(LObservedData, LTyped);
              LInFlight := False;
              Result := Null;
            end)
        else
          Result := BrowserYield._then(function(AValue: JSValue): JSValue
            begin
              LInFlight := False;
              if AKind = 'write-failure' then
                Result := TJSPromise.reject('fixture MIDI write failure')
              else Result := Null;
            end);
      end;
    LWritable['close'] := function: JSValue
      begin
        BrowserAssert(not LInFlight, 'MIDI close waits for the final write');
        BrowserAssert(FSeed.disabled and FSeconds.disabled and FCancel.disabled,
          'MIDI commit freezes edits and cancellation');
        Inc(LCloseCalls);
        if AKind = 'release-close' then
        begin
          LStateAtRelease := document.body.getAttribute('data-stream-state');
          Release;
          Result := BrowserYield;
        end
        else if AKind = 'close-failure' then
          Result := TJSPromise.reject('fixture MIDI close failure')
        else Result := BrowserYield;
      end;
    LWritable['abort'] := function: JSValue
      begin
        BrowserAssert(not LInFlight, 'MIDI abort waits for a pending write');
        Inc(LAbortCalls);
        Result := BrowserYield;
      end;
    LFile := TJSObject.new;
    LFile['createWritable'] := function: JSValue
      begin
        Inc(LOpenCalls);
        if AKind = 'open-edit' then
        begin
          FSeconds.value := '1.75';
          FSeconds.dispatchEvent(TJSEvent.new('input'));
          Result := BrowserYield._then(function(AValue: JSValue): JSValue
            begin Result := LWritable; end);
        end
        else Result := TJSPromise.resolve(LWritable);
      end;
    LPicker := TJSPromise.resolve(LFile);
    if AKind = 'stale' then
      LPicker := LPicker._then(function(AValue: JSValue): JSValue
        begin
          FActive := nil;
          Result := AValue;
        end);
    await(SaveMidi(LPicker));
    if AKind = 'saved' then
    begin
      BrowserAssert((LOpenCalls = 1) and (LWriteCalls > 0) and
        (LWritten = LExpectedBytes) and (LCloseCalls = 1) and
        (LAbortCalls = 0),
        'planned MIDI replay writes exact bytes and commits once');
      BrowserAssert(MidiBytesEqual(LObservedData, LExpectedData),
        'Uint8Array-backed MIDI Blobs preserve every planned replay byte');
      BrowserAssert((document.body.getAttribute('data-stream-state') =
        'midi-saved') and
        (document.body.getAttribute('data-midi-stream-plan-signature') =
          LPlanSignature),
        'MIDI saved state remains bound to the counted plan signature');
      BrowserAssert(document.body.getAttribute(
        'data-midi-stream-peak-bytes') <> '',
        'MIDI save publishes its bounded peak block only after close');
    end
    else if (AKind = 'cancel') or (AKind = 'edit') then
    begin
      BrowserAssert((LOpenCalls = 1) and (LWriteCalls = 1) and
        (LCloseCalls = 0) and (LAbortCalls = 1),
        'MIDI cancel/edit aborts after the pending block');
      BrowserAssert(document.body.getAttribute('data-stream-state') =
        'cancelled', 'cancelled MIDI replay cannot claim completion');
    end
    else if AKind = 'open-edit' then
    begin
      BrowserAssert((LOpenCalls = 1) and (LWriteCalls = 0) and
        (LCloseCalls = 0) and (LAbortCalls = 1),
        'edit while createWritable is pending aborts the returned writable');
      BrowserAssert(document.body.getAttribute('data-stream-state') =
        'cancelled', 'stale writable completion cannot start MIDI replay');
    end
    else if (AKind = 'write-failure') or (AKind = 'close-failure') then
    begin
      BrowserAssert((LOpenCalls = 1) and (LAbortCalls = 1),
        'MIDI write/close failure aborts its writable transaction');
      BrowserAssert(document.body.getAttribute('data-stream-state') = 'failed',
        'failed MIDI replay cannot claim completion');
    end
    else if AKind = 'release-close' then
    begin
      BrowserAssert((LOpenCalls = 1) and (LWriteCalls > 0) and
        (LCloseCalls = 1) and (LAbortCalls = 0),
        'release during MIDI close lets the irreversible commit settle once');
      BrowserAssert(FReleased and
        (document.body.getAttribute('data-stream-state') = LStateAtRelease),
        'released MIDI controller publishes no post-close status');
      BrowserAssert(not document.body.hasAttribute(
        'data-midi-stream-peak-bytes'),
        'released MIDI controller publishes no post-close peak marker');
      BrowserAssert((FStart.onclick = nil) and
        (FMidiPlanButton.onclick = nil) and
        (FMidiSaveButton.onclick = nil) and (FCancel.onclick = nil) and
        not FSeed.disabled and FStart.disabled and
        FMidiPlanButton.disabled and FMidiSaveButton.disabled and
        FCancel.disabled,
        'MIDI release detaches handlers and cleanup restores shared controls');
    end
    else if AKind = 'stale' then
    begin
      BrowserAssert((LOpenCalls = 0) and (LWriteCalls = 0) and
        (LCloseCalls = 0) and (LAbortCalls = 0),
        'stale MIDI picker completion cannot open a destination');
    end;
    BrowserAssert(not FBusy and (FActive = nil),
      'MIDI async operation releases active state');
  finally
    FSeconds.value := LOriginalSeconds;
    SetBusy(False);
  end;
end;

procedure TBrowserEnsembleStreamController.RunSelfTest; async;
var
  LMidiReleaseController, LReleaseController:
    TBrowserEnsembleStreamController;
begin
  if FReleased or FSelfTestRunning or FBusy then Exit;
  FSelfTestRunning := True;
  Inc(FAsyncCount);
  document.body.setAttribute('data-stream-self-test', 'pending');
  document.body.setAttribute('data-midi-stream-self-test', 'pending');
  LMidiReleaseController := nil;
  LReleaseController := nil;
  try
    try
      BrowserAssert(Pos('--backtracks 256 --pass-backtracks 16',
        FFallback.textContent) > 0,
        'native fallback preserves the captured search allowances');
      FTrace.checked := True;
      RefreshPlan;
      BrowserAssert(Pos(' --trace --output ', FFallback.textContent) > 0,
        'native fallback preserves trace capture');
      FTrace.checked := False;
      RefreshPlan;
      await(CheckSaveFixture('saved'));
      await(CheckSaveFixture('cancel'));
      await(CheckSaveFixture('edit'));
      await(CheckSaveFixture('write-failure'));
      await(CheckSaveFixture('close-failure'));
      await(CheckSaveFixture('stale'));
      await(CheckSaveFixture('invalid'));
      await(CheckSaveFixture('invalid-segment'));
      await(CheckSaveFixture('invalid-budget'));
      if FReleased then Exit;
      LReleaseController := TBrowserEnsembleStreamController.Create;
      try
        LReleaseController.Run;
        { An extra outer async lease keeps the temporary controller alive
          after Release until its fixture can inspect the settled close. }
        Inc(LReleaseController.FAsyncCount);
        try
          await(LReleaseController.CheckSaveFixture('release-close'));
        finally
          if not LReleaseController.FReleased then
            LReleaseController.Release;
          LReleaseController.AsyncFinished;
          LReleaseController := nil;
        end;
      finally
        { The temporary controller owned the same fixture briefly. Restore the
          real controller's direct button handlers after its listener cleanup. }
        FStart.onclick := @HandleStart;
        FMidiPlanButton.onclick := @HandleMidiPlan;
        FMidiSaveButton.onclick := @HandleMidiSave;
        FCancel.onclick := @HandleCancel;
        SetBusy(False);
      end;
      BrowserAssert(not FStart.disabled and FCancel.disabled,
        'parent stream controls are usable after the release fixture');
      document.body.setAttribute('data-stream-cancel', 'passed');
      document.body.setAttribute('data-stream-stale', 'passed');
      document.body.setAttribute('data-stream-write-failure', 'passed');
      document.body.setAttribute('data-stream-commit', 'passed');
      document.body.setAttribute('data-stream-release', 'passed');
      document.body.setAttribute('data-stream-self-test', 'passed');

      await(CheckMidiSaveFixture('no-plan'));
      await(CheckMidiSaveFixture('plan-pending-edit'));
      await(CheckMidiSaveFixture('plan-stale'));
      await(CheckMidiSaveFixture('saved'));
      await(CheckMidiSaveFixture('cancel'));
      await(CheckMidiSaveFixture('edit'));
      await(CheckMidiSaveFixture('open-edit'));
      await(CheckMidiSaveFixture('write-failure'));
      await(CheckMidiSaveFixture('close-failure'));
      await(CheckMidiSaveFixture('stale'));
      if FReleased then Exit;
      LMidiReleaseController := TBrowserEnsembleStreamController.Create;
      try
        LMidiReleaseController.Run;
        Inc(LMidiReleaseController.FAsyncCount);
        try
          await(LMidiReleaseController.CheckMidiSaveFixture('release-close'));
        finally
          if not LMidiReleaseController.FReleased then
            LMidiReleaseController.Release;
          LMidiReleaseController.AsyncFinished;
          LMidiReleaseController := nil;
        end;
      finally
        FStart.onclick := @HandleStart;
        FMidiPlanButton.onclick := @HandleMidiPlan;
        FMidiSaveButton.onclick := @HandleMidiSave;
        FCancel.onclick := @HandleCancel;
        SetBusy(False);
      end;
      BrowserAssert(not FStart.disabled and not FMidiPlanButton.disabled and
        FMidiSaveButton.disabled and FCancel.disabled,
        'parent controls are usable after the MIDI release fixture');
      document.body.setAttribute('data-midi-stream-release', 'passed');
      document.body.setAttribute('data-midi-stream-self-test', 'passed');
      RefreshPlan;
      FProgress.value := 0;
      SetStatus('ready',
        'Stream transaction checks passed; no user file was opened.');
      FDetail.textContent :=
        'Fake picker checks covered WAVE and MIDI preflight, held seams, cancellation, stale completion, bounded writes, replay, and commit failure.';
    except
      if not FReleased then
      begin
        document.body.setAttribute('data-stream-self-test', 'failed');
        document.body.setAttribute('data-midi-stream-self-test', 'failed');
        document.body.setAttribute('data-stream-test-message',
          FailureText(JSExceptValue));
        SetStatus('failed', FailureText(JSExceptValue));
      end;
    end;
  finally
    FSelfTestRunning := False;
    AsyncFinished;
  end;
end;

procedure TBrowserEnsembleStreamController.Run;
begin
  FSeed := TJSHTMLInputElement(RequireElement('seed-input'));
  FSeconds := TJSHTMLInputElement(RequireElement('stream-seconds-input'));
  FSegmentCells := TJSHTMLInputElement(
    RequireElement('stream-segment-cells-input'));
  FBacktracks := TJSHTMLInputElement(
    RequireElement('stream-backtracks-input'));
  FPassBacktracks := TJSHTMLInputElement(
    RequireElement('stream-pass-backtracks-input'));
  FTrace := TJSHTMLInputElement(RequireElement('stream-trace-input'));
  FStart := TJSHTMLButtonElement(RequireElement('stream-start-button'));
  FMidiPlanButton := TJSHTMLButtonElement(
    RequireElement('stream-midi-plan-button'));
  FMidiSaveButton := TJSHTMLButtonElement(
    RequireElement('stream-midi-save-button'));
  FCancel := TJSHTMLButtonElement(RequireElement('stream-cancel-button'));
  FProgress := TJSHTMLProgressElement(RequireElement('stream-progress'));
  FStatus := RequireElement('stream-status');
  FDetail := RequireElement('stream-detail');
  FPlan := RequireElement('stream-plan');
  FFallback := RequireElement('stream-fallback');
  FStart.onclick := @HandleStart;
  FMidiPlanButton.onclick := @HandleMidiPlan;
  FMidiSaveButton.onclick := @HandleMidiSave;
  FCancel.onclick := @HandleCancel;
  document.addEventListener('input', @HandleMutation, True);
  document.addEventListener('change', @HandleMutation, True);
  document.addEventListener('click', @HandleMutation, True);
  FBound := True;
  ClearMidiPlan;
  SetBusy(False);
  RefreshPlan;
  if isFunction(TJSObject(window)['showSaveFilePicker']) then
    SetStatus('ready', 'Ready to stream. Save As may replace a user-selected file.')
  else SetStatus('unavailable',
    'Direct streaming save is unavailable here; use the displayed native command.');
end;

procedure TBrowserEnsembleStreamController.Release;
begin
  if FReleased then Exit;
  FReleased := True;
  if FActive <> nil then FActive.Cancelled := True;
  FreeAndNil(FMidiPlan);
  if FRefreshTimer <> 0 then
  begin
    window.clearTimeout(FRefreshTimer);
    FRefreshTimer := 0;
  end;
  if FBound then
  begin
    TEnsembleEventDocument(document).removeEventListener(
      'input', @HandleMutation, True);
    TEnsembleEventDocument(document).removeEventListener(
      'change', @HandleMutation, True);
    TEnsembleEventDocument(document).removeEventListener(
      'click', @HandleMutation, True);
    FStart.onclick := nil;
    FMidiPlanButton.onclick := nil;
    FMidiSaveButton.onclick := nil;
    FCancel.onclick := nil;
  end;
  if FAsyncCount = 0 then Free;
end;

procedure InstallEnsembleStreamBrowserTestFixture;
begin
  TJSHTMLElement(document.body).innerHTML :=
    '<main><input id="seed-input" inputmode="text" autocomplete="off" value="0">' +
    '<input id="stream-seconds-input" inputmode="decimal" autocomplete="off" value="1.5">' +
    '<input id="stream-segment-cells-input" type="number" min="1" step="1" value="5">' +
    '<input id="stream-backtracks-input" type="number" min="0" step="1" value="256">' +
    '<input id="stream-pass-backtracks-input" type="number" min="0" step="1" value="16">' +
    '<input id="stream-trace-input" type="checkbox">' +
    '<button id="new-session-button" type="button"></button>' +
    '<button id="stream-start-button" type="button"></button>' +
    '<button id="stream-midi-plan-button" type="button"></button>' +
    '<button id="stream-midi-save-button" type="button" disabled></button>' +
    '<button id="stream-cancel-button" type="button"></button>' +
    '<progress id="stream-progress" max="1" value="0"></progress>' +
    '<span id="stream-status"></span><span id="stream-detail"></span>' +
    '<span id="stream-plan"></span><pre id="stream-fallback"></pre></main>';
end;

end.
