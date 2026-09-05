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
  ensemble_studio_stream;

type
  TBrowserEnsembleStreamOperation = class
  public
    Cancelled, Committing: Boolean;
  end;

  { Browser transport for the independent long-form stream. It owns no finite
    editor state. Release detaches listeners immediately and delays destruction
    until outstanding picker/write promises settle. }
  TBrowserEnsembleStreamController = class
  strict private
    FSeed, FSeconds, FSegmentCells, FBacktracks, FPassBacktracks:
      TJSHTMLInputElement;
    FTrace: TJSHTMLInputElement;
    FStart, FCancel: TJSHTMLButtonElement;
    FProgress: TJSHTMLProgressElement;
    FStatus, FDetail, FPlan, FFallback: TJSElement;
    FActive: TBrowserEnsembleStreamOperation;
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
    procedure Cancel(const AReason: String);
    procedure AsyncFinished;
    function HandleStart(AEvent: TJSMouseEvent): Boolean;
    function HandleCancel(AEvent: TJSMouseEvent): Boolean;
    function HandleMutation(AEvent: TJSEvent): Boolean;
    procedure SaveStream(const ATestPicker: TJSPromise = nil); async;
    procedure CheckSaveFixture(const AKind: String); async;
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
  FCancel.disabled := not AValue or (FActive = nil);
end;

procedure TBrowserEnsembleStreamController.AsyncFinished;
begin
  Dec(FAsyncCount);
  if FReleased and (FAsyncCount = 0) then Free;
end;

procedure TBrowserEnsembleStreamController.RefreshPlan;
var
  LOptions: TEnsembleStudioStreamOptions;
  LPlan: TEnsembleStudioStreamPlan;
  LTraceOption: String;
begin
  if FReleased then Exit;
  try
    LOptions := ReadOptions;
    LPlan := PlanEnsembleStudioStream(FSeconds.value);
    FPlan.textContent := 'Requested ' + LPlan.RequestedText +
      ' s → actual ' + EnsembleStudioStreamSecondsText(LPlan.ActualTicks) +
      ' s (' + IntToStr(LPlan.CellCount) + ' eighth-note cells, ' +
      IntToStr(LPlan.ExpectedFrames) + ' PCM frames). Working segment ' +
      IntToStr(LOptions.SegmentCellCount) + ' cells; seed ' +
      IntToStr(LOptions.Seed) + '.';
    FPlan.setAttribute('data-valid', 'true');
    if LOptions.CaptureTrace then LTraceOption := ' --trace'
    else LTraceOption := '';
    FFallback.textContent := 'Native fallback (new output path): ' +
      'EnsembleStudioRender --seconds ' + LPlan.RequestedText + ' --seed ' +
      IntToStr(LOptions.Seed) + ' --segment-cells ' +
      IntToStr(LOptions.SegmentCellCount) + ' --backtracks ' +
      IntToStr(LOptions.MaxBacktracks) + ' --pass-backtracks ' +
      IntToStr(LOptions.MaxPassBacktracks) + LTraceOption +
      ' --output NEW.wav';
  except
    FPlan.textContent := 'Stream cannot start: ' + FailureText(JSExceptValue) + '.';
    FPlan.setAttribute('data-valid', 'false');
    FFallback.textContent := 'Correct the stream inputs to show the native command.';
  end;
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
  if (LId = 'stream-start-button') or (LId = 'stream-cancel-button') then Exit;
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
        if FPlan.getAttribute('data-valid') = 'true' then
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

procedure TBrowserEnsembleStreamController.RunSelfTest; async;
var
  LReleaseController: TBrowserEnsembleStreamController;
begin
  if FReleased or FSelfTestRunning or FBusy then Exit;
  FSelfTestRunning := True;
  Inc(FAsyncCount);
  document.body.setAttribute('data-stream-self-test', 'pending');
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
      RefreshPlan;
      FProgress.value := 0;
      SetStatus('ready',
        'Stream transaction checks passed; no user file was opened.');
      FDetail.textContent :=
        'Fake picker checks covered capacity preflight, held seams, cancellation, stale completion, bounded writes, and commit failure.';
    except
      if not FReleased then
      begin
        document.body.setAttribute('data-stream-self-test', 'failed');
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
  FCancel := TJSHTMLButtonElement(RequireElement('stream-cancel-button'));
  FProgress := TJSHTMLProgressElement(RequireElement('stream-progress'));
  FStatus := RequireElement('stream-status');
  FDetail := RequireElement('stream-detail');
  FPlan := RequireElement('stream-plan');
  FFallback := RequireElement('stream-fallback');
  FStart.onclick := @HandleStart;
  FCancel.onclick := @HandleCancel;
  document.addEventListener('input', @HandleMutation, True);
  document.addEventListener('change', @HandleMutation, True);
  document.addEventListener('click', @HandleMutation, True);
  FBound := True;
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
    '<button id="stream-cancel-button" type="button"></button>' +
    '<progress id="stream-progress" max="1" value="0"></progress>' +
    '<span id="stream-status"></span><span id="stream-detail"></span>' +
    '<span id="stream-plan"></span><pre id="stream-fallback"></pre></main>';
end;

end.
