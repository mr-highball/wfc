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
unit browser_music_arrangement;

{$mode delphi}{$H+}
{$modeswitch externalclass}

interface

uses
  JS, Web, SysUtils, wfc, wfc_music_arrangement, music_studio_workbench;

type
  TBrowserMusicArrangementOperation = class
  public
    Cancelled, Committing: Boolean;
  end;

  { All solver/audio work remains in shared Pascal units; this controller owns
    only browser interaction and one-section transfers. Its owner calls Release,
    not Free: listeners detach immediately and destruction waits for outstanding
    picker/write promises, without retaining the borrowed studio. }
  TBrowserMusicArrangement = class
  strict private
    FStudio: TWfcMusicStudio;
    FDuration, FSeed: TJSHTMLInputElement;
    FStart, FCancel: TJSHTMLButtonElement;
    FProgress: TJSHTMLProgressElement;
    FStatus, FDetail, FPlan, FPitches, FFallback: TJSElement;
    FActive: TBrowserMusicArrangementOperation;
    FBusy, FReleased, FBound: Boolean;
    FAsyncCount: Integer;
    FRefreshTimer: NativeInt;
    function RequireElement(const AId: String): TJSElement;
    procedure SetStatus(const AState, AMessage: String);
    procedure RefreshFallback;
    procedure SetBusy(const AValue: Boolean);
    procedure AsyncFinished;
    procedure Cancel(const AReason: String);
    function HandleStart(AEvent: TJSMouseEvent): Boolean;
    function HandleCancel(AEvent: TJSMouseEvent): Boolean;
    function HandleMutation(AEvent: TJSEvent): Boolean;
    procedure SaveComposition(const ATestPicker: TJSPromise = nil); async;
    procedure CheckSaveFixture(const AKind: String); async;
    procedure RunSelfTest; async;
  public
    constructor Create(const AStudio: TWfcMusicStudio);
    procedure Run;
    procedure Release;
  end;

implementation

uses
  wfc_music, wfc_music_sequence, wfc_music_audio, wfc_music_audio_stream,
  music_studio_arrangement;

const
  SAMPLE_RATE = 44100;
  TICKS_PER_SECOND = 960;
  MAX_SECTION_BYTES = 4 * SAMPLE_RATE * 2 + 80;

type
  EArrangementCancelled = class(Exception);

  { Small bindings for standard browser APIs absent from older pas2js RTLs.
    https://wicg.github.io/file-system-access/#api-showsavefilepicker
    https://fs.spec.whatwg.org/#filesystemwritablefilestream }
  TMusicWritableFile = class external name 'Object' (TJSObject)
    function write(const AData: TJSBlob): TJSPromise;
    function close: TJSPromise;
    function abort: TJSPromise;
  end;
  TMusicFileHandle = class external name 'Object' (TJSObject)
    function createWritable: TJSPromise;
  end;
  TMusicSaveWindow = class external name 'Window' (TJSWindow)
    function showSaveFilePicker(const AOptions: TJSObject): TJSPromise; reintroduce;
  end;
  TMusicEventDocument = class external name 'Document' (TJSObject)
    procedure removeEventListener(const AName: String;
      const AListener: TJSEventHandler; const AUseCapture: Boolean);
  end;

  { Synchronous sink copies borrowed encoder blocks. DetachBlob transfers at
    most a single section plus its initial header. The next section is not
    generated until the previous browser write promise resolves. }
  TSectionByteSink = class(TWfcMusicAudioByteSink)
  private
    FParts: TJSArray;
    FCountOnly: Boolean;
    FBuffered, FPeak: Integer;
    FTotal: TWfcMusicAudioStreamCount;
  public
    constructor Create(const ACountOnly: Boolean);
    procedure WriteBytes(const ABytes: array of Byte); override;
    function DetachBlob: TJSBlob;
    procedure DiscardSection;
    property Total: TWfcMusicAudioStreamCount read FTotal;
    property Peak: Integer read FPeak;
    property Buffered: Integer read FBuffered;
  end;

  TControlStates = array of record
    Element: TJSElement;
    WasDisabled: Boolean;
  end;

function OperationCurrent(const AActive, ACandidate:
  TBrowserMusicArrangementOperation): Boolean;
begin
  Result := (ACandidate <> nil) and (AActive = ACandidate) and
    not ACandidate.Cancelled;
end;

procedure CheckOperation(const AActive, ACandidate:
  TBrowserMusicArrangementOperation);
begin
  if not OperationCurrent(AActive, ACandidate) then
    raise EArrangementCancelled.Create('composition save was cancelled');
end;

function BrowserYield: TJSPromise;
begin
  Result := TJSPromise.new(
    procedure(AResolve, AReject: TJSPromiseResolver)
    begin
      window.setTimeout(procedure begin AResolve(Null); end, 0);
    end);
end;

function BrowserFailureText(const AValue: JSValue): String;
begin
  if isObject(AValue) and (TObject(AValue) is Exception) then
    Exit(Exception(AValue).Message);
  if isObject(AValue) and
    not isUndefined(TJSObject(AValue)['message']) then
    Result := String(TJSObject(AValue)['message'])
  else if isString(AValue) then Result := String(AValue)
  else Result := 'browser operation failed';
end;

function BrowserFailureName(const AValue: JSValue): String;
begin
  Result := '';
  if isObject(AValue) and not isUndefined(TJSObject(AValue)['name']) then
    Result := String(TJSObject(AValue)['name']);
end;

function TryParseSessionSeed(const AText: String;
  out ASeed: TGraphSeed): Boolean;
var
  LText: String;
  I, LStart, LBase, LDigit: Integer;
  LValue: NativeInt;
  C: Char;
begin
  Result := False;
  ASeed := 0;
  LText := Trim(AText);
  if LText = '' then Exit;
  LStart := 1;
  LBase := 10;
  if LText[1] = '$' then begin LStart := 2; LBase := 16; end
  else if (Length(LText) >= 2) and (LText[1] = '0') and
    ((LText[2] = 'x') or (LText[2] = 'X')) then
  begin LStart := 3; LBase := 16; end;
  if LStart > Length(LText) then Exit;
  LValue := 0;
  for I := LStart to Length(LText) do
  begin
    C := LText[I];
    if C in ['0'..'9'] then LDigit := Ord(C) - Ord('0')
    else if (LBase = 16) and (C in ['a'..'f']) then
      LDigit := Ord(C) - Ord('a') + 10
    else if (LBase = 16) and (C in ['A'..'F']) then
      LDigit := Ord(C) - Ord('A') + 10
    else Exit;
    if LValue > (4294967295 - LDigit) div LBase then Exit;
    LValue := LValue * LBase + LDigit;
  end;
  ASeed := TGraphSeed(LValue);
  Result := True;
end;

function ExpectedFrames(const AConfig: TWfcMusicArrangementConfig):
  TWfcMusicAudioStreamCount;
var LSeconds: TWfcMusicArrangementWide;
begin
  { Config rounds to complete two-second bars. Divide before multiplying and
    validate before either a file picker or a file-header write. }
  if (AConfig.RequestedTicks mod TICKS_PER_SECOND) <> 0 then
    raise EMusicStudio.Create('arrangement must end on a complete bar');
  LSeconds := AConfig.RequestedTicks div TICKS_PER_SECOND;
  if LSeconds > WFC_MUSIC_AUDIO_STREAM_MAX_FRAMES div SAMPLE_RATE then
    raise EMusicStudio.Create('duration exceeds the exact WAVE file-size envelope');
  Result := LSeconds * SAMPLE_RATE;
end;

function SectionPitches(const ASection: TWfcMusicArrangementSection): String;
var LCells: TWfcMusicMelodyCells; I: Integer;
begin
  LCells := ASection.Composition.CopyMelodyCells;
  Result := '';
  for I := 0 to High(LCells) do
  begin
    if I > 0 then Result := Result + ' ';
    if LCells[I].Action = wmcaRest then Result := Result + 'rest'
    else
    begin
      Result := Result + MusicStudioPitchName(LCells[I].Pitch);
      if LCells[I].Action = wmcaHold then Result := Result + '~';
    end;
  end;
end;

procedure AppendSection(const ASection: TWfcMusicArrangementSection;
  const AWriter: TWfcMusicWaveStream);
var LScore: TWfcMusicScore; LClip: TWfcMusicPcm16Clip;
begin
  LScore := ASection.Composition.CopyScore;
  try
    LClip := RenderWfcMusicAudio(LScore, DefaultWfcMusicAudioOptions);
    try
      AWriter.AppendClip(LClip);
    finally
      LClip.Free;
    end;
  finally
    LScore.Free;
  end;
end;

procedure FreezeControls(out AStates: TControlStates);
var LControls: TJSNodeList; I: Integer; LElement: TJSElement;
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
var I: Integer;
begin
  for I := 0 to High(AStates) do
    if not AStates[I].WasDisabled then
      AStates[I].Element.removeAttribute('disabled');
end;

constructor TSectionByteSink.Create(const ACountOnly: Boolean);
begin
  inherited Create;
  FCountOnly := ACountOnly;
  FParts := TJSArray.new;
end;

procedure TSectionByteSink.WriteBytes(const ABytes: array of Byte);
var LCopy: TJSUint8Array; I: Integer;
begin
  if Length(ABytes) > MAX_SECTION_BYTES - FBuffered then
    raise EMusicStudio.Create('section transfer exceeded its bounded buffer');
  if not FCountOnly then
  begin
    LCopy := TJSUint8Array.new(Length(ABytes));
    for I := 0 to High(ABytes) do LCopy[I] := ABytes[I];
    FParts.push(LCopy);
  end;
  Inc(FBuffered, Length(ABytes));
  if FBuffered > FPeak then FPeak := FBuffered;
  FTotal := FTotal + Length(ABytes);
end;

function TSectionByteSink.DetachBlob: TJSBlob;
var LOptions: TJSBlobInit;
begin
  if FCountOnly then
    raise EMusicStudio.Create('counting sink cannot produce a file block');
  LOptions := TJSBlobInit.new;
  LOptions.type_ := 'audio/wav';
  Result := TJSBlob.new(FParts, LOptions);
  DiscardSection;
end;

procedure TSectionByteSink.DiscardSection;
begin
  FParts := TJSArray.new;
  FBuffered := 0;
end;

constructor TBrowserMusicArrangement.Create(const AStudio: TWfcMusicStudio);
begin
  inherited Create;
  if AStudio = nil then raise EMusicStudio.Create('arrangement studio is required');
  FStudio := AStudio;
end;

function TBrowserMusicArrangement.RequireElement(const AId: String): TJSElement;
begin
  Result := document.getElementById(AId);
  if Result = nil then raise EMusicStudio.Create('arrangement is missing #' + AId);
end;

procedure TBrowserMusicArrangement.SetStatus(const AState, AMessage: String);
begin
  if FReleased then Exit;
  document.body.setAttribute('data-arrangement-state', AState);
  FStatus.textContent := AMessage;
end;

procedure TBrowserMusicArrangement.SetBusy(const AValue: Boolean);
begin
  FBusy := AValue;
  if FReleased then Exit;
  FStart.disabled := AValue;
  FCancel.disabled := not AValue or (FActive = nil);
end;

procedure TBrowserMusicArrangement.AsyncFinished;
begin
  Dec(FAsyncCount);
  if FReleased and (FAsyncCount = 0) then Free;
end;

procedure TBrowserMusicArrangement.Release;
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
    TMusicEventDocument(document).removeEventListener('input', @HandleMutation, True);
    TMusicEventDocument(document).removeEventListener('change', @HandleMutation, True);
    TMusicEventDocument(document).removeEventListener('click', @HandleMutation, True);
    FStart.onclick := nil;
    FCancel.onclick := nil;
  end;
  { The borrowed studio can now be freed by its owner. Each active save already
    owns detached seed/lock/source state, and checks cancellation after await. }
  FStudio := nil;
  if FAsyncCount = 0 then Free;
end;

procedure TBrowserMusicArrangement.RefreshFallback;
var LSeed: TGraphSeed; LConfig: TWfcMusicArrangementConfig;
begin
  if FReleased then Exit;
  try
    LConfig := MusicStudioArrangementConfig(Trim(FDuration.value), FStudio.Seed);
    ExpectedFrames(LConfig);
    if not TryParseSessionSeed(FSeed.value, LSeed) or (LSeed <> FStudio.Seed) then
      raise EMusicStudio.Create('start a new session for the edited seed first');
    FPlan.textContent := 'Next composition: requested ' + Trim(FDuration.value) +
      ' seconds → actual ' + IntToStr(LConfig.RequestedTicks div TICKS_PER_SECOND) +
      ' seconds (' + IntToStr(LConfig.RequestedTicks div 1920) +
      ' complete 4/4 bars). Session seed ' + IntToStr(LSeed) +
      '; opening locks will be copied when you begin.';
    FPlan.setAttribute('data-valid', 'true');
    FFallback.textContent := 'Native FPC fallback (new output path): ' +
      'MusicStudioRender --seconds ' + Trim(FDuration.value) + ' --seed ' +
      IntToStr(FStudio.Seed) + ' --output NEW.wav' + #10 +
      'This command uses the session seed; browser opening locks are not transferred.';
  except
    FPlan.textContent := 'Next composition cannot start: ' + BrowserFailureText(JSExceptValue) + '.';
    FPlan.setAttribute('data-valid', 'false');
    FFallback.textContent := 'Choose valid seconds and an active session seed to ' +
      'show the native MusicStudioRender command.';
  end;
end;

procedure TBrowserMusicArrangement.Cancel(const AReason: String);
begin
  if (FActive = nil) or FActive.Committing then Exit;
  FActive.Cancelled := True;
  FCancel.disabled := True;
  SetStatus('cancelling', AReason + ' Waiting for the current section/write to settle.');
end;

function TBrowserMusicArrangement.HandleStart(AEvent: TJSMouseEvent): Boolean;
begin
  Result := False;
  if not FBusy then SaveComposition;
end;

function TBrowserMusicArrangement.HandleCancel(AEvent: TJSMouseEvent): Boolean;
begin
  Result := False;
  Cancel('Cancellation requested.');
end;

function TBrowserMusicArrangement.HandleMutation(AEvent: TJSEvent): Boolean;
var LId: String;
begin
  Result := True;
  if FReleased then Exit;
  if not (AEvent.target is TJSElement) then Exit;
  LId := TJSElement(AEvent.target).id;
  if (LId = 'arrangement-start') or (LId = 'arrangement-cancel') then Exit;
  if (AEvent._type = 'click') and
    (LId <> 'new-session-button') and (LId <> 'generate-button') and
    (LId <> 'motif-button') and (LId <> 'add-lock-button') and
    (LId <> 'remove-lock-button') and (LId <> 'clear-locks-button') then Exit;
  if (AEvent._type <> 'click') and
    (LId <> 'seed-input') and (LId <> 'arrangement-seconds') and
    (LId <> 'strategy-select') and (LId <> 'scope-select') and
    (LId <> 'backtracks-input') and (LId <> 'pass-backtracks-input') and
    (LId <> 'trace-input') then Exit;
  if (FActive <> nil) and FActive.Committing then
  begin
    AEvent.preventDefault;
    AEvent.stopImmediatePropagation;
    Exit(False);
  end;
  Cancel('Inputs changed; captured composition invalidated.');
  { The main session handler may run after this capture-phase listener. }
  if FRefreshTimer <> 0 then window.clearTimeout(FRefreshTimer);
  FRefreshTimer := window.setTimeout(
    procedure
    begin
      FRefreshTimer := 0;
      RefreshFallback;
      if not FBusy and not FReleased then
      begin
        FProgress.value := 0;
        FPitches.textContent := 'The next generated section will show its pitches here.';
        FDetail.textContent := 'The plan above reflects the current inputs. No new save has started.';
        if FPlan.getAttribute('data-valid') <> 'true' then
          SetStatus('invalid', 'Correct the plan above before starting a save.')
        else if isFunction(TJSObject(window)['showSaveFilePicker']) then
          SetStatus('ready', 'Plan updated. Choose a new file when you are ready.')
        else
          SetStatus('unavailable', 'Plan updated. Use the native FPC command to save long output.');
      end;
    end, 0);
end;

procedure TBrowserMusicArrangement.SaveComposition(
  const ATestPicker: TJSPromise); async;
var
  LOperation: TBrowserMusicArrangementOperation;
  LSource: TMusicStudioSectionSource;
  LArrangement: TWfcMusicArrangement;
  LWriter: TWfcMusicWaveStream;
  LSink: TSectionByteSink;
  LSection: TWfcMusicArrangementSection;
  LFile: TMusicFileHandle;
  LWritable: TMusicWritableFile;
  LBlob: TJSBlob;
  LOptions, LType, LAccept: TJSObject;
  LConfig: TWfcMusicArrangementConfig;
  LSeed: TGraphSeed;
  LLocks: TWfcMusicStudioLocks;
  LSeconds, LFailure: String;
  LFrames: TWfcMusicAudioStreamCount;
  LControls: TControlStates;
  LCommitted, LCancelled: Boolean;
begin
  if FReleased or FBusy then Exit;
  Inc(FAsyncCount);
  LOperation := nil;
  LSource := nil;
  LArrangement := nil;
  LWriter := nil;
  LSink := nil;
  LWritable := nil;
  LCommitted := False;
  LControls := nil;
  try
    try
      LSeconds := Trim(FDuration.value);
      if not TryParseSessionSeed(FSeed.value, LSeed) or (LSeed <> FStudio.Seed) then
        raise EMusicStudio.Create('seed differs from this session; start a new session first');
      LConfig := MusicStudioArrangementConfig(LSeconds, LSeed);
      LFrames := ExpectedFrames(LConfig);
      LLocks := FStudio.CopyLocks;
      RefreshFallback;
      if (ATestPicker = nil) and
        not isFunction(TJSObject(window)['showSaveFilePicker']) then
      begin
        SetStatus('unavailable', 'This browser cannot stream-save a chosen file. Use the native FPC command below.');
        Exit;
      end;
      LOperation := TBrowserMusicArrangementOperation.Create;
      FActive := LOperation;
      SetBusy(True);
      FProgress.value := 0;
      document.body.setAttribute('data-arrangement-frames', '0');
      document.body.removeAttribute('data-arrangement-peak-bytes');
      FDetail.textContent := 'Captured seed ' + IntToStr(LSeed) + ', ' +
        IntToStr(Length(LLocks)) + ' opening locks; requested ' + LSeconds +
        ' s, actual ' + IntToStr(LConfig.RequestedTicks div TICKS_PER_SECOND) +
        ' s (complete 4/4 bars at 120 BPM). Locks apply only to section 1.';
      FPitches.textContent := 'Waiting for a save location.';
      SetStatus('choosing', 'Choose a new WAV file. No composition audio has been generated yet.');
      LOptions := TJSObject.new;
      LOptions['suggestedName'] := 'wfc-composition-seed-' + IntToStr(LSeed) + '.wav';
      LType := TJSObject.new;
      LType['description'] := 'PCM16 WAVE / RF64 audio';
      LAccept := TJSObject.new;
      LAccept['audio/wav'] := TJSArray.new('.wav');
      LType['accept'] := LAccept;
      LOptions['types'] := TJSArray.new(LType);
      { Must be called directly in the user's click turn, before the first await. }
      if ATestPicker <> nil then
        LFile := await(TMusicFileHandle, ATestPicker)
      else
        LFile := await(TMusicFileHandle,
          TMusicSaveWindow(window).showSaveFilePicker(LOptions));
      CheckOperation(FActive, LOperation);
      LWritable := await(TMusicWritableFile, LFile.createWritable);
      CheckOperation(FActive, LOperation);
      LSource := TMusicStudioSectionSource.Create(LLocks);
      LArrangement := TWfcMusicArrangement.Create(LConfig, LSource);
      LSink := TSectionByteSink.Create(False);
      LWriter := TWfcMusicWaveStream.Create(LSink, SAMPLE_RATE, LFrames);
      while True do
      begin
        await(BrowserYield);
        CheckOperation(FActive, LOperation);
        if LArrangement.Next(LSection) <> wmaspProduced then
        begin
          if LArrangement.Status <> wmasCompleted then
            raise EMusicStudio.Create(LArrangement.Failure);
          Break;
        end;
        try
          SetStatus('generating', 'Rendering section ' + IntToStr(LSection.Index + 1) +
            ' of ' + IntToStr(LArrangement.SectionCount) + '.');
          FPitches.textContent := SectionPitches(LSection);
          AppendSection(LSection, LWriter);
        finally
          LSection.Free;
        end;
        CheckOperation(FActive, LOperation);
        LBlob := LSink.DetachBlob;
        SetStatus('writing', 'Writing section ' + IntToStr(LArrangement.NextIndex) +
          ' of ' + IntToStr(LArrangement.SectionCount) + '…');
        await(LWritable.write(LBlob));
        LBlob := nil;
        CheckOperation(FActive, LOperation);
        FProgress.value := LWriter.FrameCount / LFrames;
        document.body.setAttribute('data-arrangement-frames', IntToStr(LWriter.FrameCount));
        FDetail.textContent := 'Seed ' + IntToStr(LSeed) + ' · requested ' +
          LSeconds + ' s · written ' + IntToStr(LWriter.FrameCount div SAMPLE_RATE) +
          ' / ' + IntToStr(LFrames div SAMPLE_RATE) + ' actual seconds · ' +
          IntToStr(Length(LLocks)) + ' captured opening locks · at most one section buffered.';
      end;
      CheckOperation(FActive, LOperation);
      LWriter.Finish;
      { close is the irreversible commit boundary. Do not advertise abort after
        it starts. Preserve disabled controls rather than enabling them blindly. }
      LOperation.Committing := True;
      FreezeControls(LControls);
      SetStatus('committing', 'Committing the captured composition; editing is briefly locked.');
      await(LWritable.close);
      LCommitted := True;
      FProgress.value := 1;
      SetStatus('saved', 'Saved the full ' + IntToStr(LFrames div SAMPLE_RATE) +
        '-second composition (requested ' + LSeconds + ' seconds).');
      document.body.setAttribute('data-arrangement-peak-bytes', IntToStr(LSink.Peak));
    except
      LFailure := BrowserFailureText(JSExceptValue);
      LCancelled := ((LOperation <> nil) and LOperation.Cancelled) or
        (BrowserFailureName(JSExceptValue) = 'AbortError');
      if (LWritable <> nil) and not LCommitted then
      begin
        try
          await(LWritable.abort);
        except
          LFailure := LFailure + ' Abort also failed: ' + BrowserFailureText(JSExceptValue);
          LCancelled := False;
        end;
      end;
      if LCancelled then
        SetStatus('cancelled', 'Save cancelled; no completed composition was committed.')
      else
        SetStatus('failed', 'Save failed: ' + LFailure + '. No completed save is claimed.');
    end;
  finally
    RestoreControls(LControls);
    if FActive = LOperation then FActive := nil;
    LWriter.Free;
    LSink.Free;
    LArrangement.Free;
    LSource.Free;
    LOperation.Free;
    SetBusy(False);
    AsyncFinished;
  end;
end;

procedure ArrangementAssert(const ACondition: Boolean; const AMessage: String);
begin
  if not ACondition then raise EMusicStudio.Create('arrangement self-test: ' + AMessage);
end;

procedure TBrowserMusicArrangement.CheckSaveFixture(const AKind: String); async;
var
  LFile, LWritable: TJSObject;
  LPicker: TJSPromise;
  LOriginalDuration: String;
  LWriteCalls, LOpenCalls, LCloseCalls, LAbortCalls: Integer;
  LWritten: NativeInt;
  LInFlight: Boolean;
begin
  { A private fake backend exercises the real async controller and actual
    bounded Blob blocks without a disk picker, file, or whole-song buffer. }
  LWriteCalls := 0;
  LOpenCalls := 0;
  LCloseCalls := 0;
  LAbortCalls := 0;
  LWritten := 0;
  LInFlight := False;
  LWritable := TJSObject.new;
  LWritable['write'] := function(AData: JSValue): JSValue
    begin
      ArrangementAssert(not LInFlight, 'writes never overlap or queue unbounded sections');
      ArrangementAssert(TJSBlob(AData).size <= MAX_SECTION_BYTES, 'actual Blob section bound');
      LInFlight := True;
      Inc(LWriteCalls);
      Inc(LWritten, TJSBlob(AData).size);
      if AKind = 'cancel' then FCancel.click;
      if AKind = 'edit' then FDuration.dispatchEvent(TJSEvent.new('input'));
      Result := BrowserYield._then(function(AValue: JSValue): JSValue
        begin
          LInFlight := False;
          if AKind = 'write-failure' then
            Result := TJSPromise.reject('self-test write failure')
          else Result := Null;
        end);
    end;
  LWritable['close'] := function: JSValue
    begin
      ArrangementAssert(not LInFlight, 'commit waits for final write');
      ArrangementAssert(FSeed.disabled and FDuration.disabled and FCancel.disabled,
        'commit disables generation changes and cancellation');
      Inc(LCloseCalls);
      if AKind = 'close-failure' then
        Result := TJSPromise.reject('self-test close failure')
      else Result := BrowserYield;
    end;
  LWritable['abort'] := function: JSValue
    begin
      ArrangementAssert(not LInFlight, 'abort waits for outstanding write');
      Inc(LAbortCalls);
      Result := BrowserYield;
    end;
  LFile := TJSObject.new;
  LFile['createWritable'] := function: JSValue
    begin Inc(LOpenCalls); Result := TJSPromise.resolve(LWritable); end;
  LPicker := TJSPromise.resolve(LFile);
  if AKind = 'stale' then
    LPicker := LPicker._then(function(AValue: JSValue): JSValue
      begin FActive := nil; Result := AValue; end);
  LOriginalDuration := FDuration.value;
  try
    FDuration.value := '6';
    SetBusy(False);
    await(SaveComposition(LPicker));
    if AKind = 'saved' then
    begin
      ArrangementAssert((LOpenCalls = 1) and (LWriteCalls = 2) and
        (LWritten = 44 + 6 * SAMPLE_RATE * 2) and (LCloseCalls = 1) and
        (LAbortCalls = 0), 'complete two-section async save commits once');
      ArrangementAssert(document.body.getAttribute('data-arrangement-state') = 'saved',
        'saved state requires a successful close');
      ArrangementAssert(not FSeed.disabled and not FDuration.disabled,
        'commit restores previously enabled controls');
    end
    else if (AKind = 'cancel') or (AKind = 'edit') then
    begin
      ArrangementAssert((LOpenCalls = 1) and (LWriteCalls = 1) and
        (LCloseCalls = 0) and (LAbortCalls = 1),
        'cancel aborts after the pending first write without committing');
      ArrangementAssert(document.body.getAttribute('data-arrangement-state') = 'cancelled',
        'cancelled state does not claim completion');
    end
    else if (AKind = 'write-failure') or (AKind = 'close-failure') then
    begin
      ArrangementAssert((LOpenCalls = 1) and (LAbortCalls = 1),
        'failed browser promise aborts the transaction');
      if AKind = 'write-failure' then
        ArrangementAssert((LWriteCalls = 1) and (LCloseCalls = 0),
          'write failure prevents later sections and commit')
      else
        ArrangementAssert((LWriteCalls = 2) and (LCloseCalls = 1),
          'close failure is not retried');
      ArrangementAssert(document.body.getAttribute('data-arrangement-state') = 'failed',
        'browser failure cannot claim a completed save');
      ArrangementAssert(not FSeed.disabled and not FDuration.disabled,
        'failed commit also restores controls');
    end
    else
    begin
      ArrangementAssert((LOpenCalls = 0) and (LWriteCalls = 0) and
        (LCloseCalls = 0) and (LAbortCalls = 0),
        'stale picker completion cannot open or write a destination');
      ArrangementAssert(document.body.getAttribute('data-arrangement-state') = 'failed',
        'stale result cannot publish saved state');
    end;
    ArrangementAssert(not FBusy and (FActive = nil), 'async operation releases its active state');
  finally
    FDuration.value := LOriginalDuration;
    SetBusy(True);
    RefreshFallback;
  end;
end;

function CheckArrangementFixture(const ASeconds: String;
  const AExpectedSections, AFinalCells: Integer): String; async;
var
  LSource: TMusicStudioSectionSource;
  LArrangement: TWfcMusicArrangement;
  LSection: TWfcMusicArrangementSection;
  LSink: TSectionByteSink;
  LWriter: TWfcMusicWaveStream;
  LConfig: TWfcMusicArrangementConfig;
  LCount, LLastCells: Integer;
  LFrames: TWfcMusicAudioStreamCount;
begin
  Result := '';
  LConfig := MusicStudioArrangementConfig(ASeconds, 0);
  LFrames := ExpectedFrames(LConfig);
  LSource := TMusicStudioSectionSource.Create(nil);
  LArrangement := nil;
  LSink := nil;
  LWriter := nil;
  try
    LArrangement := TWfcMusicArrangement.Create(LConfig, LSource);
    LSink := TSectionByteSink.Create(True);
    LWriter := TWfcMusicWaveStream.Create(LSink, SAMPLE_RATE, LFrames);
    LCount := 0;
    LLastCells := 0;
    while LArrangement.Next(LSection) = wmaspProduced do
    begin
      try
        Inc(LCount);
        LLastCells := LSection.CellCount;
        if LCount = 1 then
          Result := IntToHex(LSection.Composition.Signature, 8);
        AppendSection(LSection, LWriter);
      finally
        LSection.Free;
      end;
      ArrangementAssert(LSink.Buffered <= MAX_SECTION_BYTES, 'one-section sink bound');
      LSink.DiscardSection;
      await(BrowserYield);
    end;
    ArrangementAssert(LArrangement.Status = wmasCompleted, 'all sections complete');
    LWriter.Finish;
    ArrangementAssert(LWriter.Finished and (LWriter.FrameCount = LFrames),
      'exact frames for ' + ASeconds + ' seconds');
    ArrangementAssert(LSink.Total = 44 + LFrames * 2, 'exact file byte count');
    ArrangementAssert((LCount = AExpectedSections) and (LLastCells = AFinalCells),
      'section count and final partial phrase');
    document.body.setAttribute('data-arrangement-' + ASeconds + '-frames', IntToStr(LFrames));
  finally
    LWriter.Free;
    LSink.Free;
    LArrangement.Free;
    LSource.Free;
  end;
end;

procedure TBrowserMusicArrangement.RunSelfTest; async;
var
  LFirst, LReplay: String;
  LA, LB: TBrowserMusicArrangementOperation;
  LSource: TMusicStudioSectionSource;
  LArrangement: TWfcMusicArrangement;
  LSection: TWfcMusicArrangementSection;
  LConfig: TWfcMusicArrangementConfig;
begin
  if FReleased then Exit;
  Inc(FAsyncCount);
  try
  document.body.setAttribute('data-arrangement-test', 'pending');
  SetBusy(True);
  try
    SetStatus('testing', 'Checking 4-, 6-, and 180-second arrangements without opening or writing files.');
    LFirst := await(CheckArrangementFixture('4', 1, 16));
    LReplay := await(CheckArrangementFixture('4', 1, 16));
    ArrangementAssert(LFirst = LReplay, 'fixed-seed deterministic replay');
    await(CheckArrangementFixture('6', 2, 8));
    await(CheckArrangementFixture('180', 45, 16));
    await(CheckSaveFixture('saved'));
    await(CheckSaveFixture('cancel'));
    await(CheckSaveFixture('edit'));
    await(CheckSaveFixture('write-failure'));
    await(CheckSaveFixture('close-failure'));
    await(CheckSaveFixture('stale'));
    LConfig := MusicStudioArrangementConfig('4.001', 0);
    ArrangementAssert(ExpectedFrames(LConfig) = 6 * SAMPLE_RATE,
      'fractional request rounds to a complete bar');
    LSource := TMusicStudioSectionSource.Create(nil);
    LArrangement := nil;
    try
      LArrangement := TWfcMusicArrangement.Create(
        MusicStudioArrangementConfig('180', 0), LSource);
      ArrangementAssert(LArrangement.Next(LSection) = wmaspProduced, 'cancel fixture first section');
      LSection.Free;
      LArrangement.Cancel;
      ArrangementAssert((LArrangement.Next(LSection) = wmaspCancelled) and
        (LSection = nil) and (LArrangement.ProducedTicks = 3840),
        'cancellation produces no later section');
    finally
      LArrangement.Free;
      LSource.Free;
    end;
    LA := TBrowserMusicArrangementOperation.Create;
    LB := TBrowserMusicArrangementOperation.Create;
    try
      ArrangementAssert(OperationCurrent(LA, LA), 'current operation guard');
      await(BrowserYield);
      ArrangementAssert(not OperationCurrent(LB, LA), 'stale async operation is rejected');
      LA.Cancelled := True;
      ArrangementAssert(not OperationCurrent(LA, LA), 'cancelled operation is rejected');
    finally
      LB.Free;
      LA.Free;
    end;
    document.body.setAttribute('data-arrangement-cancel', 'passed');
    document.body.setAttribute('data-arrangement-stale', 'passed');
    document.body.setAttribute('data-arrangement-write-failure', 'passed');
    document.body.setAttribute('data-arrangement-commit', 'passed');
    document.body.setAttribute('data-arrangement-test', 'passed');
    FProgress.value := 0;
    document.body.setAttribute('data-arrangement-frames', '0');
    document.body.removeAttribute('data-arrangement-peak-bytes');
    FDetail.textContent := 'Checks rendered 4-, 6-, and 180-second arrangements and exercised save, cancel, edit, failure, and stale-result paths without touching disk.';
    FPitches.textContent := 'No user composition has been saved by these checks.';
    if isFunction(TJSObject(window)['showSaveFilePicker']) then
      SetStatus('ready', 'Long-composition checks passed. Choose any valid duration to stream-save a WAV.')
    else
      SetStatus('unavailable', 'Long-composition checks passed. This browser needs the native FPC command to save long output.');
  except
    document.body.setAttribute('data-arrangement-test', 'failed');
    document.body.setAttribute('data-arrangement-test-message', BrowserFailureText(JSExceptValue));
    SetStatus('failed', BrowserFailureText(JSExceptValue));
  end;
  SetBusy(False);
  finally
    AsyncFinished;
  end;
end;

procedure TBrowserMusicArrangement.Run;
begin
  if FReleased or FBound then Exit;
  FDuration := TJSHTMLInputElement(RequireElement('arrangement-seconds'));
  FSeed := TJSHTMLInputElement(RequireElement('seed-input'));
  FStart := TJSHTMLButtonElement(RequireElement('arrangement-start'));
  FCancel := TJSHTMLButtonElement(RequireElement('arrangement-cancel'));
  FProgress := TJSHTMLProgressElement(RequireElement('arrangement-progress'));
  FStatus := RequireElement('arrangement-status');
  FDetail := RequireElement('arrangement-detail');
  FPlan := RequireElement('arrangement-plan');
  FPitches := RequireElement('arrangement-pitches');
  FFallback := RequireElement('arrangement-fallback');
  FStart.onclick := @HandleStart;
  FCancel.onclick := @HandleCancel;
  document.addEventListener('input', @HandleMutation, True);
  document.addEventListener('change', @HandleMutation, True);
  document.addEventListener('click', @HandleMutation, True);
  FBound := True;
  SetBusy(False);
  RefreshFallback;
  if not isFunction(TJSObject(window)['showSaveFilePicker']) then
    SetStatus('unavailable', 'File streaming is unavailable in this browser; use the native FPC command below.')
  else
    SetStatus('ready', 'Ready to generate a full composition into a chosen file.');
  if Pos('selftest=1', window.location.search) > 0 then RunSelfTest;
end;

end.
