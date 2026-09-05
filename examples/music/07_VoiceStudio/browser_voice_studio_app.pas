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
unit browser_voice_studio_app;

{$mode delphi}{$H+}
{$modeswitch externalclass}

interface

uses
  JS,
  Web,
  SysUtils,
  wfc,
  wfc_midi_stream,
  wfc_midi_smf,
  wfc_music_audio,
  wfc_music_audio_stream,
  wfc_music_arrangement,
  voice_studio_stream,
  voice_studio_midi_stream,
  wfc_browser_stream_file;

type
  TVoiceStudioBrowserOperationKind = (vsbokWave, vsbokMidiPlan, vsbokMidiSave);

  TVoiceStudioBrowserOperation = class
  public
    Kind: TVoiceStudioBrowserOperationKind;
    FileTarget: TWfcBrowserStreamFile;
    Pcm: TVoiceStudioPcmStream;
    Planner: TVoiceStudioMidiPlanner;
    Midi: TVoiceStudioMidiStream;
    MidiPlan: TVoiceStudioMidiPlan;
    Cancelled, Committing: Boolean;
    destructor Destroy; override;
    procedure Cancel;
  end;

  TVoiceStudioBrowserApplication = class
  strict private
    FSeed, FSeconds, FSegmentCells, FBacktracks, FPassBacktracks:
      TJSHTMLInputElement;
    FTrace: TJSHTMLInputElement;
    FWaveButton, FMidiPlanButton, FMidiSaveButton,
      FCancelButton: TJSHTMLButtonElement;
    FProgress: TJSHTMLProgressElement;
    FStatus, FPlanText, FProgressText, FFallback, FCellPosition,
      FCellNovelty, FHarmony, FRhythm, FBass, FChord, FUpper,
      FCoverage, FMetrics: TJSElement;
    FActive: TVoiceStudioBrowserOperation;
    FMidiPlan: TVoiceStudioMidiPlan;
    FBusy, FReleased, FBound, FSelfTesting: Boolean;
    FAsyncCount: Integer;
    function RequireElement(const AId: String): TJSElement;
    function ReadSeed: TGraphSeed;
    function ReadNonnegative(const AInput: TJSHTMLInputElement;
      const AName: String): Integer;
    function ReadOptions: TVoiceStudioStreamOptions;
    procedure SetStatus(const AState, AText: String);
    procedure SetBusy(const AValue: Boolean);
    procedure SetProgress(const ACurrent, ATotal: TWfcMusicArrangementWide;
      const AText: String);
    procedure ClearMidiPlan;
    procedure ClearOutput;
    procedure RefreshPlan;
    procedure DisplayCell(const ACell: TVoiceStudioCell);
    procedure DisplayMetrics(const ACells, ASegments, ASeams,
      ANovel, AShared: TWfcMusicArrangementWide; const ABytes: NativeInt);
    procedure CancelActive(const AReason: String);
    procedure AsyncFinished;
    function HandleWave(AEvent: TJSMouseEvent): Boolean;
    function HandleMidiPlan(AEvent: TJSMouseEvent): Boolean;
    function HandleMidiSave(AEvent: TJSMouseEvent): Boolean;
    function HandleCancel(AEvent: TJSMouseEvent): Boolean;
    function HandleMutation(AEvent: TJSEvent): Boolean;
    procedure SaveWave(const ATestPicker: TJSPromise = nil); async;
    procedure PlanMidi; async;
    procedure SaveMidi(const ATestPicker: TJSPromise = nil); async;
    procedure CheckStreamFileFixtures; async;
    procedure CheckWaveFixture(const AKind: String); async;
    procedure CheckMidiFixtures; async;
    procedure RestoreReleasedControls;
  public
    constructor Create;
    procedure Run;
    procedure Release;
    procedure RunSelfTest; async;
  end;

procedure InstallVoiceStudioBrowserTestFixture;
function VoiceStudioBrowserSelfTestRequested: Boolean;

implementation

uses
  wfc_music_ensemble,
  wfc_music_sequence,
  voice_studio_corpus;

type
  TVoiceStudioEventDocument = class external name 'Document' (TJSObject)
    procedure removeEventListener(const AName: String;
      const AListener: TJSEventHandler; const AUseCapture: Boolean);
  end;

  TOneBlockWaveSink = class(TWfcMusicAudioByteSink)
  strict private
    FBytes: TWfcMusicAudioBytes;
    function GetSize: Integer;
  public
    procedure WriteBytes(const ABytes: array of Byte); override;
    function Detach: TWfcMusicAudioBytes;
    procedure Discard;
    property Size: Integer read GetSize;
  end;

function BrowserYield: TJSPromise;
begin
  Result := TJSPromise.new(
    procedure(AResolve, AReject: TJSPromiseResolver)
    begin
      window.setTimeout(procedure begin AResolve(Null); end, 0);
    end);
end;

function VoiceStudioBrowserSelfTestRequested: Boolean;
begin
  Result := Pos('selftest=1', window.location.search) > 0;
end;

procedure BrowserAssert(const ACondition: Boolean; const AMessage: String);
begin
  if not ACondition then
    raise EVoiceStudioStream.Create('browser self-test: ' + AMessage);
end;

procedure AppendBytes(var ADestination: TWfcMidiBytes;
  const ASource: TJSUint8Array); overload;
var
  I, LOldLength: Integer;
begin
  LOldLength := Length(ADestination);
  SetLength(ADestination, LOldLength + ASource.length);
  for I := 0 to ASource.length - 1 do
    ADestination[LOldLength + I] := ASource[I];
end;

procedure AppendBytes(var ADestination: TWfcMidiBytes;
  const ASource: array of Byte); overload;
var
  I, LOldLength: Integer;
begin
  LOldLength := Length(ADestination);
  SetLength(ADestination, LOldLength + Length(ASource));
  for I := 0 to High(ASource) do
    ADestination[LOldLength + I] := ASource[I];
end;

function BytesEqual(const ALeft, ARight: array of Byte): Boolean;
var I: Integer;
begin
  if Length(ALeft) <> Length(ARight) then Exit(False);
  for I := 0 to High(ALeft) do
    if ALeft[I] <> ARight[I] then Exit(False);
  Result := True;
end;

function CollectPlannedMidiBytes(const APlan: TVoiceStudioMidiPlan):
  TWfcMidiBytes;
var
  LBlock: TWfcMidiBytes;
  LStep: TWfcMusicArrangementStep;
  LStream: TVoiceStudioMidiStream;
begin
  Result := nil;
  LStream := TVoiceStudioMidiStream.Create(APlan);
  try
    repeat
      LStep := LStream.NextBytes(LBlock);
      case LStep of
        wmaspProduced: AppendBytes(Result, LBlock);
        wmaspCompleted: ;
        wmaspCancelled:
          raise EVoiceStudioStream.Create(
            'bounded MIDI fixture was unexpectedly cancelled');
        wmaspFailed: raise EVoiceStudioStream.Create(LStream.Failure);
      end;
    until LStep = wmaspCompleted;
    if (Length(Result) <> APlan.FileByteCount) or
        (LStream.TickCount <> APlan.EndTick) then
      raise EVoiceStudioStream.Create(
        'bounded MIDI fixture replay differs from its plan');
  finally
    LStream.Free;
  end;
end;

function FailureText(const AValue: JSValue): String;
begin
  Result := WfcBrowserStreamFailureText(AValue);
end;

function OperationCurrent(const AActive, ACandidate:
  TVoiceStudioBrowserOperation): Boolean;
begin
  Result := (AActive <> nil) and (AActive = ACandidate) and
    not ACandidate.Cancelled;
end;

function StreamOptionsEqual(const ALeft, ARight:
  TVoiceStudioStreamOptions): Boolean;
begin
  Result := (ALeft.Seed = ARight.Seed) and
    (ALeft.SegmentCellCount = ARight.SegmentCellCount) and
    (ALeft.MaxBacktracks = ARight.MaxBacktracks) and
    (ALeft.MaxPassBacktracks = ARight.MaxPassBacktracks) and
    (ALeft.CaptureTrace = ARight.CaptureTrace);
end;

function FramePlansEqual(const ALeft, ARight: TVoiceStudioFramePlan): Boolean;
begin
  Result := (ALeft.RequestedText = ARight.RequestedText) and
    (ALeft.RequestedTicks = ARight.RequestedTicks) and
    (ALeft.ActualTicks = ARight.ActualTicks) and
    (ALeft.CellCount = ARight.CellCount);
end;

destructor TVoiceStudioBrowserOperation.Destroy;
begin
  MidiPlan.Free;
  Midi.Free;
  Planner.Free;
  Pcm.Free;
  FileTarget.Free;
  inherited Destroy;
end;

procedure TVoiceStudioBrowserOperation.Cancel;
begin
  if Cancelled or Committing then Exit;
  Cancelled := True;
  if Pcm <> nil then Pcm.Cancel;
  if Planner <> nil then Planner.Cancel;
  if Midi <> nil then Midi.Cancel;
  if FileTarget <> nil then FileTarget.Cancel;
end;

procedure TOneBlockWaveSink.WriteBytes(const ABytes: array of Byte);
var I: Integer;
begin
  if Length(ABytes) < 1 then
    raise EVoiceStudioStream.Create('empty WAVE block');
  if Length(FBytes) <> 0 then
    raise EVoiceStudioStream.Create('WAVE block was not consumed');
  SetLength(FBytes, Length(ABytes));
  for I := 0 to High(ABytes) do FBytes[I] := ABytes[I];
end;

function TOneBlockWaveSink.GetSize: Integer;
begin
  Result := Length(FBytes);
end;

function TOneBlockWaveSink.Detach: TWfcMusicAudioBytes;
begin
  Result := FBytes;
  FBytes := nil;
end;

procedure TOneBlockWaveSink.Discard;
begin
  FBytes := nil;
end;

constructor TVoiceStudioBrowserApplication.Create;
begin
  inherited Create;
end;

function TVoiceStudioBrowserApplication.RequireElement(
  const AId: String): TJSElement;
begin
  Result := document.getElementById(AId);
  if Result = nil then
    raise EVoiceStudioStream.Create('Voice Studio UI is missing #' + AId);
end;

function TryReadUnsignedDecimal(const AText: String;
  const AMaximum: Cardinal; out AValue: Cardinal): Boolean;
var
  I: Integer;
  Digit: Cardinal;
begin
  Result := False;
  AValue := 0;
  if AText = '' then Exit;
  for I := 1 to Length(AText) do
  begin
    if not (AText[I] in ['0'..'9']) then Exit;
    Digit := Ord(AText[I]) - Ord('0');
    if AValue > (AMaximum - Digit) div 10 then Exit;
    AValue := AValue * 10 + Digit;
  end;
  Result := True;
end;

function TVoiceStudioBrowserApplication.ReadSeed: TGraphSeed;
var Value: Cardinal;
begin
  if not TryReadUnsignedDecimal(Trim(FSeed.value), High(Cardinal), Value) then
    raise EVoiceStudioStream.Create(
      'seed must be unsigned decimal from 0 through 4294967295');
  Result := TGraphSeed(Value);
end;

function TVoiceStudioBrowserApplication.ReadNonnegative(
  const AInput: TJSHTMLInputElement; const AName: String): Integer;
var Value: Cardinal;
begin
  if not TryReadUnsignedDecimal(Trim(AInput.value), High(Integer), Value) then
    raise EVoiceStudioStream.Create(AName +
      ' must be a whole decimal integer from 0 through ' +
      IntToStr(High(Integer)));
  Result := Integer(Value);
end;

function TVoiceStudioBrowserApplication.ReadOptions:
  TVoiceStudioStreamOptions;
begin
  Result := DefaultVoiceStudioStreamOptions;
  Result.Seed := ReadSeed;
  Result.SegmentCellCount := ReadNonnegative(FSegmentCells,
    'segment cell count');
  if (Result.SegmentCellCount < 1) or
      (Result.SegmentCellCount > High(Integer) div VOICE_STUDIO_QUANTUM) then
    raise EVoiceStudioStream.Create('segment cell count must be from 1 through ' +
      IntToStr(High(Integer) div VOICE_STUDIO_QUANTUM));
  Result.MaxBacktracks := ReadNonnegative(FBacktracks,
    'local search allowance');
  Result.MaxPassBacktracks := ReadNonnegative(FPassBacktracks,
    'pass search allowance');
  Result.CaptureTrace := FTrace.checked;
end;

procedure TVoiceStudioBrowserApplication.SetStatus(
  const AState, AText: String);
begin
  if FReleased then Exit;
  document.body.setAttribute('data-voice-state', AState);
  FStatus.textContent := AText;
end;

procedure TVoiceStudioBrowserApplication.SetBusy(const AValue: Boolean);
begin
  FBusy := AValue;
  if FReleased then Exit;
  FSeed.disabled := AValue;
  FSeconds.disabled := AValue;
  FSegmentCells.disabled := AValue;
  FBacktracks.disabled := AValue;
  FPassBacktracks.disabled := AValue;
  FTrace.disabled := AValue;
  FWaveButton.disabled := AValue or not WfcBrowserStreamFileAvailable;
  FMidiPlanButton.disabled := AValue;
  FMidiSaveButton.disabled := AValue or (FMidiPlan = nil) or
    not WfcBrowserStreamFileAvailable;
  FCancelButton.disabled := not AValue or (FActive = nil) or
    ((FActive <> nil) and FActive.Committing);
end;

procedure TVoiceStudioBrowserApplication.RestoreReleasedControls;
begin
  { Release can happen while an irreversible close promise is pending. These
    are the exact elements captured by this controller, so restoring their
    enabled state is cleanup rather than publication into replacement DOM. }
  if FSeed <> nil then FSeed.disabled := False;
  if FSeconds <> nil then FSeconds.disabled := False;
  if FSegmentCells <> nil then FSegmentCells.disabled := False;
  if FBacktracks <> nil then FBacktracks.disabled := False;
  if FPassBacktracks <> nil then FPassBacktracks.disabled := False;
  if FTrace <> nil then FTrace.disabled := False;
  if FWaveButton <> nil then FWaveButton.disabled := True;
  if FMidiPlanButton <> nil then FMidiPlanButton.disabled := True;
  if FMidiSaveButton <> nil then FMidiSaveButton.disabled := True;
  if FCancelButton <> nil then FCancelButton.disabled := True;
end;

procedure TVoiceStudioBrowserApplication.SetProgress(
  const ACurrent, ATotal: TWfcMusicArrangementWide; const AText: String);
begin
  if FReleased then Exit;
  FProgress.max := ATotal;
  FProgress.value := ACurrent;
  FProgressText.textContent := AText;
end;

procedure TVoiceStudioBrowserApplication.ClearMidiPlan;
begin
  FreeAndNil(FMidiPlan);
  if FReleased then Exit;
  document.body.setAttribute('data-voice-midi-plan', 'none');
  document.body.removeAttribute('data-voice-midi-bytes');
  document.body.removeAttribute('data-voice-midi-signature');
  FMidiSaveButton.disabled := True;
end;

procedure TVoiceStudioBrowserApplication.ClearOutput;
begin
  if FReleased then Exit;
  FCellPosition.textContent := 'No generated cell yet.';
  FCellNovelty.textContent := 'Waiting';
  FHarmony.textContent := '—';
  FRhythm.textContent := '—';
  FBass.textContent := '—';
  FChord.textContent := '—';
  FUpper.textContent := '—';
  FCoverage.textContent := '—';
  FMetrics.textContent := 'No transaction has generated output.';
  SetProgress(0, 1, 'Idle');
end;

function TraceOption(const AEnabled: Boolean): String;
begin
  if AEnabled then Result := ' --trace' else Result := '';
end;

procedure TVoiceStudioBrowserApplication.RefreshPlan;
var
  FramePlan: TVoiceStudioFramePlan;
  Options: TVoiceStudioStreamOptions;
  WavePlan: TVoiceStudioWavePlan;
  WaveText: String;
begin
  if FReleased then Exit;
  try
    Options := ReadOptions;
    FramePlan := PlanVoiceStudioFrames(FSeconds.value);
    FPlanText.textContent := 'Requested ' + FramePlan.RequestedText +
      ' s; actual ' + VoiceStudioSecondsText(FramePlan.ActualTicks) +
      ' s after upward 0.25-second cell rounding (' +
      IntToStr(FramePlan.CellCount) + ' cells). Local working segment: ' +
      IntToStr(Options.SegmentCellCount) + ' cells.';
    WaveText := '';
    try
      WavePlan := PlanVoiceStudioWave(FSeconds.value);
      WaveText := 'Native WAVE: VoiceStudioRender --format wave --seconds ' +
        WavePlan.RequestedText + ' --output NEW.wav';
    except
      WaveText := 'Native WAVE unavailable for this transport extent: ' +
        FailureText(JSExceptValue);
    end;
    if Pos('Native WAVE:', WaveText) = 1 then
      WaveText := WaveText + ' --seed ' + IntToStr(Options.Seed) +
        ' --segment-cells ' + IntToStr(Options.SegmentCellCount) +
        ' --backtracks ' + IntToStr(Options.MaxBacktracks) +
        ' --pass-backtracks ' + IntToStr(Options.MaxPassBacktracks) +
        TraceOption(Options.CaptureTrace);
    FFallback.textContent := WaveText + #10 +
      'Native MIDI: VoiceStudioRender --format midi --seconds ' +
      FramePlan.RequestedText + ' --output NEW.mid --seed ' +
      IntToStr(Options.Seed) + ' --segment-cells ' +
      IntToStr(Options.SegmentCellCount) + ' --backtracks ' +
      IntToStr(Options.MaxBacktracks) + ' --pass-backtracks ' +
      IntToStr(Options.MaxPassBacktracks) + TraceOption(Options.CaptureTrace);
    FPlanText.setAttribute('data-valid', 'true');
  except
    FPlanText.textContent := 'Cannot plan: ' + FailureText(JSExceptValue);
    FPlanText.setAttribute('data-valid', 'false');
    FFallback.textContent := 'Correct the inputs to show native commands.';
  end;
end;

function PitchName(const APitch: Integer): String;
const Names: array[0..11] of String =
  ('C', 'C#', 'D', 'D#', 'E', 'F', 'F#', 'G', 'G#', 'A', 'A#', 'B');
begin
  Result := Names[APitch mod 12] + IntToStr(APitch div 12 - 1);
end;

function VoiceText(const AVoice: TWfcMusicVoiceCell): String;
var I: Integer;
begin
  case AVoice.Action of
    wmcaRest: Exit('rest');
    wmcaAttack: Result := 'attack ';
    wmcaHold: Result := 'hold ';
  end;
  for I := 0 to High(AVoice.Tones) do
  begin
    if I > 0 then Result := Result + ' + ';
    Result := Result + PitchName(AVoice.Tones[I].Pitch);
  end;
end;

function RhythmText(const AToken: String): String;
var
  Frame: TWfcMusicRhythmFrame;
  I: Integer;
begin
  Frame := DecodeWfcMusicRhythmFrame(AToken);
  Result := '';
  for I := 0 to High(Frame.Actions) do
  begin
    if I > 0 then Result := Result + ' / ';
    case Frame.Actions[I] of
      wmcaRest: Result := Result + 'rest';
      wmcaAttack: Result := Result + 'attack';
      wmcaHold: Result := Result + 'hold';
    end;
  end;
end;

function CoverageText(const ASuppliers: TVoiceStudioCoverageSuppliers): String;
const Names: array[0..11] of String =
  ('C', 'C#', 'D', 'D#', 'E', 'F', 'F#', 'G', 'G#', 'A', 'A#', 'B');
var I: Integer;
begin
  Result := '';
  for I := 0 to High(ASuppliers) do
    if ASuppliers[I] >= 0 then
    begin
      if Result <> '' then Result := Result + ', ';
      Result := Result + Names[I] + '←lowest matching ' +
        VoiceStudioRoleName(ASuppliers[I]);
    end;
  if Result = '' then Result := 'no sounded harmony classes';
end;

procedure TVoiceStudioBrowserApplication.DisplayCell(
  const ACell: TVoiceStudioCell);
begin
  if FReleased then Exit;
  FCellPosition.textContent := 'Cell ' + IntToStr(ACell.Position) +
    '; segment ' + IntToStr(ACell.SegmentIndex) + ', local ' +
    IntToStr(ACell.SegmentOffset) + '.';
  if ACell.ObservedVertical then
  begin
    FCellNovelty.textContent := 'Observed vertical';
    FCellNovelty.setAttribute('data-novel', 'false');
  end
  else
  begin
    FCellNovelty.textContent := 'Novel recombination';
    FCellNovelty.setAttribute('data-novel', 'true');
  end;
  FHarmony.textContent := ACell.HarmonyToken;
  FRhythm.textContent := RhythmText(ACell.RhythmToken);
  FBass.textContent := VoiceText(ACell.Frame.Voices[0]);
  FChord.textContent := VoiceText(ACell.Frame.Voices[1]);
  FUpper.textContent := VoiceText(ACell.Frame.Voices[2]);
  FCoverage.textContent := CoverageText(ACell.CoverageSuppliers);
end;

procedure TVoiceStudioBrowserApplication.DisplayMetrics(
  const ACells, ASegments, ASeams, ANovel, AShared:
    TWfcMusicArrangementWide; const ABytes: NativeInt);
begin
  if FReleased then Exit;
  FMetrics.textContent := IntToStr(ACells) + ' cells; ' +
    IntToStr(ASegments) + ' segments; ' + IntToStr(ASeams) +
    ' held voice seams; ' + IntToStr(ANovel) +
    ' novel verticals; ' + IntToStr(AShared) +
    ' cells whose harmony witness used multiple roles; ' +
    IntToStr(ABytes) + ' bytes written.';
end;

procedure TVoiceStudioBrowserApplication.CancelActive(const AReason: String);
begin
  if (FActive = nil) or FActive.Committing then Exit;
  FActive.Cancel;
  if not FReleased then
  begin
    FCancelButton.disabled := True;
    SetStatus('cancelling', AReason +
      ' Waiting for the bounded active step to settle.');
  end;
end;

procedure TVoiceStudioBrowserApplication.AsyncFinished;
begin
  Dec(FAsyncCount);
  if FReleased and (FAsyncCount = 0) then Free;
end;

function TVoiceStudioBrowserApplication.HandleWave(
  AEvent: TJSMouseEvent): Boolean;
begin
  Result := False;
  if not FBusy then SaveWave;
end;

function TVoiceStudioBrowserApplication.HandleMidiPlan(
  AEvent: TJSMouseEvent): Boolean;
begin
  Result := False;
  if not FBusy then PlanMidi;
end;

function TVoiceStudioBrowserApplication.HandleMidiSave(
  AEvent: TJSMouseEvent): Boolean;
begin
  Result := False;
  if not FBusy and (FMidiPlan <> nil) then SaveMidi;
end;

function TVoiceStudioBrowserApplication.HandleCancel(
  AEvent: TJSMouseEvent): Boolean;
begin
  Result := False;
  CancelActive('Cancellation requested.');
end;

function TVoiceStudioBrowserApplication.HandleMutation(
  AEvent: TJSEvent): Boolean;
var Id: String;
begin
  Result := True;
  if FReleased or not (AEvent.target is TJSElement) then Exit;
  Id := TJSElement(AEvent.target).id;
  if (Id <> 'voice-seed') and (Id <> 'voice-seconds') and
      (Id <> 'voice-segment') and (Id <> 'voice-backtracks') and
      (Id <> 'voice-pass-backtracks') and (Id <> 'voice-trace') then Exit;
  if (FActive <> nil) and FActive.Committing then
  begin
    AEvent.preventDefault;
    AEvent.stopImmediatePropagation;
    Exit(False);
  end;
  ClearMidiPlan;
  CancelActive('Inputs changed; the captured transaction is stale.');
  RefreshPlan;
  if not FBusy then
  begin
    ClearOutput;
    if FPlanText.getAttribute('data-valid') = 'true' then
      SetStatus('ready', 'Plan updated; no stale output remains.')
    else SetStatus('invalid', 'Correct the plan before exporting.');
  end;
end;

procedure SafeAbort(const AFile: TWfcBrowserStreamFile); async;
begin
  if (AFile = nil) or (AFile.State = wbsfsCommitted) then Exit;
  try
    await(AFile.Abort);
  except
    { Publication already failed/cancelled. Cleanup diagnostics are secondary
      to the original user-facing failure and no file is claimed complete. }
  end;
end;

procedure TVoiceStudioBrowserApplication.SaveWave(
  const ATestPicker: TJSPromise); async;
var
  Block: TWfcMusicAudioBytes;
  Cell: TVoiceStudioCell;
  Op: TVoiceStudioBrowserOperation;
  Options: TVoiceStudioStreamOptions;
  Picker: TJSPromise;
  Plan: TVoiceStudioWavePlan;
  Samples: TWfcMusicPcm16Samples;
  Sink: TOneBlockWaveSink;
  Step: TWfcMusicArrangementStep;
  Wave: TWfcMusicWaveStream;
begin
  if FBusy or FReleased then Exit;
  document.body.removeAttribute('data-voice-wave-bytes');
  document.body.removeAttribute('data-voice-wave-peak-bytes');
  Op := TVoiceStudioBrowserOperation.Create;
  Op.Kind := vsbokWave;
  Sink := nil;
  Wave := nil;
  Inc(FAsyncCount);
  try
    try
      Options := ReadOptions;
      Plan := PlanVoiceStudioWave(FSeconds.value);
      Op.Pcm := TVoiceStudioPcmStream.Create(Plan, Options);
      Op.FileTarget := TWfcBrowserStreamFile.Create;
      if ATestPicker <> nil then Picker := ATestPicker
      else Picker := BeginWfcBrowserStreamFilePicker('voice-studio.wav',
        'Voice Studio streamed WAVE', 'audio/wav', '.wav');
      FActive := Op;
      SetBusy(True);
      ClearOutput;
      SetStatus('opening', 'Choose a WAVE destination. The browser controls replacement.');
      await(Op.FileTarget.Open(Picker));
      if not OperationCurrent(FActive, Op) then
        raise EWfcBrowserStreamCancelled.Create('stale WAVE open');
      Sink := TOneBlockWaveSink.Create;
      Wave := TWfcMusicWaveStream.Create(Sink,
        VOICE_STUDIO_STREAM_SAMPLE_RATE, Plan.ExpectedFrames);
      Block := Sink.Detach;
      await(Op.FileTarget.WriteBytes(Block, 'audio/wav'));
      repeat
        if Op.Cancelled then
          raise EWfcBrowserStreamCancelled.Create('WAVE save cancelled');
        Step := Op.Pcm.NextSamples(Samples);
        case Step of
          wmaspProduced:
            begin
              Wave.AppendSamples(Samples);
              Block := Sink.Detach;
              if Length(Block) > 0 then
                await(Op.FileTarget.WriteBytes(Block, 'audio/wav'));
              if Op.Pcm.CopyLatestCell(Cell) then DisplayCell(Cell);
              SetProgress(Op.Pcm.ProducedTicks div VOICE_STUDIO_QUANTUM,
                Plan.CellCount, 'Streaming WAVE: ' +
                IntToStr(Op.Pcm.ProducedTicks div VOICE_STUDIO_QUANTUM) +
                ' / ' + IntToStr(Plan.CellCount) + ' cells');
              DisplayMetrics(Op.Pcm.ProducedTicks div VOICE_STUDIO_QUANTUM,
                Op.Pcm.SegmentsProduced, Op.Pcm.SeamHoldCount,
                Op.Pcm.NovelVerticalCount, Op.Pcm.SharedCoverageCellCount,
                Op.FileTarget.ByteCount);
              await(BrowserYield);
            end;
          wmaspCompleted: ;
          wmaspCancelled:
            raise EWfcBrowserStreamCancelled.Create('WAVE save cancelled');
          wmaspFailed: raise EVoiceStudioStream.Create(Op.Pcm.Failure);
        end;
      until Step = wmaspCompleted;
      if Op.Cancelled then
        raise EWfcBrowserStreamCancelled.Create('WAVE save cancelled');
      Wave.Finish;
      Block := Sink.Detach;
      if Length(Block) > 0 then
        await(Op.FileTarget.WriteBytes(Block, 'audio/wav'));
      if (Wave.FrameCount <> Plan.ExpectedFrames) or
          (Op.Pcm.EmittedFrames <> Plan.ExpectedFrames) then
        raise EVoiceStudioStream.Create('WAVE replay differs from preflight');
      Op.Committing := True;
      SetBusy(True);
      SetStatus('committing', 'Closing the complete WAVE transaction.');
      await(Op.FileTarget.Commit);
      if not FReleased then
      begin
        document.body.setAttribute('data-voice-wave-bytes',
          IntToStr(Op.FileTarget.ByteCount));
        document.body.setAttribute('data-voice-wave-peak-bytes',
          IntToStr(Op.FileTarget.PeakBlockBytes));
        SetStatus('saved', 'WAVE saved after exact generation and byte checks.');
      end;
    except
      if not FReleased then
      begin
        if Op.Cancelled or WfcBrowserStreamFailureIsPickerCancel(JSExceptValue) or
            (isObject(JSExceptValue) and
             (TObject(JSExceptValue) is EWfcBrowserStreamCancelled)) then
          SetStatus('cancelled', 'WAVE was not published.')
        else SetStatus('failed', FailureText(JSExceptValue));
      end;
    end;
  finally
    if (Op.FileTarget <> nil) and
        (Op.FileTarget.State <> wbsfsCommitted) then
      await(SafeAbort(Op.FileTarget));
    Sink.Free;
    Wave.Free;
    if FActive = Op then FActive := nil;
    if not FReleased then SetBusy(False);
    Op.Free;
    AsyncFinished;
  end;
end;

procedure TVoiceStudioBrowserApplication.PlanMidi; async;
var
  Cell: TVoiceStudioCell;
  Op: TVoiceStudioBrowserOperation;
  Options: TVoiceStudioStreamOptions;
  Plan: TVoiceStudioFramePlan;
  Step: TWfcMusicArrangementStep;
begin
  if FBusy or FReleased then Exit;
  Op := TVoiceStudioBrowserOperation.Create;
  Op.Kind := vsbokMidiPlan;
  Inc(FAsyncCount);
  try
    try
      ClearMidiPlan;
      Options := ReadOptions;
      Plan := PlanVoiceStudioFrames(FSeconds.value);
      Op.Planner := TVoiceStudioMidiPlanner.Create(Plan, Options);
      FActive := Op;
      SetBusy(True);
      ClearOutput;
      SetStatus('planning', 'Counting MIDI before any destination is opened.');
      repeat
        if Op.Cancelled then
          raise EWfcBrowserStreamCancelled.Create('MIDI planning cancelled');
        Step := Op.Planner.Next;
        case Step of
          wmaspProduced:
            begin
              if Op.Planner.CopyLatestCell(Cell) then DisplayCell(Cell);
              SetProgress(Op.Planner.FramesProcessed, Plan.CellCount,
                'Planning MIDI: ' + IntToStr(Op.Planner.FramesProcessed) +
                ' / ' + IntToStr(Plan.CellCount) + ' cells');
              await(BrowserYield);
            end;
          wmaspCompleted: ;
          wmaspCancelled:
            raise EWfcBrowserStreamCancelled.Create('MIDI planning cancelled');
          wmaspFailed: raise EVoiceStudioStream.Create(Op.Planner.Failure);
        end;
      until Step = wmaspCompleted;
      if not OperationCurrent(FActive, Op) then
        raise EWfcBrowserStreamCancelled.Create('stale MIDI plan');
      Op.MidiPlan := Op.Planner.DetachPlan;
      FMidiPlan := Op.MidiPlan;
      Op.MidiPlan := nil;
      document.body.setAttribute('data-voice-midi-plan', 'ready');
      document.body.setAttribute('data-voice-midi-bytes',
        IntToStr(FMidiPlan.FileByteCount));
      document.body.setAttribute('data-voice-midi-signature',
        IntToHex(FMidiPlan.MidiSignature, 8));
      DisplayMetrics(FMidiPlan.FrameCount, FMidiPlan.SegmentsProduced,
        FMidiPlan.SeamHoldCount, FMidiPlan.NovelVerticalCount,
        FMidiPlan.SharedCoverageCellCount, 0);
      SetStatus('planned', 'MIDI plan is ready. Click Save planned MIDI to choose a destination.');
    except
      if not FReleased then
      begin
        ClearMidiPlan;
        if Op.Cancelled or (isObject(JSExceptValue) and
            (TObject(JSExceptValue) is EWfcBrowserStreamCancelled)) then
          SetStatus('cancelled', 'MIDI planning cancelled; no destination was opened.')
        else SetStatus('failed', FailureText(JSExceptValue));
      end;
    end;
  finally
    if FActive = Op then FActive := nil;
    if not FReleased then SetBusy(False);
    Op.Free;
    AsyncFinished;
  end;
end;

procedure TVoiceStudioBrowserApplication.SaveMidi(
  const ATestPicker: TJSPromise); async;
var
  Block: TWfcMidiBytes;
  Cell: TVoiceStudioCell;
  CurrentOptions: TVoiceStudioStreamOptions;
  CurrentPlan: TVoiceStudioFramePlan;
  Op: TVoiceStudioBrowserOperation;
  Picker: TJSPromise;
  Step: TWfcMusicArrangementStep;
begin
  if FBusy or FReleased or (FMidiPlan = nil) then Exit;
  document.body.removeAttribute('data-voice-midi-saved-bytes');
  document.body.removeAttribute('data-voice-midi-peak-bytes');
  Op := TVoiceStudioBrowserOperation.Create;
  Op.Kind := vsbokMidiSave;
  Op.MidiPlan := FMidiPlan;
  FMidiPlan := nil;
  ClearMidiPlan;
  Inc(FAsyncCount);
  try
    try
      CurrentOptions := ReadOptions;
      CurrentPlan := PlanVoiceStudioFrames(FSeconds.value);
      if not StreamOptionsEqual(CurrentOptions, Op.MidiPlan.Options) or
          not FramePlansEqual(CurrentPlan, Op.MidiPlan.FramePlan) then
        raise EVoiceStudioStream.Create(
          'MIDI plan is stale; plan again before choosing a destination');
      Op.Midi := TVoiceStudioMidiStream.Create(Op.MidiPlan);
      Op.FileTarget := TWfcBrowserStreamFile.Create;
      if ATestPicker <> nil then Picker := ATestPicker
      else Picker := BeginWfcBrowserStreamFilePicker('voice-studio.mid',
        'Voice Studio streamed MIDI', 'audio/midi', '.mid');
      FActive := Op;
      SetBusy(True);
      SetStatus('opening', 'Choose a MIDI destination. The browser controls replacement.');
      await(Op.FileTarget.Open(Picker));
      if not OperationCurrent(FActive, Op) then
        raise EWfcBrowserStreamCancelled.Create('stale MIDI open');
      repeat
        if Op.Cancelled then
          raise EWfcBrowserStreamCancelled.Create('MIDI save cancelled');
        Step := Op.Midi.NextBytes(Block);
        case Step of
          wmaspProduced:
            begin
              if Length(Block) > 0 then
                await(Op.FileTarget.WriteBytes(Block, 'audio/midi'));
              if Op.Midi.CopyLatestCell(Cell) then DisplayCell(Cell);
              SetProgress(Op.Midi.FramesProcessed,
                Op.MidiPlan.FrameCount, 'Saving planned MIDI: ' +
                IntToStr(Op.Midi.FramesProcessed) + ' / ' +
                IntToStr(Op.MidiPlan.FrameCount) + ' cells');
              DisplayMetrics(Op.Midi.FramesProcessed,
                Op.MidiPlan.SegmentsProduced, Op.MidiPlan.SeamHoldCount,
                Op.MidiPlan.NovelVerticalCount,
                Op.MidiPlan.SharedCoverageCellCount,
                Op.FileTarget.ByteCount);
              await(BrowserYield);
            end;
          wmaspCompleted: ;
          wmaspCancelled:
            raise EWfcBrowserStreamCancelled.Create('MIDI save cancelled');
          wmaspFailed: raise EVoiceStudioStream.Create(Op.Midi.Failure);
        end;
      until Step = wmaspCompleted;
      if Op.Cancelled then
        raise EWfcBrowserStreamCancelled.Create('MIDI save cancelled');
      if (Op.Midi.EmittedBytes <> Op.MidiPlan.FileByteCount) or
          (Op.FileTarget.ByteCount <> Op.MidiPlan.FileByteCount) or
          (Op.Midi.TickCount <> Op.MidiPlan.EndTick) then
        raise EVoiceStudioStream.Create('MIDI replay differs from its plan');
      Op.Committing := True;
      SetBusy(True);
      SetStatus('committing', 'Closing the verified MIDI transaction.');
      await(Op.FileTarget.Commit);
      if not FReleased then
      begin
        document.body.setAttribute('data-voice-midi-saved-bytes',
          IntToStr(Op.FileTarget.ByteCount));
        document.body.setAttribute('data-voice-midi-peak-bytes',
          IntToStr(Op.FileTarget.PeakBlockBytes));
        SetStatus('saved', 'Planned MIDI saved after replay verification.');
      end;
    except
      if not FReleased then
      begin
        if Op.Cancelled or WfcBrowserStreamFailureIsPickerCancel(JSExceptValue) or
            (isObject(JSExceptValue) and
             (TObject(JSExceptValue) is EWfcBrowserStreamCancelled)) then
          SetStatus('cancelled', 'MIDI was not published; plan was consumed.')
        else SetStatus('failed', FailureText(JSExceptValue));
      end;
    end;
  finally
    if (Op.FileTarget <> nil) and
        (Op.FileTarget.State <> wbsfsCommitted) then
      await(SafeAbort(Op.FileTarget));
    if FActive = Op then FActive := nil;
    if not FReleased then SetBusy(False);
    Op.Free;
    AsyncFinished;
  end;
end;

procedure TVoiceStudioBrowserApplication.Run;
var
  LMutationHandler: TJSEventHandler;
begin
  document.body.setAttribute('data-self-test', 'not-requested');
  document.body.setAttribute('data-voice-stream-self-test', 'not-requested');
  document.body.setAttribute('data-voice-stream-release', 'not-run');
  FSeed := TJSHTMLInputElement(RequireElement('voice-seed'));
  FSeconds := TJSHTMLInputElement(RequireElement('voice-seconds'));
  FSegmentCells := TJSHTMLInputElement(RequireElement('voice-segment'));
  FBacktracks := TJSHTMLInputElement(RequireElement('voice-backtracks'));
  FPassBacktracks := TJSHTMLInputElement(RequireElement('voice-pass-backtracks'));
  FTrace := TJSHTMLInputElement(RequireElement('voice-trace'));
  FWaveButton := TJSHTMLButtonElement(RequireElement('voice-save-wave'));
  FMidiPlanButton := TJSHTMLButtonElement(RequireElement('voice-plan-midi'));
  FMidiSaveButton := TJSHTMLButtonElement(RequireElement('voice-save-midi'));
  FCancelButton := TJSHTMLButtonElement(RequireElement('voice-cancel'));
  FProgress := TJSHTMLProgressElement(RequireElement('voice-progress'));
  FStatus := RequireElement('voice-status');
  FPlanText := RequireElement('voice-plan');
  FProgressText := RequireElement('voice-progress-text');
  FFallback := RequireElement('voice-fallback');
  FCellPosition := RequireElement('voice-cell-position');
  FCellNovelty := RequireElement('voice-cell-novelty');
  FHarmony := RequireElement('voice-harmony');
  FRhythm := RequireElement('voice-rhythm');
  FBass := RequireElement('voice-bass');
  FChord := RequireElement('voice-chord');
  FUpper := RequireElement('voice-upper');
  FCoverage := RequireElement('voice-coverage');
  FMetrics := RequireElement('voice-metrics');
  FWaveButton.onclick := @HandleWave;
  FMidiPlanButton.onclick := @HandleMidiPlan;
  FMidiSaveButton.onclick := @HandleMidiSave;
  FCancelButton.onclick := @HandleCancel;
  LMutationHandler := @HandleMutation;
  document.addEventListener('input', LMutationHandler, True);
  document.addEventListener('change', LMutationHandler, True);
  FBound := True;
  ClearMidiPlan;
  SetBusy(False);
  ClearOutput;
  RefreshPlan;
  if WfcBrowserStreamFileAvailable then
    SetStatus('ready', 'Ready. Browser Save As may replace a file you select.')
  else SetStatus('unavailable',
    'Streaming Save As is unavailable here; use the shown native command.');
end;

procedure TVoiceStudioBrowserApplication.Release;
begin
  if FReleased then Exit;
  FReleased := True;
  if FActive <> nil then
  begin
    FActive.Cancel;
    if FActive.FileTarget <> nil then FActive.FileTarget.Release;
  end;
  FreeAndNil(FMidiPlan);
  if FBound then
  begin
    TVoiceStudioEventDocument(document).removeEventListener(
      'input', @HandleMutation, True);
    TVoiceStudioEventDocument(document).removeEventListener(
      'change', @HandleMutation, True);
    FWaveButton.onclick := nil;
    FMidiPlanButton.onclick := nil;
    FMidiSaveButton.onclick := nil;
    FCancelButton.onclick := nil;
  end;
  RestoreReleasedControls;
  if FAsyncCount = 0 then Free;
end;

procedure TVoiceStudioBrowserApplication.CheckStreamFileFixtures; async;
var
  LAbortCalls: Integer;
  LAbortRejected, LTaskDone, LTaskRejected: Boolean;
  LBlob: TJSBlob;
  LBytes: TWfcMidiBytes;
  LClosePromise, LOpenPromise, LPicker, LWritePromise: TJSPromise;
  LCloseResolve, LOpenResolve, LWriteResolve: TJSPromiseResolver;
  LFile, LWritable: TJSObject;
  LTarget: TWfcBrowserStreamFile;
  LTyped: TJSUint8Array;

  procedure RunOpenTask; async;
  begin
    try
      await(LTarget.Open(LPicker));
    except
      LTaskRejected := True;
    end;
    LTaskDone := True;
  end;

  procedure RunWriteTask; async;
  begin
    try
      await(LTarget.WriteBytes(LBytes, 'application/octet-stream'));
    except
      LTaskRejected := True;
    end;
    LTaskDone := True;
  end;

  procedure RunCommitTask; async;
  begin
    try
      await(LTarget.Commit);
    except
      LTaskRejected := True;
    end;
    LTaskDone := True;
  end;

  function NewWritable: TJSObject;
  begin
    Result := TJSObject.new;
    Result['write'] := function(AData: JSValue): JSValue
      begin
        LBlob := TJSBlob(AData);
        Result := LWritePromise;
      end;
    Result['close'] := function: JSValue
      begin Result := LClosePromise; end;
    Result['abort'] := function: JSValue
      begin
        Inc(LAbortCalls);
        Result := TJSPromise.resolve(Null);
      end;
  end;

  procedure OpenImmediate; async;
  begin
    LFile := TJSObject.new;
    LFile['createWritable'] := function: JSValue
      begin Result := TJSPromise.resolve(LWritable); end;
    await(LTarget.Open(TJSPromise.resolve(LFile)));
  end;

begin
  { Cancel while createWritable is pending. The writable returned after the
    cancellation must still be aborted exactly once. }
  LAbortCalls := 0;
  LTaskDone := False;
  LTaskRejected := False;
  LWritePromise := TJSPromise.resolve(Null);
  LClosePromise := TJSPromise.resolve(Null);
  LOpenPromise := TJSPromise.new(
    procedure(AResolve, AReject: TJSPromiseResolver)
    begin LOpenResolve := AResolve; end);
  LWritable := NewWritable;
  LFile := TJSObject.new;
  LFile['createWritable'] := function: JSValue
    begin Result := LOpenPromise; end;
  LPicker := TJSPromise.resolve(LFile);
  LTarget := TWfcBrowserStreamFile.Create;
  try
    RunOpenTask;
    await(BrowserYield);
    LAbortRejected := False;
    try
      await(LTarget.Abort);
    except
      LAbortRejected := True;
    end;
    BrowserAssert(LAbortRejected and (LAbortCalls = 0),
      'abort cannot overlap a pending writable open');
    LTarget.Cancel;
    LOpenResolve(LWritable);
    while not LTaskDone do await(BrowserYield);
    BrowserAssert(LTaskRejected, 'cancelled writable open rejects its waiter');
    await(LTarget.Abort);
    await(LTarget.Abort);
    BrowserAssert((LAbortCalls = 1) and
      (LTarget.State = wbsfsCancelled),
      'settled writable is aborted exactly once and replayed abort is harmless');
  finally
    LTarget.Free;
  end;

  { WriteBytes must snapshot its borrowed array before awaiting the backend. }
  LAbortCalls := 0;
  LTaskDone := False;
  LTaskRejected := False;
  LBlob := nil;
  LWritePromise := TJSPromise.new(
    procedure(AResolve, AReject: TJSPromiseResolver)
    begin LWriteResolve := AResolve; end);
  LClosePromise := TJSPromise.resolve(Null);
  LWritable := NewWritable;
  LTarget := TWfcBrowserStreamFile.Create;
  try
    await(OpenImmediate);
    LBytes := [1, 2, 3];
    RunWriteTask;
    await(BrowserYield);
    LAbortRejected := False;
    try
      await(LTarget.Abort);
    except
      LAbortRejected := True;
    end;
    BrowserAssert(LAbortRejected and (LAbortCalls = 0),
      'abort cannot overlap a pending write');
    LBytes[0] := 99;
    SetLength(LBytes, 0);
    LWriteResolve(Null);
    while not LTaskDone do await(BrowserYield);
    BrowserAssert(not LTaskRejected and (LTarget.ByteCount = 3) and
      (LTarget.PeakBlockBytes = 3),
      'borrowed-array mutation cannot change completed byte accounting');
    LTyped := TJSUint8Array.new(TJSArrayBuffer(await(LBlob.arrayBuffer)));
    BrowserAssert((LTyped.length = 3) and (LTyped[0] = 1) and
      (LTyped[1] = 2) and (LTyped[2] = 3),
      'Uint8Array-backed Blob preserves the pre-await byte snapshot');
    LTarget.Cancel;
    await(LTarget.Abort);
    BrowserAssert(LAbortCalls = 1, 'successful pending write remains abortable');
  finally
    LTarget.Free;
  end;

  { Cooperative cancellation marks the pending write stale, then cleanup waits
    for that promise and aborts the destination once. }
  LAbortCalls := 0;
  LTaskDone := False;
  LTaskRejected := False;
  LWritePromise := TJSPromise.new(
    procedure(AResolve, AReject: TJSPromiseResolver)
    begin LWriteResolve := AResolve; end);
  LClosePromise := TJSPromise.resolve(Null);
  LWritable := NewWritable;
  LTarget := TWfcBrowserStreamFile.Create;
  try
    await(OpenImmediate);
    LBytes := [4, 5];
    RunWriteTask;
    await(BrowserYield);
    LTarget.Cancel;
    LWriteResolve(Null);
    while not LTaskDone do await(BrowserYield);
    BrowserAssert(LTaskRejected and (LTarget.ByteCount = 0),
      'cancelled pending write cannot publish byte accounting');
    await(LTarget.Abort);
    BrowserAssert(LAbortCalls = 1,
      'cancelled pending write is aborted only after it settles');
  finally
    LTarget.Free;
  end;

  { Once close starts it is the irreversible publication boundary. }
  LAbortCalls := 0;
  LTaskDone := False;
  LTaskRejected := False;
  LWritePromise := TJSPromise.resolve(Null);
  LClosePromise := TJSPromise.new(
    procedure(AResolve, AReject: TJSPromiseResolver)
    begin LCloseResolve := AResolve; end);
  LWritable := NewWritable;
  LTarget := TWfcBrowserStreamFile.Create;
  try
    await(OpenImmediate);
    RunCommitTask;
    await(BrowserYield);
    LAbortRejected := False;
    try
      await(LTarget.Abort);
    except
      LAbortRejected := True;
    end;
    BrowserAssert(LAbortRejected and (LTarget.State = wbsfsCommitting) and
      not LTarget.Cancelled and (LAbortCalls = 0),
      'abort cannot race or relabel a pending commit');
    LCloseResolve(Null);
    while not LTaskDone do await(BrowserYield);
    BrowserAssert(not LTaskRejected and
      (LTarget.State = wbsfsCommitted), 'pending close commits exactly once');
    await(LTarget.Abort);
    BrowserAssert(LAbortCalls = 0, 'abort after commit is a no-op');
  finally
    LTarget.Free;
  end;
end;

procedure TVoiceStudioBrowserApplication.CheckWaveFixture(
  const AKind: String); async;
var
  LAbortCalls, LCloseCalls, LOpenCalls, LPickerCalls, LWriteCalls: Integer;
  LBytes, LPeak: NativeInt;
  LFile, LWritable: TJSObject;
  LInFlight: Boolean;
  LOldPicker: JSValue;
  LStateAtRelease: String;
begin
  LAbortCalls := 0;
  LCloseCalls := 0;
  LOpenCalls := 0;
  LPickerCalls := 0;
  LWriteCalls := 0;
  LBytes := 0;
  LPeak := 0;
  LInFlight := False;
  LStateAtRelease := '';
  LWritable := TJSObject.new;
  LWritable['write'] := function(AData: JSValue): JSValue
    var LSize: NativeInt;
    begin
      BrowserAssert(not LInFlight, 'WAVE writes cannot overlap');
      LInFlight := True;
      LSize := TJSBlob(AData).size;
      BrowserAssert((LSize > 0) and (LSize <= 4096),
        'WAVE Blob stays within one bounded transport block');
      Inc(LWriteCalls);
      Inc(LBytes, LSize);
      if LSize > LPeak then LPeak := LSize;
      if (AKind = 'cancel') and (LWriteCalls = 1) then FCancelButton.click;
      if (AKind = 'edit') and (LWriteCalls = 1) then
      begin
        FSeconds.value := '1.75';
        FSeconds.dispatchEvent(TJSEvent.new('input'));
      end;
      Result := TJSBlob(AData).arrayBuffer._then(
        function(AValue: JSValue): JSValue
        var LData: TJSUint8Array;
        begin
          LData := TJSUint8Array.new(TJSArrayBuffer(AValue));
          if LWriteCalls = 1 then
            BrowserAssert((LData.length = 44) and (LData[0] = Ord('R')) and
              (LData[1] = Ord('I')) and (LData[2] = Ord('F')) and
              (LData[3] = Ord('F')),
              'first WAVE Blob contains the binary RIFF header');
          LInFlight := False;
          Result := Null;
        end);
    end;
  LWritable['close'] := function: JSValue
    begin
      BrowserAssert(not LInFlight, 'WAVE close waits for the final block');
      BrowserAssert(FSeed.disabled and FSeconds.disabled and
        FCancelButton.disabled, 'WAVE commit freezes edits and cancellation');
      Inc(LCloseCalls);
      if AKind = 'release-close' then
      begin
        LStateAtRelease := document.body.getAttribute('data-voice-state');
        Release;
      end;
      Result := BrowserYield;
    end;
  LWritable['abort'] := function: JSValue
    begin
      BrowserAssert(not LInFlight, 'WAVE abort waits for the active write');
      Inc(LAbortCalls);
      Result := TJSPromise.resolve(Null);
    end;
  LFile := TJSObject.new;
  LFile['createWritable'] := function: JSValue
    begin
      Inc(LOpenCalls);
      Result := TJSPromise.resolve(LWritable);
    end;
  LOldPicker := TJSObject(window)['showSaveFilePicker'];
  TJSObject(window)['showSaveFilePicker'] :=
    function(AOptions: JSValue): JSValue
    begin
      Inc(LPickerCalls);
      Result := TJSPromise.resolve(LFile);
    end;
  try
    FSeconds.value := '1.5';
    FSeed.value := '1';
    FSegmentCells.value := '5';
    FBacktracks.value := '1024';
    FPassBacktracks.value := '64';
    if AKind = 'invalid' then FSeconds.value := '1.5x';
    if AKind = 'solve-failure' then
    begin
      FSeed.value := '0';
      FPassBacktracks.value := '0';
    end;
    RefreshPlan;
    SetBusy(False);
    FWaveButton.click;
    if AKind = 'release-close' then
      while FActive <> nil do await(BrowserYield)
    else
      while FBusy do await(BrowserYield);
    if AKind = 'saved' then
    begin
      BrowserAssert((LPickerCalls = 1) and (LOpenCalls = 1) and
        (LWriteCalls > 2) and (LBytes = 132344) and
        (LCloseCalls = 1) and (LAbortCalls = 0),
        'WAVE button streams exact bytes and commits once');
      BrowserAssert((document.body.getAttribute('data-voice-state') = 'saved') and
        (document.body.getAttribute('data-voice-wave-bytes') = '132344') and
        (StrToInt(document.body.getAttribute('data-voice-wave-peak-bytes')) = LPeak),
        'WAVE completion markers publish only after close');
      BrowserAssert(Pos('novel verticals', FMetrics.textContent) > 0,
        'WAVE event path displays independent-voice novelty evidence');
    end
    else if (AKind = 'cancel') or (AKind = 'edit') then
    begin
      BrowserAssert((LPickerCalls = 1) and (LOpenCalls = 1) and
        (LWriteCalls = 1) and (LCloseCalls = 0) and (LAbortCalls = 1),
        'cancel/edit waits for one active WAVE block then aborts once');
      BrowserAssert(document.body.getAttribute('data-voice-state') = 'cancelled',
        'cancelled WAVE cannot claim a saved artifact');
    end
    else if AKind = 'invalid' then
    begin
      BrowserAssert((LPickerCalls = 0) and (LOpenCalls = 0) and
        (LWriteCalls = 0) and (LCloseCalls = 0) and (LAbortCalls = 0),
        'invalid duration is rejected before the picker');
      BrowserAssert(document.body.getAttribute('data-voice-state') = 'failed',
        'invalid WAVE request has an explicit failure state');
    end
    else if AKind = 'solve-failure' then
    begin
      BrowserAssert((LPickerCalls = 1) and (LOpenCalls = 1) and
        (LCloseCalls = 0) and (LAbortCalls = 1),
        'terminal generation failure aborts its opened WAVE destination');
      BrowserAssert((document.body.getAttribute('data-voice-state') = 'failed') and
        not document.body.hasAttribute('data-voice-wave-bytes'),
        'failed generation retains no current WAVE marker');
    end
    else if AKind = 'release-close' then
    begin
      BrowserAssert((LPickerCalls = 1) and (LOpenCalls = 1) and
        (LWriteCalls > 2) and (LCloseCalls = 1) and (LAbortCalls = 0),
        'release during close lets the irreversible WAVE commit settle once');
      BrowserAssert(FReleased and
        (document.body.getAttribute('data-voice-state') = LStateAtRelease) and
        not document.body.hasAttribute('data-voice-wave-bytes'),
        'released controller publishes no post-close WAVE markers');
      BrowserAssert((FWaveButton.onclick = nil) and
        (FMidiPlanButton.onclick = nil) and (FMidiSaveButton.onclick = nil) and
        (FCancelButton.onclick = nil) and not FSeed.disabled and
        FWaveButton.disabled and FMidiPlanButton.disabled and
        FMidiSaveButton.disabled and FCancelButton.disabled,
        'release detaches handlers and restores captured controls safely');
    end;
  finally
    TJSObject(window)['showSaveFilePicker'] := LOldPicker;
    if not FReleased then
    begin
      FSeconds.value := '1.5';
      FSeed.value := '1';
      FPassBacktracks.value := '64';
      RefreshPlan;
      SetBusy(False);
    end;
  end;
end;

procedure TVoiceStudioBrowserApplication.CheckMidiFixtures; async;
var
  LAbortCalls, LCloseCalls, LOpenCalls, LPickerCalls, LWriteCalls: Integer;
  LExpected, LObserved: TWfcMidiBytes;
  LFile, LWritable: TJSObject;
  LInFlight: Boolean;
  LOldPicker: JSValue;
  LOpenPromise: TJSPromise;
  LOpenResolve: TJSPromiseResolver;
  LPlannedBytes: TWfcMidiStreamCount;
begin
  FSeconds.value := '8';
  ClearMidiPlan;
  SetBusy(False);
  FMidiPlanButton.click;
  FSeconds.value := '1.75';
  FSeconds.dispatchEvent(TJSEvent.new('input'));
  while FBusy do await(BrowserYield);
  BrowserAssert((FMidiPlan = nil) and FMidiSaveButton.disabled and
    (document.body.getAttribute('data-voice-midi-plan') = 'none'),
    'input mutation cancels an asynchronous MIDI counting pass');

  FSeconds.value := '1.5';
  FSeconds.dispatchEvent(TJSEvent.new('input'));
  SetBusy(False);
  FMidiPlanButton.click;
  while FBusy do await(BrowserYield);
  BrowserAssert((FMidiPlan <> nil) and (FMidiPlan.FrameCount = 6) and
    (document.body.getAttribute('data-voice-midi-plan') = 'ready'),
    'Plan MIDI button completes before any destination is chosen');

  FSeconds.value := '1.75';
  await(SaveMidi(TJSPromise.resolve(Null)));
  BrowserAssert((FMidiPlan = nil) and
    (document.body.getAttribute('data-voice-midi-plan') = 'none') and
    (document.body.getAttribute('data-voice-state') = 'failed'),
    'programmatic input changes cannot carry a stale plan to the picker');
  FSeconds.value := '1.5';
  FSeconds.dispatchEvent(TJSEvent.new('input'));
  FMidiPlanButton.click;
  while FBusy do await(BrowserYield);
  BrowserAssert(FMidiPlan <> nil,
    'fresh MIDI plan recovers after programmatic stale-plan rejection');
  LExpected := CollectPlannedMidiBytes(FMidiPlan);
  LPlannedBytes := FMidiPlan.FileByteCount;

  LAbortCalls := 0;
  LCloseCalls := 0;
  LOpenCalls := 0;
  LPickerCalls := 0;
  LWriteCalls := 0;
  LObserved := nil;
  LInFlight := False;
  LWritable := TJSObject.new;
  LWritable['write'] := function(AData: JSValue): JSValue
    begin
      BrowserAssert(not LInFlight, 'MIDI writes cannot overlap');
      BrowserAssert((TJSBlob(AData).size > 0) and
        (TJSBlob(AData).size <= 4096),
        'MIDI Blob stays within one bounded transport block');
      LInFlight := True;
      Inc(LWriteCalls);
      Result := TJSBlob(AData).arrayBuffer._then(
        function(AValue: JSValue): JSValue
        begin
          AppendBytes(LObserved,
            TJSUint8Array.new(TJSArrayBuffer(AValue)));
          LInFlight := False;
          Result := Null;
        end);
    end;
  LWritable['close'] := function: JSValue
    begin
      BrowserAssert(not LInFlight, 'MIDI close waits for the final block');
      BrowserAssert(FSeed.disabled and FSeconds.disabled and
        FCancelButton.disabled, 'MIDI commit freezes edits and cancellation');
      Inc(LCloseCalls);
      Result := BrowserYield;
    end;
  LWritable['abort'] := function: JSValue
    begin
      Inc(LAbortCalls);
      Result := TJSPromise.resolve(Null);
    end;
  LFile := TJSObject.new;
  LFile['createWritable'] := function: JSValue
    begin
      Inc(LOpenCalls);
      Result := TJSPromise.resolve(LWritable);
    end;
  LOldPicker := TJSObject(window)['showSaveFilePicker'];
  TJSObject(window)['showSaveFilePicker'] :=
    function(AOptions: JSValue): JSValue
    begin
      Inc(LPickerCalls);
      Result := TJSPromise.resolve(LFile);
    end;
  try
    SetBusy(False);
    FMidiSaveButton.click;
    while FBusy do await(BrowserYield);
    BrowserAssert((LPickerCalls = 1) and (LOpenCalls = 1) and
      (LWriteCalls > 0) and (LCloseCalls = 1) and (LAbortCalls = 0) and
      (Length(LObserved) = LPlannedBytes),
      'Save planned MIDI replays exact byte count and commits once');
    BrowserAssert(BytesEqual(LObserved, LExpected),
      'Uint8Array-backed MIDI Blobs preserve every independently replayed byte');
    BrowserAssert((FMidiPlan = nil) and
      (document.body.getAttribute('data-voice-state') = 'saved') and
      (document.body.getAttribute('data-voice-midi-saved-bytes') =
        IntToStr(LPlannedBytes)),
      'saved MIDI consumes its plan and publishes only after close');
  finally
    TJSObject(window)['showSaveFilePicker'] := LOldPicker;
  end;

  { A completed plan is stale immediately when any captured option changes. }
  FMidiPlanButton.click;
  while FBusy do await(BrowserYield);
  BrowserAssert(FMidiPlan <> nil, 'second MIDI plan is available for stale test');
  FSeed.value := '2';
  FSeed.dispatchEvent(TJSEvent.new('input'));
  BrowserAssert((FMidiPlan = nil) and FMidiSaveButton.disabled,
    'captured MIDI plan is invalidated synchronously by input mutation');
  FSeed.value := '1';
  FSeed.dispatchEvent(TJSEvent.new('input'));

  { Mutation while createWritable is pending must abort the returned writable
    and must never write replay bytes into it. }
  FMidiPlanButton.click;
  while FBusy do await(BrowserYield);
  BrowserAssert(FMidiPlan <> nil, 'open-race fixture has a counted MIDI plan');
  LAbortCalls := 0;
  LCloseCalls := 0;
  LOpenCalls := 0;
  LPickerCalls := 0;
  LWriteCalls := 0;
  LOpenPromise := TJSPromise.new(
    procedure(AResolve, AReject: TJSPromiseResolver)
    begin LOpenResolve := AResolve; end);
  LWritable := TJSObject.new;
  LWritable['write'] := function(AData: JSValue): JSValue
    begin Inc(LWriteCalls); Result := TJSPromise.resolve(Null); end;
  LWritable['close'] := function: JSValue
    begin Inc(LCloseCalls); Result := TJSPromise.resolve(Null); end;
  LWritable['abort'] := function: JSValue
    begin Inc(LAbortCalls); Result := TJSPromise.resolve(Null); end;
  LFile := TJSObject.new;
  LFile['createWritable'] := function: JSValue
    begin Inc(LOpenCalls); Result := LOpenPromise; end;
  LOldPicker := TJSObject(window)['showSaveFilePicker'];
  TJSObject(window)['showSaveFilePicker'] :=
    function(AOptions: JSValue): JSValue
    begin Inc(LPickerCalls); Result := TJSPromise.resolve(LFile); end;
  try
    SetBusy(False);
    FMidiSaveButton.click;
    await(BrowserYield);
    FSeconds.value := '1.75';
    FSeconds.dispatchEvent(TJSEvent.new('input'));
    LOpenResolve(LWritable);
    while FBusy do await(BrowserYield);
    BrowserAssert((LPickerCalls = 1) and (LOpenCalls = 1) and
      (LWriteCalls = 0) and (LCloseCalls = 0) and (LAbortCalls = 1),
      'stale writable completion aborts before any MIDI replay write');
    BrowserAssert(document.body.getAttribute('data-voice-state') = 'cancelled',
      'pending-open mutation cannot publish MIDI');
  finally
    TJSObject(window)['showSaveFilePicker'] := LOldPicker;
    FSeconds.value := '1.5';
    FSeconds.dispatchEvent(TJSEvent.new('input'));
  end;
end;

procedure TVoiceStudioBrowserApplication.RunSelfTest; async;
var
  LReleaseController: TVoiceStudioBrowserApplication;
begin
  if FReleased or FSelfTesting or FBusy then Exit;
  FSelfTesting := True;
  Inc(FAsyncCount);
  document.body.removeAttribute('data-self-test-message');
  document.body.removeAttribute('data-voice-stream-test-message');
  document.body.setAttribute('data-self-test', 'pending');
  document.body.setAttribute('data-voice-stream-self-test', 'pending');
  document.body.setAttribute('data-voice-stream-release', 'pending');
  LReleaseController := nil;
  try
    try
      BrowserAssert(Pos('--segment-cells 5 --backtracks 1024 ' +
        '--pass-backtracks 64', FFallback.textContent) > 0,
        'native fallback preserves all captured search allowances');
      await(CheckStreamFileFixtures);
      await(CheckWaveFixture('saved'));
      await(CheckWaveFixture('cancel'));
      await(CheckWaveFixture('edit'));
      await(CheckWaveFixture('solve-failure'));
      await(CheckWaveFixture('invalid'));
      await(CheckMidiFixtures);

      LReleaseController := TVoiceStudioBrowserApplication.Create;
      try
        LReleaseController.Run;
        Inc(LReleaseController.FAsyncCount);
        try
          await(LReleaseController.CheckWaveFixture('release-close'));
        finally
          if not LReleaseController.FReleased then
            LReleaseController.Release;
          LReleaseController.AsyncFinished;
          LReleaseController := nil;
        end;
      finally
        { The temporary controller used the same fixture. Restore this live
          controller's handlers after the temporary listener is detached. }
        FWaveButton.onclick := @HandleWave;
        FMidiPlanButton.onclick := @HandleMidiPlan;
        FMidiSaveButton.onclick := @HandleMidiSave;
        FCancelButton.onclick := @HandleCancel;
        SetBusy(False);
      end;
      BrowserAssert(not FWaveButton.disabled and not FMidiPlanButton.disabled and
        FMidiSaveButton.disabled and FCancelButton.disabled,
        'live controls remain usable after temporary-controller release');
      document.body.setAttribute('data-voice-stream-release', 'passed');
      document.body.setAttribute('data-voice-stream-self-test', 'passed');
      document.body.setAttribute('data-self-test', 'passed');
      document.body.setAttribute('data-voice-stream-helper', 'passed');
      RefreshPlan;
      ClearOutput;
      SetStatus('ready',
        'Browser transaction checks passed; no user destination was opened.');
    except
      document.body.setAttribute('data-voice-stream-self-test', 'failed');
      document.body.setAttribute('data-voice-stream-release', 'failed');
      document.body.setAttribute('data-self-test', 'failed');
      document.body.setAttribute('data-self-test-message',
        FailureText(JSExceptValue));
      document.body.setAttribute('data-voice-stream-test-message',
        FailureText(JSExceptValue));
      if not FReleased then SetStatus('failed', FailureText(JSExceptValue));
    end;
  finally
    FSelfTesting := False;
    AsyncFinished;
  end;
end;

procedure InstallVoiceStudioBrowserTestFixture;
begin
  TJSHTMLElement(document.body).innerHTML :=
    '<main>' +
    '<input id="voice-seed" type="text" value="1">' +
    '<input id="voice-seconds" type="text" value="1.5">' +
    '<input id="voice-segment" type="number" min="1" step="1" value="5">' +
    '<input id="voice-backtracks" type="number" min="0" step="1" value="1024">' +
    '<input id="voice-pass-backtracks" type="number" min="0" step="1" value="64">' +
    '<input id="voice-trace" type="checkbox">' +
    '<button id="voice-save-wave">Save WAVE</button>' +
    '<button id="voice-plan-midi">Plan MIDI</button>' +
    '<button id="voice-save-midi">Save MIDI</button>' +
    '<button id="voice-cancel">Cancel</button>' +
    '<progress id="voice-progress"></progress>' +
    '<p id="voice-status"></p><p id="voice-plan"></p>' +
    '<p id="voice-progress-text"></p><pre id="voice-fallback"></pre>' +
    '<p id="voice-cell-position"></p><span id="voice-cell-novelty"></span>' +
    '<span id="voice-harmony"></span><span id="voice-rhythm"></span>' +
    '<span id="voice-bass"></span><span id="voice-chord"></span>' +
    '<span id="voice-upper"></span><span id="voice-coverage"></span>' +
    '<p id="voice-metrics"></p></main>';
end;

end.
