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
program VoiceStudioRender;

{$mode delphi}{$H+}

uses
  SysUtils,
  wfc,
  wfc_midi_smf,
  wfc_music_audio,
  wfc_music_audio_stream,
  wfc_music_arrangement,
  voice_studio_corpus,
  voice_studio_stream,
  voice_studio_midi_stream,
  voice_studio_demo,
  wfc_atomic_new_file
  {$IFDEF MSWINDOWS}, Windows{$ELSE}, BaseUnix{$ENDIF};

type
  TAtomicWaveSink = class(TWfcMusicAudioByteSink)
  private
    FFile: TWfcAtomicNewFile;
  public
    constructor Create(const AFile: TWfcAtomicNewFile);
    procedure WriteBytes(const ABytes: array of Byte); override;
  end;

var
  CancelRequested: LongInt;

{$IFDEF MSWINDOWS}
function ConsoleControl(const AEvent: DWORD): BOOL; stdcall;
begin
  Result := (AEvent = CTRL_C_EVENT) or (AEvent = CTRL_BREAK_EVENT);
  if Result then InterlockedExchange(CancelRequested, 1);
end;
{$ELSE}
procedure InterruptSignal(const ASignal: cint); cdecl;
begin
  CancelRequested := 1;
end;
{$ENDIF}

constructor TAtomicWaveSink.Create(const AFile: TWfcAtomicNewFile);
begin
  inherited Create;
  if AFile = nil then raise Exception.Create('atomic file cannot be nil');
  FFile := AFile;
end;

procedure TAtomicWaveSink.WriteBytes(const ABytes: array of Byte);
begin
  FFile.WriteBytes(ABytes);
end;

function ParseUnsigned(const AText, AName: String;
  const AMaximum: Cardinal): Cardinal;
var
  I: Integer;
  Digit: Cardinal;
begin
  Result := 0;
  if AText = '' then
    raise Exception.Create(AName + ' requires unsigned decimal digits');
  for I := 1 to Length(AText) do
  begin
    if not (AText[I] in ['0'..'9']) then
      raise Exception.Create(AName + ' requires unsigned decimal digits');
    Digit := Ord(AText[I]) - Ord('0');
    if Result > (AMaximum - Digit) div 10 then
      raise Exception.Create(AName + ' exceeds its integer capacity');
    Result := Result * 10 + Digit;
  end;
end;

procedure PrintPlan(const APlan: TVoiceStudioFramePlan;
  const AOptions: TVoiceStudioStreamOptions);
begin
  WriteLn('Requested seconds: ', APlan.RequestedText,
    '; actual seconds: ', VoiceStudioSecondsText(APlan.ActualTicks));
  WriteLn('Requested ticks: ', APlan.RequestedTicks,
    '; actual ticks: ', APlan.ActualTicks, '; cells: ', APlan.CellCount,
    '; segment cells: ', AOptions.SegmentCellCount,
    '; seed: ', AOptions.Seed);
end;

function CountMidi(const APlan: TVoiceStudioFramePlan;
  const AOptions: TVoiceStudioStreamOptions): TVoiceStudioMidiPlan;
var
  Planner: TVoiceStudioMidiPlanner;
  Step: TWfcMusicArrangementStep;
  LastProgress: QWord;
begin
  Result := nil;
  Planner := TVoiceStudioMidiPlanner.Create(APlan, AOptions);
  try
    LastProgress := GetTickCount64;
    repeat
      if CancelRequested <> 0 then Planner.Cancel;
      Step := Planner.Next;
      case Step of
        wmaspProduced:
          if GetTickCount64 - LastProgress >= 2000 then
          begin
            WriteLn('Planning cells: ', Planner.FramesProcessed, '/',
              APlan.CellCount);
            Flush(Output);
            LastProgress := GetTickCount64;
          end;
        wmaspCompleted: Result := Planner.DetachPlan;
        wmaspCancelled:
          raise Exception.Create('MIDI planning cancelled; no output was opened');
        wmaspFailed:
          raise Exception.Create('MIDI planning failed: ' + Planner.Failure);
      end;
    until Step = wmaspCompleted;
  finally
    Planner.Free;
  end;
end;

procedure RenderWave(const ASeconds, AOutput: String;
  const AOptions: TVoiceStudioStreamOptions);
var
  FileTarget: TWfcAtomicNewFile;
  Plan: TVoiceStudioWavePlan;
  Samples: TWfcMusicPcm16Samples;
  Sink: TAtomicWaveSink;
  Source: TVoiceStudioPcmStream;
  Step: TWfcMusicArrangementStep;
  Wave: TWfcMusicWaveStream;
  LastProgress: QWord;
  ContainerName: String;
  FramePlan: TVoiceStudioFramePlan;
begin
  Plan := PlanVoiceStudioWave(ASeconds);
  FramePlan.RequestedText := Plan.RequestedText;
  FramePlan.RequestedTicks := Plan.RequestedTicks;
  FramePlan.ActualTicks := Plan.ActualTicks;
  FramePlan.CellCount := Plan.CellCount;
  if Plan.ExpectedFrames > (Int64(4294967295) - 36) div 2 then
    ContainerName := 'RF64' else ContainerName := 'RIFF';
  FileTarget := nil;
  Sink := nil;
  Source := nil;
  Wave := nil;
  try
    { Model/options validation completes before any destination is created. }
    Source := TVoiceStudioPcmStream.Create(Plan, AOptions);
    if CancelRequested <> 0 then
      raise Exception.Create('render cancelled before opening output');
    FileTarget := TWfcAtomicNewFile.Create(AOutput);
    Sink := TAtomicWaveSink.Create(FileTarget);
    Wave := TWfcMusicWaveStream.Create(Sink,
      VOICE_STUDIO_STREAM_SAMPLE_RATE, Plan.ExpectedFrames);
    PrintPlan(FramePlan, AOptions);
    WriteLn('Expected frames: ', Plan.ExpectedFrames,
      '; sample rate: ', VOICE_STUDIO_STREAM_SAMPLE_RATE,
      '; container: ', ContainerName);
    Flush(Output);
    LastProgress := GetTickCount64;
    repeat
      if CancelRequested <> 0 then Source.Cancel;
      Step := Source.NextSamples(Samples);
      case Step of
        wmaspProduced:
          begin
            if CancelRequested <> 0 then
            begin
              Source.Cancel;
              raise Exception.Create('render cancelled; output was not published');
            end;
            Wave.AppendSamples(Samples);
            if GetTickCount64 - LastProgress >= 2000 then
            begin
              WriteLn('Progress frames: ', Wave.FrameCount, '/',
                Plan.ExpectedFrames, '; segments: ', Source.SegmentsProduced);
              Flush(Output);
              LastProgress := GetTickCount64;
            end;
          end;
        wmaspCompleted: ;
        wmaspCancelled:
          raise Exception.Create('render cancelled; output was not published');
        wmaspFailed:
          raise Exception.Create('render failed: ' + Source.Failure);
      end;
    until Step = wmaspCompleted;
    if CancelRequested <> 0 then
      raise Exception.Create('render cancelled before publication');
    if (Source.EmittedFrames <> Plan.ExpectedFrames) or
        (Wave.FrameCount <> Plan.ExpectedFrames) then
      raise Exception.Create('PCM count differs from the preflight plan');
    Wave.Finish;
    FileTarget.Publish;
    WriteLn('Wrote ', FileTarget.OutputPath);
    WriteLn('Completed frames: ', Wave.FrameCount,
      '; segments: ', Source.SegmentsProduced,
      '; held seams: ', Source.SeamHoldCount,
      '; novel verticals: ', Source.NovelVerticalCount,
      '; shared-witness cells: ', Source.SharedCoverageCellCount);
  finally
    Wave.Free;
    Sink.Free;
    if FileTarget <> nil then
    begin
      FileTarget.Cancel;
      if FileTarget.CleanupError <> '' then
        WriteLn(StdErr, FileTarget.CleanupError);
    end;
    FileTarget.Free;
    Source.Free;
  end;
end;

procedure RenderMidi(const ASeconds, AOutput: String;
  const AOptions: TVoiceStudioStreamOptions);
var
  Block: TWfcMidiBytes;
  FileTarget: TWfcAtomicNewFile;
  FramePlan: TVoiceStudioFramePlan;
  Plan: TVoiceStudioMidiPlan;
  Replay: TVoiceStudioMidiStream;
  Step: TWfcMusicArrangementStep;
  LastProgress: QWord;
begin
  FramePlan := PlanVoiceStudioFrames(ASeconds);
  PrintPlan(FramePlan, AOptions);
  Flush(Output);
  Plan := nil;
  Replay := nil;
  FileTarget := nil;
  try
    { The complete counting pass precedes the picker-equivalent file open. }
    Plan := CountMidi(FramePlan, AOptions);
    WriteLn('Planned bytes: ', Plan.FileByteCount,
      '; track bytes: ', Plan.TrackByteCount,
      '; events: ', Plan.EventCount,
      '; frames: ', Plan.FrameCount,
      '; MIDI signature: ', IntToHex(Plan.MidiSignature, 8),
      '; frame signature: ', IntToHex(Plan.FrameSignature, 8));
    Flush(Output);
    if CancelRequested <> 0 then
      raise Exception.Create('MIDI render cancelled before opening output');
    Replay := TVoiceStudioMidiStream.Create(Plan);
    FileTarget := TWfcAtomicNewFile.Create(AOutput);
    LastProgress := GetTickCount64;
    repeat
      if CancelRequested <> 0 then Replay.Cancel;
      Step := Replay.NextBytes(Block);
      case Step of
        wmaspProduced:
          begin
            if CancelRequested <> 0 then
            begin
              Replay.Cancel;
              raise Exception.Create(
                'MIDI render cancelled; output was not published');
            end;
            if Length(Block) > 0 then FileTarget.WriteBytes(Block);
            if GetTickCount64 - LastProgress >= 2000 then
            begin
              WriteLn('Replay bytes: ', FileTarget.ByteCount, '/',
                Plan.FileByteCount, '; cells: ', Replay.FramesProcessed);
              Flush(Output);
              LastProgress := GetTickCount64;
            end;
          end;
        wmaspCompleted: ;
        wmaspCancelled:
          raise Exception.Create('MIDI render cancelled; output was not published');
        wmaspFailed:
          raise Exception.Create('MIDI replay failed: ' + Replay.Failure);
      end;
    until Step = wmaspCompleted;
    if CancelRequested <> 0 then
      raise Exception.Create('MIDI render cancelled before publication');
    if (Replay.TickCount <> Plan.EndTick) or
        (Replay.EmittedBytes <> Plan.FileByteCount) or
        (FileTarget.ByteCount <> Plan.FileByteCount) then
      raise Exception.Create('MIDI replay differs from the counting plan');
    FileTarget.Publish;
    WriteLn('Wrote ', FileTarget.OutputPath);
    WriteLn('Completed bytes: ', FileTarget.ByteCount,
      '; segments: ', Plan.SegmentsProduced,
      '; held seams: ', Plan.SeamHoldCount,
      '; novel verticals: ', Plan.NovelVerticalCount,
      '; shared-witness cells: ', Plan.SharedCoverageCellCount);
  finally
    if FileTarget <> nil then
    begin
      FileTarget.Cancel;
      if FileTarget.CleanupError <> '' then
        WriteLn(StdErr, FileTarget.CleanupError);
    end;
    FileTarget.Free;
    Replay.Free;
    Plan.Free;
  end;
end;

procedure Usage;
begin
  WriteLn('VoiceStudioRender --format wave|midi --seconds DURATION --output NEW-PATH [options]');
  WriteLn('  --seed UINT32          unsigned decimal replay seed (default 1)');
  WriteLn('  --segment-cells N      positive local generation horizon (default 5)');
  WriteLn('  --backtracks N         local search allowance (default 1024)');
  WriteLn('  --pass-backtracks N    negotiated pass allowance (default 64)');
  WriteLn('  --trace                 capture final segment search trace metadata');
  WriteLn('Duration is positive decimal seconds and rounds upward to 0.25-second cells.');
  WriteLn('WAVE is mono PCM16 at 44100 Hz. MIDI is format 0, TPQ 480, channels 0..2.');
  WriteLn('There is no duration policy cap; exact transport capacities are checked.');
  WriteLn('Existing paths are never replaced. Ctrl+C requests cooperative cancellation.');
end;

procedure Main;
var
  FormatName, OptionName, OutputPath, Seconds, Value: String;
  Options: TVoiceStudioStreamOptions;
  I, Kind: Integer;
  Seen: array[0..7] of Boolean;
begin
  if (ParamCount = 1) and (ParamStr(1) = '--version') then
  begin
    WriteLn('VoiceStudioRender 1');
    Exit;
  end;
  if (ParamCount = 1) and
      ((ParamStr(1) = '--help') or (ParamStr(1) = '-h')) then
  begin
    Usage;
    Exit;
  end;
  if (ParamCount = 1) and (ParamStr(1) = '--selftest') then
  begin
    WriteLn('Voice Studio self-test passed: ', VoiceStudioSelfTest);
    Exit;
  end;
  Options := DefaultVoiceStudioStreamOptions;
  for I := 0 to High(Seen) do Seen[I] := False;
  FormatName := '';
  Seconds := '';
  OutputPath := '';
  I := 1;
  while I <= ParamCount do
  begin
    OptionName := ParamStr(I);
    if OptionName = '--format' then Kind := 0
    else if OptionName = '--seconds' then Kind := 1
    else if OptionName = '--output' then Kind := 2
    else if OptionName = '--seed' then Kind := 3
    else if OptionName = '--segment-cells' then Kind := 4
    else if OptionName = '--backtracks' then Kind := 5
    else if OptionName = '--pass-backtracks' then Kind := 6
    else if OptionName = '--trace' then Kind := 7
    else raise Exception.Create('unknown option: ' + OptionName);
    if Seen[Kind] then raise Exception.Create('duplicate ' + OptionName);
    Seen[Kind] := True;
    Inc(I);
    if Kind = 7 then
    begin
      Options.CaptureTrace := True;
      Continue;
    end;
    if I > ParamCount then
      raise Exception.Create('missing value for ' + OptionName);
    Value := ParamStr(I);
    Inc(I);
    case Kind of
      0: FormatName := LowerCase(Value);
      1: Seconds := Value;
      2: OutputPath := Value;
      3: Options.Seed := ParseUnsigned(Value, OptionName, High(Cardinal));
      4: Options.SegmentCellCount := Integer(ParseUnsigned(Value, OptionName,
        High(Integer) div VOICE_STUDIO_QUANTUM));
      5: Options.MaxBacktracks := Integer(ParseUnsigned(Value, OptionName,
        High(Integer)));
      6: Options.MaxPassBacktracks := Integer(ParseUnsigned(Value, OptionName,
        High(Integer)));
    end;
  end;
  if not Seen[0] or not Seen[1] or not Seen[2] then
    raise Exception.Create(
      '--format, --seconds, and --output are required; see --help');
  if FormatName = 'wave' then RenderWave(Seconds, OutputPath, Options)
  else if FormatName = 'midi' then RenderMidi(Seconds, OutputPath, Options)
  else raise Exception.Create('--format must be wave or midi');
end;

begin
  {$IFDEF MSWINDOWS}
  SetConsoleCtrlHandler(@ConsoleControl, True);
  {$ELSE}
  fpSignal(SIGINT, @InterruptSignal);
  fpSignal(SIGTERM, @InterruptSignal);
  {$ENDIF}
  try
    Main;
  except
    on E: Exception do
    begin
      WriteLn(StdErr, 'VoiceStudioRender: ', E.Message);
      ExitCode := 1;
    end;
  end;
end.
