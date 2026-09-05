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
program EnsembleStudioMidiRender;

{$mode delphi}{$H+}

uses
  SysUtils,
  wfc,
  wfc_midi_smf,
  wfc_music_arrangement,
  ensemble_studio_stream,
  ensemble_studio_midi_stream,
  wfc_atomic_new_file
  {$IFDEF MSWINDOWS}, Windows{$ELSE}, BaseUnix{$ENDIF};

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

function ParseUnsigned(const AText, AName: String;
  const AMaximum: Cardinal): Cardinal;
var
  I: Integer;
  LDigit: Cardinal;
begin
  Result := 0;
  if AText = '' then
    raise Exception.Create(AName + ' requires unsigned decimal digits');
  for I := 1 to Length(AText) do
  begin
    if not (AText[I] in ['0'..'9']) then
      raise Exception.Create(AName + ' requires unsigned decimal digits');
    LDigit := Ord(AText[I]) - Ord('0');
    if Result > (AMaximum - LDigit) div 10 then
      raise Exception.Create(AName + ' exceeds its integer capacity');
    Result := Result * 10 + LDigit;
  end;
end;

function BuildPlan(const AFramePlan: TEnsembleStudioFramePlan;
  const AOptions: TEnsembleStudioStreamOptions): TEnsembleStudioMidiPlan;
var
  LPlanner: TEnsembleStudioMidiPlanner;
  LStep: TWfcMusicArrangementStep;
  LLastProgress: QWord;
begin
  Result := nil;
  LPlanner := TEnsembleStudioMidiPlanner.Create(AFramePlan, AOptions);
  try
    LLastProgress := GetTickCount64;
    repeat
      if CancelRequested <> 0 then LPlanner.Cancel;
      LStep := LPlanner.Next;
      case LStep of
        wmaspProduced:
          if GetTickCount64 - LLastProgress >= 2000 then
          begin
            WriteLn('Planning frames: ', LPlanner.FramesProcessed, '/',
              AFramePlan.CellCount);
            Flush(Output);
            LLastProgress := GetTickCount64;
          end;
        wmaspCompleted: Result := LPlanner.DetachPlan;
        wmaspCancelled:
          raise Exception.Create('MIDI planning cancelled; no output was opened');
        wmaspFailed:
          raise Exception.Create('MIDI planning failed: ' + LPlanner.Failure);
      end;
    until LStep = wmaspCompleted;
  finally
    LPlanner.Free;
  end;
end;

procedure Render(const ASeconds, AOutput: String;
  const AOptions: TEnsembleStudioStreamOptions);
var
  LBytes: TWfcMidiBytes;
  LFile: TWfcAtomicNewFile;
  LFramePlan: TEnsembleStudioFramePlan;
  LPlan: TEnsembleStudioMidiPlan;
  LReplay: TEnsembleStudioMidiStream;
  LStep: TWfcMusicArrangementStep;
  LLastProgress: QWord;
begin
  { The entire deterministic counting pass precedes creation of a destination
    or owned sibling. It retains configuration and counts, never frames. }
  LFramePlan := PlanEnsembleStudioFrames(ASeconds);
  WriteLn('Requested seconds: ', LFramePlan.RequestedText,
    '; actual seconds: ', EnsembleStudioStreamSecondsText(LFramePlan.ActualTicks));
  WriteLn('Requested ticks: ', LFramePlan.RequestedTicks,
    '; actual ticks: ', LFramePlan.ActualTicks, '; cells: ', LFramePlan.CellCount,
    '; seed: ', AOptions.Seed);
  Flush(Output);
  LPlan := nil;
  LReplay := nil;
  LFile := nil;
  try
    LPlan := BuildPlan(LFramePlan, AOptions);
    WriteLn('Planned bytes: ', LPlan.FileByteCount, '; track bytes: ',
      LPlan.TrackByteCount, '; events: ', LPlan.EventCount,
      '; delay bridges: ', LPlan.BridgeCount, '; frames: ', LPlan.FrameCount,
      '; MIDI signature: ', IntToHex(LPlan.MidiSignature, 8),
      '; frame signature: ', IntToHex(LPlan.FrameSignature, 8));
    Flush(Output);
    if CancelRequested <> 0 then
      raise Exception.Create('MIDI render cancelled before opening output');

    LReplay := TEnsembleStudioMidiStream.Create(LPlan);
    LFile := TWfcAtomicNewFile.Create(AOutput);
    LLastProgress := GetTickCount64;
    repeat
      if CancelRequested <> 0 then LReplay.Cancel;
      LStep := LReplay.NextBytes(LBytes);
      case LStep of
        wmaspProduced:
          begin
            if CancelRequested <> 0 then
            begin
              LReplay.Cancel;
              raise Exception.Create(
                'MIDI render cancelled; output was not published');
            end;
            LFile.WriteBytes(LBytes);
            if GetTickCount64 - LLastProgress >= 2000 then
            begin
              WriteLn('Replay bytes: ', LFile.ByteCount, '/',
                LPlan.FileByteCount, '; frames: ', LReplay.FramesProcessed);
              Flush(Output);
              LLastProgress := GetTickCount64;
            end;
          end;
        wmaspCompleted: ;
        wmaspCancelled:
          raise Exception.Create('MIDI render cancelled; output was not published');
        wmaspFailed:
          raise Exception.Create('MIDI replay failed: ' + LReplay.Failure);
      end;
    until LStep = wmaspCompleted;
    if CancelRequested <> 0 then
      raise Exception.Create('MIDI render cancelled before publication');
    if (LReplay.TickCount <> LPlan.EndTick) or
        (LReplay.EmittedBytes <> LPlan.FileByteCount) or
        (LFile.ByteCount <> LPlan.FileByteCount) then
      raise Exception.Create('MIDI replay differs from the exact preflight plan');
    LFile.Publish;
    WriteLn('Wrote ', LFile.OutputPath);
    WriteLn('Completed bytes: ', LFile.ByteCount, '; end tick: ',
      LReplay.TickCount, '; frames: ', LReplay.FramesProcessed,
      '; segments: ', LPlan.SegmentsProduced, '; held seams: ',
      LPlan.SeamHoldCount);
    if AOptions.CaptureTrace then
      WriteLn('Last segment signature: ',
        IntToHex(LPlan.LastSegmentSignature, 8), '; transcript: ',
        IntToHex(LPlan.LastTranscriptHash, 8));
  finally
    if LFile <> nil then
    begin
      LFile.Cancel;
      if LFile.CleanupError <> '' then WriteLn(StdErr, LFile.CleanupError);
    end;
    LFile.Free;
    LReplay.Free;
    LPlan.Free;
  end;
end;

procedure Usage;
begin
  WriteLn('EnsembleStudioMidiRender --seconds DURATION --output NEW-MIDI-PATH [options]');
  WriteLn('  --seed UINT32          unsigned decimal replay seed (default 0)');
  WriteLn('  --segment-cells N      positive local generation horizon (default 5)');
  WriteLn('  --backtracks N         local search allowance (default 256)');
  WriteLn('  --pass-backtracks N    negotiated pass allowance (default 16)');
  WriteLn('  --trace                 capture and print final segment trace metadata');
  WriteLn('Duration is positive decimal seconds and rounds upward to quarter-second cells.');
  WriteLn('The planning pass finishes before any output is opened, then replay is verified.');
  WriteLn('The format-0 file uses channels 0,1,2, TPQ 480, 120 BPM, and 4/4 meter.');
  WriteLn('No song-length policy cap: exact tick and format capacities apply.');
  WriteLn('Existing paths are never replaced. Ctrl+C requests cooperative cancellation.');
end;

procedure Main;
var
  LOptions: TEnsembleStudioStreamOptions;
  LSeconds, LOutput, LOption, LValue: String;
  I, LKind: Integer;
  LSeen: array[0..6] of Boolean;
begin
  if (ParamCount = 1) and (ParamStr(1) = '--version') then
  begin
    WriteLn('EnsembleStudioMidiRender 1');
    Exit;
  end;
  if (ParamCount = 1) and
      ((ParamStr(1) = '--help') or (ParamStr(1) = '-h')) then
  begin
    Usage;
    Exit;
  end;
  LOptions := DefaultEnsembleStudioStreamOptions;
  for I := 0 to High(LSeen) do LSeen[I] := False;
  LSeconds := '';
  LOutput := '';
  I := 1;
  while I <= ParamCount do
  begin
    LOption := ParamStr(I);
    if LOption = '--seconds' then LKind := 0
    else if LOption = '--output' then LKind := 1
    else if LOption = '--seed' then LKind := 2
    else if LOption = '--segment-cells' then LKind := 3
    else if LOption = '--backtracks' then LKind := 4
    else if LOption = '--pass-backtracks' then LKind := 5
    else if LOption = '--trace' then LKind := 6
    else raise Exception.Create('unknown option: ' + LOption);
    if LSeen[LKind] then raise Exception.Create('duplicate ' + LOption);
    LSeen[LKind] := True;
    Inc(I);
    if LKind = 6 then
    begin
      LOptions.CaptureTrace := True;
      Continue;
    end;
    if I > ParamCount then
      raise Exception.Create('missing value for ' + LOption);
    LValue := ParamStr(I);
    Inc(I);
    case LKind of
      0: LSeconds := LValue;
      1: LOutput := LValue;
      2: LOptions.Seed := ParseUnsigned(LValue, LOption, High(Cardinal));
      3: LOptions.SegmentCellCount := Integer(ParseUnsigned(LValue, LOption,
        High(Integer) div ENSEMBLE_STUDIO_STREAM_QUANTUM));
      4: LOptions.MaxBacktracks := Integer(ParseUnsigned(LValue, LOption,
        High(Integer)));
      5: LOptions.MaxPassBacktracks := Integer(ParseUnsigned(LValue, LOption,
        High(Integer)));
    end;
  end;
  if not LSeen[0] or not LSeen[1] then
    raise Exception.Create('--seconds and --output are required; see --help');
  Render(LSeconds, LOutput, LOptions);
end;

begin
  {$IFDEF MSWINDOWS}SetConsoleCtrlHandler(@ConsoleControl, True);
  {$ELSE}fpSignal(SIGINT, @InterruptSignal); fpSignal(SIGTERM, @InterruptSignal);{$ENDIF}
  try
    Main;
  except
    on E: Exception do
    begin
      WriteLn(StdErr, 'EnsembleStudioMidiRender: ', E.Message);
      ExitCode := 1;
    end;
  end;
end.
