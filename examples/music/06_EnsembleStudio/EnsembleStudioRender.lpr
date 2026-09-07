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
program EnsembleStudioRender;

{$mode delphi}{$H+}

uses
  SysUtils, wfc, wfc_music_audio, wfc_music_audio_stream,
  wfc_music_arrangement, ensemble_studio_profiles, ensemble_studio_stream, wfc_atomic_new_file
  {$IFDEF MSWINDOWS}, Windows{$ELSE}, BaseUnix{$ENDIF};

type
  TAtomicWaveSink = class(TWfcMusicAudioByteSink)
  private
    FFile: TWfcAtomicNewFile;
  public
    constructor Create(const AFile: TWfcAtomicNewFile);
    procedure WriteBytes(const ABytes: array of Byte); override;
  end;

var CancelRequested: LongInt;

{$IFDEF MSWINDOWS}
function ConsoleControl(const AEvent: DWORD): BOOL; stdcall;
begin
  Result := (AEvent = CTRL_C_EVENT) or (AEvent = CTRL_BREAK_EVENT);
  if Result then InterlockedExchange(CancelRequested, 1);
end;
{$ELSE}
procedure InterruptSignal(const ASignal: cint); cdecl;
begin
  { Signal handlers do not allocate, print, close files or call the generator.
    The ordinary pull loop observes this cooperative cancellation request. }
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
var I: Integer; LDigit: Cardinal;
begin
  Result := 0;
  if AText = '' then raise Exception.Create(AName + ' requires unsigned decimal digits');
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

procedure Render(const ASeconds, AOutput: String;
  const AOptions: TEnsembleStudioStreamOptions);
var
  LPlan: TEnsembleStudioStreamPlan;
  LSource: TEnsembleStudioPcmStream;
  LFile: TWfcAtomicNewFile;
  LSink: TAtomicWaveSink;
  LWave: TWfcMusicWaveStream;
  LSamples: TWfcMusicPcm16Samples;
  LStep: TWfcMusicArrangementStep;
  LLastProgress: QWord;
  LContainer: String;
begin
  { Parse exact decimal duration, quantization, file-size envelope, and source
    options before creating any output or temporary file. }
  LPlan := PlanEnsembleStudioStream(ASeconds);
  if LPlan.ExpectedFrames > (Int64(4294967295) - 36) div 2 then
    LContainer := 'RF64'
  else LContainer := 'RIFF';
  LSource := nil; LFile := nil; LSink := nil; LWave := nil;
  try
    LSource := TEnsembleStudioPcmStream.Create(LPlan, AOptions);
    if CancelRequested <> 0 then
      raise Exception.Create('render cancelled before opening output');
    LFile := TWfcAtomicNewFile.Create(AOutput);
    LSink := TAtomicWaveSink.Create(LFile);
    LWave := TWfcMusicWaveStream.Create(LSink,
      ENSEMBLE_STUDIO_STREAM_SAMPLE_RATE, LPlan.ExpectedFrames);
    WriteLn('Requested seconds: ', LPlan.RequestedText,
      '; actual seconds: ', EnsembleStudioStreamSecondsText(LPlan.ActualTicks));
    WriteLn('Requested ticks: ', LPlan.RequestedTicks,
      '; actual ticks: ', LPlan.ActualTicks, '; cells: ', LPlan.CellCount);
    WriteLn('Expected frames: ', LPlan.ExpectedFrames, '; sample rate: ',
      ENSEMBLE_STUDIO_STREAM_SAMPLE_RATE, '; container: ', LContainer,
      '; seed: ', AOptions.Seed);
    Flush(Output);
    LLastProgress := GetTickCount64;
    repeat
      if CancelRequested <> 0 then LSource.Cancel;
      LStep := LSource.NextSamples(LSamples);
      case LStep of
        wmaspProduced:
          begin
            if CancelRequested <> 0 then
            begin
              LSource.Cancel;
              raise Exception.Create('render cancelled; output was not published');
            end;
            LWave.AppendSamples(LSamples);
            if GetTickCount64 - LLastProgress >= 2000 then
            begin
              WriteLn('Progress frames: ', LWave.FrameCount, '/',
                LPlan.ExpectedFrames, '; segments: ', LSource.SegmentsProduced);
              Flush(Output);
              LLastProgress := GetTickCount64;
            end;
          end;
        wmaspCompleted: Break;
        wmaspCancelled:
          raise Exception.Create('render cancelled; output was not published');
        wmaspFailed:
          raise Exception.Create('render failed: ' + LSource.Failure);
      end;
    until False;
    if CancelRequested <> 0 then
      raise Exception.Create('render cancelled before publication');
    if (LSource.EmittedFrames <> LPlan.ExpectedFrames) or
      (LWave.FrameCount <> LPlan.ExpectedFrames) then
      raise Exception.Create('PCM count differs from the exact preflight plan');
    LWave.Finish;
    LFile.Publish;
    WriteLn('Wrote ', LFile.OutputPath);
    WriteLn('Completed frames: ', LWave.FrameCount, '; actual ticks: ',
      LPlan.ActualTicks, '; segments: ', LSource.SegmentsProduced,
      '; held seams: ', LSource.SeamHoldCount, '; RF64: ',
      BoolToStr(LWave.IsRF64, True));
    if AOptions.CaptureTrace then
      WriteLn('Last segment signature: ', IntToHex(LSource.LastSegmentSignature, 8),
        '; negotiation status: ', Ord(LSource.LastNegotiationStatus),
        '; transcript: ', IntToHex(LSource.LastTranscriptHash, 8));
  finally
    LWave.Free;
    LSink.Free;
    if LFile <> nil then
    begin
      { Explicit cleanup allows a diagnostic without masking the original
        generation/I/O error. The destructor retries only the owned sibling. }
      LFile.Cancel;
      if LFile.CleanupError <> '' then WriteLn(StdErr, LFile.CleanupError);
    end;
    LFile.Free;
    LSource.Free;
  end;
end;

procedure Usage;
begin
  WriteLn('EnsembleStudioRender --seconds DURATION --output NEW-WAVE-PATH [options]');
  WriteLn('  --seed UINT32          unsigned decimal replay seed (default 0)');
  WriteLn('  --profile NAME         structural-v1 or developed-period-v1');
  WriteLn('  --segment-cells N      positive local generation horizon (default 5)');
  WriteLn('  --backtracks N         local search allowance (default 256)');
  WriteLn('  --pass-backtracks N    negotiated pass allowance (default 16)');
  WriteLn('  --trace               capture and print final segment trace metadata');
  WriteLn('Search/trace options apply to acoustic segments; developed form uses 256/64 backtracks per phrase, trace off.');
  WriteLn('Duration is positive decimal seconds; supplied fractional precision is retained.');
  WriteLn('It rounds upward to quarter-second cells at 120 BPM, not whole bars.');
  WriteLn('PCM is mono 44100 Hz; sustained voices remain continuous between segments.');
  WriteLn('No song-length policy cap: exact tick/frame arithmetic and file capacity apply.');
  WriteLn('Existing paths are never replaced. Ctrl+C requests cooperative cancellation.');
end;

procedure Main;
var
  LOptions: TEnsembleStudioStreamOptions;
  LSeconds, LOutput, LOption, LValue: String;
  I: Integer;
  LSeen: array[0..7] of Boolean;
  LKind: Integer;
begin
  if (ParamCount = 1) and (ParamStr(1) = '--version') then
  begin WriteLn('EnsembleStudioRender 1'); Exit; end;
  if (ParamCount = 1) and ((ParamStr(1) = '--help') or (ParamStr(1) = '-h')) then
  begin Usage; Exit; end;
  LOptions := DefaultEnsembleStudioStreamOptions;
  for I := 0 to High(LSeen) do LSeen[I] := False;
  LSeconds := ''; LOutput := ''; I := 1;
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
    else if LOption = '--profile' then LKind := 7
    else raise Exception.Create('unknown option: ' + LOption);
    if LSeen[LKind] then raise Exception.Create('duplicate ' + LOption);
    LSeen[LKind] := True;
    Inc(I);
    if LKind = 6 then begin LOptions.CaptureTrace := True; Continue; end;
    if I > ParamCount then raise Exception.Create('missing value for ' + LOption);
    LValue := ParamStr(I); Inc(I);
    case LKind of
      0: LSeconds := LValue;
      1: LOutput := LValue;
      2: LOptions.Seed := ParseUnsigned(LValue, LOption, High(Cardinal));
      3: LOptions.SegmentCellCount := Integer(ParseUnsigned(LValue, LOption,
        High(Integer) div ENSEMBLE_STUDIO_STREAM_QUANTUM));
      4: LOptions.MaxBacktracks := Integer(ParseUnsigned(LValue, LOption, High(Integer)));
      5: LOptions.MaxPassBacktracks := Integer(ParseUnsigned(LValue, LOption, High(Integer)));
      7: LOptions.Profile := ParseEnsembleStudioProfile(LValue);
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
      WriteLn(StdErr, 'EnsembleStudioRender: ', E.Message);
      ExitCode := 1;
    end;
  end;
end.
