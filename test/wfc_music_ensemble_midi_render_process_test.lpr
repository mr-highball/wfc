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
program wfc_music_ensemble_midi_render_process_test;

{$mode delphi}{$H+}

uses
  Classes,
  SysUtils,
  Process, wfc_process_test_support,
  wfc_midi_smf,
  wfc_midi_stream,
  wfc_atomic_new_file;

var
  Checks: Integer;

procedure Check(const ACondition: Boolean; const AMessage: String);
begin
  Inc(Checks);
  if not ACondition then raise Exception.Create(AMessage);
end;

function StartChild(const AExecutable: String;
  const AArguments: array of String): TProcess;
var
  I: Integer;
begin
  Result := TProcess.Create(nil);
  try
    Result.Executable := ExpandFileName(AExecutable);
    for I := 0 to High(AArguments) do Result.Parameters.Add(AArguments[I]);
    Result.Options := [poUsePipes, poStderrToOutPut, poNoConsole];
    Result.Execute;
  except
    Result.Free;
    raise;
  end;
end;

procedure ReadAvailable(const AProcess: TProcess; var AText: String);
var
  LBuffer: array[0..4095] of Byte;
  I, LCount, LOffset: Integer;
begin
  while AProcess.Output.NumBytesAvailable > 0 do
  begin
    LCount := AProcess.Output.NumBytesAvailable;
    if LCount > SizeOf(LBuffer) then LCount := SizeOf(LBuffer);
    LCount := AProcess.Output.Read(LBuffer[0], LCount);
    if LCount <= 0 then Exit;
    if Length(AText) > 1048576 - LCount then
      raise Exception.Create('child output exceeds the test capture bound');
    LOffset := Length(AText);
    SetLength(AText, LOffset + LCount);
    for I := 0 to LCount - 1 do
      AText[LOffset + I + 1] := Chr(LBuffer[I]);
  end;
end;

function FinishChild(const AProcess: TProcess; var AText: String): Integer;
var
  LStart: QWord;
begin
  LStart := GetTickCount64;
  repeat
    ReadAvailable(AProcess, AText);
    if not AProcess.Running then Break;
    if GetTickCount64 - LStart > 240000 then
      raise Exception.Create('MIDI renderer process-test deadline exceeded');
    Sleep(5);
  until False;
  ReadAvailable(AProcess, AText);
  Result := WfcProcessExitCode(AProcess);
end;

procedure ReleaseChild(const AProcess: TProcess);
begin
  if AProcess = nil then Exit;
  if AProcess.Running then
  begin
    AProcess.Terminate(1);
    AProcess.WaitOnExit(5000);
  end;
  AProcess.Free;
end;

function Invoke(const AExecutable: String;
  const AArguments: array of String; const AExpectedExit: Integer): String;
var
  LActualExit: Integer;
  LProcess: TProcess;
begin
  Result := '';
  LProcess := StartChild(AExecutable, AArguments);
  try
    LActualExit := FinishChild(LProcess, Result);
    if LActualExit <> AExpectedExit then
      raise Exception.CreateFmt('expected exit %d, got %d: %s',
        [AExpectedExit, LActualExit, Result]);
    Check(True, 'child exit status');
  finally
    ReleaseChild(LProcess);
  end;
end;

function HasPartial(const AOutput: String): Boolean;
var
  LSearch: TSearchRec;
begin
  Result := FindFirst(AOutput + '.partial-*', faAnyFile, LSearch) = 0;
  if Result then FindClose(LSearch);
end;

procedure WriteNewText(const APath, AText: String);
var
  I: Integer;
  LBytes: array of Byte;
  LFile: TWfcAtomicNewFile;
begin
  SetLength(LBytes, Length(AText));
  for I := 1 to Length(AText) do LBytes[I - 1] := Ord(AText[I]);
  LFile := TWfcAtomicNewFile.Create(APath);
  try
    LFile.WriteBytes(LBytes);
    LFile.Publish;
  finally
    LFile.Free;
  end;
end;

function ReadSmallText(const APath: String): String;
var
  LFile: TFileStream;
begin
  LFile := TFileStream.Create(APath, fmOpenRead or fmShareDenyNone);
  try
    Check(LFile.Size <= 1024, 'small text fixture bound');
    SetLength(Result, LFile.Size);
    if Result <> '' then LFile.ReadBuffer(Result[1], Length(Result));
  finally
    LFile.Free;
  end;
end;

function ReadMidiBytes(const APath: String): TWfcMidiBytes;
var
  LFile: TFileStream;
begin
  Result := nil;
  LFile := TFileStream.Create(APath, fmOpenRead or fmShareDenyNone);
  try
    Check((LFile.Size >= WFC_MIDI_STREAM_HEADER_BYTES) and
      (LFile.Size <= 16 * 1024 * 1024), 'MIDI fixture file bound');
    SetLength(Result, LFile.Size);
    LFile.ReadBuffer(Result[0], Length(Result));
  finally
    LFile.Free;
  end;
end;

procedure HashByte(var AHash: Cardinal; const AByte: Byte);
{$PUSH}{$Q-}
var
  LValue: Cardinal;
begin
  LValue := AHash xor Cardinal(AByte);
  AHash := (LValue + (LValue shl 1) + (LValue shl 4) +
    (LValue shl 7) + (LValue shl 8) + (LValue shl 24)) and
    Cardinal($FFFFFFFF);
end;
{$POP}

function ByteSignature(const ABytes: TWfcMidiBytes): Cardinal;
var
  I: Integer;
begin
  Result := Cardinal($811C9DC5);
  for I := 0 to High(ABytes) do HashByte(Result, ABytes[I]);
end;

procedure CompareFiles(const AFirst, ASecond: String);
var
  I: Integer;
  LFirst, LSecond: TWfcMidiBytes;
begin
  LFirst := ReadMidiBytes(AFirst);
  LSecond := ReadMidiBytes(ASecond);
  Check(Length(LFirst) = Length(LSecond), 'deterministic MIDI file lengths');
  for I := 0 to High(LFirst) do
    if LFirst[I] <> LSecond[I] then
      raise Exception.Create('deterministic MIDI byte mismatch');
  Check(True, 'deterministic MIDI files match every byte');
end;

procedure VerifyContinuedMidi(const APath: String);
var
  I: Integer;
  LAbsolute: Cardinal;
  LBassActive, LBassCrossedFirstSeam: Boolean;
  LBytes: TWfcMidiBytes;
  LEvent: TWfcMidiEvent;
  LFile: TWfcMidiFile;
begin
  LBytes := ReadMidiBytes(APath);
  Check((Length(LBytes) = 310) and
    (ByteSignature(LBytes) = Cardinal($9A9C3708)),
    'native streamed MIDI matches the portable replay golden');
  LFile := DecodeWfcMidiFile(LBytes);
  Check((LFile.Format = 0) and (LFile.TicksPerQuarter = 480) and
    (Length(LFile.Tracks) = 1),
    'independent decoder sees format 0, TPQ 480, and one track');
  LAbsolute := 0;
  LBassActive := False;
  LBassCrossedFirstSeam := False;
  for I := 0 to High(LFile.Tracks[0].Events) do
  begin
    LEvent := LFile.Tracks[0].Events[I];
    Inc(LAbsolute, LEvent.DeltaTicks);
    if (LAbsolute >= 1200) and LBassActive then
      LBassCrossedFirstSeam := True;
    if (LEvent.Status and $F0 = $90) and
        (LEvent.Status and $0F = 0) and (Length(LEvent.Data) = 2) then
    begin
      if LEvent.Data[1] = 0 then LBassActive := False
      else LBassActive := True;
    end
    else if (LEvent.Status and $F0 = $80) and
        (LEvent.Status and $0F = 0) then
      LBassActive := False;
  end;
  Inc(LAbsolute, LFile.Tracks[0].EndDeltaTicks);
  Check(LAbsolute = 5040, 'decoded deltas reach the exact planned end tick');
  Check(LBassCrossedFirstSeam,
    'a bass note remains active across the five-cell generation seam');
end;

procedure TestArguments(const AExecutable, ABase: String);
const
  BAD_SECONDS: array[0..7] of String =
    ('0', '-1', '1e3', '1x', '1.', '.5', '1..2',
     '99999999999999999999');
var
  I: Integer;
  LOutput, LText: String;
begin
  LText := Invoke(AExecutable, ['--version'], 0);
  Check(Pos('EnsembleStudioMidiRender 1', LText) > 0,
    'standalone version');
  LText := Invoke(AExecutable, ['--help'], 0);
  Check((Pos('planning pass', LText) > 0) and
    (Pos('--segment-cells', LText) > 0),
    'help exposes two-pass output and local horizon');
  LOutput := ABase + '/invalid.mid';
  Invoke(AExecutable, [], 1);
  Invoke(AExecutable, ['--seconds'], 1);
  Invoke(AExecutable, ['--unknown', '1'], 1);
  Invoke(AExecutable,
    ['--seconds', '1', '--seconds', '1', '--output', LOutput], 1);
  Invoke(AExecutable,
    ['--seconds', '1', '--output', LOutput, '--trace', '--trace'], 1);
  for I := 0 to High(BAD_SECONDS) do
  begin
    Invoke(AExecutable,
      ['--seconds', BAD_SECONDS[I], '--output', LOutput], 1);
    Check(not FileExists(LOutput) and not HasPartial(LOutput),
      'invalid duration creates no output: ' + BAD_SECONDS[I]);
  end;
  Invoke(AExecutable,
    ['--seconds', '1', '--output', LOutput, '--seed', '4294967296'], 1);
  Invoke(AExecutable,
    ['--seconds', '1', '--output', LOutput, '--seed', '1x'], 1);
  Invoke(AExecutable,
    ['--seconds', '1', '--output', LOutput, '--segment-cells', '0'], 1);
  Invoke(AExecutable,
    ['--seconds', '1', '--output', LOutput,
     '--segment-cells', '8947849'], 1);
  Invoke(AExecutable,
    ['--seconds', '1', '--output', LOutput,
     '--backtracks', '2147483648'], 1);
  Invoke(AExecutable,
    ['--seconds', '1', '--output', LOutput,
     '--pass-backtracks', '-1'], 1);
  Invoke(AExecutable,
    ['--seconds', '1', '--output', ABase + '/missing/target.mid'], 1);
  Check(not FileExists(LOutput) and not HasPartial(LOutput),
    'malformed options never create a partial');
end;

procedure TestRendering(const AExecutable, ABase: String);
var
  LExisting, LText: String;
begin
  LText := Invoke(AExecutable,
    ['--seconds', '5.25', '--seed', '0', '--segment-cells', '5',
     '--trace', '--output', ABase + '/continued.mid'], 0);
  Check((Pos('Planned bytes: 310', LText) > 0) and
    (Pos('events: 67', LText) > 0) and
    (Pos('held seams: 5', LText) > 0) and
    (Pos('Last segment signature: 433EDED7', LText) > 0),
    'host reports exact plan, continuation, and trace metadata');
  VerifyContinuedMidi(ABase + '/continued.mid');
  Invoke(AExecutable,
    ['--seconds', '5.25', '--seed', '0', '--segment-cells', '5',
     '--output', ABase + '/repeat.mid'], 0);
  CompareFiles(ABase + '/continued.mid', ABase + '/repeat.mid');

  LText := Invoke(AExecutable,
    ['--seconds', '0.251', '--output', ABase + '/rounded.mid'], 0);
  Check((Pos('Requested ticks: 241; actual ticks: 480; cells: 2', LText) > 0) and
    (Pos('end tick: 480; frames: 2', LText) > 0),
    'native host reports exact decimal-to-quantum rounding');

  LExisting := ABase + '/existing.mid';
  WriteNewText(LExisting, 'preserve existing target');
  Invoke(AExecutable,
    ['--seconds', '1.5', '--output', LExisting], 1);
  Check((ReadSmallText(LExisting) = 'preserve existing target') and
    not HasPartial(LExisting),
    'existing destination remains byte-exact and has no partial sibling');

  LText := Invoke(AExecutable,
    ['--seconds', '17.25', '--seed', '0', '--segment-cells', '7',
     '--output', ABase + '/long.mid'], 0);
  Check((Pos('actual seconds: 17.25', LText) > 0) and
    (Pos('actual ticks: 16560; cells: 69', LText) > 0),
    'MIDI transport continues beyond the authored 16-cell corpus');
  Check(DecodeWfcMidiFile(ReadMidiBytes(ABase + '/long.mid')).Format = 0,
    'long streamed output remains independently decodable');
end;

procedure Run(const AExecutable, AParent: String);
const
  OUTPUTS: array[0..5] of String =
    ('invalid.mid', 'continued.mid', 'repeat.mid', 'rounded.mid',
     'existing.mid', 'long.mid');
var
  I: Integer;
  LBase: String;
begin
  Check(FileExists(AExecutable), 'MIDI renderer executable exists');
  Check(DirectoryExists(AParent), 'test output parent exists');
  LBase := IncludeTrailingPathDelimiter(ExpandFileName(AParent)) +
    'ensemble-midi-render-test-' + IntToStr(GetProcessID) + '-' +
    IntToStr(GetTickCount64);
  if DirectoryExists(LBase) or not CreateDir(LBase) then
    raise Exception.Create('cannot create isolated process-test directory');
  try
    TestArguments(AExecutable, LBase);
    TestRendering(AExecutable, LBase);
  finally
    { Delete only the exact fixture files in this newly owned directory. }
    for I := 0 to High(OUTPUTS) do
      if FileExists(LBase + '/' + OUTPUTS[I]) then
        DeleteFile(LBase + '/' + OUTPUTS[I]);
    if not RemoveDir(LBase) then WriteLn('Retained fixture directory: ', LBase);
  end;
end;

begin
  try
    if ParamCount <> 2 then
      raise Exception.Create(
        'Usage: wfc_music_ensemble_midi_render_process_test RENDERER OUTPUT_PARENT');
    Run(ParamStr(1), ParamStr(2));
    WriteLn('Ensemble MIDI render process checks: ', Checks, '/', Checks);
  except
    on E: Exception do
    begin
      WriteLn(StdErr, E.Message);
      ExitCode := 1;
    end;
  end;
end.
