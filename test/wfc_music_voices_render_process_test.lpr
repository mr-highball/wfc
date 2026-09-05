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
program wfc_music_voices_render_process_test;

{$mode delphi}{$H+}

uses
  Classes,
  SysUtils,
  Process, wfc_process_test_support,
  wfc_midi_smf,
  wfc_midi_stream;

var
  Checks: Integer;

procedure Check(const ACondition: Boolean; const AMessage: String);
begin
  Inc(Checks);
  if not ACondition then raise Exception.Create(AMessage);
end;

function StartChild(const AExecutable: String;
  const AArguments: array of String): TProcess;
var I: Integer;
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
  Buffer: array[0..4095] of Byte;
  Count, I, Offset: Integer;
begin
  while AProcess.Output.NumBytesAvailable > 0 do
  begin
    Count := AProcess.Output.NumBytesAvailable;
    if Count > SizeOf(Buffer) then Count := SizeOf(Buffer);
    Count := AProcess.Output.Read(Buffer[0], Count);
    if Count <= 0 then Exit;
    if Length(AText) > 1048576 - Count then
      raise Exception.Create('child output exceeds the test capture bound');
    Offset := Length(AText);
    SetLength(AText, Offset + Count);
    for I := 0 to Count - 1 do AText[Offset + I + 1] := Chr(Buffer[I]);
  end;
end;

function FinishChild(const AProcess: TProcess; var AText: String): Integer;
var Start: QWord;
begin
  Start := GetTickCount64;
  repeat
    ReadAvailable(AProcess, AText);
    if not AProcess.Running then Break;
    if GetTickCount64 - Start > 240000 then
      raise Exception.Create('Voice Studio process-test deadline exceeded');
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
  ActualExit: Integer;
  Child: TProcess;
begin
  Result := '';
  Child := StartChild(AExecutable, AArguments);
  try
    ActualExit := FinishChild(Child, Result);
    if ActualExit <> AExpectedExit then
      raise Exception.CreateFmt('expected exit %d, got %d: %s',
        [AExpectedExit, ActualExit, Result]);
    Check(True, 'child exit status');
  finally
    ReleaseChild(Child);
  end;
end;

function HasPartial(const AOutput: String): Boolean;
var Search: TSearchRec;
begin
  Result := FindFirst(AOutput + '.partial-*', faAnyFile, Search) = 0;
  if Result then FindClose(Search);
end;

procedure WriteMarker(const APath, AText: String);
var FileData: TFileStream;
begin
  FileData := TFileStream.Create(APath, fmCreate);
  try
    if AText <> '' then FileData.WriteBuffer(AText[1], Length(AText));
  finally
    FileData.Free;
  end;
end;

function ReadMarker(const APath: String): String;
var FileData: TFileStream;
begin
  FileData := TFileStream.Create(APath, fmOpenRead or fmShareDenyNone);
  try
    Check(FileData.Size <= 1024, 'marker remains bounded');
    SetLength(Result, FileData.Size);
    if Result <> '' then FileData.ReadBuffer(Result[1], Length(Result));
  finally
    FileData.Free;
  end;
end;

procedure CompareFiles(const AFirst, ASecond: String);
var
  FirstFile, SecondFile: TFileStream;
  A, B: array[0..8191] of Byte;
  Count, I: Integer;
begin
  FirstFile := TFileStream.Create(AFirst, fmOpenRead or fmShareDenyNone);
  try
    SecondFile := TFileStream.Create(ASecond, fmOpenRead or fmShareDenyNone);
    try
      Check(FirstFile.Size = SecondFile.Size, 'deterministic file lengths');
      while FirstFile.Position < FirstFile.Size do
      begin
        Count := SizeOf(A);
        if FirstFile.Size - FirstFile.Position < Count then
          Count := FirstFile.Size - FirstFile.Position;
        FirstFile.ReadBuffer(A[0], Count);
        SecondFile.ReadBuffer(B[0], Count);
        for I := 0 to Count - 1 do
          if A[I] <> B[I] then
            raise Exception.Create('deterministic output byte mismatch');
      end;
      Check(True, 'deterministic files match every byte');
    finally
      SecondFile.Free;
    end;
  finally
    FirstFile.Free;
  end;
end;

procedure VerifyWave(const APath: String; const AFrames: Int64);
var
  FileData: TFileStream;
  Header: array[0..43] of Byte;
  DataBytes: Int64;
  function Little(const AOffset, ACount: Integer): Int64;
  var I: Integer;
  begin
    Result := 0;
    for I := ACount - 1 downto 0 do
      Result := Result * 256 + Header[AOffset + I];
  end;
  function Tag(const AOffset: Integer): String;
  var I: Integer;
  begin
    Result := '';
    for I := 0 to 3 do Result := Result + Chr(Header[AOffset + I]);
  end;
begin
  DataBytes := AFrames * 2;
  FileData := TFileStream.Create(APath, fmOpenRead or fmShareDenyNone);
  try
    Check(FileData.Size = 44 + DataBytes, 'complete WAVE physical extent');
    FileData.ReadBuffer(Header[0], SizeOf(Header));
    Check((Tag(0) = 'RIFF') and (Tag(8) = 'WAVE') and
      (Tag(36) = 'data'), 'canonical WAVE tags');
    Check((Little(20, 2) = 1) and (Little(22, 2) = 1) and
      (Little(24, 4) = 44100) and (Little(34, 2) = 16),
      'mono PCM16 44100 Hz header');
    Check(Little(40, 4) = DataBytes, 'exact PCM payload extent');
  finally
    FileData.Free;
  end;
end;

procedure VerifyMidi(const APath: String; const AEndTick: Cardinal);
var
  Bytes: TWfcMidiBytes;
  FileData: TFileStream;
  Midi: TWfcMidiFile;
  I: Integer;
  Tick: Cardinal;
begin
  FileData := TFileStream.Create(APath, fmOpenRead or fmShareDenyNone);
  try
    Check((FileData.Size >= WFC_MIDI_STREAM_HEADER_BYTES) and
      (FileData.Size <= 1024 * 1024), 'MIDI fixture extent');
    SetLength(Bytes, FileData.Size);
    FileData.ReadBuffer(Bytes[0], Length(Bytes));
  finally
    FileData.Free;
  end;
  Midi := DecodeWfcMidiFile(Bytes);
  Check((Midi.Format = 0) and (Midi.TicksPerQuarter = 480) and
    (Length(Midi.Tracks) = 1), 'format-0 MIDI time base');
  Tick := 0;
  for I := 0 to High(Midi.Tracks[0].Events) do
    Inc(Tick, Midi.Tracks[0].Events[I].DeltaTicks);
  Inc(Tick, Midi.Tracks[0].EndDeltaTicks);
  Check(Tick = AEndTick, 'decoded MIDI reaches planned end tick');
end;

procedure TestArguments(const AExecutable, ABase: String);
const
  BadSeconds: array[0..7] of String =
    ('0', '-1', '1e3', '1x', '1.', '.5', '1..2',
     '99999999999999999999');
var
  I: Integer;
  OutputPath, Text: String;
begin
  Text := Invoke(AExecutable, ['--version'], 0);
  Check(Pos('VoiceStudioRender 1', Text) > 0, 'version output');
  Text := Invoke(AExecutable, ['--help'], 0);
  Check((Pos('--format wave|midi', Text) > 0) and
    (Pos('--segment-cells', Text) > 0) and
    (Pos('Existing paths are never replaced', Text) > 0), 'help contract');
  Text := Invoke(AExecutable, ['--selftest'], 0);
  Check(Pos('self-test passed', Text) > 0, 'native shared self-test');
  Invoke(AExecutable, [], 1);
  Invoke(AExecutable, ['--format', 'wave'], 1);
  Invoke(AExecutable, ['--unknown'], 1);
  Invoke(AExecutable, ['--format', 'flac', '--seconds', '1', '--output',
    ABase + DirectorySeparator + 'bad.bin'], 1);
  Invoke(AExecutable, ['--format', 'wave', '--format', 'midi', '--seconds',
    '1', '--output', ABase + DirectorySeparator + 'duplicate.bin'], 1);
  Invoke(AExecutable, ['--format', 'wave', '--seconds', '1', '--output',
    ABase + DirectorySeparator + 'bad-seed.wav', '--seed', '2x'], 1);
  Invoke(AExecutable, ['--format', 'wave', '--seconds', '1', '--output',
    ABase + DirectorySeparator + 'bad-segment.wav', '--segment-cells', '0'], 1);
  Invoke(AExecutable, ['--format', 'midi', '--seconds', '1', '--output',
    ABase + DirectorySeparator + 'bad-budget.mid', '--backtracks',
    '2147483648'], 1);
  for I := 0 to High(BadSeconds) do
  begin
    OutputPath := ABase + DirectorySeparator + 'bad-' + IntToStr(I) + '.wav';
    Invoke(AExecutable, ['--format', 'wave', '--seconds', BadSeconds[I],
      '--output', OutputPath], 1);
    Check(not FileExists(OutputPath) and not HasPartial(OutputPath),
      'bad duration creates no output ' + IntToStr(I));
  end;
end;

procedure TestOutputs(const AExecutable, ABase: String);
var
  MidiA, MidiB, WaveA, WaveB, Existing, Failed: String;
  Text: String;
begin
  WaveA := ABase + DirectorySeparator + 'voices-a.wav';
  WaveB := ABase + DirectorySeparator + 'voices-b.wav';
  MidiA := ABase + DirectorySeparator + 'voices-a.mid';
  MidiB := ABase + DirectorySeparator + 'voices-b.mid';
  Text := Invoke(AExecutable, ['--format', 'wave', '--seconds', '5.25',
    '--output', WaveA], 0);
  Check((Pos('cells: 21', Text) > 0) and (Pos('held seams:', Text) > 0) and
    (Pos('novel verticals:', Text) > 0), 'WAVE progress and proof summary');
  Invoke(AExecutable, ['--format', 'wave', '--seconds', '5.25',
    '--output', WaveB], 0);
  VerifyWave(WaveA, 231525);
  CompareFiles(WaveA, WaveB);

  Text := Invoke(AExecutable, ['--format', 'midi', '--seconds', '5.25',
    '--output', MidiA], 0);
  Check((Pos('Planned bytes:', Text) > 0) and
    (Pos('shared-witness cells:', Text) > 0), 'MIDI count/replay summary');
  Invoke(AExecutable, ['--format', 'midi', '--seconds', '5.25',
    '--output', MidiB], 0);
  VerifyMidi(MidiA, 5040);
  CompareFiles(MidiA, MidiB);

  Existing := ABase + DirectorySeparator + 'existing.mid';
  WriteMarker(Existing, 'keep-this');
  Invoke(AExecutable, ['--format', 'midi', '--seconds', '1.5',
    '--output', Existing], 1);
  Check(ReadMarker(Existing) = 'keep-this', 'existing target is unchanged');
  Check(not HasPartial(Existing), 'existing-target refusal leaves no sibling');

  Failed := ABase + DirectorySeparator + 'failed.mid';
  Invoke(AExecutable, ['--format', 'midi', '--seconds', '1.5',
    '--output', Failed, '--seed', '0', '--pass-backtracks', '0'], 1);
  Check(not FileExists(Failed) and not HasPartial(Failed),
    'failed MIDI plan creates no output');
end;

procedure Main;
var
  Base, ExecutablePath: String;
begin
  if ParamCount <> 2 then
    raise Exception.Create(
      'usage: wfc_music_voices_render_process_test RENDERER ARTIFACT-DIRECTORY');
  ExecutablePath := ExpandFileName(ParamStr(1));
  Base := ExpandFileName(ParamStr(2)) + DirectorySeparator +
    'voice-process-' + IntToHex(GetTickCount64, 16);
  if not FileExists(ExecutablePath) then
    raise Exception.Create('renderer does not exist: ' + ExecutablePath);
  if not ForceDirectories(Base) then
    raise Exception.Create('cannot create process-test directory: ' + Base);
  TestArguments(ExecutablePath, Base);
  TestOutputs(ExecutablePath, Base);
end;

begin
  try
    Main;
    WriteLn('Voice Studio process tests passed: ', Checks);
  except
    on E: Exception do
    begin
      WriteLn('FAIL: ', E.ClassName, ': ', E.Message);
      ExitCode := 1;
    end;
  end;
end.
