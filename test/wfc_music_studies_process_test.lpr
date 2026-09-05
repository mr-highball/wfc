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
program wfc_music_studies_process_test;

{$mode delphi}{$H+}

uses
  Classes,
  SysUtils,
  Process,
  wfc_midi_smf;

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
    if GetTickCount64 - Start > 60000 then
      raise Exception.Create('Music study process-test deadline exceeded');
    Sleep(5);
  until False;
  ReadAvailable(AProcess, AText);
  Result := AProcess.ExitStatus;
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
    Check((FileData.Size >= 22) and
      (FileData.Size <= 1024 * 1024), 'MIDI fixture extent');
    SetLength(Bytes, FileData.Size);
    FileData.ReadBuffer(Bytes[0], Length(Bytes));
  finally
    FileData.Free;
  end;
  Midi := DecodeWfcMidiFile(Bytes);
  Check((Midi.Format = 0) and (Midi.TicksPerQuarter = 1) and
    (Length(Midi.Tracks) = 1), 'format-0 MIDI time base');
  Tick := 0;
  for I := 0 to High(Midi.Tracks[0].Events) do
    Inc(Tick, Midi.Tracks[0].Events[I].DeltaTicks);
  Inc(Tick, Midi.Tracks[0].EndDeltaTicks);
  Check(Tick = AEndTick, 'decoded MIDI reaches planned end tick');
end;

procedure TestStudy(const AExecutable, ABase: String; const ARiff: Boolean);
const
  InvalidNotes: array[0..6] of String =
    ('0', '-1', '1e3', '1.5', '1x', '2147483648', '99999999999999999999');
var
  I: Integer;
  OutputPath, FirstWave, SecondWave, FirstMidi, SecondMidi, Text: String;
begin
  Text := Invoke(AExecutable, ['--help'], 0);
  Check((Pos('--notes N', Text) > 0) and (Pos('--tempo-us N', Text) > 0),
    'help documents user-controlled length and tempo');
  Text := Invoke(AExecutable, [], 0);
  Check((Pos('notes=24', Text) > 0) and (Pos('duration-us=12000000', Text) > 0)
    and (Pos('published=', Text) = 0), 'default run only reports generated score');
  Text := Invoke(AExecutable, ['--notes', '1', '--tempo-us', '1',
    '--seed', '4294967295'], 0);
  Check(Pos('duration-us=1', Text) > 0, 'exact minimum time and maximum seed');
  Invoke(AExecutable, ['--unknown', '1'], 1);
  Invoke(AExecutable, ['--notes'], 1);
  Invoke(AExecutable, ['--seed', '4294967296'], 1);
  Invoke(AExecutable, ['--seed', '2x'], 1);
  Invoke(AExecutable, ['--notes', ''], 1);
  Invoke(AExecutable, ['--tempo-us', '0'], 1);
  Invoke(AExecutable, ['--tempo-us', '2147483648'], 1);
  if ARiff then
  begin
    Invoke(AExecutable, ['--songs', ''], 1);
    Invoke(AExecutable, ['--songs', 'mary,'], 1);
    Invoke(AExecutable, ['--songs', 'unknown'], 1);
    Invoke(AExecutable, ['--songs', 'bridge', '--seed', '55'], 0);
  end
  else Invoke(AExecutable, ['--songs', 'mary'], 1);
  for I := 0 to High(InvalidNotes) do
  begin
    OutputPath := ABase + DirectorySeparator + 'invalid-' + IntToStr(I) + '.wav';
    Invoke(AExecutable, ['--wave', OutputPath, '--notes', InvalidNotes[I]], 1);
    Check(not FileExists(OutputPath) and not HasPartial(OutputPath),
      'invalid request has no public or partial file');
  end;

  FirstWave := ABase + DirectorySeparator + 'first.wav';
  SecondWave := ABase + DirectorySeparator + 'second.wav';
  FirstMidi := ABase + DirectorySeparator + 'first.mid';
  SecondMidi := ABase + DirectorySeparator + 'second.mid';
  Text := Invoke(AExecutable, ['--notes', '3', '--tempo-us', '333333',
    '--seed', '55', '--wave', FirstWave], 0);
  Check((Pos('duration-us=999999', Text) > 0) and (Pos('published=', Text) > 0),
    'requested fractional-second timeline is published');
  Invoke(AExecutable, ['--notes', '3', '--tempo-us', '333333',
    '--seed', '55', '--wave', SecondWave], 0);
  VerifyWave(FirstWave, 44099);
  CompareFiles(FirstWave, SecondWave);
  Check(not HasPartial(FirstWave) and not HasPartial(SecondWave),
    'successful WAVE publication leaves no partial siblings');

  Invoke(AExecutable, ['--notes', '123', '--seed', '55', '--midi', FirstMidi], 0);
  Invoke(AExecutable, ['--notes', '123', '--seed', '55', '--midi', SecondMidi], 0);
  VerifyMidi(FirstMidi, 123);
  CompareFiles(FirstMidi, SecondMidi);
  Check(not HasPartial(FirstMidi), 'successful MIDI publication leaves no partial');
  OutputPath := ABase + DirectorySeparator + 'existing.wav';
  WriteMarker(OutputPath, 'keep-this');
  Invoke(AExecutable, ['--wave', OutputPath, '--notes', '2'], 1);
  Invoke(AExecutable, ['--midi', OutputPath, '--notes', '2'], 1);
  Check(ReadMarker(OutputPath) = 'keep-this', 'existing targets are never overwritten');
  Check(not HasPartial(OutputPath), 'refusal leaves no partial sibling');

  OutputPath := ABase + DirectorySeparator + 'failed.mid';
  Invoke(AExecutable, ['--midi', OutputPath, '--notes', '2',
    '--tempo-us', '16777216'], 1);
  Check(not FileExists(OutputPath) and not HasPartial(OutputPath),
    'MIDI tempo-format failure leaves no output or partial');
  Invoke(AExecutable, ['--midi', OutputPath, '--wave', OutputPath], 1);
  Check(not FileExists(OutputPath) and not HasPartial(OutputPath),
    'two selected formats are rejected before output');
  Invoke(AExecutable, ['--wave', ABase + DirectorySeparator + 'missing' +
    DirectorySeparator + 'file.wav', '--notes', '2'], 1);
end;

procedure Main;
var Base, ScaleExecutable, RiffExecutable: String;
begin
  if ParamCount <> 3 then
    raise Exception.Create(
      'usage: wfc_music_studies_process_test SCALE RIFF ARTIFACT-DIRECTORY');
  ScaleExecutable := ExpandFileName(ParamStr(1));
  RiffExecutable := ExpandFileName(ParamStr(2));
  Check(FileExists(ScaleExecutable) and FileExists(RiffExecutable),
    'native music study executables exist');
  Base := ExpandFileName(ParamStr(3)) + DirectorySeparator +
    'music-studies-process-' + IntToHex(GetTickCount64, 16);
  Check(not DirectoryExists(Base), 'isolated process artifact directory');
  if not ForceDirectories(Base + DirectorySeparator + 'scale') or
    not ForceDirectories(Base + DirectorySeparator + 'riff') then
    raise Exception.Create('cannot create music-study process directory');
  TestStudy(ScaleExecutable, Base + DirectorySeparator + 'scale', False);
  TestStudy(RiffExecutable, Base + DirectorySeparator + 'riff', True);
end;

begin
  try
    Main;
    WriteLn('Music study process tests passed: ', Checks);
  except
    on E: Exception do
    begin
      WriteLn('FAIL: ', E.ClassName, ': ', E.Message);
      ExitCode := 1;
    end;
  end;
end.
