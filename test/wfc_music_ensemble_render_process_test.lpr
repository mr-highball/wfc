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
program wfc_music_ensemble_render_process_test;

{$mode delphi}{$H+}

uses
  Classes, SysUtils, Process, wfc_process_test_support, wfc_atomic_new_file,
  wfc_music_audio, wfc_music_audio_stream
  {$IFDEF UNIX}, BaseUnix{$ENDIF};

type
  TAtomicSink = class(TWfcMusicAudioByteSink)
  public
    FileTarget: TWfcAtomicNewFile;
    procedure WriteBytes(const ABytes: array of Byte); override;
  end;

var Checks: Integer;

procedure Check(const ACondition: Boolean; const AMessage: String);
begin
  Inc(Checks);
  if not ACondition then raise Exception.Create(AMessage);
end;

procedure TAtomicSink.WriteBytes(const ABytes: array of Byte);
begin FileTarget.WriteBytes(ABytes); end;

procedure WriteNewText(const APath, AText: String);
var LFile: TWfcAtomicNewFile; LBytes: array of Byte; I: Integer;
begin
  SetLength(LBytes, Length(AText));
  for I := 1 to Length(AText) do LBytes[I-1] := Ord(AText[I]);
  LFile := TWfcAtomicNewFile.Create(APath);
  try LFile.WriteBytes(LBytes); LFile.Publish; finally LFile.Free; end;
end;

function ReadSmallFile(const APath: String): String;
var LFile: TFileStream;
begin
  LFile := TFileStream.Create(APath, fmOpenRead or fmShareDenyNone);
  try
    Check(LFile.Size <= 1024, 'text fixture bound');
    SetLength(Result, LFile.Size);
    if Result <> '' then LFile.ReadBuffer(Result[1], Length(Result));
  finally LFile.Free; end;
end;

function HasPartial(const AOutput: String): Boolean;
var LSearch: TSearchRec;
begin
  Result := FindFirst(AOutput + '.partial-*', faAnyFile, LSearch) = 0;
  if Result then FindClose(LSearch);
end;

function StartChild(const AExecutable: String; const AArguments: array of String): TProcess;
var I: Integer;
begin
  Result := TProcess.Create(nil);
  try
    Result.Executable := ExpandFileName(AExecutable);
    for I := 0 to High(AArguments) do Result.Parameters.Add(AArguments[I]);
    Result.Options := [poUsePipes, poStderrToOutPut, poNoConsole];
    Result.Execute;
  except Result.Free; raise; end;
end;

procedure ReadAvailable(const AProcess: TProcess; var AText: String);
var LBytes: array[0..4095] of Byte; LCount, I, LOffset: Integer;
begin
  while AProcess.Output.NumBytesAvailable > 0 do
  begin
    LCount := AProcess.Output.NumBytesAvailable;
    if LCount > SizeOf(LBytes) then LCount := SizeOf(LBytes);
    LCount := AProcess.Output.Read(LBytes[0], LCount);
    if LCount <= 0 then Exit;
    if Length(AText) > 1048576 - LCount then
      raise Exception.Create('child output exceeds the test capture bound');
    LOffset := Length(AText); SetLength(AText, LOffset + LCount);
    for I := 0 to LCount - 1 do AText[LOffset + I + 1] := Chr(LBytes[I]);
  end;
end;

function FinishChild(const AProcess: TProcess; var AText: String): Integer;
var LStart: QWord;
begin
  LStart := GetTickCount64;
  repeat
    ReadAvailable(AProcess, AText);
    if not AProcess.Running then Break;
    if GetTickCount64 - LStart > 180000 then
      raise Exception.Create('renderer process-test deadline exceeded');
    Sleep(5);
  until False;
  ReadAvailable(AProcess, AText);
  Result := WfcProcessExitCode(AProcess);
end;

procedure ReleaseChild(const AProcess: TProcess);
begin
  if AProcess = nil then Exit;
  if AProcess.Running then
  begin AProcess.Terminate(1); AProcess.WaitOnExit(5000); end;
  AProcess.Free;
end;

function Invoke(const AExecutable: String; const AArguments: array of String;
  const AExit: Integer): String;
var LProcess: TProcess; LExit: Integer;
begin
  Result := '';
  LProcess := StartChild(AExecutable, AArguments);
  try
    LExit := FinishChild(LProcess, Result);
    if LExit <> AExit then
      raise Exception.CreateFmt('expected exit %d, got %d: %s', [AExit, LExit, Result]);
    Check(True, 'child exit status');
  finally ReleaseChild(LProcess); end;
end;

procedure VerifyWave(const APath: String; const AFrames: Int64);
var LFile: TFileStream; LHeader: array[0..43] of Byte; I: Integer;
  LDataBytes: Int64;
  function Little(const AOffset, ACount: Integer): Int64;
  var J: Integer;
  begin
    Result := 0;
    for J := ACount - 1 downto 0 do Result := Result * 256 + LHeader[AOffset + J];
  end;
  function Tag(const AOffset: Integer): String;
  var J: Integer;
  begin
    Result := '';
    for J := 0 to 3 do Result := Result + Chr(LHeader[AOffset + J]);
  end;
begin
  LDataBytes := AFrames * 2;
  LFile := TFileStream.Create(APath, fmOpenRead or fmShareDenyNone);
  try
    Check(LFile.Size = 44 + LDataBytes, 'physical extent includes the complete requested PCM');
    LFile.ReadBuffer(LHeader[0], SizeOf(LHeader));
    Check((Tag(0) = 'RIFF') and (Tag(8) = 'WAVE'), 'RIFF/WAVE tags');
    Check(Little(4, 4) = 36 + LDataBytes, 'exact RIFF size');
    Check((Tag(12) = 'fmt ') and (Little(16, 4) = 16), 'canonical format extent');
    Check((Little(20, 2) = 1) and (Little(22, 2) = 1), 'integer mono PCM');
    Check((Little(24, 4) = 44100) and (Little(28, 4) = 88200), 'sample and byte rates');
    Check((Little(32, 2) = 2) and (Little(34, 2) = 16), 'PCM16 block alignment');
    Check((Tag(36) = 'data') and (Little(40, 4) = LDataBytes), 'exact PCM data extent');
    I := 0;
    LFile.Position := 44 + LDataBytes - 2;
    LFile.ReadBuffer(I, 2);
    Check(I = 0, 'final release sample is zero without an appended tail');
  finally LFile.Free; end;
end;

procedure CompareFiles(const AFirst, ASecond: String);
var A, B: TFileStream; X, Y: array[0..8191] of Byte; LCount, I: Integer;
begin
  A := TFileStream.Create(AFirst, fmOpenRead or fmShareDenyNone);
  try
    B := TFileStream.Create(ASecond, fmOpenRead or fmShareDenyNone);
    try
      Check(A.Size = B.Size, 'deterministic file lengths');
      while A.Position < A.Size do
      begin
        LCount := SizeOf(X);
        if A.Size - A.Position < LCount then LCount := A.Size - A.Position;
        A.ReadBuffer(X[0], LCount); B.ReadBuffer(Y[0], LCount);
        for I := 0 to LCount - 1 do if X[I] <> Y[I] then
          raise Exception.Create('deterministic PCM byte mismatch');
      end;
      Check(True, 'deterministic files match every byte');
    finally B.Free; end;
  finally A.Free; end;
end;

procedure TestAtomicHelper(const ABase: String);
var A, B: TWfcAtomicNewFile; LPath, LTemporary: String;
  Rejected: Boolean; LSink: TAtomicSink; LWave: TWfcMusicWaveStream;
begin
  LPath := ABase + '/atomic.bin';
  A := TWfcAtomicNewFile.Create(LPath);
  try
    LTemporary := A.TemporaryPath;
    Check(FileExists(LTemporary) and not FileExists(LPath), 'exclusive sibling precedes publication');
    A.WriteBytes([0, 1, 127, 128, 255]);
    Check(A.ByteCount = 5, 'atomic byte count includes completed writes');
    A.Publish; A.Publish; A.Cancel;
    Check(A.Published and not A.Cancelled and FileExists(LPath), 'successful publication is idempotent');
    Check(not FileExists(LTemporary), 'successful publication removes its sibling');
    Rejected := False;
    try A.WriteBytes([]); except on EWfcAtomicNewFile do Rejected := True; end;
    Check(Rejected, 'published file cannot receive more bytes');
  finally A.Free; end;
  Check(ReadSmallFile(LPath) = #0#1#127#128#255, 'binary bytes survive helper publication');
  A := nil; Rejected := False;
  try A := TWfcAtomicNewFile.Create(LPath); except on EWfcAtomicNewFile do Rejected := True; end;
  A.Free;
  Check(Rejected and (ReadSmallFile(LPath) = #0#1#127#128#255), 'constructor refuses existing file unchanged');

  LPath := ABase + '/cancel.bin';
  A := TWfcAtomicNewFile.Create(LPath);
  try
    LTemporary := A.TemporaryPath; A.WriteBytes([5, 6]); A.Cancel; A.Cancel;
    Check(A.Cancelled and not A.Published and not FileExists(LTemporary), 'explicit cancel removes only its partial');
    Rejected := False;
    try A.Publish; except on EWfcAtomicNewFile do Rejected := True; end;
    Check(Rejected and not FileExists(LPath), 'cancel never publishes incomplete bytes');
  finally A.Free; end;
  A := TWfcAtomicNewFile.Create(LPath); LTemporary := A.TemporaryPath;
  A.WriteBytes([9]); A.Free;
  Check(not FileExists(LTemporary) and not FileExists(LPath), 'destructor cancels unpublished output');

  LPath := ABase + '/competing.bin';
  WriteNewText(LPath + '.partial-unrelated', 'leave unrelated sibling alone');
  A := TWfcAtomicNewFile.Create(LPath); B := nil;
  try
    LTemporary := A.TemporaryPath; A.WriteBytes([0, 1, 2]);
    B := TWfcAtomicNewFile.Create(LPath);
    Check(A.TemporaryPath <> B.TemporaryPath, 'concurrent writers own distinct exclusive siblings');
    B.WriteBytes([7, 8, 9]); B.Publish;
    Rejected := False;
    try A.Publish; except on EWfcAtomicNewFile do Rejected := True; end;
    Check(Rejected and A.Failed and not A.Published, 'late competing destination poisons losing publication');
    A.Cancel;
    Check(not FileExists(LTemporary), 'losing publication cleans its exact partial');
    Check(ReadSmallFile(LPath) = #7#8#9, 'winner remains byte-exact after failed competitor');
    Check(ReadSmallFile(LPath + '.partial-unrelated') = 'leave unrelated sibling alone',
      'cleanup never deletes unrelated partial-like names');
  finally B.Free; A.Free; end;

  LPath := ABase + '/short-rf64.wav';
  A := TWfcAtomicNewFile.Create(LPath); LSink := TAtomicSink.Create; LWave := nil;
  try
    LSink.FileTarget := A;
    LWave := TWfcMusicWaveStream.Create(LSink, 44100, 2205000000);
    Check(LWave.IsRF64 and (A.ByteCount = 80), 'known long count selects RF64 without allocating song PCM');
    Rejected := False;
    try LWave.Finish; except on EWfcMusicAudioStream do Rejected := True; end;
    Check(Rejected, 'short RF64 cannot be mistaken for complete audio');
    LTemporary := A.TemporaryPath; A.Cancel;
    Check(not FileExists(LPath) and not FileExists(LTemporary), 'failed short stream leaves no published or partial output');
  finally LWave.Free; LSink.Free; A.Free; end;
end;

procedure TestArguments(const AExecutable, ABase: String);
const BAD_SECONDS: array[0..8] of String =
  ('0', '-1', '1e3', '1x', '1.', '.5', '1..2', '1000000000000', '99999999999999999999');
var LText, LOutput: String; I: Integer;
begin
  LText := Invoke(AExecutable, ['--version'], 0);
  Check(Pos('EnsembleStudioRender 1', LText) > 0, 'standalone version');
  LText := Invoke(AExecutable, ['--help'], 0);
  Check(Pos('--segment-cells', LText) > 0, 'help exposes bounded local horizon');
  LOutput := ABase + '/invalid.wav';
  Invoke(AExecutable, [], 1);
  Invoke(AExecutable, ['--seconds'], 1);
  Invoke(AExecutable, ['--unknown', '1'], 1);
  Invoke(AExecutable, ['--seconds', '1', '--seconds', '1', '--output', LOutput], 1);
  Invoke(AExecutable, ['--seconds', '1', '--output', LOutput, '--trace', '--trace'], 1);
  for I := 0 to High(BAD_SECONDS) do
  begin
    Invoke(AExecutable, ['--seconds', BAD_SECONDS[I], '--output', LOutput], 1);
    Check(not FileExists(LOutput) and not HasPartial(LOutput),
      'invalid duration leaves no output: ' + BAD_SECONDS[I]);
  end;
  Invoke(AExecutable, ['--seconds', '1', '--output', LOutput, '--seed', '4294967296'], 1);
  Invoke(AExecutable, ['--seconds', '1', '--output', LOutput, '--seed', '-1'], 1);
  Invoke(AExecutable, ['--seconds', '1', '--output', LOutput, '--seed', '1x'], 1);
  Invoke(AExecutable, ['--seconds', '1', '--output', LOutput, '--segment-cells', '0'], 1);
  Invoke(AExecutable, ['--seconds', '1', '--output', LOutput, '--segment-cells', '8947849'], 1);
  Invoke(AExecutable, ['--seconds', '1', '--output', LOutput, '--backtracks', '2147483648'], 1);
  Invoke(AExecutable, ['--seconds', '1', '--output', LOutput, '--pass-backtracks', '-1'], 1);
  Invoke(AExecutable, ['--seconds', '1', '--output', ABase + '/missing/target.wav'], 1);
  Invoke(AExecutable, ['--seconds', '1', '--output', ABase], 1);
  Check(not FileExists(LOutput) and not HasPartial(LOutput), 'malformed options never create a partial');
end;

procedure TestRendering(const AExecutable, ABase: String);
var LText: String; LStart: QWord;
begin
  LText := Invoke(AExecutable, ['--seconds', '0.251', '--output', ABase + '/rounded.wav'], 0);
  Check((Pos('Requested ticks: 241; actual ticks: 480; cells: 2', LText) > 0) and
    (Pos('Completed frames: 22050', LText) > 0), 'requested and actual timeline accounting is explicit');
  VerifyWave(ABase + '/rounded.wav', 22050);
  LText := Invoke(AExecutable, ['--seconds', '4', '--seed', '0', '--output', ABase + '/four.wav', '--trace'], 0);
  Check((Pos('Last segment signature:', LText) > 0) and
    (Pos('transcript:', LText) > 0), 'trace option emits actual last-segment metadata');
  VerifyWave(ABase + '/four.wav', 176400);
  Invoke(AExecutable, ['--seconds', '4', '--seed', '0', '--output', ABase + '/repeat.wav'], 0);
  CompareFiles(ABase + '/four.wav', ABase + '/repeat.wav');
  LStart := GetTickCount64;
  LText := Invoke(AExecutable, ['--seconds', '61', '--seed', '0', '--output', ABase + '/long.wav'], 0);
  WriteLn('61-second ensemble render elapsed ms: ', GetTickCount64 - LStart);
  Check(Pos('Completed frames: 2690100', LText) > 0, 'long render reports every frame beyond the old 60-second preview limit');
  VerifyWave(ABase + '/long.wav', 2690100);
  LText := Invoke(AExecutable, ['--seconds', '1.000000000000000001', '--seed', '4294967295', '--segment-cells', '1',
    '--backtracks', '256', '--pass-backtracks', '16', '--output', ABase + '/one-cell.wav'], 0);
  Check(Pos('Requested ticks: 961; actual ticks: 1200; cells: 5', LText) > 0,
    'arbitrary fractional precision rounds upward exactly before cell alignment');
  VerifyWave(ABase + '/one-cell.wav', 55125);
  Invoke(AExecutable, ['--seconds', '2', '--seed', '1', '--output', ABase + '/four.wav'], 1);
  CompareFiles(ABase + '/four.wav', ABase + '/repeat.wav');
  Check(not HasPartial(ABase + '/four.wav') and not HasPartial(ABase + '/long.wav'),
    'successful and refused-existing renders leave no partial');
end;

procedure TestPublicationRace(const AExecutable, ABase: String);
var LChild: TProcess; LOutput, LText: String; LStart: QWord; LRaced: Boolean;
begin
  LOutput := ABase + '/race.wav'; LText := '';
  LChild := StartChild(AExecutable, ['--seconds', '61', '--output', LOutput]);
  try
    LRaced := False; LStart := GetTickCount64;
    while LChild.Running do
    begin
      ReadAvailable(LChild, LText);
      if HasPartial(LOutput) then
      begin
        WriteNewText(LOutput, 'preserve competing output'); LRaced := True; Break;
      end;
      if GetTickCount64 - LStart > 15000 then
        raise Exception.Create('publication race setup timed out');
      Sleep(1);
    end;
    Check(LRaced, 'competing output created after renderer opened its sibling');
    Check(FinishChild(LChild, LText) = 1, 'native publication refuses a newly appeared destination');
    Check(ReadSmallFile(LOutput) = 'preserve competing output', 'competing destination survives unchanged');
    Check(not HasPartial(LOutput), 'native publication failure removes its partial');
  finally ReleaseChild(LChild); end;
end;

{$IFDEF UNIX}
procedure TestCooperativeSignal(const AExecutable, ABase: String);
var LChild: TProcess; LOutput, LText: String; LStart: QWord; LSignalled: Boolean;
begin
  LOutput := ABase + '/cancelled.wav'; LText := '';
  LChild := StartChild(AExecutable, ['--seconds', '61', '--output', LOutput]);
  try
    LSignalled := False; LStart := GetTickCount64;
    while LChild.Running do
    begin
      ReadAvailable(LChild, LText);
      if HasPartial(LOutput) then
      begin
        Check(fpKill(LChild.ProcessID, SIGINT) = 0, 'send cooperative interrupt to owned child');
        LSignalled := True; Break;
      end;
      if GetTickCount64 - LStart > 15000 then raise Exception.Create('cancel fixture setup timed out');
      Sleep(1);
    end;
    Check(LSignalled, 'interrupt follows output opening');
    Check(FinishChild(LChild, LText) = 1, 'cooperative cancellation has unsuccessful exit');
    Check((Pos('cancelled', LText) > 0) and not FileExists(LOutput) and not HasPartial(LOutput),
      'cooperative interrupt cleans partial without publication');
  finally ReleaseChild(LChild); end;
end;
{$ENDIF}

procedure Run(const AExecutable, AParent: String);
const OUTPUTS: array[0..12] of String = ('atomic.bin', 'cancel.bin', 'competing.bin',
  'competing.bin.partial-unrelated', 'short-rf64.wav', 'invalid.wav', 'rounded.wav',
  'four.wav', 'repeat.wav', 'long.wav', 'one-cell.wav', 'race.wav', 'cancelled.wav');
var LBase: String; I: Integer;
begin
  Check(FileExists(AExecutable), 'renderer executable exists');
  Check(DirectoryExists(AParent), 'test output parent exists');
  LBase := IncludeTrailingPathDelimiter(ExpandFileName(AParent)) + 'ensemble-render-test-' +
    IntToStr(GetProcessID) + '-' + IntToStr(GetTickCount64);
  if DirectoryExists(LBase) or not CreateDir(LBase) then
    raise Exception.Create('cannot create isolated process-test directory');
  try
    TestAtomicHelper(LBase);
    TestArguments(AExecutable, LBase);
    TestRendering(AExecutable, LBase);
    TestPublicationRace(AExecutable, LBase);
    {$IFDEF UNIX}TestCooperativeSignal(AExecutable, LBase);{$ENDIF}
  finally
    { Only exact paths inside this newly owned directory may be removed.
      Unexpected leftovers are retained for investigation, never swept. }
    for I := 0 to High(OUTPUTS) do if FileExists(LBase + '/' + OUTPUTS[I]) then
      DeleteFile(LBase + '/' + OUTPUTS[I]);
    if not RemoveDir(LBase) then WriteLn('Retained fixture directory: ', LBase);
  end;
end;

begin
  try
    if ParamCount <> 2 then
      raise Exception.Create('Usage: wfc_music_ensemble_render_process_test RENDERER OUTPUT_PARENT');
    Run(ParamStr(1), ParamStr(2));
    WriteLn('Ensemble render process checks: ', Checks, '/', Checks);
  except on E: Exception do
    begin WriteLn(StdErr, E.Message); ExitCode := 1; end;
  end;
end.
