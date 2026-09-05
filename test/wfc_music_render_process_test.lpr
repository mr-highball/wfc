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
program wfc_music_render_process_test;

{$mode delphi}{$H+}

uses
  Classes, SysUtils, Process, wfc_process_test_support;

var
  Checks: Integer;

procedure Check(const ACondition: Boolean; const AMessage: String);
begin
  Inc(Checks);
  if not ACondition then
    raise Exception.Create(AMessage);
end;

function StartRender(const AExecutable, ASeconds, ASeed,
  AOutput: String): TProcess;
begin
  Result := TProcess.Create(nil);
  try
    Result.Executable := ExpandFileName(AExecutable);
    Result.Parameters.Add('--seconds');
    Result.Parameters.Add(ASeconds);
    Result.Parameters.Add('--seed');
    Result.Parameters.Add(ASeed);
    Result.Parameters.Add('--output');
    Result.Parameters.Add(AOutput);
    Result.Options := [poUsePipes, poNoConsole];
    Result.Execute;
  except
    Result.Free;
    raise;
  end;
end;

function WaitRender(const AProcess: TProcess): Integer;
var
  LStart: QWord;
begin
  LStart := GetTickCount64;
  while not AProcess.WaitOnExit(250) do
    if GetTickCount64 - LStart > 180000 then
    begin
      AProcess.Terminate(1);
      AProcess.WaitOnExit(5000);
      raise Exception.Create('render process-test deadline exceeded');
    end;
  Result := WfcProcessExitCode(AProcess);
end;

procedure Render(const AExecutable, ASeconds, ASeed,
  AOutput: String; const AExpectedExit: Integer);
var
  LProcess: TProcess;
begin
  LProcess := StartRender(AExecutable, ASeconds, ASeed, AOutput);
  try
    Check(WaitRender(LProcess) = AExpectedExit, 'render exit: ' + ASeconds);
  finally
    if LProcess.Running then
    begin
      LProcess.Terminate(1);
      LProcess.WaitOnExit(5000);
    end;
    LProcess.Free;
  end;
end;

function FileText(const APath: String): String;
var
  LStream: TFileStream;
begin
  LStream := TFileStream.Create(APath, fmOpenRead);
  try
    if LStream.Size > 1024 then
      raise Exception.Create('text fixture exceeds fixed bound');
    SetLength(Result, LStream.Size);
    if Result <> '' then
      LStream.ReadBuffer(Result[1], Length(Result));
  finally
    LStream.Free;
  end;
end;

procedure WriteNewText(const APath, AText: String);
var
  LStream: TFileStream;
begin
  if FileExists(APath) or DirectoryExists(APath) then
    raise Exception.Create('refusing to overwrite fixture');
  LStream := TFileStream.Create(APath, fmCreate);
  try
    LStream.WriteBuffer(AText[1], Length(AText));
  finally
    LStream.Free;
  end;
end;

procedure VerifyWave(const APath: String; const ASeconds: Integer);
var
  LStream: TFileStream;
  LHeader: array[0..43] of Byte;
  LFrames: Int64;
  LBytes: Int64;

  function Little(const AOffset, ACount: Integer): Int64;
  var
    I: Integer;
  begin
    Result := 0;
    for I := ACount - 1 downto 0 do
      Result := Result * 256 + LHeader[AOffset + I];
  end;

  function Tag(const AOffset: Integer): String;
  var
    I: Integer;
  begin
    Result := '';
    for I := 0 to 3 do
      Result := Result + Chr(LHeader[AOffset + I]);
  end;

begin
  LFrames := Int64(ASeconds) * 44100;
  LBytes := LFrames * 2;
  LStream := TFileStream.Create(APath, fmOpenRead);
  try
    Check(LStream.Size = 44 + LBytes, 'physical WAVE extent: ' + IntToStr(ASeconds));
    LStream.ReadBuffer(LHeader[0], SizeOf(LHeader));
    Check((Tag(0) = 'RIFF') and (Tag(8) = 'WAVE'), 'RIFF/WAVE tags');
    Check(Little(4, 4) = LBytes + 36, 'RIFF size');
    Check((Tag(12) = 'fmt ') and (Little(16, 4) = 16), 'canonical format chunk');
    Check((Little(20, 2) = 1) and (Little(22, 2) = 1), 'mono PCM');
    Check((Little(24, 4) = 44100) and (Little(28, 4) = 88200), 'sample/byte rates');
    Check((Little(32, 2) = 2) and (Little(34, 2) = 16), 'PCM16 block alignment');
    Check((Tag(36) = 'data') and (Little(40, 4) = LBytes), 'data chunk extent');
  finally
    LStream.Free;
  end;
end;

procedure EqualFiles(const AFirst, ASecond: String; const ADataOnly: Boolean;
  const ACount: Int64);
var
  A, B: TFileStream;
  ABuf, BBuf: array[0..65535] of Byte;
  LRemaining: Int64;
  LWant, I: Integer;
begin
  A := TFileStream.Create(AFirst, fmOpenRead);
  try
    B := TFileStream.Create(ASecond, fmOpenRead);
    try
      if ADataOnly then
      begin
        A.Position := 44;
        B.Position := 44;
        LRemaining := ACount;
      end
      else
      begin
        Check(A.Size = B.Size, 'repeat file extents match');
        LRemaining := A.Size;
      end;
      while LRemaining > 0 do
      begin
        LWant := SizeOf(ABuf);
        if LRemaining < LWant then
          LWant := Integer(LRemaining);
        A.ReadBuffer(ABuf[0], LWant);
        B.ReadBuffer(BBuf[0], LWant);
        for I := 0 to LWant - 1 do
          if ABuf[I] <> BBuf[I] then
            raise Exception.Create('rendered audio differs at compared byte');
        Dec(LRemaining, LWant);
      end;
      Check(True, 'streamed files/prefixes are byte-identical');
    finally
      B.Free;
    end;
  finally
    A.Free;
  end;
end;

function PartialExists(const APath: String): Boolean;
var
  LSearch: TSearchRec;
begin
  Result := FindFirst(APath + '.partial-*', faAnyFile, LSearch) = 0;
  if Result then
    FindClose(LSearch);
end;

procedure Run(const AExecutable, AParent: String);
var
  LBase: String;
  LProcess: TProcess;
  LStart: QWord;
  LPublishedRaceTarget: Boolean;
begin
  Check(FileExists(AExecutable), 'renderer executable exists');
  Check(DirectoryExists(AParent), 'output parent exists');
  LBase := IncludeTrailingPathDelimiter(ExpandFileName(AParent)) +
    'music-render-test-' + IntToStr(GetTickCount64);
  if DirectoryExists(LBase) or not CreateDir(LBase) then
    raise Exception.Create('cannot create isolated process-test directory');
  try
    Render(AExecutable, '4', '0', LBase + '/four.wav', 0);
    Render(AExecutable, '4', '0', LBase + '/repeat.wav', 0);
    Render(AExecutable, '6', '0', LBase + '/six.wav', 0);
    LStart := GetTickCount64;
    Render(AExecutable, '180', '0', LBase + '/long.wav', 0);
    WriteLn('180-second render elapsed ms: ', GetTickCount64 - LStart);
    Render(AExecutable, '0.001', '0', LBase + '/rounded.wav', 0);
    VerifyWave(LBase + '/four.wav', 4);
    VerifyWave(LBase + '/six.wav', 6);
    VerifyWave(LBase + '/long.wav', 180);
    VerifyWave(LBase + '/rounded.wav', 2);
    EqualFiles(LBase + '/four.wav', LBase + '/repeat.wav', False, 0);
    EqualFiles(LBase + '/four.wav', LBase + '/six.wav', True, 4 * 88200);
    EqualFiles(LBase + '/four.wav', LBase + '/long.wav', True, 4 * 88200);
    Render(AExecutable, '6', '1', LBase + '/four.wav', 1);
    EqualFiles(LBase + '/four.wav', LBase + '/repeat.wav', False, 0);
    Render(AExecutable, '4', '4294967296', LBase + '/invalid.wav', 1);
    Check(not FileExists(LBase + '/invalid.wav'), 'bad seed publishes no file');
    Render(AExecutable, '0', '0', LBase + '/invalid.wav', 1);
    Check(not FileExists(LBase + '/invalid.wav'), 'bad duration publishes no file');
    Render(AExecutable, '99999999999999999999', '0', LBase + '/invalid.wav', 1);
    Check(not FileExists(LBase + '/invalid.wav'), 'overflow duration publishes no file');
    LProcess := StartRender(AExecutable, '180', '0', LBase + '/race.wav');
    try
      LPublishedRaceTarget := False;
      LStart := GetTickCount64;
      while LProcess.Running do
      begin
        if PartialExists(LBase + '/race.wav') then
        begin
          WriteNewText(LBase + '/race.wav', 'preserved competing destination');
          LPublishedRaceTarget := True;
          Break;
        end;
        if GetTickCount64 - LStart > 10000 then
          raise Exception.Create('publication-race fixture setup deadline exceeded');
        Sleep(1);
      end;
      Check(LPublishedRaceTarget, 'competing destination created after partial opened');
      Check(WaitRender(LProcess) = 1, 'publication refuses competing destination');
      Check(FileText(LBase + '/race.wav') = 'preserved competing destination',
        'competing destination survives byte-for-byte');
      Check(not PartialExists(LBase + '/race.wav'), 'failed publication removes owned partial');
    finally
      if LProcess.Running then
      begin
        LProcess.Terminate(1);
        LProcess.WaitOnExit(5000);
      end;
      LProcess.Free;
    end;
    Check(not PartialExists(LBase + '/four.wav'), 'successful render leaves no partial');
    Check(not PartialExists(LBase + '/invalid.wav'), 'invalid options leave no partial');
  finally
    { No recursive cleanup: only the exact fixture output paths above. }
    DeleteFile(LBase + '/four.wav');
    DeleteFile(LBase + '/repeat.wav');
    DeleteFile(LBase + '/six.wav');
    DeleteFile(LBase + '/long.wav');
    DeleteFile(LBase + '/rounded.wav');
    DeleteFile(LBase + '/invalid.wav');
    DeleteFile(LBase + '/race.wav');
    RemoveDir(LBase);
  end;
end;

begin
  try
    if ParamCount <> 2 then
      raise Exception.Create('Usage: wfc_music_render_process_test RENDERER OUTPUT_PARENT');
    Run(ParamStr(1), ParamStr(2));
    WriteLn('Music render process checks: ', Checks, '/', Checks);
  except
    on E: Exception do
    begin
      WriteLn(StdErr, E.Message);
      ExitCode := 1;
    end;
  end;
end.
