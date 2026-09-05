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
program wfc_connectivity_process_test;

{$mode delphi}{$H+}

uses
  Classes,
  SysUtils,
  Process;

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
  LCount, I, LOffset: Integer;
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
    if GetTickCount64 - LStart > 60000 then
      raise Exception.Create('connected-routes process deadline exceeded');
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
  LActualExit: Integer;
  LChild: TProcess;
begin
  Result := '';
  LChild := StartChild(AExecutable, AArguments);
  try
    LActualExit := FinishChild(LChild, Result);
    if LActualExit <> AExpectedExit then
      raise Exception.CreateFmt('expected exit %d, got %d: %s',
        [AExpectedExit, LActualExit, Result]);
    Check(True, 'child exit status');
  finally
    ReleaseChild(LChild);
  end;
end;

function ReadSmallFile(const APath: String): String;
var
  LFile: TFileStream;
begin
  LFile := TFileStream.Create(APath, fmOpenRead or fmShareDenyNone);
  try
    Check((LFile.Size > 0) and (LFile.Size <= 1048576),
      'SVG fixture has a bounded nonempty extent');
    SetLength(Result, LFile.Size);
    LFile.ReadBuffer(Result[1], Length(Result));
  finally
    LFile.Free;
  end;
end;

procedure WriteMarker(const APath, AText: String);
var
  LFile: TFileStream;
begin
  LFile := TFileStream.Create(APath, fmCreate);
  try
    if AText <> '' then LFile.WriteBuffer(AText[1], Length(AText));
  finally
    LFile.Free;
  end;
end;

function HasPartial(const AOutput: String): Boolean;
var
  LSearch: TSearchRec;
begin
  Result := FindFirst(AOutput + '.partial-*', faAnyFile, LSearch) = 0;
  if Result then FindClose(LSearch);
end;

procedure CheckSvg(const AText, ACase, ASignature: String;
  const AMaximumZ: Integer);
begin
  Check(Pos('<?xml version="1.0" encoding="UTF-8"?>' + #10, AText) = 1,
    ACase + ' SVG declaration');
  Check(Pos(#13, AText) = 0, ACase + ' SVG is LF-only');
  Check(Pos('data-case="' + ACase + '"', AText) > 0,
    ACase + ' SVG case metadata');
  Check(Pos('data-signature="' + ASignature + '"', AText) > 0,
    ACase + ' SVG signature metadata');
  Check(Pos('data-z="' + IntToStr(AMaximumZ) + '"', AText) > 0,
    ACase + ' SVG coordinate depth');
  Check(Pos('</svg>' + #10, AText) = Length(AText) - 6,
    ACase + ' SVG closes canonically');
end;

procedure TestArguments(const AExecutable, ABase: String);
var
  LText: String;
begin
  LText := Invoke(AExecutable, ['--version'], 0);
  Check(Pos('ConnectedRoutes 1', LText) > 0, 'version output');
  LText := Invoke(AExecutable, ['--help'], 0);
  Check((Pos('--case town|circulation', LText) > 0)
    and (Pos('--repair-to', LText) > 0)
    and (Pos('never replaced', LText) > 0), 'help contract');
  LText := Invoke(AExecutable, ['--selftest'], 0);
  Check(Pos('self-test passed:', LText) > 0, 'native self-test');
  Invoke(AExecutable, ['--unknown'], 1);
  Invoke(AExecutable, ['--case'], 1);
  Invoke(AExecutable, ['--case', 'flat'], 1);
  Invoke(AExecutable, ['--seed', '-1'], 1);
  Invoke(AExecutable, ['--seed', '4294967296'], 1);
  Invoke(AExecutable, ['--backtracks', '2x'], 1);
  Invoke(AExecutable, ['--pass-backtracks', '2147483648'], 1);
  Invoke(AExecutable, ['--portal', 'first', '--portal', 'second'], 1);
  Invoke(AExecutable, ['--portal', 'none', '--svg',
    ABase + DirectorySeparator + 'impossible.svg'], 2);
  Check(not FileExists(ABase + DirectorySeparator + 'impossible.svg')
    and not HasPartial(ABase + DirectorySeparator + 'impossible.svg'),
    'unsolved request creates no SVG or partial');
end;

procedure TestSvgOutputs(const AExecutable, ABase: String);
var
  LCirculation, LExisting, LRepair, LTown, LTownReplay, LText: String;
begin
  LTown := ABase + DirectorySeparator + 'town.svg';
  LTownReplay := ABase + DirectorySeparator + 'town-replay.svg';
  LRepair := ABase + DirectorySeparator + 'town-repair.svg';
  LCirculation := ABase + DirectorySeparator + 'circulation.svg';
  LText := Invoke(AExecutable, ['--case', 'town', '--seed', '0',
    '--portal', 'first', '--svg', LTown], 0);
  Check(Pos('signature=61943F3F', LText) > 0,
    'town stdout signature');
  CheckSvg(ReadSmallFile(LTown), 'town', '61943F3F', 0);
  Invoke(AExecutable, ['--seed', '0', '--case', 'town',
    '--portal', 'first', '--svg', LTownReplay], 0);
  Check(ReadSmallFile(LTown) = ReadSmallFile(LTownReplay),
    'same-seed town SVG is byte-identical');

  LText := Invoke(AExecutable, ['--case', 'town', '--portal', 'first',
    '--repair-to', 'second', '--svg', LRepair], 0);
  Check((Pos('baseline-signature=61943F3F', LText) > 0)
    and (Pos('signature=5C735E7A', LText) > 0)
    and (Pos('repair-provider-reused=TRUE', LText) > 0),
    'town repair stdout proves scope and reroute');
  CheckSvg(ReadSmallFile(LRepair), 'town', '5C735E7A', 0);

  LText := Invoke(AExecutable, ['--case', 'circulation', '--portal', 'first',
    '--svg', LCirculation], 0);
  Check(Pos('signature=9F2CC7A4', LText) > 0,
    'circulation stdout signature');
  CheckSvg(ReadSmallFile(LCirculation), 'circulation', '9F2CC7A4', 1);
  Check(Pos('floor z=0', ReadSmallFile(LCirculation)) > 0,
    'circulation SVG contains the lower floor');
  Check(Pos('floor z=1', ReadSmallFile(LCirculation)) > 0,
    'circulation SVG contains the upper floor');

  LExisting := ABase + DirectorySeparator + 'existing.svg';
  WriteMarker(LExisting, 'keep-this');
  Invoke(AExecutable, ['--case', 'town', '--svg', LExisting], 1);
  Check(ReadSmallFile(LExisting) = 'keep-this',
    'existing SVG remains unchanged');
  Check(not HasPartial(LExisting),
    'existing-target refusal leaves no owned partial');
end;

procedure Main;
var
  LBase, LExecutable: String;
begin
  if ParamCount <> 2 then
    raise Exception.Create(
      'usage: wfc_connectivity_process_test CONNECTED-ROUTES OUTPUT-PARENT');
  LExecutable := ExpandFileName(ParamStr(1));
  if not FileExists(LExecutable) then
    raise Exception.Create('ConnectedRoutes executable does not exist');
  if not DirectoryExists(ExpandFileName(ParamStr(2))) then
    raise Exception.Create('output parent does not exist');
  LBase := IncludeTrailingPathDelimiter(ExpandFileName(ParamStr(2))) +
    'connectivity-process-' + IntToHex(GetProcessID, 8) + '-' +
    IntToHex(GetTickCount64, 16);
  if not CreateDir(LBase) then
    raise Exception.Create('cannot create isolated process fixture directory');
  TestArguments(LExecutable, LBase);
  TestSvgOutputs(LExecutable, LBase);
end;

begin
  try
    Main;
    WriteLn('Connected routes process checks: ', Checks, '/', Checks);
  except
    on E: Exception do
    begin
      WriteLn(StdErr, 'FAIL: ', E.Message);
      ExitCode := 1;
    end;
  end;
end.
