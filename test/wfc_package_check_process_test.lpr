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
program wfc_package_check_process_test;

{$mode delphi}{$H+}
{$IFDEF PAS2JS}{$FATAL package filesystem process tests require native FPC}{$ENDIF}

uses Classes, SysUtils, Process, Pipes, wfc_process_test_support,
  {$IFDEF MSWINDOWS}Windows{$ELSE}BaseUnix{$ENDIF};

const
  ALPHA_TEXT = '{unit ghost;}'#10'unit alpha; interface implementation end.'#10;
  BETA_TEXT = '(* unit ghost; *)'#10'unit beta; interface implementation end.'#10;
  FPM_TEXT = 'program fpmake;'#10+
    'const bait = ''P.Targets.AddUnit(''''ghost.pas'''');'';'#10+
    'begin { P.Targets.AddUnit(''ghost.pas''); }'#10+
    'P.SourcePath.Add(''src'');'#10+
    'P.Targets.AddUnit(''alpha.pas'');'#10+
    'P.Targets.AddUnit(''beta.pas'');'#10'end.'#10;
  LPK_TEXT = '<?xml version="1.0"?>'#10+
    '<CONFIG><Package><Files Count="3">'#10+
    '<Item1><Filename Value="wfc_package.pas"/><Type Value="Main Unit"/>'+
    '<UnitName Value="wfc_package"/></Item1>'#10+
    '<Item2><Filename Value="src/alpha.pas"/><UnitName Value="alpha"/></Item2>'#10+
    '<Item3><Filename Value="src/beta.pas"/><UnitName Value="beta"/></Item3>'#10+
    '</Files></Package></CONFIG>'#10;
  PACKAGE_TEXT = 'unit wfc_package; interface'#10+
    '{ uses ghost; } uses alpha, beta;'#10+
    'implementation const bait = ''uses ghost;''; end.'#10;

var Checks: Integer; Checker, FixtureParent, Repo, Elsewhere: String;

procedure Check(const ACondition: Boolean; const AMessage: String);
begin
  Inc(Checks);
  if not ACondition then raise Exception.Create(AMessage);
end;

procedure WriteOwned(const APath, AText: String);
var LStream: TFileStream; LPath: String;
begin
  LPath := ExpandFileName(APath);
  if Pos(IncludeTrailingPathDelimiter(FixtureParent), LPath) <> 1 then
    raise Exception.Create('fixture write escaped its owned parent');
  LStream := TFileStream.Create(LPath, fmCreate);
  try if AText <> '' then LStream.WriteBuffer(AText[1], Length(AText));
  finally LStream.Free; end;
end;

function ReadOwned(const APath: String): String;
var LStream: TFileStream;
begin
  LStream := TFileStream.Create(APath, fmOpenRead);
  try
    if LStream.Size > 2097152 then raise Exception.Create('fixture read limit exceeded');
    SetLength(Result, LStream.Size);
    if Result <> '' then LStream.ReadBuffer(Result[1], Length(Result));
  finally LStream.Free; end;
end;

procedure RestoreFixture;
begin
  WriteOwned(Repo + DirectorySeparator + 'src' + DirectorySeparator + 'alpha.pas', ALPHA_TEXT);
  WriteOwned(Repo + DirectorySeparator + 'src' + DirectorySeparator + 'beta.pas', BETA_TEXT);
  WriteOwned(Repo + DirectorySeparator + 'fpmake.pp', FPM_TEXT);
  WriteOwned(Repo + DirectorySeparator + 'wfc.lpk', LPK_TEXT);
  WriteOwned(Repo + DirectorySeparator + 'wfc_package.pas', PACKAGE_TEXT);
end;

function WaitForOwnedExit(const AProcess: TProcess; const ATimeout: DWord): Boolean;
begin
  if not AProcess.Running then Exit(True);
  Result := AProcess.WaitOnExit(ATimeout);
  if not Result then Result := not AProcess.Running;
end;

function ReadPipe(const APipe: TInputPipeStream): String;
var LCount, LRead: Integer;
begin
  LCount := APipe.NumBytesAvailable;
  if LCount > 4096 then raise Exception.Create('checker output exceeds fixed diagnostic bound');
  SetLength(Result, LCount);
  if LCount > 0 then
  begin
    LRead := APipe.Read(Result[1], LCount);
    if LRead <> LCount then raise Exception.Create('checker output read was incomplete');
  end;
end;

procedure RunCheck(const AArguments: array of String; const AExpectedExit: Integer;
  const AMessage: String; const AExpectedOutput: String = '');
var LProcess: TProcess; I: Integer; LOut, LError, LBefore: String;
begin
  LBefore := GetCurrentDir;
  LProcess := TProcess.Create(nil);
  try
    LProcess.Executable := Checker;
    LProcess.CurrentDirectory := Elsewhere;
    for I := 0 to High(AArguments) do LProcess.Parameters.Add(AArguments[I]);
    LProcess.Options := [poUsePipes, poNoConsole];
    LProcess.Execute;
    Check(WaitForOwnedExit(LProcess, 5000), AMessage + ': bounded checker exit');
    LOut := ReadPipe(LProcess.Output); LError := ReadPipe(LProcess.Stderr);
    Check(WfcProcessExitCode(LProcess) = AExpectedExit,
      AMessage + ': exit mismatch; stderr=' + LError);
    if AExpectedExit = 0 then
    begin
      Check((LOut <> '') and (Pos(AExpectedOutput, LOut) > 0), AMessage + ': expected success output');
      Check(LError = '', AMessage + ': success stderr is empty');
    end
    else
    begin
      Check(LOut = '', AMessage + ': failure never publishes stdout');
      Check((Pos('wfc_package_check: ', LError) = 1) and
        (Length(LError) <= 550), AMessage + ': bounded failure diagnostic');
      if AExpectedOutput <> '' then
        Check(Pos(AExpectedOutput, LError) > 0, AMessage + ': expected failure cause');
    end;
    Check(GetCurrentDir = LBefore, AMessage + ': caller working directory unchanged');
  finally
    if LProcess.Running then
    begin
      {$IFDEF MSWINDOWS}
      LProcess.Terminate(1);
      {$ELSE}
      { Stable Unix TProcess.Terminate calls an untimed WaitOnExit internally.
        Signal only our known child directly, then retain the bounded wait. }
      fpKill(LProcess.ProcessID, SIGKILL);
      {$ENDIF}
      Check(WaitForOwnedExit(LProcess, 5000), 'owned checker post-termination exit');
    end;
    LProcess.Free;
  end;
end;

procedure CheckRepository(const AExpectedExit: Integer; const AMessage: String);
begin
  if AExpectedExit = 0 then RunCheck(['--root', Repo], AExpectedExit, AMessage, '2 source units')
  else RunCheck(['--root', Repo], AExpectedExit, AMessage);
end;

function Replace(const AText, AOld, ANew: String): String;
begin Result := StringReplace(AText, AOld, ANew, [rfReplaceAll]); end;

procedure TestManifests;
var LText: String;
begin
  RestoreFixture;
  CheckRepository(0, 'complete manifests ignore Pascal comment/string bait');
  RunCheck(['--root', '..' + DirectorySeparator + 'repo with spaces'], 0,
    'explicit relative root works from separate working directory', '2 source units');
  Check(ReadOwned(Repo + DirectorySeparator + 'fpmake.pp') = FPM_TEXT, 'fpmake input unchanged');
  Check(ReadOwned(Repo + DirectorySeparator + 'wfc.lpk') = LPK_TEXT, 'Lazarus input unchanged');
  Check(ReadOwned(Repo + DirectorySeparator + 'wfc_package.pas') = PACKAGE_TEXT, 'package input unchanged');
  Check(ReadOwned(Repo + DirectorySeparator + 'src' + DirectorySeparator + 'alpha.pas') = ALPHA_TEXT,
    'source input unchanged');
  WriteOwned(Repo + DirectorySeparator + 'fpmake.pp',
    Replace(FPM_TEXT, 'P.Targets.AddUnit(''beta.pas'');', '// P.Targets.AddUnit(''beta.pas'');'));
  CheckRepository(1, 'commented missing fpmake registration');
  RestoreFixture;
  LText := Replace(LPK_TEXT, '<Item3><Filename Value="src/beta.pas"/><UnitName Value="beta"/></Item3>', '');
  WriteOwned(Repo + DirectorySeparator + 'wfc.lpk', Replace(LText, 'Count="3"', 'Count="2"'));
  CheckRepository(1, 'missing Lazarus registration');
  RestoreFixture;
  WriteOwned(Repo + DirectorySeparator + 'wfc_package.pas', Replace(PACKAGE_TEXT, 'uses alpha, beta;', 'uses alpha;'));
  CheckRepository(1, 'missing package uses registration');
  RestoreFixture;
  WriteOwned(Repo + DirectorySeparator + 'fpmake.pp', Replace(FPM_TEXT, 'end.', 'P.Targets.AddUnit(''alpha.pas''); end.'));
  CheckRepository(1, 'duplicate fpmake registration');
  RestoreFixture;
  WriteOwned(Repo + DirectorySeparator + 'wfc.lpk', Replace(LPK_TEXT, 'src/beta.pas', 'src/alpha.pas'));
  CheckRepository(1, 'duplicate Lazarus registration');
  RestoreFixture;
  WriteOwned(Repo + DirectorySeparator + 'wfc_package.pas', Replace(PACKAGE_TEXT, 'uses alpha, beta;', 'uses alpha, beta, alpha;'));
  CheckRepository(1, 'duplicate package uses registration');
  RestoreFixture;
  WriteOwned(Repo + DirectorySeparator + 'src' + DirectorySeparator + 'alpha.pas',
    '{ unit alpha; } unit other; interface implementation end.');
  CheckRepository(1, 'filename and actual declaration mismatch');
  RestoreFixture;
  WriteOwned(Repo + DirectorySeparator + 'src' + DirectorySeparator + 'alpha.pas',
    'program alpha; const bait = ''unit alpha;''; begin end.');
  CheckRepository(1, 'program cannot impersonate a unit');
  RestoreFixture;
  WriteOwned(Repo + DirectorySeparator + 'src' + DirectorySeparator + 'UPPER.pas',
    'unit upper; interface implementation end.');
  CheckRepository(1, 'noncanonical filename case');
  Check(SysUtils.DeleteFile(Repo + DirectorySeparator + 'src' + DirectorySeparator + 'UPPER.pas'), 'remove owned uppercase fixture');
  RestoreFixture;
  WriteOwned(Repo + DirectorySeparator + 'outside.pas', 'never evaluate or modify this source');
  WriteOwned(FixtureParent + DirectorySeparator + 'outside.pas', 'outside-root sentinel');
  WriteOwned(Repo + DirectorySeparator + 'fpmake.pp', Replace(FPM_TEXT, '''beta.pas''', '''../outside.pas'''));
  CheckRepository(1, 'out-of-source fpmake path rejected');
  RestoreFixture;
  WriteOwned(Repo + DirectorySeparator + 'wfc.lpk', Replace(LPK_TEXT, 'src/beta.pas', '../outside.pas'));
  CheckRepository(1, 'out-of-root Lazarus path rejected');
  Check(ReadOwned(Repo + DirectorySeparator + 'outside.pas') = 'never evaluate or modify this source',
    'out-of-scope file remains unchanged');
  Check(ReadOwned(FixtureParent + DirectorySeparator + 'outside.pas') = 'outside-root sentinel',
    'outside-root file remains unchanged');
  RestoreFixture;
  WriteOwned(Repo + DirectorySeparator + 'fpmake.pp', StringOfChar(' ', 1048577));
  CheckRepository(1, 'oversized manifest rejected');
  RestoreFixture;
  WriteOwned(Repo + DirectorySeparator + 'src' + DirectorySeparator + 'alpha.pas', StringOfChar(' ', 1048577));
  CheckRepository(1, 'oversized source rejected');
  RestoreFixture;
  Check(SysUtils.DeleteFile(Repo + DirectorySeparator + 'wfc.lpk'), 'remove owned manifest for I/O fixture');
  CheckRepository(3, 'missing manifest reports I/O');
  RestoreFixture;
end;

procedure TestArguments;
begin
  RunCheck([], 2, 'missing required root');
  RunCheck(['--root'], 2, 'missing root value');
  RunCheck(['--root', ''], 2, 'quoted empty root');
  RunCheck(['--root', '--help'], 2, 'option cannot replace root value');
  RunCheck(['--root', Repo, '--root', Repo], 2, 'duplicate root');
  RunCheck(['--unknown'], 2, 'unknown flag');
  RunCheck([Repo], 2, 'positional root rejected');
  RunCheck(['--help', '--version'], 2, 'standalone modes cannot mix');
  RunCheck(['--root', Repo, '--help'], 2, 'help cannot hide extra arguments');
  RunCheck(['--help'], 0, 'help', 'Usage: wfc_package_check --root DIRECTORY');
  RunCheck(['--version'], 0, 'version', 'wfc_package_check 1');
  RunCheck(['--root', Repo + DirectorySeparator + 'does-not-exist'], 3, 'nonexistent root');
end;

function CreateFixtureLink(const ALink, ATarget: String; const ADirectory: Boolean): Boolean;
{$IFDEF MSWINDOWS}
type TCreateSymbolicLink = function(ALink, ATarget: PChar; AFlags: DWORD): Byte; stdcall;
var LCreate: TCreateSymbolicLink; LFlags: DWORD;
{$ENDIF}
begin
  {$IFDEF MSWINDOWS}
  LCreate := TCreateSymbolicLink(GetProcAddress(GetModuleHandle('kernel32.dll'), 'CreateSymbolicLinkA'));
  LFlags := 2; if ADirectory then LFlags := LFlags or 1;
  Result := Assigned(LCreate) and (LCreate(PChar(ALink), PChar(ATarget), LFlags) <> 0);
  {$ELSE}
  Result := fpSymlink(PChar(ATarget), PChar(ALink)) = 0;
  {$ENDIF}
end;

procedure TestLink;
var LLink, LTarget: String; LCreated: Boolean;
begin
  LLink := Repo + DirectorySeparator + 'src' + DirectorySeparator + 'linked.pas';
  LTarget := FixtureParent + DirectorySeparator + 'outside.pas';
  WriteOwned(LTarget, 'unit linked; interface implementation end.');
  LCreated := CreateFixtureLink(LLink, LTarget, False);
  {$IFNDEF MSWINDOWS}
  Check(LCreated, 'create owned Unix symbolic-link fixture');
  {$ENDIF}
  if LCreated then
  try
    RunCheck(['--root', Repo], 1, 'linked source cannot escape root', 'ordinary file');
    Check(ReadOwned(LTarget) = 'unit linked; interface implementation end.', 'link target unchanged');
  finally Check(SysUtils.DeleteFile(LLink), 'remove only owned source link'); end
  else WriteLn('Windows symbolic-link fixture unavailable without host privilege; no link proof claimed.');
  if not LCreated then Exit;
  LLink := FixtureParent + DirectorySeparator + 'linked-root';
  Check(CreateFixtureLink(LLink, Repo, True), 'create owned directory link fixture');
  try
    RunCheck(['--root', LLink], 1, 'linked root rejected', 'directory');
    Check(ReadOwned(Repo + DirectorySeparator + 'wfc.lpk') = LPK_TEXT, 'linked root target unchanged');
  finally
    {$IFDEF MSWINDOWS}
    Check(RemoveDir(LLink), 'remove only owned directory link');
    {$ELSE}
    Check(SysUtils.DeleteFile(LLink), 'remove only owned directory link');
    {$ENDIF}
  end;
  LLink := Repo + DirectorySeparator + 'wfc.lpk';
  Check(SysUtils.DeleteFile(LLink), 'remove owned regular manifest for link fixture');
  Check(CreateFixtureLink(LLink, LTarget, False), 'create owned manifest link fixture');
  try RunCheck(['--root', Repo], 1, 'linked manifest rejected before content parse', 'ordinary file');
  finally Check(SysUtils.DeleteFile(LLink), 'remove only owned manifest link'); end;
  RestoreFixture;
end;

procedure Cleanup;
const Names: array[0..7] of String = ('alpha.pas', 'beta.pas', 'UPPER.pas', 'linked.pas',
  'fpmake.pp', 'wfc.lpk', 'wfc_package.pas', 'outside.pas');
var I: Integer; LPath: String;
begin
  for I := 0 to High(Names) do
  begin
    if I < 4 then LPath := Repo + DirectorySeparator + 'src' + DirectorySeparator + Names[I]
    else LPath := Repo + DirectorySeparator + Names[I];
    if FileExists(LPath) then Check(SysUtils.DeleteFile(LPath), 'remove exact owned fixture file');
  end;
  LPath := FixtureParent + DirectorySeparator + 'outside.pas';
  if FileExists(LPath) then Check(SysUtils.DeleteFile(LPath), 'remove exact owned outside fixture');
  Check(RemoveDir(Repo + DirectorySeparator + 'src'), 'remove empty source fixture directory');
  Check(RemoveDir(Repo), 'remove empty repository fixture directory');
  Check(RemoveDir(Elsewhere), 'remove empty working fixture directory');
  Check(RemoveDir(FixtureParent), 'remove empty owned fixture parent');
end;

var LId: TGuid;
begin
  try
    if ParamCount <> 1 then raise Exception.Create('expected checker executable path');
    Checker := ExpandFileName(ParamStr(1));
    if not FileExists(Checker) then raise Exception.Create('checker executable is unavailable');
    if CreateGuid(LId) <> 0 then raise Exception.Create('cannot name owned package fixture');
    FixtureParent := IncludeTrailingPathDelimiter(ExtractFilePath(ExpandFileName(ParamStr(0)))) +
      'package-check-' + GuidToString(LId);
    Check(CreateDir(FixtureParent), 'create fresh owned fixture parent');
    Repo := FixtureParent + DirectorySeparator + 'repo with spaces';
    Elsewhere := FixtureParent + DirectorySeparator + 'working';
    Check(CreateDir(Repo), 'create repository fixture');
    Check(CreateDir(Repo + DirectorySeparator + 'src'), 'create source fixture');
    Check(CreateDir(Elsewhere), 'create independent working directory');
    try
      TestArguments; TestManifests; TestLink;
    finally Cleanup; end;
    WriteLn('Package checker process checks: ', Checks);
  except on E: Exception do begin WriteLn(StdErr, 'wfc_package_check_process_test: ', E.Message); Halt(1); end; end;
end.
