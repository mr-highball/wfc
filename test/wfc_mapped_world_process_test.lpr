{ SPDX-License-Identifier: MIT
  Copyright (c) 2021 mr-highball }
program wfc_mapped_world_process_test;
{$mode delphi}{$H+}
uses Classes, SysUtils, Process, wfc_process_test_support;
var Checks, Cases: Integer;
procedure Check(const ACondition: Boolean; const AMessage: String);
begin
  Inc(Checks);
  if not ACondition then raise Exception.Create(AMessage);
end;
procedure ReadAvailable(const AProcess: TProcess; var AText: String);
var Buffer: array[0..4095] of Byte; N, I, Offset: Integer;
begin
  while AProcess.Output.NumBytesAvailable > 0 do
  begin
    N := AProcess.Output.NumBytesAvailable;
    if N > SizeOf(Buffer) then N := SizeOf(Buffer);
    N := AProcess.Output.Read(Buffer[0], N);
    if N <= 0 then Exit;
    if Length(AText) > 1048576 - N then raise Exception.Create('child output exceeded one MiB test containment');
    Offset := Length(AText);
    SetLength(AText, Offset + N);
    for I := 0 to N - 1 do AText[Offset + I + 1] := Chr(Buffer[I]);
  end;
end;
function Invoke(const AExecutable: String; const Args: array of String; const Expected: Integer): String;
var Child: TProcess; I, Actual: Integer; Started: QWord;
begin
  Inc(Cases);
  Result := '';
  Child := TProcess.Create(nil);
  try
    Child.Executable := AExecutable;
    for I := 0 to High(Args) do Child.Parameters.Add(Args[I]);
    Child.Options := [poUsePipes, poStderrToOutput, poNoConsole];
    Child.Execute;
    Started := GetTickCount64;
    repeat
      ReadAvailable(Child, Result);
      if not Child.Running then Break;
      if GetTickCount64 - Started > 60000 then raise Exception.Create('native CLI process deadline exceeded');
      Sleep(5);
    until False;
    ReadAvailable(Child, Result);
    Actual := WfcProcessExitCode(Child);
    Check(Actual = Expected, Format('case %d expected exit %d, got %d: %s', [Cases, Expected, Actual, Result]));
    WriteLn('case ', Cases, ': exit ', Actual, ' accepted');
  finally
    if Child.Running then
    begin
      Child.Terminate(1);
      if not Child.WaitOnExit(5000) then raise Exception.Create('owned child cleanup deadline exceeded');
    end;
    Child.Free;
  end;
end;
function ReadSmall(const Path: String): String;
var F: TFileStream;
begin
  F := TFileStream.Create(Path, fmOpenRead or fmShareDenyNone);
  try
    Check((F.Size > 0) and (F.Size <= 1048576), 'artifact size within test containment');
    SetLength(Result, F.Size);
    F.ReadBuffer(Result[1], Length(Result));
  finally F.Free; end;
end;
procedure Marker(const Path: String);
const Text = 'caller-owned-preserve-me';
var F: TFileStream;
begin
  if FileExists(Path) or DirectoryExists(Path) then raise Exception.Create('marker needs a fresh private path');
  F := TFileStream.Create(Path, fmCreate);
  try F.WriteBuffer(Text[1], Length(Text)); finally F.Free; end;
end;
function HasPartial(const Path: String): Boolean;
var Search: TSearchRec;
begin
  Result := FindFirst(Path + '.partial-*', faAnyFile, Search) = 0;
  if Result then FindClose(Search);
end;
procedure Absent(const Path: String);
begin
  Check(not FileExists(Path) and not DirectoryExists(Path), 'refused output absent: ' + Path);
  Check(not HasPartial(Path), 'no partial output remains: ' + Path);
end;
procedure BadArguments(const Exe, Base: String);
var Text: String;
begin
  Text := Invoke(Exe, ['--help'], 0);
  Check((Pos('--repair housing|foliage|all', Text) > 0) and (Pos('never replaced', Text) > 0), 'help documents scope and exclusive publication');
  Text := Invoke(Exe, ['--version'], 0);
  Check(Pos('MappedWorld 1 mapping=1', Text) > 0, 'public version');
  Text := Invoke(Exe, ['--selftest'], 0);
  Check(Pos('Mapped world self-test passed:', Text) > 0, 'shared selftest actually executed');
  Invoke(Exe, ['--unknown'], 1);
  Invoke(Exe, ['--selftest', '--seed', '3'], 1);
  Invoke(Exe, ['--seed'], 1);
  Invoke(Exe, ['--seed', '--trace'], 1);
  Invoke(Exe, ['--seed='], 1);
  Invoke(Exe, ['--seed=-1'], 1);
  Invoke(Exe, ['--seed=+1'], 1);
  Invoke(Exe, ['--seed=1.0'], 1);
  Invoke(Exe, ['--seed=1e2'], 1);
  Invoke(Exe, ['--seed=0x1'], 1);
  Invoke(Exe, ['--seed= 1'], 1);
  Invoke(Exe, ['--seed=4294967296'], 1);
  Invoke(Exe, ['--seed=3', '--seed', '3'], 1);
  Invoke(Exe, ['--backtracks=2147483648'], 1);
  Invoke(Exe, ['--pass-backtracks=-1'], 1);
  Invoke(Exe, ['--trace=1'], 1);
  Invoke(Exe, ['--trace', '--trace'], 1);
  Invoke(Exe, ['--ordinary', '--negotiated'], 1);
  Invoke(Exe, ['--preset=other'], 1);
  Invoke(Exe, ['--sampling=other'], 1);
  Invoke(Exe, ['--repair=other'], 1);
  Invoke(Exe, ['--demand=0,0=house'], 1);
  Invoke(Exe, ['--demand=3,0=house', '--repair=all'], 1);
  Invoke(Exe, ['--demand=0,2=house', '--repair=all'], 1);
  Invoke(Exe, ['--demand=0,0=required', '--repair=all'], 1);
  Invoke(Exe, ['--demand=0,0=house', '--demand=0,0=optional', '--repair=all'], 1);
  Invoke(Exe, ['--domain=foliage:7,7=water', '--repair=foliage'], 1);
  Invoke(Exe, ['--domain=foliage:7,7=tree|tree', '--repair=foliage'], 1);
  Invoke(Exe, ['--domain=foliage:7,7=clear|tree|clear', '--repair=foliage'], 1);
  Invoke(Exe, ['--domain=foliage:7,7=', '--repair=foliage'], 1);
  Invoke(Exe, ['--domain=foliage:7,7=clear', '--clear-domain=foliage:7,7', '--repair=foliage'], 1);
  Invoke(Exe, ['--lock=foliage:7,7=tree', '--unlock=foliage:7,7', '--repair=foliage'], 1);
  Invoke(Exe, ['--lock=foliage:7,7=land', '--repair=foliage'], 1);
  Invoke(Exe, ['--domain=plants:0,0=tree', '--repair=all'], 1);
  Invoke(Exe, ['--region=0,0,0:8,8,1'], 1);
  Invoke(Exe, ['--sampling=region', '--region=0,0:8,8,1'], 1);
  Invoke(Exe, ['--sampling=region', '--region=0,0,0:8,8,1,0'], 1);
  Invoke(Exe, ['--sampling=region', '--region=0,0,0:8,8,1:0'], 1);
  Invoke(Exe, ['--sampling=region', '--region=0,0,0:0,8,1'], 1);
  Invoke(Exe, ['--sampling=region', '--region=-2147483649,0,0:8,8,1'], 1);
  Invoke(Exe, ['--sampling=region', '--region=0,0,0:2147483647,8,1'], 1);
  Invoke(Exe, ['--land-weight=5'], 1);
  Invoke(Exe, ['--preset=sandbox', '--land-weight=0'], 1);
  Invoke(Exe, ['--preset=sandbox', '--land-weight=2147483648'], 1);
  Invoke(Exe, ['--inspect=-1,0'], 1);
  Invoke(Exe, ['--inspect=0,0,0'], 1);
  Invoke(Exe, ['--svg=' + Base + '/must-not-exist.svg', '--diagnostic-svg=' + Base + '/also-absent.svg'], 1);
  Absent(Base + '/must-not-exist.svg');
  Absent(Base + '/also-absent.svg');
end;
procedure Workflows(const Exe, Base: String);
var Text, Safe, Replay, Unsafe, Stale, Existing: String;
begin
  Safe := Base + '/safe.svg'; Replay := Base + '/replay.svg';
  Unsafe := Base + '/unsafe.svg'; Stale := Base + '/stale.svg'; Existing := Base + '/existing.svg';
  Text := Invoke(Exe, [], 0);
  Check((Pos('seed=3 preset=interior', Text) > 0) and (Pos('no caller locks before edits', Text) > 0), 'default tutorial disclosure');
  Check((Pos('physical-blocker layer=foliage cell=7,7,0 value=tree', Text) > 0) and (Pos('current=TRUE', Text) > 0), 'generated interior blocker visible without house demand');
  Text := Invoke(Exe, ['--demand=0,0=house', '--repair=housing'], 2);
  Check((Pos('phase=baseline;', Text) > 0) and (Pos('phase=apply-edits count=1', Text) > 0) and (Pos('phase=explicit-repair;', Text) > 0), 'explicit baseline then edit then requested repair');
  Check(Pos('current=FALSE', Text) > 0, 'failed repair inspection is not current');
  Text := Invoke(Exe, ['--demand=0,0=house', '--repair=foliage', '--trace', '--svg=' + Safe], 0);
  Check(Pos('selected-model-valid=TRUE physical-safe=TRUE', Text) > 0, 'authorized upstream repair physically verified');
  Check(Pos('CURRENT / MODEL VALID / PHYSICAL POLICY SAFE', ReadSmall(Safe)) > 0, 'safe SVG has current independent-validity caption');
  Invoke(Exe, ['--demand', '0,0=house', '--repair', 'foliage', '--trace', '--svg', Replay], 0);
  Check(ReadSmall(Safe) = ReadSmall(Replay), 'deterministic same-run SVG replay bytes');
  Invoke(Exe, ['--sampling=point', '--demand=0,0=house', '--repair=housing', '--svg=' + Unsafe], 3);
  Absent(Unsafe);
  Text := Invoke(Exe, ['--sampling=point', '--demand=0,0=house', '--repair=housing', '--diagnostic-svg=' + Unsafe], 3);
  Check(Pos('UNSAFE STUDY', Text) > 0, 'weak model CLI warns unsafe');
  Check(Pos('DIAGNOSTIC / UNSAFE STUDY / NOT SAFE OUTPUT', ReadSmall(Unsafe)) > 0, 'unsafe SVG is visibly diagnostic');
  Invoke(Exe, ['--demand=0,0=house', '--repair=housing', '--diagnostic-svg=' + Stale], 2);
  Check(Pos('DIAGNOSTIC / NOT CURRENT / RETAINED BASELINE', ReadSmall(Stale)) > 0, 'failed-repair diagnostic does not impersonate current output');
  Marker(Existing);
  Invoke(Exe, ['--svg=' + Existing], 1);
  Check(ReadSmall(Existing) = 'caller-owned-preserve-me', 'preexisting artifact remains byte-identical');
  Check(not HasPartial(Existing), 'existing-path refusal has no partial');
  Invoke(Exe, ['--svg=' + Base + '/no-parent/output.svg'], 1);
  Absent(Base + '/no-parent/output.svg');
  Invoke(Exe, ['--demand=0,0=house', '--repair=housing', '--svg=' + Base + '/failed-safe.svg'], 2);
  Absent(Base + '/failed-safe.svg');
  Invoke(Exe, ['--domain=terrain:0,0=water', '--repair=housing'], 3);
  Text := Invoke(Exe, ['--domain=foliage:7,7=clear', '--repair=foliage'], 0);
  Check(Pos('value=clear generated=TRUE locked=FALSE zoned=TRUE', Text) > 0, 'domain edit remains generated not locked');
  Text := Invoke(Exe, ['--lock=foliage:7,7=clear', '--repair=foliage'], 0);
  Check(Pos('value=clear generated=FALSE locked=TRUE', Text) > 0, 'caller lock is distinctly owned');
  Invoke(Exe, ['--clear-domain=foliage:7,7', '--repair=foliage'], 1);
  Invoke(Exe, ['--unlock=foliage:7,7', '--repair=foliage'], 1);
  Invoke(Exe, ['--domain=foliage:7,7=none', '--repair=foliage'], 2);
  Invoke(Exe, ['--sampling=region', '--region=0,0,0:8,8,1', '--demand=0,0=house', '--repair=foliage'], 0);
  Invoke(Exe, ['--sampling=region', '--region=-1,-1,0:9,9,1', '--demand=0,0=house', '--repair=foliage'], 0);
  Invoke(Exe, ['--seed=4294967295', '--inspect=2,1', '--backtracks=0', '--pass-backtracks=0'], 0);
  Invoke(Exe, ['--ordinary', '--demand=0,0=house', '--repair=housing'], 2);
  Text := Invoke(Exe, ['--preset=sandbox', '--seed=42', '--land-weight=3', '--water-weight=1',
    '--clear-weight=4', '--tree-weight=2', '--domain=foliage:7,7=clear',
    '--lock=terrain:1,1=land', '--repair=all'], 0);
  Check((Pos('preset=landscape-sandbox', Text) > 0) and (Pos('selected-model-valid=TRUE physical-safe=TRUE', Text) > 0), 'sandbox genuine generation and full validation');
  Check(Pos('value=land generated=FALSE locked=TRUE', Text) > 0, 'sandbox edit preserves visible caller ownership');
  Check(Pos('value=clear generated=TRUE locked=FALSE zoned=TRUE', Text) > 0, 'sandbox clear domain remains generated');
  Invoke(Exe, ['--lock=foliage:7,7=tree', '--demand=0,0=house', '--repair=foliage'], 2);
  Text := Invoke(Exe, ['--demand=0,0=house', '--repair=foliage', '--pass-backtracks=0'], 2);
  Check(Pos('pass-limit', Text) > 0, 'insufficient negotiation budget reports a limit not infeasibility');
  Invoke(Exe, ['--domain=terrain:0,0=water', '--repair=housing', '--svg=' + Base + '/scope-safe.svg'], 3);
  Absent(Base + '/scope-safe.svg');
  Invoke(Exe, ['--svg=' + Base], 1);
  Check(DirectoryExists(Base) and not HasPartial(Base), 'existing directory is preserved without partial output');
  Invoke(Exe, ['--sampling=region', '--region=-2147483648,0,0:8,8,1'], 0);
end;
var Base, Exe: String;
begin
  try
    if ParamCount <> 2 then raise Exception.Create('usage: wfc_mapped_world_process_test MAPPEDWORLD OUTPUT-PARENT');
    Exe := ExpandFileName(ParamStr(1));
    if not FileExists(Exe) then raise Exception.Create('CLI executable missing');
    Base := IncludeTrailingPathDelimiter(ExpandFileName(ParamStr(2)));
    if not DirectoryExists(Base) then raise Exception.Create('private parent missing');
    Base := Base + 'process-' + IntToHex(GetProcessID,8) + '-' + IntToHex(GetTickCount64,16);
    if not CreateDir(Base) then raise Exception.Create('cannot create fresh private process fixture');
    WriteLn('Evidence directory: ', Base);
    BadArguments(Exe, Base);
    Workflows(Exe, Base);
    WriteLn('MappedWorld process cases: ', Cases, '; checks: ', Checks, '/', Checks);
  except
    on E:Exception do begin WriteLn(StdErr, 'FAIL: ', E.Message); ExitCode := 1; end;
  end;
end.
