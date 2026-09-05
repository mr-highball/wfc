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
program ConnectedRoutes;

{$mode delphi}{$H+}

uses
  SysUtils,
  wfc,
  connected_routes_demo,
  wfc_atomic_new_file;

type
  TConnectedRoutesArguments = record
    Config: TConnectedRoutesConfig;
    HasRepair: Boolean;
    RepairPortal: TConnectedRoutesPortal;
    SvgPath: String;
  end;

procedure Usage;
begin
  WriteLn('ConnectedRoutes [OPTIONS]');
  WriteLn('ConnectedRoutes --selftest');
  WriteLn('  --case town|circulation');
  WriteLn('  --seed UINT32');
  WriteLn('  --portal first|second|both|none');
  WriteLn('  --repair-to first|second|both|none');
  WriteLn('  --required-only');
  WriteLn('  --backtracks NONNEGATIVE');
  WriteLn('  --pass-backtracks NONNEGATIVE');
  WriteLn('  --trace');
  WriteLn('  --svg NEW-FILE');
  WriteLn('A repair first commits the configured portal, then changes only the');
  WriteLn('route pass and its descendants. Existing SVG paths are never replaced.');
end;

function ParseUnsigned(const AText, AName: String;
  const AMaximum: Cardinal): Cardinal;
var
  I: Integer;
  LDigit: Cardinal;
begin
  Result := 0;
  if AText = '' then
    raise EConnectedRoutesDemo.Create(AName + ' requires decimal digits');
  for I := 1 to Length(AText) do
  begin
    if not (AText[I] in ['0'..'9']) then
      raise EConnectedRoutesDemo.Create(AName + ' requires decimal digits');
    LDigit := Cardinal(Ord(AText[I]) - Ord('0'));
    if Result > (AMaximum - LDigit) div 10 then
      raise EConnectedRoutesDemo.Create(AName + ' exceeds its integer range');
    Result := Result * 10 + LDigit;
  end;
end;

function ParseCase(const AText: String): TConnectedRoutesCase;
begin
  if AText = 'town' then Result := crcTown2D
  else if AText = 'circulation' then Result := crcCirculation3D
  else raise EConnectedRoutesDemo.Create(
    'case must be town or circulation');
end;

function ParsePortal(const AText: String): TConnectedRoutesPortal;
begin
  if AText = 'first' then Result := crpFirst
  else if AText = 'second' then Result := crpSecond
  else if AText = 'both' then Result := crpBoth
  else if AText = 'none' then Result := crpNone
  else raise EConnectedRoutesDemo.Create(
    'portal must be first, second, both, or none');
end;

function NextValue(var AIndex: Integer; const AOption: String): String;
begin
  Inc(AIndex);
  if AIndex > ParamCount then
    raise EConnectedRoutesDemo.Create('missing value for ' + AOption);
  Result := ParamStr(AIndex);
end;

function ParseArguments: TConnectedRoutesArguments;
var
  I: Integer;
  LCase: TConnectedRoutesCase;
  LOption, LValue: String;
  LSeenCase, LSeenSeed, LSeenPortal, LSeenRepair, LSeenAll,
    LSeenBacktracks, LSeenPassBacktracks, LSeenTrace, LSeenSvg: Boolean;
begin
  Result := Default(TConnectedRoutesArguments);
  Result.Config := DefaultConnectedRoutesConfig(crcTown2D);
  LSeenCase := False;
  LSeenSeed := False;
  LSeenPortal := False;
  LSeenRepair := False;
  LSeenAll := False;
  LSeenBacktracks := False;
  LSeenPassBacktracks := False;
  LSeenTrace := False;
  LSeenSvg := False;
  I := 1;
  while I <= ParamCount do
  begin
    LOption := ParamStr(I);
    if LOption = '--case' then
    begin
      if LSeenCase then raise EConnectedRoutesDemo.Create('duplicate --case');
      LSeenCase := True;
      LValue := NextValue(I, LOption);
      LCase := ParseCase(LValue);
      Result.Config.CaseKind := LCase;
    end
    else if LOption = '--seed' then
    begin
      if LSeenSeed then raise EConnectedRoutesDemo.Create('duplicate --seed');
      LSeenSeed := True;
      Result.Config.Seed := ParseUnsigned(NextValue(I, LOption),
        'seed', High(Cardinal));
    end
    else if LOption = '--portal' then
    begin
      if LSeenPortal then
        raise EConnectedRoutesDemo.Create('duplicate --portal');
      LSeenPortal := True;
      Result.Config.Portal := ParsePortal(NextValue(I, LOption));
    end
    else if LOption = '--repair-to' then
    begin
      if LSeenRepair then
        raise EConnectedRoutesDemo.Create('duplicate --repair-to');
      LSeenRepair := True;
      Result.HasRepair := True;
      Result.RepairPortal := ParsePortal(NextValue(I, LOption));
    end
    else if LOption = '--required-only' then
    begin
      if LSeenAll then
        raise EConnectedRoutesDemo.Create('duplicate --required-only');
      LSeenAll := True;
      Result.Config.RequireAllParticipants := False;
    end
    else if LOption = '--backtracks' then
    begin
      if LSeenBacktracks then
        raise EConnectedRoutesDemo.Create('duplicate --backtracks');
      LSeenBacktracks := True;
      Result.Config.MaxBacktracks := Integer(ParseUnsigned(
        NextValue(I, LOption), 'backtracks', Cardinal(High(Integer))));
    end
    else if LOption = '--pass-backtracks' then
    begin
      if LSeenPassBacktracks then
        raise EConnectedRoutesDemo.Create('duplicate --pass-backtracks');
      LSeenPassBacktracks := True;
      Result.Config.MaxPassBacktracks := Integer(ParseUnsigned(
        NextValue(I, LOption), 'pass backtracks', Cardinal(High(Integer))));
    end
    else if LOption = '--trace' then
    begin
      if LSeenTrace then raise EConnectedRoutesDemo.Create('duplicate --trace');
      LSeenTrace := True;
      Result.Config.CaptureTrace := True;
    end
    else if LOption = '--svg' then
    begin
      if LSeenSvg then raise EConnectedRoutesDemo.Create('duplicate --svg');
      LSeenSvg := True;
      Result.SvgPath := NextValue(I, LOption);
      if Result.SvgPath = '' then
        raise EConnectedRoutesDemo.Create('SVG output path cannot be empty');
    end
    else
      raise EConnectedRoutesDemo.Create('unknown option: ' + LOption);
    Inc(I);
  end;
end;

function AsciiBytes(const AText: String): TBytes;
var
  I: Integer;
begin
  Result := nil;
  SetLength(Result, Length(AText));
  for I := 1 to Length(AText) do
  begin
    if Ord(AText[I]) > 127 then
      raise EConnectedRoutesDemo.Create('SVG output must be canonical ASCII');
    Result[I - 1] := Byte(Ord(AText[I]));
  end;
end;

procedure WriteSvg(const APath, AText: String);
var
  LBytes: TBytes;
  LFile: TWfcAtomicNewFile;
begin
  LBytes := AsciiBytes(AText);
  LFile := TWfcAtomicNewFile.Create(APath);
  try
    LFile.WriteBytes(LBytes);
    LFile.Publish;
  finally
    LFile.Free;
  end;
end;

procedure PrintResult(const AResult: TConnectedRoutesResult);
begin
  WriteLn('Connected routes v', CONNECTED_ROUTES_DEMO_VERSION,
    ': ', AResult.Status);
  WriteLn('case=', ConnectedRoutesCaseName(AResult.CaseKind),
    ' portal=', ConnectedRoutesPortalName(AResult.Portal),
    ' all-participants=', AResult.RequireAllParticipants);
  WriteLn('shape=', AResult.Width, 'x', AResult.Height, 'x', AResult.Depth,
    ' signature=', AResult.Signature);
  WriteLn(AResult.Detail);
  if AResult.Solved then
    WriteLn('Independent reciprocal-port BFS: passed');
end;

procedure Main;
var
  A: TConnectedRoutesArguments;
  Baseline, FinalResult: TConnectedRoutesResult;
  Session: TConnectedRoutesSession;
begin
  if (ParamCount = 1) and (ParamStr(1) = '--selftest') then
  begin
    WriteLn('Connected routes self-test passed: ', ConnectedRoutesSelfTest);
    Exit;
  end;
  if (ParamCount = 1) and ((ParamStr(1) = '--help') or
      (ParamStr(1) = '-h')) then
  begin
    Usage;
    Exit;
  end;
  if (ParamCount = 1) and (ParamStr(1) = '--version') then
  begin
    WriteLn('ConnectedRoutes ', CONNECTED_ROUTES_DEMO_VERSION);
    Exit;
  end;
  A := ParseArguments;
  Session := TConnectedRoutesSession.Create(A.Config);
  try
    if not Session.Generate(Baseline) then
    begin
      PrintResult(Baseline);
      ExitCode := 2;
      Exit;
    end;
    FinalResult := Baseline;
    if A.HasRepair then
    begin
      Session.SetPortal(A.RepairPortal);
      if not Session.Repair(FinalResult) then
      begin
        PrintResult(FinalResult);
        ExitCode := 2;
        Exit;
      end;
      WriteLn('baseline-signature=', Baseline.Signature,
        ' repair-provider-reused=', FinalResult.ProviderReused);
    end;
    PrintResult(FinalResult);
    if A.SvgPath <> '' then
    begin
      WriteSvg(A.SvgPath, FinalResult.SvgText);
      WriteLn('Wrote ', ExpandFileName(A.SvgPath));
    end;
  finally
    Session.Free;
  end;
end;

begin
  try
    Main;
  except
    on E: Exception do
    begin
      WriteLn(StdErr, 'ConnectedRoutes: ', E.Message);
      ExitCode := 1;
    end;
  end;
end.
