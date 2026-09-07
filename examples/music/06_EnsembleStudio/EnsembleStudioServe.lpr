{ SPDX-License-Identifier: MIT
  Copyright (c) 2021 mr-highball }
program EnsembleStudioServe;

{$mode delphi}{$H+}

uses
  SysUtils, wfc_serve_http, ensemble_studio_server;

procedure Usage;
begin
  WriteLn('EnsembleStudioServe --root DIRECTORY [--port 4178] [--bind ADDRESS]');
  WriteLn('  --max-requests N    stop after N accepted connections (test support)');
  WriteLn('Serve the compiled Ensemble Studio and user-requested native WAVE downloads.');
  WriteLn('Default bind: 127.0.0.1; explicit private IPv4 enables trusted LAN access.');
  WriteLn('No authentication or TLS: never expose this development host to the Internet.');
  WriteLn('GET/HEAD only; no uploads, arbitrary commands, output files or path arguments.');
  WriteLn('One connection is processed at a time. The browser download manager owns cancellation.');
  WriteLn('WAVE uses bounded PCM blocks; no song-length policy cap or whole-song buffer.');
  WriteLn('Numeric/file-format limits, compute time, connection and destination capacity still apply.');
  WriteLn('Stop with Ctrl+C. --version alone prints the version.');
end;

function Number(const AText: String; const AMinimum, AMaximum: Integer): Integer;
var I: Integer;
begin
  if AText = '' then raise EWfcServe.Create('empty numeric argument');
  for I := 1 to Length(AText) do
    if not (AText[I] in ['0'..'9']) then
      raise EWfcServe.Create('numeric arguments must be decimal digits');
  if not TryStrToInt(AText, Result) or (Result < AMinimum) or (Result > AMaximum) then
    raise EWfcServe.Create('numeric argument is outside its supported range');
end;

procedure Main;
var
  LRoot, LBind, LOption, LValue: String;
  LPort, LMaximum, I, LKind: Integer;
  LSeen: array[0..3] of Boolean;
  LServer: TEnsembleStudioServer;
begin
  if (ParamCount = 1) and (ParamStr(1) = '--version') then
  begin WriteLn('EnsembleStudioServe 1'); Exit; end;
  if (ParamCount = 1) and ((ParamStr(1) = '--help') or (ParamStr(1) = '-h')) then
  begin Usage; Exit; end;
  LRoot := '';
  LBind := '127.0.0.1';
  LPort := 4178;
  LMaximum := 0;
  for I := 0 to High(LSeen) do LSeen[I] := False;
  I := 1;
  while I <= ParamCount do
  begin
    LOption := ParamStr(I);
    if LOption = '--root' then LKind := 0
    else if LOption = '--port' then LKind := 1
    else if LOption = '--bind' then LKind := 2
    else if LOption = '--max-requests' then LKind := 3
    else raise EWfcServe.Create('unknown option');
    if LSeen[LKind] then raise EWfcServe.Create('duplicate option: ' + LOption);
    LSeen[LKind] := True;
    Inc(I);
    if I > ParamCount then raise EWfcServe.Create('missing option value: ' + LOption);
    LValue := ParamStr(I);
    case LKind of
      0: LRoot := LValue;
      1: LPort := Number(LValue, 1, 65535);
      2: LBind := LValue;
      3: LMaximum := Number(LValue, 1, High(Integer));
    end;
    Inc(I);
  end;
  if not LSeen[0] then raise EWfcServe.Create('--root is required');
  { Validate bind/root before generating a capability; Run checks the opened
    root handle again and binds exclusively through the shared server. }
  if not ValidWfcServeBindAddress(LBind) then
    raise EWfcServe.Create('bind must be a literal loopback or private IPv4 address');
  ValidateWfcServeRoot(LRoot);
  LServer := TEnsembleStudioServer.Create;
  try
    RunWfcServe(LRoot, LPort, LMaximum, LBind, LServer);
  finally
    LServer.Free;
  end;
end;

begin
  try
    Main;
  except
    on E: Exception do
    begin
      WriteLn(StdErr, 'EnsembleStudioServe: ', E.Message);
      ExitCode := 1;
    end;
  end;
end.
