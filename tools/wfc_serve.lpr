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
program wfc_serve;

{$mode delphi}{$H+}

uses
  SysUtils, wfc_serve_http;

procedure Usage;
begin
  WriteLn('Usage: wfc_serve --root DIRECTORY [--port 8000] [--bind ADDRESS] [--max-requests N]');
  WriteLn('Serve static files on 127.0.0.1 by default. Stop with Ctrl+C.');
  WriteLn('--bind selects a literal loopback or private IPv4 address on this computer.');
  WriteLn('LAN access is for trusted networks only: no authentication or TLS.');
  WriteLn('GET/HEAD, no directory listing, no uploads, no script execution.');
  WriteLn('Paths are printable ASCII; links/reparse points are not served.');
  WriteLn('--max-requests bounds accepted connections for automated checks.');
  WriteLn('Use --version alone to print the utility version.');
end;

function DecimalArgument(const AValue: String;
  const AMinimum, AMaximum: Integer): Integer;
var
  I: Integer;
begin
  if AValue = '' then
    raise EWfcServe.Create('empty numeric argument');
  for I := 1 to Length(AValue) do
    if not (AValue[I] in ['0'..'9']) then
      raise EWfcServe.Create('numeric arguments must be decimal digits');
  if not TryStrToInt(AValue, Result) or (Result < AMinimum) or
      (Result > AMaximum) then
    raise EWfcServe.Create('numeric argument is outside the supported range');
end;

procedure Main;
var
  LRoot, LOption, LValue, LBindAddress: String;
  LPort, LMaxRequests, I: Integer;
  LSeenRoot, LSeenPort, LSeenMax, LSeenBind: Boolean;
begin
  if (ParamCount = 1) and (ParamStr(1) = '--version') then
  begin
    WriteLn('WFC static server ', WFC_SERVE_VERSION);
    Exit;
  end;
  LRoot := '';
  LBindAddress := '127.0.0.1';
  LPort := 8000;
  LMaxRequests := 0;
  LSeenRoot := False;
  LSeenPort := False;
  LSeenMax := False;
  LSeenBind := False;
  I := 1;
  while I <= ParamCount do
  begin
    LOption := ParamStr(I);
    if (LOption = '--help') or (LOption = '-h') then
    begin
      Usage;
      Exit;
    end;
    Inc(I);
    if I > ParamCount then
      raise EWfcServe.Create('missing value for ' + LOption);
    LValue := ParamStr(I);
    if LOption = '--root' then
    begin
      if LSeenRoot then
        raise EWfcServe.Create('duplicate --root');
      LSeenRoot := True;
      LRoot := LValue;
    end
    else if LOption = '--port' then
    begin
      if LSeenPort then
        raise EWfcServe.Create('duplicate --port');
      LSeenPort := True;
      LPort := DecimalArgument(LValue, 1, 65535);
    end
    else if LOption = '--bind' then
    begin
      if LSeenBind then
        raise EWfcServe.Create('duplicate --bind');
      LSeenBind := True;
      if not ValidWfcServeBindAddress(LValue) then
        raise EWfcServe.Create('bind address must be a canonical loopback or private IPv4 address');
      LBindAddress := LValue;
    end
    else if LOption = '--max-requests' then
    begin
      if LSeenMax then
        raise EWfcServe.Create('duplicate --max-requests');
      LSeenMax := True;
      LMaxRequests := DecimalArgument(LValue, 1, High(Integer));
    end
    else
      raise EWfcServe.Create('unknown option: ' + LOption);
    Inc(I);
  end;
  if not LSeenRoot then
    raise EWfcServe.Create('--root is required');
  RunWfcServe(LRoot, LPort, LMaxRequests, LBindAddress);
end;

begin
  try
    Main;
  except
    on E: Exception do
    begin
      WriteLn(StdErr, 'wfc_serve: ', E.Message);
      ExitCode := 1;
    end;
  end;
end.
