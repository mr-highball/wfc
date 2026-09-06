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
program wfc_browser_capture;

{$mode delphi}{$H+}

uses Classes, SysUtils, wfc_browser_capture_app, wfc_atomic_new_file,
  wfc_browser_socket, wfc_browser_args;

function Decimal(const AValue: String): QWord;
var I: Integer;
begin
  if AValue = '' then raise Exception.Create('unsigned decimal value required');
  for I := 1 to Length(AValue) do
    if not (AValue[I] in ['0'..'9']) then raise Exception.Create('unsigned decimal value required');
  if not TryStrToQWord(AValue, Result) or (Result = 0) then
    raise Exception.Create('positive decimal value is out of range');
end;

function DeadlineAfter(const ADuration: QWord): QWord;
var LNow: QWord;
begin
  LNow := WfcBrowserTickCount64;
  if ADuration > High(QWord) - LNow then raise Exception.Create('deadline addition overflow');
  Result := LNow + ADuration;
end;

function LogText(const AText: String): String;
var I: Integer; LPiece: String;
begin
  Result := '';
  for I := 1 to Length(AText) do
  begin
    if AText[I] in [#32..#126] then LPiece := AText[I]
    else LPiece := '\x' + IntToHex(Ord(AText[I]), 2);
    if Length(Result) + Length(LPiece) + 12 > 8192 then
    begin Result := Result + ' [truncated]'; Break;end;
    Result := Result + LPiece;
  end;
end;

procedure Run(const AArguments: TWfcBrowserArguments);
var
  I, J, P, LOffset, LCount: Integer;
  LProfile, LUrl, LOutput, LKey, LValue: String;
  LDeadline: QWord;
  LHasDeadline: Boolean;
  LExpected: TStringList;
  LFile: TWfcAtomicNewFile;
  LHtml: UTF8String;
  LBlock: array[0..65535] of Byte;
begin
  if (AArguments.Count = 1) and (AArguments.NativeValue(1) = '--version') then
  begin WriteLn('wfc_browser_capture 1'); Exit;end;
  if (AArguments.Count = 1) and (AArguments.NativeValue(1) = '--help') then
  begin
    WriteLn('wfc_browser_capture --prepare-profile NEW-DIRECTORY');
    WriteLn('wfc_browser_capture --deadline-after MILLISECONDS');
    WriteLn('wfc_browser_capture --profile DIRECTORY --url LOOPBACK-URL --dom NEW-FILE');
    WriteLn('  [--deadline MONOTONIC-TICKS | --timeout-ms MILLISECONDS] [--expect NAME=VALUE ...]');
    WriteLn('A fresh prepared profile and data-self-test=passed are always required.');
    WriteLn('The caller owns Chromium startup and bounded process cleanup.');
    Exit;
  end;
  if (AArguments.Count = 2) and (AArguments.NativeValue(1) = '--prepare-profile') then
  begin WfcBrowserPrepareProfile(AArguments.NativeValue(2)); Exit;end;
  if (AArguments.Count = 2) and (AArguments.NativeValue(1) = '--deadline-after') then
  begin WriteLn(DeadlineAfter(Decimal(AArguments.NativeValue(2)))); Exit;end;
  LProfile := ''; LUrl := ''; LOutput := ''; LHasDeadline := False;
  LDeadline := DeadlineAfter(WFC_BROWSER_CAPTURE_TIMEOUT_MS);
  LExpected := TStringList.Create;
  LExpected.CaseSensitive := True;
  LFile := nil;
  try
    I := 1;
    while I <= AArguments.Count do
    begin
      LKey := AArguments.NativeValue(I); Inc(I);
      if I > AArguments.Count then raise Exception.Create('missing option value');
      if LKey = '--expect' then LValue := AArguments.Utf8Value(I)
      else LValue := AArguments.NativeValue(I);
      if LValue = '' then raise Exception.Create('option values must not be empty');
      if LKey = '--profile' then
      begin
        if LProfile <> '' then raise Exception.Create('duplicate profile option');
        LProfile := LValue;
      end
      else if LKey = '--url' then
      begin
        if LUrl <> '' then raise Exception.Create('duplicate URL option');
        LUrl := LValue;
      end
      else if LKey = '--dom' then
      begin
        if LOutput <> '' then raise Exception.Create('duplicate DOM option');
        LOutput := LValue;
      end
      else if (LKey = '--deadline') or (LKey = '--timeout-ms') then
      begin
        if LHasDeadline then raise Exception.Create('deadline options are exclusive and unique');
        LDeadline := Decimal(LValue);
        if LKey = '--timeout-ms' then LDeadline := DeadlineAfter(LDeadline);
        LHasDeadline := True;
      end
      else if LKey = '--expect' then
      begin
        P := Pos('=', LValue);
        if P < 2 then raise Exception.Create('expectation requires NAME=VALUE');
        LKey := Copy(LValue, 1, P - 1);
        for J := 1 to Length(LKey) do
          if not (LKey[J] in ['a'..'z', '0'..'9', '-', '_', ':']) then
            raise Exception.Create('expectation names must be lowercase ASCII');
        if LExpected.IndexOfName(LKey) >= 0 then raise Exception.Create('duplicate expectation');
        LExpected.Add(LValue);
      end
      else raise Exception.Create('unknown capture option');
      Inc(I);
    end;
    if (LProfile = '') or (LUrl = '') or (LOutput = '') then
      raise Exception.Create('profile, URL, and new DOM output are required');
    if LExpected.IndexOfName('data-self-test') < 0 then LExpected.Add('data-self-test=passed');
    if LExpected.Values['data-self-test'] <> 'passed' then
      raise Exception.Create('data-self-test must be expected to pass');
    WfcBrowserValidateUrl(LUrl);
    { Refuse output collisions before connecting to the owned browser. }
    LFile := TWfcAtomicNewFile.Create(LOutput);
    try
      LHtml := WfcBrowserCapture(LProfile, LUrl, LDeadline, LExpected);
      LOffset := 1;
      while LOffset <= Length(LHtml) do
      begin
        WfcBrowserCheckDeadline(LDeadline);
        LCount := Length(LHtml) - LOffset + 1;
        if LCount > SizeOf(LBlock) then LCount := SizeOf(LBlock);
        Move(LHtml[LOffset], LBlock[0], LCount);
        LFile.WriteBytes(Slice(LBlock, LCount));
        Inc(LOffset, LCount);
      end;
      WfcBrowserCheckDeadline(LDeadline);
      LFile.Publish;
      if LFile.CleanupError <> '' then raise Exception.Create('DOM published but partial-file cleanup failed');
      if WfcBrowserTickCount64 >= LDeadline then
        raise Exception.Create('complete DOM published, but publication exceeded the capture deadline');
      WriteLn('Browser completion captured: ', LExpected.Count, ' exact assertions, ', Length(LHtml), ' DOM bytes.');
    except
      on E: Exception do
      begin
        LFile.Cancel;
        if LFile.CleanupError <> '' then E.Message := E.Message + '; cleanup: ' + LFile.CleanupError;
        raise;
      end;
    end;
  finally
    LFile.Free;
    LExpected.Free;
  end;
end;

procedure Main;
var LArguments: TWfcBrowserArguments;
begin
  LArguments := TWfcBrowserArguments.Create;
  try Run(LArguments);finally LArguments.Free;end;
end;

begin
  try Main;
  except on E: Exception do
    begin WriteLn(StdErr, 'wfc_browser_capture: ', LogText(E.Message)); Halt(1);end;
  end;
end.
