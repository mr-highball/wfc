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
unit wfc_browser_args;

{$mode delphi}{$H+}
{$IFDEF PAS2JS}{$FATAL browser host arguments require native FPC}{$ENDIF}

interface

uses SysUtils;

type
  EWfcBrowserArguments = class(Exception);
  TWfcBrowserArguments = class
  private
    {$IFDEF MSWINDOWS}
    FValues: array of UnicodeString;
    procedure ParseWindows(const ACommandLine: UnicodeString);
    {$ELSE}
    FValues: array of String;
    {$ENDIF}
    function GetCount: Integer;
    procedure CheckIndex(const AIndex: Integer);
  public
    constructor Create;
    {$IFDEF MSWINDOWS}
    { Also useful to test Windows quoting without launching a child process. }
    constructor CreateWindowsCommandLine(const ACommandLine: UnicodeString);
    {$ENDIF}
    { Index zero is the executable. Count excludes it. One parser supplies all
      indices, including quoted empty values and escaped quotation marks. }
    property Count: Integer read GetCount;
    { Retains the native tools' existing ANSI filesystem convention on Windows;
      this does not claim arbitrary Unicode filesystem-path support. }
    function NativeValue(const AIndex: Integer): String;
    { UTF-8 BYTES in a String, matching wfc_browser_dom's byte representation.
      No global code-page change or implicit UTF8String-to-ANSI assignment. }
    function Utf8Value(const AIndex: Integer): String;
  end;

{ Encode UTF-16 scalars strictly, returning byte-preserved UTF-8 for the DOM
  assertion API. Reject isolated surrogates rather than silently replacing. }
function WfcBrowserUtf8ArgumentBytes(const AValue: UnicodeString): String;

implementation

{$IFDEF MSWINDOWS}uses Windows, ShellApi;{$ENDIF}

function WfcBrowserUtf8ArgumentBytes(const AValue: UnicodeString): String;
var I, LCode: Integer; LUtf8: UTF8String;
begin
  I := 1;
  while I <= Length(AValue) do
  begin
    LCode := Ord(AValue[I]);
    if (LCode >= $D800) and (LCode <= $DBFF) then
    begin
      Inc(I);
      if (I > Length(AValue)) or (Ord(AValue[I]) < $DC00) or
        (Ord(AValue[I]) > $DFFF) then
        raise EWfcBrowserArguments.Create('argument contains an isolated UTF-16 high surrogate');
    end
    else if (LCode >= $DC00) and (LCode <= $DFFF) then
      raise EWfcBrowserArguments.Create('argument contains an isolated UTF-16 low surrogate');
    Inc(I);
  end;
  LUtf8 := UTF8Encode(AValue);
  SetString(Result, PAnsiChar(LUtf8), Length(LUtf8));
end;

{$IFDEF MSWINDOWS}
procedure TWfcBrowserArguments.ParseWindows(const ACommandLine: UnicodeString);
type
  TWideArguments = array[0..32767] of PWideChar;
  PWideArguments = ^TWideArguments;
var LArguments: pLPWSTR; LCount, I: LongInt;
begin
  if (Length(ACommandLine) > 32767) or (Pos(#0, ACommandLine) <> 0) then
    raise EWfcBrowserArguments.Create('invalid Windows command-line length or embedded NUL');
  { Standard OS parser, not a shell invocation. Its allocation is released once.
    https://learn.microsoft.com/windows/win32/api/shellapi/nf-shellapi-commandlinetoargvw }
  LArguments := CommandLineToArgvW(PWideChar(ACommandLine), @LCount);
  if LArguments = nil then
    raise EWfcBrowserArguments.Create('cannot read Unicode command-line arguments');
  try
    if (LCount < 1) or (LCount > 32768) then
      raise EWfcBrowserArguments.Create('invalid Windows argument count');
    SetLength(FValues, LCount);
    for I := 0 to LCount - 1 do
      FValues[I] := UnicodeString(PWideArguments(LArguments)^[I]);
  finally
    LocalFree(HLOCAL(LArguments));
  end;
end;

constructor TWfcBrowserArguments.CreateWindowsCommandLine(
  const ACommandLine: UnicodeString);
begin
  inherited Create;
  ParseWindows(ACommandLine);
end;
{$ENDIF}

constructor TWfcBrowserArguments.Create;
{$IFNDEF MSWINDOWS}var I: Integer;{$ENDIF}
begin
  inherited Create;
  {$IFDEF MSWINDOWS}
  ParseWindows(UnicodeString(GetCommandLineW));
  {$ELSE}
  SetLength(FValues, System.ParamCount + 1);
  for I := 0 to High(FValues) do FValues[I] := System.ParamStr(I);
  {$ENDIF}
end;

function TWfcBrowserArguments.GetCount: Integer;
begin
  Result := Length(FValues) - 1;
end;

procedure TWfcBrowserArguments.CheckIndex(const AIndex: Integer);
begin
  if (AIndex < 0) or (AIndex >= Length(FValues)) then
    raise EWfcBrowserArguments.Create('argument index is out of range');
end;

function TWfcBrowserArguments.NativeValue(const AIndex: Integer): String;
begin
  CheckIndex(AIndex);
  Result := String(FValues[AIndex]);
end;

function TWfcBrowserArguments.Utf8Value(const AIndex: Integer): String;
begin
  CheckIndex(AIndex);
  {$IFDEF MSWINDOWS}
  Result := WfcBrowserUtf8ArgumentBytes(FValues[AIndex]);
  {$ELSE}
  { Unix argv already contains bytes; preserve them without locale conversion. }
  SetString(Result, PAnsiChar(FValues[AIndex]), Length(FValues[AIndex]));
  {$ENDIF}
end;

end.
