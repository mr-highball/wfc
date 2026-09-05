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
unit wfc_browser_test_host;
{$mode delphi}{$H+}
interface
uses SysUtils {$IFDEF PAS2JS}, JS, Web{$ENDIF};
type
  EWfcBrowserTestExit = class(Exception)
  public
    ExitStatus: Integer;
  end;
{ Standalone conformance programs use the same explicit exit contract in a
  browser. This adapter has no filesystem, process or network dependency. }
procedure Halt(const AExitStatus: Integer = 0);
implementation
{$IFDEF PAS2JS}
procedure WriteTestOutput(S: JSValue; NewLine: Boolean);
var E: TJSElement; T: String;
begin
  E := document.getElementById('test-output');
  if E = nil then
  begin
    E := document.createElement('pre');
    E.id := 'test-output';
    document.body.appendChild(E);
  end;
  T := E.textContent + String(S);
  if NewLine then T := T + #10;
  if Length(T) > 65536 then T := Copy(T, Length(T) - 65535, 65536);
  E.textContent := T;
end;
{$ENDIF}
procedure Halt(const AExitStatus: Integer);
var E: EWfcBrowserTestExit;
begin
  System.ExitCode := AExitStatus;
  E := EWfcBrowserTestExit.Create('Pascal test exit ' + IntToStr(AExitStatus));
  E.ExitStatus := AExitStatus;
  raise E;
end;
{$IFDEF PAS2JS}
initialization
  SetWriteCallBack(@WriteTestOutput);
{$ENDIF}
end.
