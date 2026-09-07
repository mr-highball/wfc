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
program wfc_browser_args_test;

{$mode delphi}{$H+}
{$IFDEF PAS2JS}{$FATAL browser host argument tests require native FPC}{$ENDIF}

uses Classes, SysUtils, wfc_browser_args, wfc_browser_dom;

var Checks: Integer;

procedure Check(const ACondition: Boolean; const AMessage: String);
begin
  Inc(Checks);
  if not ACondition then raise Exception.Create(AMessage);
end;

function MusicText: UnicodeString;
begin
  Result := WideChar($00E9) + UnicodeString(WideChar($D83C)) + WideChar($DFB5);
end;

function MusicBytes: String;
begin
  Result := #$C3#$A9#$F0#$9F#$8E#$B5;
end;

procedure RejectUtf16(const AValue: UnicodeString);
var LRaised: Boolean;
begin
  LRaised := False;
  try WfcBrowserUtf8ArgumentBytes(AValue);
  except on EWfcBrowserArguments do LRaised := True;end;
  Check(LRaised, 'isolated UTF-16 surrogate must reject');
end;

procedure TestEncoding;
var LText: UnicodeString; LCodePage: TSystemCodePage;
begin
  LCodePage := DefaultSystemCodePage;
  Check(WfcBrowserUtf8ArgumentBytes('') = '', 'empty UTF-16 value');
  Check(WfcBrowserUtf8ArgumentBytes('plain "quoted" text\') = 'plain "quoted" text\', 'ASCII bytes');
  Check(WfcBrowserUtf8ArgumentBytes(WideChar($7F)) = #$7F, 'one-byte boundary');
  Check(WfcBrowserUtf8ArgumentBytes(WideChar($80)) = #$C2#$80, 'two-byte start');
  Check(WfcBrowserUtf8ArgumentBytes(WideChar($7FF)) = #$DF#$BF, 'two-byte end');
  Check(WfcBrowserUtf8ArgumentBytes(WideChar($800)) = #$E0#$A0#$80, 'three-byte start');
  Check(WfcBrowserUtf8ArgumentBytes(WideChar($D7FF)) = #$ED#$9F#$BF, 'before surrogates');
  Check(WfcBrowserUtf8ArgumentBytes(WideChar($E000)) = #$EE#$80#$80, 'after surrogates');
  Check(WfcBrowserUtf8ArgumentBytes(WideChar($FFFF)) = #$EF#$BF#$BF, 'BMP end');
  LText := UnicodeString(WideChar($D800)) + WideChar($DC00);
  Check(WfcBrowserUtf8ArgumentBytes(LText) = #$F0#$90#$80#$80, 'supplementary start');
  LText := UnicodeString(WideChar($DBFF)) + WideChar($DFFF);
  Check(WfcBrowserUtf8ArgumentBytes(LText) = #$F4#$8F#$BF#$BF, 'last Unicode scalar');
  Check(WfcBrowserUtf8ArgumentBytes(MusicText) = MusicBytes, 'BMP and emoji exact UTF-8 bytes');
  LText := 'data-unicode=' + MusicText;
  Check(WfcBrowserUtf8ArgumentBytes(LText) = 'data-unicode=' + MusicBytes, 'expectation assignment preserves UTF-8');
  RejectUtf16(WideChar($D800));
  RejectUtf16(WideChar($DBFF));
  RejectUtf16(WideChar($DC00));
  RejectUtf16(WideChar($DFFF));
  RejectUtf16(UnicodeString(WideChar($D800)) + 'x');
  RejectUtf16(UnicodeString(WideChar($D800)) + WideChar($D800));
  RejectUtf16(UnicodeString(WideChar($D800)) + WideChar($DC00) + WideChar($DC00));
  Check(DefaultSystemCodePage = LCodePage, 'encoding does not change the global code page');
end;

procedure AssertUnicodeDom(const AExpectation: String);
var LActual, LExpected: TStringList; LRaised: Boolean;
begin
  LActual := WfcBrowserBodyAttributes('<body data-self-test="passed" data-unicode="&#233;&#x1F3B5;" data-empty=""></body>');
  LExpected := TStringList.Create;
  try
    LExpected.CaseSensitive := True;
    LExpected.Add('data-self-test=passed');
    LExpected.Add(AExpectation);
    LExpected.Add('data-empty=');
    Check(LExpected.Values['data-unicode'] = MusicBytes, 'TStringList retains exact UTF-8 bytes');
    Check(LActual.Values['data-unicode'] = MusicBytes, 'DOM entities decode to the same bytes');
    WfcBrowserAssertBody(LActual, LExpected);
    Check(True, 'Unicode and empty DOM attributes match exactly');
    LExpected.Values['data-unicode'] := MusicBytes + 'x';
    LRaised := False;
    try WfcBrowserAssertBody(LActual, LExpected);
    except on EWfcBrowserDom do LRaised := True;end;
    Check(LRaised, 'Unicode mismatch must still fail');
  finally LExpected.Free; LActual.Free;end;
end;

procedure TestIndices(const AArguments: TWfcBrowserArguments);
var LRaised: Boolean;
begin
  LRaised := False;
  try AArguments.NativeValue(-1);except on EWfcBrowserArguments do LRaised := True;end;
  Check(LRaised, 'negative native argument index');
  LRaised := False;
  try AArguments.Utf8Value(-1);except on EWfcBrowserArguments do LRaised := True;end;
  Check(LRaised, 'negative UTF-8 argument index');
  LRaised := False;
  try AArguments.NativeValue(AArguments.Count + 1);except on EWfcBrowserArguments do LRaised := True;end;
  Check(LRaised, 'native argument index past end');
  LRaised := False;
  try AArguments.Utf8Value(AArguments.Count + 1);except on EWfcBrowserArguments do LRaised := True;end;
  Check(LRaised, 'UTF-8 argument index past end');
end;

{$IFDEF MSWINDOWS}
procedure RejectCommandLine(const ACommandLine: UnicodeString);
var LArguments: TWfcBrowserArguments; LRaised: Boolean;
begin
  LArguments := nil; LRaised := False;
  try LArguments := TWfcBrowserArguments.CreateWindowsCommandLine(ACommandLine);
  except on EWfcBrowserArguments do LRaised := True;end;
  LArguments.Free;
  Check(LRaised, 'invalid command line must reject');
end;

procedure TestWindowsParser;
var LArguments: TWfcBrowserArguments; LText: UnicodeString;
begin
  LText := '"C:\tools with spaces\capture.exe" --empty "" --expect "data-unicode=' + MusicText + '"';
  LArguments := TWfcBrowserArguments.CreateWindowsCommandLine(LText);
  try
    Check(LArguments.Count = 4, 'quoted empty value retains argument count');
    Check(LArguments.NativeValue(0) = 'C:\tools with spaces\capture.exe', 'quoted executable path');
    Check(LArguments.NativeValue(1) = '--empty', 'first option');
    Check(LArguments.NativeValue(2) = '', 'quoted empty native value');
    Check(LArguments.Utf8Value(2) = '', 'quoted empty UTF-8 value');
    Check(LArguments.NativeValue(3) = '--expect', 'index after empty value stays aligned');
    Check(LArguments.Utf8Value(4) = 'data-unicode=' + MusicBytes, 'wide argv BMP and emoji');
    AssertUnicodeDom(LArguments.Utf8Value(4));
    TestIndices(LArguments);
  finally LArguments.Free;end;
  LArguments := TWfcBrowserArguments.CreateWindowsCommandLine('program.exe "a b" "a\"b" "C:\path with space\\" tail');
  try
    Check(LArguments.Count = 4, 'escaped quoting argument count');
    Check(LArguments.NativeValue(1) = 'a b', 'quoted space');
    Check(LArguments.Utf8Value(2) = 'a"b', 'escaped quote');
    Check(LArguments.NativeValue(3) = 'C:\path with space\', 'trailing backslash before closing quote');
    Check(LArguments.NativeValue(4) = 'tail', 'argument after quoted path');
  finally LArguments.Free;end;
  LArguments := TWfcBrowserArguments.CreateWindowsCommandLine('program.exe "" ""');
  try
    Check(LArguments.Count = 2, 'multiple empty argument count');
    Check(LArguments.NativeValue(1) = '', 'first empty argument');
    Check(LArguments.Utf8Value(2) = '', 'last empty argument');
  finally LArguments.Free;end;
  RejectCommandLine('program.exe ' + #0 + 'invisible');
  LText := UnicodeString(StringOfChar('x', 32768));
  RejectCommandLine(LText);
end;
{$ENDIF}

procedure TestActualArguments;
var LArguments: TWfcBrowserArguments;
begin
  LArguments := TWfcBrowserArguments.Create;
  try
    Check(LArguments.Count >= 0, 'actual argument count');
    Check(LArguments.NativeValue(0) <> '', 'actual executable present');
    if LArguments.Count > 0 then
    begin
      Check(LArguments.NativeValue(1) = '--argv-fixture', 'only the explicit argument fixture is accepted');
      Check(LArguments.Count = 5, 'actual quoted empty argument preserves count');
      Check(LArguments.Utf8Value(2) = '', 'actual quoted empty value');
      Check(LArguments.Utf8Value(3) = 'data-unicode=' + MusicBytes, 'actual Unicode argv');
      Check(LArguments.NativeValue(4) = 'a b', 'actual quoted spaces');
      Check(LArguments.Utf8Value(5) = 'a"b', 'actual escaped quote');
      AssertUnicodeDom(LArguments.Utf8Value(3));
    end;
    TestIndices(LArguments);
  finally LArguments.Free;end;
end;

begin
  try
    TestEncoding;
    AssertUnicodeDom(WfcBrowserUtf8ArgumentBytes('data-unicode=' + MusicText));
    {$IFDEF MSWINDOWS}TestWindowsParser;{$ENDIF}
    TestActualArguments;
    WriteLn('Browser argument checks passed: ', Checks);
  except on E: Exception do
    begin WriteLn(StdErr, 'wfc_browser_args_test: ', E.Message); Halt(1);end;
  end;
end.
