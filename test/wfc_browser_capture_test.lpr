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
program wfc_browser_capture_test;
{$mode delphi}{$H+}
uses Classes, SysUtils, wfc_browser_capture_app, wfc_browser_dom,
  wfc_browser_socket;
var Checks: Integer;

procedure Check(const AValue: Boolean; const AMessage: String);
begin
  Inc(Checks);
  if not AValue then raise Exception.Create(AMessage);
end;

procedure RejectEndpoint(const AText: String);
var LPort: Integer; LPath: String; LRaised: Boolean;
begin
  LRaised := False;
  try WfcBrowserParseEndpoint(AText, LPort, LPath);
  except on EWfcBrowserCapture do LRaised := True;end;
  Check(LRaised, 'invalid endpoint must reject');
end;

procedure RejectUrl(const AText: String);
var LRaised: Boolean;
begin
  LRaised := False;
  try WfcBrowserValidateUrl(AText);
  except on EWfcBrowserCapture do LRaised := True;end;
  Check(LRaised, 'non-loopback/ambiguous URL must reject');
end;

procedure TestProfiles;
var
  LId: TGuid;
  LRoot, LProfile, LMarker, LStarted, LText, LError: String;
  LFile: TFileStream;
  LExpected: TStringList;
  LRaised: Boolean;
begin
  if CreateGuid(LId) <> 0 then raise Exception.Create('cannot name owned profile fixture');
  LRoot := IncludeTrailingPathDelimiter(ExtractFilePath(ExpandFileName(ParamStr(0)))) +
    'capture-profile-' + GuidToString(LId);
  Check(CreateDir(LRoot), 'create new owned fixture parent');
  LProfile := IncludeTrailingPathDelimiter(LRoot) + 'profile';
  LMarker := IncludeTrailingPathDelimiter(LProfile) + WFC_BROWSER_PROFILE_MARKER;
  LStarted := IncludeTrailingPathDelimiter(LProfile) + WFC_BROWSER_CAPTURE_STARTED;
  LExpected := TStringList.Create;
  try
    LExpected.Add('data-self-test=passed');
    LRaised := False;
    try WfcBrowserPrepareProfile(LRoot);
    except on EWfcBrowserCapture do LRaised := True;end;
    Check(LRaised, 'existing profile directory refuses reuse');
    LRaised := False;
    try WfcBrowserPrepareProfile(LProfile + DirectorySeparator + 'missing-parent');
    except on EWfcBrowserCapture do LRaised := True;end;
    Check(LRaised and not DirectoryExists(LProfile), 'missing parent causes no profile creation');
    WfcBrowserPrepareProfile(LProfile);
    Check(DirectoryExists(LProfile) and FileExists(LMarker), 'new prepared profile has ownership marker');
    LFile := TFileStream.Create(LMarker, fmOpenRead or fmShareDenyNone);
    try
      SetLength(LText, LFile.Size);
      if LText <> '' then LFile.ReadBuffer(LText[1], Length(LText));
    finally LFile.Free;end;
    Check(LText = 'WFC fresh browser profile 1'#10, 'exact ownership marker bytes');
    LRaised := False;
    try WfcBrowserCapture(LProfile, 'http://127.0.0.1:4180/test.html', 0, LExpected);
    except on EWfcBrowserSocket do LRaised := True;end;
    Check(LRaised and not FileExists(LStarted), 'expired deadline rejects before consuming profile');
    LRaised := False;
    LError := '';
    try
      WfcBrowserCapture(LProfile, 'http://127.0.0.1:4180/test.html',
        WfcBrowserTickCount64 + 1000, LExpected);
    except on E: EWfcBrowserCapture do begin LRaised := True; LError := E.Message;end;end;
    Check(LRaised and FileExists(LStarted), 'endpoint timeout consumes profile without publishing success');
    Check((Pos('browser socket deadline expired', LError) > 0) and
      (Pos('waiting for the owned browser endpoint', LError) > 0),
      'endpoint timeout includes exact phase, not an unrelated exception');
    LRaised := False;
    try
      WfcBrowserCapture(LProfile, 'http://127.0.0.1:4180/test.html',
        WfcBrowserTickCount64 + 1000, LExpected);
    except on Exception do LRaised := True;end;
    Check(LRaised, 'failed capture profile cannot be retried');
  finally
    LExpected.Free;
    { Only these exact files and empty directories were created by this test;
      unknown contents are never recursively removed. }
    if FileExists(LStarted) then Check(DeleteFile(LStarted), 'remove owned consumption marker');
    if FileExists(LMarker) then Check(DeleteFile(LMarker), 'remove owned profile marker');
    if DirectoryExists(LProfile) then Check(RemoveDir(LProfile), 'remove empty owned profile');
    Check(RemoveDir(LRoot), 'remove empty owned fixture parent');
  end;
end;

procedure Test;
var
  LPort, I: Integer;
  LPath, LDiagnostic, LBefore: String;
  LActual, LExpected: TStringList;
  LRaised: Boolean;
begin
  WfcBrowserParseEndpoint('49152'#10'/devtools/browser/abcd-1234', LPort, LPath);
  Check((LPort = 49152) and (LPath = '/devtools/browser/abcd-1234'), 'Chromium endpoint without final newline');
  WfcBrowserParseEndpoint('65535'#13#10'/devtools/browser/A0-B9'#13#10, LPort, LPath);
  Check(LPort = 65535, 'CRLF endpoint and maximum port');
  RejectEndpoint(''); RejectEndpoint('49152');
  RejectEndpoint('0'#10'/devtools/browser/abc');
  RejectEndpoint('65536'#10'/devtools/browser/abc');
  RejectEndpoint('049152'#10'/devtools/browser/abc');
  RejectEndpoint('+49152'#10'/devtools/browser/abc');
  RejectEndpoint('49152 '#10'/devtools/browser/abc');
  RejectEndpoint('49152'#10'/devtools/page/abc');
  RejectEndpoint('49152'#10'/devtools/browser/');
  RejectEndpoint('49152'#10'/devtools/browser/abc/def');
  RejectEndpoint('49152'#10'/devtools/browser/abc?query=1');
  RejectEndpoint('49152'#10'/devtools/browser/abc'#10'third-line');
  RejectEndpoint('49152'#10'/devtools/browser/' + StringOfChar('a', 129));
  for I := 0 to 255 do
    if not (Char(I) in ['a'..'z', 'A'..'Z', '0'..'9', '-']) then
      RejectEndpoint('49152'#10'/devtools/browser/x' + Char(I) + 'x');
  WfcBrowserValidateUrl('http://127.0.0.1:4180/test.html?selftest=1');
  Check(True, 'explicit local URL with query');
  RejectUrl('https://127.0.0.1:4180/test.html');
  RejectUrl('http://localhost:4180/test.html');
  RejectUrl('http://127.0.0.1.evil:4180/test.html');
  RejectUrl('http://127.0.0.1:4180@evil/test.html');
  RejectUrl('http://127.0.0.1:0/test.html');
  RejectUrl('http://127.0.0.1:65536/test.html');
  RejectUrl('http://127.0.0.1:04180/test.html');
  RejectUrl('http://127.0.0.1:4180');
  RejectUrl('http://127.0.0.1:4180/test.html#fragment');
  RejectUrl('http://127.0.0.1:4180\evil/test.html');
  RejectUrl('http://127.0.0.1:4180/test.html'#13#10'Host: evil');
  LActual := TStringList.Create; LActual.CaseSensitive := True;
  LExpected := TStringList.Create; LExpected.CaseSensitive := True;
  try
    LExpected.Add('data-self-test=passed');
    LExpected.Add('data-voice-stream-release=passed');
    Check(WfcBrowserCaptureState(nil, LExpected, LDiagnostic) = wbcsPending, 'no body is pending');
    LActual.Add('data-self-test=not-requested');
    Check(WfcBrowserCaptureState(LActual, LExpected, LDiagnostic) = wbcsPending, 'not-requested is pending');
    LActual.Values['data-self-test'] := 'passed';
    Check(WfcBrowserCaptureState(LActual, LExpected, LDiagnostic) = wbcsPending, 'generic harness pass cannot omit release');
    LActual.Values['data-voice-stream-release'] := 'pending';
    Check(WfcBrowserCaptureState(LActual, LExpected, LDiagnostic) = wbcsPending, 'pending release is not success');
    LActual.Values['data-voice-stream-release'] := 'failed';
    Check(WfcBrowserCaptureState(LActual, LExpected, LDiagnostic) = wbcsFailed, 'explicit expected release failure is terminal');
    LActual.Values['data-voice-stream-release'] := 'pending';
    LActual.Values['data-voice-state'] := 'failed';
    Check(WfcBrowserCaptureState(LActual, LExpected, LDiagnostic) = wbcsPending, 'negative application fixture is not terminal test failure');
    LActual.Values['data-voice-stream-release'] := 'passed';
    LBefore := LActual.Text;
    Check(WfcBrowserCaptureState(LActual, LExpected, LDiagnostic) = wbcsPassed, 'all exact markers pass');
    Check(LActual.Text = LBefore, 'readiness does not rewrite markers');
    LActual.Values['data-self-test'] := 'failed';
    Check(WfcBrowserCaptureState(LActual, LExpected, LDiagnostic) = wbcsFailed, 'explicit self-test failure');
    LActual.Values['data-self-test'] := 'passed';
    LActual.Values['data-self-test-message'] := 'no success'#10'::error::injected';
    Check(WfcBrowserCaptureState(LActual, LExpected, LDiagnostic) = wbcsFailed, 'failure message rejects matching markers');
    Check((Pos(#10, LDiagnostic) = 0) and (Pos('\x0A', LDiagnostic) > 0), 'failure diagnostics remain escaped');
    LExpected.Clear; LRaised := False;
    try WfcBrowserCaptureState(LActual, LExpected, LDiagnostic);
    except on EWfcBrowserCapture do LRaised := True;end;
    Check(LRaised, 'no expectation cannot certify completion');
    LExpected.Add('class=example'); LRaised := False;
    try WfcBrowserCaptureState(LActual, LExpected, LDiagnostic);
    except on EWfcBrowserCapture do LRaised := True;end;
    Check(LRaised, 'main self-test expectation is mandatory');
  finally LExpected.Free; LActual.Free;end;
  LRaised := False;
  try WfcBrowserPrepareProfile('must-not-create' + #0 + 'hidden-tail');
  except on EWfcBrowserCapture do LRaised := True;end;
  Check(LRaised, 'profile NUL rejected before any filesystem path truncation');
end;

begin Checks := 0; Test; TestProfiles; WriteLn('Checks: ', Checks, ', Failures: 0');end.
