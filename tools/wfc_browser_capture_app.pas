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
unit wfc_browser_capture_app;

{$mode delphi}{$H+}
{$IFDEF PAS2JS}{$FATAL this browser capture host requires native FPC}{$ENDIF}

interface

uses Classes, SysUtils;

const
  WFC_BROWSER_PROFILE_MARKER = 'wfc-browser-profile-v1';
  WFC_BROWSER_CAPTURE_STARTED = 'wfc-browser-capture-started';
  WFC_BROWSER_CAPTURE_TIMEOUT_MS = 60000;
  WFC_BROWSER_CAPTURE_POLL_MS = 25;

type
  EWfcBrowserCapture = class(Exception);
  TWfcBrowserCaptureState = (wbcsPending, wbcsPassed, wbcsFailed);

{ A new profile only. Existing directories, including ordinary user profiles,
  are refused. The marker records workflow ownership, not a security boundary
  against hostile concurrent edits in the selected parent directory. }
procedure WfcBrowserPrepareProfile(const APath: String);
procedure WfcBrowserParseEndpoint(const AText: RawByteString;
  out APort: Integer; out APath: String);
procedure WfcBrowserValidateUrl(const AUrl: String);
function WfcBrowserCaptureState(const AActual, AExpected: TStrings;
  out ADiagnostic: String): TWfcBrowserCaptureState;

{ Attaches only through an explicitly prepared profile's endpoint, creates its
  own page, and evaluates browser-serialized DOM in Pascal. One caller-supplied
  absolute monotonic deadline covers discovery and all commands. A profile is
  single-use even after failure. The caller still owns the Chromium process.
  Success returns one complete snapshot that passes all exact expectations;
  failure raises with bounded state diagnostics and returns no success DOM. }
function WfcBrowserCapture(const AProfile, AUrl: String;
  const ADeadline: QWord; const AExpected: TStrings): UTF8String;

implementation

uses fpjson, wfc_browser_socket, wfc_browser_cdp, wfc_browser_dom,
  wfc_atomic_new_file;

const
  PROFILE_CONTENT = 'WFC fresh browser profile 1'#10;
  { JSON escaping can expand a DOM byte sixfold, plus a bounded envelope. }
  MAX_CDP_MESSAGE = WFC_BROWSER_MAX_DOM_BYTES * 6 + 65536;

procedure Fail(const AMessage: String);
begin
  raise EWfcBrowserCapture.Create(AMessage);
end;

procedure PublishNewText(const APath: String; const AText: RawByteString);
var
  LFile: TWfcAtomicNewFile;
  LBlock: array[0..65535] of Byte;
  LOffset, LCount: Integer;
begin
  LFile := TWfcAtomicNewFile.Create(APath);
  try
    try
      LOffset := 1;
      while LOffset <= Length(AText) do
      begin
        LCount := Length(AText) - LOffset + 1;
        if LCount > SizeOf(LBlock) then LCount := SizeOf(LBlock);
        Move(AText[LOffset], LBlock[0], LCount);
        LFile.WriteBytes(Slice(LBlock, LCount));
        Inc(LOffset, LCount);
      end;
      LFile.Publish;
      if LFile.CleanupError <> '' then Fail('profile publication cleanup failed');
    except
      on E: Exception do
      begin
        LFile.Cancel;
        if LFile.CleanupError <> '' then
          E.Message := E.Message + '; cleanup: ' + LFile.CleanupError;
        raise;
      end;
    end;
  finally
    LFile.Free;
  end;
end;

function ReadSmallFile(const APath: String; const AMaximum: Integer): RawByteString;
var LFile: TFileStream;
begin
  LFile := TFileStream.Create(APath, fmOpenRead or fmShareDenyNone);
  try
    if LFile.Size > AMaximum then Fail('browser endpoint/ownership file exceeds limit');
    SetLength(Result, Integer(LFile.Size));
    if Result <> '' then LFile.ReadBuffer(Result[1], Length(Result));
  finally
    LFile.Free;
  end;
end;

procedure WfcBrowserPrepareProfile(const APath: String);
var LPath: String;
begin
  if APath = '' then Fail('a new browser profile path is required');
  if Pos(#0, APath) <> 0 then Fail('browser profile path must not contain NUL');
  LPath := ExpandFileName(APath);
  if FileExists(LPath) or DirectoryExists(LPath) then
    Fail('browser profile must be a new directory');
  if not DirectoryExists(ExtractFileDir(LPath)) then
    Fail('browser profile parent directory must exist');
  if not CreateDir(LPath) then Fail('cannot create new browser profile directory');
  PublishNewText(IncludeTrailingPathDelimiter(LPath) + WFC_BROWSER_PROFILE_MARKER,
    PROFILE_CONTENT);
end;

procedure WfcBrowserParseEndpoint(const AText: RawByteString;
  out APort: Integer; out APath: String);
var LPortText, LPath: String; I, P: Integer;
begin
  APort := 0; APath := '';
  if Length(AText) > 1024 then Fail('browser endpoint file exceeds limit');
  P := Pos(#10, AText);
  if P = 0 then Fail('browser endpoint file is incomplete');
  LPortText := Copy(AText, 1, P - 1);
  if (LPortText <> '') and (LPortText[Length(LPortText)] = #13) then
    Delete(LPortText, Length(LPortText), 1);
  LPath := Copy(AText, P + 1, MaxInt);
  if (LPath <> '') and (LPath[Length(LPath)] = #10) then
    Delete(LPath, Length(LPath), 1);
  if (LPath <> '') and (LPath[Length(LPath)] = #13) then
    Delete(LPath, Length(LPath), 1);
  if (LPortText = '') or (Length(LPortText) > 5) or (LPortText[1] = '0') then
    Fail('browser endpoint port must be canonical decimal');
  for I := 1 to Length(LPortText) do
  begin
    if not (LPortText[I] in ['0'..'9']) then Fail('invalid browser endpoint port');
    APort := APort * 10 + Ord(LPortText[I]) - Ord('0');
  end;
  if (APort < 1) or (APort > 65535) then Fail('browser endpoint port is out of range');
  if (Copy(LPath, 1, 18) <> '/devtools/browser/') or
    (Length(LPath) <= 18) or (Length(LPath) > 146) then
    Fail('an explicit browser debugging endpoint is required');
  for I := 19 to Length(LPath) do
    if not (LPath[I] in ['a'..'z', 'A'..'Z', '0'..'9', '-']) then
      Fail('invalid browser debugging endpoint identifier');
  APath := LPath;
end;

procedure WfcBrowserValidateUrl(const AUrl: String);
const PREFIX = 'http://127.0.0.1:';
var I, P, LPort: Integer; LPortText: String;
begin
  if (Copy(AUrl, 1, Length(PREFIX)) <> PREFIX) or (Length(AUrl) > 2048) then
    Fail('capture URL must use explicit http://127.0.0.1:PORT/path');
  P := Length(PREFIX) + 1;
  I := P;
  while (I <= Length(AUrl)) and (AUrl[I] in ['0'..'9']) do Inc(I);
  LPortText := Copy(AUrl, P, I - P);
  if (LPortText = '') or (Length(LPortText) > 5) or (LPortText[1] = '0') or
    (I > Length(AUrl)) or (AUrl[I] <> '/') then Fail('invalid capture URL port/path');
  LPort := StrToInt(LPortText);
  if (LPort < 1) or (LPort > 65535) then Fail('capture URL port is out of range');
  for I := 1 to Length(AUrl) do
    if not (AUrl[I] in [#33..#126]) or (AUrl[I] in ['\', '#']) then
      Fail('capture URL must be printable ASCII without fragment or backslash');
end;

function WfcBrowserCaptureState(const AActual, AExpected: TStrings;
  out ADiagnostic: String): TWfcBrowserCaptureState;
var I: Integer;
begin
  ADiagnostic := '';
  if (AExpected = nil) or (AExpected.Count = 0) then Fail('capture expectations are required');
  if AExpected.Values['data-self-test'] <> 'passed' then
    Fail('capture requires the data-self-test=passed expectation');
  if AActual = nil then
  begin
    ADiagnostic := 'waiting for a rendered body';
    Exit(wbcsPending);
  end;
  try
    WfcBrowserAssertBody(AActual, AExpected);
    Exit(wbcsPassed);
  except
    on E: EWfcBrowserDom do ADiagnostic := E.Message;
  end;
  if (AActual.Values['data-self-test'] = 'failed') or
    (AActual.Values['data-self-test-message'] <> '') then Result := wbcsFailed
  else Result := wbcsPending;
  for I := 0 to AExpected.Count - 1 do
    if (AExpected.ValueFromIndex[I] = 'passed') and
      (AActual.Values[AExpected.Names[I]] = 'failed') then Exit(wbcsFailed);
end;

function RequiredObject(const AObject: TJSONObject; const AName: String): TJSONObject;
var LValue: TJSONData;
begin
  LValue := AObject.Find(AName);
  if (LValue = nil) or (LValue.JSONType <> jtObject) then
    Fail('CDP response is missing object ' + AName);
  Result := TJSONObject(LValue);
end;

function RequiredString(const AObject: TJSONObject; const AName: String): UTF8String;
var LValue: TJSONData;
begin
  LValue := AObject.Find(AName);
  if (LValue = nil) or (LValue.JSONType <> jtString) then
    Fail('CDP response is missing string ' + AName);
  Result := LValue.AsString;
end;

function RequiredNodeId(const AObject: TJSONObject;
  const AName: String = 'nodeId'): Integer;
var LValue: TJSONData;
begin
  LValue := AObject.Find(AName);
  if (LValue = nil) or (LValue.JSONType <> jtNumber) or
    not TryStrToInt(LValue.AsJSON, Result) or (Result < 1) then
    Fail('CDP response has no positive DOM node identifier');
end;

function WfcBrowserCapture(const AProfile, AUrl: String;
  const ADeadline: QWord; const AExpected: TStrings): UTF8String;
var
  LProfile, LEndpointFile, LPath, LTargetId, LSessionId, LLastState, LUrl: String;
  LPort, LNodeId, LBackendId, LCheckBackendId: Integer;
  LClient: TWfcBrowserCDP;
  LParams, LReply, LRoot: TJSONObject;
  LActual: TStringList;
  LHtml: UTF8String;
  LState: TWfcBrowserCaptureState;
  LText: RawByteString;
  LHtmlBytes: String;

  function Command(const AMethod: String; const AParameters: TJSONObject;
    const ASession: String = ''): TJSONObject;
  begin
    try Result := LClient.Call(AMethod, AParameters, ASession);
    finally AParameters.Free;end;
  end;

  procedure Pause;
  var LRemaining, LNow: QWord;
  begin
    LNow := WfcBrowserTickCount64;
    if LNow >= ADeadline then WfcBrowserCheckDeadline(ADeadline);
    LRemaining := ADeadline - LNow;
    if LRemaining > WFC_BROWSER_CAPTURE_POLL_MS then LRemaining := WFC_BROWSER_CAPTURE_POLL_MS;
    Sleep(Cardinal(LRemaining));
  end;

begin
  Result := '';
  WfcBrowserValidateUrl(AUrl);
  if (AExpected = nil) or (AExpected.Count = 0) then Fail('capture expectations are required');
  if AExpected.Values['data-self-test'] <> 'passed' then
    Fail('capture requires the data-self-test=passed expectation');
  WfcBrowserCheckDeadline(ADeadline);
  if AProfile = '' then Fail('an explicitly prepared browser profile is required');
  if Pos(#0, AProfile) <> 0 then Fail('browser profile path must not contain NUL');
  LProfile := IncludeTrailingPathDelimiter(ExpandFileName(AProfile));
  if ReadSmallFile(LProfile + WFC_BROWSER_PROFILE_MARKER, 128) <> PROFILE_CONTENT then
    Fail('profile was not prepared by the FPC capture tool');
  PublishNewText(LProfile + WFC_BROWSER_CAPTURE_STARTED, 'capture started'#10);
  LEndpointFile := LProfile + 'DevToolsActivePort';
  LLastState := 'waiting for the owned browser endpoint';
  LClient := nil;
  try
    try
      repeat
        WfcBrowserCheckDeadline(ADeadline);
        if FileExists(LEndpointFile) then
        begin
          LText := '';
          try
            LText := ReadSmallFile(LEndpointFile, 1024);
          except
            { Windows Chromium can briefly hold an exclusive writer handle;
              a visible path alone is not an endpoint-publication signal. }
            on EFOpenError do LText := '';
            on EReadError do LText := '';
          end;
          { Chromium may still be writing the two-line endpoint file. }
          if (Pos(#10, LText) > 0) and
            (Length(LText) > Pos(#10, LText) + 18) then Break;
        end;
        Pause;
      until False;
      WfcBrowserParseEndpoint(LText, LPort, LPath);
      LClient := TWfcBrowserCDP.Create(LPort, LPath, ADeadline, MAX_CDP_MESSAGE);
      LParams := TJSONObject.Create; LParams.Add('url', 'about:blank');
      LReply := Command('Target.createTarget', LParams);
      try LTargetId := RequiredString(LReply, 'targetId');finally LReply.Free;end;
      if LTargetId = '' then Fail('CDP returned an empty owned target identifier');
      LParams := TJSONObject.Create;
      LParams.Add('targetId', LTargetId); LParams.Add('flatten', True);
      LReply := Command('Target.attachToTarget', LParams);
      try LSessionId := RequiredString(LReply, 'sessionId');finally LReply.Free;end;
      if LSessionId = '' then Fail('CDP returned an empty owned session identifier');
      LReply := Command('Page.enable', nil, LSessionId); LReply.Free;
      LParams := TJSONObject.Create; LParams.Add('url', AUrl);
      LReply := Command('Page.navigate', LParams, LSessionId);
      try
        if LReply.Find('errorText') <> nil then Fail('CDP page navigation failed');
      finally LReply.Free;end;
      LLastState := 'waiting for the exact requested document';
      repeat
        WfcBrowserCheckDeadline(ADeadline);
        try
          LParams := TJSONObject.Create; LParams.Add('depth', 1); LParams.Add('pierce', False);
          LReply := Command('DOM.getDocument', LParams, LSessionId);
          try
            LRoot := RequiredObject(LReply, 'root');
            LNodeId := RequiredNodeId(LRoot);
            LBackendId := RequiredNodeId(LRoot, 'backendNodeId');
            LUrl := RequiredString(LRoot, 'documentURL');
          finally LReply.Free;end;
          if LUrl = AUrl then
          begin
            LParams := TJSONObject.Create; LParams.Add('nodeId', LNodeId);
            LReply := Command('DOM.getOuterHTML', LParams, LSessionId);
            try LHtml := RequiredString(LReply, 'outerHTML');finally LReply.Free;end;
            if Length(LHtml) > WFC_BROWSER_MAX_DOM_BYTES then Fail('captured DOM exceeds limit');
            { The DOM reader is a UTF-8 byte parser. Do not transcode browser
              text through the Windows system ANSI code page at this boundary. }
            SetString(LHtmlBytes, PAnsiChar(LHtml), Length(LHtml));
            LActual := nil;
            try
              try
                LActual := WfcBrowserBodyAttributes(LHtmlBytes);
              except
                on E: EWfcBrowserDom do
                  if E.Message <> 'rendered DOM has no body element' then raise;
              end;
              LState := WfcBrowserCaptureState(LActual, AExpected, LLastState);
            finally LActual.Free;end;
            if LState <> wbcsPending then
            begin
              { Revalidate document identity after serialization. Frontend IDs
                can be rebound by getDocument; compare the backend node instead.
                Assertions apply to this exact captured HTML, never a prior poll.
                Protocol implementation detail, no borrowed implementation:
                https://github.com/chromium/chromium/blob/main/third_party/blink/renderer/core/inspector/inspector_dom_agent.cc }
              LParams := TJSONObject.Create; LParams.Add('depth', 1); LParams.Add('pierce', False);
              LReply := Command('DOM.getDocument', LParams, LSessionId);
              try
                LRoot := RequiredObject(LReply, 'root');
                LCheckBackendId := RequiredNodeId(LRoot, 'backendNodeId');
                LUrl := RequiredString(LRoot, 'documentURL');
              finally LReply.Free;end;
              if (LUrl = AUrl) and (LCheckBackendId = LBackendId) then
              begin
                WfcBrowserCheckDeadline(ADeadline);
                if LState = wbcsFailed then Fail(LLastState);
                Result := LHtml;
                Exit;
              end;
              LLastState := 'document changed during final DOM capture';
            end;
          end;
        except
          on E: EWfcBrowserCDPCommand do
          begin
            if (E.Code <> -32000) or
              ((Pos('Could not find node', E.CommandMessage) = 0) and
               (Pos('No node with given id', E.CommandMessage) = 0)) then raise;
            LLastState := 'document node changed; awaiting a fresh snapshot';
          end;
        end;
        Pause;
      until False;
    except
      on E: EWfcBrowserSocket do Fail(E.Message + '; ' + LLastState);
      on E: EWfcBrowserCDP do Fail(E.Message + '; ' + LLastState);
    end;
  finally
    { Only the connection is ours here; launch wrappers must close/terminate
      their fresh browser within their own bounded cleanup allowance. }
    LClient.Free;
  end;
end;

end.
