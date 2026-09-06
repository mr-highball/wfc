{ SPDX-License-Identifier: MIT
  Copyright (c) 2021 mr-highball }
unit browser_ensemble_http;

{$mode delphi}{$H+}
{$modeswitch externalclass}

interface

uses JS, Web, SysUtils, ensemble_studio_stream;

type
  { Only a tiny capability response is fetched. Audio is a normal browser
    navigation: this object never buffers, fetches, or claims completion of it. }
  TBrowserEnsembleHttpDownload = class
  strict private
    FLink: TJSHTMLAnchorElement;
    FStatus: TJSElement;
    FToken, FSeconds: String;
    FOptions: TEnsembleStudioStreamOptions;
    FSettingsValid, FReleased, FDiscovering: Boolean;
    FAbort: TJSAbortController;
    procedure DisableLink;
    procedure RefreshLink;
    function HandleClick(AEvent: TJSMouseEvent): Boolean;
  public
    constructor Create(const ALink: TJSHTMLAnchorElement; const AStatus: TJSElement);
    procedure InvalidateSettings;
    procedure UpdateSettings(const ASeconds: String; const AOptions: TEnsembleStudioStreamOptions);
    procedure Discover(const ATestResponse: TJSPromise = nil); async;
    procedure Release;
  end;

procedure RunEnsembleHttpBrowserSelfTest; async;

implementation

uses ensemble_studio_http, ensemble_studio_profiles;

const CAPABILITY_LIMIT = 256;
  FIXTURE_TOKEN = '0123456789abcdef0123456789abcdef';
  UNAVAILABLE_TEXT = 'FPC server download availability is not confirmed. The host may be static, busy rendering, or unreachable. Reload after any current download, use browser Save As when available, or use the displayed native command.';

type
  TCapabilityWindow = class external name 'Window' (TJSWindow)
    function fetch(const AUrl: String; const AOptions: TJSObject): TJSPromise; reintroduce;
  end;
  TCapabilityReader = class external name 'Object' (TJSObject)
    function read: TJSPromise;
    procedure releaseLock;
  end;

function ParseCapability(const AText: String): String;
var P, I, Seen, Bit: Integer; Name, Value: String;

  procedure Bad;
  begin raise EEnsembleStudioHttp.Create('invalid streaming capability'); end;

  procedure Space;
  begin
    while (P <= Length(AText)) and (AText[P] in [' ', #9, #10, #13]) do Inc(P);
  end;

  procedure Expect(const C: Char);
  begin
    Space;
    if (P > Length(AText)) or (AText[P] <> C) then Bad;
    Inc(P);
  end;

  function Quoted: String;
  var Start: Integer;
  begin
    Expect('"'); Start := P;
    while (P <= Length(AText)) and (AText[P] <> '"') do
    begin
      { The protocol's three fixed keys, ASCII path and hex token never need
        escaped spellings. Reject escapes rather than accepting ambiguous keys. }
      if (Ord(AText[P]) < 32) or (Ord(AText[P]) > 126) or (AText[P] = '\') then Bad;
      Inc(P);
    end;
    if P > Length(AText) then Bad;
    Result := Copy(AText, Start, P - Start); Inc(P);
  end;

begin
  Result := '';
  if (Length(AText) = 0) or (Length(AText) > CAPABILITY_LIMIT) then Bad;
  for I := 1 to Length(AText) do if Ord(AText[I]) > 127 then Bad;
  P := 1; Seen := 0; Expect('{');
  for I := 0 to 2 do
  begin
    if I <> 0 then Expect(',');
    Name := Quoted; Expect(':');
    if Name = 'version' then
    begin
      Bit := 1; Expect('1');
    end
    else
    begin
      Value := Quoted;
      if Name = 'wavePath' then
      begin
        Bit := 2;
        if Value <> ENSEMBLE_STUDIO_HTTP_WAVE_PATH then Bad;
      end
      else if Name = 'token' then
      begin
        Bit := 4;
        if Length(Value) <> 32 then Bad;
        Result := Value;
      end
      else begin Bad; Bit := 0; end;
    end;
    if Seen and Bit <> 0 then Bad;
    Seen := Seen or Bit;
  end;
  Expect('}'); Space;
  if (Seen <> 7) or (P <= Length(AText)) then Bad;
  for I := 1 to Length(Result) do
    if not (Result[I] in ['0'..'9', 'a'..'f']) then Bad;
end;

constructor TBrowserEnsembleHttpDownload.Create(const ALink: TJSHTMLAnchorElement;
  const AStatus: TJSElement);
begin
  inherited Create;
  FLink := ALink; FStatus := AStatus;
  FLink.target := '_blank';
  FLink.rel := 'noopener noreferrer';
  FLink.removeAttribute('download');
  FLink.onclick := @HandleClick;
  DisableLink;
  FStatus.textContent := 'Checking this FPC server for direct WAVE downloads…';
end;

procedure TBrowserEnsembleHttpDownload.DisableLink;
begin
  FLink.removeAttribute('href');
  FLink.setAttribute('aria-disabled', 'true');
  FLink.setAttribute('tabindex', '-1');
  FLink.className := 'button-link disabled';
end;

procedure TBrowserEnsembleHttpDownload.InvalidateSettings;
begin
  if FReleased then Exit;
  FSettingsValid := False;
  DisableLink;
end;

procedure TBrowserEnsembleHttpDownload.UpdateSettings(const ASeconds: String;
  const AOptions: TEnsembleStudioStreamOptions);
begin
  if FReleased then Exit;
  InvalidateSettings;
  try
    { Validate settings even while discovery is outstanding. The placeholder
      token never becomes a link or a network request. }
    BuildEnsembleStudioHttpTarget(FIXTURE_TOKEN, ASeconds, AOptions);
    FSeconds := ASeconds; FOptions := AOptions; FSettingsValid := True;
    RefreshLink;
  except
    on E: EEnsembleStudioHttp do
      FStatus.textContent := 'Correct the stream settings before downloading from the FPC server.';
    on E: EEnsembleStudioStream do
      FStatus.textContent := 'Correct the stream settings before downloading from the FPC server.';
  end;
end;

procedure TBrowserEnsembleHttpDownload.RefreshLink;
var Target: String;
begin
  DisableLink;
  if FReleased or (FToken = '') or not FSettingsValid then Exit;
  Target := BuildEnsembleStudioHttpTarget(FToken, FSeconds, FOptions);
  { The current origin is authoritative; neither a response URL nor an HTML
    base URL can redirect this capability to another host. }
  FLink.href := window.location.origin + Target;
  FLink.onclick := @HandleClick;
  FLink.setAttribute('aria-disabled', 'false');
  FLink.removeAttribute('tabindex');
  FLink.className := 'button-link';
  FStatus.textContent := 'FPC server download ready. Your browser manages progress and cancellation; this page does not monitor completion.';
end;

function TBrowserEnsembleHttpDownload.HandleClick(AEvent: TJSMouseEvent): Boolean;
begin
  Result := not FReleased and FSettingsValid and (FToken <> '') and FLink.hasAttribute('href');
  if not Result then
  begin if AEvent <> nil then AEvent.preventDefault; Exit; end;
  FStatus.textContent := 'Download requested from the FPC server. Check your browser downloads for progress, errors, completion, or cancellation. The page Cancel button does not cancel this download.';
end;

procedure TBrowserEnsembleHttpDownload.Discover(const ATestResponse: TJSPromise); async;
var
  Response: TJSResponse; Reader: TCapabilityReader; Item, Options: TJSObject;
  Chunk: TJSUint8Array; Text, ExpectedUrl: String; I: Integer; Timer: NativeInt;
begin
  if FReleased or FDiscovering then Exit;
  FDiscovering := True; Timer := 0; Reader := nil;
  FToken := ''; DisableLink;
  try
    try
      ExpectedUrl := window.location.origin + ENSEMBLE_STUDIO_HTTP_CAPABILITIES_PATH;
      if ATestResponse <> nil then Response := await(TJSResponse, ATestResponse)
      else
      begin
        FAbort := TJSAbortController.new;
        Timer := window.setTimeout(procedure begin if FAbort <> nil then FAbort.abort; end, 5000);
        Options := TJSObject.new;
        Options['method'] := 'GET'; Options['mode'] := 'same-origin';
        Options['credentials'] := 'same-origin'; Options['redirect'] := 'error';
        Options['cache'] := 'no-store'; Options['referrerPolicy'] := 'no-referrer';
        Options['signal'] := FAbort.signal;
        Response := await(TJSResponse, TCapabilityWindow(window).fetch(ExpectedUrl, Options));
      end;
      if FReleased then Exit;
      if (Response.status <> 200) or Response.redirected or (Response.url <> ExpectedUrl) then
        raise EEnsembleStudioHttp.Create('streaming capability unavailable');
      if Response.body = nil then raise EEnsembleStudioHttp.Create('capability body unavailable');
      Reader := TCapabilityReader(Response.body.getReader);
      Text := '';
      repeat
        Item := await(TJSObject, Reader.read);
        if FReleased then Exit;
        if Boolean(Item['done']) then Break;
        Chunk := TJSUint8Array(Item['value']);
        if Chunk.length > CAPABILITY_LIMIT - Length(Text) then
          raise EEnsembleStudioHttp.Create('streaming capability too large');
        for I := 0 to Chunk.length - 1 do
        begin
          if Chunk[I] > 127 then raise EEnsembleStudioHttp.Create('streaming capability must be ASCII');
          Text := Text + Chr(Chunk[I]);
        end;
      until False;
      FToken := ParseCapability(Text);
      RefreshLink;
    except
      { Discovery is optional. Do not print the response, URL, or token in a
        diagnostic. Static hosting and unsupported browsers retain fallback. }
      if not FReleased then
      begin
        FToken := ''; DisableLink;
        FStatus.textContent := UNAVAILABLE_TEXT;
      end;
    end;
  finally
    try
      try
        if Timer <> 0 then window.clearTimeout(Timer);
        if FAbort <> nil then FAbort.abort;
        if Reader <> nil then Reader.releaseLock;
      except
        { Discovery cleanup is optional transport too. A rejected lock release
          cannot leave a claimed capability or prevent the lifetime cleanup. }
        if not FReleased then
        begin FToken := ''; DisableLink; FStatus.textContent := UNAVAILABLE_TEXT; end;
      end;
    finally
      FAbort := nil;
      FDiscovering := False;
      if FReleased then Free;
    end;
  end;
end;

procedure TBrowserEnsembleHttpDownload.Release;
begin
  if FReleased then Exit;
  FReleased := True; FToken := ''; DisableLink; FLink.onclick := nil;
  if FAbort <> nil then FAbort.abort;
  if not FDiscovering then Free;
end;

procedure AssertHttp(const Condition: Boolean; const MessageText: String);
begin
  if not Condition then raise EEnsembleStudioHttp.Create('HTTP download browser check: ' + MessageText);
end;

function FakeResponse(const Text: String; const Status: Integer = 200;
  const ChunkSize: Integer = CAPABILITY_LIMIT + 1;
  const ReleaseFails: Boolean = False): TJSObject;
var Body, Reader: TJSObject; Offset: Integer;
begin
  Offset := 0; Reader := TJSObject.new; Body := TJSObject.new;
  Reader['read'] := function: TJSPromise
    var Item: TJSObject; Data: TJSUint8Array; I, Count: Integer;
    begin
      Item := TJSObject.new; Item['done'] := Offset = Length(Text);
      if not Boolean(Item['done']) then
      begin
        Count := Length(Text) - Offset;
        if Count > ChunkSize then Count := ChunkSize;
        Data := TJSUint8Array.new(Count);
        for I := 0 to Count - 1 do Data[I] := Ord(Text[Offset + I + 1]);
        Inc(Offset, Count);
        Item['value'] := Data;
      end;
      Result := TJSPromise.resolve(Item);
    end;
  Reader['releaseLock'] := procedure
    begin if ReleaseFails then raise EEnsembleStudioHttp.Create('simulated release failure'); end;
  Body['getReader'] := function: TJSObject begin Result := Reader; end;
  Result := TJSObject.new; Result['status'] := Status; Result['redirected'] := False;
  Result['url'] := window.location.origin + ENSEMBLE_STUDIO_HTTP_CAPABILITIES_PATH;
  Result['body'] := Body;
end;

procedure RunEnsembleHttpBrowserSelfTest; async;
const Good = '{"version":1,"wavePath":"/ensemble-wave-v1","token":"0123456789abcdef0123456789abcdef"}';
var
  Link: TJSHTMLAnchorElement; Status: TJSElement; C: TBrowserEnsembleHttpDownload;
  O: TEnsembleStudioStreamOptions; Target, Before, Bad, Token: String;
  Pending, Completion: TJSPromise; Resolve: TJSPromiseResolver; I: Integer; Failed: Boolean;
  Response: TJSObject;
begin
  Link := TJSHTMLAnchorElement(document.createElement('a'));
  Status := document.createElement('span');
  C := TBrowserEnsembleHttpDownload.Create(Link, Status);
  try
    AssertHttp(ParseCapability(Good) = FIXTURE_TOKEN, 'canonical capability parser');
    O := DefaultEnsembleStudioStreamOptions; O.Profile := espDevelopedPeriodV1;
    O.Seed := High(Cardinal); O.CaptureTrace := True;
    C.UpdateSettings('73.125', O);
    AssertHttp(not Link.hasAttribute('href'), 'settings alone cannot invent a capability');
    await(C.Discover(TJSPromise.resolve(FakeResponse(Good, 200, 13))));
    Target := window.location.origin + BuildEnsembleStudioHttpTarget(FIXTURE_TOKEN, '73.125', O);
    AssertHttp((Link.href = Target) and (Link.target = '_blank') and
      (Link.rel = 'noopener noreferrer') and not Link.hasAttribute('download'), 'normal same-origin download preserves all settings and server filename');
    AssertHttp(Pos(FIXTURE_TOKEN, Status.textContent) = 0, 'capability absent from visible diagnostics');
    AssertHttp(Link.onclick(nil) and (Pos('Check your browser downloads', Status.textContent) > 0),
      'click hands off without claiming completed audio or page cancellation');
    C.InvalidateSettings;
    AssertHttp(not Link.hasAttribute('href'), 'editing removes stale navigation synchronously');
    C.UpdateSettings('invalid', O);
    AssertHttp(not Link.hasAttribute('href'), 'invalid settings cannot publish a target');
    C.UpdateSettings('91', O);
    AssertHttp(Pos('seconds=91', Link.href) > 0, 'valid edit builds the newly requested length');
    await(C.Discover(TJSPromise.resolve(FakeResponse('', 404))));
    AssertHttp(not Link.hasAttribute('href'), 'ordinary static host preserves fallback');
    Response := FakeResponse(Good);
    Response['url'] := 'https://elsewhere.invalid/ensemble-stream-v1';
    await(C.Discover(TJSPromise.resolve(Response)));
    AssertHttp(not Link.hasAttribute('href'), 'cross-origin response cannot supply capability');
    Response := FakeResponse(Good); Response['redirected'] := True;
    await(C.Discover(TJSPromise.resolve(Response)));
    AssertHttp(not Link.hasAttribute('href'), 'redirected response cannot supply capability');
    await(C.Discover(TJSPromise.reject('simulated unavailable fetch')));
    AssertHttp(not Link.hasAttribute('href') and (Pos('simulated', Status.textContent) = 0),
      'fetch rejection preserves fallback without echoing transport diagnostics');
    await(C.Discover(TJSPromise.resolve(FakeResponse(Good + StringOfChar(' ', 257), 200, 13))));
    AssertHttp(not Link.hasAttribute('href'), 'aggregate multi-chunk body is bounded before append');
    await(C.Discover(TJSPromise.resolve(FakeResponse(Good, 200, 13, True))));
    AssertHttp(not Link.hasAttribute('href') and (Pos('busy rendering', Status.textContent) > 0),
      'reader cleanup failure safely disables optional capability without claiming unsupported host');
    for I := 0 to 9 do
    begin
      case I of
        0: Bad := StringReplace(Good, '"version":1', '"version":2', []);
        1: Bad := StringReplace(Good, '"version":1', '"version":"1"', []);
        2: Bad := StringReplace(Good, '"wavePath"', '"token"', []);
        3: Bad := Copy(Good, 1, Length(Good)-1) + ',"other":0}';
        4: Bad := StringReplace(Good, '/ensemble-wave-v1', '//elsewhere/ensemble-wave-v1', []);
        5: Bad := StringReplace(Good, FIXTURE_TOKEN, UpperCase(FIXTURE_TOKEN), []);
        6: Bad := Good + '{}';
        7: Bad := StringOfChar(' ', CAPABILITY_LIMIT + 1);
        8: Bad := StringReplace(Good, '"version":1', '"version":1.0', []);
        9: Bad := StringReplace(Good, '"version"', '"ver\u0073ion"', []);
      end;
      Failed := False; Token := '';
      try Token := ParseCapability(Bad); except on E: EEnsembleStudioHttp do Failed := True; end;
      AssertHttp(Failed and (Token = ''), 'malformed capability rejected');
      await(C.Discover(TJSPromise.resolve(FakeResponse(Bad))));
      AssertHttp(not Link.hasAttribute('href'), 'malformed response cannot enable navigation');
    end;
    Token := ParseCapability('{ "token":"' + FIXTURE_TOKEN + '","wavePath":"/ensemble-wave-v1","version":1 }'#10);
    AssertHttp(Token = FIXTURE_TOKEN, 'field order and JSON whitespace are harmless');
    Resolve := nil;
    Pending := TJSPromise.new(procedure(AResolve, AReject: TJSPromiseResolver) begin Resolve := AResolve; end);
    Completion := C.Discover(Pending);
    C.UpdateSettings('102', O);
    Resolve(FakeResponse(Good));
    await(JSValue, Completion);
    AssertHttp(Pos('seconds=102', Link.href) > 0, 'late capability uses current settings, not a stale snapshot');
    Pending := TJSPromise.new(procedure(AResolve, AReject: TJSPromiseResolver) begin Resolve := AResolve; end);
    Completion := C.Discover(Pending);
    C.Release; C := nil; Before := Status.textContent;
    Resolve(FakeResponse(Good));
    await(JSValue, Completion);
    AssertHttp(not Link.hasAttribute('href') and (Link.onclick = nil) and (Status.textContent = Before),
      'released owner cannot publish a late capability or mutate status');
  finally if C <> nil then C.Release; end;
end;

end.
