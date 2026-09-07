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
program wfc_ensemble_http_process_test;

{$mode delphi}{$H+}
{$IFDEF PAS2JS}{$FATAL ensemble HTTP process tests require native FPC}{$ENDIF}

uses
  Classes, SysUtils, Process, Pipes, fpjson, jsonparser,
  wfc_browser_socket, wfc_process_test_support, wfc_music_audio,
  wfc_music_audio_stream, wfc_music_arrangement, ensemble_studio_stream,
  ensemble_studio_profiles,
  {$IFDEF MSWINDOWS}Windows, WinSock2{$ELSE}Sockets, BaseUnix{$ENDIF};

const
  REQUEST_COUNT = 32;
  MAX_BODY_BYTES = 6291456;
  INDEX_BYTES = '<!doctype html><title>Private Ensemble fixture</title>'#10;

type
  THttpResponse = record
    Status: Integer;
    ContentLength: Int64;
    Header, Body: RawByteString;
  end;

  TCompareSink = class(TWfcMusicAudioByteSink)
  public
    Body: RawByteString;
    Offset: Integer;
    procedure WriteBytes(const ABytes: array of Byte); override;
  end;

var
  Checks, Requests, Port: Integer;
  ServerPath, FixtureRoot, Capability: String;
  ServerOutput, ServerError: String;
  Child: TProcess;

procedure Check(const ACondition: Boolean; const ALabel: String);
begin
  Inc(Checks);
  if not ACondition then raise Exception.Create(ALabel);
end;

procedure Ensure(const ACondition: Boolean; const ALabel: String);
begin
  { Transport chunking/readiness polling is nondeterministic; enforce its
    bounds without making the reported assertion count depend on scheduling. }
  if not ACondition then raise Exception.Create(ALabel);
end;

function Elapsed(const AStart, ANow: QWord): QWord;
begin
  if ANow >= AStart then Result := ANow - AStart
  else Result := (High(QWord) - AStart) + ANow + 1;
end;

procedure ReadPipe(const APipe: TInputPipeStream; var AText: String);
var LBuffer: array[0..4095] of Byte; N, LOffset: Integer;
begin
  while APipe.NumBytesAvailable > 0 do
  begin
    N := APipe.NumBytesAvailable;
    if N > SizeOf(LBuffer) then N := SizeOf(LBuffer);
    N := APipe.Read(LBuffer[0], N);
    if N <= 0 then Exit;
    Ensure(Length(AText) <= 65536 - N, 'server diagnostic capture is bounded');
    LOffset := Length(AText); SetLength(AText, LOffset + N);
    Move(LBuffer[0], AText[LOffset + 1], N);
  end;
end;

procedure ReadServerOutput;
begin
  ReadPipe(Child.Output, ServerOutput); ReadPipe(Child.Stderr, ServerError);
end;

function WaitForChild(const ATimeout: QWord): Boolean;
var LStart: QWord;
begin
  LStart := WfcBrowserTickCount64;
  repeat
    ReadServerOutput;
    if not Child.Running then Exit(True);
    Sleep(2);
  until Elapsed(LStart, WfcBrowserTickCount64) >= ATimeout;
  Result := not Child.Running;
end;

procedure StopChild;
begin
  if Child = nil then Exit;
  try
    if Child.Running then
    begin
      {$IFDEF MSWINDOWS}Child.Terminate(1);
      {$ELSE}fpKill(Child.ProcessID, SIGKILL);{$ENDIF}
      Check(WaitForChild(5000), 'owned server stops under monotonic cleanup deadline');
    end;
  finally FreeAndNil(Child); end;
end;

function PickPrivatePort: Integer;
{$IFDEF MSWINDOWS}
var S: TSocket; A: TSockAddrIn; N: LongInt; W: TWSAData;
{$ELSE}
var S: LongInt; A: TInetSockAddr; N: TSockLen;
{$ENDIF}
begin
  {$IFDEF MSWINDOWS}
  Check(WSAStartup($0202, W) = 0, 'port reservation initializes Winsock');
  try
  S := WinSock2.socket(AF_INET, SOCK_STREAM, IPPROTO_TCP);
  Check(S <> INVALID_SOCKET, 'allocate private port reservation');
  {$ELSE}
  S := fpSocket(AF_INET, SOCK_STREAM, 0);
  Check(S >= 0, 'allocate private port reservation');
  {$ENDIF}
  try
    FillChar(A, SizeOf(A), 0);
    {$IFDEF DARWIN}A.sin_len := SizeOf(A);{$ENDIF}
    A.sin_family := AF_INET; A.sin_addr.s_addr := htonl($7F000001);
    N := SizeOf(A);
    {$IFDEF MSWINDOWS}
    Check(WinSock2.bind(S, PSockAddr(@A), SizeOf(A)) = 0, 'reserve ephemeral loopback port');
    Check(WinSock2.getsockname(S, PSockAddr(@A)^, N) = 0, 'read private port');
    {$ELSE}
    Check(fpBind(S, @A, SizeOf(A)) = 0, 'reserve ephemeral loopback port');
    Check(fpGetSockName(S, @A, @N) = 0, 'read private port');
    {$ENDIF}
    Result := ntohs(A.sin_port);
    Check((Result > 0) and (Result <> 4178), 'private test never selects live studio port');
  finally
    {$IFDEF MSWINDOWS}WinSock2.closesocket(S);{$ELSE}fpClose(S);{$ENDIF}
  end;
  {$IFDEF MSWINDOWS}finally WSACleanup; end;{$ENDIF}
end;

procedure StartChild;
var LStart: QWord; LMarker: String;
begin
  Port := PickPrivatePort;
  Child := TProcess.Create(nil);
  Child.Executable := ServerPath; Child.CurrentDirectory := FixtureRoot;
  Child.Parameters.Add('--root'); Child.Parameters.Add(FixtureRoot);
  Child.Parameters.Add('--port'); Child.Parameters.Add(IntToStr(Port));
  Child.Parameters.Add('--max-requests'); Child.Parameters.Add(IntToStr(REQUEST_COUNT));
  Child.Options := [poUsePipes, poNoConsole];
  Child.Execute;
  { Do not resolve Input first: the server has no stdin protocol. }
  Child.CloseInput;
  LStart := WfcBrowserTickCount64;
  LMarker := 'WFC development server: http://127.0.0.1:' + IntToStr(Port) + '/';
  repeat
    ReadServerOutput;
    if Pos(LMarker, ServerOutput) > 0 then Exit;
    Ensure(Child.Running, 'private server remains running until readiness');
    Ensure(Elapsed(LStart, WfcBrowserTickCount64) < 15000, 'bounded private server readiness');
    Sleep(5);
  until False;
end;

function HeaderValue(const AHeader, AName: String): String;
var LLines: TStringList; I, P: Integer; LSeen: Boolean; S: String;
begin
  Result := ''; LSeen := False;
  LLines := TStringList.Create;
  try
    LLines.Text := AHeader;
    for I := 1 to LLines.Count - 1 do
    begin
      S := LLines[I]; P := Pos(':', S);
      if (P > 0) and SameText(Copy(S, 1, P - 1), AName) then
      begin
        Check(not LSeen, 'response must not duplicate a relevant header');
        LSeen := True; Result := Trim(Copy(S, P + 1, Length(S)));
      end;
    end;
  finally LLines.Free; end;
end;

function OpenRequest(const AMethod, ATarget, AHost: String;
  const ATimeout: QWord; out AResponse: THttpResponse): TWfcBrowserSocket;
var S: RawByteString; P: Integer; LLength: String;
begin
  AResponse := Default(THttpResponse);
  Check(Requests < REQUEST_COUNT, 'request budget is not exceeded');
  Result := TWfcBrowserSocket.Create(Port, WfcBrowserTickCount64 + ATimeout);
  Inc(Requests);
  try
    Result.WriteAll(AMethod + ' ' + ATarget + ' HTTP/1.1'#13#10 +
      'Host: ' + AHost + #13#10'Connection: close'#13#10#13#10);
    S := '';
    repeat
      S := S + Result.ReadSome(4096);
      P := Pos(#13#10#13#10, S);
      Ensure((P > 0) or (Length(S) < 16384), 'response header stays bounded');
    until P > 0;
    AResponse.Header := Copy(S, 1, P + 3);
    AResponse.Body := Copy(S, P + 4, Length(S));
    Check(Pos('HTTP/1.1 ', AResponse.Header) = 1, 'HTTP response version');
    Check(TryStrToInt(Copy(AResponse.Header, 10, 3), AResponse.Status), 'HTTP status is decimal');
    LLength := HeaderValue(AResponse.Header, 'Content-Length');
    Check(TryStrToInt64(LLength, AResponse.ContentLength) and
      (AResponse.ContentLength >= 0) and (IntToStr(AResponse.ContentLength) = LLength),
      'response declares canonical nonnegative exact Content-Length');
    Check(HeaderValue(AResponse.Header, 'Connection') = 'close', 'response closes its connection');
    Check(HeaderValue(AResponse.Header, 'Transfer-Encoding') = '', 'known-size response is not chunked');
    Check(HeaderValue(AResponse.Header, 'Cache-Control') = 'no-store', 'response is not cached');
    Check(HeaderValue(AResponse.Header, 'X-Content-Type-Options') = 'nosniff', 'response disables MIME sniffing');
  except Result.Free; raise; end;
end;

function Request(const AMethod, ATarget: String; const AExpectedStatus: Integer;
  const ATimeout: QWord = 60000; const AHost: String = '127.0.0.1'): THttpResponse;
var S: TWfcBrowserSocket; LPart: RawByteString; LExpected: Int64; LClosed: Boolean;
begin
  S := OpenRequest(AMethod, ATarget, AHost, ATimeout, Result);
  try
    Check(Result.Status = AExpectedStatus, 'request has expected status (target deliberately not logged)');
    LExpected := Result.ContentLength;
    if AMethod = 'HEAD' then LExpected := 0;
    Check(LExpected <= MAX_BODY_BYTES, 'test response body stays inside fixed capture bound');
    while Length(Result.Body) < LExpected do
    begin
      LPart := S.ReadSome(65536);
      Ensure(Length(Result.Body) <= MAX_BODY_BYTES - Length(LPart), 'body capture never exceeds bound');
      Result.Body := Result.Body + LPart;
    end;
    Check(Length(Result.Body) = LExpected, 'response supplies exactly its promised body length');
    if Capability <> '' then
    begin
      Check(Pos(Capability, Result.Header) = 0, 'HTTP headers never reveal the capability');
      if Result.Status <> 200 then
        Check(Pos(Capability, Result.Body) = 0, 'error responses do not reflect capability queries');
    end;
    LClosed := False;
    try LPart := S.ReadSome(1);
    except on E: EWfcBrowserSocket do
      begin
        if Pos('reached EOF', E.Message) = 0 then raise;
        LClosed := True;
      end;
    end;
    Check(LClosed, 'server closes without an extra byte, including HEAD');
  finally S.Free; end;
end;

procedure CheckDynamicHeaders(const R: THttpResponse; const AType: String);
begin
  Check(HeaderValue(R.Header, 'Content-Type') = AType, 'dynamic response has exact MIME type');
  Check(HeaderValue(R.Header, 'Cross-Origin-Resource-Policy') = 'same-origin', 'dynamic response is same-origin');
  Check(HeaderValue(R.Header, 'Referrer-Policy') = 'no-referrer', 'download token cannot leak through referrer');
  Check(HeaderValue(R.Header, 'X-Frame-Options') = 'DENY', 'dynamic response cannot be framed');
  Check(HeaderValue(R.Header, 'Access-Control-Allow-Origin') = '', 'dynamic response does not grant CORS');
end;

function ReadCapability(const R: THttpResponse): String;
var D: TJSONData; O: TJSONObject; I: Integer;
begin
  CheckDynamicHeaders(R, 'application/json');
  Check((R.Body <> '') and (R.Body[Length(R.Body)] = #10), 'capability JSON has terminal LF');
  D := GetJSON(R.Body);
  try
    Check(D.JSONType = jtObject, 'capability document is an object');
    O := TJSONObject(D);
    Check(O.Count = 3, 'capability response has only three documented fields');
    Check(O.Get('version', 0) = 1, 'capability version');
    Check(O.Get('wavePath', '') = '/ensemble-wave-v1', 'capability wave path');
    Result := O.Get('token', '');
    Check(Length(Result) = 32, 'capability is 128 bits encoded as lowercase hex');
    for I := 1 to Length(Result) do
      Check(Result[I] in ['0'..'9', 'a'..'f'], 'capability hex spelling');
    Check(R.Body = '{"version":1,"wavePath":"/ensemble-wave-v1","token":"' +
      Result + '"}'#10, 'capability JSON has exact documented bytes');
  finally D.Free; end;
end;

function WaveTarget(const ASeconds: String; const ASeed: Cardinal;
  const AProfile: String = 'developed-period-v1'; const ATrace: String = '0'): String;
begin
  Result := '/ensemble-wave-v1?token=' + Capability + '&seconds=' + ASeconds +
    '&seed=' + IntToStr(Int64(ASeed)) + '&profile=' + AProfile +
    '&segment-cells=5&backtracks=256&pass-backtracks=16&trace=' + ATrace;
end;

procedure TCompareSink.WriteBytes(const ABytes: array of Byte);
begin
  Check((Offset >= 0) and (Length(ABytes) <= Length(Body) - Offset),
    'native reference writer stays within downloaded response');
  if Length(ABytes) > 0 then
    Check(CompareMem(@ABytes[0], @Body[Offset + 1], Length(ABytes)),
      'HTTP WAVE bytes equal independent native renderer bytes');
  Inc(Offset, Length(ABytes));
end;

procedure CompareWave(const R: THttpResponse; const ASeconds: String;
  const ASeed: Cardinal; const AProfile: TEnsembleStudioProfile; const ATrace: Boolean);
var Plan: TEnsembleStudioStreamPlan; Options: TEnsembleStudioStreamOptions;
  Source: TEnsembleStudioPcmStream; Writer: TWfcMusicWaveStream;
  Sink: TCompareSink; Samples: TWfcMusicPcm16Samples; Step: TWfcMusicArrangementStep;
begin
  CheckDynamicHeaders(R, 'audio/wav');
  Check(Pos(Capability, R.Header) = 0, 'audio headers never disclose capability');
  Plan := PlanEnsembleStudioStream(ASeconds);
  Check(HeaderValue(R.Header, 'Content-Disposition') = 'attachment; filename="ensemble-' +
    IntToStr(Int64(ASeed)) + '-' + EnsembleStudioProfileName(AProfile) + '-' +
    IntToStr(Plan.ActualTicks) + 'ticks.wav"', 'wave attachment has exact deterministic filename');
  Check(R.ContentLength = 44 + Plan.ExpectedFrames * 2, 'RIFF byte length matches duration plan');
  Options := DefaultEnsembleStudioStreamOptions;
  Options.Seed := ASeed; Options.Profile := AProfile; Options.CaptureTrace := ATrace;
  Source := nil; Writer := nil; Sink := TCompareSink.Create;
  try
    Sink.Body := R.Body;
    Source := TEnsembleStudioPcmStream.Create(Plan, Options);
    Writer := TWfcMusicWaveStream.Create(Sink, ENSEMBLE_STUDIO_STREAM_SAMPLE_RATE, Plan.ExpectedFrames);
    repeat
      Step := Source.NextSamples(Samples);
      if Step = wmaspProduced then Writer.AppendSamples(Samples);
    until Step <> wmaspProduced;
    Check(Step = wmaspCompleted, 'independent native source completes');
    Writer.Finish;
    Check(Writer.Finished and (Writer.FrameCount = Plan.ExpectedFrames), 'native WAVE reaches exact frame count');
    Check(Sink.Offset = Length(R.Body), 'comparison includes every header and PCM byte');
  finally Writer.Free; Source.Free; Sink.Free; end;
end;

procedure TestRequests;
var R: THttpResponse; T, LBadToken: String; S: TWfcBrowserSocket; LCapabilityLength: Int64;
begin
  R := Request('GET', '/', 200);
  Check(R.Body = INDEX_BYTES, 'static index is served byte-exactly');
  R := Request('HEAD', '/', 200);
  Check(R.ContentLength = Length(INDEX_BYTES), 'static HEAD declares full file length');
  R := Request('GET', '/ensemble-stream-v1', 200);
  Capability := ReadCapability(R);
  LCapabilityLength := R.ContentLength;
  R := Request('HEAD', '/ensemble-stream-v1', 200);
  CheckDynamicHeaders(R, 'application/json');
  Check(R.ContentLength = LCapabilityLength, 'capability HEAD describes exact JSON length without emitting token body');

  T := WaveTarget('1.25', 4);
  Request('GET', '/ensemble-wave-v1?seconds=1.25', 403);
  LBadToken := StringOfChar('0', 32);
  if LBadToken = Capability then LBadToken := StringOfChar('1', 32);
  Request('GET', StringReplace(T, Capability, LBadToken, []), 403);
  Request('HEAD', '/ensemble-wave-v1', 403);
  Request('GET', StringReplace(T, 'seconds=1.25', 'seconds=0', []), 400);
  Request('GET', T + '&seed=4', 400);
  Request('GET', T + '&unknown=x', 400);
  Request('GET', StringReplace(T, '&trace=0', '', []), 400);
  Request('GET', StringReplace(T, 'seed=4&', 'seed=4294967296&', []), 400);
  Request('GET', StringReplace(T, 'trace=0', 'trace=true', []), 400);
  Request('GET', StringReplace(T, 'profile=developed-period-v1', 'profile=unknown', []), 400);
  Request('GET', StringReplace(T, 'seconds=1.25', 'seconds=1%2E25', []), 400);
  Request('POST', '/ensemble-stream-v1', 405);
  Request('GET', '/', 400, 5000, 'remote.example');
  Request('GET', '/../private', 400);

  R := Request('GET', WaveTarget('61.25', 4), 200);
  Check(R.ContentLength = 5402294, 'actual download exceeds sixty seconds at native sample rate');
  CompareWave(R, '61.25', 4, espDevelopedPeriodV1, False);
  R := Request('GET', WaveTarget('1.25', High(Cardinal)), 200);
  CompareWave(R, '1.25', High(Cardinal), espDevelopedPeriodV1, False);
  R := Request('GET', WaveTarget('0.001', 4, 'structural-v1', '1'), 200);
  CompareWave(R, '0.001', 4, espStructuralV1, True);
  R := Request('HEAD', WaveTarget('1000000', 4), 200, 5000);
  CheckDynamicHeaders(R, 'audio/wav');
  Check(R.ContentLength = Int64(88200000080), 'large RF64 HEAD returns exact metadata without rendering');
  Check(HeaderValue(R.Header, 'Content-Disposition') =
    'attachment; filename="ensemble-4-developed-period-v1-960000000ticks.wav"',
    'large metadata-only download has exact deterministic filename');

  S := OpenRequest('GET', WaveTarget('61.25', 4), '127.0.0.1', 10000, R);
  try
    Check(R.Status = 200, 'abandoned download has begun');
    while Length(R.Body) <= 44 do R.Body := R.Body + S.ReadSome(256);
    Check(Length(R.Body) < R.ContentLength, 'client disconnects with unfinished declared body');
  finally S.Free; end;
  R := Request('GET', '/ensemble-stream-v1', 200);
  Check(ReadCapability(R) = Capability, 'server survives disconnect with unchanged session capability');
  R := Request('GET', WaveTarget('1.25', 4), 200);
  CompareWave(R, '1.25', 4, espDevelopedPeriodV1, False);
  R := Request('HEAD', T, 200);
  Check(R.ContentLength = 110294, 'short audio HEAD has exact WAVE size');
  while Requests < REQUEST_COUNT do
  begin
    R := Request('GET', '/', 200);
    Check(R.Body = INDEX_BYTES, 'static serving remains intact after streaming and invalid requests');
  end;
end;

procedure Main;
var G: TGuid; F: TFileStream; LIndex, LFailure: String; LStart: QWord; LRemoved: Boolean;
begin
  if ParamCount <> 2 then raise Exception.Create('usage: wfc_ensemble_http_process_test SERVER REPO_ROOT');
  ServerPath := ExpandFileName(ParamStr(1));
  Check(FileExists(ServerPath), 'native streaming server exists');
  Check(DirectoryExists(ExpandFileName(ParamStr(2))), 'repository root exists');
  if CreateGuid(G) <> 0 then raise Exception.Create('cannot name owned HTTP fixture');
  FixtureRoot := IncludeTrailingPathDelimiter(ExtractFilePath(ExpandFileName(ParamStr(0)))) +
    'ensemble-http-' + GuidToString(G);
  Check(CreateDir(FixtureRoot), 'create fresh owned static root');
  LIndex := FixtureRoot + DirectorySeparator + 'index.html';
  LFailure := '';
  try
    F := TFileStream.Create(LIndex, fmCreate);
    try F.WriteBuffer(INDEX_BYTES[1], Length(INDEX_BYTES)); finally F.Free; end;
    try
      StartChild; TestRequests;
      Check(WaitForChild(5000), 'server exits after exact accepted-connection count');
      Check(WfcProcessExitCode(Child) = 0, 'max-requests is a clean server exit');
      ReadServerOutput;
      Check((Pos(Capability, ServerOutput) = 0) and (Pos(Capability, ServerError) = 0),
        'server diagnostics never log capability or query URLs');
      Check((Pos('/ensemble-wave-v1?', ServerOutput) = 0) and
        (Pos('/ensemble-wave-v1?', ServerError) = 0), 'server diagnostics omit raw download queries');
    finally StopChild; end;
  except on E: Exception do LFailure := E.Message; end;
  try
    if FileExists(LIndex) then Check(SysUtils.DeleteFile(LIndex), 'remove only owned static fixture');
    { A stopped Windows child may release its working-directory handle after
      its exit is observable. Retry only this exact, now-empty directory. }
    LStart := WfcBrowserTickCount64;
    repeat
      LRemoved := RemoveDir(FixtureRoot);
      if LRemoved then Break;
      Sleep(5);
    until Elapsed(LStart, WfcBrowserTickCount64) >= 5000;
    Check(LRemoved, 'remove empty owned HTTP fixture directory; prior failure=' + LFailure);
  except
    if LFailure <> '' then raise Exception.Create('HTTP test failed before cleanup: ' + LFailure);
    raise;
  end;
  if LFailure <> '' then raise Exception.Create(LFailure);
end;

begin
  try
    Main;
    WriteLn('Ensemble HTTP process requests: ', Requests, '; checks: ', Checks, '/', Checks);
  except on E: Exception do
    begin
      if Capability <> '' then E.Message := StringReplace(E.Message, Capability, '[capability]', [rfReplaceAll]);
      WriteLn(StdErr, 'wfc_ensemble_http_process_test: request ', Requests, ': ', E.Message); Halt(1);
    end;
  end;
end.
