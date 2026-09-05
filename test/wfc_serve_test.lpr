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
program wfc_serve_test;

{$mode delphi}{$H+}

uses
  Classes, SysUtils, Process, Sockets, wfc_serve_http
  {$IFDEF MSWINDOWS}, Windows{$ELSE}, BaseUnix{$ENDIF};

var
  Checks: Integer;

procedure Check(const ACondition: Boolean; const ALabel: String);
begin
  Inc(Checks);
  if not ACondition then
    raise Exception.Create('check failed: ' + ALabel);
end;

procedure Target(const AValue, AExpected: String);
var
  LPath: String;
begin
  Check(DecodeWfcServeTarget(AValue, LPath), 'target accepted: ' + AValue);
  Check(LPath = AExpected, 'target exact: ' + AValue);
end;

procedure BadTarget(const AValue: String);
var
  LPath: String;
begin
  LPath := 'old';
  Check(not DecodeWfcServeTarget(AValue, LPath), 'target rejected');
  Check(LPath = '', 'rejected target cleared');
end;

procedure Request(const AValue: String; const AStatus: Integer;
  const AHead: Boolean = False);
var
  LRequest: TWfcServeRequest;
begin
  Check(ParseWfcServeRequest(AValue, LRequest) = AStatus, 'request status');
  Check(LRequest.HeadOnly = AHead, 'request HEAD flag');
end;

procedure Run;
var
  LRequest: TWfcServeRequest;
  LHeader: String;
  LRoot: String;
  LRaised: Boolean;
begin
  Target('/', '');
  Target('/?seed=1', '');
  Target('/index.html?selftest=1', 'index.html');
  Target('/assets/a%20b.js', 'assets/a b.js');
  Target('/assets%2fstyle.css', 'assets/style.css');
  Target('/a+b.txt', 'a+b.txt');
  Target('/nested/', 'nested/');
  BadTarget('');
  BadTarget('index.html');
  BadTarget('http://127.0.0.1/index.html');
  BadTarget('//server/file');
  BadTarget('/a//b');
  BadTarget('/.');
  BadTarget('/..');
  BadTarget('/a/../b');
  BadTarget('/%2e%2e/secret');
  BadTarget('/%2E./secret');
  BadTarget('/%252e%252e/secret');
  BadTarget('/a%5cb');
  BadTarget('/a\b');
  BadTarget('/a%00b');
  BadTarget('/a%0db');
  BadTarget('/a%7fb');
  BadTarget('/a%ffb');
  BadTarget('/a%');
  BadTarget('/a%2');
  BadTarget('/a%xy');
  BadTarget('/a%3ab');
  BadTarget('/a:stream');
  BadTarget('/C:/secret');
  BadTarget('/a%3fb');
  BadTarget('/a#fragment');
  BadTarget('/a?x=' + #0);
  BadTarget('/a b');
  BadTarget('/a/..%20/b');
  BadTarget('/a%20');
  BadTarget('/a.');
  BadTarget('/.git/config');
  BadTarget('/.env');
  BadTarget('/CON');
  BadTarget('/con.txt');
  BadTarget('/PRN');
  BadTarget('/aux.png');
  BadTarget('/NUL');
  BadTarget('/COM1.js');
  BadTarget('/lpt9.txt');
  BadTarget('/CLOCK$');
  BadTarget('/a*');
  BadTarget('/a|b');
  BadTarget('/a"b');
  BadTarget('/' + StringOfChar('a', WFC_SERVE_MAX_TARGET_BYTES));
  Request('GET / HTTP/1.1'#13#10'Host: 127.0.0.1:8000'#13#10#13#10, 200);
  Request('HEAD /file.wav HTTP/1.1'#13#10'Host: localhost'#13#10#13#10, 200, True);
  Request('GET / HTTP/1.0'#13#10#13#10, 200);
  Request('GET / HTTP/1.1'#13#10#13#10, 400);
  Request('GET / HTTP/1.1'#13#10'Host: remote.example'#13#10#13#10, 400);
  Request('GET / HTTP/1.1'#13#10'Host: localhost:0'#13#10#13#10, 400);
  Request('GET / HTTP/1.1'#13#10'Host: localhost:65536'#13#10#13#10, 400);
  Request('GET / HTTP/1.1'#13#10'Host: localhost:+80'#13#10#13#10, 400);
  Request('GET / HTTP/1.1'#13#10'Host: localhost'#13#10'Host: localhost'#13#10#13#10, 400);
  Request('POST / HTTP/1.1'#13#10'Host: localhost'#13#10#13#10, 405);
  Request('GET / HTTP/2.0'#13#10#13#10, 400);
  Request('GET  / HTTP/1.0'#13#10#13#10, 400);
  Request('GET / HTTP/1.0'#10#10, 400);
  Request('GET / HTTP/1.0'#13#10'Bad Header: x'#13#10#13#10, 400);
  Request('GET / HTTP/1.0'#13#10' X: y'#13#10#13#10, 400);
  Request('GET / HTTP/1.0'#13#10'X: '#0'x'#13#10#13#10, 400);
  Request('GET / HTTP/1.0'#13#10'X: x'#0#13#10#13#10, 400);
  Request('GET / HTTP/1.0'#13#10'X: a'#9'b'#13#10#13#10, 200);
  Request('GET / HTTP/1.0'#13#10'Content-Length: 0'#13#10#13#10, 200);
  Request('GET / HTTP/1.0'#13#10'Content-Length: 1'#13#10#13#10, 400);
  Request('GET / HTTP/1.0'#13#10'Content-Length: 0'#13#10'Content-Length: 0'#13#10#13#10, 400);
  Request('GET / HTTP/1.0'#13#10'Transfer-Encoding: chunked'#13#10#13#10, 400);
  Request('GET / HTTP/1.0'#13#10'Expect: 100-continue'#13#10#13#10, 400);
  Request('HEAD /../secret HTTP/1.0'#13#10#13#10, 400, True);
  Request('GET / HTTP/1.0'#13#10#13#10'trailing', 400);
  Request(StringOfChar('x', WFC_SERVE_MAX_HEADER_BYTES + 1), 431);
  Check(ParseWfcServeRequest('GET /a%20b.js?x=1 HTTP/1.0'#13#10#13#10,
    LRequest) = 200, 'request target accepted');
  Check(LRequest.Target = '/a%20b.js?x=1', 'raw target preserved');
  Check(LRequest.RelativePath = 'a b.js', 'decoded request target');
  Check(WfcServeContentType('index.HTML') = 'text/html; charset=utf-8', 'HTML');
  Check(WfcServeContentType('a.css') = 'text/css; charset=utf-8', 'CSS');
  Check(WfcServeContentType('a.js') = 'text/javascript; charset=utf-8', 'JS');
  Check(WfcServeContentType('a.json') = 'application/json', 'JSON');
  Check(WfcServeContentType('a.svg') = 'image/svg+xml', 'SVG');
  Check(WfcServeContentType('a.mid') = 'audio/midi', 'MIDI');
  Check(WfcServeContentType('a.midi') = 'audio/midi', 'MIDI alias');
  Check(WfcServeContentType('a.wav') = 'audio/wav', 'WAVE');
  Check(WfcServeContentType('a.ico') = 'image/x-icon', 'ICO');
  Check(WfcServeContentType('a.txt') = 'text/plain; charset=utf-8', 'text');
  Check(WfcServeContentType('a.wasm') = 'application/wasm', 'Wasm');
  Check(WfcServeContentType('a.bin') = 'application/octet-stream', 'binary');
  LHeader := WfcServeResponseHeader(200, Int64(4294967296), 'audio/wav');
  Check(Pos('Content-Length: 4294967296'#13#10, LHeader) > 0, 'wide content length');
  Check(Pos('Connection: close'#13#10, LHeader) > 0, 'connection close');
  Check(Pos('X-Content-Type-Options: nosniff'#13#10, LHeader) > 0, 'no MIME sniff');
  Check(Copy(LHeader, Length(LHeader) - 3, 4) = #13#10#13#10, 'header terminator');
  LHeader := WfcServeResponseHeader(405, 0, 'text/plain');
  Check(Pos('Allow: GET, HEAD'#13#10, LHeader) > 0, 'method allow');
  LHeader := WfcServeResponseHeader(301, 0, 'text/plain', '/nested/');
  Check(Pos('Location: /nested/'#13#10, LHeader) > 0, 'redirect location');
  LRaised := False;
  try WfcServeResponseHeader(200, -1, 'text/plain');
  except on EWfcServe do LRaised := True; end;
  Check(LRaised, 'negative response length rejected');
  LRaised := False;
  try WfcServeResponseHeader(200, 0, 'text/plain'#13#10'Injected: yes');
  except on EWfcServe do LRaised := True; end;
  Check(LRaised, 'response header injection rejected');
  LRaised := False;
  try WfcServeResponseHeader(301, 0, 'text/plain', '/'#10'Injected: yes');
  except on EWfcServe do LRaised := True; end;
  Check(LRaised, 'redirect injection rejected');
  LRoot := ValidateWfcServeRoot(GetCurrentDir);
  Check(ExtractFileDrive(LRoot) = ExtractFileDrive(ExpandFileName(GetCurrentDir)),
    'root drive retained');
  Check(LRoot[Length(LRoot)] = DirectorySeparator, 'root trailing separator');
  LRaised := False;
  try ValidateWfcServeRoot('');
  except on EWfcServe do LRaised := True; end;
  Check(LRaised, 'empty root rejected');
  LRaised := False;
  try ValidateWfcServeRoot('wfc-serve-nonexistent-test-root');
  except on EWfcServe do LRaised := True; end;
  Check(LRaised, 'nonexistent root rejected');
end;


function UnusedLoopbackPort: Integer;
var
  LSocket: Integer;
  LAddress: TInetSockAddr;
  LLength: TSockLen;
begin
  LSocket := fpSocket(AF_INET, SOCK_STREAM, 0);
  if LSocket < 0 then
    raise Exception.Create('cannot create port-selection socket');
  try
    FillChar(LAddress, SizeOf(LAddress), 0);
    {$IFDEF DARWIN}LAddress.sin_len := SizeOf(LAddress);{$ENDIF}
    LAddress.sin_family := AF_INET;
    LAddress.sin_addr := StrToNetAddr('127.0.0.1');
    if fpBind(LSocket, @LAddress, SizeOf(LAddress)) <> 0 then
      raise Exception.Create('cannot select unused loopback port');
    LLength := SizeOf(LAddress);
    if fpGetSockName(LSocket, @LAddress, @LLength) <> 0 then
      raise Exception.Create('cannot inspect selected loopback port');
    Result := NToHs(LAddress.sin_port);
  finally
    CloseSocket(LSocket);
  end;
end;

function ConnectClient(const APort: Integer): Integer;
var
  LAddress: TInetSockAddr;
  {$IFDEF MSWINDOWS}LTimeout: DWORD;{$ELSE}LTimeout: TTimeVal;{$ENDIF}
  {$IFDEF DARWIN}LNoSigPipe: LongInt;{$ENDIF}
begin
  Result := fpSocket(AF_INET, SOCK_STREAM, 0);
  if Result < 0 then
    Exit;
  FillChar(LAddress, SizeOf(LAddress), 0);
  {$IFDEF DARWIN}LAddress.sin_len := SizeOf(LAddress);{$ENDIF}
  LAddress.sin_family := AF_INET;
  LAddress.sin_port := htons(Word(APort));
  LAddress.sin_addr := StrToNetAddr('127.0.0.1');
  if fpConnect(Result, @LAddress, SizeOf(LAddress)) <> 0 then
  begin
    CloseSocket(Result);
    Exit(-1);
  end;
  {$IFDEF DARWIN}
  LNoSigPipe := 1;
  if fpSetSockOpt(Result, SOL_SOCKET, SO_NOSIGPIPE,
      @LNoSigPipe, SizeOf(LNoSigPipe)) <> 0 then
  begin
    CloseSocket(Result);
    Exit(-1);
  end;
  {$ENDIF}
  {$IFDEF MSWINDOWS}
  LTimeout := 4000;
  {$ELSE}
  LTimeout.tv_sec := 4;
  LTimeout.tv_usec := 0;
  {$ENDIF}
  if (fpSetSockOpt(Result, SOL_SOCKET, SO_RCVTIMEO,
      @LTimeout, SizeOf(LTimeout)) <> 0) or
      (fpSetSockOpt(Result, SOL_SOCKET, SO_SNDTIMEO,
      @LTimeout, SizeOf(LTimeout)) <> 0) then
  begin
    CloseSocket(Result);
    Exit(-1);
  end;
end;

function Exchange(const APort: Integer; const ARequest: String): String;
var
  LSocket, LCount, LOffset: Integer;
  LBuffer: array[0..8191] of Char;
  LPart: String;
begin
  LSocket := ConnectClient(APort);
  if LSocket < 0 then
    raise Exception.Create('cannot connect to owned test server');
  try
    LOffset := 1;
    while LOffset <= Length(ARequest) do
    begin
      LCount := fpSend(LSocket, @ARequest[LOffset],
        Length(ARequest) - LOffset + 1,
        {$IFDEF LINUX}MSG_NOSIGNAL{$ELSE}0{$ENDIF});
      if LCount <= 0 then
        raise Exception.Create('test request send failed');
      Inc(LOffset, LCount);
    end;
    Result := '';
    repeat
      LCount := fpRecv(LSocket, @LBuffer[0], SizeOf(LBuffer), 0);
      if LCount < 0 then
        raise Exception.Create('test response receive failed or timed out');
      if LCount = 0 then
        Break;
      if Length(Result) + LCount > 2 * 1024 * 1024 then
        raise Exception.Create('test response exceeds its fixed bound');
      SetString(LPart, PChar(@LBuffer[0]), LCount);
      Result := Result + LPart;
    until False;
  finally
    CloseSocket(LSocket);
  end;
end;

procedure AbandonResponse(const APort: Integer);
var
  LSocket: Integer;
  LRequest: String;
begin
  LSocket := ConnectClient(APort);
  if LSocket < 0 then
    raise Exception.Create('cannot connect abandoned-response fixture');
  try
    LRequest := 'GET /binary.wav HTTP/1.0'#13#10#13#10;
    if fpSend(LSocket, @LRequest[1], Length(LRequest),
        {$IFDEF LINUX}MSG_NOSIGNAL{$ELSE}0{$ENDIF}) <> Length(LRequest) then
      raise Exception.Create('cannot send abandoned-response fixture');
    { Refuse the response after requesting several output chunks. The
      server must treat EPIPE/reset as a client failure, never SIGPIPE exit. }
    fpShutdown(LSocket, SHUT_RDWR);
  finally
    CloseSocket(LSocket);
  end;
end;

procedure WriteFixture(const APath, ABytes: String);
var
  LStream: TFileStream;
begin
  if FileExists(APath) then
    raise Exception.Create('test fixture already exists');
  LStream := TFileStream.Create(APath, fmCreate);
  try
    if ABytes <> '' then
      LStream.WriteBuffer(ABytes[1], Length(ABytes));
  finally
    LStream.Free;
  end;
end;

procedure MakeDirectoryLink(const ALink, ATarget: String);
{$IFDEF MSWINDOWS}
type
  TMountPoint = packed record
    Tag: Cardinal;
    DataLength: Word;
    Reserved: Word;
    SubstituteOffset: Word;
    SubstituteLength: Word;
    PrintOffset: Word;
    PrintLength: Word;
    Buffer: array[0..2047] of WideChar;
  end;
var
  LMount: TMountPoint;
  LSubstitute, LPrint: UnicodeString;
  LHandle: THandle;
  LReturned: DWORD;
begin
  LPrint := UnicodeString(ExpandFileName(ATarget));
  LSubstitute := '\??\' + LPrint;
  if Length(LSubstitute) + Length(LPrint) + 2 > Length(LMount.Buffer) then
    raise Exception.Create('directory-link test path exceeds fixed buffer');
  if not CreateDir(ALink) then
    raise Exception.Create('cannot create directory-link fixture');
  FillChar(LMount, SizeOf(LMount), 0);
  LMount.Tag := Cardinal($A0000003);
  LMount.SubstituteLength := Length(LSubstitute) * 2;
  LMount.PrintOffset := LMount.SubstituteLength + 2;
  LMount.PrintLength := Length(LPrint) * 2;
  LMount.DataLength := 8 + LMount.PrintOffset + LMount.PrintLength + 2;
  Move(LSubstitute[1], LMount.Buffer[0], LMount.SubstituteLength);
  Move(LPrint[1], LMount.Buffer[LMount.PrintOffset div 2], LMount.PrintLength);
  LHandle := CreateFile(PChar(ALink), GENERIC_WRITE, 0, nil,
    OPEN_EXISTING, DWORD($00200000) or FILE_FLAG_BACKUP_SEMANTICS, 0);
  if LHandle = THandle(-1) then
    raise Exception.Create('cannot open directory-link fixture');
  try
    if not DeviceIoControl(LHandle, $000900A4, @LMount,
        LMount.DataLength + 8, nil, 0, LReturned, nil) then
      raise Exception.Create('cannot create junction fixture');
  finally
    FileClose(LHandle);
  end;
end;
{$ELSE}
begin
  if fpSymlink(PChar(ExpandFileName(ATarget)), PChar(ALink)) <> 0 then
    raise Exception.Create('cannot create directory-link fixture');
end;
{$ENDIF}

procedure RemoveDirectoryLink(const APath: String);
begin
  {$IFDEF MSWINDOWS}
  RemoveDir(APath);
  {$ELSE}
  SysUtils.DeleteFile(APath);
  {$ENDIF}
end;

procedure LiveChecks(const AServer, AParent: String);
var
  LBase, LRoot, LOutside, LBytes, LResponse, LRequest: String;
  LProcess: TProcess;
  LPort, LSocket, I, LSplit: Integer;
  LStart: QWord;
  LRaised: Boolean;

  procedure Response(const AMethod, ATarget: String;
    const AStatus: Integer; const ABody: String);
  begin
    LRequest := AMethod + ' ' + ATarget + ' HTTP/1.1'#13#10 +
      'Host: 127.0.0.1:' + IntToStr(LPort) + #13#10#13#10;
    LResponse := Exchange(LPort, LRequest);
    Check(Pos('HTTP/1.1 ' + IntToStr(AStatus) + ' ', LResponse) = 1,
      'live HTTP status: ' + ATarget);
    LSplit := Pos(#13#10#13#10, LResponse);
    Check(LSplit > 0, 'live response header boundary');
    Check(Copy(LResponse, LSplit + 4, MaxInt) = ABody,
      'live body exact: ' + ATarget);
  end;

begin
  if not FileExists(AServer) then
    raise Exception.Create('integration server executable does not exist');
  if not DirectoryExists(AParent) then
    raise Exception.Create('integration output parent must already exist');
  LBase := IncludeTrailingPathDelimiter(ExpandFileName(AParent)) +
    'wfc-serve-test-' + IntToStr(GetTickCount64);
  if DirectoryExists(LBase) or not CreateDir(LBase) then
    raise Exception.Create('cannot create isolated integration fixture');
  LRoot := LBase + DirectorySeparator + 'www';
  LOutside := LBase + DirectorySeparator + 'outside';
  LProcess := nil;
  try
    Check(CreateDir(LRoot), 'create serving root');
    Check(CreateDir(LOutside), 'create outside fixture');
    Check(CreateDir(LRoot + '/nested'), 'create nested fixture');
    Check(CreateDir(LRoot + '/empty'), 'create empty fixture');
    WriteFixture(LRoot + '/index.html', '<p>WFC server fixture</p>');
    WriteFixture(LRoot + '/nested/index.html', '<p>nested</p>');
    WriteFixture(LRoot + '/a b.txt', 'space name');
    WriteFixture(LRoot + '/empty.txt', '');
    WriteFixture(LOutside + '/secret.txt', 'outside must not be served');
    SetLength(LBytes, 262267);
    for I := 1 to Length(LBytes) do
      LBytes[I] := Chr((I - 1) mod 256);
    WriteFixture(LRoot + '/binary.wav', LBytes);
    MakeDirectoryLink(LRoot + '/escape', LOutside);
    LRaised := False;
    try ValidateWfcServeRoot(LRoot + '/escape');
    except on EWfcServe do LRaised := True; end;
    Check(LRaised, 'link cannot be selected as root');
    LPort := UnusedLoopbackPort;
    LProcess := TProcess.Create(nil);
    LProcess.Executable := ExpandFileName(AServer);
    LProcess.Parameters.Add('--root');
    LProcess.Parameters.Add(LRoot);
    LProcess.Parameters.Add('--port');
    LProcess.Parameters.Add(IntToStr(LPort));
    LProcess.Options := [poUsePipes, poNoConsole];
    LProcess.Execute;
    LStart := GetTickCount64;
    repeat
      if not LProcess.Running then
        raise Exception.Create('owned test server exited during startup');
      LSocket := ConnectClient(LPort);
      if LSocket >= 0 then
      begin
        CloseSocket(LSocket);
        Break;
      end;
      if GetTickCount64 - LStart > 5000 then
        raise Exception.Create('owned test server startup deadline exceeded');
      Sleep(25);
    until False;
    Response('GET', '/', 200, '<p>WFC server fixture</p>');
    Response('GET', '/binary.wav', 200, LBytes);
    Check(Pos('Content-Type: audio/wav'#13#10, LResponse) > 0,
      'live binary MIME');
    Check(Pos('Content-Length: 262267'#13#10, LResponse) > 0,
      'live binary size');
    Response('HEAD', '/binary.wav', 200, '');
    Check(Pos('Content-Length: 262267'#13#10, LResponse) > 0,
      'HEAD preserves represented byte length');
    Response('GET', '/a%20b.txt?query=ignored', 200, 'space name');
    Response('GET', '/empty.txt', 200, '');
    Check(Pos('Content-Length: 0'#13#10, LResponse) > 0, 'empty file length');
    Response('GET', '/nested?selftest=1', 301, 'Moved Permanently'#10);
    Check(Pos('Location: /nested/?selftest=1'#13#10, LResponse) > 0,
      'directory redirect preserves query');
    Response('GET', '/nested/', 200, '<p>nested</p>');
    Response('GET', '/empty/', 404, 'Not Found'#10);
    Response('GET', '/missing', 404, 'Not Found'#10);
    Response('GET', '/%2e%2e/outside/secret.txt', 400, 'Bad Request'#10);
    Response('HEAD', '/%2e%2e/outside/secret.txt', 400, '');
    Response('GET', '/escape/secret.txt', 404, 'Not Found'#10);
    Response('POST', '/', 405, 'Method Not Allowed'#10);
    Check(Pos('Allow: GET, HEAD'#13#10, LResponse) > 0, 'live method allow');
    LResponse := Exchange(LPort,
      'GET / HTTP/1.1'#13#10'Host: remote.example'#13#10#13#10);
    Check(Pos('HTTP/1.1 400 ', LResponse) = 1, 'live foreign Host rejected');
    LRequest := 'GET / HTTP/1.0'#13#10'X: ';
    LRequest := LRequest + StringOfChar('x',
      WFC_SERVE_MAX_HEADER_BYTES + 1 - Length(LRequest));
    LResponse := Exchange(LPort, LRequest);
    Check(Pos('HTTP/1.1 431 ', LResponse) = 1, 'live header byte limit');
    LStart := GetTickCount64;
    LResponse := Exchange(LPort, '');
    Check(Pos('HTTP/1.1 408 ', LResponse) = 1, 'idle client times out');
    Check(GetTickCount64 - LStart < 4000, 'idle timeout is bounded');
    for I := 1 to 8 do
      AbandonResponse(LPort);
    Response('GET', '/', 200, '<p>WFC server fixture</p>');
    Check(LProcess.Running, 'server survives invalid, idle and abandoned clients');
  finally
    if LProcess <> nil then
    begin
      if LProcess.Running then
        LProcess.Terminate(0);
      LProcess.WaitOnExit(5000);
      LProcess.Free;
    end;
    { Exact paths created above only; never enumerate or recurse. }
    RemoveDirectoryLink(LRoot + '/escape');
    SysUtils.DeleteFile(LRoot + '/index.html');
    SysUtils.DeleteFile(LRoot + '/nested/index.html');
    SysUtils.DeleteFile(LRoot + '/a b.txt');
    SysUtils.DeleteFile(LRoot + '/empty.txt');
    SysUtils.DeleteFile(LRoot + '/binary.wav');
    SysUtils.DeleteFile(LOutside + '/secret.txt');
    RemoveDir(LRoot + '/nested');
    RemoveDir(LRoot + '/empty');
    RemoveDir(LRoot);
    RemoveDir(LOutside);
    RemoveDir(LBase);
  end;
end;

begin
  try
    Run;
    if ParamCount <> 0 then
    begin
      if (ParamCount <> 3) or (ParamStr(1) <> '--integration') then
        raise Exception.Create('Usage: wfc_serve_test [--integration SERVER OUTPUT_PARENT]');
      LiveChecks(ParamStr(2), ParamStr(3));
    end;
    WriteLn('WFC static server checks: ', Checks, '/', Checks);
  except
    on E: Exception do
    begin
      WriteLn(StdErr, E.Message);
      ExitCode := 1;
    end;
  end;
end.
