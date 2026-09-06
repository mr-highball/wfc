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
  const AHead: Boolean = False; const ABindAddress: String = '127.0.0.1');
var
  LRequest: TWfcServeRequest;
begin
  Check(ParseWfcServeRequest(AValue, LRequest, ABindAddress) = AStatus,
    'request status for bind ' + ABindAddress);
  Check(LRequest.HeadOnly = AHead, 'request HEAD flag');
end;

procedure BindAddressChecks;

  procedure Address(const AValue: String; const AExpected: Boolean);
  begin
    Check(ValidWfcServeBindAddress(AValue) = AExpected,
      'canonical private or loopback bind address: ' + AValue);
  end;

  procedure Host(const ABindAddress, AHost: String; const AStatus: Integer);
  begin
    Request('GET / HTTP/1.1'#13#10'Host: ' + AHost + #13#10#13#10,
      AStatus, False, ABindAddress);
  end;

begin
  Address('127.0.0.1', True);
  Address('127.0.0.2', True);
  Address('127.1.2.3', True);
  Address('10.0.0.1', True);
  Address('10.255.255.254', True);
  Address('172.16.0.1', True);
  Address('172.31.255.254', True);
  Address('192.168.0.1', True);
  Address('192.168.255.254', True);
  Address('', False);
  Address('0.0.0.0', False);
  Address('*', False);
  Address('localhost', False);
  Address('example.test', False);
  Address('::1', False);
  Address('[::1]', False);
  Address('::ffff:127.0.0.1', False);
  Address('8.8.8.8', False);
  Address('9.255.255.254', False);
  Address('11.0.0.1', False);
  Address('126.255.255.254', False);
  Address('128.0.0.1', False);
  Address('169.254.1.2', False);
  Address('172.15.255.254', False);
  Address('172.32.0.1', False);
  Address('192.167.255.254', False);
  Address('192.169.0.1', False);
  Address('224.0.0.1', False);
  Address('255.255.255.255', False);
  Address('127.1', False);
  Address('2130706433', False);
  Address('0x7f.0.0.1', False);
  Address('0177.0.0.1', False);
  Address('127.00.0.1', False);
  Address('127.0.00.1', False);
  Address('127.0.0.01', False);
  Address('127.0.0.256', False);
  Address('127.0.0.-1', False);
  Address('127.0.0.+1', False);
  Address('127.0.0.1.', False);
  Address('127.0..1', False);
  Address('.127.0.0.1', False);
  Address('127.0.0.1:8000', False);
  Address(' 127.0.0.1', False);
  Address('127.0.0.1 ', False);
  Address('127.0.0.1'#0, False);
  Address('127.0.0.1'#10, False);
  Host('127.0.0.1', '127.0.0.1', 200);
  Host('127.0.0.1', 'LOCALHOST:8000', 200);
  Host('127.0.0.1', '127.0.0.2', 400);
  Host('127.0.0.1', '192.168.1.20', 400);
  Host('127.0.0.1', 'remote.example', 400);
  Host('127.0.0.2', '127.0.0.2', 200);
  Host('127.0.0.2', '127.0.0.2:1', 200);
  Host('127.0.0.2', '127.0.0.2:65535', 200);
  Host('127.0.0.2', '127.0.0.1', 400);
  Host('127.0.0.2', 'localhost', 400);
  Host('127.0.0.2', 'LOCALHOST:8000', 400);
  Host('192.168.1.20', '192.168.1.20:8000', 200);
  Host('192.168.1.20', '192.168.1.21:8000', 400);
  Host('192.168.1.20', '192.168.001.20:8000', 400);
  Host('192.168.1.20', '192.168.1.20.example:8000', 400);
  Host('192.168.1.20', 'localhost:8000', 400);
  Host('192.168.1.20', '192.168.1.20:0', 400);
  Host('192.168.1.20', '192.168.1.20:65536', 400);
  Host('192.168.1.20', '192.168.1.20:+8000', 400);
  Host('192.168.1.20', '192.168.1.20:8000:80', 400);
  Host('10.2.3.4', '10.2.3.4', 200);
  Host('172.16.2.3', '172.16.2.3', 200);
  Host('0.0.0.0', '0.0.0.0', 400);
  Host('8.8.8.8', '8.8.8.8', 400);
  Host('localhost', 'localhost', 400);
  Request('GET / HTTP/1.0'#13#10#13#10, 200, False, '127.0.0.2');
  Request('GET / HTTP/1.1'#13#10#13#10, 400, False, '127.0.0.2');
  Request('GET / HTTP/1.0'#13#10#13#10, 400, False, '0.0.0.0');
  Request('GET / HTTP/1.1'#13#10'Host: 127.0.0.2'#13#10 +
    'Host: 127.0.0.2'#13#10#13#10, 400, False, '127.0.0.2');
end;

procedure Run;
var
  LRequest: TWfcServeRequest;
  LHeader: String;
  LRoot: String;
  LRaised: Boolean;
begin
  BindAddressChecks;
  Check(WFC_SERVE_VERSION = 2, 'explicit bind server version');
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


function UnusedLoopbackPort(const AAddress: String = '127.0.0.1'): Integer;
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
    LAddress.sin_addr := StrToNetAddr(AAddress);
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

function ConnectClient(const APort: Integer;
  const AAddress: String = '127.0.0.1'): Integer;
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
  LAddress.sin_addr := StrToNetAddr(AAddress);
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

function Exchange(const APort: Integer; const ARequest: String;
  const AAddress: String = '127.0.0.1'): String;
var
  LSocket, LCount, LOffset: Integer;
  LBuffer: array[0..8191] of Char;
  LPart: String;
begin
  LSocket := ConnectClient(APort, AAddress);
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

function WaitForOwnedExit(const AProcess: TProcess;
  const ATimeout: DWord): Boolean;
begin
  { Unix Running/Terminate may already reap the child. FPC's timed wait
    still calls waitpid then and reports ECHILD, not a timeout. Preserve
    the stopped state and exit status without waiting on a reaped child. }
  if not AProcess.Running then
    Exit(True);
  Result := AProcess.WaitOnExit(ATimeout);
  if not Result then
    Result := not AProcess.Running;
end;

procedure LiveChecks(const AServer, AParent: String);
var
  LBase, LRoot, LOutside, LBytes, LResponse, LRequest, LBindAddress: String;
  LProcess: TProcess;
  LPort, I, LSplit: Integer;
  LStart: QWord;
  LRaised, LExplicitBind: Boolean;

  procedure StopServer;
  begin
    if LProcess = nil then
      Exit;
    try
      { Existing TProcess.Terminate can itself wait internally on Unix. The
        helper below bounds only the subsequent exit observation, not that
        OS/RTL termination call or total cleanup under an arbitrary stall. }
      if LProcess.Running then
        LProcess.Terminate(0);
      Check(WaitForOwnedExit(LProcess, 5000), 'owned server post-termination wait completes');
    finally
      FreeAndNil(LProcess);
    end;
  end;

  procedure StartServer;
  var
    LSocket: Integer;
    LStarted: QWord;
  begin
    LProcess := TProcess.Create(nil);
    LProcess.Executable := ExpandFileName(AServer);
    LProcess.Parameters.Add('--root');
    LProcess.Parameters.Add(LRoot);
    LProcess.Parameters.Add('--port');
    LProcess.Parameters.Add(IntToStr(LPort));
    if LExplicitBind then
    begin
      LProcess.Parameters.Add('--bind');
      LProcess.Parameters.Add(LBindAddress);
    end;
    LProcess.Options := [poUsePipes, poNoConsole];
    LProcess.Execute;
    LStarted := GetTickCount64;
    repeat
      if not LProcess.Running then
        raise Exception.Create('owned test server exited during startup');
      LSocket := ConnectClient(LPort, LBindAddress);
      if LSocket >= 0 then
      begin
        CloseSocket(LSocket);
        Break;
      end;
      if GetTickCount64 - LStarted > 5000 then
        raise Exception.Create('owned test server startup deadline exceeded');
      Sleep(25);
    until False;
  end;

  procedure ListenerCollision;
  var
    LOther: TProcess;
    LError: String;
    LLength, LExitStatus: Integer;
  begin
    LOther := TProcess.Create(nil);
    try
      LOther.Executable := ExpandFileName(AServer);
      LOther.Parameters.Add('--root');
      LOther.Parameters.Add(LRoot);
      LOther.Parameters.Add('--port');
      LOther.Parameters.Add(IntToStr(LPort));
      if LExplicitBind then
      begin
        LOther.Parameters.Add('--bind');
        LOther.Parameters.Add(LBindAddress);
      end;
      LOther.Options := [poUsePipes, poNoConsole];
      LOther.Execute;
      Check(WaitForOwnedExit(LOther, 5000), 'active listener collision fails promptly');
      LExitStatus := LOther.ExitStatus;
      Check(WaitForOwnedExit(LOther, 0), 'already-reaped colliding server remains stopped');
      Check(LOther.ExitStatus = LExitStatus, 'repeated stopped-child wait preserves exit status');
      Check(LOther.ExitStatus <> 0, 'active listener collision fails closed');
      LLength := LOther.Stderr.NumBytesAvailable;
      Check((LLength > 0) and (LLength <= 4096),
        'listener collision has a bounded diagnostic');
      SetLength(LError, LLength);
      LOther.Stderr.ReadBuffer(LError[1], LLength);
      Check(Pos('cannot bind ' + LBindAddress + ':' + IntToStr(LPort), LError) > 0,
        'listener collision reports the exact occupied loopback address');
      Check(LProcess.Running, 'listener collision leaves owner running');
    finally
      if LOther.Running then
        LOther.Terminate(0);
      Check(WaitForOwnedExit(LOther, 5000), 'colliding server post-termination wait completes');
      LOther.Free;
    end;
  end;

  procedure VersionCLI;
  var
    LOther: TProcess;
    LText: String;
    LLength: Integer;
  begin
    LOther := TProcess.Create(nil);
    try
      LOther.Executable := ExpandFileName(AServer);
      LOther.Parameters.Add('--version');
      LOther.Options := [poUsePipes, poNoConsole];
      LOther.Execute;
      Check(WaitForOwnedExit(LOther, 5000), 'version CLI exits promptly');
      Check(LOther.ExitStatus = 0, 'version CLI succeeds');
      LLength := LOther.Output.NumBytesAvailable;
      Check((LLength > 0) and (LLength <= 256), 'version CLI has bounded output');
      SetLength(LText, LLength);
      LOther.Output.ReadBuffer(LText[1], LLength);
      Check(Trim(LText) = 'WFC static server 2', 'version CLI reports explicit-bind version');
      Check(LOther.Stderr.NumBytesAvailable = 0, 'version CLI has no errors');
    finally
      try
        if LOther.Running then
          LOther.Terminate(0);
        Check(WaitForOwnedExit(LOther, 5000), 'version CLI post-termination wait completes');
      finally
        LOther.Free;
      end;
    end;
  end;

  procedure SeparateAddressListener;
  var
    LOther: TProcess;
    LText: String;
    LSocket: Integer;
    LStarted: QWord;
  begin
    LOther := TProcess.Create(nil);
    try
      { Both listeners are owned by this test. Coexistence on one port
        proves the selected-address bind is not silently INADDR_ANY. }
      LOther.Executable := ExpandFileName(AServer);
      LOther.Parameters.Add('--root');
      LOther.Parameters.Add(LRoot);
      LOther.Parameters.Add('--port');
      LOther.Parameters.Add(IntToStr(LPort));
      LOther.Options := [poUsePipes, poNoConsole];
      LOther.Execute;
      LStarted := GetTickCount64;
      repeat
        if not LOther.Running then
          raise Exception.Create('separate-address listener exited during startup');
        LSocket := ConnectClient(LPort);
        if LSocket >= 0 then
        begin
          CloseSocket(LSocket);
          Break;
        end;
        if GetTickCount64 - LStarted > 5000 then
          raise Exception.Create('separate-address listener startup deadline exceeded');
        Sleep(25);
      until False;
      Check(LOther.Running, 'separate-address listener starts on the same port');
      LText := Exchange(LPort, 'GET / HTTP/1.1'#13#10 +
        'Host: 127.0.0.1'#13#10#13#10);
      Check(Pos('HTTP/1.1 200 ', LText) = 1,
        'separate-address listener serves on the same port');
      Check(Pos('<p>WFC server fixture</p>', LText) > 0,
        'separate-address listener serves the owned fixture');
      Check(LOther.Running and LProcess.Running,
        'distinct owned addresses coexist on the same port');
    finally
      try
        if LOther.Running then
          LOther.Terminate(0);
        Check(WaitForOwnedExit(LOther, 5000), 'separate-address listener exit completes');
      finally
        LOther.Free;
      end;
    end;
  end;

  procedure BadBindCLI(const AArguments: array of String);
  var
    LOther: TProcess;
    LError: String;
    LLength, J: Integer;
  begin
    LOther := TProcess.Create(nil);
    try
      LOther.Executable := ExpandFileName(AServer);
      LOther.Parameters.Add('--root');
      LOther.Parameters.Add(LRoot);
      LOther.Parameters.Add('--port');
      LOther.Parameters.Add(IntToStr(LPort));
      for J := Low(AArguments) to High(AArguments) do
        LOther.Parameters.Add(AArguments[J]);
      LOther.Options := [poUsePipes, poNoConsole];
      LOther.Execute;
      Check(WaitForOwnedExit(LOther, 5000), 'bad bind CLI exits promptly');
      Check(LOther.ExitStatus <> 0, 'bad bind CLI fails closed');
      LLength := LOther.Stderr.NumBytesAvailable;
      Check((LLength > 0) and (LLength <= 4096), 'bad bind CLI has a bounded diagnostic');
      SetLength(LError, LLength);
      LOther.Stderr.ReadBuffer(LError[1], LLength);
      Check(Pos('bind', LowerCase(LError)) > 0, 'bad bind CLI identifies its argument');
      Check(Pos('cannot bind ', LowerCase(LError)) = 0,
        'bad bind CLI is rejected before attempting a socket bind');
    finally
      try
        if LOther.Running then
          LOther.Terminate(0);
        Check(WaitForOwnedExit(LOther, 5000), 'bad bind CLI post-termination wait completes');
      finally
        LOther.Free;
      end;
    end;
  end;

  procedure LiveHost(const AHost: String; const AStatus: Integer);
  begin
    LResponse := Exchange(LPort, 'GET / HTTP/1.1'#13#10'Host: ' +
      AHost + #13#10#13#10, LBindAddress);
    Check(Pos('HTTP/1.1 ' + IntToStr(AStatus) + ' ', LResponse) = 1,
      'live configured-address Host check: ' + AHost);
  end;

  procedure Response(const AMethod, ATarget: String;
    const AStatus: Integer; const ABody: String);
  begin
    LRequest := AMethod + ' ' + ATarget + ' HTTP/1.1'#13#10 +
      'Host: ' + LBindAddress + ':' + IntToStr(LPort) + #13#10#13#10;
    LResponse := Exchange(LPort, LRequest, LBindAddress);
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
  LBindAddress := '127.0.0.1';
  LExplicitBind := False;
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
    VersionCLI;
    BadBindCLI(['--bind']);
    BadBindCLI(['--bind', '']);
    BadBindCLI(['--bind', '0.0.0.0']);
    BadBindCLI(['--bind', '*']);
    BadBindCLI(['--bind', 'localhost']);
    BadBindCLI(['--bind', '8.8.8.8']);
    BadBindCLI(['--bind', '::1']);
    BadBindCLI(['--bind', '127.0.0.01']);
    BadBindCLI(['--bind', '127.0.0.1', '--bind', '127.0.0.2']);
    StartServer;
    Check(not WaitForOwnedExit(LProcess, 0), 'live server is not accepted as stopped');
    ListenerCollision;
    LiveHost('localhost:' + IntToStr(LPort), 200);
    LiveHost('127.0.0.2:' + IntToStr(LPort), 400);
    LiveHost('192.168.1.20:' + IntToStr(LPort), 400);
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
    {$IFNDEF MSWINDOWS}
    { Exchange waits for server EOF before closing the client. Thus the
      server actively closes each response and leaves TIME_WAIT connections.
      Restart immediately on the same port, without waiting for TCP expiry,
      and prove reuse never allows a second active listener to take over. }
    for I := 1 to 3 do
    begin
      StopServer;
      StartServer;
      ListenerCollision;
      Response('GET', '/', 200, '<p>WFC server fixture</p>');
    end;
    {$ENDIF}
    StopServer;
    LExplicitBind := True;
    LPort := UnusedLoopbackPort;
    StartServer;
    ListenerCollision;
    Response('GET', '/', 200, '<p>WFC server fixture</p>');
    Response('HEAD', '/binary.wav', 200, '');
    LiveHost('localhost:' + IntToStr(LPort), 200);
    LiveHost('127.0.0.2:' + IntToStr(LPort), 400);
    LiveHost('192.168.1.20:' + IntToStr(LPort), 400);
    {$IFNDEF DARWIN}
    { Windows/Linux provide alternate 127/8 loopback addresses without
      interface setup. Darwin's portable explicit-bind case is above. }
    StopServer;
    LBindAddress := '127.0.0.2';
    LPort := UnusedLoopbackPort(LBindAddress);
    StartServer;
    SeparateAddressListener;
    ListenerCollision;
    Response('GET', '/', 200, '<p>WFC server fixture</p>');
    Response('HEAD', '/binary.wav', 200, '');
    LiveHost('127.0.0.2', 200);
    LiveHost('127.0.0.1:' + IntToStr(LPort), 400);
    LiveHost('localhost:' + IntToStr(LPort), 400);
    LiveHost('192.168.1.20:' + IntToStr(LPort), 400);
    LiveHost('remote.example', 400);
    LiveHost('127.0.0.2:' + IntToStr(LPort) + ':80', 400);
    Check(LProcess.Running, 'explicit-bind server survives foreign Host requests');
    {$ENDIF}
  finally
    StopServer;
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
