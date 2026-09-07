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
program wfc_browser_socket_test;

{$mode delphi}{$H+}
{$IFDEF PAS2JS}{$FATAL this transport fixture requires native FPC}{$ENDIF}

uses
  SysUtils, wfc_browser_socket,
  {$IFDEF MSWINDOWS}Windows, WinSock2{$ELSE}Sockets, BaseUnix{$ENDIF};

const
  BAD_PORTS: array[0..2] of Integer = (-1, 0, 65536);
  BAD_READS: array[0..2] of Integer = (-1, 0, 65537);

var Checks: Integer;

procedure Check(const ACondition: Boolean; const ALabel: String);
begin
  Inc(Checks);
  if not ACondition then raise Exception.Create('check failed: ' + ALabel);
end;

procedure ClosePeer(var AHandle: PtrInt);
begin
  if AHandle = -1 then Exit;
  {$IFDEF MSWINDOWS}WinSock2.closesocket(TSocket(AHandle));
  {$ELSE}fpClose(AHandle);{$ENDIF}
  AHandle := -1;
end;

procedure NonBlocking(const AHandle: PtrInt);
{$IFDEF MSWINDOWS}var LValue: u_long;
{$ELSE}var LFlags: Integer;{$ENDIF}
begin
  {$IFDEF MSWINDOWS}
  LValue := 1;
  Check(ioctlsocket(TSocket(AHandle), LongInt(FIONBIO), LValue) = 0,
    'fixture nonblocking');
  {$ELSE}
  LFlags := fpFcntl(AHandle, F_GETFL, 0);
  Check((LFlags >= 0) and
    (fpFcntl(AHandle, F_SETFL, LFlags or O_NONBLOCK) = 0), 'fixture nonblocking');
  {$ENDIF}
end;

function NewListener(out APort: Integer; const AListen: Boolean = True): PtrInt;
{$IFDEF MSWINDOWS}
var LAddress: TSockAddrIn; LSize: LongInt;
{$ELSE}
var LAddress: TInetSockAddr; LSize: TSockLen;
{$ENDIF}
begin
  {$IFDEF MSWINDOWS}Result := PtrInt(WinSock2.socket(AF_INET, SOCK_STREAM, IPPROTO_TCP));
  {$ELSE}Result := fpSocket(AF_INET, SOCK_STREAM, 0);{$ENDIF}
  Check(Result <> -1, 'fixture listener allocated');
  try
    NonBlocking(Result);
    FillChar(LAddress, SizeOf(LAddress), 0);
    {$IFDEF DARWIN}LAddress.sin_len := SizeOf(LAddress);{$ENDIF}
    LAddress.sin_family := AF_INET;
    LAddress.sin_addr.s_addr := htonl($7F000001);
    {$IFDEF MSWINDOWS}
    Check(WinSock2.bind(TSocket(Result), PSockAddr(@LAddress), SizeOf(LAddress)) = 0,
      'fixture loopback bind');
    LSize := SizeOf(LAddress);
    Check(WinSock2.getsockname(TSocket(Result), PSockAddr(@LAddress)^, LSize) = 0,
      'fixture selected port');
    if AListen then Check(WinSock2.listen(TSocket(Result), 1) = 0, 'fixture listen');
    {$ELSE}
    Check(fpBind(Result, @LAddress, SizeOf(LAddress)) = 0, 'fixture loopback bind');
    LSize := SizeOf(LAddress);
    Check(fpGetSockName(Result, @LAddress, @LSize) = 0, 'fixture selected port');
    if AListen then Check(fpListen(Result, 1) = 0, 'fixture listen');
    {$ENDIF}
    APort := ntohs(LAddress.sin_port);
  except ClosePeer(Result); raise; end;
end;

function AcceptPeer(const AListener: PtrInt): PtrInt;
var LDeadline: QWord;
begin
  LDeadline := WfcBrowserTickCount64 + 2000;
  repeat
    WfcBrowserCheckDeadline(LDeadline);
    {$IFDEF MSWINDOWS}Result := PtrInt(WinSock2.accept(TSocket(AListener), nil, PLongInt(nil)));
    {$ELSE}Result := fpAccept(AListener, nil, nil);{$ENDIF}
    if Result <> -1 then Break;
    Sleep(1);
  until False;
  NonBlocking(Result);
end;

procedure PeerSend(const AHandle: PtrInt; const ABytes: RawByteString);
var LOffset, LSent: Integer; LDeadline: QWord;
begin
  LOffset := 0; LDeadline := WfcBrowserTickCount64 + 2000;
  while LOffset < Length(ABytes) do
  begin
    WfcBrowserCheckDeadline(LDeadline);
    {$IFDEF MSWINDOWS}
    LSent := WinSock2.send(TSocket(AHandle), Pointer(PByte(Pointer(ABytes)) + LOffset),
      Length(ABytes) - LOffset, 0);
    {$ELSE}
    LSent := fpSend(AHandle, PByte(Pointer(ABytes)) + LOffset,
      Length(ABytes) - LOffset, {$IFDEF LINUX}MSG_NOSIGNAL{$ELSE}0{$ENDIF});
    {$ENDIF}
    if LSent > 0 then Inc(LOffset, LSent) else Sleep(1);
  end;
end;

function PeerRead(const AHandle: PtrInt; const ACount: Integer): RawByteString;
var LOffset, LRead: Integer; LDeadline: QWord;
begin
  SetLength(Result, ACount); LOffset := 0; LDeadline := WfcBrowserTickCount64 + 2000;
  while LOffset < ACount do
  begin
    WfcBrowserCheckDeadline(LDeadline);
    {$IFDEF MSWINDOWS}
    LRead := WinSock2.recv(TSocket(AHandle), Pointer(PByte(Pointer(Result)) + LOffset),
      ACount - LOffset, 0);
    {$ELSE}
    LRead := fpRecv(AHandle, PByte(Pointer(Result)) + LOffset, ACount - LOffset, 0);
    {$ENDIF}
    if LRead = 0 then raise Exception.Create('fixture premature EOF');
    if LRead > 0 then Inc(LOffset, LRead) else Sleep(1);
  end;
end;

procedure TestArguments;
var LSocket: TWfcBrowserSocket; LPort: Integer; LRaised: Boolean;
  LBytes: array[0..65535] of Byte; LOther: array[0..31] of Byte;
begin
  for LPort in BAD_PORTS do
  begin
    LRaised := False;
    try LSocket := TWfcBrowserSocket.Create(LPort, WfcBrowserTickCount64 + 1000);
      LSocket.Free;
    except on E: EWfcBrowserSocket do LRaised := True; end;
    Check(LRaised, 'invalid port rejected');
  end;
  LRaised := False;
  try LSocket := TWfcBrowserSocket.Create(1, WfcBrowserTickCount64); LSocket.Free;
  except on E: EWfcBrowserSocket do LRaised := Pos('deadline', E.Message) > 0; end;
  Check(LRaised, 'expired connect rejected before allocation');
  LRaised := False;
  try WfcBrowserCheckDeadline(0);
  except on E: EWfcBrowserSocket do LRaised := True; end;
  Check(LRaised, 'zero deadline rejected');
  WfcBrowserCheckDeadline(WfcBrowserTickCount64 + 1000);
  FillChar(LBytes, SizeOf(LBytes), $AA);
  WfcBrowserRandomBytes(LBytes, 0);
  Check(LBytes[0] = $AA, 'zero random request does not write');
  LRaised := False;
  try WfcBrowserRandomBytes(LBytes, -1);
  except on E: EWfcBrowserSocket do LRaised := True; end;
  Check(LRaised, 'negative random request rejected');
  LRaised := False;
  try WfcBrowserRandomBytes(LBytes, 65537);
  except on E: EWfcBrowserSocket do LRaised := True; end;
  Check(LRaised, 'oversized random request rejected');
  WfcBrowserRandomBytes(LBytes, SizeOf(LBytes));
  WfcBrowserRandomBytes(LOther, SizeOf(LOther));
  Check(not CompareMem(@LBytes[0], @LOther[0], SizeOf(LOther)),
    'independent OS random requests differ (smoke check, not entropy proof)');
end;

procedure TestClock;
var LPrevious, LNow: QWord; I: Integer;
begin
  LPrevious := WfcBrowserTickCount64;
  for I := 1 to 32 do
  begin
    LNow := WfcBrowserTickCount64;
    Check(LNow >= LPrevious, 'shared clock does not move backwards');
    LPrevious := LNow;
  end;
  Sleep(20);
  Check(WfcBrowserTickCount64 > LPrevious, 'shared clock advances without page timers');
end;

procedure TestTransfer;
var LListener, LPeer: PtrInt; LPort, I: Integer; LSocket: TWfcBrowserSocket;
  LBytes, LRead, LPart: RawByteString; LRaised: Boolean; LDeadline: QWord;
begin
  LListener := NewListener(LPort); LPeer := -1; LSocket := nil;
  try
    LDeadline := WfcBrowserTickCount64 + 3000;
    LSocket := TWfcBrowserSocket.Create(LPort, LDeadline);
    Check(LSocket.Deadline = LDeadline, 'absolute deadline preserved');
    LPeer := AcceptPeer(LListener);
    for I in BAD_READS do
    begin
      LRaised := False;
      try LSocket.ReadSome(I);
      except on E: EWfcBrowserSocket do LRaised := True; end;
      Check(LRaised, 'invalid bounded read rejected');
    end;
    SetLength(LBytes, 8192);
    for I := 1 to Length(LBytes) do LBytes[I] := AnsiChar((I * 31) mod 256);
    LSocket.WriteAll('');
    LSocket.WriteAll(LBytes);
    Check(PeerRead(LPeer, Length(LBytes)) = LBytes, 'write preserves every binary byte');
    PeerSend(LPeer, LBytes);
    LRead := '';
    while Length(LRead) < Length(LBytes) do
    begin
      LPart := LSocket.ReadSome(37);
      Check((Length(LPart) >= 1) and (Length(LPart) <= 37), 'read chunk respects bound');
      LRead := LRead + LPart;
    end;
    Check(LRead = LBytes, 'partial reads preserve binary order');
    ClosePeer(LPeer);
    LRaised := False;
    try LSocket.ReadSome;
    except on E: EWfcBrowserSocket do LRaised := Pos('EOF', E.Message) > 0; end;
    Check(LRaised, 'orderly close raises EOF');
  finally LSocket.Free; ClosePeer(LPeer); ClosePeer(LListener); end;
end;

procedure TestRefused;
var LBound: PtrInt; LPort: Integer; LSocket: TWfcBrowserSocket; LRaised: Boolean;
begin
  LBound := NewListener(LPort, False);
  try
    LRaised := False;
    try LSocket := TWfcBrowserSocket.Create(LPort, WfcBrowserTickCount64 + 2000); LSocket.Free;
    except on E: EWfcBrowserSocket do
      begin WriteLn('non-listening peer: ', E.Message);
        LRaised := (Pos('connect', E.Message) > 0) or (Pos('deadline', E.Message) > 0); end;
    end;
    Check(LRaised, 'bound non-listening peer fails closed or reaches deadline');
  finally ClosePeer(LBound); end;
end;

procedure TestDisconnect;
var LListener, LPeer: PtrInt; LPort, LRead: Integer;
  LSocket: TWfcBrowserSocket; LDeadline: QWord; LByte: Byte;
  LLinger: TLinger; LRaised: Boolean;
begin
  LListener := NewListener(LPort); LPeer := -1; LSocket := nil;
  try
    LSocket := TWfcBrowserSocket.Create(LPort, WfcBrowserTickCount64 + 3000);
    LPeer := AcceptPeer(LListener);
    FreeAndNil(LSocket);
    LDeadline := WfcBrowserTickCount64 + 2000;
    repeat
      WfcBrowserCheckDeadline(LDeadline);
      {$IFDEF MSWINDOWS}LRead := WinSock2.recv(TSocket(LPeer), @LByte, 1, 0);
      {$ELSE}LRead := fpRecv(LPeer, @LByte, 1, 0);{$ENDIF}
      if LRead >= 0 then Break;
      Sleep(1);
    until False;
    Check(LRead = 0, 'destructor closes its owned socket');
    ClosePeer(LPeer);

    LSocket := TWfcBrowserSocket.Create(LPort, WfcBrowserTickCount64 + 3000);
    LPeer := AcceptPeer(LListener);
    FillChar(LLinger, SizeOf(LLinger), 0); LLinger.l_onoff := 1;
    {$IFDEF MSWINDOWS}
    Check(WinSock2.setsockopt(TSocket(LPeer), SOL_SOCKET, SO_LINGER,
      @LLinger, SizeOf(LLinger)) = 0, 'fixture reset configured');
    {$ELSE}
    Check(fpSetSockOpt(LPeer, SOL_SOCKET, SO_LINGER,
      @LLinger, SizeOf(LLinger)) = 0, 'fixture reset configured');
    {$ENDIF}
    ClosePeer(LPeer);
    LRaised := False;
    try LSocket.ReadSome;
    except on E: EWfcBrowserSocket do LRaised := Pos('socket read failed', E.Message) > 0; end;
    Check(LRaised, 'reset peer reports read failure, not successful EOF');
    LRaised := False;
    try LSocket.WriteAll('after-reset');
    except on E: EWfcBrowserSocket do LRaised := Pos('socket write failed', E.Message) > 0; end;
    Check(LRaised, 'write after reset fails without process-wide SIGPIPE');
  finally LSocket.Free; ClosePeer(LPeer); ClosePeer(LListener); end;
end;

procedure TestDeadline(const AWrite: Boolean);
var LListener, LPeer: PtrInt; LPort: Integer; LSocket: TWfcBrowserSocket;
  LBytes: RawByteString; LStart, LDeadline: QWord; LRaised: Boolean;
begin
  LListener := NewListener(LPort); LPeer := -1; LSocket := nil;
  try
    if AWrite then LBytes := StringOfChar('x', 64 * 1024 * 1024);
    LStart := WfcBrowserTickCount64; LDeadline := LStart + 250;
    LSocket := TWfcBrowserSocket.Create(LPort, LDeadline);
    LPeer := AcceptPeer(LListener);
    if not AWrite then
    begin
      PeerSend(LPeer, 'first');
      Check(LSocket.ReadSome = 'first', 'earlier read succeeds on same deadline');
    end;
    LRaised := False;
    try
      if AWrite then LSocket.WriteAll(LBytes) else LSocket.ReadSome;
    except on E: EWfcBrowserSocket do LRaised := Pos('deadline', E.Message) > 0; end;
    Check(LRaised, 'stalled operation reaches absolute deadline');
    Check(WfcBrowserTickCount64 >= LDeadline, 'deadline did not expire early');
    Check(WfcBrowserTickCount64 - LStart < 2000, 'deadline remains bounded in real time');
    LRaised := False;
    try LSocket.WriteAll('');
    except on E: EWfcBrowserSocket do LRaised := Pos('deadline', E.Message) > 0; end;
    Check(LRaised, 'empty write cannot refresh expired deadline');
    Check(LSocket.Deadline = LDeadline, 'I/O did not extend deadline');
  finally LSocket.Free; ClosePeer(LPeer); ClosePeer(LListener); end;
end;

procedure Run;
{$IFDEF MSWINDOWS}var LData: TWSAData;{$ENDIF}
begin
  {$IFDEF MSWINDOWS}Check(WSAStartup($0202, LData) = 0, 'fixture Winsock startup');{$ENDIF}
  try
    TestArguments;
    TestClock;
    TestTransfer;
    TestRefused;
    TestDisconnect;
    TestDeadline(False);
    TestDeadline(True);
    WriteLn('browser socket tests passed: ', Checks, ' checks');
  finally {$IFDEF MSWINDOWS}WSACleanup;{$ENDIF} end;
end;

begin
  try Run;
  except on E: Exception do begin WriteLn(StdErr, E.Message); Halt(1); end; end;
end.
