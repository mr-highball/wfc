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
unit wfc_browser_socket;

{$mode delphi}{$H+}
{$IFDEF PAS2JS}{$FATAL native browser transport requires FPC}{$ENDIF}

interface

uses SysUtils;

type
  EWfcBrowserSocket = class(Exception);
  { One owned, unbuffered TCP connection to literal IPv4 loopback. The absolute
    monotonic deadline is shared by connect and every subsequent operation. }
  TWfcBrowserSocket = class
  private
    FSocket: PtrInt;
    FDeadline: QWord;
    {$IFDEF MSWINDOWS}FStarted: Boolean;{$ENDIF}
    procedure WaitReady(const AWrite: Boolean);
  public
    constructor Create(const APort: Integer; const ADeadline: QWord);
    destructor Destroy; override;
    procedure WriteAll(const ABytes: RawByteString);
    { Exactly 1..AMaxBytes bytes; orderly EOF is an exception, never ''. }
    function ReadSome(const AMaxBytes: Integer = 65536): RawByteString;
    property Deadline: QWord read FDeadline;
  end;

{ Use this clock to create deadlines: stable FPC's Darwin GetTickCount64 uses
  wall time. This helper fails closed if a monotonic OS clock is unavailable. }
function WfcBrowserTickCount64: QWord;
procedure WfcBrowserCheckDeadline(const ADeadline: QWord);
{ OS cryptographic randomness only. Zero is a no-op; 1..65536 bytes supported.
  Failure never falls back to the generator used by WFC compositions. }
procedure WfcBrowserRandomBytes(var ABuffer; const ACount: Integer);

implementation

uses
  {$IFDEF MSWINDOWS}Windows, WinSock2{$ELSE}Sockets, BaseUnix, ctypes{$ENDIF};

{$IFNDEF MSWINDOWS}
  {$IFNDEF LINUX}
    {$IFNDEF DARWIN}{$FATAL browser sockets support Windows, Linux and macOS}{$ENDIF}
  {$ENDIF}
const
  { Stable/current FPC BaseUnix omit FD_CLOEXEC on Linux. This descriptor flag
    is 1 on both supported Unix hosts; keep it project-scoped, not an RTL alias.
    OS ABI facts only:
    https://github.com/torvalds/linux/blob/master/include/uapi/asm-generic/fcntl.h
    https://github.com/apple-oss-distributions/xnu/blob/main/bsd/sys/fcntl.h }
  BROWSER_FD_CLOEXEC = 1;
{$ENDIF}

{$IFDEF MSWINDOWS}
function BrowserBCryptGenRandom(AAlgorithm: Pointer; ABuffer: Pointer;
  ACount, AFlags: Cardinal): LongInt; stdcall;
  external 'bcrypt.dll' name 'BCryptGenRandom';
{$ENDIF}
{$IFDEF LINUX}
function BrowserGetRandom(ABuffer: Pointer; ACount: SizeUInt;
  AFlags: Cardinal): PtrInt; cdecl; external 'c' name 'getrandom';
function BrowserLibcErrNo: PcInt; cdecl; external 'c' name '__errno_location';
{$ENDIF}
{$IFDEF DARWIN}
function BrowserGetEntropy(ABuffer: Pointer; ACount: SizeUInt): LongInt;
  cdecl; external 'c' name 'getentropy';
function BrowserLibcErrNo: PcInt; cdecl; external 'c' name '__error';
{$ENDIF}
{$IFNDEF MSWINDOWS}
function BrowserClockGetTime(AClock: cInt; AValue: PTimeSpec): cInt;
  cdecl; external 'c' name 'clock_gettime';
{$ENDIF}

function WfcBrowserTickCount64: QWord;
{$IFNDEF MSWINDOWS}var LTime: TTimeSpec; LFraction: QWord;{$ENDIF}
begin
  {$IFDEF MSWINDOWS}
  Result := SysUtils.GetTickCount64;
  {$ELSE}
  { CLOCK_MONOTONIC: Linux=1, Darwin=6 (macOS 10.12+). OS ABI facts only:
    https://github.com/apple-oss-distributions/Libc/blob/main/include/_time.h
    https://man7.org/linux/man-pages/man3/clock_gettime.3.html }
  if BrowserClockGetTime({$IFDEF DARWIN}6{$ELSE}1{$ENDIF}, @LTime) <> 0 then
    raise EWfcBrowserSocket.Create('monotonic OS clock failed (error ' +
      IntToStr(BrowserLibcErrNo^) + ')');
  if (LTime.tv_sec < 0) or (LTime.tv_nsec < 0) or (LTime.tv_nsec >= 1000000000) then
    raise EWfcBrowserSocket.Create('monotonic OS clock returned invalid time');
  LFraction := QWord(LTime.tv_nsec) div 1000000;
  if QWord(LTime.tv_sec) > (High(QWord) - LFraction) div 1000 then
    raise EWfcBrowserSocket.Create('monotonic OS clock exceeds millisecond range');
  Result := QWord(LTime.tv_sec) * 1000 + LFraction;
  {$ENDIF}
end;

procedure WfcBrowserCheckDeadline(const ADeadline: QWord);
begin
  if WfcBrowserTickCount64 >= ADeadline then
    raise EWfcBrowserSocket.Create('browser socket deadline expired');
end;

function LastSocketError: Integer;
begin
  {$IFDEF MSWINDOWS}Result := WSAGetLastError;{$ELSE}Result := fpGetErrNo;{$ENDIF}
end;

procedure SocketFailure(const AOperation: String; const AError: Integer);
begin
  raise EWfcBrowserSocket.Create(AOperation + ' failed (socket error ' +
    IntToStr(AError) + ')');
end;

function Interrupted(const AError: Integer): Boolean;
begin
  Result := AError = {$IFDEF MSWINDOWS}WSAEINTR{$ELSE}ESysEINTR{$ENDIF};
end;

function WouldBlock(const AError: Integer): Boolean;
begin
  {$IFDEF MSWINDOWS}
  Result := AError = WSAEWOULDBLOCK;
  {$ELSE}
  Result := (AError = ESysEAGAIN) or (AError = ESysEWOULDBLOCK);
  {$ENDIF}
end;

{$IFNDEF MSWINDOWS}
function ConfigureSocket(const AHandle, ACommand, AValue: Integer;
  const ADeadline: QWord): Integer;
var LError: Integer;
begin
  repeat
    WfcBrowserCheckDeadline(ADeadline);
    Result := fpFcntl(AHandle, ACommand, AValue);
    if Result >= 0 then Exit;
    LError := fpGetErrNo;
    if not Interrupted(LError) then SocketFailure('socket configuration', LError);
  until False;
end;
{$ENDIF}

procedure WfcBrowserRandomBytes(var ABuffer; const ACount: Integer);
{$IFDEF MSWINDOWS}
var LStatus: LongInt;
{$ELSE}
var LOffset, LCount, LRead, LError: Integer; LDeadline: QWord;
{$ENDIF}
begin
  if (ACount < 0) or (ACount > 65536) then
    raise EWfcBrowserSocket.Create('random byte count must be in 0..65536');
  if ACount = 0 then Exit;
  {$IFDEF MSWINDOWS}
  { System-preferred CNG generator; no provider handle or weak fallback.
    https://learn.microsoft.com/windows/win32/api/bcrypt/nf-bcrypt-bcryptgenrandom }
  LStatus := BrowserBCryptGenRandom(nil, @ABuffer, ACount, 2);
  if LStatus <> 0 then
    raise EWfcBrowserSocket.Create('OS random generator failed (status ' +
      IntToStr(LStatus) + ')');
  {$ELSE}
  LOffset := 0;
  LDeadline := WfcBrowserTickCount64 + 1000;
  while LOffset < ACount do
  begin
    WfcBrowserCheckDeadline(LDeadline);
    LCount := ACount - LOffset;
    if LCount > 256 then LCount := 256;
    {$IFDEF LINUX}
    { GRND_NONBLOCK fails closed while the kernel entropy pool is unready.
      https://man7.org/linux/man-pages/man2/getrandom.2.html }
    LRead := BrowserGetRandom(PByte(@ABuffer) + LOffset, LCount, 1);
    {$ELSE}
    { Darwin getentropy accepts at most 256 bytes per call (macOS 10.12+).
      https://github.com/apple-oss-distributions/xnu/blob/main/bsd/man/man2/getentropy.2 }
    LRead := BrowserGetEntropy(PByte(@ABuffer) + LOffset, LCount);
    if LRead = 0 then LRead := LCount;
    {$ENDIF}
    if LRead < 0 then
    begin
      { These two random APIs call libc, not FPC's Linux syscall wrappers. }
      LError := BrowserLibcErrNo^;
      if Interrupted(LError) then Continue;
      SocketFailure('OS random generator', LError);
    end;
    if LRead = 0 then
      raise EWfcBrowserSocket.Create('OS random generator returned no bytes');
    Inc(LOffset, LRead);
  end;
  {$ENDIF}
end;

procedure TWfcBrowserSocket.WaitReady(const AWrite: Boolean);
var
  LResult, LError, LMilliseconds: Integer;
  LRemaining, LNow: QWord;
  {$IFDEF MSWINDOWS}
  LSet, LExcept: TFDSet;
  LTimeout: TTimeVal;
  {$ELSE}
  LPoll: TPollFD;
  {$ENDIF}
begin
  repeat
    WfcBrowserCheckDeadline(FDeadline);
    LNow := WfcBrowserTickCount64;
    if LNow >= FDeadline then WfcBrowserCheckDeadline(FDeadline);
    LRemaining := FDeadline - LNow;
    if LRemaining > 1000 then LMilliseconds := 1000
    else LMilliseconds := Integer(LRemaining);
    {$IFDEF MSWINDOWS}
    FD_ZERO(LSet); FD_SET(TSocket(FSocket), LSet);
    FD_ZERO(LExcept); FD_SET(TSocket(FSocket), LExcept);
    LTimeout.tv_sec := LMilliseconds div 1000;
    LTimeout.tv_usec := (LMilliseconds mod 1000) * 1000;
    { Failed nonblocking connects appear in exceptfds on Windows.
      https://learn.microsoft.com/windows/win32/api/winsock2/nf-winsock2-select }
    if AWrite then
      LResult := WinSock2.select(0, nil, @LSet, @LExcept, @LTimeout)
    else
      LResult := WinSock2.select(0, @LSet, nil, @LExcept, @LTimeout);
    {$ELSE}
    FillChar(LPoll, SizeOf(LPoll), 0);
    LPoll.fd := FSocket;
    if AWrite then LPoll.events := POLLOUT else LPoll.events := POLLIN;
    LResult := fpPoll(@LPoll, 1, LMilliseconds);
    if (LResult > 0) and ((LPoll.revents and POLLNVAL) <> 0) then
      raise EWfcBrowserSocket.Create('browser socket descriptor is invalid');
    {$ENDIF}
    if LResult < 0 then
    begin
      LError := LastSocketError;
      if Interrupted(LError) then Continue;
      SocketFailure('socket readiness', LError);
    end;
    WfcBrowserCheckDeadline(FDeadline);
    if LResult > 0 then Exit;
  until False;
end;

constructor TWfcBrowserSocket.Create(const APort: Integer;
  const ADeadline: QWord);
var
  LResult, LError: Integer;
  {$IFDEF MSWINDOWS}
  LAddress: TSockAddrIn;
  LData: TWSAData;
  LNonBlock: u_long;
  LSize: LongInt;
  {$ELSE}
  LAddress: TInetSockAddr;
  LSize: TSockLen;
  LFlags: Integer;
  {$IFDEF DARWIN}LNoSigPipe: LongInt;{$ENDIF}
  {$ENDIF}
begin
  inherited Create;
  FSocket := -1;
  FDeadline := ADeadline;
  if (APort < 1) or (APort > 65535) then
    raise EWfcBrowserSocket.Create('loopback port must be in 1..65535');
  WfcBrowserCheckDeadline(FDeadline);
  {$IFDEF MSWINDOWS}
  LResult := WSAStartup($0202, LData);
  if LResult <> 0 then SocketFailure('Winsock startup', LResult);
  FStarted := True;
  FSocket := PtrInt(WinSock2.socket(AF_INET, SOCK_STREAM, IPPROTO_TCP));
  {$ELSE}
  FSocket := fpSocket(AF_INET, SOCK_STREAM, 0);
  {$ENDIF}
  if FSocket = -1 then SocketFailure('socket creation', LastSocketError);
  {$IFDEF MSWINDOWS}
  if not SetHandleInformation(THandle(FSocket), HANDLE_FLAG_INHERIT, 0) then
    SocketFailure('socket inheritance configuration', GetLastError);
  LNonBlock := 1;
  if ioctlsocket(TSocket(FSocket), LongInt(FIONBIO), LNonBlock) <> 0 then
    SocketFailure('nonblocking socket configuration', LastSocketError);
  {$ELSE}
  LFlags := ConfigureSocket(FSocket, F_GETFL, 0, FDeadline);
  ConfigureSocket(FSocket, F_SETFL, LFlags or O_NONBLOCK, FDeadline);
  LFlags := ConfigureSocket(FSocket, F_GETFD, 0, FDeadline);
  ConfigureSocket(FSocket, F_SETFD, LFlags or BROWSER_FD_CLOEXEC, FDeadline);
  {$IFDEF DARWIN}
  { Socket-local SIGPIPE protection, never a process-wide signal change.
    https://developer.apple.com/library/archive/documentation/System/Conceptual/ManPages_iPhoneOS/man2/setsockopt.2.html }
  LNoSigPipe := 1;
  if fpSetSockOpt(FSocket, SOL_SOCKET, SO_NOSIGPIPE,
      @LNoSigPipe, SizeOf(LNoSigPipe)) <> 0 then
    SocketFailure('socket SIGPIPE protection', LastSocketError);
  {$ENDIF}
  {$ENDIF}
  FillChar(LAddress, SizeOf(LAddress), 0);
  {$IFDEF DARWIN}LAddress.sin_len := SizeOf(LAddress);{$ENDIF}
  LAddress.sin_family := AF_INET;
  LAddress.sin_port := htons(APort);
  {$IFDEF MSWINDOWS}
  LAddress.sin_addr.s_addr := htonl($7F000001);
  LResult := WinSock2.connect(TSocket(FSocket), PSockAddr(@LAddress), SizeOf(LAddress));
  {$ELSE}
  LAddress.sin_addr.s_addr := htonl($7F000001);
  LResult := fpConnect(FSocket, @LAddress, SizeOf(LAddress));
  {$ENDIF}
  if LResult <> 0 then
  begin
    LError := LastSocketError;
    {$IFDEF MSWINDOWS}
    if not (WouldBlock(LError) or (LError = WSAEALREADY) or Interrupted(LError)) then
    {$ELSE}
    if not ((LError = ESysEINPROGRESS) or (LError = ESysEALREADY) or Interrupted(LError)) then
    {$ENDIF}
      SocketFailure('loopback connect', LError);
    WaitReady(True);
    repeat
      WfcBrowserCheckDeadline(FDeadline);
      LError := 0;
      LSize := SizeOf(LError);
      {$IFDEF MSWINDOWS}
      LResult := getsockopt(TSocket(FSocket), SOL_SOCKET, SO_ERROR, @LError, LSize);
      {$ELSE}
      LResult := fpGetSockOpt(FSocket, SOL_SOCKET, SO_ERROR, @LError, @LSize);
      {$ENDIF}
      if LResult = 0 then Break;
      LError := LastSocketError;
      if not Interrupted(LError) then SocketFailure('connect status', LError);
    until False;
    if LError <> 0 then SocketFailure('loopback connect', LError);
  end;
  WfcBrowserCheckDeadline(FDeadline);
end;

destructor TWfcBrowserSocket.Destroy;
begin
  if FSocket <> -1 then
  begin
    {$IFDEF MSWINDOWS}WinSock2.closesocket(TSocket(FSocket));
    {$ELSE}fpClose(FSocket);{$ENDIF}
    FSocket := -1;
  end;
  {$IFDEF MSWINDOWS}if FStarted then WSACleanup;{$ENDIF}
  inherited Destroy;
end;

procedure TWfcBrowserSocket.WriteAll(const ABytes: RawByteString);
var LOffset: SizeInt; LCount, LSent, LError: Integer;
begin
  WfcBrowserCheckDeadline(FDeadline);
  LOffset := 0;
  while LOffset < Length(ABytes) do
  begin
    WfcBrowserCheckDeadline(FDeadline);
    if Length(ABytes) - LOffset > 65536 then LCount := 65536
    else LCount := Length(ABytes) - LOffset;
    {$IFDEF MSWINDOWS}
    LSent := WinSock2.send(TSocket(FSocket), Pointer(PByte(Pointer(ABytes)) + LOffset), LCount, 0);
    {$ELSE}
    LSent := fpSend(FSocket, PByte(Pointer(ABytes)) + LOffset, LCount,
      {$IFDEF LINUX}MSG_NOSIGNAL{$ELSE}0{$ENDIF});
    {$ENDIF}
    if LSent < 0 then
    begin
      LError := LastSocketError;
      if Interrupted(LError) then Continue;
      if WouldBlock(LError) then begin WaitReady(True); Continue; end;
      SocketFailure('socket write', LError);
    end;
    if LSent = 0 then raise EWfcBrowserSocket.Create('socket write made no progress');
    Inc(LOffset, LSent);
  end;
  WfcBrowserCheckDeadline(FDeadline);
end;

function TWfcBrowserSocket.ReadSome(const AMaxBytes: Integer): RawByteString;
var LRead, LError: Integer;
begin
  if (AMaxBytes < 1) or (AMaxBytes > 65536) then
    raise EWfcBrowserSocket.Create('socket read size must be in 1..65536');
  SetLength(Result, AMaxBytes);
  repeat
    WfcBrowserCheckDeadline(FDeadline);
    {$IFDEF MSWINDOWS}
    LRead := WinSock2.recv(TSocket(FSocket), Pointer(Result), AMaxBytes, 0);
    {$ELSE}
    LRead := fpRecv(FSocket, Pointer(Result), AMaxBytes, 0);
    {$ENDIF}
    if LRead < 0 then
    begin
      LError := LastSocketError;
      if Interrupted(LError) then Continue;
      if WouldBlock(LError) then begin WaitReady(False); Continue; end;
      SocketFailure('socket read', LError);
    end;
    if LRead = 0 then raise EWfcBrowserSocket.Create('browser socket reached EOF');
    WfcBrowserCheckDeadline(FDeadline);
    SetLength(Result, LRead);
    Exit;
  until False;
end;

end.
