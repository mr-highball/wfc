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
program wfc_browser_websocket_test;

{$mode delphi}{$H+}

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  Classes, SysUtils, SHA1, Base64, wfc_browser_socket, wfc_browser_websocket,
  {$IFDEF MSWINDOWS}Windows, WinSock2{$ELSE}Sockets, BaseUnix{$ENDIF};

type
  TExpectedFrame = record Opcode: Byte; Payload: RawByteString; end;
  TExpectedFrames = array of TExpectedFrame;
  TPeer = class(TThread)
  private
    FListener, FClient: PtrInt;
    FPort: Integer;
    FDeadline: QWord;
    FInput, FWire, FRequest, FError: RawByteString;
    FHeaderMode: Integer;
    FExpected: TExpectedFrames;
    FReceivedFrames: Integer;
    FSplit, FStarted: Boolean;
    procedure CheckTime;
    procedure SendBytes(const S: RawByteString);
    function ReadBytes(const Count: Integer): RawByteString;
    function ReadClientFrame: TExpectedFrame;
  protected
    procedure Execute; override;
  public
    constructor Create(const AWire: RawByteString; const AHeaderMode: Integer;
      const AExpected: array of TExpectedFrame; const ASplit: Boolean = False);
    destructor Destroy; override;
    procedure Join;
    property Port: Integer read FPort;
    property Request: RawByteString read FRequest;
    property Error: RawByteString read FError;
    property ReceivedFrames: Integer read FReceivedFrames;
  end;

var Checks: Integer;

procedure Check(const Value: Boolean; const Message: String);
begin Inc(Checks);if not Value then raise Exception.Create(Message);end;

function SameBytes(const A, B: RawByteString): Boolean;
begin
  Result := Length(A) = Length(B);
  if Result and (Length(A) > 0) then Result := CompareMem(@A[1], @B[1], Length(A));
end;

procedure CloseOwnedSocket(var S: PtrInt);
begin
  if S <> -1 then
  begin
    {$IFDEF MSWINDOWS}closesocket(TSocket(S));{$ELSE}CloseSocket(S);{$ENDIF}
    S := -1;
  end;
end;

procedure NonBlocking(const S: PtrInt);
{$IFDEF MSWINDOWS}var Mode: u_long;{$ELSE}var Flags: Integer;{$ENDIF}
begin
  {$IFDEF MSWINDOWS}
  Mode := 1;
  if ioctlsocket(TSocket(S), LongInt(FIONBIO), @Mode) <> 0 then raise Exception.Create('mock nonblocking socket failed');
  {$ELSE}
  Flags := fpFcntl(S, F_GETFL, 0);
  if (Flags < 0) or (fpFcntl(S, F_SETFL, Flags or O_NONBLOCK) < 0) then
    raise Exception.Create('mock nonblocking socket failed');
  {$ENDIF}
end;

function RetrySocket: Boolean;
var E: Integer;
begin
  {$IFDEF MSWINDOWS}
  E := WSAGetLastError; Result := (E = WSAEWOULDBLOCK) or (E = WSAEINTR);
  {$ELSE}
  E := fpGetErrNo; Result := (E = ESysEWOULDBLOCK) or (E = ESysEAGAIN) or (E = ESysEINTR);
  {$ENDIF}
end;

constructor TPeer.Create(const AWire: RawByteString; const AHeaderMode: Integer;
  const AExpected: array of TExpectedFrame; const ASplit: Boolean);
var Address: {$IFDEF MSWINDOWS}sockaddr_in{$ELSE}TInetSockAddr{$ENDIF};
  Size, I: Integer;
begin
  FListener := -1; FClient := -1;
  inherited Create(True);
  FWire := AWire; FHeaderMode := AHeaderMode; FSplit := ASplit;
  FDeadline := WfcBrowserTickCount64 + 4000;
  SetLength(FExpected, Length(AExpected));
  for I := 0 to High(AExpected) do FExpected[I] := AExpected[I];
  {$IFDEF MSWINDOWS}FListener := PtrInt(socket(AF_INET, SOCK_STREAM, 0));
  {$ELSE}FListener := fpSocket(AF_INET, SOCK_STREAM, 0);{$ENDIF}
  if FListener = -1 then raise Exception.Create('mock socket creation failed');
  NonBlocking(FListener);
  FillChar(Address, SizeOf(Address), 0);
  Address.sin_family := AF_INET;
  {$IFDEF MSWINDOWS}
  Address.sin_addr.S_addr := htonl($7F000001);
  if bind(TSocket(FListener), Address, SizeOf(Address)) <> 0 then raise Exception.Create('mock bind failed');
  if listen(TSocket(FListener), 1) <> 0 then raise Exception.Create('mock listen failed');
  Size := SizeOf(Address);
  if getsockname(TSocket(FListener), Address, Size) <> 0 then raise Exception.Create('mock getsockname failed');
  FPort := ntohs(Address.sin_port);
  {$ELSE}
  Address.sin_addr.s_addr := htonl($7F000001);
  if fpBind(FListener, @Address, SizeOf(Address)) <> 0 then raise Exception.Create('mock bind failed');
  if fpListen(FListener, 1) <> 0 then raise Exception.Create('mock listen failed');
  Size := SizeOf(Address);
  if fpGetSockName(FListener, @Address, @Size) <> 0 then raise Exception.Create('mock getsockname failed');
  FPort := ntohs(Address.sin_port);
  {$ENDIF}
  Start; FStarted := True;
end;

destructor TPeer.Destroy;
begin
  Terminate;
  if not FStarted then Start;
  Join;
  CloseOwnedSocket(FListener);
  inherited Destroy;
end;

procedure TPeer.Join;
var Deadline: QWord;
begin
  Deadline := WfcBrowserTickCount64 + 5000;
  while not Finished do
  begin
    if WfcBrowserTickCount64 >= Deadline then raise Exception.Create('bounded mock peer did not finish');
    Sleep(1);
  end;
  WaitFor;
end;

procedure TPeer.CheckTime;
begin
  if Terminated or (WfcBrowserTickCount64 >= FDeadline) then raise Exception.Create('mock peer deadline');
end;

procedure TPeer.SendBytes(const S: RawByteString);
var Offset, Count, Sent: Integer;
begin
  Offset := 0;
  while Offset < Length(S) do
  begin
    CheckTime;
    Count := Length(S) - Offset;
    if FSplit then Count := 1;
    {$IFDEF MSWINDOWS}
    Sent := send(TSocket(FClient), S[Offset + 1], Count, 0);
    {$ELSE}
    Sent := fpSend(FClient, @S[Offset + 1], Count, {$IFDEF LINUX}MSG_NOSIGNAL{$ELSE}0{$ENDIF});
    {$ENDIF}
    if Sent < 0 then
    begin
      if not RetrySocket then raise Exception.Create('mock send failed');
      Sleep(1);Continue;
    end;
    if Sent = 0 then raise Exception.Create('mock send EOF');
    Inc(Offset, Sent);
  end;
end;

function TPeer.ReadBytes(const Count: Integer): RawByteString;
var Buffer: array[0..4095] of AnsiChar; Received: Integer; Part: RawByteString;
begin
  while Length(FInput) < Count do
  begin
    CheckTime;
    {$IFDEF MSWINDOWS}
    Received := recv(TSocket(FClient), Buffer, SizeOf(Buffer), 0);
    {$ELSE}
    Received := fpRecv(FClient, @Buffer[0], SizeOf(Buffer), 0);
    {$ENDIF}
    if Received < 0 then
    begin
      if not RetrySocket then raise Exception.Create('mock receive failed');
      Sleep(1);Continue;
    end;
    if Received = 0 then raise Exception.Create('mock receive EOF');
    SetString(Part, PAnsiChar(@Buffer[0]), Received); FInput := FInput + Part;
  end;
  Result := Copy(FInput, 1, Count); Delete(FInput, 1, Count);
end;

function TPeer.ReadClientFrame: TExpectedFrame;
var Header, Mask: RawByteString; Count: QWord; I: Integer;
begin
  Header := ReadBytes(2);
  if (Ord(Header[1]) and $F0) <> $80 then raise Exception.Create('client frame bits');
  if (Ord(Header[2]) and $80) = 0 then raise Exception.Create('client frame not masked');
  Result.Opcode := Ord(Header[1]) and $0F;
  Count := Ord(Header[2]) and $7F;
  if Count = 126 then
  begin Header := ReadBytes(2); Count := Ord(Header[1]) * 256 + Ord(Header[2]);
    if Count < 126 then raise Exception.Create('noncanonical client16 length'); end
  else if Count = 127 then
  begin
    Header := ReadBytes(8); Count := 0;
    for I := 1 to 8 do Count := (Count shl 8) or Ord(Header[I]);
    if Count < 65536 then raise Exception.Create('noncanonical client64 length');
  end;
  if Count > 200000 then raise Exception.Create('mock client frame limit');
  Mask := ReadBytes(4); Result.Payload := ReadBytes(Integer(Count));
  for I := 1 to Length(Result.Payload) do
    Result.Payload[I] := AnsiChar(Ord(Result.Payload[I]) xor Ord(Mask[(I - 1) mod 4 + 1]));
end;

procedure TPeer.Execute;
var P, E, I: Integer; Key, AcceptValue, Response, RawDigest: RawByteString;
  Digest: TSHA1Digest; Actual: TExpectedFrame;
  {$IFDEF DARWIN}NoSigPipe: Integer;{$ENDIF}
begin
  try
    repeat
      CheckTime;
      {$IFDEF MSWINDOWS}FClient := PtrInt(accept(TSocket(FListener), nil, nil));
      {$ELSE}FClient := fpAccept(FListener, nil, nil);{$ENDIF}
      if FClient <> -1 then Break;
      if not RetrySocket then raise Exception.Create('mock accept failed');
      Sleep(1);
    until False;
    NonBlocking(FClient);
    {$IFDEF DARWIN}
    NoSigPipe := 1;
    if fpSetSockOpt(FClient, SOL_SOCKET, SO_NOSIGPIPE, @NoSigPipe, SizeOf(NoSigPipe)) <> 0 then
      raise Exception.Create('mock SIGPIPE protection failed');
    {$ENDIF}
    while Pos(#13#10#13#10, FRequest) = 0 do
    begin
      FRequest := FRequest + ReadBytes(1);
      if Length(FRequest) > 16384 then raise Exception.Create('mock request limit');
    end;
    P := Pos('Sec-WebSocket-Key: ', FRequest);
    if P = 0 then raise Exception.Create('missing client nonce');
    Inc(P, Length('Sec-WebSocket-Key: '));
    E := Pos(#13#10, Copy(FRequest, P, MaxInt));
    Key := Copy(FRequest, P, E - 1);
    if Length(DecodeStringBase64(Key, True)) <> 16 then raise Exception.Create('client nonce must be16 bytes');
    Digest := SHA1String(Key + '258EAFA5-E914-47DA-95CA-C5AB0DC85B11');
    SetString(RawDigest, PAnsiChar(@Digest[0]), SizeOf(Digest));
    AcceptValue := EncodeStringBase64(RawDigest);
    Response := 'HTTP/1.1 101 Switching Protocols'#13#10 +
      'uPgRaDe: WebSocket'#13#10'Connection: keep-alive, Upgrade'#13#10 +
      'Sec-WebSocket-Accept: ' + AcceptValue + #13#10;
    case FHeaderMode of
      1: Response := StringReplace(Response, AcceptValue, 'wrong', []);
      2: Response := StringReplace(Response, '101 Switching Protocols', '302 Found', []);
      3: Response := Response + 'Sec-WebSocket-Accept: ' + AcceptValue + #13#10;
      4: Response := StringReplace(Response, 'uPgRaDe: WebSocket'#13#10, '', []);
      5: Response := StringReplace(Response, 'keep-alive, Upgrade', 'notupgrade', []);
      6: Response := Response + 'Sec-WebSocket-Extensions: permessage-deflate'#13#10;
      7: Response := Response + 'Sec-WebSocket-Protocol: invented'#13#10;
      8: Response := Response + ' folded: value'#13#10;
      9: Response := Response + 'X-Unsafe: '#0#13#10;
      10: Response := Response + 'X-Huge: ' + StringOfChar('x', 17000) + #13#10;
      11: Response := StringReplace(Response, 'Sec-WebSocket-Accept: ' + AcceptValue + #13#10, '', []);
      12: Response := StringReplace(Response, 'HTTP/1.1', 'HTTP/1.0', []);
      13: Response := Response + 'Upgrade: websocket'#13#10;
      14: Response := Response + 'Content-Length: 3'#13#10;
      15: Response := Response + 'Transfer-Encoding: chunked'#13#10;
      16: Response := StringReplace(Response, 'keep-alive, Upgrade', 'Upgrade,', []);
      17: Response := StringReplace(Response, 'keep-alive, Upgrade', 'keep-alive', []) + 'Connection: Upgrade'#13#10;
      18: Response := StringReplace(Response, 'keep-alive, Upgrade', '', []) + 'Connection: Upgrade'#13#10;
      19: Response := StringReplace(Response, '101 Switching Protocols', '101', []);
    end;
    SendBytes(Response + #13#10 + FWire);
    if FHeaderMode = 20 then
      repeat CheckTime;Sleep(1);until False;
    for I := 0 to High(FExpected) do
    begin
      Actual := ReadClientFrame;
      if (Actual.Opcode <> FExpected[I].Opcode) or not SameBytes(Actual.Payload, FExpected[I].Payload) then
        raise Exception.Create('client frame differs from expectation');
      Inc(FReceivedFrames);
    end;
  except on X: Exception do FError := X.Message;end;
  CloseOwnedSocket(FClient);
end;

function Frame(const Opcode: Byte; const Payload: RawByteString;
  const FinalFrame: Boolean = True): RawByteString;
var Count: QWord; I: Integer;
begin
  Count := Length(Payload);
  if FinalFrame then Result := AnsiChar($80 or Opcode) else Result := AnsiChar(Opcode);
  if Count < 126 then Result := Result + AnsiChar(Count)
  else if Count <= 65535 then Result := Result + #126 + AnsiChar(Count shr 8) + AnsiChar(Count and $FF)
  else begin Result := Result + #127;for I := 7 downto 0 do Result := Result + AnsiChar((Count shr (I*8)) and $FF);end;
  Result := Result + Payload;
end;

procedure ReceiveCase(const Wire, Expected: RawByteString; const Limit: Integer = 200000;
  const HeaderMode: Integer = 0; const Split: Boolean = False);
var Peer: TPeer; Client: TWfcBrowserWebSocket; Actual: UTF8String;
begin
  Peer := TPeer.Create(Wire, HeaderMode, [], Split);Client := nil;
  try
    Client := TWfcBrowserWebSocket.Create(Peer.Port, '/devtools/browser/test-id', WfcBrowserTickCount64 + 3000, Limit);
    Actual := Client.ReceiveText;
    Check(SameBytes(RawByteString(Actual), Expected), 'received exact UTF8 bytes (check ' +
      IntToStr(Checks + 1) + ', expected length ' + IntToStr(Length(Expected)) +
      ', actual length ' + IntToStr(Length(Actual)) + ')');
    Peer.Join;Check(Peer.Error = '', 'mock peer succeeded: ' + Peer.Error);
    Check(Pos('Host: 127.0.0.1:' + IntToStr(Peer.Port), Peer.Request)>0, 'loopback Host');
    Check(Pos('Origin:', Peer.Request)=0, 'native client needs no Origin wildcard');
    Check(Pos('Authorization:', Peer.Request)=0, 'no authentication header');
  finally Client.Free;Peer.Free;end;
end;

procedure RejectCase(const Wire, ExpectedError: RawByteString; const HeaderMode: Integer = 0;
  const Limit: Integer = 200000);
var Peer: TPeer; Client: TWfcBrowserWebSocket; Failure: String;
begin
  Peer := TPeer.Create(Wire, HeaderMode, []);Client := nil;Failure := '';
  try
    try
      Client := TWfcBrowserWebSocket.Create(Peer.Port, '/devtools/page/test-id', WfcBrowserTickCount64 + 3000, Limit);
      Client.ReceiveText;
    except on X: EWfcBrowserWebSocket do Failure := X.Message;end;
    Check(Failure <> '', 'malformed peer must raise WebSocket exception');
    Check((ExpectedError = '') or (Pos(ExpectedError, Failure)>0),
      'expected ' + ExpectedError + ', received ' + Failure);
    if Client <> nil then
    begin
      Failure := '';
      try Client.SendText('later');except on X: EWfcBrowserWebSocket do Failure := X.Message;end;
      Check(Pos('closed', Failure)>0, 'protocol failure closes transport');
    end;
  finally Client.Free;Peer.Free;end;
end;

procedure BadArguments(const Port, Limit: Integer; const Path: String);
var Client: TWfcBrowserWebSocket; Failure: String;
begin
  Client := nil;Failure := '';
  try
    try Client := TWfcBrowserWebSocket.Create(Port, Path, WfcBrowserTickCount64+1000, Limit);
    except on X: EWfcBrowserWebSocket do Failure := X.Message;end;
    Check((Failure<>'') and(Pos('transport:', Failure)=0), 'bad argument rejected before socket connect');
  finally Client.Free;end;
end;

procedure SendCases;
var Peer: TPeer; Client: TWfcBrowserWebSocket; Expected: array[0..5] of TExpectedFrame;
  I: Integer; Text: UTF8String;
begin
  Expected[0].Opcode:=1;Expected[0].Payload:='';
  Expected[1].Opcode:=1;Expected[1].Payload:=StringOfChar('a',125);
  Expected[2].Opcode:=1;Expected[2].Payload:=StringOfChar('b',126);
  Expected[3].Opcode:=1;Expected[3].Payload:=StringOfChar('c',65536);
  Expected[4].Opcode:=1;Expected[4].Payload:=StringOfChar('d',65535);
  Expected[5].Opcode:=1;Expected[5].Payload:=#$E2#$82#$AC+#$F0#$9F#$8E#$B5;
  Peer:=TPeer.Create(Frame(1,'ack'),0,Expected);Client:=nil;
  try
    Client:=TWfcBrowserWebSocket.Create(Peer.Port,'/devtools/page/test-id',WfcBrowserTickCount64+3000,100000);
    for I:=0 to High(Expected) do
    begin
      SetLength(Text,Length(Expected[I].Payload));
      if Text<>'' then Move(Expected[I].Payload[1],Text[1],Length(Text));
      Client.SendText(Text);
    end;
    Check(Client.ReceiveText='ack','outbound exchange ack');Peer.Join;
    Check(Peer.Error='','masked canonical client frames: '+Peer.Error);
    Check(Peer.ReceivedFrames=Length(Expected),'all outbound frame lengths read');
  finally Client.Free;Peer.Free;end;
end;

procedure ControlCases;
var Peer: TPeer; Client: TWfcBrowserWebSocket; Expected: array[0..0] of TExpectedFrame;
  Failure: String;
begin
  Expected[0].Opcode:=10;Expected[0].Payload:='ping-data';
  Peer:=TPeer.Create(Frame(1,'a',False)+Frame(9,'ping-data')+Frame(10,'unsolicited')+Frame(0,'b'),0,Expected);
  Client:=nil;
  try
    Client:=TWfcBrowserWebSocket.Create(Peer.Port,'/devtools/browser/test-id',WfcBrowserTickCount64+3000,2);
    Check(Client.ReceiveText='ab','interleaved controls preserve fragmented message');Peer.Join;
    Check(Peer.Error='','masked pong exact: '+Peer.Error);Check(Peer.ReceivedFrames=1,'one pong returned');
  finally Client.Free;Peer.Free;end;
  Expected[0].Opcode:=8;Expected[0].Payload:=#$03#$E8+'done';
  Peer:=TPeer.Create(Frame(8,Expected[0].Payload),0,Expected);Client:=nil;Failure:='';
  try
    Client:=TWfcBrowserWebSocket.Create(Peer.Port,'/devtools/browser/test-id',WfcBrowserTickCount64+3000,20);
    try Client.ReceiveText;except on X:EWfcBrowserWebSocket do Failure:=X.Message;end;
    Check(Pos('code 1000',Failure)>0,'normal peer close is EOF, not empty text');Peer.Join;
    Check(Peer.Error='','masked close echo exact: '+Peer.Error);Check(Peer.ReceivedFrames=1,'one close reply');
  finally Client.Free;Peer.Free;end;
end;

procedure OutboundReject(const Text: RawByteString; const Limit: Integer; const Error: String);
var Peer: TPeer; Client: TWfcBrowserWebSocket; Failure: String; UTF8: UTF8String;
begin
  Peer:=TPeer.Create('',0,[]);Client:=nil;Failure:='';
  try
    Client:=TWfcBrowserWebSocket.Create(Peer.Port,'/devtools/page/test-id',WfcBrowserTickCount64+3000,Limit);
    SetLength(UTF8,Length(Text));if Text<>'' then Move(Text[1],UTF8[1],Length(Text));
    try Client.SendText(UTF8);except on X:EWfcBrowserWebSocket do Failure:=X.Message;end;
    Check(Pos(Error,Failure)>0,'outbound validation: '+Failure);
  finally Client.Free;Peer.Free;end;
end;

procedure DeadlineAndBufferCases;
var Peer:TPeer;Client:TWfcBrowserWebSocket;Failure:String;Started:QWord;
begin
  Peer:=TPeer.Create(Frame(1,'one')+Frame(1,'two'),0,[]);Client:=nil;
  try
    Client:=TWfcBrowserWebSocket.Create(Peer.Port,'/devtools/page/test-id',WfcBrowserTickCount64+3000,20);
    Check(Client.ReceiveText='one','first buffered message');
    Check(Client.ReceiveText='two','second buffered message survives first return');
    Failure:='';try Client.ReceiveText;except on X:EWfcBrowserWebSocket do Failure:=X.Message;end;
    Check(Pos('transport:',Failure)>0,'EOF after buffered messages is failure');
  finally Client.Free;Peer.Free;end;
  Peer:=TPeer.Create('',20,[]);Client:=nil;Started:=WfcBrowserTickCount64;
  try
    Client:=TWfcBrowserWebSocket.Create(Peer.Port,'/devtools/page/test-id',Started+1000,20);
    Failure:='';try Client.ReceiveText;except on X:EWfcBrowserWebSocket do Failure:=X.Message;end;
    Check(Pos('deadline',Failure)>0,'silent peer hits original deadline');
    Check(WfcBrowserTickCount64-Started<3000,'receive timeout is bounded');
  finally Client.Free;Peer.Free;end;
  Peer:=TPeer.Create('',20,[]);Client:=nil;Started:=WfcBrowserTickCount64;
  try
    Client:=TWfcBrowserWebSocket.Create(Peer.Port,'/devtools/page/test-id',Started+1000,20);
    Sleep(1100);
    Failure:='';try Client.SendText('late');except on X:EWfcBrowserWebSocket do Failure:=X.Message;end;
    Check(Pos('deadline',Failure)>0,'send does not reset an expired constructor deadline');
  finally Client.Free;Peer.Free;end;
  Peer:=TPeer.Create('',20,[]);Client:=nil;
  try
    Client:=TWfcBrowserWebSocket.Create(Peer.Port,'/devtools/page/test-id',WfcBrowserTickCount64+3000,20);
    Started:=WfcBrowserTickCount64;FreeAndNil(Client);
    Check(WfcBrowserTickCount64-Started<1000,'destructor does not wait for peer close');
  finally Client.Free;Peer.Free;end;
end;

procedure Run;
var I:Integer; Bytes:RawByteString;
begin
  BadArguments(0,1,'/devtools/page/id');BadArguments(65536,1,'/devtools/page/id');
  BadArguments(1,0,'/devtools/page/id');BadArguments(1,High(Integer),'/devtools/page/id');
  BadArguments(1,1,'');BadArguments(1,1,'/');BadArguments(1,1,'//host/path');
  BadArguments(1,1,'http://host/path');BadArguments(1,1,'/devtools/page/');
  BadArguments(1,1,'/devtools/page/id'#13#10'Host: attacker');
  BadArguments(1,1,'/devtools/page/id?host=attacker');BadArguments(1,1,'/devtools/page/id/../x');
  BadArguments(1,1,'/devtools/page/'+StringOfChar('x',1100));
  ReceiveCase(Frame(1,''),'',1);
  ReceiveCase(Frame(1,'hello'),'hello',5);
  ReceiveCase(Frame(1,'split'),'split',20,0,True);
  ReceiveCase(Frame(1,'tokens'),'tokens',20,17);
  ReceiveCase(Frame(1,'ab',False)+Frame(0,'',False)+Frame(0,'cd'),'abcd',4);
  Bytes:=#$E2#$82#$AC+#$F0#$9F#$8E#$B5;
  ReceiveCase(Frame(1,Copy(Bytes,1,1),False)+Frame(0,Copy(Bytes,2,4),False)+Frame(0,Copy(Bytes,6,2)),Bytes,7);
  ReceiveCase(Frame(1,StringOfChar('x',126)),StringOfChar('x',126),126);
  ReceiveCase(Frame(1,StringOfChar('x',125)),StringOfChar('x',125),125);
  ReceiveCase(Frame(1,StringOfChar('x',65535)),StringOfChar('x',65535),65535);
  ReceiveCase(Frame(1,StringOfChar('x',65536)),StringOfChar('x',65536),65536);
  for I:=1 to 16 do RejectCase('', '', I);
  RejectCase('', 'tokens', 18);RejectCase('', 'HTTP 101', 19);
  RejectCase(#$C1#0,'reserved');RejectCase(#$81#$80,'must not be masked');
  RejectCase(Frame(2,'binary'),'opcode');RejectCase(Frame(3,''),'opcode');
  RejectCase(Frame(0,'orphan'),'fragmentation');
  RejectCase(Frame(1,'first',False)+Frame(1,'second'),'fragmentation');
  RejectCase(Frame(9,'',False),'control');RejectCase(#$89#126,'control');
  RejectCase(#$81#126#0#1,'noncanonical');
  RejectCase(#$81#127#0#0#0#0#0#0#0#126,'noncanonical');
  RejectCase(#$81#127#$80#0#0#0#0#0#0#0,'64-bit');
  RejectCase(#$81#127#0#0#0#1#0#0#0#0,'exceeds limit',0,16);
  RejectCase(Frame(1,'abcdef'),'exceeds limit',0,5);
  RejectCase(Frame(1,'abc',False)+Frame(0,'def'),'exceeds limit',0,5);
  RejectCase(Frame(1,#$C0#$80),'UTF-8');RejectCase(Frame(1,#$ED#$A0#$80),'UTF-8');
  RejectCase(Frame(1,#$F4#$90#$80#$80),'UTF-8');RejectCase(Frame(1,#$80),'UTF-8');
  RejectCase(Frame(1,#$E2#$82),'incomplete');
  RejectCase(Frame(1,#$E2,False)+Frame(0,'a'),'UTF-8');
  RejectCase(Frame(1,#$E0#$80#$80),'UTF-8');RejectCase(Frame(1,#$F0#$80#$80#$80),'UTF-8');
  RejectCase(Frame(1,#$F5#$80#$80#$80),'UTF-8');RejectCase(Frame(1,#$FF),'UTF-8');
  ReceiveCase(Frame(1,#$00+#$7F+#$C2#$80+#$DF#$BF+#$E0#$A0#$80+#$ED#$9F#$BF+
    #$EF#$BF#$BF+#$F0#$90#$80#$80+#$F4#$8F#$BF#$BF),
    #$00+#$7F+#$C2#$80+#$DF#$BF+#$E0#$A0#$80+#$ED#$9F#$BF+
    #$EF#$BF#$BF+#$F0#$90#$80#$80+#$F4#$8F#$BF#$BF);
  RejectCase(Frame(8,'x'),'close payload');RejectCase(Frame(8,#$03#$ED),'close code');
  RejectCase(Frame(8,#$03#$E7),'close code');RejectCase(Frame(8,#$13#$88),'close code');
  RejectCase(Frame(8,#$03#$F7),'close code');
  RejectCase(Frame(8,#$03#$E8+#$C0),'close reason');
  RejectCase(#$81#5+'ab','transport:');
  OutboundReject('abcdef',5,'exceeds limit');OutboundReject(#$C0#$80,20,'UTF-8');
  OutboundReject(#$E2#$82,20,'incomplete');
  SendCases;ControlCases;DeadlineAndBufferCases;
end;

{$IFDEF MSWINDOWS}var Data:TWSAData;{$ENDIF}
begin
  {$IFDEF MSWINDOWS}if WSAStartup($0202,Data)<>0 then raise Exception.Create('mock WSAStartup failed');{$ENDIF}
  try
    Checks:=0;Run;WriteLn('WebSocket checks: ',Checks,', failures: 0');
  finally {$IFDEF MSWINDOWS}WSACleanup;{$ENDIF}end;
end.
