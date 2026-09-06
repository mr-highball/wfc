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
unit wfc_browser_websocket;

{$mode delphi}{$H+}

interface

uses SysUtils, wfc_browser_socket;

type
  EWfcBrowserWebSocket = class(Exception);

  { A synchronous, loopback-only CDP text transport. RFC6455 sections 4.1,
    5, 7 and 8: https://www.rfc-editor.org/rfc/rfc6455.html
    No TLS, extensions,
    compression, authentication, redirects or externally supplied Host header.
    Every operation shares the constructor's absolute monotonic deadline.
    Destroy closes the socket immediately: it never waits for a close reply. }
  TWfcBrowserWebSocket = class
  private
    FSocket: TWfcBrowserSocket;
    FDeadline: QWord;
    FMaxMessageBytes: Integer;
    FBuffer: RawByteString;
    FReadPosition: Integer;
    procedure RaiseFailure(const AMessage: String);
    procedure CheckOpen;
    function ReadExact(const ACount: Integer): RawByteString;
    procedure Handshake(const APort: Integer; const APath: String);
    procedure SendFrame(const AOpcode: Byte; const APayload: RawByteString);
  public
    constructor Create(APort: Integer; const APath: String;
      ADeadline: QWord; AMaxMessageBytes: Integer);
    destructor Destroy; override;
    procedure SendText(const AText: UTF8String);
    function ReceiveText: UTF8String;
  end;

implementation

uses Classes, SHA1, Base64;

const
  WFC_WEBSOCKET_MAX_HEADER_BYTES = 16384;
  WFC_WEBSOCKET_CHUNK_BYTES = 65536;
  WFC_WEBSOCKET_GUID = '258EAFA5-E914-47DA-95CA-C5AB0DC85B11';

type
  TUTF8State = record
    Remaining: Integer;
    NextMin, NextMax: Byte;
  end;

function FeedUTF8(const S: RawByteString; var AState: TUTF8State): Boolean;
var I: Integer; B: Byte;
begin
  Result := False;
  for I := 1 to Length(S) do
  begin
    B := Ord(S[I]);
    if AState.Remaining > 0 then
    begin
      if (B < AState.NextMin) or (B > AState.NextMax) then Exit;
      Dec(AState.Remaining);
      AState.NextMin := $80; AState.NextMax := $BF;
    end
    else
    begin
      AState.NextMin := $80; AState.NextMax := $BF;
      case B of
        $00..$7F: ;
        $C2..$DF: AState.Remaining := 1;
        $E0..$EF:
          begin
            AState.Remaining := 2;
            if B = $E0 then AState.NextMin := $A0;
            if B = $ED then AState.NextMax := $9F;
          end;
        $F0..$F4:
          begin
            AState.Remaining := 3;
            if B = $F0 then AState.NextMin := $90;
            if B = $F4 then AState.NextMax := $8F;
          end;
        else Exit;
      end;
    end;
  end;
  Result := True;
end;

function IsToken(const S: RawByteString): Boolean;
var I: Integer;
begin
  Result := False;
  if S = '' then Exit;
  for I := 1 to Length(S) do
    if not (S[I] in ['a'..'z', 'A'..'Z', '0'..'9', '!', '#', '$', '%',
      '&', '''', '*', '+', '-', '.', '^', '_', '`', '|', '~']) then Exit;
  Result := True;
end;

function ConnectionHasUpgrade(const S: RawByteString): Boolean;
var Start, I: Integer; Token: RawByteString;
begin
  Result := False; Start := 1;
  for I := 1 to Length(S) + 1 do
    if (I > Length(S)) or (S[I] = ',') then
    begin
      Token := Trim(Copy(S, Start, I - Start));
      if not IsToken(Token) then Exit(False);
      if LowerCase(Token) = 'upgrade' then Result := True;
      Start := I + 1;
    end;
end;

procedure TWfcBrowserWebSocket.RaiseFailure(const AMessage: String);
begin
  FreeAndNil(FSocket);
  FBuffer := ''; FReadPosition := 1;
  raise EWfcBrowserWebSocket.Create(AMessage);
end;

procedure TWfcBrowserWebSocket.CheckOpen;
begin
  if FSocket = nil then RaiseFailure('WebSocket transport is closed');
  WfcBrowserCheckDeadline(FDeadline);
end;

constructor TWfcBrowserWebSocket.Create(APort: Integer; const APath: String;
  ADeadline: QWord; AMaxMessageBytes: Integer);
var I, PrefixLength: Integer;
begin
  inherited Create;
  FDeadline := ADeadline; FReadPosition := 1;
  if (APort < 1) or (APort > 65535) then RaiseFailure('invalid WebSocket loopback port');
  if (AMaxMessageBytes < 1) or (AMaxMessageBytes > High(Integer) - 14) then
    RaiseFailure('invalid WebSocket message byte limit');
  FMaxMessageBytes := AMaxMessageBytes;
  PrefixLength := 0;
  if Copy(APath, 1, 18) = '/devtools/browser/' then PrefixLength := 18
  else if Copy(APath, 1, 15) = '/devtools/page/' then PrefixLength := 15;
  if (PrefixLength = 0) or (Length(APath) <= PrefixLength) or
      (Length(APath) > 1024) then RaiseFailure('owned CDP WebSocket path required');
  for I := PrefixLength + 1 to Length(APath) do
    if not (APath[I] in ['a'..'z', 'A'..'Z', '0'..'9', '-', '_']) then
      RaiseFailure('invalid owned CDP WebSocket path');
  try
    FSocket := TWfcBrowserSocket.Create(APort, FDeadline);
    Handshake(APort, APath);
  except
    on E: EWfcBrowserSocket do RaiseFailure('WebSocket transport: ' + E.Message);
  end;
end;

destructor TWfcBrowserWebSocket.Destroy;
begin
  FSocket.Free;
  inherited Destroy;
end;

procedure TWfcBrowserWebSocket.Handshake(const APort: Integer; const APath: String);
var
  Nonce: array[0..15] of Byte;
  Digest: TSHA1Digest;
  RawNonce, Key, Headers, Request, Line, Name, Value: RawByteString;
  AcceptValue, UpgradeValue, ConnectionValue, ExpectedAccept: RawByteString;
  HeaderEnd, Position, NextLine, Colon, I: Integer;
  HasAccept, HasUpgrade, HasConnection: Boolean;
begin
  WfcBrowserRandomBytes(Nonce, SizeOf(Nonce));
  SetString(RawNonce, PAnsiChar(@Nonce[0]), SizeOf(Nonce));
  Key := EncodeStringBase64(RawNonce);
  Digest := SHA1String(Key + WFC_WEBSOCKET_GUID);
  SetString(RawNonce, PAnsiChar(@Digest[0]), SizeOf(Digest));
  ExpectedAccept := EncodeStringBase64(RawNonce);
  Request := 'GET ' + RawByteString(APath) + ' HTTP/1.1'#13#10 +
    'Host: 127.0.0.1:' + IntToStr(APort) + #13#10 +
    'Upgrade: websocket'#13#10'Connection: Upgrade'#13#10 +
    'Sec-WebSocket-Key: ' + Key + #13#10 +
    'Sec-WebSocket-Version: 13'#13#10#13#10;
  FSocket.WriteAll(Request);
  Headers := '';
  repeat
    WfcBrowserCheckDeadline(FDeadline);
    Headers := Headers + FSocket.ReadSome(4096);
    HeaderEnd := Pos(#13#10#13#10, Headers);
    if HeaderEnd > 0 then
    begin
      if HeaderEnd + 3 > WFC_WEBSOCKET_MAX_HEADER_BYTES then
        RaiseFailure('WebSocket upgrade headers exceed limit');
      Break;
    end;
    if Length(Headers) >= WFC_WEBSOCKET_MAX_HEADER_BYTES then
      RaiseFailure('WebSocket upgrade headers exceed limit');
  until False;
  FBuffer := Copy(Headers, HeaderEnd + 4, MaxInt);
  SetLength(Headers, HeaderEnd + 1);
  Position := 1;
  NextLine := Pos(#13#10, Headers);
  Line := Copy(Headers, 1, NextLine - 1);
  if Copy(Line, 1, 13) <> 'HTTP/1.1 101 ' then
    RaiseFailure('WebSocket upgrade did not return HTTP 101');
  for I := 1 to Length(Line) do
    if not (Line[I] in [#32..#126]) then RaiseFailure('invalid WebSocket status line');
  Position := NextLine + 2;
  HasAccept := False; HasUpgrade := False; HasConnection := False;
  AcceptValue := ''; UpgradeValue := ''; ConnectionValue := '';
  while Position <= Length(Headers) do
  begin
    NextLine := Pos(#13#10, Copy(Headers, Position, MaxInt));
    if NextLine = 0 then RaiseFailure('malformed WebSocket upgrade header');
    Line := Copy(Headers, Position, NextLine - 1);
    Inc(Position, NextLine + 1);
    Colon := Pos(':', Line);
    if Colon < 2 then RaiseFailure('malformed WebSocket upgrade header');
    Name := Copy(Line, 1, Colon - 1);
    if not IsToken(Name) then RaiseFailure('invalid WebSocket header name');
    Value := Copy(Line, Colon + 1, MaxInt);
    for I := 1 to Length(Value) do
      if not (Value[I] in [#9, #32..#126]) then RaiseFailure('invalid WebSocket header value');
    Name := LowerCase(Name); Value := Trim(Value);
    if Name = 'sec-websocket-accept' then
    begin
      if HasAccept then RaiseFailure('duplicate WebSocket accept header');
      HasAccept := True; AcceptValue := Value;
    end
    else if Name = 'upgrade' then
    begin
      if HasUpgrade then RaiseFailure('duplicate WebSocket upgrade header');
      HasUpgrade := True; UpgradeValue := LowerCase(Value);
    end
    else if Name = 'connection' then
    begin
      if HasConnection then ConnectionValue := ConnectionValue + ',';
      HasConnection := True;
      ConnectionValue := ConnectionValue + Value;
    end
    else if (Name = 'sec-websocket-extensions') or (Name = 'sec-websocket-protocol') then
      RaiseFailure('unsolicited WebSocket extension or subprotocol')
    else if (Name = 'transfer-encoding') or
      ((Name = 'content-length') and (Value <> '0')) then
      RaiseFailure('unexpected WebSocket upgrade body');
  end;
  if not HasAccept or (AcceptValue <> ExpectedAccept) then
    RaiseFailure('invalid WebSocket accept digest');
  if not HasUpgrade or (UpgradeValue <> 'websocket') or
      not ConnectionHasUpgrade(ConnectionValue) then
    RaiseFailure('missing WebSocket upgrade tokens');
end;

function TWfcBrowserWebSocket.ReadExact(const ACount: Integer): RawByteString;
var Offset, Available, Take: Integer;
begin
  SetLength(Result, ACount); Offset := 0;
  while Offset < ACount do
  begin
    CheckOpen;
    Available := Length(FBuffer) - FReadPosition + 1;
    if Available = 0 then
    begin
      FBuffer := FSocket.ReadSome(WFC_WEBSOCKET_CHUNK_BYTES);
      FReadPosition := 1; Available := Length(FBuffer);
    end;
    Take := ACount - Offset;
    if Take > Available then Take := Available;
    Move(FBuffer[FReadPosition], Result[Offset + 1], Take);
    Inc(Offset, Take); Inc(FReadPosition, Take);
  end;
end;

procedure TWfcBrowserWebSocket.SendFrame(const AOpcode: Byte; const APayload: RawByteString);
var Header, Part: RawByteString; Mask: array[0..3] of Byte;
  PayloadLength: QWord; Offset, Take, I: Integer;
begin
  CheckOpen;
  PayloadLength := Length(APayload);
  Header := AnsiChar($80 or AOpcode);
  if PayloadLength < 126 then Header := Header + AnsiChar($80 or Byte(PayloadLength))
  else if PayloadLength <= 65535 then
    Header := Header + #$FE + AnsiChar(PayloadLength shr 8) + AnsiChar(PayloadLength and $FF)
  else
  begin
    Header := Header + #$FF;
    for I := 7 downto 0 do Header := Header + AnsiChar((PayloadLength shr (I * 8)) and $FF);
  end;
  WfcBrowserRandomBytes(Mask, SizeOf(Mask));
  for I := 0 to 3 do Header := Header + AnsiChar(Mask[I]);
  FSocket.WriteAll(Header);
  Offset := 0;
  while Offset < Length(APayload) do
  begin
    CheckOpen;
    Take := Length(APayload) - Offset;
    if Take > WFC_WEBSOCKET_CHUNK_BYTES then Take := WFC_WEBSOCKET_CHUNK_BYTES;
    SetLength(Part, Take);
    for I := 0 to Take - 1 do
      Part[I + 1] := AnsiChar(Ord(APayload[Offset + I + 1]) xor Mask[(Offset + I) and 3]);
    FSocket.WriteAll(Part);
    Inc(Offset, Take);
  end;
end;

procedure TWfcBrowserWebSocket.SendText(const AText: UTF8String);
var State: TUTF8State; Offset, Take: Integer; Bytes: RawByteString;
begin
  try
    CheckOpen;
    if Length(AText) > FMaxMessageBytes then RaiseFailure('outgoing WebSocket message exceeds limit');
    State := Default(TUTF8State); Offset := 0; Bytes := RawByteString(AText);
    while Offset < Length(Bytes) do
    begin
      CheckOpen;
      Take := Length(Bytes) - Offset;
      if Take > WFC_WEBSOCKET_CHUNK_BYTES then Take := WFC_WEBSOCKET_CHUNK_BYTES;
      if not FeedUTF8(Copy(Bytes, Offset + 1, Take), State) then
        RaiseFailure('invalid outgoing WebSocket UTF-8');
      Inc(Offset, Take);
    end;
    if State.Remaining <> 0 then RaiseFailure('incomplete outgoing WebSocket UTF-8');
    SendFrame(1, Bytes);
  except
    on E: EWfcBrowserSocket do RaiseFailure('WebSocket transport: ' + E.Message);
  end;
end;

function TWfcBrowserWebSocket.ReceiveText: UTF8String;
var
  Header, Payload, MessageBytes: RawByteString;
  Opcode, LengthTag: Byte;
  FinalFrame, Fragmented: Boolean;
  FrameLength: QWord;
  MessageLength, Capacity, NewCapacity, Take, I, CloseCode: Integer;
  State, CloseState: TUTF8State;
begin
  Result := ''; MessageBytes := ''; MessageLength := 0; Capacity := 0;
  Fragmented := False; State := Default(TUTF8State);
  try
    repeat
      CheckOpen;
      Header := ReadExact(2);
      FinalFrame := (Ord(Header[1]) and $80) <> 0;
      Opcode := Ord(Header[1]) and $0F;
      if (Ord(Header[1]) and $70) <> 0 then RaiseFailure('WebSocket reserved frame bits set');
      if not (Opcode in [0, 1, 8, 9, 10]) then RaiseFailure('unsupported WebSocket opcode');
      if (Ord(Header[2]) and $80) <> 0 then RaiseFailure('WebSocket server frame must not be masked');
      LengthTag := Ord(Header[2]) and $7F;
      if (Opcode >= 8) and (not FinalFrame or (LengthTag > 125)) then
        RaiseFailure('invalid WebSocket control frame');
      FrameLength := LengthTag;
      if LengthTag = 126 then
      begin
        Header := ReadExact(2);
        FrameLength := QWord(Ord(Header[1])) * 256 + Ord(Header[2]);
        if FrameLength < 126 then RaiseFailure('noncanonical WebSocket frame length');
      end
      else if LengthTag = 127 then
      begin
        Header := ReadExact(8);
        if (Ord(Header[1]) and $80) <> 0 then RaiseFailure('invalid 64-bit WebSocket frame length');
        FrameLength := 0;
        for I := 1 to 8 do FrameLength := (FrameLength shl 8) or Ord(Header[I]);
        if FrameLength <= 65535 then RaiseFailure('noncanonical WebSocket frame length');
      end;
      if Opcode >= 8 then
      begin
        Payload := ReadExact(Integer(FrameLength));
        case Opcode of
          8:
            begin
              if Length(Payload) = 1 then RaiseFailure('invalid WebSocket close payload');
              CloseCode := 1005;
              if Length(Payload) >= 2 then
              begin
                CloseCode := Ord(Payload[1]) * 256 + Ord(Payload[2]);
                { Known protocol codes (excluding forbidden wire sentinels),
                  plus registered/private application ranges. No extensions.
                  https://www.iana.org/assignments/websocket/#close-code-number }
                if not (((CloseCode >= 1000) and (CloseCode <= 1014) and
                    (CloseCode <> 1004) and (CloseCode <> 1005) and (CloseCode <> 1006)) or
                    ((CloseCode >= 3000) and (CloseCode <= 4999))) then
                  RaiseFailure('invalid WebSocket close code');
                CloseState := Default(TUTF8State);
                if not FeedUTF8(Copy(Payload, 3, MaxInt), CloseState) or
                    (CloseState.Remaining <> 0) then RaiseFailure('invalid WebSocket close reason UTF-8');
              end;
              SendFrame(8, Payload);
              RaiseFailure('WebSocket peer closed (code ' + IntToStr(CloseCode) + ')');
            end;
          9: SendFrame(10, Payload);
          10: ;
        end;
        Continue;
      end;
      if ((Opcode = 0) and not Fragmented) or
          ((Opcode = 1) and Fragmented) then RaiseFailure('invalid WebSocket fragmentation sequence');
      if FrameLength > QWord(FMaxMessageBytes - MessageLength) then
        RaiseFailure('incoming WebSocket message exceeds limit');
      while FrameLength > 0 do
      begin
        Take := WFC_WEBSOCKET_CHUNK_BYTES;
        if FrameLength < QWord(Take) then Take := Integer(FrameLength);
        Payload := ReadExact(Take);
        if not FeedUTF8(Payload, State) then RaiseFailure('invalid incoming WebSocket UTF-8');
        if MessageLength + Take > Capacity then
        begin
          NewCapacity := Capacity;
          if NewCapacity = 0 then NewCapacity := 256;
          if NewCapacity > FMaxMessageBytes then NewCapacity := FMaxMessageBytes;
          while NewCapacity < MessageLength + Take do
            if NewCapacity > FMaxMessageBytes div 2 then NewCapacity := FMaxMessageBytes
            else NewCapacity := NewCapacity * 2;
          SetLength(MessageBytes, NewCapacity); Capacity := NewCapacity;
        end;
        Move(Payload[1], MessageBytes[MessageLength + 1], Take);
        Inc(MessageLength, Take); Dec(FrameLength, Take);
      end;
      Fragmented := not FinalFrame;
      if FinalFrame then
      begin
        if State.Remaining <> 0 then RaiseFailure('incomplete incoming WebSocket UTF-8');
        SetLength(MessageBytes, MessageLength);
        SetCodePage(MessageBytes, 65001, False);
        Result := UTF8String(MessageBytes);
        Exit;
      end;
    until False;
  except
    on E: EWfcBrowserSocket do RaiseFailure('WebSocket transport: ' + E.Message);
  end;
end;

end.
