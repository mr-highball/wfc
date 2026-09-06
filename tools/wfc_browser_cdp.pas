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
unit wfc_browser_cdp;
{$mode delphi}{$H+}
interface
uses SysUtils, fpjson, wfc_browser_websocket;
const
  WFC_BROWSER_CDP_MAX_JSON_DEPTH = 64;
  WFC_BROWSER_CDP_MAX_DIAGNOSTIC_BYTES = 1024;
type
  EWfcBrowserCDP = class(Exception);
  { Only a well-formed error response to the exact pending command uses this
    type. Callers may distinguish a stale DOM node from protocol corruption. }
  EWfcBrowserCDPCommand = class(EWfcBrowserCDP)
  private
    FCode: Integer;
    FCommandMessage: String;
  public
    constructor Create(ACode: Integer; const ACommandMessage: String); reintroduce;
    property Code: Integer read FCode;
    property CommandMessage: String read FCommandMessage;
  end;
  TWfcBrowserCDP = class
  private
    FSocket: TWfcBrowserWebSocket;
    FDeadline: QWord;
    FLastId, FMaxMessageBytes: Integer;
    procedure CheckDeadline;
  public
    constructor Create(APort: Integer; const APath: String; ADeadline: QWord;
      AMaxMessageBytes: Integer);
    destructor Destroy; override;
    { Sequential calls only. Params are borrowed; caller owns the result. }
    function Call(const AMethod: String; const AParams: TJSONObject = nil;
      const ASessionId: String = ''): TJSONObject;
  end;
{ Pure protocol helpers. Requests borrow params. A valid notification returns
  nil; a matching successful response returns an independently owned object.
  Invalid JSON, envelopes, IDs and session IDs always raise EWfcBrowserCDP. }
function WfcBrowserCDPRequest(AId: Integer; const AMethod: String;
  const AParams: TJSONObject; const ASessionId: String): UTF8String;
function WfcBrowserCDPResponse(const AText: UTF8String; AExpectedId: Integer;
  const ASessionId: String; AMaxMessageBytes: Integer): TJSONObject;
implementation
uses jsonparser, jsonscanner, wfc_browser_socket;
type
  TCDPJSONParser = class(TJSONParser)
  protected
    procedure KeyValue(const AKey: TJSONStringType); override;
    procedure StringValue(const AValue: TJSONStringType); override;
  end;

procedure RaiseCDPError(const AMessage: String);
begin
  raise EWfcBrowserCDP.Create(AMessage);
end;

function Diagnostic(const AText: String): String;
var I: Integer; Piece: String;
begin
  Result := '';
  for I := 1 to Length(AText) do
  begin
    case AText[I] of
      '"': Piece := '\"';
      '\': Piece := '\\';
      #32..#33, #35..#91, #93..#126: Piece := AText[I];
      else Piece := '\x' + IntToHex(Ord(AText[I]), 2);
    end;
    if Length(Result) + Length(Piece) + 3 >
        WFC_BROWSER_CDP_MAX_DIAGNOSTIC_BYTES then
    begin
      Result := Result + '...';
      Break;
    end;
    Result := Result + Piece;
  end;
end;

constructor EWfcBrowserCDPCommand.Create(ACode: Integer;
  const ACommandMessage: String);
begin
  FCode := ACode;
  FCommandMessage := Diagnostic(ACommandMessage);
  inherited Create('CDP command error ' + IntToStr(ACode) + ': ' +
    FCommandMessage);
end;

procedure CheckMethod(const AMethod: String);
var I: Integer;
begin
  if (AMethod = '') or (Length(AMethod) > 256) then
    RaiseCDPError('CDP method must be a nonempty bounded name');
  for I := 1 to Length(AMethod) do
    if not (AMethod[I] in ['a'..'z', 'A'..'Z', '0'..'9', '_', '.']) then
      RaiseCDPError('CDP method contains an invalid character');
end;

function WfcBrowserCDPRequest(AId: Integer; const AMethod: String;
  const AParams: TJSONObject; const ASessionId: String): UTF8String;
var Request: TJSONObject;
begin
  if AId < 1 then RaiseCDPError('CDP request ID must be positive');
  CheckMethod(AMethod);
  Request := TJSONObject.Create;
  try
    Request.Add('id', AId);
    Request.Add('method', UTF8String(AMethod));
    if AParams <> nil then Request.Add('params', AParams.Clone);
    if ASessionId <> '' then Request.Add('sessionId', UTF8String(ASessionId));
    Result := Request.AsJSON;
  finally
    Request.Free;
  end;
end;

procedure CheckJSONBounds(const AText: UTF8String; AMaxMessageBytes: Integer);
var I, Depth: Integer; InString, Escaped: Boolean;
begin
  if AMaxMessageBytes < 1 then RaiseCDPError('CDP message limit must be positive');
  if (Length(AText) = 0) or (Length(AText) > AMaxMessageBytes) then
    RaiseCDPError('CDP JSON message is empty or exceeds its byte limit');
  { Bound recursion before entering the standard JSON parser. Brackets inside
    escaped JSON strings (including captured HTML) do not contribute depth. }
  Depth := 0;
  InString := False;
  Escaped := False;
  for I := 1 to Length(AText) do
    if InString then
    begin
      if Escaped then Escaped := False
      else if AText[I] = '\' then Escaped := True
      else if AText[I] = '"' then InString := False;
    end
    else
      case AText[I] of
        '"': InString := True;
        '{', '[':
          begin
            Inc(Depth);
            if Depth > WFC_BROWSER_CDP_MAX_JSON_DEPTH then
              RaiseCDPError('CDP JSON nesting exceeds its depth limit');
          end;
        '}', ']':
          begin
            Dec(Depth);
            if Depth < 0 then RaiseCDPError('CDP JSON has unbalanced containers');
          end;
      end;
  if InString or (Depth <> 0) then
    RaiseCDPError('CDP JSON has an unterminated string or container');
end;

function HexDigit(const AChar: AnsiChar): Integer;
begin
  case AChar of
    '0'..'9': Result := Ord(AChar) - Ord('0');
    'a'..'f': Result := Ord(AChar) - Ord('a') + 10;
    'A'..'F': Result := Ord(AChar) - Ord('A') + 10;
    else Result := -1;
  end;
end;

function EncodeJSONStrings(const AText: UTF8String): UTF8String;
const Hex: array[0..15] of AnsiChar = '0123456789ABCDEF';
var I, N, Used, Code, LowCode, B, Count, J: Integer;
  procedure Put(const AChar: AnsiChar);
  begin
    Inc(Used);
    Result[Used] := AChar;
  end;
  procedure PutByte(const AByte: Byte);
  begin
    Put(Hex[AByte shr 4]);
    Put(Hex[AByte and 15]);
  end;
  function CodeUnit: Integer;
  var Digit, K: Integer;
  begin
    if I + 3 > N then RaiseCDPError('CDP JSON Unicode escape is incomplete');
    Result := 0;
    for K := 1 to 4 do
    begin
      Digit := HexDigit(AText[I]);
      if Digit < 0 then RaiseCDPError('CDP JSON Unicode escape is invalid');
      Result := Result * 16 + Digit;
      Inc(I);
    end;
  end;
  procedure PutScalar(const ACode: Integer);
  begin
    if ACode < $80 then PutByte(ACode)
    else if ACode < $800 then
    begin
      PutByte($C0 or (ACode shr 6));
      PutByte($80 or (ACode and $3F));
    end
    else if ACode < $10000 then
    begin
      PutByte($E0 or (ACode shr 12));
      PutByte($80 or ((ACode shr 6) and $3F));
      PutByte($80 or (ACode and $3F));
    end
    else
    begin
      PutByte($F0 or (ACode shr 18));
      PutByte($80 or ((ACode shr 12) and $3F));
      PutByte($80 or ((ACode shr 6) and $3F));
      PutByte($80 or (ACode and $3F));
    end;
  end;
begin
  { FPC 3.2.2's JSON scanner passes isolated Unicode escapes through the
    system ANSI code page and loses \u0000. Encode only string tokens as ASCII
    hex, then decode them in parser callbacks before FCL checks object keys.
    JSON structure, numbers, duplicate keys and trailing data remain FCL's
    responsibility. No global code-page or parser defaults are changed. }
  N := Length(AText);
  if N > High(Integer) div 2 then RaiseCDPError('CDP JSON string workspace exceeds limit');
  SetLength(Result, N * 2);
  I := 1;
  Used := 0;
  while I <= N do
    if AText[I] <> '"' then
    begin
      Put(AText[I]);
      Inc(I);
    end
    else
    begin
      Put('"');
      Inc(I);
      while (I <= N) and (AText[I] <> '"') do
      begin
        if AText[I] = '\' then
        begin
          Inc(I);
          if I > N then RaiseCDPError('CDP JSON escape is incomplete');
          case AText[I] of
            '"', '\', '/': begin PutByte(Ord(AText[I])); Inc(I); end;
            'b': begin PutByte(8); Inc(I); end;
            'f': begin PutByte(12); Inc(I); end;
            'n': begin PutByte(10); Inc(I); end;
            'r': begin PutByte(13); Inc(I); end;
            't': begin PutByte(9); Inc(I); end;
            'u':
              begin
                Inc(I);
                Code := CodeUnit;
                if (Code >= $D800) and (Code <= $DBFF) then
                begin
                  if (I + 1 > N) or (AText[I] <> '\') or (AText[I + 1] <> 'u') then
                    RaiseCDPError('CDP JSON high surrogate requires a low surrogate');
                  Inc(I, 2);
                  LowCode := CodeUnit;
                  if (LowCode < $DC00) or (LowCode > $DFFF) then
                    RaiseCDPError('CDP JSON surrogate pair is invalid');
                  Code := $10000 + ((Code - $D800) shl 10) + LowCode - $DC00;
                end
                else if (Code >= $DC00) and (Code <= $DFFF) then
                  RaiseCDPError('CDP JSON low surrogate has no high surrogate');
                PutScalar(Code);
              end;
            else RaiseCDPError('CDP JSON escape is invalid');
          end;
        end
        else
        begin
          B := Ord(AText[I]);
          if B < 32 then RaiseCDPError('CDP JSON string has an unescaped control');
          { Validate raw UTF-8 too, including pure helper calls without a
            WebSocket. Unicode escapes and raw strings share scalar rules. }
          if B < $80 then Count := 1
          else if (B >= $C2) and (B <= $DF) then Count := 2
          else if (B >= $E0) and (B <= $EF) then Count := 3
          else if (B >= $F0) and (B <= $F4) then Count := 4
          else begin RaiseCDPError('CDP JSON string has invalid UTF-8'); Count := 0; end;
          if I + Count - 1 > N then RaiseCDPError('CDP JSON UTF-8 sequence is incomplete');
          for J := 1 to Count - 1 do
            if (Ord(AText[I + J]) < $80) or (Ord(AText[I + J]) > $BF) then
              RaiseCDPError('CDP JSON string has invalid UTF-8 continuation');
          if (Count > 1) and
              (((B = $E0) and (Ord(AText[I + 1]) < $A0)) or
               ((B = $ED) and (Ord(AText[I + 1]) > $9F)) or
               ((B = $F0) and (Ord(AText[I + 1]) < $90)) or
               ((B = $F4) and (Ord(AText[I + 1]) > $8F))) then
            RaiseCDPError('CDP JSON UTF-8 scalar is invalid');
          for J := 0 to Count - 1 do PutByte(Ord(AText[I + J]));
          Inc(I, Count);
        end;
      end;
      if I > N then RaiseCDPError('CDP JSON string is unterminated');
      Put('"');
      Inc(I);
    end;
  SetLength(Result, Used);
end;

function DecodeStringToken(const AText: TJSONStringType): TJSONStringType;
var I: Integer;
begin
  SetLength(Result, Length(AText) div 2);
  for I := 1 to Length(Result) do
    Result[I] := AnsiChar((HexDigit(AText[I * 2 - 1]) shl 4) or
      HexDigit(AText[I * 2]));
end;

procedure TCDPJSONParser.KeyValue(const AKey: TJSONStringType);
begin
  inherited KeyValue(DecodeStringToken(AKey));
end;

procedure TCDPJSONParser.StringValue(const AValue: TJSONStringType);
begin
  inherited StringValue(DecodeStringToken(AValue));
end;

function IntegerValue(const AValue: TJSONData; out AInteger: Integer): Boolean;
begin
  Result := False;
  if (AValue = nil) or (AValue.JSONType <> jtNumber) then Exit;
  if TJSONNumber(AValue).NumberType = ntFloat then Exit;
  Result := TryStrToInt(String(AValue.AsJSON), AInteger);
end;

function WfcBrowserCDPResponse(const AText: UTF8String; AExpectedId: Integer;
  const ASessionId: String; AMaxMessageBytes: Integer): TJSONObject;
var
  Parser: TJSONParser;
  Data, Id, Session, Method, Params, Value, ErrorValue, Code, MessageValue: TJSONData;
  Envelope, ErrorObject: TJSONObject;
  ResponseId, ErrorCode: Integer;
begin
  Result := nil;
  if AExpectedId < 1 then RaiseCDPError('CDP expected response ID must be positive');
  CheckJSONBounds(AText, AMaxMessageBytes);
  Data := nil;
  { Strict parsing also rejects duplicate keys, trailing content, comments and
    trailing commas in both FPC 3.2.2 and current FCL JSON implementations. }
  Parser := TCDPJSONParser.Create(EncodeJSONStrings(AText), [joUTF8, joStrict]);
  try
    try
      Data := Parser.Parse;
    except
      on E: Exception do RaiseCDPError('CDP invalid JSON: ' + Diagnostic(E.Message));
    end;
  finally
    Parser.Free;
  end;
  try
    if (Data = nil) or (Data.JSONType <> jtObject) then
      RaiseCDPError('CDP message must be a JSON object');
    Envelope := TJSONObject(Data);
    Id := Envelope.Find('id');
    Session := Envelope.Find('sessionId');
    Method := Envelope.Find('method');
    Params := Envelope.Find('params');
    Value := Envelope.Find('result');
    ErrorValue := Envelope.Find('error');
    if Session <> nil then
      if (Session.JSONType <> jtString) or (Session.AsString = '') then
        RaiseCDPError('CDP session ID must be a nonempty string');
    if Id = nil then
    begin
      if (Method = nil) or (Method.JSONType <> jtString) then
        RaiseCDPError('CDP notification must contain a method');
      CheckMethod(String(Method.AsString));
      if (Value <> nil) or (ErrorValue <> nil) then
        RaiseCDPError('CDP notification cannot contain a response result or error');
      if (Params <> nil) and (Params.JSONType <> jtObject) then
        RaiseCDPError('CDP notification params must be an object');
      Exit;
    end;
    if (Method <> nil) or (Params <> nil) then
      RaiseCDPError('CDP response cannot contain a notification method or params');
    if not IntegerValue(Id, ResponseId) or (ResponseId <> AExpectedId) then
      RaiseCDPError('CDP response ID does not match pending command ' + IntToStr(AExpectedId));
    if ASessionId = '' then
    begin
      if Session <> nil then RaiseCDPError('CDP browser response has an unexpected session ID');
    end
    else if (Session = nil) or (String(Session.AsString) <> ASessionId) then
      RaiseCDPError('CDP response session ID does not match pending command');
    if (Value = nil) = (ErrorValue = nil) then
      RaiseCDPError('CDP response requires exactly one result or error');
    if ErrorValue <> nil then
    begin
      if ErrorValue.JSONType <> jtObject then RaiseCDPError('CDP error must be an object');
      ErrorObject := TJSONObject(ErrorValue);
      Code := ErrorObject.Find('code');
      MessageValue := ErrorObject.Find('message');
      if not IntegerValue(Code, ErrorCode) or (MessageValue = nil) or
          (MessageValue.JSONType <> jtString) then
        RaiseCDPError('CDP error requires an integer code and string message');
      raise EWfcBrowserCDPCommand.Create(ErrorCode, String(MessageValue.AsString));
    end;
    if Value.JSONType <> jtObject then RaiseCDPError('CDP result must be an object');
    Result := TJSONObject(Envelope.Extract('result'));
  finally
    Data.Free;
  end;
end;

procedure TWfcBrowserCDP.CheckDeadline;
begin
  if WfcBrowserTickCount64 >= FDeadline then RaiseCDPError('CDP real-time deadline exceeded');
end;

constructor TWfcBrowserCDP.Create(APort: Integer; const APath: String;
  ADeadline: QWord; AMaxMessageBytes: Integer);
begin
  inherited Create;
  FDeadline := ADeadline;
  FMaxMessageBytes := AMaxMessageBytes;
  if FMaxMessageBytes < 1 then RaiseCDPError('CDP message limit must be positive');
  CheckDeadline;
  try
    FSocket := TWfcBrowserWebSocket.Create(APort, APath, ADeadline, AMaxMessageBytes);
  except
    on E: Exception do RaiseCDPError('CDP transport: ' + Diagnostic(E.Message));
  end;
  CheckDeadline;
end;

destructor TWfcBrowserCDP.Destroy;
begin
  FSocket.Free;
  inherited Destroy;
end;

function TWfcBrowserCDP.Call(const AMethod: String; const AParams: TJSONObject;
  const ASessionId: String): TJSONObject;
var Text: UTF8String;
begin
  Result := nil;
  CheckDeadline;
  if FLastId = High(Integer) then RaiseCDPError('CDP command IDs are exhausted');
  Text := WfcBrowserCDPRequest(FLastId + 1, AMethod, AParams, ASessionId);
  CheckJSONBounds(Text, FMaxMessageBytes);
  Inc(FLastId);
  try
    CheckDeadline;
    FSocket.SendText(Text);
    repeat
      CheckDeadline;
      Text := FSocket.ReceiveText;
      CheckDeadline;
      Result := WfcBrowserCDPResponse(Text, FLastId, ASessionId, FMaxMessageBytes);
      { Notifications neither satisfy a command nor renew its deadline. }
      CheckDeadline;
    until Result <> nil;
  except
    on E: EWfcBrowserCDP do
    begin
      FreeAndNil(Result);
      raise;
    end;
    on E: Exception do
    begin
      FreeAndNil(Result);
      RaiseCDPError('CDP transport: ' + Diagnostic(E.Message));
    end;
  end;
end;
end.
