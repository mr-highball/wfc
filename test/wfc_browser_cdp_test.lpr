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
program wfc_browser_cdp_test;
{$mode delphi}{$H+}
{$IFDEF PAS2JS}{$FATAL this protocol test requires the native target}{$ENDIF}
uses SysUtils, fpjson, jsonparser, wfc_browser_cdp;
var Checks: Integer;

procedure Check(ACondition: Boolean; const AMessage: String);
begin
  Inc(Checks);
  if not ACondition then raise Exception.Create(AMessage);
end;

procedure Reject(const AText: UTF8String; const ASessionId: String = '';
  AMaxBytes: Integer = 65536; AExpectedId: Integer = 1);
var Value: TJSONObject; Raised: Boolean;
begin
  Value := nil;
  Raised := False;
  try
    Value := WfcBrowserCDPResponse(AText, AExpectedId, ASessionId, AMaxBytes);
  except
    on E: EWfcBrowserCDPCommand do
      raise Exception.Create('malformed response must not look like a command error');
    on E: EWfcBrowserCDP do Raised := True;
  end;
  Value.Free;
  Check(Raised, 'malformed CDP reply must reject');
end;

procedure TestRequests;
var Params, Nested, Request: TJSONObject; Text, Before: UTF8String; Raised: Boolean;
begin
  Params := TJSONObject.Create;
  try
    Nested := TJSONObject.Create;
    Nested.Add('value', UTF8String('quotes " and slash \ and line' + #10));
    Params.Add('nested', Nested);
    Before := Params.AsJSON;
    Text := WfcBrowserCDPRequest(1, 'Target.createTarget', Params, '');
    Request := TJSONObject(GetJSON(Text));
    try
      Check(Request.Integers['id'] = 1, 'positive request ID');
      Check(Request.Strings['method'] = 'Target.createTarget', 'method preserved');
      Check(Request.Find('sessionId') = nil, 'browser command omits session ID');
      Check(Request.Objects['params'].AsJSON = Before, 'structured params preserved');
      Request.Objects['params'].Objects['nested'].Strings['value'] := 'changed';
    finally
      Request.Free;
    end;
    Check(Params.AsJSON = Before, 'params remain owned and unchanged by caller');
    Text := WfcBrowserCDPRequest(2, 'DOM.getDocument', nil, 'ABC-123');
    Request := TJSONObject(GetJSON(Text));
    try
      Check(Request.Integers['id'] = 2, 'next request ID preserved');
      Check(Request.Strings['sessionId'] = 'ABC-123', 'flattened session serialized');
      Check(Request.Find('params') = nil, 'nil params omitted');
    finally
      Request.Free;
    end;
    Raised := False;
    try Text := WfcBrowserCDPRequest(0, 'DOM.getDocument', nil, '');
    except on EWfcBrowserCDP do Raised := True; end;
    Check(Raised, 'zero request ID rejected');
    Raised := False;
    try Text := WfcBrowserCDPRequest(1, 'DOM.' + #10 + 'getDocument', nil, '');
    except on EWfcBrowserCDP do Raised := True; end;
    Check(Raised, 'malformed method rejected');
  finally
    Params.Free;
  end;
end;

procedure TestReplies;
var Value: TJSONObject; Text, Nested: UTF8String; Hex: String; I: Integer;
begin
  Value := WfcBrowserCDPResponse('{"id":1,"result":{"targetId":"one"}}', 1, '', 1000);
  try
    Check(Value.Strings['targetId'] = 'one', 'matching browser response extracted');
    Value.Add('stillOwned', True);
    Check(Value.Booleans['stillOwned'], 'result survives envelope destruction');
  finally
    Value.Free;
  end;
  Value := WfcBrowserCDPResponse('{"id":2,"sessionId":"ABC","result":{}}', 2, 'ABC', 1000);
  try Check((Value <> nil) and (Value.Count = 0), 'empty object is a successful result');
  finally Value.Free; end;
  Value := WfcBrowserCDPResponse('{"id":1,"result":{"outerHTML":"caf\u00e9 \ud83c\udfb5"}}', 1, '', 1000);
  try
    Text := Value.Strings['outerHTML'];
    Hex := '';
    for I := 1 to Length(Text) do Hex := Hex + IntToHex(Ord(Text[I]), 2);
    Check((Length(Text) = 10) and (Ord(Text[4]) = $C3) and
      (Ord(Text[5]) = $A9) and (Ord(Text[7]) = $F0) and
      (Ord(Text[8]) = $9F) and (Ord(Text[9]) = $8E) and (Ord(Text[10]) = $B5),
      'DOM text retains UTF-8 including a surrogate pair: ' + Hex);
  finally Value.Free; end;
  Value := WfcBrowserCDPResponse('{"method":"Target.attachedToTarget","params":{"sessionId":"ABC"}}', 1, '', 1000);
  Check(Value = nil, 'notification before reply is not a response');
  Value := WfcBrowserCDPResponse('{"method":"DOM.documentUpdated","sessionId":"OTHER"}', 1, 'ABC', 1000);
  Check(Value = nil, 'other-session notification is not a response');
  Value := WfcBrowserCDPResponse('{"method":"DOM.documentUpdated"}', 1, '', 1000);
  Check(Value = nil, 'parameterless notification accepted');
  Reject('{"id":2,"result":{}}');
  Reject('{"id":0,"result":{}}');
  Reject('{"id":-1,"result":{}}');
  Reject('{"id":1.0,"result":{}}');
  Reject('{"id":1e0,"result":{}}');
  Reject('{"id":"1","result":{}}');
  Reject('{"id":true,"result":{}}');
  Reject('{"id":null,"result":{}}');
  Reject('{"id":18446744073709551615,"result":{}}');
  Reject('{"id":1,"sessionId":"ABC","result":{}}');
  Reject('{"id":1,"result":{}}', 'ABC');
  Reject('{"id":1,"sessionId":"other","result":{}}', 'ABC');
  Reject('{"id":1,"sessionId":null,"result":{}}');
  Reject('{"id":1,"sessionId":"","result":{}}');
  Reject('{"id":1,"result":null}');
  Reject('{"id":1,"result":[]}');
  Reject('{"id":1}');
  Reject('{"id":1,"result":{},"error":{"code":-1,"message":"bad"}}');
  Reject('{"id":1,"method":"DOM.updated","result":{}}');
  Reject('{"id":1,"params":{},"result":{}}');
  Reject('{"method":"DOM.updated","result":{}}');
  Reject('{"method":"DOM.updated","params":[]}');
  Reject('{"method":"","params":{}}');
  Reject('{"method":null}');
  Reject('{"result":{}}');
  Reject('{"id":1,"error":null}');
  Reject('{"id":1,"error":{"code":-32000}}');
  Reject('{"id":1,"error":{"code":-32000.0,"message":"bad"}}');
  Reject('{"id":1,"error":{"code":-32000,"message":null}}');
  Reject('{"id":2,"error":{"code":-32000,"message":"bad"}}');
  Reject('{"id":1,"error":{"code":-32000,"message":"bad"}}', 'ABC');
  Reject(''); Reject('{}'); Reject('null'); Reject('[]');
  Reject('{"id":1,"result":{}} {}');
  Reject('{"id":1,"result":{},}');
  Reject('{"id":1,/*comment*/"result":{}}');
  Reject('{"id":1,"id":1,"result":{}}');
  Reject('{"id":1,"result":{"same":1,"same":2}}');
  Reject('{"id":1,"result":{}}', '', 5);
  Reject('{"id":1,"result":{}}', '', 0);
  Reject('{"id":1,"result":{}}', '', 1000, 0);
  Nested := '{}';
  for I := 1 to WFC_BROWSER_CDP_MAX_JSON_DEPTH do Nested := '[' + Nested + ']';
  Reject('{"id":1,"result":{"deep":' + Nested + '}}');
  Text := '{"id":1,"result":{"text":"' + StringOfChar('[', 100) +
    '\"escaped quote and \\ slash"}}';
  Value := WfcBrowserCDPResponse(Text, 1, '', 1000);
  try Check(Length(Value.Strings['text']) > 100, 'string brackets and escapes do not count as nesting');
  finally Value.Free; end;
end;

procedure TestCommandErrors;
var Envelope, ErrorObject, Value: TJSONObject; Text: UTF8String;
  I: Integer; Raised: Boolean;
begin
  Envelope := TJSONObject.Create;
  try
    Envelope.Add('id', 1);
    ErrorObject := TJSONObject.Create;
    ErrorObject.Add('code', -32000);
    ErrorObject.Add('message', UTF8String('Could not find node with given id' +
      #10 + '::error::injected' + #27 + StringOfChar('x', 5000)));
    Envelope.Add('error', ErrorObject);
    Text := Envelope.AsJSON;
  finally Envelope.Free; end;
  Raised := False;
  Value := nil;
  try
    Value := WfcBrowserCDPResponse(Text, 1, '', 10000);
  except
    on E: EWfcBrowserCDPCommand do
    begin
      Raised := True;
      Check(E.Code = -32000, 'valid protocol error exposes numeric code');
      Check(Pos('Could not find node with given id', E.CommandMessage) = 1,
        'command error preserves known printable diagnostic');
      Check(Length(E.CommandMessage) <= WFC_BROWSER_CDP_MAX_DIAGNOSTIC_BYTES,
        'remote error diagnostic is bounded');
      Check(Pos('...', E.CommandMessage) > 0, 'error truncation is explicit');
      Check(Pos('\x0A::error::injected\x1B', E.CommandMessage) > 0,
        'remote error control characters are escaped');
      for I := 1 to Length(E.Message) do
        Check(E.Message[I] in [#32..#126], 'protocol diagnostic is printable ASCII');
    end;
  end;
  Value.Free;
  Check(Raised, 'matching CDP error is not a successful result');
end;

function Bytes(const AValues: array of Byte): UTF8String;
var I: Integer;
begin
  SetLength(Result, Length(AValues));
  for I := 0 to High(AValues) do Result[I + 1] := AnsiChar(AValues[I]);
end;

procedure TestStrings;
var Value: TJSONObject; Text: UTF8String;
  procedure StringEquals(const AJSON, AExpected: UTF8String);
  begin
    Value := WfcBrowserCDPResponse('{"id":1,"result":{"value":' + AJSON + '}}', 1, '', 1000);
    try Check(Value.Strings['value'] = AExpected, 'JSON string decoding preserves exact bytes');
    finally Value.Free; end;
  end;
begin
  StringEquals('"\u65e5\u672c"', Bytes([$E6, $97, $A5, $E6, $9C, $AC]));
  StringEquals('"\u0000\u0001\b\f\n\r\t\u001f\u007f"',
    Bytes([0, 1, 8, 12, 10, 13, 9, 31, 127]));
  StringEquals('"literal \\u00e9 \/ \\ \""', 'literal \u00e9 / \ "');
  StringEquals('""', '');
  StringEquals('"\u0080\u07ff\u0800\uffff\ud800\udc00"',
    Bytes([$C2, $80, $DF, $BF, $E0, $A0, $80, $EF, $BF, $BF, $F0, $90, $80, $80]));
  Text := Bytes([$C3, $A9, $20, $F0, $9F, $8E, $B5]);
  StringEquals('"' + Text + '"', Text);
  Value := WfcBrowserCDPResponse('{"id":1,"result":{"":"empty key","null\u0000key":"nul"}}', 1, '', 1000);
  try
    Check(Value.Strings[''] = 'empty key', 'empty JSON member name retained');
    Check(Value.Strings[UTF8String('null' + #0 + 'key')] = 'nul', 'NUL in JSON member name retained');
  finally Value.Free; end;
  Reject('{"id":1,"\u0069d":1,"result":{}}');
  Reject('{"id":1,"result":{"\u0061":1,"a":2}}');
  Reject('{"id":1,"result":{"caf\u00e9":1,"caf' + Bytes([$C3, $A9]) + '":2}}');
  Reject('{"id":1,"result":{"value":"\ud800"}}');
  Reject('{"id":1,"result":{"value":"\udc00"}}');
  Reject('{"id":1,"result":{"value":"\ud800\u0041"}}');
  Reject('{"id":1,"result":{"value":"\udc00\ud800"}}');
  Reject('{"id":1,"result":{"value":"\u00xz"}}');
  Reject('{"id":1,"result":{"value":"\u00"}}');
  Reject('{"id":1,"result":{"value":"\x41"}}');
  Reject('{"id":1,"result":{"value":"' + #10 + '"}}');
  Reject('{"id":1,"result":{"value":"' + Bytes([$C0, $80]) + '"}}');
  Reject('{"id":1,"result":{"value":"' + Bytes([$ED, $A0, $80]) + '"}}');
  Reject('{"id":1,"result":{"value":"' + Bytes([$F4, $90, $80, $80]) + '"}}');
  Reject('{"id":1,"result":{"value":"' + Bytes([$E9, $20]) + '"}}');
end;

begin
  Checks := 0;
  TestRequests;
  TestReplies;
  TestCommandErrors;
  TestStrings;
  WriteLn('Checks: ', Checks, ', Failures: 0');
end.
