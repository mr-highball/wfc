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
program wfc_rule_text_test;

{$mode delphi}{$H+}

uses
  {$IFDEF PAS2JS}wfc_browser_test_host,{$ENDIF}
  SysUtils,
  wfc,
  wfc_model,
  wfc_rule_model,
  wfc_rule_text;

type
  TTestProcedure = procedure;

const
  GOLDEN_RULE_TEXT =
    'wfcrules=1'#10 +
    'rank=1'#10 +
    'values=2'#10 +
    'v=0,2,A'#10 +
    'v=1,3,B'#10 +
    'rules=4'#10 +
    'r=0,0,E,allow,1,1,1'#10 +
    'r=1,0,W,deny,0,0'#10 +
    'r=2,1,E,deny,0,0'#10 +
    'r=3,1,W,allow,1,1,0'#10 +
    'signature=5F6B1DDF'#10 +
    'end'#10;

var
  GCheckCount: Integer = 0;
  GFailureCount: Integer = 0;

procedure Check(const ACondition: Boolean; const AMessage: String);
begin
  Inc(GCheckCount);
  if ACondition then
    WriteLn('  [PASS] ', AMessage)
  else
  begin
    Inc(GFailureCount);
    WriteLn('  [FAIL] ', AMessage);
  end;
end;

procedure RunTest(const AName: String; const ATest: TTestProcedure);
begin
  WriteLn('[TEST] ', AName);
  try
    ATest;
  except
    on E: Exception do
    begin
      Inc(GFailureCount);
      WriteLn('  [EXCEPTION] ', E.ClassName, ': ', E.Message);
    end;
  end;
end;

function TokensOf(const AValues: array of TWfcModelToken): TWfcModelTokens;
var
  I: Integer;
begin
  Result := nil;
  SetLength(Result, Length(AValues));
  for I := 0 to Length(AValues) - 1 do
    Result[I] := AValues[I];
end;

function IntegersOf(const AValues: array of Integer):
  TWfcModelIntegerArray;
var
  I: Integer;
begin
  Result := nil;
  SetLength(Result, Length(AValues));
  for I := 0 to Length(AValues) - 1 do
    Result[I] := AValues[I];
end;

function MusicalNoteToken: TWfcModelToken;
begin
  {$IFDEF PAS2JS}
  Result := TWfcModelToken(Chr($266B));
  {$ELSE}
  Result := TWfcModelToken(UTF8Encode(
    UnicodeString(WideChar($266B))));
  {$ENDIF}
end;

function RowsOf(const AValues: array of TWfcRuleRow): TWfcRuleRows;
var
  I: Integer;
begin
  Result := nil;
  SetLength(Result, Length(AValues));
  for I := 0 to Length(AValues) - 1 do
    Result[I] := AValues[I];
end;

function NewFixture: TWfcRuleModel;
begin
  Result := TWfcRuleModel.Create(1, TokensOf(['A', 'B']),
    IntegersOf([2, 3]), RowsOf([
      MakeWfcAllowRuleRow(0, gdEast, True, IntegersOf([1])),
      MakeWfcDenyRuleRow(0, gdWest),
      MakeWfcDenyRuleRow(1, gdEast),
      MakeWfcAllowRuleRow(1, gdWest, True, IntegersOf([0]))
    ]));
end;

function ReplaceOnce(const AText, AOld, ANew: String): String;
var
  LPosition: Integer;
begin
  LPosition := Pos(AOld, AText);
  if LPosition = 0 then
    raise Exception.Create('test mutation source was not found: ' + AOld);
  if Pos(AOld, Copy(AText, LPosition + Length(AOld),
      Length(AText) - LPosition - Length(AOld) + 1)) <> 0 then
    raise Exception.Create('test mutation source was not unique: ' + AOld);
  Result := Copy(AText, 1, LPosition - 1) + ANew +
    Copy(AText, LPosition + Length(AOld),
      Length(AText) - LPosition - Length(AOld) + 1);
end;

procedure ExpectDecodeError(const AText, AExpected, ALabel: String);
var
  LCorrectClass: Boolean;
  LMessage: String;
  LModel: TWfcRuleModel;
  LRaised: Boolean;
begin
  LCorrectClass := False;
  LMessage := '';
  LModel := nil;
  LRaised := False;
  try
    try
      LModel := DecodeWfcRuleText(AText);
    except
      on E: Exception do
      begin
        LRaised := True;
        LCorrectClass := E is EConvertError;
        LMessage := E.Message;
      end;
    end;
  finally
    LModel.Free;
  end;
  Check(LRaised and LCorrectClass and (Pos(AExpected, LMessage) > 0),
    ALabel);
  if not LRaised then
    WriteLn('    expected EConvertError containing: ', AExpected)
  else if not LCorrectClass then
    WriteLn('    wrong exception class: ', LMessage)
  else if Pos(AExpected, LMessage) = 0 then
    WriteLn('    actual: ', LMessage);
end;

procedure TestGoldenRoundTrip;
var
  LDecoded: TWfcRuleModel;
  LEncoded: String;
  LFixture: TWfcRuleModel;
begin
  LFixture := NewFixture;
  LDecoded := nil;
  try
    LEncoded := EncodeWfcRuleText(LFixture);
    Check(WFC_RULE_TEXT_VERSION = 1,
      'the canonical rule text format publishes version one');
    Check(LEncoded = GOLDEN_RULE_TEXT,
      'the fixture encodes to the exact wfcrules=1 golden document');
    Check(LFixture.Signature = TWfcRuleModelSignature($5F6B1DDF),
      'the semantic signature has a portable golden value');
    LDecoded := DecodeWfcRuleText(LEncoded);
    Check((LDecoded.Rank = 1) and (LDecoded.ValueCount = 2) and
      (LDecoded.RuleCount = 4) and (LDecoded.TokenAt(0) = 'A') and
      (LDecoded.WeightAt(1) = 3) and
      (LDecoded.RuleDirectionAt(3) = gdWest) and
      LDecoded.RuleRequiredAt(3) and
      (LDecoded.RuleTargetAt(3, 0) = 0),
      'decoding reconstructs the complete immutable semantic model');
    Check(EncodeWfcRuleText(LDecoded) = LEncoded,
      'decoded rule text re-encodes byte-for-byte');
  finally
    LDecoded.Free;
    LFixture.Free;
  end;
end;

procedure TestUnicodeAndReservedTokens;
var
  LDecoded: TWfcRuleModel;
  LEncoded: String;
  LModel: TWfcRuleModel;
begin
  LModel := TWfcRuleModel.Create(2, TokensOf([',',
    MusicalNoteToken, 'space token', '100%']),
    IntegersOf([1, 2, 3, 4]), nil);
  LDecoded := nil;
  try
    LEncoded := EncodeWfcRuleText(LModel);
    Check((Pos('v=0,1,%2C'#10, LEncoded) > 0) and
      (Pos('v=1,2,%E2%99%AB'#10, LEncoded) > 0) and
      (Pos('v=2,3,space%20token'#10, LEncoded) > 0) and
      (Pos('v=3,4,100%25'#10, LEncoded) > 0),
      'reserved bytes and Unicode scalars use canonical percent encoding');
    LDecoded := DecodeWfcRuleText(LEncoded);
    Check((LDecoded.TokenAt(0) = ',') and
      (LDecoded.TokenAt(1) = MusicalNoteToken) and
      (LDecoded.TokenAt(2) = 'space token') and
      (LDecoded.TokenAt(3) = '100%'),
      'canonical tokens decode identically on native and pas2js');
  finally
    LDecoded.Free;
    LModel.Free;
  end;
end;

procedure TestDocumentShapeRejections;
begin
  ExpectDecodeError('', 'document is empty',
    'an empty document is rejected');
  ExpectDecodeError(StringReplace(GOLDEN_RULE_TEXT, #10, #13#10,
    [rfReplaceAll]), 'CR is not permitted', 'CRLF is rejected');
  ExpectDecodeError(Copy(GOLDEN_RULE_TEXT, 1,
    Length(GOLDEN_RULE_TEXT) - 1), 'must end with LF',
    'a missing final LF is rejected');
  ExpectDecodeError(ReplaceOnce(GOLDEN_RULE_TEXT, 'rank=1'#10,
    'rank=1'#10#10), 'blank lines are not permitted',
    'blank lines are rejected');
  ExpectDecodeError(ReplaceOnce(GOLDEN_RULE_TEXT, 'wfcrules=1',
    'wfcrules=2'), 'unsupported or noncanonical format version',
    'an unknown format version is rejected');
  ExpectDecodeError(ReplaceOnce(GOLDEN_RULE_TEXT, 'rank=1'#10 +
    'values=2', 'values=2'#10 + 'rank=1'), 'expected rank',
    'fixed fields cannot be reordered');
  ExpectDecodeError(GOLDEN_RULE_TEXT + 'extra'#10,
    'trailing data is not permitted', 'trailing data is rejected');
  ExpectDecodeError(ReplaceOnce(GOLDEN_RULE_TEXT, 'end'#10,
    'finish'#10), 'end marker is missing or misplaced',
    'the exact end marker is required');
end;

procedure TestVersionedEnvelopeLimits;
begin
  Check((WFC_RULE_MAX_ENCODED_TEXT_LENGTH = 16777216) and
    (WFC_RULE_MAX_TEXT_LINE_COUNT = 262144),
    'version-one rule-text envelope limits are exact public constants');
  ExpectDecodeError(StringOfChar('x',
    WFC_RULE_MAX_ENCODED_TEXT_LENGTH + 1),
    'encoded length limit',
    'oversized rule text is rejected before line splitting');
  ExpectDecodeError(StringOfChar(#10,
    WFC_RULE_MAX_TEXT_LINE_COUNT + 1),
    'line-count limit',
    'excessive rule-text newlines are rejected before line splitting');
  ExpectDecodeError(ReplaceOnce(GOLDEN_RULE_TEXT, 'values=2',
    'values=1025'), 'value count exceeds the version-1 limit',
    'serialized value cardinality is rejected before allocation');
  ExpectDecodeError(ReplaceOnce(GOLDEN_RULE_TEXT, 'rules=4',
    'rules=6145'), 'rule count exceeds the version-1 limit',
    'serialized rule cardinality is rejected before allocation');
  ExpectDecodeError(ReplaceOnce(GOLDEN_RULE_TEXT,
    'r=0,0,E,allow,1,1,1',
    'r=0' + StringOfChar(',', 6 + WFC_RULE_MAX_VALUE_COUNT)),
    'too many fields',
    'a comma-dense rule record is bounded before field allocation');
end;

procedure TestCountAndValueRejections;
begin
  ExpectDecodeError(ReplaceOnce(GOLDEN_RULE_TEXT, 'rank=1', 'rank=01'),
    'leading zero', 'rank rejects leading zeros');
  ExpectDecodeError(ReplaceOnce(GOLDEN_RULE_TEXT, 'rank=1', 'rank=4'),
    'rank must be 1, 2, or 3', 'unsupported ranks fail semantic validation');
  ExpectDecodeError(ReplaceOnce(GOLDEN_RULE_TEXT, 'values=2', 'values=0'),
    'values must be positive', 'zero values are rejected before allocation');
  ExpectDecodeError(ReplaceOnce(GOLDEN_RULE_TEXT, 'values=2',
    'values=2147483647'), 'value count exceeds the version-1 limit',
    'an impossible value count fails before allocation');
  ExpectDecodeError(ReplaceOnce(GOLDEN_RULE_TEXT, 'values=2', 'values=99'),
    'value records exceed the remaining document lines',
    'a declared value count is bounded by physical records');
  ExpectDecodeError(ReplaceOnce(GOLDEN_RULE_TEXT, 'v=0,2,A', 'v=1,2,A'),
    'indices must be canonical and contiguous',
    'value indices must be contiguous');
  ExpectDecodeError(ReplaceOnce(GOLDEN_RULE_TEXT, 'v=0,2,A', 'v=0,0,A'),
    'weight must be positive', 'zero serialized weight is rejected');
  ExpectDecodeError(ReplaceOnce(GOLDEN_RULE_TEXT, 'v=0,2,A', 'v=0,2,%41'),
    'unnecessarily escapes', 'unnecessary token escapes are rejected');
  ExpectDecodeError(ReplaceOnce(GOLDEN_RULE_TEXT, 'v=0,2,A', 'v=0,2,%ff'),
    'uppercase hexadecimal', 'lowercase token escapes are rejected');
  ExpectDecodeError(ReplaceOnce(GOLDEN_RULE_TEXT, 'v=0,2,A', 'v=0,2,%FF'),
    'invalid UTF-8', 'malformed UTF-8 tokens are rejected');
  ExpectDecodeError(ReplaceOnce(GOLDEN_RULE_TEXT, 'v=1,3,B', 'v=1,3,A'),
    'tokens must be unique', 'duplicate serialized tokens are rejected');
end;

procedure TestRuleRecordRejections;
begin
  ExpectDecodeError(ReplaceOnce(GOLDEN_RULE_TEXT, 'rules=4', 'rules=13'),
    'rule count exceeds the owner/direction table size',
    'rule count is bounded by the finite owner/direction table');
  ExpectDecodeError(ReplaceOnce(GOLDEN_RULE_TEXT, 'rules=4', 'rules=5'),
    'rule records exceed the remaining document lines',
    'rule count is bounded by physical records');
  ExpectDecodeError(ReplaceOnce(GOLDEN_RULE_TEXT,
    'r=0,0,E,allow,1,1,1', 'r=1,0,E,allow,1,1,1'),
    'indices must be canonical and contiguous',
    'rule record indices must be contiguous');
  ExpectDecodeError(ReplaceOnce(GOLDEN_RULE_TEXT,
    'r=0,0,E,allow,1,1,1', 'r=0,0,e,allow,1,1,1'),
    'direction has an unknown value',
    'direction codes are case-sensitive');
  ExpectDecodeError(ReplaceOnce(GOLDEN_RULE_TEXT,
    'r=0,0,E,allow,1,1,1', 'r=0,0,E,ALLOW,1,1,1'),
    'state has an unknown value', 'rule states are case-sensitive');
  ExpectDecodeError(ReplaceOnce(GOLDEN_RULE_TEXT,
    'r=0,0,E,allow,1,1,1', 'r=0,0,E,allow,2,1,1'),
    'required flag must be 0 or 1',
    'required metadata has one canonical Boolean encoding');
  ExpectDecodeError(ReplaceOnce(GOLDEN_RULE_TEXT,
    'r=0,0,E,allow,1,1,1', 'r=0,0,E,allow,1,2,1'),
    'target count does not match', 'missing target fields are rejected');
  ExpectDecodeError(ReplaceOnce(GOLDEN_RULE_TEXT,
    'r=0,0,E,allow,1,1,1', 'r=0,0,E,allow,1,3,0,1,2'),
    'target count exceeds the value count',
    'target count is bounded before target allocation');
  ExpectDecodeError(ReplaceOnce(GOLDEN_RULE_TEXT,
    'r=0,0,E,allow,1,1,1', 'r=0,0,E,allow,1,1,01'),
    'leading zero', 'target indices reject leading zeros');
  ExpectDecodeError(ReplaceOnce(GOLDEN_RULE_TEXT,
    'r=0,0,E,allow,1,1,1', 'r=0,2,E,allow,1,1,1'),
    'owner index is out of range', 'owner indices are range checked');
  ExpectDecodeError(ReplaceOnce(GOLDEN_RULE_TEXT,
    'r=1,0,W,deny,0,0', 'r=1,0,W,deny,1,0'),
    'deny rule cannot be required',
    'deny records reject required metadata');
  ExpectDecodeError(ReplaceOnce(GOLDEN_RULE_TEXT,
    'r=1,0,W,deny,0,0', 'r=1,0,W,deny,0,1,0'),
    'deny rule cannot contain targets',
    'deny records reject target lists');
  ExpectDecodeError(ReplaceOnce(GOLDEN_RULE_TEXT,
    'r=0,0,E,allow,1,1,1', 'r=0,0,E,allow,1,0'),
    'allow rule must contain at least one target',
    'finite allow records cannot encode an empty wildcard');
  ExpectDecodeError(ReplaceOnce(GOLDEN_RULE_TEXT,
    'r=3,1,W,allow,1,1,0', 'r=3,1,W,deny,0,0'),
    'no reciprocal target', 'missing reciprocal support is rejected');
  ExpectDecodeError(ReplaceOnce(GOLDEN_RULE_TEXT,
    'r=3,1,W,allow,1,1,0', 'r=3,1,W,allow,0,1,0'),
    'required metadata does not match',
    'required metadata must already be reciprocally closed');
  ExpectDecodeError(ReplaceOnce(GOLDEN_RULE_TEXT,
    'r=0,0,E,allow,1,1,1'#10 + 'r=1,0,W,deny,0,0',
    'r=0,0,W,deny,0,0'#10 + 'r=1,0,E,allow,1,1,1'),
    'strictly ordered', 'rule rows cannot be reordered');
end;

procedure TestSignatureRejections;
begin
  ExpectDecodeError(ReplaceOnce(GOLDEN_RULE_TEXT,
    'signature=5F6B1DDF', 'signature=5f6b1ddf'),
    'eight uppercase hexadecimal digits',
    'lowercase signatures are noncanonical');
  ExpectDecodeError(ReplaceOnce(GOLDEN_RULE_TEXT,
    'signature=5F6B1DDF', 'signature=00000000'),
    'signature does not match', 'signature tampering is detected');
  ExpectDecodeError(ReplaceOnce(GOLDEN_RULE_TEXT,
    'v=0,2,A', 'v=0,4,A'), 'signature does not match',
    'semantic payload tampering is detected independently of syntax');
end;

procedure TestNilEncode;
var
  LMessage: String;
  LRaised: Boolean;
begin
  LMessage := '';
  LRaised := False;
  try
    EncodeWfcRuleText(nil);
  except
    on E: Exception do
    begin
      LRaised := True;
      LMessage := E.Message;
    end;
  end;
  Check(LRaised and (Pos('cannot be nil', LMessage) > 0),
    'the encoder rejects a nil model');
end;

begin
  WriteLn('WFC generic rule-text conformance suite');
  WriteLn('=======================================');
  RunTest('exact golden and round trip', @TestGoldenRoundTrip);
  RunTest('Unicode and reserved tokens', @TestUnicodeAndReservedTokens);
  RunTest('canonical document shape', @TestDocumentShapeRejections);
  RunTest('versioned envelope limits', @TestVersionedEnvelopeLimits);
  RunTest('count and value rejection matrix', @TestCountAndValueRejections);
  RunTest('rule record rejection matrix', @TestRuleRecordRejections);
  RunTest('signature verification', @TestSignatureRejections);
  RunTest('nil encode preflight', @TestNilEncode);
  WriteLn('=======================================');
  WriteLn('Checks: ', GCheckCount, '  Failures: ', GFailureCount);
  if GFailureCount <> 0 then
  begin
    {$IFDEF PAS2JS}
    raise Exception.CreateFmt('%d rule-text checks failed',
      [GFailureCount]);
    {$ELSE}
    Halt(1);
    {$ENDIF}
  end;
end.
