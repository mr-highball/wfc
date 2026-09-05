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
program wfc_text_codec_test;

{$mode delphi}{$H+}

uses
  {$IFDEF PAS2JS}wfc_browser_test_host,{$ENDIF}
  SysUtils,
  wfc_model,
  wfc_text_codec;

type
  TCanonicalParserKind = (cpkCardinal, cpkSignedInteger);
  TTestProcedure = procedure;

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

procedure ExpectError(const AKind: TCanonicalParserKind;
  const AText, AFieldName, AArtifactName, AExpectedMessage,
  ALabel: String);
var
  LActualMessage: String;
  LCorrectClass: Boolean;
  LRaised: Boolean;
begin
  LActualMessage := '';
  LCorrectClass := False;
  LRaised := False;
  try
    case AKind of
      cpkCardinal:
        WfcTextParseCanonicalCardinal(AText, AFieldName,
          AArtifactName);
      cpkSignedInteger:
        WfcTextParseCanonicalSignedInteger(AText, AFieldName,
          AArtifactName);
    end;
  except
    on E: Exception do
    begin
      LRaised := True;
      LCorrectClass := E is EConvertError;
      LActualMessage := E.Message;
    end;
  end;

  Check(LRaised and LCorrectClass and
    (LActualMessage = AExpectedMessage), ALabel);
  if not LRaised then
    WriteLn('    expected EConvertError: ', AExpectedMessage)
  else if not LCorrectClass then
    WriteLn('    wrong exception class: ', LActualMessage)
  else if LActualMessage <> AExpectedMessage then
  begin
    WriteLn('    expected: ', AExpectedMessage);
    WriteLn('    actual:   ', LActualMessage);
  end;
end;

procedure ExpectCardinalError(const AText, ASuffix,
  ALabel: String);
begin
  ExpectError(cpkCardinal, AText, 'seed', 'pipeline bundle',
    'invalid pipeline bundle text: seed ' + ASuffix, ALabel);
end;

procedure ExpectSignedError(const AText, ASuffix,
  ALabel: String);
begin
  ExpectError(cpkSignedInteger, AText, 'offset',
    'pipeline bundle', 'invalid pipeline bundle text: offset ' +
    ASuffix, ALabel);
end;

procedure TestCardinalBoundaries;
begin
  Check(WfcTextParseCanonicalCardinal('0', 'seed',
    'pipeline bundle') = Cardinal(0), 'Cardinal accepts zero');
  Check(WfcTextParseCanonicalCardinal('1', 'seed',
    'pipeline bundle') = Cardinal(1), 'Cardinal accepts one');
  Check(WfcTextParseCanonicalCardinal('9', 'seed',
    'pipeline bundle') = Cardinal(9), 'Cardinal accepts one digit');
  Check(WfcTextParseCanonicalCardinal('10', 'seed',
    'pipeline bundle') = Cardinal(10), 'Cardinal accepts multiple digits');
  Check(WfcTextParseCanonicalCardinal('2147483647', 'seed',
    'pipeline bundle') = Cardinal($7FFFFFFF),
    'Cardinal accepts High(Integer)');
  Check(WfcTextParseCanonicalCardinal('2147483648', 'seed',
    'pipeline bundle') = Cardinal($80000000),
    'Cardinal accepts High(Integer) plus one');
  Check(WfcTextParseCanonicalCardinal('4294967294', 'seed',
    'pipeline bundle') = Cardinal($FFFFFFFE),
    'Cardinal accepts the value below its upper bound');
  Check(WfcTextParseCanonicalCardinal('4294967295', 'seed',
    'pipeline bundle') = Cardinal($FFFFFFFF),
    'Cardinal accepts its complete upper bound');
end;

procedure TestSignedIntegerBoundaries;
begin
  Check(WfcTextParseCanonicalSignedInteger('0', 'offset',
    'pipeline bundle') = 0, 'signed Integer accepts zero');
  Check(WfcTextParseCanonicalSignedInteger('1', 'offset',
    'pipeline bundle') = 1, 'signed Integer accepts one');
  Check(WfcTextParseCanonicalSignedInteger('-1', 'offset',
    'pipeline bundle') = -1, 'signed Integer accepts negative one');
  Check(WfcTextParseCanonicalSignedInteger('10', 'offset',
    'pipeline bundle') = 10, 'signed Integer accepts positive digits');
  Check(WfcTextParseCanonicalSignedInteger('-10', 'offset',
    'pipeline bundle') = -10, 'signed Integer accepts negative digits');
  Check(WfcTextParseCanonicalSignedInteger('2147483646', 'offset',
    'pipeline bundle') = High(Integer) - 1,
    'signed Integer accepts the value below High(Integer)');
  Check(WfcTextParseCanonicalSignedInteger('2147483647', 'offset',
    'pipeline bundle') = High(Integer),
    'signed Integer accepts High(Integer)');
  Check(WfcTextParseCanonicalSignedInteger('-2147483647', 'offset',
    'pipeline bundle') = -High(Integer),
    'signed Integer accepts negative High(Integer)');
  Check(WfcTextParseCanonicalSignedInteger('-2147483648', 'offset',
    'pipeline bundle') = Low(Integer),
    'signed Integer accepts Low(Integer) without negating it');
end;

procedure TestMalformedCardinals;
begin
  ExpectCardinalError('', 'is empty',
    'Cardinal rejects an empty field with exact context');
  ExpectCardinalError('00', 'has a leading zero',
    'Cardinal rejects zero with an extra leading zero');
  ExpectCardinalError('01', 'has a leading zero',
    'Cardinal rejects a nonzero value with a leading zero');
  ExpectCardinalError('+0',
    'is not a canonical unsigned decimal integer',
    'Cardinal rejects a plus sign');
  ExpectCardinalError('-0',
    'is not a canonical unsigned decimal integer',
    'Cardinal rejects a minus sign');
  ExpectCardinalError(' 0',
    'is not a canonical unsigned decimal integer',
    'Cardinal rejects leading whitespace');
  ExpectCardinalError('0 ',
    'has a leading zero',
    'Cardinal rejects trailing whitespace canonically');
  ExpectCardinalError('1.0',
    'is not a canonical unsigned decimal integer',
    'Cardinal rejects decimal punctuation');
  ExpectCardinalError('1a',
    'is not a canonical unsigned decimal integer',
    'Cardinal rejects a trailing non-digit');
  ExpectCardinalError('4294967296',
    'exceeds the supported Cardinal range',
    'Cardinal rejects its upper bound plus one');
  ExpectCardinalError('999999999999999999999999999999999999',
    'exceeds the supported Cardinal range',
    'Cardinal rejects arbitrarily long overflow');
end;

procedure TestMalformedSignedIntegers;
begin
  ExpectSignedError('', 'is empty',
    'signed Integer rejects an empty field with exact context');
  ExpectSignedError('-', 'is not a canonical signed decimal integer',
    'signed Integer rejects a bare minus sign');
  ExpectSignedError('+1', 'is not a canonical signed decimal integer',
    'signed Integer rejects a plus sign');
  ExpectSignedError('00', 'has a leading zero',
    'signed Integer rejects zero with an extra leading zero');
  ExpectSignedError('01', 'has a leading zero',
    'signed Integer rejects a positive leading zero');
  ExpectSignedError('-0', 'is negative zero',
    'signed Integer rejects negative zero explicitly');
  ExpectSignedError('-00', 'has a leading zero',
    'signed Integer rejects repeated negative zero digits');
  ExpectSignedError('-01', 'has a leading zero',
    'signed Integer rejects a negative leading zero');
  ExpectSignedError('--1', 'is not a canonical signed decimal integer',
    'signed Integer rejects two minus signs');
  ExpectSignedError('-+1', 'is not a canonical signed decimal integer',
    'signed Integer rejects a sign after minus');
  ExpectSignedError(' 1', 'is not a canonical signed decimal integer',
    'signed Integer rejects leading whitespace');
  ExpectSignedError('1 ', 'is not a canonical signed decimal integer',
    'signed Integer rejects trailing whitespace');
  ExpectSignedError('1.0', 'is not a canonical signed decimal integer',
    'signed Integer rejects decimal punctuation');
  ExpectSignedError('2147483648',
    'exceeds the supported Integer range',
    'signed Integer rejects High(Integer) plus one');
  ExpectSignedError('-2147483649',
    'exceeds the supported Integer range',
    'signed Integer rejects Low(Integer) minus one');
  ExpectSignedError('999999999999999999999999999999999999',
    'exceeds the supported Integer range',
    'signed Integer rejects arbitrarily long positive overflow');
  ExpectSignedError('-999999999999999999999999999999999999',
    'exceeds the supported Integer range',
    'signed Integer rejects arbitrarily long negative overflow');
end;

function RepeatText(const AText: String; const ACount: Integer): String;
var I: Integer;
begin
  Result := '';
  for I := 1 to ACount do Result := Result + AText;
end;

procedure TestLargeTokensAndUnicode;
var Token, UnicodeToken, Bad: TWfcModelToken; Encoded, Expected: String;
  I: Integer; Rejected: Boolean;
begin
  Token := TWfcModelToken(StringOfChar('x', 60000));
  Encoded := WfcTextEncodeToken(Token, 'large token fixture');
  Check((Length(Encoded) = 60000) and (Encoded = String(Token)),
    'large unreserved token preserves every byte');
  Check(WfcTextDecodeToken(Encoded, 'large token fixture') = Token,
    'large unreserved token roundtrip');
  Token := TWfcModelToken(RepeatText(':% ', 10000));
  Expected := RepeatText('%3A%25%20', 10000);
  Encoded := WfcTextEncodeToken(Token, 'large escaped fixture');
  Check((Length(Encoded) = 90000) and (Encoded = Expected),
    'large reserved token matches independently specified escapes');
  Check(WfcTextDecodeToken(Encoded, 'large escaped fixture') = Token,
    'large reserved token roundtrip');
  UnicodeToken := WfcTextDecodeToken('%C3%A9%F0%9F%8E%B5', 'Unicode fixture');
  Token := '';
  for I := 1 to 8192 do Token := Token + UnicodeToken;
  Expected := RepeatText('%C3%A9%F0%9F%8E%B5', 8192);
  Encoded := WfcTextEncodeToken(Token, 'large Unicode fixture');
  Check((Length(Encoded) = 147456) and (Encoded = Expected),
    'large BMP and supplementary Unicode token has exact UTF8 escapes');
  Check(WfcTextDecodeToken(Encoded, 'large Unicode fixture') = Token,
    'large Unicode token roundtrip preserves surrogate pairs');
  Token := 'AZaz09-._~' + #0 + #10 + ':%';
  Check(WfcTextEncodeToken(Token, 'byte fixture') =
    'AZaz09-._~%00%0A%3A%25', 'unreserved and control-byte canonical spelling unchanged');
  Check(WfcTextEncodeToken('', 'empty fixture') = '', 'empty token encoding remains empty');
  for I := 0 to 3 do
  begin
    {$IFDEF PAS2JS}
    case I of
      0: Bad := Chr($D800);
      1: Bad := Chr($DC00);
      2: Bad := Chr($D800) + 'A';
      3: Bad := Chr($DC00) + Chr($D800);
    end;
    {$ELSE}
    { Populate raw UTF8String bytes. Assigning character literals directly can
      transcode them through the source code page and accidentally make them
      valid UTF8 before the encoder sees this adversarial input. }
    case I of
      0: begin SetLength(Bad, 2); Bad[1] := AnsiChar($C0); Bad[2] := AnsiChar($AF); end;
      1: begin SetLength(Bad, 4); Bad[1] := AnsiChar($F4); Bad[2] := AnsiChar($90);
        Bad[3] := AnsiChar($80); Bad[4] := AnsiChar($80); end;
      2: begin SetLength(Bad, 3); Bad[1] := AnsiChar($ED); Bad[2] := AnsiChar($A0);
        Bad[3] := AnsiChar($80); end;
      3: begin SetLength(Bad, 2); Bad[1] := AnsiChar($E2); Bad[2] := AnsiChar($82); end;
    end;
    {$ENDIF}
    Rejected := False;
    try WfcTextEncodeToken(Bad, 'malformed Unicode fixture');
    except on E: EConvertError do Rejected := True; end;
    Check(Rejected, 'malformed host Unicode still rejects ' + IntToStr(I));
  end;
end;

procedure TestLargeCanonicalLines;
var Lines, Parsed: TWfcTextLines; Joined, Expected: String; I: Integer;
begin
  SetLength(Lines, 3);
  Lines[0] := StringOfChar('a', 60000);
  Lines[1] := RepeatText('%3A%25%20', 10000);
  Lines[2] := 'terminal';
  Expected := Lines[0] + #10 + Lines[1] + #10 + Lines[2] + #10;
  Joined := WfcTextJoinCanonicalLines(Lines, 'large lines fixture');
  Check((Length(Joined) = 150011) and (Joined = Expected),
    'large multiline document preserves exact boundaries and final LF');
  WfcTextSplitCanonicalLines(Joined, 'large lines fixture', Parsed);
  Check(Length(Parsed) = Length(Lines), 'large line join/split preserves cardinality');
  for I := 0 to High(Lines) do
    Check(Parsed[I] = Lines[I], 'large line content and borrowed input unchanged ' + IntToStr(I));
  Lines := nil;
  Check(WfcTextJoinCanonicalLines(Lines, 'empty lines fixture') = '',
    'empty line vector does not introduce an LF');
  SetLength(Lines, 1); Lines[0] := '';
  Check(WfcTextJoinCanonicalLines(Lines, 'blank line fixture') = #10,
    'join retains historical blank-line behavior; validation remains separate');
end;

begin
  WriteLn('WFC text-codec numeric conformance suite');
  WriteLn('========================================');
  RunTest('canonical Cardinal boundaries', TestCardinalBoundaries);
  RunTest('canonical signed Integer boundaries',
    TestSignedIntegerBoundaries);
  RunTest('malformed Cardinal forms and error context',
    TestMalformedCardinals);
  RunTest('malformed signed Integer forms and error context',
    TestMalformedSignedIntegers);
  RunTest('large canonical tokens and unchanged Unicode validation',
    TestLargeTokensAndUnicode);
  RunTest('large canonical line assembly', TestLargeCanonicalLines);
  WriteLn('========================================');
  WriteLn('Checks: ', GCheckCount, '  Failures: ', GFailureCount);
  if GFailureCount <> 0 then
  begin
    {$IFDEF PAS2JS}
    raise Exception.CreateFmt('%d text-codec checks failed',
      [GFailureCount]);
    {$ELSE}
    Halt(1);
    {$ENDIF}
  end;
end.
