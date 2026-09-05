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
unit wfc_rule_text;

{$mode delphi}{$H+}

interface

uses
  wfc_rule_model;

const
  WFC_RULE_TEXT_VERSION = 1;
  WFC_RULE_MAX_ENCODED_TEXT_LENGTH = 16777216;
  WFC_RULE_MAX_TEXT_LINE_COUNT = 262144;

function EncodeWfcRuleText(const AModel: TWfcRuleModel): String;
function DecodeWfcRuleText(const AText: String): TWfcRuleModel;

implementation

uses
  SysUtils,
  wfc,
  wfc_model,
  wfc_text_codec;

const
  WFC_RULE_TEXT_ARTIFACT = 'WFC rules';
  WFC_RULE_TEXT_HEADER = 'wfcrules=1';
  WFC_RULE_TEXT_FIXED_LINE_COUNT = 6;

type
  TStringParts = array of String;

procedure TextError(const AMessage: String);
begin
  WfcTextError(WFC_RULE_TEXT_ARTIFACT, AMessage);
end;

function ParseInteger(const AText, AField: String): Integer;
begin
  Result := WfcTextParseCanonicalInteger(AText, AField,
    WFC_RULE_TEXT_ARTIFACT);
end;

function ValueAfterPrefix(const ALine, APrefix,
  AField: String): String;
begin
  Result := WfcTextValueAfterPrefix(ALine, APrefix, AField,
    WFC_RULE_TEXT_ARTIFACT);
end;

function CheckedLineCount(const AValueCount,
  ARuleCount: Integer): Integer;
begin
  if (AValueCount < 0) or (ARuleCount < 0) then
    raise ERangeError.Create(
      'WFC rules text counts cannot be negative');
  if AValueCount > (High(Integer) - WFC_RULE_TEXT_FIXED_LINE_COUNT) then
    raise ERangeError.Create('WFC rules text has too many lines');
  Result := WFC_RULE_TEXT_FIXED_LINE_COUNT + AValueCount;
  if ARuleCount > (High(Integer) - Result) then
    raise ERangeError.Create('WFC rules text has too many lines');
  Inc(Result, ARuleCount);
  if Result > WFC_RULE_MAX_TEXT_LINE_COUNT then
    raise ERangeError.Create(
      'canonical WFC rules text exceeds the version-1 line-count limit');
end;

procedure RequireEncodedTextLength(const ALines: TWfcTextLines);
var
  I: Integer;
  LLineLength: SizeInt;
  LTotalLength: Integer;
begin
  LTotalLength := 0;
  for I := 0 to Length(ALines) - 1 do
  begin
    LLineLength := Length(ALines[I]);
    if LLineLength > SizeInt(
        WFC_RULE_MAX_ENCODED_TEXT_LENGTH - LTotalLength - 1) then
      raise ERangeError.Create(
        'canonical WFC rules text exceeds the version-1 length limit');
    Inc(LTotalLength, Integer(LLineLength) + 1);
  end;
end;

procedure PreflightTextEnvelope(const AText: String);
var
  I: SizeInt;
  LLineCount: Integer;
  LTextLength: SizeInt;
begin
  LTextLength := Length(AText);
  if (LTextLength < 0) or
      (LTextLength > SizeInt(WFC_RULE_MAX_ENCODED_TEXT_LENGTH)) then
    TextError('document exceeds the version-1 encoded length limit');
  LLineCount := 0;
  for I := 1 to LTextLength do
    if AText[I] = #10 then
    begin
      if LLineCount = WFC_RULE_MAX_TEXT_LINE_COUNT then
        TextError('document exceeds the version-1 line-count limit');
      Inc(LLineCount);
    end;
end;

function CheckedRuleSlotCount(const AValueCount: Integer): Integer;
const
  DIRECTION_COUNT = Ord(High(TGraphDirection)) + 1;
begin
  if AValueCount < 0 then
    TextError('value count cannot be negative');
  if AValueCount > (High(Integer) div DIRECTION_COUNT) then
    TextError('rule table exceeds the supported range');
  Result := AValueCount * DIRECTION_COUNT;
end;

function DirectionCode(const ADirection: TGraphDirection): String;
begin
  case ADirection of
    gdNorth:
      Result := 'N';
    gdEast:
      Result := 'E';
    gdSouth:
      Result := 'S';
    gdWest:
      Result := 'W';
    gdUp:
      Result := 'U';
    gdDown:
      Result := 'D';
  else
    raise ERangeError.Create('unknown WFC rule direction');
  end;
end;

function ParseDirection(const AText: String): TGraphDirection;
begin
  if AText = 'N' then
    Result := gdNorth
  else if AText = 'E' then
    Result := gdEast
  else if AText = 'S' then
    Result := gdSouth
  else if AText = 'W' then
    Result := gdWest
  else if AText = 'U' then
    Result := gdUp
  else if AText = 'D' then
    Result := gdDown
  else
    TextError('rule direction has an unknown value');
end;

function RuleStateName(const AState: TWfcRuleState): String;
begin
  case AState of
    wrsAllow:
      Result := 'allow';
    wrsDeny:
      Result := 'deny';
  else
    raise ERangeError.Create('unknown WFC rule state');
  end;
end;

function ParseRuleState(const AText: String): TWfcRuleState;
begin
  if AText = 'allow' then
    Result := wrsAllow
  else if AText = 'deny' then
    Result := wrsDeny
  else
    TextError('rule state has an unknown value');
end;

function BooleanText(const AValue: Boolean): String;
begin
  if AValue then
    Result := '1'
  else
    Result := '0';
end;

function ParseBoolean(const AText, AField: String): Boolean;
begin
  if AText = '0' then
    Result := False
  else if AText = '1' then
    Result := True
  else
  begin
    TextError(AField + ' must be 0 or 1');
    Result := False;
  end;
end;

function SplitCommaFields(const AText, AField: String;
  const AMaxFieldCount: Integer): TStringParts;
var
  I: Integer;
  LCount: Integer;
  LIndex: Integer;
  LStart: Integer;
begin
  Result := nil;
  if AMaxFieldCount < 1 then
    raise EArgumentOutOfRangeException.Create(
      'WFC rules text field limit must be positive');
  LCount := 1;
  for I := 1 to Length(AText) do
    if AText[I] = ',' then
    begin
      if LCount = AMaxFieldCount then
        TextError(AField + ' has too many fields');
      Inc(LCount);
    end;
  SetLength(Result, LCount);
  LIndex := 0;
  LStart := 1;
  for I := 1 to Length(AText) do
    if AText[I] = ',' then
    begin
      Result[LIndex] := Copy(AText, LStart, I - LStart);
      Inc(LIndex);
      LStart := I + 1;
    end;
  Result[LIndex] := Copy(AText, LStart,
    Length(AText) - LStart + 1);
end;

function SignatureTextIsCanonical(const AText: String): Boolean;
const
  HEX_DIGITS = '0123456789ABCDEF';
var
  I: Integer;
begin
  if Length(AText) <> 8 then
    Exit(False);
  for I := 1 to Length(AText) do
    if Pos(AText[I], HEX_DIGITS) = 0 then
      Exit(False);
  Result := True;
end;

procedure ParseValueLine(const ALine: String;
  const AExpectedIndex: Integer; out AToken: TWfcModelToken;
  out AWeight: Integer);
var
  LParts: TStringParts;
begin
  LParts := SplitCommaFields(ValueAfterPrefix(ALine, 'v=',
    'value record'), 'value record', 3);
  if Length(LParts) <> 3 then
    TextError('value record must contain exactly three fields');
  if ParseInteger(LParts[0], 'value index') <> AExpectedIndex then
    TextError('value indices must be canonical and contiguous');
  AWeight := ParseInteger(LParts[1], 'value weight');
  AToken := WfcTextDecodeToken(LParts[2], WFC_RULE_TEXT_ARTIFACT);
end;

procedure ParseRuleLine(const ALine: String;
  const AExpectedIndex, AValueCount: Integer; out ARow: TWfcRuleRow);
var
  I: Integer;
  LParts: TStringParts;
  LTargetCount: Integer;
begin
  ARow := Default(TWfcRuleRow);
  LParts := SplitCommaFields(ValueAfterPrefix(ALine, 'r=',
    'rule record'), 'rule record', 6 + WFC_RULE_MAX_VALUE_COUNT);
  if Length(LParts) < 6 then
    TextError('rule record must contain at least six fields');
  if ParseInteger(LParts[0], 'rule index') <> AExpectedIndex then
    TextError('rule indices must be canonical and contiguous');
  ARow.OwnerIndex := ParseInteger(LParts[1], 'rule owner index');
  ARow.Direction := ParseDirection(LParts[2]);
  ARow.State := ParseRuleState(LParts[3]);
  ARow.Required := ParseBoolean(LParts[4], 'rule required flag');
  LTargetCount := ParseInteger(LParts[5], 'rule target count');
  if LTargetCount > AValueCount then
    TextError('rule target count exceeds the value count');
  if LTargetCount > (High(Integer) - 6) then
    TextError('rule target count exceeds the supported range');
  if Length(LParts) <> 6 + LTargetCount then
    TextError('rule target count does not match its fields');
  SetLength(ARow.TargetIndices, LTargetCount);
  for I := 0 to LTargetCount - 1 do
    ARow.TargetIndices[I] := ParseInteger(LParts[6 + I],
      'rule target index');
end;

function EncodeWfcRuleText(const AModel: TWfcRuleModel): String;
var
  I: Integer;
  J: Integer;
  LLine: String;
  LLineIndex: Integer;
  LLines: TWfcTextLines;
begin
  if not Assigned(AModel) then
    raise EArgumentNilException.Create('WFC rule model cannot be nil');
  if CalculateWfcRuleModelSignature(AModel) <> AModel.Signature then
    raise EWfcRuleModel.Create('WFC rule model signature is inconsistent');

  SetLength(LLines, CheckedLineCount(AModel.ValueCount,
    AModel.RuleCount));
  LLineIndex := 0;
  LLines[LLineIndex] := WFC_RULE_TEXT_HEADER;
  Inc(LLineIndex);
  LLines[LLineIndex] := 'rank=' + IntToStr(AModel.Rank);
  Inc(LLineIndex);
  LLines[LLineIndex] := 'values=' + IntToStr(AModel.ValueCount);
  Inc(LLineIndex);
  for I := 0 to AModel.ValueCount - 1 do
  begin
    LLines[LLineIndex] := 'v=' + IntToStr(I) + ',' +
      IntToStr(AModel.WeightAt(I)) + ',' +
      WfcTextEncodeToken(AModel.TokenAt(I), WFC_RULE_TEXT_ARTIFACT);
    Inc(LLineIndex);
  end;
  LLines[LLineIndex] := 'rules=' + IntToStr(AModel.RuleCount);
  Inc(LLineIndex);
  for I := 0 to AModel.RuleCount - 1 do
  begin
    LLine := 'r=' + IntToStr(I) + ',' +
      IntToStr(AModel.RuleOwnerAt(I)) + ',' +
      DirectionCode(AModel.RuleDirectionAt(I)) + ',' +
      RuleStateName(AModel.RuleStateAt(I)) + ',' +
      BooleanText(AModel.RuleRequiredAt(I)) + ',' +
      IntToStr(AModel.RuleTargetCountAt(I));
    for J := 0 to AModel.RuleTargetCountAt(I) - 1 do
      LLine := LLine + ',' + IntToStr(AModel.RuleTargetAt(I, J));
    LLines[LLineIndex] := LLine;
    Inc(LLineIndex);
  end;
  LLines[LLineIndex] := 'signature=' +
    WfcRuleModelSignatureHex(AModel.Signature);
  Inc(LLineIndex);
  LLines[LLineIndex] := 'end';
  RequireEncodedTextLength(LLines);
  Result := WfcTextJoinCanonicalLines(LLines,
    WFC_RULE_TEXT_ARTIFACT);
end;

function DecodeWfcRuleText(const AText: String): TWfcRuleModel;
var
  I: Integer;
  LLineIndex: Integer;
  LLines: TWfcTextLines;
  LModel: TWfcRuleModel;
  LRank: Integer;
  LRuleCount: Integer;
  LRuleSlotCount: Integer;
  LRows: TWfcRuleRows;
  LSignatureText: String;
  LTokens: TWfcModelTokens;
  LValueCount: Integer;
  LWeights: TWfcModelIntegerArray;
  LTotalTargetCount: Integer;
begin
  Result := nil;
  PreflightTextEnvelope(AText);
  WfcTextSplitCanonicalLines(AText, WFC_RULE_TEXT_ARTIFACT,
    LLines);
  if Length(LLines) < WFC_RULE_TEXT_FIXED_LINE_COUNT then
    TextError('document is incomplete');
  LLineIndex := 0;
  if LLines[LLineIndex] <> WFC_RULE_TEXT_HEADER then
    TextError('unsupported or noncanonical format version');
  Inc(LLineIndex);

  LRank := ParseInteger(ValueAfterPrefix(LLines[LLineIndex],
    'rank=', 'rank'), 'rank');
  Inc(LLineIndex);
  LValueCount := ParseInteger(ValueAfterPrefix(LLines[LLineIndex],
    'values=', 'values'), 'values');
  Inc(LLineIndex);
  if LValueCount < 1 then
    TextError('values must be positive');
  if LValueCount > WFC_RULE_MAX_VALUE_COUNT then
    TextError('value count exceeds the version-1 limit');
  LRuleSlotCount := CheckedRuleSlotCount(LValueCount);
  { Values plus the rules field, signature, and end marker must already be
    physically present before allocation. }
  if LValueCount > (Length(LLines) - LLineIndex - 3) then
    TextError('value records exceed the remaining document lines');
  SetLength(LTokens, LValueCount);
  SetLength(LWeights, LValueCount);
  for I := 0 to LValueCount - 1 do
  begin
    ParseValueLine(LLines[LLineIndex], I, LTokens[I], LWeights[I]);
    Inc(LLineIndex);
  end;

  LRuleCount := ParseInteger(ValueAfterPrefix(LLines[LLineIndex],
    'rules=', 'rules'), 'rules');
  Inc(LLineIndex);
  if LRuleCount > WFC_RULE_MAX_RULE_COUNT then
    TextError('rule count exceeds the version-1 limit');
  if LRuleCount > LRuleSlotCount then
    TextError('rule count exceeds the owner/direction table size');
  if LRuleCount > (Length(LLines) - LLineIndex - 2) then
    TextError('rule records exceed the remaining document lines');
  SetLength(LRows, LRuleCount);
  LTotalTargetCount := 0;
  for I := 0 to LRuleCount - 1 do
  begin
    ParseRuleLine(LLines[LLineIndex], I, LValueCount, LRows[I]);
    if Length(LRows[I].TargetIndices) >
        WFC_RULE_MAX_TOTAL_TARGET_COUNT - LTotalTargetCount then
      TextError('rule target count exceeds the version-1 aggregate limit');
    Inc(LTotalTargetCount, Length(LRows[I].TargetIndices));
    Inc(LLineIndex);
  end;

  LSignatureText := ValueAfterPrefix(LLines[LLineIndex],
    'signature=', 'signature');
  Inc(LLineIndex);
  if not SignatureTextIsCanonical(LSignatureText) then
    TextError('signature must be eight uppercase hexadecimal digits');
  if (LLineIndex >= Length(LLines)) or
      (LLines[LLineIndex] <> 'end') then
    TextError('end marker is missing or misplaced');
  Inc(LLineIndex);
  if LLineIndex <> Length(LLines) then
    TextError('trailing data is not permitted');

  LModel := nil;
  try
    try
      LModel := TWfcRuleModel.Create(LRank, LTokens, LWeights, LRows);
    except
      on E: EWfcRuleModel do
        raise EConvertError.Create('invalid ' + WFC_RULE_TEXT_ARTIFACT +
          ' text: ' + E.Message);
    end;
    if WfcRuleModelSignatureHex(LModel.Signature) <> LSignatureText then
      TextError('signature does not match the rule model');
    if EncodeWfcRuleText(LModel) <> AText then
      TextError('document is not in canonical form');
    Result := LModel;
    LModel := nil;
  finally
    LModel.Free;
  end;
end;

end.
