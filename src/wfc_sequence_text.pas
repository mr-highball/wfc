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
unit wfc_sequence_text;

{$mode delphi}{$H+}

interface

uses
  wfc_sequence;

const
  WFC_SEQUENCE_TEXT_VERSION = 1;
  WFC_SEQUENCE_MAX_ENCODED_TEXT_LENGTH = 16777216;
  WFC_SEQUENCE_MAX_TEXT_LINE_COUNT = 262144;

function EncodeWfcSequenceText(
  const AModel: TWfcSequenceModel): String;
function DecodeWfcSequenceText(
  const AText: String): TWfcSequenceModel;

implementation

uses
  SysUtils,
  wfc_model,
  wfc_text_codec;

const
  WFC_SEQUENCE_TEXT_ARTIFACT = 'WFC sequence';
  WFC_SEQUENCE_FIXED_LINE_COUNT = 6;

procedure TextError(const AMessage: String);
begin
  WfcTextError(WFC_SEQUENCE_TEXT_ARTIFACT, AMessage);
end;

function ParseCanonicalInteger(const AText,
  AFieldName: String): Integer;
begin
  Result := WfcTextParseCanonicalInteger(AText, AFieldName,
    WFC_SEQUENCE_TEXT_ARTIFACT);
end;

function ValueAfterPrefix(const ALine, APrefix,
  AFieldName: String): String;
begin
  Result := WfcTextValueAfterPrefix(ALine, APrefix, AFieldName,
    WFC_SEQUENCE_TEXT_ARTIFACT);
end;

function CheckedAdd(const A, B: Integer;
  const ALabel: String): Integer;
begin
  if (A < 0) or (B < 0) then
    TextError(ALabel + ' cannot be negative');
  if A > High(Integer) - B then
    TextError(ALabel + ' exceeds the supported integer range');
  Result := A + B;
end;

function CheckedLineCount(const ASampleCount, ATokenCount,
  AStateCount: Integer): Integer;
begin
  Result := WFC_SEQUENCE_FIXED_LINE_COUNT;
  Result := CheckedAdd(Result, ASampleCount,
    'sequence text line count');
  Result := CheckedAdd(Result, ATokenCount,
    'sequence text line count');
  Result := CheckedAdd(Result, AStateCount,
    'sequence text line count');
  if Result > WFC_SEQUENCE_MAX_TEXT_LINE_COUNT then
    TextError(
      'canonical WFC sequence text exceeds the version-1 line-count limit');
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
        WFC_SEQUENCE_MAX_ENCODED_TEXT_LENGTH - LTotalLength - 1) then
      raise ERangeError.Create(
        'canonical WFC sequence text exceeds the version-1 length limit');
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
      (LTextLength > SizeInt(WFC_SEQUENCE_MAX_ENCODED_TEXT_LENGTH)) then
    TextError('document exceeds the version-1 encoded length limit');
  LLineCount := 0;
  for I := 1 to LTextLength do
    if AText[I] = #10 then
    begin
      if LLineCount = WFC_SEQUENCE_MAX_TEXT_LINE_COUNT then
        TextError('document exceeds the version-1 line-count limit');
      Inc(LLineCount);
    end;
end;

function RequireLine(const ALines: TWfcTextLines;
  const AIndex: Integer; const AFieldName: String): String;
begin
  if (AIndex < 0) or (AIndex >= Length(ALines)) then
    TextError(AFieldName + ' is missing');
  Result := ALines[AIndex];
end;

function TakeCommaField(const ALine, AFieldName: String;
  var AStart: Integer): String;
var
  LComma: Integer;
begin
  LComma := WfcTextFindCharacter(ALine, ',', AStart);
  if LComma = 0 then
    TextError('state record is missing ' + AFieldName);
  Result := Copy(ALine, AStart, LComma - AStart);
  AStart := LComma + 1;
end;

procedure ParseSampleLine(const ALine: String;
  const AExpectedIndex: Integer; out ALength: Integer);
var
  LComma: Integer;
  LIndex: Integer;
begin
  if Copy(ALine, 1, 2) <> 's=' then
    TextError('expected sample record');
  LComma := WfcTextFindCharacter(ALine, ',', 3);
  if LComma = 0 then
    TextError('sample record is missing its length');
  if WfcTextFindCharacter(ALine, ',', LComma + 1) <> 0 then
    TextError('sample record has extra fields');
  LIndex := ParseCanonicalInteger(Copy(ALine, 3,
    LComma - 3), 'sample index');
  if LIndex <> AExpectedIndex then
    TextError('sample indices must be complete and ordered');
  ALength := ParseCanonicalInteger(Copy(ALine, LComma + 1,
    Length(ALine) - LComma), 'sample length');
  if ALength < 1 then
    TextError('sample lengths must be positive');
end;

procedure ParseTokenLine(const ALine: String;
  const AExpectedIndex: Integer; out AToken: TWfcModelToken);
var
  LComma: Integer;
  LIndex: Integer;
begin
  if Copy(ALine, 1, 2) <> 't=' then
    TextError('expected token record');
  LComma := WfcTextFindCharacter(ALine, ',', 3);
  if LComma = 0 then
    TextError('token record is missing its token');
  if WfcTextFindCharacter(ALine, ',', LComma + 1) <> 0 then
    TextError('token record contains an unescaped comma');
  LIndex := ParseCanonicalInteger(Copy(ALine, 3,
    LComma - 3), 'token index');
  if LIndex <> AExpectedIndex then
    TextError('token indices must be complete and ordered');
  AToken := WfcTextDecodeToken(Copy(ALine, LComma + 1,
    Length(ALine) - LComma), WFC_SEQUENCE_TEXT_ARTIFACT);
end;

function ParseHistoryAtom(const AText: String;
  const ATokenCount: Integer): TWfcSequenceHistoryItem;
var
  LTokenIndex: Integer;
begin
  if AText = 'B' then
    Exit(MakeWfcSequenceBosHistoryItem);
  if (Length(AText) < 2) or (AText[1] <> 'T') then
    TextError('history atom must be B or T followed by a token index');
  LTokenIndex := ParseCanonicalInteger(Copy(AText, 2,
    Length(AText) - 1), 'history token index');
  if LTokenIndex >= ATokenCount then
    TextError('history token index is out of range');
  Result := MakeWfcSequenceTokenHistoryItem(LTokenIndex);
end;

function ParseEmittedAtom(const AText: String;
  const ATokenCount: Integer): Integer;
begin
  if (Length(AText) < 2) or (AText[1] <> 'E') then
    TextError('emitted atom must be E followed by a token index');
  Result := ParseCanonicalInteger(Copy(AText, 2,
    Length(AText) - 1), 'emitted token index');
  if Result >= ATokenCount then
    TextError('emitted token index is out of range');
end;

procedure ParseStateLine(const ALine: String;
  const AExpectedIndex, AOrder, ATokenCount: Integer;
  out AState: TWfcSequenceState;
  out ACount, AStartCount, AEndCount: Integer);
var
  I: Integer;
  LCommaCount: Integer;
  LEmittedTokenIndex: Integer;
  LExpectedCommaCount: Integer;
  LField: String;
  LHistory: TWfcSequenceHistory;
  LIndex: Integer;
  LStart: Integer;
begin
  if Copy(ALine, 1, 2) <> 'q=' then
    TextError('expected state record');

  { Reject an impossible field count before allocating an attacker-controlled
    history length. A valid record has Order + 3 commas. }
  if AOrder > High(Integer) - 3 then
    TextError('sequence order exceeds the supported state-record size');
  LExpectedCommaCount := AOrder + 3;
  LCommaCount := 0;
  for I := 1 to Length(ALine) do
    if ALine[I] = ',' then
      Inc(LCommaCount);
  if LCommaCount <> LExpectedCommaCount then
    TextError('state record has the wrong number of fields');

  LStart := 3;
  LIndex := ParseCanonicalInteger(TakeCommaField(ALine,
    'its index', LStart), 'state index');
  if LIndex <> AExpectedIndex then
    TextError('state indices must be complete and ordered');
  ACount := ParseCanonicalInteger(TakeCommaField(ALine,
    'its observation count', LStart), 'state observation count');
  if ACount < 1 then
    TextError('state observation counts must be positive');
  AStartCount := ParseCanonicalInteger(TakeCommaField(ALine,
    'its start count', LStart), 'state start count');
  AEndCount := ParseCanonicalInteger(TakeCommaField(ALine,
    'its end count', LStart), 'state end count');
  if AStartCount > ACount then
    TextError('state start count exceeds its observation count');
  if AEndCount > ACount then
    TextError('state end count exceeds its observation count');

  SetLength(LHistory, AOrder - 1);
  for I := 0 to Length(LHistory) - 1 do
  begin
    LField := TakeCommaField(ALine,
      'a history atom', LStart);
    LHistory[I] := ParseHistoryAtom(LField, ATokenCount);
  end;
  LField := Copy(ALine, LStart, Length(ALine) - LStart + 1);
  LEmittedTokenIndex := ParseEmittedAtom(LField, ATokenCount);
  AState := MakeWfcSequenceState(LHistory, LEmittedTokenIndex);
end;

function EncodeWfcSequenceText(
  const AModel: TWfcSequenceModel): String;
var
  I: Integer;
  J: Integer;
  LEndCounts: TWfcModelIntegerArray;
  LLine: String;
  LLineIndex: Integer;
  LLines: TWfcTextLines;
  LSampleLengths: TWfcSequenceSampleLengths;
  LStartCounts: TWfcModelIntegerArray;
  LStateCounts: TWfcModelIntegerArray;
  LStates: TWfcSequenceStates;
  LTokens: TWfcModelTokens;
begin
  if not Assigned(AModel) then
    raise EArgumentNilException.Create(
      'WFC sequence model cannot be nil');

  LSampleLengths := AModel.CopySampleLengths;
  LTokens := AModel.CopyPublicTokens;
  LStates := AModel.CopyStates;
  LStateCounts := AModel.CopyStateCounts;
  LStartCounts := AModel.CopyStartCounts;
  LEndCounts := AModel.CopyEndCounts;
  SetLength(LLines, CheckedLineCount(AModel.SampleCount,
    AModel.PublicTokenCount, AModel.StateCount));

  LLineIndex := 0;
  LLines[LLineIndex] := 'wfcs=' +
    IntToStr(WFC_SEQUENCE_TEXT_VERSION);
  Inc(LLineIndex);
  LLines[LLineIndex] := 'order=' + IntToStr(AModel.Order);
  Inc(LLineIndex);
  LLines[LLineIndex] := 'samples=' + IntToStr(AModel.SampleCount);
  Inc(LLineIndex);
  for I := 0 to AModel.SampleCount - 1 do
  begin
    LLines[LLineIndex] := 's=' + IntToStr(I) + ',' +
      IntToStr(LSampleLengths[I]);
    Inc(LLineIndex);
  end;

  LLines[LLineIndex] := 'tokens=' +
    IntToStr(AModel.PublicTokenCount);
  Inc(LLineIndex);
  for I := 0 to AModel.PublicTokenCount - 1 do
  begin
    LLines[LLineIndex] := 't=' + IntToStr(I) + ',' +
      WfcTextEncodeToken(LTokens[I], WFC_SEQUENCE_TEXT_ARTIFACT);
    Inc(LLineIndex);
  end;

  LLines[LLineIndex] := 'states=' + IntToStr(AModel.StateCount);
  Inc(LLineIndex);
  for I := 0 to AModel.StateCount - 1 do
  begin
    LLine := 'q=' + IntToStr(I) + ',' +
      IntToStr(LStateCounts[I]) + ',' +
      IntToStr(LStartCounts[I]) + ',' +
      IntToStr(LEndCounts[I]);
    for J := 0 to AModel.HistorySize - 1 do
      if LStates[I].History[J].Kind = wshBos then
      begin
        if LStates[I].History[J].TokenIndex <> -1 then
          TextError('BOS history atom has a noncanonical token index');
        LLine := LLine + ',B';
      end
      else if LStates[I].History[J].Kind = wshToken then
      begin
        if (LStates[I].History[J].TokenIndex < 0) or
            (LStates[I].History[J].TokenIndex >=
              AModel.PublicTokenCount) then
          TextError('history token index is out of range');
        LLine := LLine + ',T' +
          IntToStr(LStates[I].History[J].TokenIndex);
      end
      else
        TextError('history atom has an unknown kind');
    if (LStates[I].EmittedTokenIndex < 0) or
        (LStates[I].EmittedTokenIndex >= AModel.PublicTokenCount) then
      TextError('emitted token index is out of range');
    LLine := LLine + ',E' +
      IntToStr(LStates[I].EmittedTokenIndex);
    LLines[LLineIndex] := LLine;
    Inc(LLineIndex);
  end;
  LLines[LLineIndex] := 'end';

  RequireEncodedTextLength(LLines);
  Result := WfcTextJoinCanonicalLines(LLines,
    WFC_SEQUENCE_TEXT_ARTIFACT);
end;

function DecodeWfcSequenceText(
  const AText: String): TWfcSequenceModel;
var
  I: Integer;
  LEndCounts: TWfcModelIntegerArray;
  LLineIndex: Integer;
  LLines: TWfcTextLines;
  LOrder: Integer;
  LSampleCount: Integer;
  LSampleLengths: TWfcSequenceSampleLengths;
  LStartCounts: TWfcModelIntegerArray;
  LStateCount: Integer;
  LStateCounts: TWfcModelIntegerArray;
  LStates: TWfcSequenceStates;
  LTokenCount: Integer;
  LTokens: TWfcModelTokens;
begin
  Result := nil;
  PreflightTextEnvelope(AText);
  WfcTextSplitCanonicalLines(AText, WFC_SEQUENCE_TEXT_ARTIFACT,
    LLines);
  if Length(LLines) < WFC_SEQUENCE_FIXED_LINE_COUNT then
    TextError('document is incomplete');
  LLineIndex := 0;

  if RequireLine(LLines, LLineIndex, 'format version') <>
      'wfcs=1' then
    TextError('unsupported or noncanonical format version');
  Inc(LLineIndex);

  LOrder := ParseCanonicalInteger(ValueAfterPrefix(
    RequireLine(LLines, LLineIndex, 'order'),
    'order=', 'order'), 'order');
  if LOrder < 1 then
    TextError('order must be positive');
  if LOrder > WFC_SEQUENCE_MAX_ORDER then
    TextError('order exceeds the version-1 limit');
  Inc(LLineIndex);

  LSampleCount := ParseCanonicalInteger(ValueAfterPrefix(
    RequireLine(LLines, LLineIndex, 'samples'),
    'samples=', 'samples'), 'samples');
  if LSampleCount < 1 then
    TextError('samples must be positive');
  if LSampleCount > WFC_SEQUENCE_MAX_SAMPLE_COUNT then
    TextError('sample count exceeds the version-1 limit');
  Inc(LLineIndex);
  { tokens, at least one token, states, at least one state, and end remain. }
  if LSampleCount > Length(LLines) - LLineIndex - 5 then
    TextError('sample records are incomplete');
  SetLength(LSampleLengths, LSampleCount);
  for I := 0 to LSampleCount - 1 do
  begin
    ParseSampleLine(RequireLine(LLines, LLineIndex,
      'sample record'), I, LSampleLengths[I]);
    Inc(LLineIndex);
  end;

  LTokenCount := ParseCanonicalInteger(ValueAfterPrefix(
    RequireLine(LLines, LLineIndex, 'tokens'),
    'tokens=', 'tokens'), 'tokens');
  if LTokenCount < 1 then
    TextError('tokens must be positive');
  if LTokenCount > WFC_SEQUENCE_MAX_PUBLIC_TOKEN_COUNT then
    TextError('token count exceeds the version-1 limit');
  Inc(LLineIndex);
  { states, at least one state, and end remain after token records. }
  if LTokenCount > Length(LLines) - LLineIndex - 3 then
    TextError('token records are incomplete');
  SetLength(LTokens, LTokenCount);
  for I := 0 to LTokenCount - 1 do
  begin
    ParseTokenLine(RequireLine(LLines, LLineIndex,
      'token record'), I, LTokens[I]);
    Inc(LLineIndex);
  end;

  LStateCount := ParseCanonicalInteger(ValueAfterPrefix(
    RequireLine(LLines, LLineIndex, 'states'),
    'states=', 'states'), 'states');
  if LStateCount < 1 then
    TextError('states must be positive');
  if LStateCount > WFC_SEQUENCE_MAX_STATE_COUNT then
    TextError('state count exceeds the version-1 limit');
  if (LOrder > 1) and
      (LStateCount > WFC_SEQUENCE_MAX_TOTAL_HISTORY_ITEM_COUNT div
        (LOrder - 1)) then
    TextError('state history exceeds the version-1 aggregate limit');
  Inc(LLineIndex);
  if LStateCount > Length(LLines) - LLineIndex - 1 then
    TextError('state records are incomplete');
  SetLength(LStates, LStateCount);
  SetLength(LStateCounts, LStateCount);
  SetLength(LStartCounts, LStateCount);
  SetLength(LEndCounts, LStateCount);
  for I := 0 to LStateCount - 1 do
  begin
    ParseStateLine(RequireLine(LLines, LLineIndex,
      'state record'), I, LOrder, LTokenCount, LStates[I],
      LStateCounts[I], LStartCounts[I], LEndCounts[I]);
    Inc(LLineIndex);
  end;

  if RequireLine(LLines, LLineIndex, 'end marker') <> 'end' then
    TextError('end marker is missing or misplaced');
  Inc(LLineIndex);
  if LLineIndex <> Length(LLines) then
    TextError('trailing data is not permitted');

  try
    Result := TWfcSequenceModel.Create(LOrder, LSampleLengths,
      LTokens, LStates, LStateCounts, LStartCounts, LEndCounts);
  except
    on E: EWfcSequence do
      raise EConvertError.Create('invalid ' +
        WFC_SEQUENCE_TEXT_ARTIFACT + ' text: ' + E.Message);
  end;
  try
    if EncodeWfcSequenceText(Result) <> AText then
      TextError('document is not in canonical form');
  except
    Result.Free;
    Result := nil;
    raise;
  end;
end;

end.
