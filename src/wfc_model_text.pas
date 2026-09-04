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
unit wfc_model_text;

{$mode delphi}{$H+}

interface

uses
  wfc_model;

const
  { Version 1 remains the canonical representation of a one-sample model.
    Version 2 adds an ordered sample-shape corpus and is canonical only when
    that corpus contains at least two samples. }
  WFC_MODEL_TEXT_VERSION = 2;
  WFC_MODEL_MAX_ENCODED_TEXT_LENGTH = 16777216;
  WFC_MODEL_MAX_TEXT_LINE_COUNT = 262144;

function EncodeWfcModelText(const AModel: TWfcModel): String;
function DecodeWfcModelText(const AText: String): TWfcModel;

implementation

uses
  SysUtils,
  wfc_text_codec;

const
  WFC_MODEL_TEXT_ARTIFACT = 'WFC model';

procedure TextError(const AMessage: String);
begin
  WfcTextError(WFC_MODEL_TEXT_ARTIFACT, AMessage);
end;

procedure PreflightTextEnvelope(const AText: String);
var
  I: SizeInt;
  LLineCount: Integer;
  LTextLength: SizeInt;
begin
  LTextLength := Length(AText);
  if (LTextLength < 0) or
      (LTextLength > SizeInt(WFC_MODEL_MAX_ENCODED_TEXT_LENGTH)) then
    TextError('document exceeds the version-1 encoded length limit');
  LLineCount := 0;
  for I := 1 to LTextLength do
    if AText[I] = #10 then
    begin
      if LLineCount = WFC_MODEL_MAX_TEXT_LINE_COUNT then
        TextError('document exceeds the version-1 line-count limit');
      Inc(LLineCount);
    end;
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
        WFC_MODEL_MAX_ENCODED_TEXT_LENGTH - LTotalLength - 1) then
      raise ERangeError.Create(
        'canonical WFC model text exceeds the version-1 length limit');
    Inc(LTotalLength, Integer(LLineLength) + 1);
  end;
end;

function CheckedRelationSlotCount(const AValueCount: Integer): Integer;
var
  LSquare: Integer;
begin
  if AValueCount < 0 then
    raise ERangeError.Create('WFC model value count cannot be negative');
  if AValueCount > WFC_MODEL_MAX_VALUE_COUNT then
    raise ERangeError.Create(
      'WFC model value count exceeds the version-1 limit');
  if (AValueCount <> 0) and
    (AValueCount > (High(Integer) div AValueCount)) then
    raise ERangeError.Create('WFC model relation table is too large');
  LSquare := AValueCount * AValueCount;
  if LSquare > (High(Integer) div 4) then
    raise ERangeError.Create('WFC model relation table is too large');
  Result := LSquare * 4;
  if Result > WFC_MODEL_MAX_RELATION_SLOT_COUNT then
    raise ERangeError.Create(
      'WFC model relation table exceeds the version-1 slot limit');
end;

function CheckedLineCount(const AValueCount, ARelationCount,
  ASampleCount, AFormatVersion: Integer): Integer;
var
  LFixedLineCount: Integer;
begin
  if (AValueCount < 0) or (ARelationCount < 0) or
    (ASampleCount < 1) then
    raise ERangeError.Create('WFC model text line count cannot be negative');
  if ASampleCount > WFC_MODEL_MAX_SAMPLE_COUNT then
    raise ERangeError.Create(
      'WFC model sample count exceeds the version-1 limit');

  case AFormatVersion of
    1:
      begin
        if ASampleCount <> 1 then
          raise ERangeError.Create(
            'WFC model text version 1 requires exactly one sample');
        LFixedLineCount := 10;
      end;
    2:
      begin
        if ASampleCount < 2 then
          raise ERangeError.Create(
            'WFC model text version 2 requires multiple samples');
        LFixedLineCount := 9;
      end;
  else
    raise ERangeError.Create('unsupported WFC model text version');
  end;

  if (AFormatVersion = 2) then
  begin
    if ASampleCount > (High(Integer) - LFixedLineCount) then
      raise ERangeError.Create('WFC model text has too many lines');
    Inc(LFixedLineCount, ASampleCount);
  end;
  if AValueCount > (High(Integer) - LFixedLineCount) then
    raise ERangeError.Create('WFC model text has too many lines');
  Result := AValueCount + LFixedLineCount;
  if ARelationCount > (High(Integer) - Result) then
    raise ERangeError.Create('WFC model text has too many lines');
  Inc(Result, ARelationCount);
  if Result > WFC_MODEL_MAX_TEXT_LINE_COUNT then
    raise ERangeError.Create(
      'WFC model text exceeds the version-1 line-count limit');
end;

function CheckedSampleCells(const AWidth, AHeight,
  ASampleIndex: Integer): Integer;
begin
  if (AWidth < 1) or (AHeight < 1) then
    TextError('sample dimensions must be positive');
  if (AWidth > WFC_MODEL_MAX_SAMPLE_DIMENSION) or
      (AHeight > WFC_MODEL_MAX_SAMPLE_DIMENSION) then
    TextError(Format(
      'sample %d dimension exceeds the version-1 limit', [ASampleIndex]));
  if AWidth > WFC_MODEL_MAX_SAMPLE_CELL_COUNT div AHeight then
    TextError(Format(
      'sample %d cells exceed the version-1 limit', [ASampleIndex]));
  Result := AWidth * AHeight;
end;

procedure AccumulateSampleCells(var ATotal: Integer;
  const AWidth, AHeight, ASampleIndex: Integer);
var
  LCells: Integer;
begin
  LCells := CheckedSampleCells(AWidth, AHeight, ASampleIndex);
  if ATotal > WFC_MODEL_MAX_TOTAL_SAMPLE_CELL_COUNT - LCells then
    TextError('aggregate sample cells exceed the version-1 limit');
  Inc(ATotal, LCells);
end;

function ParseCanonicalInteger(const AText, AFieldName: String): Integer;
begin
  Result := WfcTextParseCanonicalInteger(AText, AFieldName,
    WFC_MODEL_TEXT_ARTIFACT);
end;

function ValueAfterPrefix(const ALine, APrefix,
  AFieldName: String): String;
begin
  Result := WfcTextValueAfterPrefix(ALine, APrefix, AFieldName,
    WFC_MODEL_TEXT_ARTIFACT);
end;

function EncodeToken(const AToken: TWfcModelToken): String;
begin
  Result := WfcTextEncodeToken(AToken, WFC_MODEL_TEXT_ARTIFACT);
end;

function DecodeToken(const AText: String): TWfcModelToken;
begin
  Result := WfcTextDecodeToken(AText, WFC_MODEL_TEXT_ARTIFACT);
end;

function DirectionCode(const ADirection: TWfcModelDirection): String;
begin
  case ADirection of
    wmdNorth:
      Result := 'N';
    wmdEast:
      Result := 'E';
    wmdSouth:
      Result := 'S';
    wmdWest:
      Result := 'W';
  else
    raise ERangeError.Create('unknown WFC model direction');
  end;
end;

function EncodeDirections(const ADirections: TWfcModelDirections): String;
var
  LDirection: TWfcModelDirection;
begin
  Result := '';
  for LDirection := Low(TWfcModelDirection) to High(TWfcModelDirection) do
    if LDirection in ADirections then
    begin
      if Result <> '' then
        Result := Result + ',';
      Result := Result + DirectionCode(LDirection);
    end;
end;

function ParseDirections(const AText: String): TWfcModelDirections;
var
  I: Integer;
  LStart: Integer;
  LPart: String;
  LDirection: TWfcModelDirection;
  LPreviousOrdinal: Integer;
begin
  Result := [];
  if AText = '' then
    Exit;

  LStart := 1;
  LPreviousOrdinal := -1;
  while LStart <= Length(AText) do
  begin
    I := LStart;
    while (I <= Length(AText)) and (AText[I] <> ',') do
      Inc(I);
    LPart := Copy(AText, LStart, I - LStart);
    if LPart = 'N' then
      LDirection := wmdNorth
    else if LPart = 'E' then
      LDirection := wmdEast
    else if LPart = 'S' then
      LDirection := wmdSouth
    else if LPart = 'W' then
      LDirection := wmdWest
    else
      TextError('directions contains an unknown name');
    if Ord(LDirection) <= LPreviousOrdinal then
      TextError('directions is duplicated or out of order');
    Include(Result, LDirection);
    LPreviousOrdinal := Ord(LDirection);

    if I > Length(AText) then
      Break;
    LStart := I + 1;
    if LStart > Length(AText) then
      TextError('directions has a trailing comma');
  end;
end;

function BoundaryName(const ABoundary: TWfcModelBoundary): String;
begin
  case ABoundary of
    wmbOpen:
      Result := 'open';
    wmbWrap:
      Result := 'wrap';
  else
    raise ERangeError.Create('unknown WFC model boundary');
  end;
end;

function SymmetryName(const ASymmetry: TWfcModelSymmetry): String;
begin
  case ASymmetry of
    wmsNone:
      Result := 'none';
    wmsD4:
      Result := 'd4';
  else
    raise ERangeError.Create('unknown WFC model symmetry');
  end;
end;

function JoinCanonicalLines(const ALines: TWfcTextLines): String;
begin
  Result := WfcTextJoinCanonicalLines(ALines,
    WFC_MODEL_TEXT_ARTIFACT);
end;

procedure SplitCanonicalLines(const AText: String;
  out ALines: TWfcTextLines);
begin
  WfcTextSplitCanonicalLines(AText, WFC_MODEL_TEXT_ARTIFACT,
    ALines);
end;

function FindCharacter(const AText: String; const ACharacter: Char;
  const AStart: Integer): Integer;
begin
  Result := WfcTextFindCharacter(AText, ACharacter, AStart);
end;

procedure ParseValueLine(const ALine: String; const AExpectedIndex: Integer;
  out AToken: TWfcModelToken; out AWeight: Integer);
var
  LFirstComma: Integer;
  LSecondComma: Integer;
  LIndex: Integer;
begin
  if Copy(ALine, 1, 2) <> 'v=' then
    TextError('expected value record');
  LFirstComma := FindCharacter(ALine, ',', 3);
  if LFirstComma = 0 then
    TextError('value record is missing fields');
  LSecondComma := FindCharacter(ALine, ',', LFirstComma + 1);
  if LSecondComma = 0 then
    TextError('value record is missing its token');

  LIndex := ParseCanonicalInteger(Copy(ALine, 3,
    LFirstComma - 3), 'value index');
  if LIndex <> AExpectedIndex then
    TextError('value indices must be complete and ordered');
  AWeight := ParseCanonicalInteger(Copy(ALine, LFirstComma + 1,
    LSecondComma - LFirstComma - 1), 'value weight');
  if AWeight <= 0 then
    TextError('value weight must be positive');
  AToken := DecodeToken(Copy(ALine, LSecondComma + 1,
    Length(ALine) - LSecondComma));
end;

procedure ParseSampleLine(const ALine: String;
  const AExpectedIndex: Integer; out AShape: TWfcModelSampleShape);
var
  LComma1: Integer;
  LComma2: Integer;
  LIndex: Integer;
begin
  if Copy(ALine, 1, 2) <> 's=' then
    TextError('expected sample-shape record');
  LComma1 := FindCharacter(ALine, ',', 3);
  if LComma1 = 0 then
    TextError('sample-shape record is missing fields');
  LComma2 := FindCharacter(ALine, ',', LComma1 + 1);
  if LComma2 = 0 then
    TextError('sample-shape record is missing fields');
  if FindCharacter(ALine, ',', LComma2 + 1) <> 0 then
    TextError('sample-shape record has extra fields');

  LIndex := ParseCanonicalInteger(Copy(ALine, 3,
    LComma1 - 3), 'sample index');
  if LIndex <> AExpectedIndex then
    TextError('sample indices must be complete and ordered');
  AShape.Width := ParseCanonicalInteger(Copy(ALine, LComma1 + 1,
    LComma2 - LComma1 - 1), 'sample width');
  AShape.Height := ParseCanonicalInteger(Copy(ALine, LComma2 + 1,
    Length(ALine) - LComma2), 'sample height');
  CheckedSampleCells(AShape.Width, AShape.Height, AExpectedIndex);
end;

procedure ParseRelationLine(const ALine: String;
  const AValueCount, APreviousSlot: Integer;
  const ADirections: TWfcModelDirections;
  out ASlot, ACount: Integer);
var
  LComma1: Integer;
  LComma2: Integer;
  LComma3: Integer;
  LDirectionName: String;
  LDirection: TWfcModelDirection;
  LSource: Integer;
  LTarget: Integer;
begin
  if Copy(ALine, 1, 2) <> 'r=' then
    TextError('expected relation record');
  LComma1 := FindCharacter(ALine, ',', 3);
  LComma2 := FindCharacter(ALine, ',', LComma1 + 1);
  LComma3 := FindCharacter(ALine, ',', LComma2 + 1);
  if (LComma1 = 0) or (LComma2 = 0) or (LComma3 = 0) then
    TextError('relation record is missing fields');
  if FindCharacter(ALine, ',', LComma3 + 1) <> 0 then
    TextError('relation record has extra fields');

  LDirectionName := Copy(ALine, 3, LComma1 - 3);
  if LDirectionName = 'N' then
    LDirection := wmdNorth
  else if LDirectionName = 'E' then
    LDirection := wmdEast
  else if LDirectionName = 'S' then
    LDirection := wmdSouth
  else if LDirectionName = 'W' then
    LDirection := wmdWest
  else
    TextError('relation has an unknown direction');
  if not (LDirection in ADirections) then
    TextError('relation uses an inactive direction');

  LSource := ParseCanonicalInteger(Copy(ALine, LComma1 + 1,
    LComma2 - LComma1 - 1), 'relation source');
  LTarget := ParseCanonicalInteger(Copy(ALine, LComma2 + 1,
    LComma3 - LComma2 - 1), 'relation target');
  ACount := ParseCanonicalInteger(Copy(ALine, LComma3 + 1,
    Length(ALine) - LComma3), 'relation count');
  if (LSource >= AValueCount) or (LTarget >= AValueCount) then
    TextError('relation value index is out of range');
  if ACount <= 0 then
    TextError('serialized relation count must be positive');

  ASlot := ((Ord(LDirection) * AValueCount + LSource) *
    AValueCount) + LTarget;
  if ASlot <= APreviousSlot then
    TextError('relations must be unique and canonically ordered');
end;

function EncodeWfcModelText(const AModel: TWfcModel): String;
var
  LLines: TWfcTextLines;
  LLineIndex: Integer;
  LFormatVersion: Integer;
  LRelationCount: Integer;
  LCount: Integer;
  LSample: Integer;
  LValue: Integer;
  LSource: Integer;
  LTarget: Integer;
  LSampleShape: TWfcModelSampleShape;
  LDirection: TWfcModelDirection;
begin
  if AModel = nil then
    raise EArgumentNilException.Create('WFC model cannot be nil');
  if AModel.SampleCount < 1 then
    raise ERangeError.Create('WFC model must contain at least one sample');

  if AModel.SampleCount = 1 then
    LFormatVersion := 1
  else
    LFormatVersion := WFC_MODEL_TEXT_VERSION;

  CheckedRelationSlotCount(AModel.ValueCount);
  LRelationCount := 0;
  for LDirection := Low(TWfcModelDirection) to High(TWfcModelDirection) do
    for LSource := 0 to AModel.ValueCount - 1 do
      for LTarget := 0 to AModel.ValueCount - 1 do
      begin
        LCount := AModel.RelationCount(LDirection, LSource, LTarget);
        if LCount < 0 then
          raise ERangeError.Create('WFC model relation count cannot be negative');
        if LCount > 0 then
        begin
          if not (LDirection in AModel.Directions) then
            raise ERangeError.Create(
              'WFC model has a relation in an inactive direction');
          if LRelationCount = High(Integer) then
            raise ERangeError.Create('WFC model has too many relations');
          Inc(LRelationCount);
        end;
      end;

  SetLength(LLines, CheckedLineCount(AModel.ValueCount,
    LRelationCount, AModel.SampleCount, LFormatVersion));
  LLineIndex := 0;
  LLines[LLineIndex] := 'wfcm=' + IntToStr(LFormatVersion);
  Inc(LLineIndex);
  LLines[LLineIndex] := 'rank=' + IntToStr(AModel.Rank);
  Inc(LLineIndex);
  if LFormatVersion = 1 then
  begin
    LSampleShape := AModel.SampleShapeAt(0);
    LLines[LLineIndex] := 'width=' + IntToStr(LSampleShape.Width);
    Inc(LLineIndex);
    LLines[LLineIndex] := 'height=' + IntToStr(LSampleShape.Height);
    Inc(LLineIndex);
  end
  else
  begin
    LLines[LLineIndex] := 'samples=' + IntToStr(AModel.SampleCount);
    Inc(LLineIndex);
    for LSample := 0 to AModel.SampleCount - 1 do
    begin
      LSampleShape := AModel.SampleShapeAt(LSample);
      if (LSampleShape.Width <= 0) or (LSampleShape.Height <= 0) then
        raise ERangeError.Create('WFC model sample dimensions must be positive');
      LLines[LLineIndex] := 's=' + IntToStr(LSample) + ',' +
        IntToStr(LSampleShape.Width) + ',' +
        IntToStr(LSampleShape.Height);
      Inc(LLineIndex);
    end;
  end;
  LLines[LLineIndex] := 'boundary=' + BoundaryName(AModel.Boundary);
  Inc(LLineIndex);
  LLines[LLineIndex] := 'symmetry=' + SymmetryName(AModel.Symmetry);
  Inc(LLineIndex);
  LLines[LLineIndex] := 'directions=' +
    EncodeDirections(AModel.Directions);
  Inc(LLineIndex);
  LLines[LLineIndex] := 'values=' + IntToStr(AModel.ValueCount);
  Inc(LLineIndex);

  for LValue := 0 to AModel.ValueCount - 1 do
  begin
    if AModel.WeightAt(LValue) <= 0 then
      raise ERangeError.Create('WFC model value weight must be positive');
    LLines[LLineIndex] := 'v=' + IntToStr(LValue) + ',' +
      IntToStr(AModel.WeightAt(LValue)) + ',' +
      EncodeToken(AModel.TokenAt(LValue));
    Inc(LLineIndex);
  end;

  LLines[LLineIndex] := 'relations=' + IntToStr(LRelationCount);
  Inc(LLineIndex);
  for LDirection := Low(TWfcModelDirection) to High(TWfcModelDirection) do
    for LSource := 0 to AModel.ValueCount - 1 do
      for LTarget := 0 to AModel.ValueCount - 1 do
      begin
        LCount := AModel.RelationCount(LDirection, LSource, LTarget);
        if LCount > 0 then
        begin
          LLines[LLineIndex] := 'r=' + DirectionCode(LDirection) + ',' +
            IntToStr(LSource) + ',' + IntToStr(LTarget) + ',' +
            IntToStr(LCount);
          Inc(LLineIndex);
        end;
      end;
  LLines[LLineIndex] := 'end';

  RequireEncodedTextLength(LLines);
  Result := JoinCanonicalLines(LLines);
end;

function DecodeWfcModelText(const AText: String): TWfcModel;
var
  LLines: TWfcTextLines;
  LLineIndex: Integer;
  LFormatVersion: Integer;
  LRank: Integer;
  LWidth: Integer;
  LHeight: Integer;
  LSampleCount: Integer;
  LSample: Integer;
  LSampleCells: Integer;
  LValueCount: Integer;
  LRelationCount: Integer;
  LRelationSlots: Integer;
  LValue: Integer;
  LRelation: Integer;
  LSlot: Integer;
  LPreviousSlot: Integer;
  LCount: Integer;
  LBoundary: TWfcModelBoundary;
  LSymmetry: TWfcModelSymmetry;
  LDirections: TWfcModelDirections;
  LSampleShapes: TWfcModelSampleShapes;
  LTokens: TWfcModelTokens;
  LWeights: TWfcModelIntegerArray;
  LRelations: TWfcModelIntegerArray;
  LTextValue: String;
  LTotalSampleCells: Integer;
begin
  Result := nil;
  PreflightTextEnvelope(AText);
  SplitCanonicalLines(AText, LLines);
  if Length(LLines) < 1 then
    TextError('document is incomplete');

  LLineIndex := 0;
  if LLines[LLineIndex] = 'wfcm=1' then
    LFormatVersion := 1
  else if LLines[LLineIndex] = 'wfcm=2' then
    LFormatVersion := 2
  else
    TextError('unsupported or noncanonical format version');
  Inc(LLineIndex);

  if ((LFormatVersion = 1) and (Length(LLines) < 10)) or
    ((LFormatVersion = 2) and (Length(LLines) < 3)) then
    TextError('document is incomplete');
  LRank := ParseCanonicalInteger(ValueAfterPrefix(LLines[LLineIndex],
    'rank=', 'rank'), 'rank');
  Inc(LLineIndex);

  if LFormatVersion = 1 then
  begin
    LWidth := ParseCanonicalInteger(ValueAfterPrefix(LLines[LLineIndex],
      'width=', 'width'), 'width');
    Inc(LLineIndex);
    LHeight := ParseCanonicalInteger(ValueAfterPrefix(LLines[LLineIndex],
      'height=', 'height'), 'height');
    Inc(LLineIndex);
    LSampleCells := CheckedSampleCells(LWidth, LHeight, 0);
    if LSampleCells > WFC_MODEL_MAX_TOTAL_SAMPLE_CELL_COUNT then
      TextError('aggregate sample cells exceed the version-1 limit');
    SetLength(LSampleShapes, 1);
    LSampleShapes[0].Width := LWidth;
    LSampleShapes[0].Height := LHeight;
  end
  else
  begin
    LSampleCount := ParseCanonicalInteger(ValueAfterPrefix(
      LLines[LLineIndex], 'samples=', 'samples'), 'samples');
    Inc(LLineIndex);
    if LSampleCount < 2 then
      TextError('version 2 requires at least two samples');
    if LSampleCount > WFC_MODEL_MAX_SAMPLE_COUNT then
      TextError('sample count exceeds the version-1 limit');
    { Nine non-sample lines are required by version 2. Check the
      declaration against the physical document before allocating. }
    if LSampleCount > (Length(LLines) - 9) then
      TextError('sample-shape records are incomplete');
    SetLength(LSampleShapes, LSampleCount);
    LTotalSampleCells := 0;
    for LSample := 0 to LSampleCount - 1 do
    begin
      ParseSampleLine(LLines[LLineIndex], LSample,
        LSampleShapes[LSample]);
      AccumulateSampleCells(LTotalSampleCells,
        LSampleShapes[LSample].Width, LSampleShapes[LSample].Height,
        LSample);
      Inc(LLineIndex);
    end;
  end;

  LTextValue := ValueAfterPrefix(LLines[LLineIndex], 'boundary=',
    'boundary');
  if LTextValue = 'open' then
    LBoundary := wmbOpen
  else if LTextValue = 'wrap' then
    LBoundary := wmbWrap
  else
    TextError('boundary has an unknown value');
  Inc(LLineIndex);

  LTextValue := ValueAfterPrefix(LLines[LLineIndex], 'symmetry=',
    'symmetry');
  if LTextValue = 'none' then
    LSymmetry := wmsNone
  else if LTextValue = 'd4' then
    LSymmetry := wmsD4
  else
    TextError('symmetry has an unknown value');
  Inc(LLineIndex);

  LDirections := ParseDirections(ValueAfterPrefix(LLines[LLineIndex],
    'directions=', 'directions'));
  Inc(LLineIndex);
  LValueCount := ParseCanonicalInteger(ValueAfterPrefix(
    LLines[LLineIndex], 'values=', 'values'), 'values');
  Inc(LLineIndex);
  if LValueCount <= 0 then
    TextError('values must be positive');
  if LValueCount > WFC_MODEL_MAX_VALUE_COUNT then
    TextError('value count exceeds the version-1 limit');
  LRelationSlots := CheckedRelationSlotCount(LValueCount);
  { The relations field and end marker remain after the value records. }
  if LValueCount > Length(LLines) - LLineIndex - 2 then
    TextError('value records are incomplete');

  SetLength(LTokens, LValueCount);
  SetLength(LWeights, LValueCount);
  for LValue := 0 to LValueCount - 1 do
  begin
    if LLineIndex >= Length(LLines) then
      TextError('value records are incomplete');
    ParseValueLine(LLines[LLineIndex], LValue, LTokens[LValue],
      LWeights[LValue]);
    Inc(LLineIndex);
  end;

  if LLineIndex >= Length(LLines) then
    TextError('relations field is missing');
  LRelationCount := ParseCanonicalInteger(ValueAfterPrefix(
    LLines[LLineIndex], 'relations=', 'relations'), 'relations');
  Inc(LLineIndex);
  if LRelationCount > LRelationSlots then
    TextError('relations exceeds the relation table size');
  if LRelationCount > (Length(LLines) - LLineIndex - 1) then
    TextError('relation records are incomplete');

  SetLength(LRelations, LRelationSlots);
  LPreviousSlot := -1;
  for LRelation := 0 to LRelationCount - 1 do
  begin
    ParseRelationLine(LLines[LLineIndex], LValueCount,
      LPreviousSlot, LDirections, LSlot, LCount);
    LRelations[LSlot] := LCount;
    LPreviousSlot := LSlot;
    Inc(LLineIndex);
  end;

  if (LLineIndex >= Length(LLines)) or
    (LLines[LLineIndex] <> 'end') then
    TextError('end marker is missing or misplaced');
  Inc(LLineIndex);
  if LLineIndex <> Length(LLines) then
    TextError('trailing data is not permitted');

  try
    Result := TWfcModel.Create(LRank, LSampleShapes, LBoundary,
      LSymmetry, LDirections, LTokens, LWeights, LRelations);
  except
    on E: EWfcModel do
      raise EConvertError.Create('invalid WFC model text: ' + E.Message);
  end;
  try
    if EncodeWfcModelText(Result) <> AText then
      TextError('document is not in canonical form');
  except
    Result.Free;
    Result := nil;
    raise;
  end;
end;

end.
