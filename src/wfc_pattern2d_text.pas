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
unit wfc_pattern2d_text;

{$mode delphi}{$H+}

interface

uses
  wfc_pattern2d;

const
  WFC_PATTERN_2D_TEXT_VERSION = 1;
  WFC_PATTERN_2D_MAX_ENCODED_TEXT_LENGTH = 16777216;
  WFC_PATTERN_2D_MAX_TEXT_LINE_COUNT = 262144;

function EncodeWfcPattern2DText(
  const AModel: TWfcOverlappingModel2D): String;
function DecodeWfcPattern2DText(
  const AText: String): TWfcOverlappingModel2D;

implementation

uses
  SysUtils,
  wfc_model,
  wfc_text_codec;

const
  WFC_PATTERN_TEXT_ARTIFACT = 'WFC pattern';
  WFC_PATTERN_DIRECTION_COUNT = 4;
  WFC_PATTERN_D4_TRANSFORM_COUNT = 8;
  WFC_PATTERN_FIXED_LINE_COUNT = 11;

type
  TWfcPatternRelationFlags = array of Boolean;

procedure TextError(const AMessage: String);
begin
  WfcTextError(WFC_PATTERN_TEXT_ARTIFACT, AMessage);
end;

procedure PreflightTextEnvelope(const AText: String);
var
  I: SizeInt;
  LLineCount: Integer;
  LTextLength: SizeInt;
begin
  LTextLength := Length(AText);
  if (LTextLength < 0) or
      (LTextLength > SizeInt(WFC_PATTERN_2D_MAX_ENCODED_TEXT_LENGTH)) then
    TextError('document exceeds the version-1 encoded length limit');
  LLineCount := 0;
  for I := 1 to LTextLength do
    if AText[I] = #10 then
    begin
      if LLineCount = WFC_PATTERN_2D_MAX_TEXT_LINE_COUNT then
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
        WFC_PATTERN_2D_MAX_ENCODED_TEXT_LENGTH - LTotalLength - 1) then
      raise ERangeError.Create(
        'canonical WFC pattern text exceeds the version-1 length limit');
    Inc(LTotalLength, Integer(LLineLength) + 1);
  end;
end;

function ParseCanonicalInteger(const AText,
  AFieldName: String): Integer;
begin
  Result := WfcTextParseCanonicalInteger(AText, AFieldName,
    WFC_PATTERN_TEXT_ARTIFACT);
end;

function ValueAfterPrefix(const ALine, APrefix,
  AFieldName: String): String;
begin
  Result := WfcTextValueAfterPrefix(ALine, APrefix, AFieldName,
    WFC_PATTERN_TEXT_ARTIFACT);
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

function CheckedArrayLength(const ALength: SizeInt;
  const ALabel: String): Integer;
begin
  if (ALength < 0) or
      ((ALength and (not SizeInt(High(Integer)))) <> 0) then
    TextError(ALabel + ' exceeds the supported integer range');
  Result := Integer(ALength);
end;

function CheckedProduct(const A, B: Integer;
  const ALabel: String): Integer;
begin
  if (A < 0) or (B < 0) then
    TextError(ALabel + ' cannot be negative');
  if (A <> 0) and (B > High(Integer) div A) then
    TextError(ALabel + ' exceeds the supported integer range');
  Result := A * B;
end;

function CheckedRelationSlotCount(
  const APatternCount: Integer): Integer;
begin
  if APatternCount < 1 then
    TextError('patterns must be positive');
  if APatternCount > WFC_PATTERN_2D_MAX_PATTERN_COUNT then
    TextError('pattern count exceeds the version-1 limit');
  Result := CheckedProduct(APatternCount, APatternCount,
    'relation table dimensions');
  Result := CheckedProduct(WFC_PATTERN_DIRECTION_COUNT, Result,
    'relation table dimensions');
  if Result > WFC_PATTERN_2D_MAX_RELATION_SLOT_COUNT then
    TextError('relation table exceeds the version-1 slot limit');
end;

function CheckedLineCount(const ASourceCount, APaletteCount,
  APatternCount, ARelationCount: Integer): Integer;
begin
  Result := WFC_PATTERN_FIXED_LINE_COUNT;
  Result := CheckedAdd(Result, ASourceCount,
    'pattern text line count');
  Result := CheckedAdd(Result, APaletteCount,
    'pattern text line count');
  Result := CheckedAdd(Result, APatternCount,
    'pattern text line count');
  Result := CheckedAdd(Result, ARelationCount,
    'pattern text line count');
  if Result > WFC_PATTERN_2D_MAX_TEXT_LINE_COUNT then
    TextError('pattern text exceeds the version-1 line-count limit');
end;

function CheckedSourceCells(const AWidth, AHeight,
  ASourceIndex: Integer): Integer;
begin
  if (AWidth < 1) or (AHeight < 1) then
    TextError('sample dimensions must be positive');
  if (AWidth > WFC_PATTERN_2D_MAX_SOURCE_DIMENSION) or
      (AHeight > WFC_PATTERN_2D_MAX_SOURCE_DIMENSION) then
    TextError(Format(
      'sample %d dimension exceeds the version-1 limit', [ASourceIndex]));
  if AWidth > WFC_PATTERN_2D_MAX_SOURCE_CELL_COUNT div AHeight then
    TextError(Format(
      'sample %d cells exceed the version-1 limit', [ASourceIndex]));
  Result := AWidth * AHeight;
end;

procedure AccumulateSourceCells(var ATotal: Integer;
  const AWidth, AHeight, ASourceIndex: Integer);
var
  LCells: Integer;
begin
  LCells := CheckedSourceCells(AWidth, AHeight, ASourceIndex);
  if ATotal > WFC_PATTERN_2D_MAX_TOTAL_SOURCE_CELL_COUNT - LCells then
    TextError('aggregate sample cells exceed the version-1 limit');
  Inc(ATotal, LCells);
end;

function CheckedFootprintCells(const AWidth, AHeight: Integer): Integer;
begin
  if (AWidth < 1) or (AHeight < 1) then
    TextError('footprint dimensions must be positive');
  if (AWidth > WFC_PATTERN_2D_MAX_FOOTPRINT_DIMENSION) or
      (AHeight > WFC_PATTERN_2D_MAX_FOOTPRINT_DIMENSION) then
    TextError('footprint dimension exceeds the version-1 limit');
  if AWidth > WFC_PATTERN_2D_MAX_FOOTPRINT_CELL_COUNT div AHeight then
    TextError('footprint cells exceed the version-1 limit');
  Result := AWidth * AHeight;
end;

procedure ValidateBoundary(const ABoundary: TWfcModelBoundary);
begin
  case ABoundary of
    wmbOpen, wmbWrap:
      Exit;
  else
    TextError('boundary has an unknown value');
  end;
end;

procedure ValidateSymmetry(const ASymmetry: TWfcModelSymmetry);
begin
  case ASymmetry of
    wmsNone, wmsD4:
      Exit;
  else
    TextError('symmetry has an unknown value');
  end;
end;

function BoundaryName(const ABoundary: TWfcModelBoundary): String;
begin
  ValidateBoundary(ABoundary);
  if ABoundary = wmbOpen then
    Result := 'open'
  else
    Result := 'wrap';
end;

function SymmetryName(const ASymmetry: TWfcModelSymmetry): String;
begin
  ValidateSymmetry(ASymmetry);
  if ASymmetry = wmsNone then
    Result := 'none'
  else
    Result := 'd4';
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
    TextError('relation has an unknown direction');
  end;
end;

function ParseDirectionCode(
  const AText: String): TWfcModelDirection;
begin
  if AText = 'N' then
    Result := wmdNorth
  else if AText = 'E' then
    Result := wmdEast
  else if AText = 'S' then
    Result := wmdSouth
  else if AText = 'W' then
    Result := wmdWest
  else
    TextError('relation has an unknown direction');
end;

function RelationIndex(const ADirection: TWfcModelDirection;
  const ASource, ATarget, APatternCount: Integer): Integer;
begin
  Result := ((Ord(ADirection) * APatternCount + ASource) *
    APatternCount) + ATarget;
end;

function PayloadsEqual(const A, B: TWfcPattern2DPayload): Boolean;
var
  I: Integer;
begin
  if Length(A) <> Length(B) then
    Exit(False);
  for I := 0 to Length(A) - 1 do
    if A[I] <> B[I] then
      Exit(False);
  Result := True;
end;

procedure TransformToSource(const AX, AY, ASize,
  ATransform: Integer; out ASourceX, ASourceY: Integer);
var
  LRotation: Integer;
begin
  LRotation := ATransform mod 4;
  case LRotation of
    0:
      begin
        ASourceX := AX;
        ASourceY := AY;
      end;
    1:
      begin
        ASourceX := AY;
        ASourceY := ASize - 1 - AX;
      end;
    2:
      begin
        ASourceX := ASize - 1 - AX;
        ASourceY := ASize - 1 - AY;
      end;
    3:
      begin
        ASourceX := ASize - 1 - AY;
        ASourceY := AX;
      end;
  else
    TextError('D4 transform is out of range');
  end;
  if ATransform >= 4 then
    ASourceX := ASize - 1 - ASourceX;
end;

function PayloadMatchesTransform(const ASource,
  ATarget: TWfcPattern2DPayload; const ASize,
  ATransform: Integer): Boolean;
var
  LSourceX: Integer;
  LSourceY: Integer;
  X: Integer;
  Y: Integer;
begin
  for Y := 0 to ASize - 1 do
    for X := 0 to ASize - 1 do
    begin
      TransformToSource(X, Y, ASize, ATransform,
        LSourceX, LSourceY);
      if ATarget[Y * ASize + X] <>
          ASource[LSourceY * ASize + LSourceX] then
        Exit(False);
    end;
  Result := True;
end;

function PayloadsCompatible(const ASource, ATarget: TWfcPattern2DPayload;
  const APatternWidth, APatternHeight: Integer;
  const ADirection: TWfcModelDirection): Boolean;
var
  X: Integer;
  Y: Integer;
begin
  case ADirection of
    wmdNorth:
      for Y := 0 to APatternHeight - 2 do
        for X := 0 to APatternWidth - 1 do
          if ASource[Y * APatternWidth + X] <>
              ATarget[(Y + 1) * APatternWidth + X] then
            Exit(False);
    wmdEast:
      for Y := 0 to APatternHeight - 1 do
        for X := 1 to APatternWidth - 1 do
          if ASource[Y * APatternWidth + X] <>
              ATarget[Y * APatternWidth + X - 1] then
            Exit(False);
    wmdSouth:
      for Y := 1 to APatternHeight - 1 do
        for X := 0 to APatternWidth - 1 do
          if ASource[Y * APatternWidth + X] <>
              ATarget[(Y - 1) * APatternWidth + X] then
            Exit(False);
    wmdWest:
      for Y := 0 to APatternHeight - 1 do
        for X := 0 to APatternWidth - 2 do
          if ASource[Y * APatternWidth + X] <>
              ATarget[Y * APatternWidth + X + 1] then
            Exit(False);
  else
    TextError('relation has an unknown direction');
  end;
  Result := True;
end;

procedure ValidateD4Closure(const APatternWidth,
  APatternHeight: Integer; const APatterns: TWfcPattern2DPayloads;
  const AWeights: TWfcModelIntegerArray);
var
  I: Integer;
  J: Integer;
  LFound: Integer;
  LTransform: Integer;
begin
  if APatternWidth <> APatternHeight then
    TextError('D4 patterns require a square footprint');
  for I := 0 to Length(APatterns) - 1 do
    for LTransform := 0 to WFC_PATTERN_D4_TRANSFORM_COUNT - 1 do
    begin
      LFound := -1;
      for J := 0 to Length(APatterns) - 1 do
        if PayloadMatchesTransform(APatterns[I], APatterns[J],
            APatternWidth, LTransform) then
        begin
          LFound := J;
          Break;
        end;
      if LFound < 0 then
        TextError(Format(
          'pattern %d is not closed under D4 transform %d',
          [I, LTransform]));
      if AWeights[LFound] <> AWeights[I] then
        TextError(Format(
          'D4-equivalent patterns %d and %d must have equal weights',
          [I, LFound]));
    end;
end;

procedure ValidateArtifactData(const APatternWidth,
  APatternHeight: Integer; const ASourceBoundary: TWfcModelBoundary;
  const ASymmetry: TWfcModelSymmetry;
  const ASourceShapes: TWfcModelSampleShapes;
  const APalette: TWfcModelTokens;
  const APatterns: TWfcPattern2DPayloads;
  const AWeights: TWfcModelIntegerArray);
var
  I: Integer;
  J: Integer;
  LBaseOrigins: Integer;
  LExpectedObservations: Integer;
  LFootprintSize: Integer;
  LObservedWeight: Integer;
  LTransformCount: Integer;
  LUsedPalette: array of Boolean;
begin
  ValidateBoundary(ASourceBoundary);
  ValidateSymmetry(ASymmetry);
  if (APatternWidth < 1) or (APatternHeight < 1) then
    TextError('footprint dimensions must be positive');
  if (ASymmetry = wmsD4) and
      (APatternWidth <> APatternHeight) then
    TextError('D4 patterns require a square footprint');
  LFootprintSize := CheckedProduct(APatternWidth, APatternHeight,
    'pattern footprint');

  if CheckedArrayLength(Length(ASourceShapes), 'samples') < 1 then
    TextError('samples must be positive');
  if ASymmetry = wmsD4 then
    LTransformCount := WFC_PATTERN_D4_TRANSFORM_COUNT
  else
    LTransformCount := 1;
  LExpectedObservations := 0;
  for I := 0 to Length(ASourceShapes) - 1 do
  begin
    if (ASourceShapes[I].Width < 1) or
        (ASourceShapes[I].Height < 1) then
      TextError('sample dimensions must be positive');
    if ASourceBoundary = wmbOpen then
    begin
      if (ASourceShapes[I].Width < APatternWidth) or
          (ASourceShapes[I].Height < APatternHeight) then
        TextError(Format(
          'open sample %d is smaller than the footprint', [I]));
      LBaseOrigins := CheckedProduct(
        ASourceShapes[I].Width - APatternWidth + 1,
        ASourceShapes[I].Height - APatternHeight + 1,
        'open origin count');
    end
    else
      LBaseOrigins := CheckedProduct(ASourceShapes[I].Width,
        ASourceShapes[I].Height, 'wrapped origin count');
    LExpectedObservations := CheckedAdd(LExpectedObservations,
      CheckedProduct(LBaseOrigins, LTransformCount,
        'transformed observation count'),
      'corpus observation count');
  end;

  if CheckedArrayLength(Length(APalette), 'palette') < 1 then
    TextError('palette must be positive');
  for I := 0 to Length(APalette) - 1 do
  begin
    if not WfcModelTokenIsValid(APalette[I]) then
      TextError(Format(
        'palette token %d must be nonempty, well-formed UTF-8', [I]));
    for J := 0 to I - 1 do
      if APalette[I] = APalette[J] then
        TextError(Format('palette tokens %d and %d are duplicated',
          [J, I]));
  end;

  if CheckedArrayLength(Length(APatterns), 'patterns') < 1 then
    TextError('patterns must be positive');
  if Length(AWeights) <> Length(APatterns) then
    TextError('pattern weight count must match patterns');
  SetLength(LUsedPalette, Length(APalette));
  LObservedWeight := 0;
  for I := 0 to Length(APatterns) - 1 do
  begin
    if Length(APatterns[I]) <> LFootprintSize then
      TextError(Format('pattern %d payload has the wrong size', [I]));
    if AWeights[I] < 1 then
      TextError('pattern weights must be positive');
    LObservedWeight := CheckedAdd(LObservedWeight, AWeights[I],
      'pattern weight total');
    for J := 0 to LFootprintSize - 1 do
    begin
      if (APatterns[I][J] < 0) or
          (APatterns[I][J] >= Length(APalette)) then
        TextError(Format(
          'pattern %d palette index %d is out of range', [I, J]));
      LUsedPalette[APatterns[I][J]] := True;
    end;
    for J := 0 to I - 1 do
      if PayloadsEqual(APatterns[I], APatterns[J]) then
        TextError(Format('patterns %d and %d are duplicated', [J, I]));
  end;
  if LObservedWeight <> LExpectedObservations then
    TextError(Format(
      'pattern weights do not match source observations [%d <> %d]',
      [LObservedWeight, LExpectedObservations]));
  for I := 0 to Length(APalette) - 1 do
    if not LUsedPalette[I] then
      TextError(Format('palette token %d is unused', [I]));

  if ASymmetry = wmsD4 then
    ValidateD4Closure(APatternWidth, APatternHeight,
      APatterns, AWeights);
end;

procedure ValidateSerializedRelations(const APatternWidth,
  APatternHeight: Integer; const APatterns: TWfcPattern2DPayloads;
  const ARelations: TWfcPatternRelationFlags);
var
  D: TWfcModelDirection;
  I: Integer;
  J: Integer;
  LExpected: Boolean;
  LOpposite: TWfcModelDirection;
  LPatternCount: Integer;
  LSlot: Integer;
begin
  LPatternCount := Length(APatterns);
  if Length(ARelations) <> CheckedRelationSlotCount(LPatternCount) then
    TextError('relation table has the wrong size');
  for D := wmdNorth to wmdWest do
    for I := 0 to LPatternCount - 1 do
      for J := 0 to LPatternCount - 1 do
      begin
        LSlot := RelationIndex(D, I, J, LPatternCount);
        LExpected := PayloadsCompatible(APatterns[I], APatterns[J],
          APatternWidth, APatternHeight, D);
        if ARelations[LSlot] <> LExpected then
        begin
          if LExpected then
            TextError(Format(
              'structural relation %s,%d,%d is missing',
              [DirectionCode(D), I, J]))
          else
            TextError(Format(
              'relation %s,%d,%d is not structurally compatible',
              [DirectionCode(D), I, J]));
        end;
        LOpposite := OppositeModelDirection(D);
        if ARelations[LSlot] <>
            ARelations[RelationIndex(LOpposite, J, I,
              LPatternCount)] then
          TextError(Format('relation %s,%d,%d is not reciprocal',
            [DirectionCode(D), I, J]));
      end;
end;

procedure ValidateCompiledRelations(const AModel: TWfcOverlappingModel2D;
  const APatterns: TWfcPattern2DPayloads);
var
  D: TWfcModelDirection;
  I: Integer;
  J: Integer;
  LActual: Integer;
  LExpected: Boolean;
begin
  if not Assigned(AModel.CompiledModel) then
    TextError('compiled model is not assigned');
  if (AModel.CompiledModel.Rank <> 2) or
      (AModel.CompiledModel.ValueCount <> AModel.PatternCount) or
      (AModel.CompiledModel.Directions <>
        [wmdNorth, wmdEast, wmdSouth, wmdWest]) then
    TextError('compiled model metadata does not match the pattern model');
  for D := wmdNorth to wmdWest do
    for I := 0 to AModel.PatternCount - 1 do
      for J := 0 to AModel.PatternCount - 1 do
      begin
        LExpected := PayloadsCompatible(APatterns[I], APatterns[J],
          AModel.PatternWidth, AModel.PatternHeight, D);
        LActual := AModel.CompiledModel.RelationCount(D, I, J);
        if (LExpected and (LActual <> 1)) or
            ((not LExpected) and (LActual <> 0)) then
          TextError(Format(
            'compiled relation %s,%d,%d does not match its payloads',
            [DirectionCode(D), I, J]));
        if AModel.PatternsCompatible(I, J, D) <> LExpected then
          TextError(Format(
            'pattern compatibility %s,%d,%d is inconsistent',
            [DirectionCode(D), I, J]));
      end;
end;

function RequireLine(const ALines: TWfcTextLines;
  const AIndex: Integer; const AFieldName: String): String;
begin
  if (AIndex < 0) or (AIndex >= Length(ALines)) then
    TextError(AFieldName + ' is missing');
  Result := ALines[AIndex];
end;

procedure ParseShapeLine(const ALine: String;
  const AExpectedIndex: Integer; out AShape: TWfcModelSampleShape);
var
  LComma1: Integer;
  LComma2: Integer;
  LIndex: Integer;
begin
  if Copy(ALine, 1, 2) <> 's=' then
    TextError('expected sample record');
  LComma1 := WfcTextFindCharacter(ALine, ',', 3);
  LComma2 := WfcTextFindCharacter(ALine, ',', LComma1 + 1);
  if (LComma1 = 0) or (LComma2 = 0) then
    TextError('sample record is missing fields');
  if WfcTextFindCharacter(ALine, ',', LComma2 + 1) <> 0 then
    TextError('sample record has extra fields');
  LIndex := ParseCanonicalInteger(Copy(ALine, 3,
    LComma1 - 3), 'sample index');
  if LIndex <> AExpectedIndex then
    TextError('sample indices must be complete and ordered');
  AShape.Width := ParseCanonicalInteger(Copy(ALine, LComma1 + 1,
    LComma2 - LComma1 - 1), 'sample width');
  AShape.Height := ParseCanonicalInteger(Copy(ALine, LComma2 + 1,
    Length(ALine) - LComma2), 'sample height');
  CheckedSourceCells(AShape.Width, AShape.Height, AExpectedIndex);
end;

procedure ParseFootprintLine(const ALine: String;
  out AWidth, AHeight: Integer);
var
  LComma: Integer;
begin
  if Copy(ALine, 1, 10) <> 'footprint=' then
    TextError('expected footprint');
  LComma := WfcTextFindCharacter(ALine, ',', 11);
  if LComma = 0 then
    TextError('footprint is missing a dimension');
  if WfcTextFindCharacter(ALine, ',', LComma + 1) <> 0 then
    TextError('footprint has extra fields');
  AWidth := ParseCanonicalInteger(Copy(ALine, 11,
    LComma - 11), 'footprint width');
  AHeight := ParseCanonicalInteger(Copy(ALine, LComma + 1,
    Length(ALine) - LComma), 'footprint height');
  CheckedFootprintCells(AWidth, AHeight);
end;

procedure ParsePaletteLine(const ALine: String;
  const AExpectedIndex: Integer; out AToken: TWfcModelToken);
var
  LComma: Integer;
  LIndex: Integer;
begin
  if Copy(ALine, 1, 2) <> 't=' then
    TextError('expected palette record');
  LComma := WfcTextFindCharacter(ALine, ',', 3);
  if LComma = 0 then
    TextError('palette record is missing its token');
  if WfcTextFindCharacter(ALine, ',', LComma + 1) <> 0 then
    TextError('palette record contains an unescaped comma');
  LIndex := ParseCanonicalInteger(Copy(ALine, 3,
    LComma - 3), 'palette index');
  if LIndex <> AExpectedIndex then
    TextError('palette indices must be complete and ordered');
  AToken := WfcTextDecodeToken(Copy(ALine, LComma + 1,
    Length(ALine) - LComma), WFC_PATTERN_TEXT_ARTIFACT);
end;

procedure ParsePatternLine(const ALine: String;
  const AExpectedIndex, AFootprintSize,
  APaletteCount: Integer; out APayload: TWfcPattern2DPayload;
  out AWeight: Integer);
var
  I: Integer;
  LComma: Integer;
  LField: String;
  LIndex: Integer;
  LStart: Integer;
begin
  if Copy(ALine, 1, 2) <> 'p=' then
    TextError('expected pattern record');
  LStart := 3;
  LComma := WfcTextFindCharacter(ALine, ',', LStart);
  if LComma = 0 then
    TextError('pattern record is missing fields');
  LIndex := ParseCanonicalInteger(Copy(ALine, LStart,
    LComma - LStart), 'pattern index');
  if LIndex <> AExpectedIndex then
    TextError('pattern indices must be complete and ordered');
  LStart := LComma + 1;
  LComma := WfcTextFindCharacter(ALine, ',', LStart);
  if LComma = 0 then
    TextError('pattern record is missing its payload');
  AWeight := ParseCanonicalInteger(Copy(ALine, LStart,
    LComma - LStart), 'pattern weight');
  if AWeight < 1 then
    TextError('pattern weights must be positive');
  LStart := LComma + 1;

  SetLength(APayload, AFootprintSize);
  for I := 0 to AFootprintSize - 1 do
  begin
    LComma := WfcTextFindCharacter(ALine, ',', LStart);
    if I < AFootprintSize - 1 then
    begin
      if LComma = 0 then
        TextError('pattern payload is incomplete');
      LField := Copy(ALine, LStart, LComma - LStart);
      LStart := LComma + 1;
    end
    else
    begin
      if LComma <> 0 then
        TextError('pattern payload has extra cells');
      LField := Copy(ALine, LStart, Length(ALine) - LStart + 1);
    end;
    APayload[I] := ParseCanonicalInteger(LField,
      'pattern palette index');
    if APayload[I] >= APaletteCount then
      TextError('pattern palette index is out of range');
  end;
end;

procedure ParseRelationLine(const ALine, AExpectedPreviousLabel: String;
  const APatternCount, APreviousSlot: Integer;
  out ASlot: Integer);
var
  LComma1: Integer;
  LComma2: Integer;
  LDirection: TWfcModelDirection;
  LSource: Integer;
  LTarget: Integer;
begin
  if Copy(ALine, 1, 2) <> 'r=' then
    TextError('expected relation record');
  LComma1 := WfcTextFindCharacter(ALine, ',', 3);
  if LComma1 = 0 then
    TextError('relation record is missing fields');
  LComma2 := WfcTextFindCharacter(ALine, ',', LComma1 + 1);
  if LComma2 = 0 then
    TextError('relation record is missing fields');
  if WfcTextFindCharacter(ALine, ',', LComma2 + 1) <> 0 then
    TextError('relation record has extra fields');
  LDirection := ParseDirectionCode(Copy(ALine, 3,
    LComma1 - 3));
  LSource := ParseCanonicalInteger(Copy(ALine, LComma1 + 1,
    LComma2 - LComma1 - 1), 'relation source');
  LTarget := ParseCanonicalInteger(Copy(ALine, LComma2 + 1,
    Length(ALine) - LComma2), 'relation target');
  if (LSource >= APatternCount) or (LTarget >= APatternCount) then
    TextError('relation pattern index is out of range');
  ASlot := RelationIndex(LDirection, LSource, LTarget,
    APatternCount);
  if ASlot <= APreviousSlot then
    TextError(AExpectedPreviousLabel);
end;

function EncodeWfcPattern2DText(
  const AModel: TWfcOverlappingModel2D): String;
var
  D: TWfcModelDirection;
  I: Integer;
  J: Integer;
  K: Integer;
  LLine: String;
  LLineIndex: Integer;
  LLines: TWfcTextLines;
  LPalette: TWfcModelTokens;
  LPatterns: TWfcPattern2DPayloads;
  LRelationCount: Integer;
  LShapes: TWfcModelSampleShapes;
  LWeights: TWfcModelIntegerArray;
begin
  if not Assigned(AModel) then
    raise EArgumentNilException.Create(
      'WFC overlapping pattern model cannot be nil');
  LShapes := AModel.CopySourceShapes;
  LPalette := AModel.CopyPalette;
  LPatterns := AModel.CopyPatterns;
  LWeights := AModel.CopyPatternWeights;
  ValidateArtifactData(AModel.PatternWidth, AModel.PatternHeight,
    AModel.SourceBoundary, AModel.Symmetry, LShapes, LPalette,
    LPatterns, LWeights);
  ValidateCompiledRelations(AModel, LPatterns);

  LRelationCount := 0;
  for D := wmdNorth to wmdWest do
    for I := 0 to AModel.PatternCount - 1 do
      for J := 0 to AModel.PatternCount - 1 do
        if PayloadsCompatible(LPatterns[I], LPatterns[J],
            AModel.PatternWidth, AModel.PatternHeight, D) then
          LRelationCount := CheckedAdd(LRelationCount, 1,
            'serialized relation count');

  SetLength(LLines, CheckedLineCount(AModel.SourceCount,
    AModel.PaletteCount, AModel.PatternCount, LRelationCount));
  LLineIndex := 0;
  LLines[LLineIndex] := 'wfcp=' +
    IntToStr(WFC_PATTERN_2D_TEXT_VERSION);
  Inc(LLineIndex);
  LLines[LLineIndex] := 'rank=2';
  Inc(LLineIndex);
  LLines[LLineIndex] := 'samples=' + IntToStr(AModel.SourceCount);
  Inc(LLineIndex);
  for I := 0 to AModel.SourceCount - 1 do
  begin
    LLines[LLineIndex] := 's=' + IntToStr(I) + ',' +
      IntToStr(LShapes[I].Width) + ',' +
      IntToStr(LShapes[I].Height);
    Inc(LLineIndex);
  end;
  LLines[LLineIndex] := 'footprint=' +
    IntToStr(AModel.PatternWidth) + ',' +
    IntToStr(AModel.PatternHeight);
  Inc(LLineIndex);
  LLines[LLineIndex] := 'boundary=' +
    BoundaryName(AModel.SourceBoundary);
  Inc(LLineIndex);
  LLines[LLineIndex] := 'symmetry=' + SymmetryName(AModel.Symmetry);
  Inc(LLineIndex);
  LLines[LLineIndex] := 'directions=N,E,S,W';
  Inc(LLineIndex);
  LLines[LLineIndex] := 'palette=' + IntToStr(AModel.PaletteCount);
  Inc(LLineIndex);
  for I := 0 to AModel.PaletteCount - 1 do
  begin
    LLines[LLineIndex] := 't=' + IntToStr(I) + ',' +
      WfcTextEncodeToken(LPalette[I], WFC_PATTERN_TEXT_ARTIFACT);
    Inc(LLineIndex);
  end;
  LLines[LLineIndex] := 'patterns=' + IntToStr(AModel.PatternCount);
  Inc(LLineIndex);
  for I := 0 to AModel.PatternCount - 1 do
  begin
    LLine := 'p=' + IntToStr(I) + ',' + IntToStr(LWeights[I]);
    for K := 0 to Length(LPatterns[I]) - 1 do
      LLine := LLine + ',' + IntToStr(LPatterns[I][K]);
    LLines[LLineIndex] := LLine;
    Inc(LLineIndex);
  end;
  LLines[LLineIndex] := 'relations=' + IntToStr(LRelationCount);
  Inc(LLineIndex);
  for D := wmdNorth to wmdWest do
    for I := 0 to AModel.PatternCount - 1 do
      for J := 0 to AModel.PatternCount - 1 do
        if PayloadsCompatible(LPatterns[I], LPatterns[J],
            AModel.PatternWidth, AModel.PatternHeight, D) then
        begin
          LLines[LLineIndex] := 'r=' + DirectionCode(D) + ',' +
            IntToStr(I) + ',' + IntToStr(J);
          Inc(LLineIndex);
        end;
  LLines[LLineIndex] := 'end';
  RequireEncodedTextLength(LLines);
  Result := WfcTextJoinCanonicalLines(LLines,
    WFC_PATTERN_TEXT_ARTIFACT);
end;

function DecodeWfcPattern2DText(
  const AText: String): TWfcOverlappingModel2D;
var
  I: Integer;
  LBoundary: TWfcModelBoundary;
  LFootprintSize: Integer;
  LLineIndex: Integer;
  LLines: TWfcTextLines;
  LPalette: TWfcModelTokens;
  LPaletteCount: Integer;
  LPatternCount: Integer;
  LPatternHeight: Integer;
  LPatterns: TWfcPattern2DPayloads;
  LPatternWidth: Integer;
  LPreviousSlot: Integer;
  LRank: Integer;
  LRelationCount: Integer;
  LRelations: TWfcPatternRelationFlags;
  LRelationSlots: Integer;
  LSampleCount: Integer;
  LShapes: TWfcModelSampleShapes;
  LSlot: Integer;
  LSymmetry: TWfcModelSymmetry;
  LTextValue: String;
  LTotalSourceCells: Integer;
  LWeights: TWfcModelIntegerArray;
begin
  Result := nil;
  PreflightTextEnvelope(AText);
  WfcTextSplitCanonicalLines(AText, WFC_PATTERN_TEXT_ARTIFACT,
    LLines);
  if Length(LLines) < WFC_PATTERN_FIXED_LINE_COUNT then
    TextError('document is incomplete');
  LLineIndex := 0;
  if RequireLine(LLines, LLineIndex, 'format version') <> 'wfcp=1' then
    TextError('unsupported or noncanonical format version');
  Inc(LLineIndex);

  LRank := ParseCanonicalInteger(ValueAfterPrefix(
    RequireLine(LLines, LLineIndex, 'rank'), 'rank=', 'rank'), 'rank');
  if LRank <> 2 then
    TextError('rank must be exactly 2');
  Inc(LLineIndex);

  LSampleCount := ParseCanonicalInteger(ValueAfterPrefix(
    RequireLine(LLines, LLineIndex, 'samples'), 'samples=', 'samples'),
    'samples');
  Inc(LLineIndex);
  if LSampleCount < 1 then
    TextError('samples must be positive');
  if LSampleCount > WFC_PATTERN_2D_MAX_SOURCE_COUNT then
    TextError('sample count exceeds the version-1 limit');
  { Eight fixed lines remain after the sample records. }
  if LSampleCount > Length(LLines) - LLineIndex - 8 then
    TextError('sample records are incomplete');
  SetLength(LShapes, LSampleCount);
  LTotalSourceCells := 0;
  for I := 0 to LSampleCount - 1 do
  begin
    ParseShapeLine(RequireLine(LLines, LLineIndex,
      'sample record'), I, LShapes[I]);
    AccumulateSourceCells(LTotalSourceCells, LShapes[I].Width,
      LShapes[I].Height, I);
    Inc(LLineIndex);
  end;

  ParseFootprintLine(RequireLine(LLines, LLineIndex,
    'footprint'), LPatternWidth, LPatternHeight);
  LFootprintSize := CheckedFootprintCells(LPatternWidth, LPatternHeight);
  Inc(LLineIndex);

  LTextValue := ValueAfterPrefix(RequireLine(LLines, LLineIndex,
    'boundary'), 'boundary=', 'boundary');
  if LTextValue = 'open' then
    LBoundary := wmbOpen
  else if LTextValue = 'wrap' then
    LBoundary := wmbWrap
  else
    TextError('boundary has an unknown value');
  Inc(LLineIndex);

  LTextValue := ValueAfterPrefix(RequireLine(LLines, LLineIndex,
    'symmetry'), 'symmetry=', 'symmetry');
  if LTextValue = 'none' then
    LSymmetry := wmsNone
  else if LTextValue = 'd4' then
    LSymmetry := wmsD4
  else
    TextError('symmetry has an unknown value');
  Inc(LLineIndex);

  if RequireLine(LLines, LLineIndex, 'directions') <>
      'directions=N,E,S,W' then
    TextError('directions must be exactly N,E,S,W');
  Inc(LLineIndex);

  LPaletteCount := ParseCanonicalInteger(ValueAfterPrefix(
    RequireLine(LLines, LLineIndex, 'palette'),
    'palette=', 'palette'), 'palette');
  Inc(LLineIndex);
  if LPaletteCount < 1 then
    TextError('palette must be positive');
  if LPaletteCount > WFC_PATTERN_2D_MAX_PALETTE_COUNT then
    TextError('palette count exceeds the version-1 limit');
  { The patterns field, at least one pattern, relations, and end remain. }
  if LPaletteCount > Length(LLines) - LLineIndex - 4 then
    TextError('palette records are incomplete');
  SetLength(LPalette, LPaletteCount);
  for I := 0 to LPaletteCount - 1 do
  begin
    ParsePaletteLine(RequireLine(LLines, LLineIndex,
      'palette record'), I, LPalette[I]);
    Inc(LLineIndex);
  end;

  LPatternCount := ParseCanonicalInteger(ValueAfterPrefix(
    RequireLine(LLines, LLineIndex, 'patterns'),
    'patterns=', 'patterns'), 'patterns');
  Inc(LLineIndex);
  if LPatternCount < 1 then
    TextError('patterns must be positive');
  if LPatternCount > WFC_PATTERN_2D_MAX_PATTERN_COUNT then
    TextError('pattern count exceeds the version-1 limit');
  { The relations field and end remain after the pattern records. }
  if LPatternCount > Length(LLines) - LLineIndex - 2 then
    TextError('pattern records are incomplete');
  LRelationSlots := CheckedRelationSlotCount(LPatternCount);
  if LPatternCount > WFC_PATTERN_2D_MAX_TOTAL_PATTERN_CELL_COUNT div
      LFootprintSize then
    TextError('aggregate pattern cells exceed the version-1 limit');
  SetLength(LPatterns, LPatternCount);
  SetLength(LWeights, LPatternCount);
  for I := 0 to LPatternCount - 1 do
  begin
    ParsePatternLine(RequireLine(LLines, LLineIndex,
      'pattern record'), I, LFootprintSize, LPaletteCount,
      LPatterns[I], LWeights[I]);
    Inc(LLineIndex);
  end;

  LRelationCount := ParseCanonicalInteger(ValueAfterPrefix(
    RequireLine(LLines, LLineIndex, 'relations'),
    'relations=', 'relations'), 'relations');
  Inc(LLineIndex);
  if LRelationCount > LRelationSlots then
    TextError('relations exceeds the relation table size');
  if LRelationCount > Length(LLines) - LLineIndex - 1 then
    TextError('relation records are incomplete');
  SetLength(LRelations, LRelationSlots);
  LPreviousSlot := -1;
  for I := 0 to LRelationCount - 1 do
  begin
    ParseRelationLine(RequireLine(LLines, LLineIndex,
      'relation record'),
      'relations must be unique and canonically ordered',
      LPatternCount, LPreviousSlot, LSlot);
    LRelations[LSlot] := True;
    LPreviousSlot := LSlot;
    Inc(LLineIndex);
  end;

  if RequireLine(LLines, LLineIndex, 'end marker') <> 'end' then
    TextError('end marker is missing or misplaced');
  Inc(LLineIndex);
  if LLineIndex <> Length(LLines) then
    TextError('trailing data is not permitted');

  ValidateArtifactData(LPatternWidth, LPatternHeight, LBoundary,
    LSymmetry, LShapes, LPalette, LPatterns, LWeights);
  ValidateSerializedRelations(LPatternWidth, LPatternHeight,
    LPatterns, LRelations);
  try
    Result := TWfcOverlappingModel2D.Create(LPatternWidth,
      LPatternHeight, LBoundary, LSymmetry, LShapes, LPalette,
      LPatterns, LWeights);
  except
    on E: EWfcModel do
      raise EConvertError.Create('invalid ' +
        WFC_PATTERN_TEXT_ARTIFACT + ' text: ' + E.Message);
  end;
  try
    ValidateCompiledRelations(Result, LPatterns);
    if EncodeWfcPattern2DText(Result) <> AText then
      TextError('document is not in canonical form');
  except
    Result.Free;
    Result := nil;
    raise;
  end;
end;

end.
