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
unit wfc_pattern2d_learn;

{$mode delphi}{$H+}

interface

uses
  wfc_model,
  wfc_learn,
  wfc_pattern2d;

function LearnOverlappingModel2DCorpus(
  const ASamples: TWfcLearnSamples;
  const APatternWidth, APatternHeight: Integer;
  const ASourceBoundary: TWfcModelBoundary;
  const ASymmetry: TWfcModelSymmetry): TWfcOverlappingModel2D;

function LearnOverlappingModel2D(const ATokens: TWfcModelTokens;
  const ASourceWidth, ASourceHeight: Integer;
  const APatternWidth, APatternHeight: Integer;
  const ASourceBoundary: TWfcModelBoundary;
  const ASymmetry: TWfcModelSymmetry): TWfcOverlappingModel2D;

implementation

uses
  SysUtils;

const
  D4_TRANSFORM_COUNT = 8;

type
  TIntArray = array of Integer;
  TIntArrays = array of TIntArray;

function CheckedLength(const ALength: SizeInt;
  const ALabel: String): Integer;
begin
  if (ALength < 0) or
      ((ALength and (not SizeInt(High(Integer)))) <> 0) then
    raise EWfcOverlapping2D.Create(ALabel + ' exceeds the Integer range');
  Result := Integer(ALength);
end;

function CheckedProduct(const A, B: Integer;
  const ALabel: String): Integer;
begin
  if (A < 0) or (B < 0) then
    raise EWfcOverlapping2D.Create(ALabel + ' cannot be negative');
  if (A <> 0) and (B > High(Integer) div A) then
    raise EWfcOverlapping2D.Create(ALabel + ' exceeds the Integer range');
  Result := A * B;
end;

function CheckedAdd(const A, B: Integer;
  const ALabel: String): Integer;
begin
  if (A < 0) or (B < 0) then
    raise EWfcOverlapping2D.Create(ALabel + ' cannot be negative');
  if A > High(Integer) - B then
    raise EWfcOverlapping2D.Create(ALabel + ' exceeds the Integer range');
  Result := A + B;
end;

procedure CheckedIncrement(var AValue: Integer; const ALabel: String);
begin
  if AValue = High(Integer) then
    raise EWfcOverlapping2D.Create(ALabel +
      ' exceeds the Integer count range');
  Inc(AValue);
end;

procedure ValidateBoundary(const ABoundary: TWfcModelBoundary);
begin
  case ABoundary of
    wmbOpen, wmbWrap:
      Exit;
  else
    raise EWfcOverlapping2D.CreateFmt(
      'unknown overlapping source boundary [%d]', [Ord(ABoundary)]);
  end;
end;

procedure ValidateSymmetry(const ASymmetry: TWfcModelSymmetry);
begin
  case ASymmetry of
    wmsNone, wmsD4:
      Exit;
  else
    raise EWfcOverlapping2D.CreateFmt(
      'unknown overlapping symmetry [%d]', [Ord(ASymmetry)]);
  end;
end;

function FindToken(const APalette: TWfcModelTokens;
  const AToken: TWfcModelToken): Integer;
var
  I: Integer;
begin
  for I := 0 to Length(APalette) - 1 do
    if APalette[I] = AToken then
      Exit(I);
  Result := -1;
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

function FindPattern(const APatterns: TWfcPattern2DPayloads;
  const APayload: TWfcPattern2DPayload): Integer;
var
  I: Integer;
begin
  for I := 0 to Length(APatterns) - 1 do
    if PayloadsEqual(APatterns[I], APayload) then
      Exit(I);
  Result := -1;
end;

procedure CheckRelationCapacity(const APatternCount: Integer);
var
  LSquare: Integer;
begin
  if APatternCount < 1 then
    Exit;
  if APatternCount > High(Integer) div APatternCount then
    raise EWfcOverlapping2D.Create(
      'overlapping relation dimensions exceed the Integer range');
  LSquare := APatternCount * APatternCount;
  if LSquare > High(Integer) div 4 then
    raise EWfcOverlapping2D.Create(
      'overlapping relation dimensions exceed the Integer range');
end;

procedure TransformDimensions(const ASourceWidth, ASourceHeight,
  ATransform: Integer; out AWidth, AHeight: Integer);
var
  LRotation: Integer;
begin
  LRotation := ATransform mod 4;
  if (LRotation = 1) or (LRotation = 3) then
  begin
    AWidth := ASourceHeight;
    AHeight := ASourceWidth;
  end
  else
  begin
    AWidth := ASourceWidth;
    AHeight := ASourceHeight;
  end;
end;

(*
  This is the same explicit transform contract used by wfc_learn. Transforms
  0..3 are identity and clockwise quarter turns. Transforms 4..7 first mirror
  the X coordinate, then apply the corresponding turn.
*)
procedure TransformToSource(const AX, AY, ASourceWidth, ASourceHeight,
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
        ASourceY := ASourceHeight - 1 - AX;
      end;
    2:
      begin
        ASourceX := ASourceWidth - 1 - AX;
        ASourceY := ASourceHeight - 1 - AY;
      end;
    3:
      begin
        ASourceX := ASourceWidth - 1 - AY;
        ASourceY := AX;
      end;
  else
    raise ERangeError.Create('unknown overlapping D4 transform');
  end;
  if ATransform >= 4 then
    ASourceX := ASourceWidth - 1 - ASourceX;
end;

function TransformedValueAt(const ASourceValues: TIntArray;
  const ASourceWidth, ASourceHeight, ATransform, AX,
  AY: Integer): Integer;
var
  LSourceX: Integer;
  LSourceY: Integer;
begin
  TransformToSource(AX, AY, ASourceWidth, ASourceHeight, ATransform,
    LSourceX, LSourceY);
  Result := ASourceValues[LSourceY * ASourceWidth + LSourceX];
end;

function SafeModuloAdd(const ABase, AOffset,
  AModulus: Integer): Integer;
var
  LOffset: Integer;
begin
  LOffset := AOffset mod AModulus;
  if LOffset = 0 then
    Exit(ABase);
  if ABase >= AModulus - LOffset then
    Result := ABase - (AModulus - LOffset)
  else
    Result := ABase + LOffset;
end;

procedure AppendPattern(const APayload: TWfcPattern2DPayload;
  var APatterns: TWfcPattern2DPayloads;
  var AWeights: TWfcModelIntegerArray);
var
  I: Integer;
  LCount: Integer;
begin
  LCount := CheckedLength(Length(APatterns),
    'overlapping pattern count');
  if LCount = High(Integer) then
    raise EWfcOverlapping2D.Create(
      'overlapping pattern count exceeds the Integer range');
  CheckRelationCapacity(LCount + 1);
  SetLength(APatterns, LCount + 1);
  SetLength(APatterns[LCount], Length(APayload));
  for I := 0 to Length(APayload) - 1 do
    APatterns[LCount][I] := APayload[I];
  SetLength(AWeights, LCount + 1);
  AWeights[LCount] := 0;
end;

function LearnOverlappingModel2DCorpus(
  const ASamples: TWfcLearnSamples;
  const APatternWidth, APatternHeight: Integer;
  const ASourceBoundary: TWfcModelBoundary;
  const ASymmetry: TWfcModelSymmetry): TWfcOverlappingModel2D;
var
  I: Integer;
  LBaseOrigins: Integer;
  LExpectedObservations: Integer;
  LExpectedSize: Integer;
  LFootprintSize: Integer;
  LOriginLimitX: Integer;
  LOriginLimitY: Integer;
  LPalette: TWfcModelTokens;
  LPaletteIndex: Integer;
  LPattern: Integer;
  LPatterns: TWfcPattern2DPayloads;
  LPatternWeights: TWfcModelIntegerArray;
  LPayload: TWfcPattern2DPayload;
  LSampleCount: Integer;
  LSampleIndex: Integer;
  LSampleShapes: TWfcModelSampleShapes;
  LSourceX: Integer;
  LSourceY: Integer;
  LTransform: Integer;
  LTransformCount: Integer;
  LTransformedWidth: Integer;
  LTransformedHeight: Integer;
  LValues: TIntArrays;
  LObservedCount: Integer;
  X: Integer;
  Y: Integer;
  PX: Integer;
  PY: Integer;
begin
  ValidateBoundary(ASourceBoundary);
  ValidateSymmetry(ASymmetry);
  if (APatternWidth < 1) or (APatternHeight < 1) then
    raise EWfcOverlapping2D.Create(
      'overlapping pattern dimensions must be positive');
  if (ASymmetry = wmsD4) and
      (APatternWidth <> APatternHeight) then
    raise EWfcOverlapping2D.Create(
      'D4 overlapping patterns must have a square footprint');
  LFootprintSize := CheckedProduct(APatternWidth, APatternHeight,
    'overlapping pattern footprint');
  LSampleCount := CheckedLength(Length(ASamples),
    'overlapping corpus sample count');
  if LSampleCount = 0 then
    raise EWfcOverlapping2D.Create(
      'an overlapping learning corpus cannot be empty');
  if ASymmetry = wmsD4 then
    LTransformCount := D4_TRANSFORM_COUNT
  else
    LTransformCount := 1;

  SetLength(LSampleShapes, LSampleCount);
  SetLength(LValues, LSampleCount);
  LExpectedObservations := 0;
  for LSampleIndex := 0 to LSampleCount - 1 do
  begin
    if (ASamples[LSampleIndex].Width < 1) or
        (ASamples[LSampleIndex].Height < 1) then
      raise EWfcOverlapping2D.CreateFmt(
        'overlapping sample dimensions must be positive [%d: %d x %d]',
        [LSampleIndex, ASamples[LSampleIndex].Width,
          ASamples[LSampleIndex].Height]);
    LExpectedSize := CheckedProduct(ASamples[LSampleIndex].Width,
      ASamples[LSampleIndex].Height, 'overlapping sample dimensions');
    if CheckedLength(Length(ASamples[LSampleIndex].Tokens),
        Format('overlapping sample %d token count', [LSampleIndex])) <>
        LExpectedSize then
      raise EWfcOverlapping2D.CreateFmt(
        'overlapping sample %d has %d tokens; expected %d',
        [LSampleIndex, Length(ASamples[LSampleIndex].Tokens),
          LExpectedSize]);
    if (ASourceBoundary = wmbOpen) and
        ((ASamples[LSampleIndex].Width < APatternWidth) or
         (ASamples[LSampleIndex].Height < APatternHeight)) then
      raise EWfcOverlapping2D.CreateFmt(
        'open overlapping sample %d is smaller than the pattern [%d x %d < %d x %d]',
        [LSampleIndex, ASamples[LSampleIndex].Width,
          ASamples[LSampleIndex].Height, APatternWidth, APatternHeight]);
    if ASourceBoundary = wmbOpen then
      LBaseOrigins := CheckedProduct(
        ASamples[LSampleIndex].Width - APatternWidth + 1,
        ASamples[LSampleIndex].Height - APatternHeight + 1,
        'open overlapping origin count')
    else
      LBaseOrigins := LExpectedSize;
    LExpectedObservations := CheckedAdd(LExpectedObservations,
      CheckedProduct(LBaseOrigins, LTransformCount,
        'overlapping transformed observation count'),
      'overlapping corpus observation count');
    LSampleShapes[LSampleIndex] := MakeWfcModelSampleShape(
      ASamples[LSampleIndex].Width, ASamples[LSampleIndex].Height);
  end;

  SetLength(LPalette, 0);
  for LSampleIndex := 0 to LSampleCount - 1 do
  begin
    SetLength(LValues[LSampleIndex],
      Length(ASamples[LSampleIndex].Tokens));
    for I := 0 to Length(ASamples[LSampleIndex].Tokens) - 1 do
    begin
      if not WfcModelTokenIsValid(ASamples[LSampleIndex].Tokens[I]) then
        raise EWfcOverlapping2D.CreateFmt(
          'overlapping sample %d token %d must be nonempty, well-formed UTF-8',
          [LSampleIndex, I]);
      LPaletteIndex := FindToken(LPalette,
        ASamples[LSampleIndex].Tokens[I]);
      if LPaletteIndex < 0 then
      begin
        if Length(LPalette) >= High(Integer) then
          raise EWfcOverlapping2D.Create(
            'overlapping palette count exceeds the Integer range');
        LPaletteIndex := Integer(Length(LPalette));
        SetLength(LPalette, LPaletteIndex + 1);
        LPalette[LPaletteIndex] := ASamples[LSampleIndex].Tokens[I];
      end;
      LValues[LSampleIndex][I] := LPaletteIndex;
    end;
  end;

  SetLength(LPatterns, 0);
  SetLength(LPatternWeights, 0);
  SetLength(LPayload, LFootprintSize);
  LObservedCount := 0;
  for LSampleIndex := 0 to LSampleCount - 1 do
    for LTransform := 0 to LTransformCount - 1 do
    begin
      TransformDimensions(ASamples[LSampleIndex].Width,
        ASamples[LSampleIndex].Height, LTransform,
        LTransformedWidth, LTransformedHeight);
      if ASourceBoundary = wmbOpen then
      begin
        LOriginLimitX := LTransformedWidth - APatternWidth + 1;
        LOriginLimitY := LTransformedHeight - APatternHeight + 1;
      end
      else
      begin
        LOriginLimitX := LTransformedWidth;
        LOriginLimitY := LTransformedHeight;
      end;
      for Y := 0 to LOriginLimitY - 1 do
        for X := 0 to LOriginLimitX - 1 do
        begin
          for PY := 0 to APatternHeight - 1 do
            for PX := 0 to APatternWidth - 1 do
            begin
              if ASourceBoundary = wmbWrap then
              begin
                LSourceX := SafeModuloAdd(X, PX,
                  LTransformedWidth);
                LSourceY := SafeModuloAdd(Y, PY,
                  LTransformedHeight);
              end
              else
              begin
                LSourceX := X + PX;
                LSourceY := Y + PY;
              end;
              LPayload[PY * APatternWidth + PX] :=
                TransformedValueAt(LValues[LSampleIndex],
                  ASamples[LSampleIndex].Width,
                  ASamples[LSampleIndex].Height, LTransform,
                  LSourceX, LSourceY);
            end;
          LPattern := FindPattern(LPatterns, LPayload);
          if LPattern < 0 then
          begin
            AppendPattern(LPayload, LPatterns, LPatternWeights);
            LPattern := High(LPatterns);
          end;
          CheckedIncrement(LPatternWeights[LPattern],
            'overlapping pattern frequency');
          CheckedIncrement(LObservedCount,
            'overlapping observation count');
        end;
    end;
  if LObservedCount <> LExpectedObservations then
    raise EWfcOverlapping2D.CreateFmt(
      'internal overlapping observation count mismatch [%d <> %d]',
      [LObservedCount, LExpectedObservations]);
  Result := TWfcOverlappingModel2D.Create(APatternWidth,
    APatternHeight, ASourceBoundary, ASymmetry, LSampleShapes,
    LPalette, LPatterns, LPatternWeights);
end;

function LearnOverlappingModel2D(const ATokens: TWfcModelTokens;
  const ASourceWidth, ASourceHeight: Integer;
  const APatternWidth, APatternHeight: Integer;
  const ASourceBoundary: TWfcModelBoundary;
  const ASymmetry: TWfcModelSymmetry): TWfcOverlappingModel2D;
var
  LSamples: TWfcLearnSamples;
begin
  SetLength(LSamples, 1);
  LSamples[0] := MakeLearnSample2D(ATokens,
    ASourceWidth, ASourceHeight);
  Result := LearnOverlappingModel2DCorpus(LSamples,
    APatternWidth, APatternHeight, ASourceBoundary, ASymmetry);
end;

end.
