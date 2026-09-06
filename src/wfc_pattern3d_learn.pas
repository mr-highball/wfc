{ SPDX-License-Identifier: MIT
  Copyright (c) 2021 mr-highball }
unit wfc_pattern3d_learn;

{$mode delphi}{$H+}

interface

uses wfc_model, wfc_learn3d, wfc_pattern3d;

{ Each sample is an independent X-fast, then Y, then Z volume. Palette order
  comes from the original samples, before augmentation. Pattern order follows
  sample, literal transform, then transformed origin in the same XYZ order.
  Counts are raw observations, including repeated symmetric observations. }
function LearnOverlappingModel3DCorpus(const ASamples: TWfcLearnVolumeSamples;
  const APatternWidth, APatternHeight, APatternDepth: Integer;
  const ASourceBoundary: TWfcModelBoundary;
  const ASymmetry: TWfcModelSymmetry): TWfcOverlappingModel3D;

function LearnOverlappingModel3D(const ATokens: TWfcModelTokens;
  const ASourceWidth, ASourceHeight, ASourceDepth: Integer;
  const APatternWidth, APatternHeight, APatternDepth: Integer;
  const ASourceBoundary: TWfcModelBoundary;
  const ASymmetry: TWfcModelSymmetry): TWfcOverlappingModel3D;

implementation

uses SysUtils, wfc_volume_symmetry;

type
  TTokenTable = record
    Tokens: TWfcModelTokens;
    Slots: TWfcModelIntegerArray;
  end;
  TPatternTable = record
    Patterns: TWfcPattern3DPayloads;
    Weights, Slots: TWfcModelIntegerArray;
  end;

procedure RequireInteger(const AValue, AMinimum, AMaximum: Integer;
  const AName: String);
{$IFDEF PAS2JS}var LValid: Boolean;{$ENDIF}
begin
  {$IFDEF PAS2JS}
  asm LValid = typeof AValue === 'number' && isFinite(AValue) && Math.floor(AValue) === AValue; end;
  if not LValid then
    raise EWfcOverlapping3D.Create(AName + ' must be an exact finite integer');
  {$ENDIF}
  if (AValue < AMinimum) or (AValue > AMaximum) then
    raise EWfcOverlapping3D.Create(AName + ' exceeds its supported range');
end;

function CheckedVolume(const W, H, D, AMaxDimension, AMaxCells: Integer;
  const AName: String): Integer;
begin
  RequireInteger(W, 1, AMaxDimension, AName + ' width');
  RequireInteger(H, 1, AMaxDimension, AName + ' height');
  RequireInteger(D, 1, AMaxDimension, AName + ' depth');
  if W > AMaxCells div H then
    raise EWfcOverlapping3D.Create(AName + ' cells exceed the supported limit');
  Result := W * H;
  if Result > AMaxCells div D then
    raise EWfcOverlapping3D.Create(AName + ' cells exceed the supported limit');
  Result := Result * D;
end;

procedure RequireToken(const AToken: TWfcModelToken);
{$IFDEF PAS2JS}var LString: Boolean;{$ENDIF}
begin
  {$IFDEF PAS2JS}
  asm LString = typeof AToken === 'string'; end;
  if not LString then
    raise EWfcOverlapping3D.Create('overlapping volume tokens must be strings');
  {$ENDIF}
  if not WfcModelTokenIsValid(AToken) then
    raise EWfcOverlapping3D.Create(
      'overlapping volume tokens must be nonempty Unicode-scalar text');
end;

procedure HashInteger(var AHash: Cardinal; const AValue: Integer);
{$PUSH}{$Q-}
var LPrevious: Cardinal;
begin
  AHash := AHash xor Cardinal(AValue);
  LPrevious := AHash;
  AHash := (LPrevious + (LPrevious shl 1) + (LPrevious shl 4) +
    (LPrevious shl 7) + (LPrevious shl 8) + (LPrevious shl 24)) and
    Cardinal($FFFFFFFF);
end;
{$POP}

function TokenHash(const AToken: TWfcModelToken): Cardinal;
var I: Integer;
begin
  Result := Cardinal(2166136261);
  for I := 1 to Length(AToken) do HashInteger(Result, Ord(AToken[I]));
end;

function InternToken(var ATable: TTokenTable;
  const AToken: TWfcModelToken): Integer;
var LSlot, LProbe: Integer;
begin
  LSlot := Integer(TokenHash(AToken) mod Cardinal(Length(ATable.Slots)));
  for LProbe := 0 to High(ATable.Slots) do
  begin
    if ATable.Slots[LSlot] = 0 then
    begin
      Result := Length(ATable.Tokens);
      if Result >= WFC_PATTERN_3D_MAX_PALETTE_COUNT then
        raise EWfcOverlapping3D.Create('overlapping volume palette exceeds its limit');
      SetLength(ATable.Tokens, Result + 1);
      ATable.Tokens[Result] := AToken;
      ATable.Slots[LSlot] := Result + 1;
      Exit;
    end;
    Result := ATable.Slots[LSlot] - 1;
    if ATable.Tokens[Result] = AToken then Exit;
    Inc(LSlot);
    if LSlot = Length(ATable.Slots) then LSlot := 0;
  end;
  raise EWfcOverlapping3D.Create('internal overlapping volume token table is full');
end;

function PayloadsEqual(const A, B: TWfcPattern3DPayload): Boolean;
var I: Integer;
begin
  if Length(A) <> Length(B) then Exit(False);
  for I := 0 to High(A) do if A[I] <> B[I] then Exit(False);
  Result := True;
end;

function InternPattern(var ATable: TPatternTable;
  const APayload: TWfcPattern3DPayload): Integer;
var I, LSlot, LProbe, LSquare: Integer; LHash: Cardinal;
begin
  LHash := Cardinal(2166136261);
  for I := 0 to High(APayload) do HashInteger(LHash, APayload[I]);
  LSlot := Integer(LHash mod Cardinal(Length(ATable.Slots)));
  for LProbe := 0 to High(ATable.Slots) do
  begin
    if ATable.Slots[LSlot] = 0 then
    begin
      Result := Length(ATable.Patterns);
      if Result >= WFC_PATTERN_3D_MAX_PATTERN_COUNT then
        raise EWfcOverlapping3D.Create('overlapping volume pattern count exceeds its limit');
      LSquare := (Result + 1) * (Result + 1);
      if LSquare > WFC_PATTERN_3D_MAX_RELATION_SLOT_COUNT div 6 then
        raise EWfcOverlapping3D.Create('overlapping volume relations exceed their limit');
      if Result + 1 > WFC_PATTERN_3D_MAX_TOTAL_PATTERN_CELL_COUNT div Length(APayload) then
        raise EWfcOverlapping3D.Create('aggregate overlapping volume patterns exceed their cell limit');
      SetLength(ATable.Patterns, Result + 1);
      SetLength(ATable.Patterns[Result], Length(APayload));
      for I := 0 to High(APayload) do ATable.Patterns[Result][I] := APayload[I];
      SetLength(ATable.Weights, Result + 1);
      ATable.Weights[Result] := 0;
      ATable.Slots[LSlot] := Result + 1;
      Exit;
    end;
    Result := ATable.Slots[LSlot] - 1;
    { Hashes only choose a bucket. Complete equality is the identity test. }
    if PayloadsEqual(ATable.Patterns[Result], APayload) then Exit;
    Inc(LSlot);
    if LSlot = Length(ATable.Slots) then LSlot := 0;
  end;
  raise EWfcOverlapping3D.Create('internal overlapping volume pattern table is full');
end;

function ModuloAdd(const ABase, AOffset, AModulus: Integer): Integer;
var LOffset: Integer;
begin
  LOffset := AOffset mod AModulus;
  if ABase >= AModulus - LOffset then Result := ABase - (AModulus - LOffset)
  else Result := ABase + LOffset;
end;

function LearnOverlappingModel3DCorpus(const ASamples: TWfcLearnVolumeSamples;
  const APatternWidth, APatternHeight, APatternDepth: Integer;
  const ASourceBoundary: TWfcModelBoundary;
  const ASymmetry: TWfcModelSymmetry): TWfcOverlappingModel3D;
var
  LTokens: TTokenTable;
  LPatterns: TPatternTable;
  LShapes: TWfcModelSampleShapes;
  LSource, LTransformed: TWfcModelIntegerArray;
  LPayload: TWfcPattern3DPayload;
  LTransform: TWfcVolumeTransform;
  LShape: TWfcModelSampleShape;
  S, T, I, LCells, LTotalCells, LFootprint, LTransforms: Integer;
  LOrigins, LExpected, LObserved, LPattern, LX, LY, LZ: Integer;
  X, Y, Z, PX, PY, PZ, SX, SY, SZ: Integer;
begin
  RequireInteger(Ord(ASourceBoundary), Ord(wmbOpen), Ord(wmbWrap), 'source boundary');
  RequireInteger(Ord(ASymmetry), Ord(wmsNone), Ord(wmsCubeFull), 'source symmetry');
  LFootprint := CheckedVolume(APatternWidth, APatternHeight, APatternDepth,
    WFC_PATTERN_3D_MAX_FOOTPRINT_DIMENSION, WFC_PATTERN_3D_MAX_FOOTPRINT_CELL_COUNT,
    'overlapping volume footprint');
  if (ASymmetry = wmsD4) and (APatternWidth <> APatternHeight) then
    raise EWfcOverlapping3D.Create('D4 overlapping volumes require a square XY footprint');
  if (ASymmetry in [wmsCubeRotations, wmsCubeFull]) and
    ((APatternWidth <> APatternHeight) or (APatternWidth <> APatternDepth)) then
    raise EWfcOverlapping3D.Create('cube symmetry requires a cubic overlapping footprint');
  if (Length(ASamples) < 1) or (Length(ASamples) > WFC_PATTERN_3D_MAX_SOURCE_COUNT) then
    raise EWfcOverlapping3D.Create('overlapping volume corpus exceeds its nonempty sample limit');
  LTransforms := WfcVolumeTransformCount(ASymmetry);

  { Preflight all source extents and tokens before owned indexed volumes,
    transformed arrays, payloads, or relation storage are allocated. The
    permitted symmetry keeps the footprint invariant under each transform. }
  LTotalCells := 0; LExpected := 0;
  for S := 0 to High(ASamples) do
  begin
    LCells := CheckedVolume(ASamples[S].Width, ASamples[S].Height, ASamples[S].Depth,
      WFC_PATTERN_3D_MAX_SOURCE_DIMENSION, WFC_PATTERN_3D_MAX_SOURCE_CELL_COUNT,
      'overlapping volume source');
    if Length(ASamples[S].Tokens) <> LCells then
      raise EWfcOverlapping3D.Create('overlapping volume source token count differs from its shape');
    if LCells > WFC_PATTERN_3D_MAX_TOTAL_SOURCE_CELL_COUNT - LTotalCells then
      raise EWfcOverlapping3D.Create('overlapping volume corpus cells exceed their aggregate limit');
    Inc(LTotalCells, LCells);
    if ASourceBoundary = wmbOpen then
    begin
      if (ASamples[S].Width < APatternWidth) or (ASamples[S].Height < APatternHeight) or
        (ASamples[S].Depth < APatternDepth) then
        raise EWfcOverlapping3D.Create('open overlapping source is smaller than the footprint');
      LOrigins := (ASamples[S].Width - APatternWidth + 1) *
        (ASamples[S].Height - APatternHeight + 1) *
        (ASamples[S].Depth - APatternDepth + 1);
    end
    else LOrigins := LCells;
    if LOrigins > (High(Integer) - LExpected) div LTransforms then
      raise EWfcOverlapping3D.Create('overlapping volume observations exceed the Integer range');
    Inc(LExpected, LOrigins * LTransforms);
    for I := 0 to High(ASamples[S].Tokens) do RequireToken(ASamples[S].Tokens[I]);
  end;

  LTokens := Default(TTokenTable); LPatterns := Default(TPatternTable);
  SetLength(LTokens.Slots, 2 * WFC_PATTERN_3D_MAX_PALETTE_COUNT);
  SetLength(LPatterns.Slots, 2 * WFC_PATTERN_3D_MAX_PATTERN_COUNT);
  SetLength(LShapes, Length(ASamples));
  for S := 0 to High(ASamples) do
  begin
    LShapes[S] := MakeWfcModelSampleShape(ASamples[S].Width,
      ASamples[S].Height, ASamples[S].Depth);
    for I := 0 to High(ASamples[S].Tokens) do InternToken(LTokens, ASamples[S].Tokens[I]);
  end;
  SetLength(LPayload, LFootprint); LObserved := 0;
  for S := 0 to High(ASamples) do
  begin
    SetLength(LSource, Length(ASamples[S].Tokens));
    for I := 0 to High(LSource) do LSource[I] := InternToken(LTokens, ASamples[S].Tokens[I]);
    for T := 0 to LTransforms - 1 do
    begin
      LTransform := WfcVolumeTransformAt(ASymmetry, T);
      LShape := WfcTransformVolumeShape(LShapes[S], LTransform);
      LTransformed := WfcTransformVolumeIntegers(LSource, LShapes[S], LTransform);
      LX := LShape.Width; LY := LShape.Height; LZ := LShape.Depth;
      if ASourceBoundary = wmbOpen then
      begin Dec(LX, APatternWidth - 1); Dec(LY, APatternHeight - 1); Dec(LZ, APatternDepth - 1); end;
      for Z := 0 to LZ - 1 do for Y := 0 to LY - 1 do for X := 0 to LX - 1 do
      begin
        for PZ := 0 to APatternDepth - 1 do for PY := 0 to APatternHeight - 1 do
          for PX := 0 to APatternWidth - 1 do
          begin
            if ASourceBoundary = wmbWrap then
            begin
              SX := ModuloAdd(X, PX, LShape.Width);
              SY := ModuloAdd(Y, PY, LShape.Height);
              SZ := ModuloAdd(Z, PZ, LShape.Depth);
            end
            else begin SX := X + PX; SY := Y + PY; SZ := Z + PZ; end;
            LPayload[(PZ * APatternHeight + PY) * APatternWidth + PX] :=
              LTransformed[(SZ * LShape.Height + SY) * LShape.Width + SX];
          end;
        LPattern := InternPattern(LPatterns, LPayload);
        { The complete observation preflight bounds every individual weight. }
        Inc(LPatterns.Weights[LPattern]); Inc(LObserved);
      end;
    end;
  end;
  if LObserved <> LExpected then
    raise EWfcOverlapping3D.Create('internal overlapping volume observation count mismatch');
  Result := TWfcOverlappingModel3D.Create(APatternWidth, APatternHeight, APatternDepth,
    ASourceBoundary, ASymmetry, LShapes, LTokens.Tokens, LPatterns.Patterns, LPatterns.Weights);
end;

function LearnOverlappingModel3D(const ATokens: TWfcModelTokens;
  const ASourceWidth, ASourceHeight, ASourceDepth: Integer;
  const APatternWidth, APatternHeight, APatternDepth: Integer;
  const ASourceBoundary: TWfcModelBoundary;
  const ASymmetry: TWfcModelSymmetry): TWfcOverlappingModel3D;
var LSamples: TWfcLearnVolumeSamples;
begin
  { Do not clone the input before the complete corpus preflight. The learner
    never mutates this temporary borrowed token array. }
  SetLength(LSamples, 1);
  LSamples[0].Width := ASourceWidth; LSamples[0].Height := ASourceHeight;
  LSamples[0].Depth := ASourceDepth; LSamples[0].Tokens := ATokens;
  Result := LearnOverlappingModel3DCorpus(LSamples, APatternWidth, APatternHeight,
    APatternDepth, ASourceBoundary, ASymmetry);
end;

end.
