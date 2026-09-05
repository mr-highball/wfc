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

unit wfc_learn3d;

{$mode delphi}{$H+}

interface

uses
  wfc_model;

const
  WFC_LEARN_3D_ALGORITHM_VERSION = 1;

type
  { X-fast rows, then Y, then Z. Each sample is a separate volume; neither
    open nor wrapped learning creates a seam between different samples. }
  TWfcLearnVolumeSample = record
    Tokens: TWfcModelTokens;
    Width: Integer;
    Height: Integer;
    Depth: Integer;
  end;
  TWfcLearnVolumeSamples = array of TWfcLearnVolumeSample;

function MakeLearnSample3D(const ATokens: TWfcModelTokens;
  const AWidth, AHeight, ADepth: Integer): TWfcLearnVolumeSample;

function LearnModel3DCorpus(const ASamples: TWfcLearnVolumeSamples;
  const ABoundary: TWfcModelBoundary;
  const ASymmetry: TWfcModelSymmetry): TWfcModel;

function LearnModel3D(const ATokens: TWfcModelTokens;
  const AWidth, AHeight, ADepth: Integer; const ABoundary: TWfcModelBoundary;
  const ASymmetry: TWfcModelSymmetry): TWfcModel;

implementation

uses
  Classes, SysUtils;

type
  TVolumeValues = array of TWfcModelIntegerArray;
  TVolumeTokenTable = record
    Tokens: TWfcModelTokens;
    Slots: array of Integer;
  end;

function CheckedVolumeSize(const AWidth, AHeight, ADepth: Integer): Integer;
begin
  {$IFDEF PAS2JS}
  if (AWidth <> Trunc(AWidth)) or (AHeight <> Trunc(AHeight)) or
    (ADepth <> Trunc(ADepth)) then
    raise ERangeError.Create('volume dimensions must be exact integers');
  {$ENDIF}
  if (AWidth < 1) or (AHeight < 1) or (ADepth < 1) then
    raise ERangeError.Create('volume dimensions must be positive');
  if (AWidth > WFC_MODEL_MAX_SAMPLE_DIMENSION) or
    (AHeight > WFC_MODEL_MAX_SAMPLE_DIMENSION) or
    (ADepth > WFC_MODEL_MAX_SAMPLE_DIMENSION) then
    raise ERangeError.Create('volume dimension exceeds the sample limit');
  if AWidth > WFC_MODEL_MAX_SAMPLE_CELL_COUNT div AHeight then
    raise ERangeError.Create('volume cells exceed the sample limit');
  Result := AWidth * AHeight;
  if Result > WFC_MODEL_MAX_SAMPLE_CELL_COUNT div ADepth then
    raise ERangeError.Create('volume cells exceed the sample limit');
  Result := Result * ADepth;
end;

function MakeLearnSample3D(const ATokens: TWfcModelTokens;
  const AWidth, AHeight, ADepth: Integer): TWfcLearnVolumeSample;
var I, LCells: Integer;
begin
  LCells := CheckedVolumeSize(AWidth, AHeight, ADepth);
  if Length(ATokens) <> LCells then
    raise EArgumentException.CreateFmt('volume has %d tokens; expected %d',
      [Length(ATokens), LCells]);
  Result.Width := AWidth;
  Result.Height := AHeight;
  Result.Depth := ADepth;
  SetLength(Result.Tokens, LCells);
  for I := 0 to LCells - 1 do Result.Tokens[I] := ATokens[I];
end;

function TokenHash(const AToken: TWfcModelToken): Cardinal;
{$PUSH}{$Q-}
var I: Integer; LPrevious: Cardinal;
begin
  Result := Cardinal(2166136261);
  for I := 1 to Length(AToken) do
  begin
    Result := Result xor Cardinal(Ord(AToken[I]));
    LPrevious := Result;
    Result := (LPrevious + (LPrevious shl 1) + (LPrevious shl 4) +
      (LPrevious shl 7) + (LPrevious shl 8) + (LPrevious shl 24)) and
      Cardinal($FFFFFFFF);
  end;
end;
{$POP}

function InternToken(var ATable: TVolumeTokenTable;
  const AToken: TWfcModelToken; const ASample, APosition: Integer): Integer;
var LSlot, LProbe: Integer;
begin
  if not WfcModelTokenIsValid(AToken) then
    raise EArgumentException.CreateFmt('volume sample %d token %d is not a nonempty Unicode-scalar token',
      [ASample, APosition]);
  LSlot := Integer(TokenHash(AToken) mod Cardinal(Length(ATable.Slots)));
  for LProbe := 0 to Length(ATable.Slots) - 1 do
  begin
    if ATable.Slots[LSlot] = 0 then
    begin
      Result := Length(ATable.Tokens);
      if Result >= WFC_MODEL_MAX_VALUE_COUNT then
        raise ERangeError.Create('volume vocabulary exceeds the model value limit');
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
  raise EInvalidOperation.Create('volume token table has no free slot');
end;

function ObservationCount(const ASymmetry: TWfcModelSymmetry): Integer;
begin
  case ASymmetry of
    wmsNone: Result := 1;
    wmsD4: Result := 8;
    wmsCubeRotations: Result := 24;
    wmsCubeFull: Result := 48;
  else
    raise ERangeError.Create('unknown volume symmetry');
  end;
end;

function CheckedMultiply(const AValue, AFactor: Integer): Integer;
begin
  if (AValue < 0) or (AFactor < 1) or (AValue > High(Integer) div AFactor) then
    raise ERangeError.Create('volume observation count exceeds Integer');
  Result := AValue * AFactor;
end;

procedure CheckedAdd(var AValue: Integer; const ADelta: Integer);
begin
  if (ADelta < 0) or (AValue > High(Integer) - ADelta) then
    raise ERangeError.Create('volume observation count exceeds Integer');
  Inc(AValue, ADelta);
end;

procedure ApplyObservationOrbits(const ASymmetry: TWfcModelSymmetry;
  const AValueCount: Integer; var AWeights, ARelations: TWfcModelIntegerArray);
var I, D, LSquare, LTotal, LCount: Integer;
begin
  LCount := ObservationCount(ASymmetry);
  for I := 0 to Length(AWeights) - 1 do
    AWeights[I] := CheckedMultiply(AWeights[I], LCount);
  if ASymmetry = wmsNone then Exit;

  { Cardinal learning only observes a source token, one direction, and a
    neighboring token. Coordinate transforms therefore act on six direction
    planes, not on token payloads. Summing their orbits is exactly equivalent
    to materializing every rotated/reflected volume, even for rectangles,
    open edges, and wrapped singleton axes. No augmentation is deduplicated. }
  LSquare := AValueCount * AValueCount;
  for I := 0 to LSquare - 1 do
  begin
    LTotal := 0;
    if ASymmetry = wmsD4 then
    begin
      //The eight gravity-preserving transforms fix both vertical directions.
      for D := 0 to 3 do CheckedAdd(LTotal, ARelations[D * LSquare + I]);
      LTotal := CheckedMultiply(LTotal, 2);
      for D := 0 to 3 do ARelations[D * LSquare + I] := LTotal;
      for D := 4 to 5 do
        ARelations[D * LSquare + I] :=
          CheckedMultiply(ARelations[D * LSquare + I], 8);
    end
    else
    begin
      //Each direction has four images in each destination under the 24
      //proper rotations, and eight under the full 48 signed permutations.
      for D := 0 to 5 do CheckedAdd(LTotal, ARelations[D * LSquare + I]);
      LTotal := CheckedMultiply(LTotal, LCount div 6);
      for D := 0 to 5 do ARelations[D * LSquare + I] := LTotal;
    end;
  end;
end;

function LearnModel3DCorpus(const ASamples: TWfcLearnVolumeSamples;
  const ABoundary: TWfcModelBoundary;
  const ASymmetry: TWfcModelSymmetry): TWfcModel;
const
  DX: array[0..5] of Integer = (0, 1, 0, -1, 0, 0);
  DY: array[0..5] of Integer = (-1, 0, 1, 0, 0, 0);
  DZ: array[0..5] of Integer = (0, 0, 0, 0, 1, -1);
var
  I, S, X, Y, Z, NX, NY, NZ, D, W, H, LDepth: Integer;
  LCells, LTotalCells, LSource, LTarget, LIndex, LSquare: Integer;
  LValues: TVolumeValues;
  LTable: TVolumeTokenTable;
  LShapes: TWfcModelSampleShapes;
  LWeights, LRelations: TWfcModelIntegerArray;

  function ResolveAxis(var APosition: Integer; const ASize: Integer): Boolean;
  begin
    Result := True;
    if (APosition >= 0) and (APosition < ASize) then Exit;
    if ABoundary = wmbOpen then Exit(False);
    if APosition < 0 then APosition := ASize - 1 else APosition := 0;
  end;

begin
  Result := nil;
  if not (ABoundary in [wmbOpen, wmbWrap]) then
    raise ERangeError.Create('unknown volume boundary');
  ObservationCount(ASymmetry);
  if (Length(ASamples) < 1) or
    (Length(ASamples) > WFC_MODEL_MAX_SAMPLE_COUNT) then
    raise ERangeError.Create('volume corpus sample count exceeds its nonempty limit');

  //Validate every shape and corpus extent before allocating indexed cells.
  LTotalCells := 0;
  for S := 0 to Length(ASamples) - 1 do
  begin
    LCells := CheckedVolumeSize(ASamples[S].Width, ASamples[S].Height,
      ASamples[S].Depth);
    if Length(ASamples[S].Tokens) <> LCells then
      raise EArgumentException.CreateFmt('volume sample %d has %d tokens; expected %d',
        [S, Length(ASamples[S].Tokens), LCells]);
    if LTotalCells > WFC_MODEL_MAX_TOTAL_SAMPLE_CELL_COUNT - LCells then
      raise ERangeError.Create('volume corpus cells exceed the aggregate sample limit');
    Inc(LTotalCells, LCells);
  end;

  SetLength(LTable.Slots, 2 * WFC_MODEL_MAX_VALUE_COUNT);
  SetLength(LTable.Tokens, 0);
  SetLength(LValues, Length(ASamples));
  SetLength(LShapes, Length(ASamples));
  for S := 0 to Length(ASamples) - 1 do
  begin
    LShapes[S] := MakeWfcModelSampleShape(ASamples[S].Width,
      ASamples[S].Height, ASamples[S].Depth);
    SetLength(LValues[S], Length(ASamples[S].Tokens));
    for I := 0 to Length(ASamples[S].Tokens) - 1 do
      LValues[S][I] := InternToken(LTable, ASamples[S].Tokens[I], S, I);
  end;

  SetLength(LWeights, Length(LTable.Tokens));
  //Vocabulary validation bounds this multiplication to the six-plane limit.
  LSquare := Length(LTable.Tokens) * Length(LTable.Tokens);
  if LSquare > WFC_MODEL_3D_MAX_RELATION_SLOT_COUNT div 6 then
    raise ERangeError.Create('volume relations exceed the model slot limit');
  SetLength(LRelations, LSquare * 6);
  for S := 0 to Length(ASamples) - 1 do
  begin
    W := ASamples[S].Width;
    H := ASamples[S].Height;
    LDepth := ASamples[S].Depth;
    for Z := 0 to LDepth - 1 do
      for Y := 0 to H - 1 do
        for X := 0 to W - 1 do
        begin
          LSource := LValues[S][(Z * H + Y) * W + X];
          CheckedAdd(LWeights[LSource], 1);
          for D := 0 to 5 do
          begin
            NX := X + DX[D]; NY := Y + DY[D]; NZ := Z + DZ[D];
            if not ResolveAxis(NX, W) or not ResolveAxis(NY, H) or
              not ResolveAxis(NZ, LDepth) then Continue;
            LTarget := LValues[S][(NZ * H + NY) * W + NX];
            LIndex := (D * Length(LWeights) + LSource) * Length(LWeights) + LTarget;
            CheckedAdd(LRelations[LIndex], 1);
          end;
        end;
  end;
  ApplyObservationOrbits(ASymmetry, Length(LWeights), LWeights, LRelations);
  Result := TWfcModel.Create(3, LShapes, ABoundary, ASymmetry,
    [wmdNorth, wmdEast, wmdSouth, wmdWest, wmdUp, wmdDown],
    LTable.Tokens, LWeights, LRelations);
end;

function LearnModel3D(const ATokens: TWfcModelTokens;
  const AWidth, AHeight, ADepth: Integer; const ABoundary: TWfcModelBoundary;
  const ASymmetry: TWfcModelSymmetry): TWfcModel;
var LSamples: TWfcLearnVolumeSamples;
begin
  SetLength(LSamples, 1);
  LSamples[0] := MakeLearnSample3D(ATokens, AWidth, AHeight, ADepth);
  Result := LearnModel3DCorpus(LSamples, ABoundary, ASymmetry);
end;

end.
