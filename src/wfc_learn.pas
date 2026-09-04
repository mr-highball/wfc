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
unit wfc_learn;

{$mode delphi}{$H+}

interface

uses
  wfc_model;

const
  WFC_LEARN_ALGORITHM_VERSION = 1;
  WFC_LEARN_CORPUS_ALGORITHM_VERSION = 1;

type
  TWfcLearnSample = record
    Tokens: TWfcModelTokens;
    Width: Integer;
    Height: Integer;
  end;
  TWfcLearnSamples = array of TWfcLearnSample;

function MakeLearnSample1D(
  const ATokens: TWfcModelTokens): TWfcLearnSample;
function MakeLearnSample2D(const ATokens: TWfcModelTokens;
  const AWidth, AHeight: Integer): TWfcLearnSample;

function LearnModel1DCorpus(const ASamples: TWfcLearnSamples;
  const ABoundary: TWfcModelBoundary): TWfcModel;
function LearnModel2DCorpus(const ASamples: TWfcLearnSamples;
  const ABoundary: TWfcModelBoundary;
  const ASymmetry: TWfcModelSymmetry): TWfcModel;

function LearnModel1D(const ATokens: TWfcModelTokens;
  const ABoundary: TWfcModelBoundary): TWfcModel;
function LearnModel2D(const ATokens: TWfcModelTokens;
  const AWidth, AHeight: Integer; const ABoundary: TWfcModelBoundary;
  const ASymmetry: TWfcModelSymmetry): TWfcModel;

implementation

uses
  Classes,
  SysUtils;

const
  WFC_MODEL_DIRECTION_COUNT = Ord(High(TWfcModelDirection)) + 1;
  D4_TRANSFORM_COUNT = 8;

type
  TWfcLearnValueArray = array of Integer;
  TWfcLearnValueArrays = array of TWfcLearnValueArray;

procedure ValidateBoundary(const ABoundary: TWfcModelBoundary);
begin
  case ABoundary of
    wmbOpen,
    wmbWrap:
      Exit;
  else
    raise ERangeError.Create('unknown WFC model boundary');
  end;
end;

procedure ValidateSymmetry(const ASymmetry: TWfcModelSymmetry);
begin
  case ASymmetry of
    wmsNone,
    wmsD4:
      Exit;
  else
    raise ERangeError.Create('unknown WFC model symmetry');
  end;
end;

function CheckedSampleSize(const AWidth, AHeight: Integer): Integer;
begin
  if (AWidth <= 0) or (AHeight <= 0) then
    raise ERangeError.Create('a WFC learning sample needs positive dimensions');
  if AWidth > High(Integer) div AHeight then
    raise ERangeError.Create('WFC learning sample dimensions are too large');
  Result := AWidth * AHeight;
end;

function CheckedTokenLength(const ATokens: TWfcModelTokens;
  const ALabel: String): Integer;
var
  LLength: SizeInt;
begin
  LLength := Length(ATokens);
  if (LLength < 0) or
    ((LLength and (not SizeInt(High(Integer)))) <> 0) then
    raise ERangeError.Create(ALabel + ' has too many tokens');
  Result := Integer(LLength);
end;

function CheckedCorpusLength(const ASamples: TWfcLearnSamples): Integer;
var
  LLength: SizeInt;
begin
  LLength := Length(ASamples);
  if LLength = 0 then
    raise ERangeError.Create('a WFC learning corpus cannot be empty');
  if (LLength < 0) or
    ((LLength and (not SizeInt(High(Integer)))) <> 0) then
    raise ERangeError.Create('WFC learning corpus has too many samples');
  Result := Integer(LLength);
end;

function CheckedRelationCapacity(const AValueCount: Integer): Integer;
var
  LValuePairs: Integer;
begin
  if AValueCount <= 0 then
    raise ERangeError.Create('a WFC learning sample needs at least one token');
  if AValueCount > High(Integer) div AValueCount then
    raise ERangeError.Create('WFC learned relation table is too large');
  LValuePairs := AValueCount * AValueCount;
  if LValuePairs > High(Integer) div WFC_MODEL_DIRECTION_COUNT then
    raise ERangeError.Create('WFC learned relation table is too large');
  Result := WFC_MODEL_DIRECTION_COUNT * LValuePairs;
end;

procedure CheckedIncrement(var AValue: Integer; const ALabel: String);
begin
  if AValue = High(Integer) then
    raise ERangeError.Create(ALabel + ' exceeds the Integer count range');
  Inc(AValue);
end;

function FindTokenIndex(const ATokens: TWfcModelTokens;
  const AToken: TWfcModelToken): Integer;
var
  I: Integer;
begin
  for I := 0 to Length(ATokens) - 1 do
    if ATokens[I] = AToken then
      Exit(I);
  Result := -1;
end;

procedure MapObservedTokens(const ATokens: TWfcModelTokens;
  const ASampleIndex: Integer; var AUniqueTokens: TWfcModelTokens;
  out AValues: TWfcLearnValueArray);
var
  I: Integer;
  LIndex: Integer;
  LUniqueCount: Integer;
begin
  SetLength(AValues, Length(ATokens));
  for I := 0 to Length(ATokens) - 1 do
  begin
    if ATokens[I] = '' then
      raise EInvalidOperation.CreateFmt(
        'WFC learning sample %d token %d is empty', [ASampleIndex, I]);

    LIndex := FindTokenIndex(AUniqueTokens, ATokens[I]);
    if LIndex < 0 then
    begin
      if Length(AUniqueTokens) >= High(Integer) then
        raise ERangeError.Create('WFC learning corpus has too many values');
      LUniqueCount := Integer(Length(AUniqueTokens));
      SetLength(AUniqueTokens, LUniqueCount + 1);
      AUniqueTokens[LUniqueCount] := ATokens[I];
      LIndex := LUniqueCount;
    end;
    AValues[I] := LIndex;
  end;
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
  Map a row-major coordinate in one explicit D4 observation back into the
  original sample. Transforms 0..3 are identity and clockwise quarter turns.
  Transforms 4..7 first mirror the X coordinate, then apply the same turns.
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
    raise ERangeError.Create('unknown WFC learning transform');
  end;

  if ATransform >= 4 then
    ASourceX := ASourceWidth - 1 - ASourceX;
end;

function TransformedValueAt(const ASourceValues: TWfcLearnValueArray;
  const ASourceWidth, ASourceHeight, ATransform, AX, AY: Integer): Integer;
var
  LSourceX: Integer;
  LSourceY: Integer;
begin
  TransformToSource(AX, AY, ASourceWidth, ASourceHeight, ATransform,
    LSourceX, LSourceY);
  Result := ASourceValues[LSourceY * ASourceWidth + LSourceX];
end;

function FindNeighbor(const AX, AY, AWidth, AHeight: Integer;
  const ADirection: TWfcModelDirection;
  const ABoundary: TWfcModelBoundary; out ANeighborX,
  ANeighborY: Integer): Boolean;
begin
  ANeighborX := AX;
  ANeighborY := AY;
  case ADirection of
    wmdNorth:
      if AY > 0 then
        Dec(ANeighborY)
      else if ABoundary = wmbWrap then
        ANeighborY := AHeight - 1
      else
        Exit(False);
    wmdEast:
      if AX + 1 < AWidth then
        Inc(ANeighborX)
      else if ABoundary = wmbWrap then
        ANeighborX := 0
      else
        Exit(False);
    wmdSouth:
      if AY + 1 < AHeight then
        Inc(ANeighborY)
      else if ABoundary = wmbWrap then
        ANeighborY := 0
      else
        Exit(False);
    wmdWest:
      if AX > 0 then
        Dec(ANeighborX)
      else if ABoundary = wmbWrap then
        ANeighborX := AWidth - 1
      else
        Exit(False);
  else
    raise ERangeError.Create('unknown WFC model direction');
  end;
  Result := True;
end;

function RelationIndex(const ADirection: TWfcModelDirection;
  const ASource, ATarget, AValueCount: Integer): Integer;
begin
  Result := ((Ord(ADirection) * AValueCount + ASource) * AValueCount)
    + ATarget;
end;

procedure ObserveTransform(const ASourceValues: TWfcLearnValueArray;
  const ASourceWidth, ASourceHeight, ATransform, AValueCount: Integer;
  const ABoundary: TWfcModelBoundary;
  const ADirections: TWfcModelDirections;
  var AWeights, ARelations: TWfcModelIntegerArray);
var
  X: Integer;
  Y: Integer;
  LWidth: Integer;
  LHeight: Integer;
  LNeighborX: Integer;
  LNeighborY: Integer;
  LSource: Integer;
  LTarget: Integer;
  LRelationIndex: Integer;
  LDirection: TWfcModelDirection;
begin
  TransformDimensions(ASourceWidth, ASourceHeight, ATransform,
    LWidth, LHeight);
  for Y := 0 to LHeight - 1 do
    for X := 0 to LWidth - 1 do
    begin
      LSource := TransformedValueAt(ASourceValues, ASourceWidth,
        ASourceHeight, ATransform, X, Y);
      CheckedIncrement(AWeights[LSource], 'WFC learned token frequency');

      for LDirection := Low(TWfcModelDirection) to
        High(TWfcModelDirection) do
        if (LDirection in ADirections) and
          FindNeighbor(X, Y, LWidth, LHeight, LDirection, ABoundary,
            LNeighborX, LNeighborY) then
        begin
          LTarget := TransformedValueAt(ASourceValues, ASourceWidth,
            ASourceHeight, ATransform, LNeighborX, LNeighborY);
          LRelationIndex := RelationIndex(LDirection, LSource, LTarget,
            AValueCount);
          CheckedIncrement(ARelations[LRelationIndex],
            'WFC learned relation frequency');
        end;
    end;
end;

function BuildCorpusModel(const ASamples: TWfcLearnSamples;
  const ARank: Integer;
  const ABoundary: TWfcModelBoundary;
  const ASymmetry: TWfcModelSymmetry;
  const ADirections: TWfcModelDirections): TWfcModel;
var
  LExpectedSize: Integer;
  LSampleCount: Integer;
  LSampleIndex: Integer;
  LTokenLength: Integer;
  LValueCount: Integer;
  LTransform: Integer;
  LTransformCount: Integer;
  LSampleShapes: TWfcModelSampleShapes;
  LUniqueTokens: TWfcModelTokens;
  LValues: TWfcLearnValueArrays;
  LWeights: TWfcModelIntegerArray;
  LRelations: TWfcModelIntegerArray;
begin
  ValidateBoundary(ABoundary);
  ValidateSymmetry(ASymmetry);

  LSampleCount := CheckedCorpusLength(ASamples);

  SetLength(LSampleShapes, LSampleCount);
  SetLength(LValues, LSampleCount);
  for LSampleIndex := 0 to LSampleCount - 1 do
  begin
    if (ARank = 1) and (ASamples[LSampleIndex].Height <> 1) then
      raise EInvalidOperation.CreateFmt(
        '1D WFC learning sample %d height must be 1; got %d',
        [LSampleIndex, ASamples[LSampleIndex].Height]);

    LExpectedSize := CheckedSampleSize(ASamples[LSampleIndex].Width,
      ASamples[LSampleIndex].Height);
    LTokenLength := CheckedTokenLength(ASamples[LSampleIndex].Tokens,
      Format('WFC learning sample %d', [LSampleIndex]));
    if LTokenLength <> LExpectedSize then
      raise EInvalidOperation.CreateFmt(
        'WFC learning sample %d has %d tokens; expected %d',
        [LSampleIndex, LTokenLength, LExpectedSize]);

    LSampleShapes[LSampleIndex].Width := ASamples[LSampleIndex].Width;
    LSampleShapes[LSampleIndex].Height := ASamples[LSampleIndex].Height;
  end;

  SetLength(LUniqueTokens, 0);
  for LSampleIndex := 0 to LSampleCount - 1 do
    MapObservedTokens(ASamples[LSampleIndex].Tokens, LSampleIndex,
      LUniqueTokens, LValues[LSampleIndex]);

  LValueCount := Length(LUniqueTokens);
  SetLength(LWeights, LValueCount);
  SetLength(LRelations, CheckedRelationCapacity(LValueCount));

  if ASymmetry = wmsD4 then
    LTransformCount := D4_TRANSFORM_COUNT
  else
    LTransformCount := 1;
  for LSampleIndex := 0 to LSampleCount - 1 do
    for LTransform := 0 to LTransformCount - 1 do
      ObserveTransform(LValues[LSampleIndex],
        LSampleShapes[LSampleIndex].Width,
        LSampleShapes[LSampleIndex].Height, LTransform, LValueCount,
        ABoundary, ADirections, LWeights, LRelations);

  Result := TWfcModel.Create(ARank, LSampleShapes, ABoundary,
    ASymmetry, ADirections, LUniqueTokens, LWeights, LRelations);
end;

procedure CopySampleTokens(const ASource: TWfcModelTokens;
  out ADestination: TWfcModelTokens);
var
  I: Integer;
begin
  SetLength(ADestination, Length(ASource));
  for I := 0 to Length(ASource) - 1 do
    ADestination[I] := ASource[I];
end;

function MakeLearnSample1D(
  const ATokens: TWfcModelTokens): TWfcLearnSample;
var
  LLength: Integer;
begin
  LLength := CheckedTokenLength(ATokens, '1D WFC learning sample');
  if LLength = 0 then
    raise ERangeError.Create('a 1D WFC learning sample cannot be empty');
  CopySampleTokens(ATokens, Result.Tokens);
  Result.Width := LLength;
  Result.Height := 1;
end;

function MakeLearnSample2D(const ATokens: TWfcModelTokens;
  const AWidth, AHeight: Integer): TWfcLearnSample;
var
  LExpectedSize: Integer;
  LTokenLength: Integer;
begin
  LExpectedSize := CheckedSampleSize(AWidth, AHeight);
  LTokenLength := CheckedTokenLength(ATokens, '2D WFC learning sample');
  if LTokenLength <> LExpectedSize then
    raise EInvalidOperation.CreateFmt(
      '2D WFC learning sample has %d tokens; expected %d',
      [LTokenLength, LExpectedSize]);
  CopySampleTokens(ATokens, Result.Tokens);
  Result.Width := AWidth;
  Result.Height := AHeight;
end;

function LearnModel1DCorpus(const ASamples: TWfcLearnSamples;
  const ABoundary: TWfcModelBoundary): TWfcModel;
begin
  Result := BuildCorpusModel(ASamples, 1, ABoundary, wmsNone,
    [wmdEast, wmdWest]);
end;

function LearnModel2DCorpus(const ASamples: TWfcLearnSamples;
  const ABoundary: TWfcModelBoundary;
  const ASymmetry: TWfcModelSymmetry): TWfcModel;
begin
  Result := BuildCorpusModel(ASamples, 2, ABoundary, ASymmetry,
    [wmdNorth, wmdEast, wmdSouth, wmdWest]);
end;

function LearnModel1D(const ATokens: TWfcModelTokens;
  const ABoundary: TWfcModelBoundary): TWfcModel;
var
  LSamples: TWfcLearnSamples;
begin
  SetLength(LSamples, 1);
  LSamples[0] := MakeLearnSample1D(ATokens);
  Result := LearnModel1DCorpus(LSamples, ABoundary);
end;

function LearnModel2D(const ATokens: TWfcModelTokens;
  const AWidth, AHeight: Integer; const ABoundary: TWfcModelBoundary;
  const ASymmetry: TWfcModelSymmetry): TWfcModel;
var
  LSamples: TWfcLearnSamples;
begin
  SetLength(LSamples, 1);
  LSamples[0] := MakeLearnSample2D(ATokens, AWidth, AHeight);
  Result := LearnModel2DCorpus(LSamples, ABoundary, ASymmetry);
end;

end.
