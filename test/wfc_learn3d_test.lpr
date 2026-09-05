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
program wfc_learn3d_test;

{$mode delphi}{$H+}

uses
  {$IFDEF PAS2JS}wfc_browser_test_host,{$ENDIF}
  SysUtils,
  wfc,
  wfc_model,
  wfc_learn3d;

type
  TTestProcedure = procedure;
  TOracleIntegers = array of Integer;
  TOracleTriple = array[0..2] of Integer;

  TOracleAxisMap = record
    Destination: Integer;
    Sign: Integer;
  end;

  TOracleTransform = record
    Axis: array[0..2] of TOracleAxisMap;
  end;
  TOracleTransforms = array of TOracleTransform;

  TOracleModel = record
    Tokens: TWfcModelTokens;
    Weights: TOracleIntegers;
    Relations: TOracleIntegers;
    Shapes: TWfcModelSampleShapes;
    Boundary: TWfcModelBoundary;
    Symmetry: TWfcModelSymmetry;
  end;

  TDirectionTotals = array[TWfcModelDirection] of Integer;

const
  MODEL_DIRECTIONS: TWfcModelDirections =
    [wmdNorth, wmdEast, wmdSouth, wmdWest, wmdUp, wmdDown];

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

function CellIndex(const AX, AY, AZ, AWidth, AHeight: Integer): Integer;
begin
  Result := AX + AWidth * (AY + AHeight * AZ);
end;

function RelationIndex(const ADirection: TWfcModelDirection;
  const ASource, ATarget, AValueCount: Integer): Integer;
begin
  Result := ((Ord(ADirection) * AValueCount) + ASource) *
    AValueCount + ATarget;
end;

function TransformCount(const ASymmetry: TWfcModelSymmetry): Integer;
begin
  case ASymmetry of
    wmsNone:
      Result := 1;
    wmsD4:
      Result := 8;
    wmsCubeRotations:
      Result := 24;
    wmsCubeFull:
      Result := 48;
  else
    Result := -1;
  end;
end;

procedure AppendTransform(var ATransforms: TOracleTransforms;
  const ADestination0, ASign0, ADestination1, ASign1,
  ADestination2, ASign2: Integer);
var
  LIndex: Integer;
begin
  LIndex := Length(ATransforms);
  SetLength(ATransforms, LIndex + 1);
  ATransforms[LIndex].Axis[0].Destination := ADestination0;
  ATransforms[LIndex].Axis[0].Sign := ASign0;
  ATransforms[LIndex].Axis[1].Destination := ADestination1;
  ATransforms[LIndex].Axis[1].Sign := ASign1;
  ATransforms[LIndex].Axis[2].Destination := ADestination2;
  ATransforms[LIndex].Axis[2].Sign := ASign2;
end;

function PermutationSign(const APermutation: TOracleTriple): Integer;
var
  I: Integer;
  J: Integer;
  LInversions: Integer;
begin
  LInversions := 0;
  for I := 0 to 1 do
    for J := I + 1 to 2 do
      if APermutation[I] > APermutation[J] then
        Inc(LInversions);
  if Odd(LInversions) then
    Result := -1
  else
    Result := 1;
end;

procedure AppendSignedPermutation(var ATransforms: TOracleTransforms;
  const APermutation: TOracleTriple; const AMask: Integer);
var
  LSign0: Integer;
  LSign1: Integer;
  LSign2: Integer;
begin
  if (AMask and 1) <> 0 then LSign0 := -1 else LSign0 := 1;
  if (AMask and 2) <> 0 then LSign1 := -1 else LSign1 := 1;
  if (AMask and 4) <> 0 then LSign2 := -1 else LSign2 := 1;
  AppendTransform(ATransforms,
    APermutation[0], LSign0,
    APermutation[1], LSign1,
    APermutation[2], LSign2);
end;

procedure AppendPermutationTransforms(var ATransforms: TOracleTransforms;
  const APermutation: TOracleTriple; const AProperOnly: Boolean);
var
  LMask: Integer;
  LProduct: Integer;
  LSign0: Integer;
  LSign1: Integer;
  LSign2: Integer;
begin
  for LMask := 0 to 7 do
  begin
    if (LMask and 1) <> 0 then LSign0 := -1 else LSign0 := 1;
    if (LMask and 2) <> 0 then LSign1 := -1 else LSign1 := 1;
    if (LMask and 4) <> 0 then LSign2 := -1 else LSign2 := 1;
    LProduct := PermutationSign(APermutation) *
      LSign0 * LSign1 * LSign2;
    if (not AProperOnly) or (LProduct = 1) then
      AppendSignedPermutation(ATransforms, APermutation, LMask);
  end;
end;

procedure BuildTransforms(const ASymmetry: TWfcModelSymmetry;
  out ATransforms: TOracleTransforms);
const
  PERMUTATIONS: array[0..5] of TOracleTriple = (
    (0, 1, 2),
    (0, 2, 1),
    (1, 0, 2),
    (1, 2, 0),
    (2, 0, 1),
    (2, 1, 0)
  );
var
  I: Integer;
begin
  ATransforms := nil;
  case ASymmetry of
    wmsNone:
      AppendTransform(ATransforms, 0, 1, 1, 1, 2, 1);
    wmsD4:
      begin
        { The first two logical axes are East and North. Raw sample Y grows
          South, so keeping the construction in this logical frame prevents
          an accidental North/South reversal. Every member fixes Up. }
        AppendTransform(ATransforms, 0,  1, 1,  1, 2, 1);
        AppendTransform(ATransforms, 1, -1, 0,  1, 2, 1);
        AppendTransform(ATransforms, 0, -1, 1, -1, 2, 1);
        AppendTransform(ATransforms, 1,  1, 0, -1, 2, 1);
        AppendTransform(ATransforms, 0, -1, 1,  1, 2, 1);
        AppendTransform(ATransforms, 1,  1, 0,  1, 2, 1);
        AppendTransform(ATransforms, 0,  1, 1, -1, 2, 1);
        AppendTransform(ATransforms, 1, -1, 0, -1, 2, 1);
      end;
    wmsCubeRotations,
    wmsCubeFull:
      for I := 0 to High(PERMUTATIONS) do
        AppendPermutationTransforms(ATransforms, PERMUTATIONS[I],
          ASymmetry = wmsCubeRotations);
  else
    raise ERangeError.Create('unknown oracle symmetry');
  end;
end;

function SameTransform(const ALeft,
  ARight: TOracleTransform): Boolean;
var
  I: Integer;
begin
  for I := 0 to 2 do
    if (ALeft.Axis[I].Destination <> ARight.Axis[I].Destination) or
      (ALeft.Axis[I].Sign <> ARight.Axis[I].Sign) then
      Exit(False);
  Result := True;
end;

function TransformDeterminant(const ATransform: TOracleTransform): Integer;
var
  LPermutation: TOracleTriple;
begin
  LPermutation[0] := ATransform.Axis[0].Destination;
  LPermutation[1] := ATransform.Axis[1].Destination;
  LPermutation[2] := ATransform.Axis[2].Destination;
  Result := PermutationSign(LPermutation) *
    ATransform.Axis[0].Sign * ATransform.Axis[1].Sign *
    ATransform.Axis[2].Sign;
end;

function ComposeTransforms(const AFirst,
  ASecond: TOracleTransform): TOracleTransform;
var
  I: Integer;
  LMiddle: Integer;
begin
  for I := 0 to 2 do
  begin
    LMiddle := AFirst.Axis[I].Destination;
    Result.Axis[I].Destination := ASecond.Axis[LMiddle].Destination;
    Result.Axis[I].Sign := AFirst.Axis[I].Sign *
      ASecond.Axis[LMiddle].Sign;
  end;
end;

function ContainsTransform(const ATransforms: TOracleTransforms;
  const ATransform: TOracleTransform): Boolean;
var
  I: Integer;
begin
  for I := 0 to High(ATransforms) do
    if SameTransform(ATransforms[I], ATransform) then
      Exit(True);
  Result := False;
end;

function TransformSetIsUnique(const ATransforms: TOracleTransforms): Boolean;
var
  I: Integer;
  J: Integer;
begin
  for I := 0 to High(ATransforms) do
    for J := I + 1 to High(ATransforms) do
      if SameTransform(ATransforms[I], ATransforms[J]) then
        Exit(False);
  Result := True;
end;

function TransformSetIsClosed(const ATransforms: TOracleTransforms): Boolean;
var
  I: Integer;
  J: Integer;
begin
  for I := 0 to High(ATransforms) do
    for J := 0 to High(ATransforms) do
      if not ContainsTransform(ATransforms,
        ComposeTransforms(ATransforms[I], ATransforms[J])) then
        Exit(False);
  Result := True;
end;

procedure TransformCoordinate(const ATransform: TOracleTransform;
  const AX, AY, AZ: Integer; const ASourceSize: TOracleTriple;
  out ADestinationSize: TOracleTriple; out ADestination: TOracleTriple);
var
  I: Integer;
  LLogical: TOracleTriple;
  LTransformed: TOracleTriple;
begin
  LLogical[0] := AX;
  LLogical[1] := ASourceSize[1] - 1 - AY;
  LLogical[2] := AZ;
  for I := 0 to 2 do
    ADestinationSize[ATransform.Axis[I].Destination] := ASourceSize[I];
  for I := 0 to 2 do
    if ATransform.Axis[I].Sign > 0 then
      LTransformed[ATransform.Axis[I].Destination] := LLogical[I]
    else
      LTransformed[ATransform.Axis[I].Destination] :=
        ASourceSize[I] - 1 - LLogical[I];
  ADestination[0] := LTransformed[0];
  ADestination[1] := ADestinationSize[1] - 1 - LTransformed[1];
  ADestination[2] := LTransformed[2];
end;

function FindOracleToken(const ATokens: TWfcModelTokens;
  const AToken: TWfcModelToken): Integer;
var
  I: Integer;
begin
  for I := 0 to High(ATokens) do
    if ATokens[I] = AToken then
      Exit(I);
  Result := -1;
end;

function AddOracleToken(var ATokens: TWfcModelTokens;
  const AToken: TWfcModelToken): Integer;
begin
  Result := FindOracleToken(ATokens, AToken);
  if Result >= 0 then
    Exit;
  Result := Length(ATokens);
  SetLength(ATokens, Result + 1);
  ATokens[Result] := AToken;
end;

procedure DirectionDelta(const ADirection: TWfcModelDirection;
  out ADeltaX, ADeltaY, ADeltaZ: Integer);
begin
  ADeltaX := 0;
  ADeltaY := 0;
  ADeltaZ := 0;
  case ADirection of
    wmdNorth:
      ADeltaY := -1;
    wmdEast:
      ADeltaX := 1;
    wmdSouth:
      ADeltaY := 1;
    wmdWest:
      ADeltaX := -1;
    wmdUp:
      ADeltaZ := 1;
    wmdDown:
      ADeltaZ := -1;
  else
    raise ERangeError.Create('unknown oracle direction');
  end;
end;

function TryNeighbor(const AX, AY, AZ, AWidth, AHeight, ADepth: Integer;
  const ADirection: TWfcModelDirection;
  const ABoundary: TWfcModelBoundary;
  out ANeighborX, ANeighborY, ANeighborZ: Integer): Boolean;
var
  LDeltaX: Integer;
  LDeltaY: Integer;
  LDeltaZ: Integer;
begin
  DirectionDelta(ADirection, LDeltaX, LDeltaY, LDeltaZ);
  ANeighborX := AX + LDeltaX;
  ANeighborY := AY + LDeltaY;
  ANeighborZ := AZ + LDeltaZ;
  if ABoundary = wmbWrap then
  begin
    if ANeighborX < 0 then ANeighborX := AWidth - 1
    else if ANeighborX >= AWidth then ANeighborX := 0;
    if ANeighborY < 0 then ANeighborY := AHeight - 1
    else if ANeighborY >= AHeight then ANeighborY := 0;
    if ANeighborZ < 0 then ANeighborZ := ADepth - 1
    else if ANeighborZ >= ADepth then ANeighborZ := 0;
    Exit(True);
  end;
  Result := (ANeighborX >= 0) and (ANeighborX < AWidth) and
    (ANeighborY >= 0) and (ANeighborY < AHeight) and
    (ANeighborZ >= 0) and (ANeighborZ < ADepth);
end;

procedure BuildOracle(const ASamples: TWfcLearnVolumeSamples;
  const ABoundary: TWfcModelBoundary;
  const ASymmetry: TWfcModelSymmetry; out AOracle: TOracleModel);
var
  D: TWfcModelDirection;
  I: Integer;
  J: Integer;
  K: Integer;
  LDestination: TOracleTriple;
  LDestinationSize: TOracleTriple;
  LGrid: TOracleIntegers;
  LNeighborX: Integer;
  LNeighborY: Integer;
  LNeighborZ: Integer;
  LSource: Integer;
  LSourceSize: TOracleTriple;
  LTarget: Integer;
  LTransforms: TOracleTransforms;
  LValueCount: Integer;
  X: Integer;
  Y: Integer;
  Z: Integer;
begin
  AOracle.Tokens := nil;
  AOracle.Weights := nil;
  AOracle.Relations := nil;
  AOracle.Shapes := nil;
  AOracle.Boundary := ABoundary;
  AOracle.Symmetry := ASymmetry;

  SetLength(AOracle.Shapes, Length(ASamples));
  for I := 0 to High(ASamples) do
  begin
    AOracle.Shapes[I].Width := ASamples[I].Width;
    AOracle.Shapes[I].Height := ASamples[I].Height;
    AOracle.Shapes[I].Depth := ASamples[I].Depth;
    for J := 0 to High(ASamples[I].Tokens) do
      AddOracleToken(AOracle.Tokens, ASamples[I].Tokens[J]);
  end;

  LValueCount := Length(AOracle.Tokens);
  SetLength(AOracle.Weights, LValueCount);
  SetLength(AOracle.Relations, 6 * LValueCount * LValueCount);
  BuildTransforms(ASymmetry, LTransforms);

  for I := 0 to High(ASamples) do
  begin
    LSourceSize[0] := ASamples[I].Width;
    LSourceSize[1] := ASamples[I].Height;
    LSourceSize[2] := ASamples[I].Depth;
    for J := 0 to High(LTransforms) do
    begin
      { Materialize every transformed volume before observing it. This is
        intentionally independent of direction-orbit aggregation. }
      TransformCoordinate(LTransforms[J], 0, 0, 0, LSourceSize,
        LDestinationSize, LDestination);
      SetLength(LGrid, LDestinationSize[0] * LDestinationSize[1] *
        LDestinationSize[2]);
      for K := 0 to High(LGrid) do
        LGrid[K] := -1;

      for Z := 0 to ASamples[I].Depth - 1 do
        for Y := 0 to ASamples[I].Height - 1 do
          for X := 0 to ASamples[I].Width - 1 do
          begin
            TransformCoordinate(LTransforms[J], X, Y, Z, LSourceSize,
              LDestinationSize, LDestination);
            K := CellIndex(LDestination[0], LDestination[1],
              LDestination[2], LDestinationSize[0], LDestinationSize[1]);
            LGrid[K] := FindOracleToken(AOracle.Tokens,
              ASamples[I].Tokens[CellIndex(X, Y, Z,
                ASamples[I].Width, ASamples[I].Height)]);
          end;

      for Z := 0 to LDestinationSize[2] - 1 do
        for Y := 0 to LDestinationSize[1] - 1 do
          for X := 0 to LDestinationSize[0] - 1 do
          begin
            LSource := LGrid[CellIndex(X, Y, Z,
              LDestinationSize[0], LDestinationSize[1])];
            if LSource < 0 then
              raise Exception.Create('oracle transform left a cell empty');
            Inc(AOracle.Weights[LSource]);
            for D := Low(TWfcModelDirection) to High(TWfcModelDirection) do
              if TryNeighbor(X, Y, Z, LDestinationSize[0],
                LDestinationSize[1], LDestinationSize[2], D, ABoundary,
                LNeighborX, LNeighborY, LNeighborZ) then
              begin
                LTarget := LGrid[CellIndex(LNeighborX, LNeighborY,
                  LNeighborZ, LDestinationSize[0], LDestinationSize[1])];
                Inc(AOracle.Relations[RelationIndex(D, LSource, LTarget,
                  LValueCount)]);
              end;
          end;
    end;
  end;
end;

function ModelMatchesOracle(const AModel: TWfcModel;
  const AOracle: TOracleModel; out ADetail: String): Boolean;
var
  D: TWfcModelDirection;
  I: Integer;
  J: Integer;
  LActual: Integer;
  LExpected: Integer;
  LShape: TWfcModelSampleShape;
begin
  Result := False;
  ADetail := '';
  if not Assigned(AModel) then
  begin
    ADetail := 'model is nil';
    Exit;
  end;
  if AModel.Rank <> 3 then
  begin
    ADetail := Format('rank %d', [AModel.Rank]);
    Exit;
  end;
  if AModel.Boundary <> AOracle.Boundary then
  begin
    ADetail := 'boundary mismatch';
    Exit;
  end;
  if AModel.Symmetry <> AOracle.Symmetry then
  begin
    ADetail := 'symmetry mismatch';
    Exit;
  end;
  if AModel.Directions <> MODEL_DIRECTIONS then
  begin
    ADetail := 'direction-set mismatch';
    Exit;
  end;
  if AModel.SampleCount <> Length(AOracle.Shapes) then
  begin
    ADetail := Format('sample count %d expected %d',
      [AModel.SampleCount, Length(AOracle.Shapes)]);
    Exit;
  end;
  for I := 0 to High(AOracle.Shapes) do
  begin
    LShape := AModel.SampleShapeAt(I);
    if (LShape.Width <> AOracle.Shapes[I].Width) or
      (LShape.Height <> AOracle.Shapes[I].Height) or
      (LShape.Depth <> AOracle.Shapes[I].Depth) then
    begin
      ADetail := Format('sample shape mismatch at %d', [I]);
      Exit;
    end;
  end;
  if AModel.ValueCount <> Length(AOracle.Tokens) then
  begin
    ADetail := Format('value count %d expected %d',
      [AModel.ValueCount, Length(AOracle.Tokens)]);
    Exit;
  end;
  for I := 0 to High(AOracle.Tokens) do
  begin
    if AModel.TokenAt(I) <> AOracle.Tokens[I] then
    begin
      ADetail := Format('token mismatch at %d', [I]);
      Exit;
    end;
    if AModel.WeightAt(I) <> AOracle.Weights[I] then
    begin
      ADetail := Format('weight mismatch at %d: %d expected %d',
        [I, AModel.WeightAt(I), AOracle.Weights[I]]);
      Exit;
    end;
  end;
  for D := Low(TWfcModelDirection) to High(TWfcModelDirection) do
    for I := 0 to Pred(AModel.ValueCount) do
      for J := 0 to Pred(AModel.ValueCount) do
      begin
        LActual := AModel.RelationCount(D, I, J);
        LExpected := AOracle.Relations[RelationIndex(D, I, J,
          AModel.ValueCount)];
        if LActual <> LExpected then
        begin
          ADetail := Format('relation mismatch d=%d s=%d t=%d: %d expected %d',
            [Ord(D), I, J, LActual, LExpected]);
          Exit;
        end;
      end;
  Result := True;
end;

procedure CheckModelAgainstOracle(const AModel: TWfcModel;
  const AOracle: TOracleModel; const AMessage: String);
var
  LDetail: String;
begin
  if ModelMatchesOracle(AModel, AOracle, LDetail) then
    Check(True, AMessage)
  else
    Check(False, AMessage + ': ' + LDetail);
end;

function MakeUniqueTokens(const AWidth, AHeight, ADepth: Integer;
  const APrefix: String): TWfcModelTokens;
var
  X: Integer;
  Y: Integer;
  Z: Integer;
begin
  Result := nil;
  SetLength(Result, AWidth * AHeight * ADepth);
  for Z := 0 to ADepth - 1 do
    for Y := 0 to AHeight - 1 do
      for X := 0 to AWidth - 1 do
        Result[CellIndex(X, Y, Z, AWidth, AHeight)] :=
          TWfcModelToken(Format('%s%d%d%d', [APrefix, X, Y, Z]));
end;

function DirectionTotal(const AModel: TWfcModel;
  const ADirection: TWfcModelDirection): Integer;
var
  I: Integer;
  J: Integer;
begin
  Result := 0;
  for I := 0 to Pred(AModel.ValueCount) do
    for J := 0 to Pred(AModel.ValueCount) do
      Inc(Result, AModel.RelationCount(ADirection, I, J));
end;

function TotalsMatch(const AModel: TWfcModel;
  const AExpected: TDirectionTotals): Boolean;
var
  D: TWfcModelDirection;
begin
  for D := Low(TWfcModelDirection) to High(TWfcModelDirection) do
    if DirectionTotal(AModel, D) <> AExpected[D] then
      Exit(False);
  Result := True;
end;

function AllWeightsEqual(const AModel: TWfcModel;
  const AExpected: Integer): Boolean;
var
  I: Integer;
begin
  for I := 0 to Pred(AModel.ValueCount) do
    if AModel.WeightAt(I) <> AExpected then
      Exit(False);
  Result := True;
end;

procedure ClearTotals(out ATotals: TDirectionTotals);
var
  D: TWfcModelDirection;
begin
  for D := Low(TWfcModelDirection) to High(TWfcModelDirection) do
    ATotals[D] := 0;
end;

procedure FillTotals(out ATotals: TDirectionTotals;
  const ANorth, AEast, ASouth, AWest, AUp, ADown: Integer);
begin
  ATotals[wmdNorth] := ANorth;
  ATotals[wmdEast] := AEast;
  ATotals[wmdSouth] := ASouth;
  ATotals[wmdWest] := AWest;
  ATotals[wmdUp] := AUp;
  ATotals[wmdDown] := ADown;
end;

procedure CheckRelation(const AModel: TWfcModel;
  const ADirection: TWfcModelDirection;
  const ASourceToken, ATargetToken: TWfcModelToken;
  const AExpected: Integer; const AMessage: String);
var
  LSource: Integer;
  LTarget: Integer;
begin
  LSource := AModel.FindToken(ASourceToken);
  LTarget := AModel.FindToken(ATargetToken);
  Check((LSource >= 0) and (LTarget >= 0) and
    (AModel.RelationCount(ADirection, LSource, LTarget) = AExpected),
    AMessage);
end;

function GraphValueAsModelToken(const AValue: TGraphValue): TWfcModelToken;
begin
  {$IFDEF PAS2JS}
  Result := TWfcModelToken(AValue);
  {$ELSE}
  Result := TWfcModelToken(UTF8Encode(UnicodeString(AValue)));
  {$ENDIF}
end;

function ModelTokenAsGraphValue(const AToken: TWfcModelToken): TGraphValue;
begin
  {$IFDEF PAS2JS}
  Result := TGraphValue(AToken);
  {$ELSE}
  Result := TGraphValue(UTF8Decode(AToken));
  {$ENDIF}
end;

function NonAsciiToken: TWfcModelToken;
begin
  {$IFDEF PAS2JS}
  Result := TWfcModelToken(Chr($266B));
  {$ELSE}
  Result := TWfcModelToken(UTF8Encode(UnicodeString(WideChar($266B))));
  {$ENDIF}
end;

function ExpectedGraphDirection(
  const ADirection: TWfcModelDirection): TGraphDirection;
begin
  case ADirection of
    wmdNorth:
      Result := gdNorth;
    wmdEast:
      Result := gdWest;
    wmdSouth:
      Result := gdSouth;
    wmdWest:
      Result := gdEast;
    wmdUp:
      Result := gdDown;
    wmdDown:
      Result := gdUp;
  else
    raise ERangeError.Create('unknown adapter direction');
  end;
end;

function GraphRulesMatchModel(const AGraph: TGraph;
  const AModel: TWfcModel): Boolean;
var
  D: TWfcModelDirection;
  I: Integer;
  J: Integer;
  LExpectedDenied: TGraphDirections;
  LExpectedRuleCount: Integer;
  LGraphDirection: TGraphDirection;
  LGroup: TGraphRuleGroup;
  LRule: TGraphRule;
  LTargetCount: Integer;
  LTargetPosition: Integer;
  LValue: TGraphValue;
begin
  Result := False;
  if AGraph.RuleGroups.Count <> AModel.ValueCount then
    Exit;
  for I := 0 to Pred(AModel.ValueCount) do
  begin
    LValue := ModelTokenAsGraphValue(AModel.TokenAt(I));
    if not AGraph.RuleGroups.ContainsKey(LValue) then
      Exit;
    LGroup := AGraph.RuleGroups[LValue];
    if (LGroup.Weight <> AModel.WeightAt(I)) or LGroup.HasRequired or
      (Length(LGroup.PreviousValues) <> 0) then
      Exit;
    LExpectedDenied := [];
    LExpectedRuleCount := 0;
    for D := Low(TWfcModelDirection) to High(TWfcModelDirection) do
    begin
      LTargetCount := 0;
      for J := 0 to Pred(AModel.ValueCount) do
        if AModel.RelationCount(D, I, J) > 0 then
          Inc(LTargetCount);
      LGraphDirection := ExpectedGraphDirection(D);
      if LTargetCount = 0 then
      begin
        Include(LExpectedDenied, LGraphDirection);
        if LGroup.Exists[LGraphDirection] or
          (not LGroup.Denied[LGraphDirection]) then
          Exit;
      end
      else
      begin
        Inc(LExpectedRuleCount);
        if (not LGroup.Exists[LGraphDirection]) or
          LGroup.Denied[LGraphDirection] then
          Exit;
        LRule := LGroup.Rule[LGraphDirection];
        if LRule.Info or (Length(LRule.Value) <> LTargetCount) then
          Exit;
        LTargetPosition := 0;
        for J := 0 to Pred(AModel.ValueCount) do
          if AModel.RelationCount(D, I, J) > 0 then
          begin
            if LRule.Value[LTargetPosition] <>
              ModelTokenAsGraphValue(AModel.TokenAt(J)) then
              Exit;
            Inc(LTargetPosition);
          end;
      end;
    end;
    if (LGroup.DeniedDirections <> LExpectedDenied) or
      (Length(LGroup.Rules) <> LExpectedRuleCount) then
      Exit;
  end;
  Result := True;
end;

function SameGraphAssignments(const ALeft, ARight: TGraph): Boolean;
var
  X: Integer;
  Y: Integer;
  Z: Integer;
begin
  if (ALeft.Dimension.Width <> ARight.Dimension.Width) or
    (ALeft.Dimension.Height <> ARight.Dimension.Height) or
    (ALeft.Dimension.Depth <> ARight.Dimension.Depth) then
    Exit(False);
  for Z := 0 to Integer(ALeft.Dimension.Depth) - 1 do
    for Y := 0 to Integer(ALeft.Dimension.Height) - 1 do
      for X := 0 to Integer(ALeft.Dimension.Width) - 1 do
        if ALeft.Entry[X, Y, Z].Empty or ARight.Entry[X, Y, Z].Empty or
          (ALeft.Entry[X, Y, Z].Value <> ARight.Entry[X, Y, Z].Value) then
          Exit(False);
  Result := True;
end;

function GraphAssignmentMatchesModel(const AGraph: TGraph;
  const AModel: TWfcModel; const ABoundary: TWfcModelBoundary): Boolean;
var
  D: TWfcModelDirection;
  LNeighbor: Integer;
  LNeighborX: Integer;
  LNeighborY: Integer;
  LNeighborZ: Integer;
  LSource: Integer;
  X: Integer;
  Y: Integer;
  Z: Integer;
begin
  for Z := 0 to Integer(AGraph.Dimension.Depth) - 1 do
    for Y := 0 to Integer(AGraph.Dimension.Height) - 1 do
      for X := 0 to Integer(AGraph.Dimension.Width) - 1 do
      begin
        if AGraph.Entry[X, Y, Z].Empty then
          Exit(False);
        LSource := AModel.FindToken(GraphValueAsModelToken(
          AGraph.Entry[X, Y, Z].Value));
        if LSource < 0 then
          Exit(False);
        for D := Low(TWfcModelDirection) to High(TWfcModelDirection) do
          if TryNeighbor(X, Y, Z, Integer(AGraph.Dimension.Width),
            Integer(AGraph.Dimension.Height), Integer(AGraph.Dimension.Depth),
            D, ABoundary, LNeighborX, LNeighborY, LNeighborZ) then
          begin
            if AGraph.Entry[LNeighborX, LNeighborY, LNeighborZ].Empty then
              Exit(False);
            LNeighbor := AModel.FindToken(GraphValueAsModelToken(
              AGraph.Entry[LNeighborX, LNeighborY, LNeighborZ].Value));
            if (LNeighbor < 0) or
              (AModel.RelationCount(D, LSource, LNeighbor) <= 0) then
              Exit(False);
          end;
      end;
  Result := True;
end;

procedure AssignSampleToGraph(const AGraph: TGraph;
  const ATokens: TWfcModelTokens; const AWidth, AHeight, ADepth: Integer);
var
  X: Integer;
  Y: Integer;
  Z: Integer;
begin
  for Z := 0 to ADepth - 1 do
    for Y := 0 to AHeight - 1 do
      for X := 0 to AWidth - 1 do
        AGraph.Entry[X, Y, Z].Value := ModelTokenAsGraphValue(
          ATokens[CellIndex(X, Y, Z, AWidth, AHeight)]);
end;

procedure ConfigureGraph(const AGraph: TGraph; const AModel: TWfcModel;
  const AWidth, AHeight, ADepth: Integer;
  const AWrap: Boolean; const ASeed: TGraphSeed);
begin
  AGraph.Seed := ASeed;
  AGraph.Reshape(AWidth, AHeight, ADepth);
  AGraph.WrapNeighbors := AWrap;
  ApplyModelToGraph(AModel, AGraph);
end;

procedure TestTransformEnumerationAndVersions;
var
  I: Integer;
  J: Integer;
  LImproper: Integer;
  LInput: TWfcModelTokens;
  LModel: TWfcModel;
  LOracle: TOracleModel;
  LProper: Integer;
  LSample: TWfcLearnVolumeSample;
  LSamples: TWfcLearnVolumeSamples;
  LSymmetry: TWfcModelSymmetry;
  LTransforms: TOracleTransforms;
begin
  Check(WFC_LEARN_3D_ALGORITHM_VERSION = 1,
    'the 3D learner publishes algorithm version 1');
  Check((Ord(wmdNorth) = 0) and (Ord(wmdEast) = 1) and
    (Ord(wmdSouth) = 2) and (Ord(wmdWest) = 3) and
    (Ord(wmdUp) = 4) and (Ord(wmdDown) = 5),
    'the six direction planes retain their canonical order');
  Check((Ord(wmsNone) = 0) and (Ord(wmsD4) = 1) and
    (Ord(wmsCubeRotations) = 2) and (Ord(wmsCubeFull) = 3),
    'the four symmetry policies retain their canonical order');

  LInput := MakeUniqueTokens(2, 1, 1, 'copy');
  LSample := MakeLearnSample3D(LInput, 2, 1, 1);
  LInput[0] := 'changed';
  Check((LSample.Width = 2) and (LSample.Height = 1) and
    (LSample.Depth = 1) and (Length(LSample.Tokens) = 2) and
    (LSample.Tokens[0] = 'copy000'),
    'the volume-sample helper owns an isolated token copy');

  SetLength(LInput, 2);
  LInput[0] := NonAsciiToken;
  LInput[1] := 'plain';
  SetLength(LSamples, 1);
  LSamples[0] := MakeLearnSample3D(LInput, 2, 1, 1);
  BuildOracle(LSamples, wmbOpen, wmsCubeFull, LOracle);
  LModel := LearnModel3D(LInput, 2, 1, 1, wmbOpen, wmsCubeFull);
  try
    CheckModelAgainstOracle(LModel, LOracle,
      'a non-ASCII token retains identity through full cube learning');
    Check((LModel.FindToken(NonAsciiToken) = 0) and
      (LModel.TokenAt(0) = NonAsciiToken),
      'non-ASCII lookup preserves first-seen vocabulary order');
  finally
    LModel.Free;
  end;

  for LSymmetry := Low(TWfcModelSymmetry) to High(TWfcModelSymmetry) do
  begin
    BuildTransforms(LSymmetry, LTransforms);
    Check(Length(LTransforms) = TransformCount(LSymmetry),
      Format('policy %d enumerates its exact transform multiplicity',
        [Ord(LSymmetry)]));
    Check(TransformSetIsUnique(LTransforms),
      Format('policy %d contains no duplicate signed transforms',
        [Ord(LSymmetry)]));
    Check(TransformSetIsClosed(LTransforms),
      Format('policy %d is closed under transform composition',
        [Ord(LSymmetry)]));
    LProper := 0;
    LImproper := 0;
    for I := 0 to High(LTransforms) do
      if TransformDeterminant(LTransforms[I]) = 1 then
        Inc(LProper)
      else
        Inc(LImproper);
    case LSymmetry of
      wmsNone:
        Check((LProper = 1) and (LImproper = 0),
          'identity policy contains only the proper identity');
      wmsD4:
        begin
          Check((LProper = 4) and (LImproper = 4),
            'D4 contains four yaw rotations and four vertical reflections');
          J := 0;
          for I := 0 to High(LTransforms) do
            if (LTransforms[I].Axis[2].Destination = 2) and
              (LTransforms[I].Axis[2].Sign = 1) then
              Inc(J);
          Check(J = 8, 'every D4 member preserves the Up axis');
        end;
      wmsCubeRotations:
        Check((LProper = 24) and (LImproper = 0),
          'cube rotations enumerate the 24 determinant-positive members');
      wmsCubeFull:
        Check((LProper = 24) and (LImproper = 24),
          'full cube symmetry enumerates all 48 signed permutations');
    end;
  end;
end;

procedure TestAsymmetricVolumeMatrices;
var
  LBoundary: TWfcModelBoundary;
  LExpected: TDirectionTotals;
  LModel: TWfcModel;
  LOracle: TOracleModel;
  LSamples: TWfcLearnVolumeSamples;
  LSymmetry: TWfcModelSymmetry;
  LTokens: TWfcModelTokens;
begin
  LTokens := MakeUniqueTokens(2, 3, 4, 'v');
  SetLength(LSamples, 1);
  LSamples[0] := MakeLearnSample3D(LTokens, 2, 3, 4);
  for LBoundary := Low(TWfcModelBoundary) to High(TWfcModelBoundary) do
    for LSymmetry := Low(TWfcModelSymmetry) to High(TWfcModelSymmetry) do
    begin
      BuildOracle(LSamples, LBoundary, LSymmetry, LOracle);
      LModel := LearnModel3D(LTokens, 2, 3, 4, LBoundary, LSymmetry);
      try
        CheckModelAgainstOracle(LModel, LOracle,
          Format('2x3x4 policy %d boundary %d matches the literal-volume oracle',
            [Ord(LSymmetry), Ord(LBoundary)]));
        Check(AllWeightsEqual(LModel, TransformCount(LSymmetry)),
          Format('2x3x4 policy %d preserves raw transform multiplicity',
            [Ord(LSymmetry)]));
        if LBoundary = wmbOpen then
          case LSymmetry of
            wmsNone:
              FillTotals(LExpected, 16, 12, 16, 12, 18, 18);
            wmsD4:
              FillTotals(LExpected, 112, 112, 112, 112, 144, 144);
            wmsCubeRotations:
              FillTotals(LExpected, 368, 368, 368, 368, 368, 368);
            wmsCubeFull:
              FillTotals(LExpected, 736, 736, 736, 736, 736, 736);
          end
        else
          case LSymmetry of
            wmsNone:
              FillTotals(LExpected, 24, 24, 24, 24, 24, 24);
            wmsD4:
              FillTotals(LExpected, 192, 192, 192, 192, 192, 192);
            wmsCubeRotations:
              FillTotals(LExpected, 576, 576, 576, 576, 576, 576);
            wmsCubeFull:
              FillTotals(LExpected, 1152, 1152, 1152, 1152, 1152, 1152);
          end;
        Check(TotalsMatch(LModel, LExpected),
          Format('2x3x4 policy %d boundary %d has literal direction totals',
            [Ord(LSymmetry), Ord(LBoundary)]));

        if (LBoundary = wmbOpen) and (LSymmetry = wmsNone) then
        begin
          CheckRelation(LModel, wmdEast, 'v000', 'v100', 1,
            'East advances raw X');
          CheckRelation(LModel, wmdSouth, 'v000', 'v010', 1,
            'South advances raw Y');
          CheckRelation(LModel, wmdUp, 'v000', 'v001', 1,
            'Up advances raw Z');
          CheckRelation(LModel, wmdNorth, 'v012', 'v002', 1,
            'North decreases raw Y');
          CheckRelation(LModel, wmdEast, 'v012', 'v112', 1,
            'an interior East edge keeps Y and Z');
          CheckRelation(LModel, wmdSouth, 'v012', 'v022', 1,
            'an interior South edge keeps X and Z');
          CheckRelation(LModel, wmdUp, 'v012', 'v013', 1,
            'an interior Up edge keeps X and Y');
          CheckRelation(LModel, wmdDown, 'v012', 'v011', 1,
            'Down decreases raw Z');
          CheckRelation(LModel, wmdWest, 'v123', 'v023', 1,
            'West decreases raw X');
          CheckRelation(LModel, wmdNorth, 'v123', 'v113', 1,
            'top corner North relation uses the prior row');
          CheckRelation(LModel, wmdDown, 'v123', 'v122', 1,
            'top corner Down relation uses the prior plane');
          CheckRelation(LModel, wmdNorth, 'v000', 'v020', 0,
            'open North does not cross the sample boundary');
          CheckRelation(LModel, wmdWest, 'v000', 'v100', 0,
            'open West does not cross the sample boundary');
          CheckRelation(LModel, wmdDown, 'v000', 'v003', 0,
            'open Down does not cross the sample boundary');
        end;
        if (LBoundary = wmbWrap) and (LSymmetry = wmsNone) then
        begin
          CheckRelation(LModel, wmdEast, 'v000', 'v100', 1,
            'wrapped East advances raw X');
          CheckRelation(LModel, wmdWest, 'v000', 'v100', 1,
            'wrapped West returns through the other width-two X slot');
          CheckRelation(LModel, wmdNorth, 'v000', 'v020', 1,
            'wrapped North reaches the last raw row');
          CheckRelation(LModel, wmdSouth, 'v000', 'v010', 1,
            'wrapped South reaches the next raw row');
          CheckRelation(LModel, wmdUp, 'v000', 'v001', 1,
            'wrapped Up reaches the next plane');
          CheckRelation(LModel, wmdDown, 'v000', 'v003', 1,
            'wrapped Down reaches the last plane');
        end;
      finally
        LModel.Free;
      end;
    end;
end;

procedure CheckSingletonCase(const AWidth, AHeight, ADepth: Integer;
  const APrefix: String; const AOpenNone,
  AWrapNone: TDirectionTotals);
var
  LBoundary: TWfcModelBoundary;
  LExpected: TDirectionTotals;
  LModel: TWfcModel;
  LOracle: TOracleModel;
  LSamples: TWfcLearnVolumeSamples;
  LSymmetry: TWfcModelSymmetry;
  LTokens: TWfcModelTokens;
begin
  LTokens := MakeUniqueTokens(AWidth, AHeight, ADepth, APrefix);
  SetLength(LSamples, 1);
  LSamples[0] := MakeLearnSample3D(LTokens, AWidth, AHeight, ADepth);
  for LBoundary := Low(TWfcModelBoundary) to High(TWfcModelBoundary) do
    for LSymmetry := Low(TWfcModelSymmetry) to High(TWfcModelSymmetry) do
    begin
      BuildOracle(LSamples, LBoundary, LSymmetry, LOracle);
      LModel := LearnModel3D(LTokens, AWidth, AHeight, ADepth,
        LBoundary, LSymmetry);
      try
        CheckModelAgainstOracle(LModel, LOracle,
          Format('%dx%dx%d policy %d boundary %d matches the oracle',
            [AWidth, AHeight, ADepth, Ord(LSymmetry), Ord(LBoundary)]));
        if LSymmetry = wmsNone then
        begin
          if LBoundary = wmbOpen then LExpected := AOpenNone
          else LExpected := AWrapNone;
          Check(TotalsMatch(LModel, LExpected),
            Format('%dx%dx%d no-symmetry direction totals are literal',
              [AWidth, AHeight, ADepth]));
        end;
        if (AWidth = 1) and (AHeight = 1) and (ADepth = 1) then
        begin
          Check((LModel.WeightAt(0) = TransformCount(LSymmetry)),
            Format('1x1x1 policy %d retains duplicate transform weights',
              [Ord(LSymmetry)]));
          if LBoundary = wmbOpen then
            FillTotals(LExpected, 0, 0, 0, 0, 0, 0)
          else
            FillTotals(LExpected, TransformCount(LSymmetry),
              TransformCount(LSymmetry), TransformCount(LSymmetry),
              TransformCount(LSymmetry), TransformCount(LSymmetry),
              TransformCount(LSymmetry));
          Check(TotalsMatch(LModel, LExpected),
            Format('1x1x1 policy %d boundary %d retains raw self-edge counts',
              [Ord(LSymmetry), Ord(LBoundary)]));
        end;
      finally
        LModel.Free;
      end;
    end;
end;

procedure TestSingletonAxes;
var
  LOpen: TDirectionTotals;
  LWrap: TDirectionTotals;
begin
  FillTotals(LOpen, 8, 0, 8, 0, 9, 9);
  FillTotals(LWrap, 12, 12, 12, 12, 12, 12);
  CheckSingletonCase(1, 3, 4, 'x', LOpen, LWrap);

  FillTotals(LOpen, 0, 4, 0, 4, 6, 6);
  FillTotals(LWrap, 8, 8, 8, 8, 8, 8);
  CheckSingletonCase(2, 1, 4, 'y', LOpen, LWrap);

  FillTotals(LOpen, 4, 3, 4, 3, 0, 0);
  FillTotals(LWrap, 6, 6, 6, 6, 6, 6);
  CheckSingletonCase(2, 3, 1, 'z', LOpen, LWrap);

  ClearTotals(LOpen);
  FillTotals(LWrap, 1, 1, 1, 1, 1, 1);
  CheckSingletonCase(1, 1, 1, 'q', LOpen, LWrap);
end;

function HasCrossSampleRelation(const AModel: TWfcModel;
  const AFirstPrefix, ASecondPrefix: Char): Boolean;
var
  D: TWfcModelDirection;
  I: Integer;
  J: Integer;
  LFirst: String;
  LSecond: String;
begin
  for D := Low(TWfcModelDirection) to High(TWfcModelDirection) do
    for I := 0 to Pred(AModel.ValueCount) do
      for J := 0 to Pred(AModel.ValueCount) do
      begin
        LFirst := String(AModel.TokenAt(I));
        LSecond := String(AModel.TokenAt(J));
        if (Length(LFirst) > 0) and (Length(LSecond) > 0) and
          (((LFirst[1] = AFirstPrefix) and (LSecond[1] = ASecondPrefix)) or
           ((LFirst[1] = ASecondPrefix) and (LSecond[1] = AFirstPrefix))) and
          (AModel.RelationCount(D, I, J) <> 0) then
          Exit(True);
      end;
  Result := False;
end;

procedure TestCorpusAndMerge;
var
  LBoundary: TWfcModelBoundary;
  LDetail: String;
  LFirst: TWfcModel;
  LFirstTokens: TWfcModelTokens;
  LMerged: TWfcModel;
  LModel: TWfcModel;
  LModels: TWfcModels;
  LOracle: TOracleModel;
  LSamples: TWfcLearnVolumeSamples;
  LSecond: TWfcModel;
  LSecondTokens: TWfcModelTokens;
  LSymmetry: TWfcModelSymmetry;
begin
  LFirstTokens := MakeUniqueTokens(2, 2, 1, 'a');
  LSecondTokens := MakeUniqueTokens(1, 2, 3, 'b');
  SetLength(LSamples, 2);
  LSamples[0] := MakeLearnSample3D(LFirstTokens, 2, 2, 1);
  LSamples[1] := MakeLearnSample3D(LSecondTokens, 1, 2, 3);

  for LBoundary := Low(TWfcModelBoundary) to High(TWfcModelBoundary) do
    for LSymmetry := Low(TWfcModelSymmetry) to High(TWfcModelSymmetry) do
    begin
      BuildOracle(LSamples, LBoundary, LSymmetry, LOracle);
      LModel := LearnModel3DCorpus(LSamples, LBoundary, LSymmetry);
      LFirst := LearnModel3D(LFirstTokens, 2, 2, 1,
        LBoundary, LSymmetry);
      LSecond := LearnModel3D(LSecondTokens, 1, 2, 3,
        LBoundary, LSymmetry);
      LMerged := nil;
      try
        CheckModelAgainstOracle(LModel, LOracle,
          Format('corpus policy %d boundary %d matches the literal oracle',
            [Ord(LSymmetry), Ord(LBoundary)]));
        Check((LModel.SampleCount = 2) and
          (LModel.SampleShapeAt(0).Width = 2) and
          (LModel.SampleShapeAt(0).Height = 2) and
          (LModel.SampleShapeAt(0).Depth = 1) and
          (LModel.SampleShapeAt(1).Width = 1) and
          (LModel.SampleShapeAt(1).Height = 2) and
          (LModel.SampleShapeAt(1).Depth = 3),
          'corpus preserves both original volume shapes in order');
        Check((LModel.TokenAt(0) = LFirstTokens[0]) and
          (LModel.TokenAt(Length(LFirstTokens)) = LSecondTokens[0]),
          'corpus vocabulary follows sample order then x-fast cell order');
        Check(not HasCrossSampleRelation(LModel, 'a', 'b'),
          'corpus learning creates no seam between samples');

        SetLength(LModels, 2);
        LModels[0] := LFirst;
        LModels[1] := LSecond;
        LMerged := MergeWfcModels(LModels);
        if ModelMatchesOracle(LMerged, LOracle, LDetail) then
          Check(True,
            Format('merged volumes equal corpus policy %d boundary %d',
              [Ord(LSymmetry), Ord(LBoundary)]))
        else
          Check(False,
            Format('merged volumes equal corpus policy %d boundary %d: %s',
              [Ord(LSymmetry), Ord(LBoundary), LDetail]));
      finally
        LMerged.Free;
        LSecond.Free;
        LFirst.Free;
        LModel.Free;
      end;
    end;
end;

procedure TestGraphAdapterAndSolver;
var
  LFirstGraph: TGraph;
  LForcedGraph: TGraph;
  LModel: TWfcModel;
  LOptions: TGraphSolveOptions;
  LReport: TGraphSolveReport;
  LSecondGraph: TGraph;
  LTokens: TWfcModelTokens;
begin
  LTokens := MakeUniqueTokens(2, 3, 4, 'g');
  LModel := LearnModel3D(LTokens, 2, 3, 4, wmbOpen, wmsNone);
  LForcedGraph := TGraph.Create;
  try
    ConfigureGraph(LForcedGraph, LModel, 2, 3, 4, False, 17);
    Check(GraphRulesMatchModel(LForcedGraph, LModel),
      'the adapter maps all six finite relation planes to public graph rules');
    AssignSampleToGraph(LForcedGraph, LTokens, 2, 3, 4);
    LOptions := DefaultGraphSolveOptions;
    LOptions.MaxBacktracks := 65536;
    Check(LForcedGraph.TrySolve(LOptions, LReport),
      'the exact asymmetric sample satisfies its adapted open model');
    Check(GraphAssignmentMatchesModel(LForcedGraph, LModel, wmbOpen),
      'independent row-coordinate validation accepts every forced edge');
  finally
    LForcedGraph.Free;
    LModel.Free;
  end;

  LModel := LearnModel3D(LTokens, 2, 3, 4, wmbWrap, wmsNone);
  LFirstGraph := TGraph.Create;
  LSecondGraph := TGraph.Create;
  try
    ConfigureGraph(LFirstGraph, LModel, 2, 3, 4, True, 424242);
    ConfigureGraph(LSecondGraph, LModel, 2, 3, 4, True, 424242);
    LOptions := DefaultGraphSolveOptions;
    LOptions.MaxBacktracks := 65536;
    Check(LFirstGraph.TrySolve(LOptions, LReport),
      'the first wrapped volume solves with a fixed shared seed');
    Check(LSecondGraph.TrySolve(LOptions, LReport),
      'the second wrapped volume solves with the same fixed seed');
    Check(SameGraphAssignments(LFirstGraph, LSecondGraph),
      'equal models and seeds produce equal 3D assignments');
    Check(GraphAssignmentMatchesModel(LFirstGraph, LModel, wmbWrap),
      'independent validation accepts every wrapped generated edge');

    LFirstGraph.Reset;
    ConfigureGraph(LFirstGraph, LModel, 2, 3, 4, True, 424242);
    Check(LFirstGraph.TrySolve(LOptions, LReport),
      'a reset graph solves again after the model is reapplied');
    Check(SameGraphAssignments(LFirstGraph, LSecondGraph),
      'reset and reapply replay the same seeded 3D assignment');
  finally
    LSecondGraph.Free;
    LFirstGraph.Free;
    LModel.Free;
  end;
end;

begin
  WriteLn('WFC 3D model-learning conformance suite');
  WriteLn('=======================================');
  RunTest('transform enumeration and versions',
    @TestTransformEnumerationAndVersions);
  RunTest('asymmetric volume matrices', @TestAsymmetricVolumeMatrices);
  RunTest('singleton-axis matrices', @TestSingletonAxes);
  RunTest('volume corpus and merge', @TestCorpusAndMerge);
  RunTest('six-direction adapter and solver', @TestGraphAdapterAndSolver);
  WriteLn('=======================================');
  WriteLn(Format('%d checks, %d failures',
    [GCheckCount, GFailureCount]));

  if GFailureCount > 0 then
  {$IFDEF PAS2JS}
    raise Exception.CreateFmt('%d 3D model-learning checks failed',
      [GFailureCount]);
  {$ELSE}
    Halt(1);
  {$ENDIF}
end.
