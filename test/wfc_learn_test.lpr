program wfc_learn_test;

{$mode delphi}{$H+}

uses
  SysUtils,
  wfc,
  wfc_model,
  wfc_learn,
  wfc_model_text;

type
  TTestProcedure = procedure;

const
  GOLDEN_WRAP_ABA =
    'wfcm=1'#10 +
    'rank=1'#10 +
    'width=3'#10 +
    'height=1'#10 +
    'boundary=wrap'#10 +
    'symmetry=none'#10 +
    'directions=E,W'#10 +
    'values=2'#10 +
    'v=0,2,A'#10 +
    'v=1,1,B'#10 +
    'relations=6'#10 +
    'r=E,0,0,1'#10 +
    'r=E,0,1,1'#10 +
    'r=E,1,0,1'#10 +
    'r=W,0,0,1'#10 +
    'r=W,0,1,1'#10 +
    'r=W,1,0,1'#10 +
    'end'#10;

  GOLDEN_WRAP_CORPUS =
    'wfcm=2'#10 +
    'rank=1'#10 +
    'samples=2'#10 +
    's=0,3,1'#10 +
    's=1,2,1'#10 +
    'boundary=wrap'#10 +
    'symmetry=none'#10 +
    'directions=E,W'#10 +
    'values=4'#10 +
    'v=0,2,A'#10 +
    'v=1,1,B'#10 +
    'v=2,1,C'#10 +
    'v=3,1,D'#10 +
    'relations=10'#10 +
    'r=E,0,0,1'#10 +
    'r=E,0,1,1'#10 +
    'r=E,1,0,1'#10 +
    'r=E,2,3,1'#10 +
    'r=E,3,2,1'#10 +
    'r=W,0,0,1'#10 +
    'r=W,0,1,1'#10 +
    'r=W,1,0,1'#10 +
    'r=W,2,3,1'#10 +
    'r=W,3,2,1'#10 +
    'end'#10;

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

function Tokens(const AFirst: TWfcModelToken): TWfcModelTokens; overload;
begin
  Result := nil;
  SetLength(Result, 1);
  Result[0] := AFirst;
end;

function Tokens(const AFirst, ASecond: TWfcModelToken):
  TWfcModelTokens; overload;
begin
  Result := nil;
  SetLength(Result, 2);
  Result[0] := AFirst;
  Result[1] := ASecond;
end;

function Tokens(const AFirst, ASecond, AThird: TWfcModelToken):
  TWfcModelTokens; overload;
begin
  Result := nil;
  SetLength(Result, 3);
  Result[0] := AFirst;
  Result[1] := ASecond;
  Result[2] := AThird;
end;

function Tokens(const AFirst, ASecond, AThird,
  AFourth: TWfcModelToken): TWfcModelTokens; overload;
begin
  Result := nil;
  SetLength(Result, 4);
  Result[0] := AFirst;
  Result[1] := ASecond;
  Result[2] := AThird;
  Result[3] := AFourth;
end;

function RelationSlot(const ADirection: TWfcModelDirection;
  const ASource, ATarget, AValueCount: Integer): Integer;
begin
  Result := ((Ord(ADirection) * AValueCount) + ASource) *
    AValueCount + ATarget;
end;

function InvalidModelBoundary: TWfcModelBoundary;
var
  LOrdinal: Integer;
begin
  LOrdinal := 99;
  {$PUSH}{$R-}
  Result := TWfcModelBoundary(LOrdinal);
  {$POP}
end;

function InvalidModelSymmetry: TWfcModelSymmetry;
var
  LOrdinal: Integer;
begin
  LOrdinal := 99;
  {$PUSH}{$R-}
  Result := TWfcModelSymmetry(LOrdinal);
  {$POP}
end;

function InvalidModelDirection: TWfcModelDirection;
var
  LOrdinal: Integer;
begin
  LOrdinal := 99;
  {$PUSH}{$R-}
  Result := TWfcModelDirection(LOrdinal);
  {$POP}
end;

procedure SetRelation(var ARelations: TWfcModelIntegerArray;
  const ADirection: TWfcModelDirection;
  const ASource, ATarget, AValueCount, ACount: Integer);
begin
  ARelations[RelationSlot(ADirection, ASource, ATarget,
    AValueCount)] := ACount;
end;

function ReplaceOnce(const AText, AOld, ANew: String): String;
var
  LPosition: Integer;
begin
  LPosition := Pos(AOld, AText);
  if LPosition = 0 then
    raise Exception.Create('test fixture replacement text was not found');
  Result := Copy(AText, 1, LPosition - 1) + ANew +
    Copy(AText, LPosition + Length(AOld),
      Length(AText) - LPosition - Length(AOld) + 1);
end;

function MusicalNoteToken: TWfcModelToken;
begin
  {$IFDEF PAS2JS}
  Result := TWfcModelToken(Chr($266B));
  {$ELSE}
  Result := TWfcModelToken(UTF8Encode(
    UnicodeString(WideChar($266B))));
  {$ENDIF}
end;

function MalformedToken: TWfcModelToken;
begin
  {$IFDEF PAS2JS}
  Result := TWfcModelToken(Chr($D800));
  {$ELSE}
  SetLength(Result, 1);
  Result[1] := AnsiChar($FF);
  {$ENDIF}
end;

function GraphValueAsModelToken(const AValue: TGraphValue):
  TWfcModelToken;
begin
  {$IFDEF PAS2JS}
  Result := TWfcModelToken(AValue);
  {$ELSE}
  Result := TWfcModelToken(UTF8Encode(UnicodeString(AValue)));
  {$ENDIF}
end;

procedure CheckDecodeRejected(const AText, AMessage: String);
var
  LModel: TWfcModel;
  LRejectedCanonically: Boolean;
begin
  LModel := nil;
  LRejectedCanonically := False;
  try
    try
      LModel := DecodeWfcModelText(AText);
    except
      on E: EConvertError do
        LRejectedCanonically :=
          Copy(E.Message, 1, Length('invalid WFC model text: ')) =
            'invalid WFC model text: ';
      on E: Exception do
        LRejectedCanonically := False;
    end;
  finally
    LModel.Free;
  end;
  Check(LRejectedCanonically, AMessage);
end;

function ConstructorRejected(const ARank, AWidth, AHeight: Integer;
  const ABoundary: TWfcModelBoundary;
  const ASymmetry: TWfcModelSymmetry;
  const ADirections: TWfcModelDirections;
  const ATokens: TWfcModelTokens;
  const AWeights, ARelations: TWfcModelIntegerArray): Boolean;
var
  LModel: TWfcModel;
begin
  Result := False;
  LModel := nil;
  try
    try
      LModel := TWfcModel.Create(ARank, AWidth, AHeight,
        ABoundary, ASymmetry, ADirections, ATokens, AWeights,
        ARelations);
    except
      on E: Exception do
        Result := True;
    end;
  finally
    LModel.Free;
  end;
end;

function ShapeConstructorRejected(const ARank: Integer;
  const ASampleShapes: TWfcModelSampleShapes): Boolean;
var
  LDirections: TWfcModelDirections;
  LModel: TWfcModel;
  LRelations: TWfcModelIntegerArray;
  LWeights: TWfcModelIntegerArray;
begin
  Result := False;
  LModel := nil;
  SetLength(LWeights, 1);
  LWeights[0] := 1;
  SetLength(LRelations, 4);
  if ARank = 1 then
    LDirections := [wmdEast, wmdWest]
  else
    LDirections := [wmdNorth, wmdEast, wmdSouth, wmdWest];
  try
    try
      LModel := TWfcModel.Create(ARank, ASampleShapes, wmbOpen,
        wmsNone, LDirections, Tokens('X'), LWeights, LRelations);
    except
      on E: Exception do
        Result := True;
    end;
  finally
    LModel.Free;
  end;
end;

function MergeRejected(const AModels: TWfcModels): Boolean;
var
  LMerged: TWfcModel;
begin
  Result := False;
  LMerged := nil;
  try
    try
      LMerged := MergeWfcModels(AModels);
    except
      on E: Exception do
        Result := True;
    end;
  finally
    LMerged.Free;
  end;
end;

function MakeSingleTokenCountModel(const AWeight,
  ARelationCount: Integer): TWfcModel;
var
  LRelations: TWfcModelIntegerArray;
  LWeights: TWfcModelIntegerArray;
begin
  SetLength(LWeights, 1);
  LWeights[0] := AWeight;
  SetLength(LRelations, 4);
  SetRelation(LRelations, wmdEast, 0, 0, 1, ARelationCount);
  SetRelation(LRelations, wmdWest, 0, 0, 1, ARelationCount);
  Result := TWfcModel.Create(1, 1, 1, wmbWrap, wmsNone,
    [wmdEast, wmdWest], Tokens('X'), LWeights, LRelations);
end;

procedure TestLearn1DOpen;
var
  D: TWfcModelDirection;
  I: Integer;
  J: Integer;
  LExact: Boolean;
  LExpected: Integer;
  LModel: TWfcModel;
begin
  LModel := LearnModel1D(Tokens('A', 'B', 'A'), wmbOpen);
  try
    Check(WFC_LEARN_ALGORITHM_VERSION = 1,
      'the learning algorithm publishes replay version 1');
    Check((LModel.Rank = 1) and (LModel.SampleWidth = 3) and
      (LModel.SampleHeight = 1) and (LModel.Boundary = wmbOpen) and
      (LModel.Symmetry = wmsNone) and
      (LModel.Directions = [wmdEast, wmdWest]) and
      (LModel.ValueCount = 2),
      '1D learning records exact model metadata');
    Check((LModel.TokenAt(0) = 'A') and (LModel.TokenAt(1) = 'B') and
      (LModel.FindToken('A') = 0) and (LModel.FindToken('B') = 1) and
      (LModel.FindToken('missing') = -1),
      '1D learning preserves deterministic first-seen token order');
    Check((LModel.WeightAt(0) = 2) and (LModel.WeightAt(1) = 1),
      '1D learning retains raw token frequencies');

    LExact := True;
    for D := Low(TWfcModelDirection) to High(TWfcModelDirection) do
      for I := 0 to 1 do
        for J := 0 to 1 do
        begin
          LExpected := 0;
          if (D in [wmdEast, wmdWest]) and
            (((I = 0) and (J = 1)) or
             ((I = 1) and (J = 0))) then
            LExpected := 1;
          if LModel.RelationCount(D, I, J) <> LExpected then
            LExact := False;
        end;
    Check(LExact,
      'open A-B-A learns the exact bounded east/west relation counts');
  finally
    LModel.Free;
  end;
end;

procedure TestLearn1DWrap;
var
  D: TWfcModelDirection;
  I: Integer;
  J: Integer;
  LExact: Boolean;
  LExpected: Integer;
  LModel: TWfcModel;
  LSingle: TWfcModel;
begin
  LModel := LearnModel1D(Tokens('A', 'B', 'A'), wmbWrap);
  try
    LExact := True;
    for D := Low(TWfcModelDirection) to High(TWfcModelDirection) do
      for I := 0 to 1 do
        for J := 0 to 1 do
        begin
          LExpected := 0;
          if D in [wmdEast, wmdWest] then
          begin
            if (I = 0) and (J = 0) then
              LExpected := 1
            else if ((I = 0) and (J = 1)) or
              ((I = 1) and (J = 0)) then
              LExpected := 1;
          end;
          if LModel.RelationCount(D, I, J) <> LExpected then
            LExact := False;
        end;
    Check(LExact,
      'wrapped A-B-A adds the exact seam self edge in both directions');
  finally
    LModel.Free;
  end;

  LSingle := LearnModel1D(Tokens('X'), wmbWrap);
  try
    Check((LSingle.WeightAt(0) = 1) and
      (LSingle.RelationCount(wmdEast, 0, 0) = 1) and
      (LSingle.RelationCount(wmdWest, 0, 0) = 1),
      'a length-one wrapped sample observes east and west self arcs');
  finally
    LSingle.Free;
  end;
end;

function ExpectedOpen2DRelation(const ADirection: TWfcModelDirection;
  const ASource, ATarget: Integer): Integer;
begin
  Result := 0;
  case ADirection of
    wmdNorth:
      if ((ASource = 2) and (ATarget = 0)) or
        ((ASource = 3) and (ATarget = 1)) then
        Result := 1;
    wmdEast:
      if ((ASource = 0) and (ATarget = 1)) or
        ((ASource = 2) and (ATarget = 3)) then
        Result := 1;
    wmdSouth:
      if ((ASource = 0) and (ATarget = 2)) or
        ((ASource = 1) and (ATarget = 3)) then
        Result := 1;
    wmdWest:
      if ((ASource = 1) and (ATarget = 0)) or
        ((ASource = 3) and (ATarget = 2)) then
        Result := 1;
  end;
end;

function ExpectedD4SixRelation(const ASource, ATarget: Integer): Integer;
begin
  Result := 0;
  case ASource of
    0:
      if ATarget in [1, 3] then Result := 2;
    1:
      if ATarget in [0, 2, 4] then Result := 2;
    2:
      if ATarget in [1, 5] then Result := 2;
    3:
      if ATarget in [0, 4] then Result := 2;
    4:
      if ATarget in [1, 3, 5] then Result := 2;
    5:
      if ATarget in [2, 4] then Result := 2;
  end;
end;

procedure TestLearn2D;
var
  D: TWfcModelDirection;
  I: Integer;
  J: Integer;
  LExact: Boolean;
  LReciprocal: Boolean;
  LModel: TWfcModel;
begin
  LModel := LearnModel2D(Tokens('A', 'B', 'C', 'D'),
    2, 2, wmbOpen, wmsNone);
  try
    Check((LModel.Rank = 2) and (LModel.SampleWidth = 2) and
      (LModel.SampleHeight = 2) and
      (LModel.Directions = [wmdNorth, wmdEast, wmdSouth, wmdWest]) and
      (LModel.ValueCount = 4),
      '2D learning records shape and all cardinal directions');
    LExact := True;
    LReciprocal := True;
    for D := Low(TWfcModelDirection) to High(TWfcModelDirection) do
      for I := 0 to 3 do
        for J := 0 to 3 do
        begin
          if LModel.RelationCount(D, I, J) <>
            ExpectedOpen2DRelation(D, I, J) then
            LExact := False;
          if LModel.RelationCount(D, I, J) <>
            LModel.RelationCount(OppositeModelDirection(D), J, I) then
            LReciprocal := False;
        end;
    Check(LExact,
      'open 2x2 A-B/C-D learns every cardinal count exactly');
    Check(LReciprocal,
      'learned 2D relation counts are directionally reciprocal');
  finally
    LModel.Free;
  end;
end;

procedure TestD4Learning;
var
  D: TWfcModelDirection;
  I: Integer;
  J: Integer;
  LExact: Boolean;
  LModel: TWfcModel;
  LTokens: TWfcModelTokens;
begin
  LTokens := nil;
  SetLength(LTokens, 6);
  LTokens[0] := 'A';
  LTokens[1] := 'B';
  LTokens[2] := 'C';
  LTokens[3] := 'D';
  LTokens[4] := 'E';
  LTokens[5] := 'F';
  LModel := LearnModel2D(LTokens, 3, 2, wmbOpen, wmsD4);
  try
    LExact := (LModel.Symmetry = wmsD4) and
      (LModel.SampleWidth = 3) and (LModel.SampleHeight = 2) and
      (LModel.ValueCount = 6);
    for I := 0 to 5 do
      if (LModel.TokenAt(I) <> LTokens[I]) or
        (LModel.WeightAt(I) <> 8) then
        LExact := False;
    Check(LExact,
      'asymmetric 3x2 D4 preserves order and observes every token eight times');
    LExact := True;
    for D := Low(TWfcModelDirection) to High(TWfcModelDirection) do
      for I := 0 to 5 do
        for J := 0 to 5 do
          if LModel.RelationCount(D, I, J) <>
            ExpectedD4SixRelation(I, J) then
            LExact := False;
    Check(LExact,
      'asymmetric 3x2 D4 aggregation has exact cardinal relations');
  finally
    LModel.Free;
  end;
end;

procedure TestInvalidLearningInputs;
var
  LModel: TWfcModel;
  LRaised: Boolean;
  LSample: TWfcLearnSample;
  LTokens: TWfcModelTokens;
begin
  LTokens := nil;
  LModel := nil;
  LRaised := False;
  try
    LModel := LearnModel1D(LTokens, wmbOpen);
  except
    on E: Exception do LRaised := True;
  end;
  LModel.Free;
  Check(LRaised, '1D learning rejects an empty sample');

  LModel := nil;
  LRaised := False;
  try
    LModel := LearnModel2D(Tokens('A', 'B', 'C'), 2, 2,
      wmbOpen, wmsNone);
  except
    on E: Exception do LRaised := True;
  end;
  LModel.Free;
  Check(LRaised, '2D learning rejects a sample-size mismatch');

  LModel := nil;
  LRaised := False;
  try
    LModel := LearnModel2D(Tokens('A'), 0, 1,
      wmbOpen, wmsNone);
  except
    on E: Exception do LRaised := True;
  end;
  LModel.Free;
  Check(LRaised, '2D learning rejects zero dimensions');

  LRaised := False;
  try
    LSample := MakeLearnSample2D(LTokens, High(Integer), 2);
  except
    on E: Exception do LRaised := True;
  end;
  Check(LRaised and (Length(LSample.Tokens) = 0),
    'sample construction rejects overflowing dimensions before allocation');

  LModel := nil;
  LRaised := False;
  try
    LModel := LearnModel1D(Tokens(''), wmbOpen);
  except
    on E: Exception do LRaised := True;
  end;
  LModel.Free;
  Check(LRaised, 'learning rejects an empty token');

  LModel := nil;
  LRaised := False;
  try
    LModel := LearnModel1D(Tokens('A'), InvalidModelBoundary);
  except
    on E: Exception do LRaised := True;
  end;
  LModel.Free;
  Check(LRaised, 'learning rejects an unknown boundary enum');

  LModel := nil;
  LRaised := False;
  try
    LModel := LearnModel2D(Tokens('A'), 1, 1, wmbOpen,
      InvalidModelSymmetry);
  except
    on E: Exception do LRaised := True;
  end;
  LModel.Free;
  Check(LRaised, 'learning rejects an unknown symmetry enum');
end;

procedure TestModelImmutabilityAndGuards;
var
  LCopyRelations: TWfcModelIntegerArray;
  LCopyTokens: TWfcModelTokens;
  LCopyWeights: TWfcModelIntegerArray;
  LModel: TWfcModel;
  LRelations: TWfcModelIntegerArray;
  LRaised: Boolean;
  LTokens: TWfcModelTokens;
  LWeights: TWfcModelIntegerArray;
begin
  LTokens := Tokens('A', 'B');
  SetLength(LWeights, 2);
  LWeights[0] := 3;
  LWeights[1] := 2;
  SetLength(LRelations, 16);
  SetRelation(LRelations, wmdEast, 0, 1, 2, 4);
  SetRelation(LRelations, wmdWest, 1, 0, 2, 4);
  LModel := TWfcModel.Create(1, 2, 1, wmbOpen, wmsNone,
    [wmdEast, wmdWest], LTokens, LWeights, LRelations);
  try
    LTokens[0] := 'changed';
    LWeights[0] := 99;
    SetRelation(LRelations, wmdEast, 0, 1, 2, 99);
    Check((LModel.TokenAt(0) = 'A') and (LModel.WeightAt(0) = 3) and
      (LModel.RelationCount(wmdEast, 0, 1) = 4),
      'model construction deep-copies caller-owned arrays');

    LCopyTokens := LModel.CopyTokens;
    LCopyWeights := LModel.CopyWeights;
    LCopyRelations := LModel.CopyRelations;
    LCopyTokens[0] := 'copy-change';
    LCopyWeights[0] := 77;
    LCopyRelations[RelationSlot(wmdEast, 0, 1, 2)] := 77;
    Check((LModel.TokenAt(0) = 'A') and (LModel.WeightAt(0) = 3) and
      (LModel.RelationCount(wmdEast, 0, 1) = 4),
      'model copy accessors do not expose mutable internal arrays');

    LRaised := False;
    try LModel.TokenAt(-1); except on E: Exception do LRaised := True; end;
    Check(LRaised, 'TokenAt rejects a negative value index');
    LRaised := False;
    try LModel.WeightAt(2); except on E: Exception do LRaised := True; end;
    Check(LRaised, 'WeightAt rejects an index at ValueCount');
    LRaised := False;
    try LModel.RelationCount(wmdEast, -1, 0);
    except on E: Exception do LRaised := True; end;
    Check(LRaised, 'RelationCount rejects an invalid source index');
    LRaised := False;
    try LModel.RelationCount(InvalidModelDirection, 0, 0);
    except on E: Exception do LRaised := True; end;
    Check(LRaised, 'RelationCount rejects an unknown direction enum');
  finally
    LModel.Free;
  end;

  LTokens := Tokens('A', 'A');
  LWeights[0] := 1;
  LWeights[1] := 1;
  SetLength(LRelations, 16);
  Check(ConstructorRejected(1, 2, 1, wmbOpen, wmsNone,
    [wmdEast, wmdWest], LTokens, LWeights, LRelations),
    'the model constructor rejects duplicate tokens');

  LTokens := Tokens('A', 'B');
  LWeights[0] := 0;
  Check(ConstructorRejected(1, 2, 1, wmbOpen, wmsNone,
    [wmdEast, wmdWest], LTokens, LWeights, LRelations),
    'the model constructor rejects a nonpositive weight');

  LWeights[0] := 1;
  LRelations[0] := -1;
  Check(ConstructorRejected(1, 2, 1, wmbOpen, wmsNone,
    [wmdEast, wmdWest], LTokens, LWeights, LRelations),
    'the model constructor rejects a negative relation count');

  LRelations[0] := 0;
  SetRelation(LRelations, wmdEast, 0, 1, 2, 1);
  Check(ConstructorRejected(1, 2, 1, wmbOpen, wmsNone,
    [wmdEast, wmdWest], LTokens, LWeights, LRelations),
    'the model constructor rejects asymmetric opposite relations');

  SetRelation(LRelations, wmdEast, 0, 1, 2, 0);
  SetRelation(LRelations, wmdNorth, 0, 0, 2, 1);
  SetRelation(LRelations, wmdSouth, 0, 0, 2, 1);
  Check(ConstructorRejected(1, 2, 1, wmbOpen, wmsNone,
    [wmdEast, wmdWest], LTokens, LWeights, LRelations),
    'the model constructor rejects relations in inactive directions');

  SetRelation(LRelations, wmdNorth, 0, 0, 2, 0);
  SetRelation(LRelations, wmdSouth, 0, 0, 2, 0);
  Check(ConstructorRejected(1, 2, 1, InvalidModelBoundary,
    wmsNone, [wmdEast, wmdWest], LTokens, LWeights, LRelations),
    'the model constructor rejects an unknown boundary enum');
  Check(ConstructorRejected(1, 2, 1, wmbOpen,
    InvalidModelSymmetry, [wmdEast, wmdWest], LTokens,
    LWeights, LRelations),
    'the model constructor rejects an unknown symmetry enum');

  LTokens := Tokens(MalformedToken);
  SetLength(LWeights, 1);
  LWeights[0] := 1;
  SetLength(LRelations, 4);
  SetRelation(LRelations, wmdEast, 0, 0, 1, 1);
  SetRelation(LRelations, wmdWest, 0, 0, 1, 1);
  Check(ConstructorRejected(1, 1, 1, wmbWrap, wmsNone,
    [wmdEast, wmdWest], LTokens, LWeights, LRelations),
    'the model constructor rejects malformed Unicode token storage');
end;

function GraphMatchesModel(const AGraph: TGraph;
  const AModel: TWfcModel): Boolean;
var
  LCurrent: Integer;
  LNeighbor: Integer;
  X: Integer;
begin
  Result := True;
  for X := 0 to Integer(AGraph.Dimension.Width) - 1 do
  begin
    if AGraph.Entry[X, 0, 0].Empty then
      Exit(False);
    LCurrent := AModel.FindToken(TWfcModelToken(
      AGraph.Entry[X, 0, 0].Value));
    if LCurrent < 0 then
      Exit(False);
    LNeighbor := AModel.FindToken(TWfcModelToken(
      AGraph.Entry[X, 0, 0][gdEast].Value));
    if (LNeighbor < 0) or
      (AModel.RelationCount(wmdEast, LCurrent, LNeighbor) <= 0) then
      Exit(False);
    LNeighbor := AModel.FindToken(TWfcModelToken(
      AGraph.Entry[X, 0, 0][gdWest].Value));
    if (LNeighbor < 0) or
      (AModel.RelationCount(wmdWest, LCurrent, LNeighbor) <= 0) then
      Exit(False);
  end;
end;

procedure TestGraphAdapter;
var
  LExistingGroup: TGraph.TParentedGraphRuleGroup;
  LGraph: TGraph;
  LMessage: String;
  LModel: TWfcModel;
  LOpenModel: TWfcModel;
  LOptions: TGraphSolveOptions;
  LReport: TGraphSolveReport;
  LRoundTripOrAtomic: Boolean;
  LRaised: Boolean;
begin
  LModel := LearnModel1D(Tokens('A', 'B', 'A'), wmbWrap);
  LGraph := TGraph.Create;
  try
    LGraph.Seed := 12345;
    LGraph.Reshape(12, 1, 1);
    LGraph.WrapNeighbors := True;
    ApplyModelToGraph(LModel, LGraph);
    Check((LGraph.RuleGroups.Count = 2) and
      (LGraph.Rules['A'].Weight = 2) and
      (LGraph.Rules['B'].Weight = 1),
      'the adapter registers learned values with raw weights');
    Check(LGraph.Rules['A'].Exists[gdEast] and
      LGraph.Rules['A'].Exists[gdWest] and
      LGraph.Rules['B'].Exists[gdEast] and
      LGraph.Rules['B'].Exists[gdWest] and
      ContainsGraphValue(LGraph.Rules['A'].Rule[gdEast].Value, 'A') and
      ContainsGraphValue(LGraph.Rules['A'].Rule[gdEast].Value, 'B') and
      ContainsGraphValue(LGraph.Rules['B'].Rule[gdEast].Value, 'A'),
      'the adapter registers learned directional supports');
    LOptions := DefaultGraphSolveOptions;
    LOptions.MaxBacktracks := 4096;
    Check(LGraph.TrySolve(LOptions, LReport),
      'an adapted wrapped A-B-A model solves deterministically');
    Check(GraphMatchesModel(LGraph, LModel),
      'independent adjacency validation accepts every generated edge');
  finally
    LGraph.Free;
    LModel.Free;
  end;

  LOpenModel := LearnModel1D(Tokens('A', 'B'), wmbOpen);
  LGraph := TGraph.Create;
  try
    LGraph.Reshape(3, 1, 1);
    LGraph.WrapNeighbors := False;
    LRaised := False;
    LMessage := '';
    try
      ApplyModelToGraph(LOpenModel, LGraph);
    except
      on E: Exception do
      begin
        LRaised := True;
        LMessage := E.Message;
      end;
    end;
    Check(LRaised and
      (Pos('empty-support-not-representable', LMessage) > 0),
      'the adapter names the zero-support representation boundary');
    Check(LGraph.RuleGroups.Count = 0,
      'a zero-support rejection leaves the target graph empty');
  finally
    LGraph.Free;
    LOpenModel.Free;
  end;

  LModel := LearnModel1D(Tokens('A', 'B', 'A'), wmbWrap);
  LGraph := TGraph.Create;
  try
    LGraph.Reshape(2, 1, 1);
    LExistingGroup := LGraph.AddValue('existing', 7);
    LRaised := False;
    try
      ApplyModelToGraph(LModel, LGraph);
    except
      on E: Exception do LRaised := True;
    end;
    Check(LRaised and (LGraph.RuleGroups.Count = 1) and
      (LGraph.Rules['existing'] = LExistingGroup) and
      (LExistingGroup.Weight = 7),
      'the adapter rejects a nonempty pass without mutating it');
  finally
    LGraph.Free;
    LModel.Free;
  end;

  LModel := LearnModel1D(Tokens(MusicalNoteToken), wmbWrap);
  LGraph := TGraph.Create;
  try
    LGraph.Seed := 0;
    LGraph.Reshape(1, 1, 1);
    LGraph.WrapNeighbors := True;
    LRoundTripOrAtomic := False;
    try
      ApplyModelToGraph(LModel, LGraph);
      LOptions := DefaultGraphSolveOptions;
      LRoundTripOrAtomic := LGraph.TrySolve(LOptions, LReport) and
        (GraphValueAsModelToken(LGraph.Entry[0, 0, 0].Value) =
          MusicalNoteToken);
    except
      on E: Exception do
        LRoundTripOrAtomic := LGraph.RuleGroups.Count = 0;
    end;
    Check(LRoundTripOrAtomic,
      'Unicode adaptation preserves exact identity or rejects atomically');
  finally
    LGraph.Free;
    LModel.Free;
  end;
end;

procedure TestAsymmetricGraphAdapter;
var
  LGraph: TGraph;
  LModel: TWfcModel;
  LOptions: TGraphSolveOptions;
  LReport: TGraphSolveReport;
  LTokens: TWfcModelTokens;
begin
  LModel := LearnModel1D(Tokens('A', 'B', 'C'), wmbWrap);
  LGraph := TGraph.Create;
  try
    LGraph.Seed := 0;
    LGraph.Reshape(3, 1, 1);
    LGraph.WrapNeighbors := True;
    ApplyModelToGraph(LModel, LGraph);

    Check(
      ContainsGraphValue(LGraph.Rules['A'].Rule[gdWest].Value, 'B') and
      ContainsGraphValue(LGraph.Rules['A'].Rule[gdEast].Value, 'C'),
      'the adapter preserves asymmetric east and west orientation');

    LGraph.Entry[0, 0, 0].Value := 'A';
    LGraph.Entry[1, 0, 0].Value := 'B';
    LGraph.Entry[2, 0, 0].Value := 'C';
    LOptions := DefaultGraphSolveOptions;
    Check(LGraph.TrySolve(LOptions, LReport) and
      GraphMatchesModel(LGraph, LModel),
      'the adapted asymmetric cycle validates in learned orientation');
  finally
    LGraph.Free;
    LModel.Free;
  end;

  LTokens := Tokens('A', 'B', 'C');
  LModel := LearnModel2D(LTokens, 1, 3, wmbWrap, wmsNone);
  LGraph := TGraph.Create;
  try
    LGraph.Seed := 0;
    LGraph.Reshape(1, 3, 1);
    LGraph.WrapNeighbors := True;
    ApplyModelToGraph(LModel, LGraph);

    Check(
      ContainsGraphValue(LGraph.Rules['A'].Rule[gdNorth].Value, 'C') and
      ContainsGraphValue(LGraph.Rules['A'].Rule[gdSouth].Value, 'B'),
      'the adapter bridges row-major and graph vertical orientation');

    LGraph.Entry[0, 0, 0].Value := 'A';
    LGraph.Entry[0, 1, 0].Value := 'B';
    LGraph.Entry[0, 2, 0].Value := 'C';
    LOptions := DefaultGraphSolveOptions;
    Check(LGraph.TrySolve(LOptions, LReport),
      'the adapted vertical cycle validates in row-major orientation');
  finally
    LGraph.Free;
    LModel.Free;
  end;
end;

procedure TestCorpusLearning;
var
  D: TWfcModelDirection;
  I: Integer;
  J: Integer;
  LExact: Boolean;
  LExpected: Integer;
  LModel: TWfcModel;
  LNoCrossSampleEdge: Boolean;
  LOpenModel: TWfcModel;
  LSamples: TWfcLearnSamples;
begin
  Check(WFC_LEARN_CORPUS_ALGORITHM_VERSION = 1,
    'the corpus learner publishes replay version 1');
  SetLength(LSamples, 2);
  LSamples[0] := MakeLearnSample1D(Tokens('A', 'B', 'A'));
  LSamples[1] := MakeLearnSample1D(Tokens('C', 'D'));

  LModel := LearnModel1DCorpus(LSamples, wmbWrap);
  try
    Check((LModel.SampleCount = 2) and
      (LModel.SampleShapeAt(0).Width = 3) and
      (LModel.SampleShapeAt(0).Height = 1) and
      (LModel.SampleShapeAt(1).Width = 2) and
      (LModel.SampleShapeAt(1).Height = 1),
      'a corpus retains heterogeneous sample shapes in observation order');
    Check((LModel.ValueCount = 4) and
      (LModel.TokenAt(0) = 'A') and (LModel.TokenAt(1) = 'B') and
      (LModel.TokenAt(2) = 'C') and (LModel.TokenAt(3) = 'D') and
      (LModel.WeightAt(0) = 2) and (LModel.WeightAt(1) = 1) and
      (LModel.WeightAt(2) = 1) and (LModel.WeightAt(3) = 1),
      'corpus learning preserves global first-seen order and raw counts');

    LExact := True;
    for D := Low(TWfcModelDirection) to High(TWfcModelDirection) do
      for I := 0 to 3 do
        for J := 0 to 3 do
        begin
          LExpected := 0;
          if (D in [wmdEast, wmdWest]) and
            (((I = 0) and (J = 0)) or
             ((I = 0) and (J = 1)) or
             ((I = 1) and (J = 0)) or
             ((I = 2) and (J = 3)) or
             ((I = 3) and (J = 2))) then
            LExpected := 1;
          if LModel.RelationCount(D, I, J) <> LExpected then
            LExact := False;
        end;
    Check(LExact,
      'wrapped corpus learning observes each sample independently and exactly');

    LNoCrossSampleEdge := True;
    for D := Low(TWfcModelDirection) to High(TWfcModelDirection) do
      for I := 0 to 1 do
        for J := 2 to 3 do
          if (LModel.RelationCount(D, I, J) <> 0) or
            (LModel.RelationCount(D, J, I) <> 0) then
            LNoCrossSampleEdge := False;
    Check(LNoCrossSampleEdge and
      (LModel.RelationCount(wmdEast, 0, 0) = 1) and
      (LModel.RelationCount(wmdEast, 3, 2) = 1),
      'wrap closes each sample locally without a cross-sample seam');
  finally
    LModel.Free;
  end;

  LOpenModel := LearnModel1DCorpus(LSamples, wmbOpen);
  try
    Check((LOpenModel.RelationCount(wmdEast, 0, 0) = 0) and
      (LOpenModel.RelationCount(wmdEast, 3, 2) = 0) and
      (LOpenModel.RelationCount(wmdEast, 0, 2) = 0) and
      (LOpenModel.RelationCount(wmdWest, 2, 0) = 0),
      'open corpus learning adds neither local wraps nor corpus seams');
  finally
    LOpenModel.Free;
  end;

  SetLength(LSamples, 2);
  LSamples[0] := MakeLearnSample2D(Tokens('A', 'B'), 2, 1);
  LSamples[1] := MakeLearnSample2D(Tokens('C', 'D', 'E'), 1, 3);
  LModel := LearnModel2DCorpus(LSamples, wmbWrap, wmsNone);
  try
    LNoCrossSampleEdge :=
      (LModel.SampleShapeAt(0).Width = 2) and
      (LModel.SampleShapeAt(0).Height = 1) and
      (LModel.SampleShapeAt(1).Width = 1) and
      (LModel.SampleShapeAt(1).Height = 3);
    for D := Low(TWfcModelDirection) to High(TWfcModelDirection) do
      for I := 0 to 1 do
        for J := 2 to 4 do
          if (LModel.RelationCount(D, I, J) <> 0) or
            (LModel.RelationCount(D, J, I) <> 0) then
            LNoCrossSampleEdge := False;
    Check(LNoCrossSampleEdge,
      'heterogeneous 2D samples remain isolated in every wrapped direction');
  finally
    LModel.Free;
  end;
end;

procedure TestCorpusD4AndDuplicateSamples;
var
  D: TWfcModelDirection;
  I: Integer;
  J: Integer;
  LCorpusModel: TWfcModel;
  LExactCounts: Boolean;
  LExactWeights: Boolean;
  LFirst: TWfcModel;
  LMerged: TWfcModel;
  LModels: TWfcModels;
  LNoCrossSampleEdge: Boolean;
  LSamples: TWfcLearnSamples;
  LSecond: TWfcModel;
  LSingle: TWfcModel;
begin
  SetLength(LSamples, 2);
  LSamples[0] := MakeLearnSample2D(Tokens('Z', 'A', 'B', 'Y'), 2, 2);
  LSamples[1] := MakeLearnSample2D(Tokens('Q', 'R', 'P'), 3, 1);
  LCorpusModel := nil;
  LFirst := nil;
  LSecond := nil;
  LMerged := nil;
  try
    LCorpusModel := LearnModel2DCorpus(LSamples, wmbOpen, wmsD4);
    LFirst := LearnModel2D(LSamples[0].Tokens, 2, 2,
      wmbOpen, wmsD4);
    LSecond := LearnModel2D(LSamples[1].Tokens, 3, 1,
      wmbOpen, wmsD4);
    SetLength(LModels, 2);
    LModels[0] := LFirst;
    LModels[1] := LSecond;
    LMerged := MergeWfcModels(LModels);

    Check((LCorpusModel.SampleCount = 2) and
      (LCorpusModel.SampleShapeAt(0).Width = 2) and
      (LCorpusModel.SampleShapeAt(0).Height = 2) and
      (LCorpusModel.SampleShapeAt(1).Width = 3) and
      (LCorpusModel.SampleShapeAt(1).Height = 1) and
      (EncodeWfcModelText(LCorpusModel) = EncodeWfcModelText(LMerged)),
      'heterogeneous D4 corpus learning equals ordered per-sample merging');

    LExactWeights := LCorpusModel.ValueCount = 7;
    for I := 0 to LCorpusModel.ValueCount - 1 do
      if LCorpusModel.WeightAt(I) <> 8 then
        LExactWeights := False;
    Check(LExactWeights,
      'D4 observes every token eight times independently in each sample');

    LNoCrossSampleEdge := True;
    for D := Low(TWfcModelDirection) to High(TWfcModelDirection) do
      for I := 0 to 3 do
        for J := 4 to 6 do
          if (LCorpusModel.RelationCount(D, I, J) <> 0) or
            (LCorpusModel.RelationCount(D, J, I) <> 0) then
            LNoCrossSampleEdge := False;
    Check(LNoCrossSampleEdge,
      'D4 augmentation never creates a relation across sample domains');
  finally
    LMerged.Free;
    LSecond.Free;
    LFirst.Free;
    LCorpusModel.Free;
  end;

  SetLength(LSamples, 2);
  LSamples[0] := MakeLearnSample1D(Tokens('Z', 'A', 'Z'));
  LSamples[1] := MakeLearnSample1D(Tokens('Z', 'A', 'Z'));
  LCorpusModel := nil;
  LSingle := nil;
  try
    LCorpusModel := LearnModel1DCorpus(LSamples, wmbOpen);
    LSingle := LearnModel1D(LSamples[0].Tokens, wmbOpen);
    LExactCounts :=
      (LCorpusModel.ValueCount = LSingle.ValueCount) and
      (LCorpusModel.SampleCount = 2);
    for I := 0 to LSingle.ValueCount - 1 do
      if LCorpusModel.WeightAt(I) <> 2 * LSingle.WeightAt(I) then
        LExactCounts := False;
    for D := Low(TWfcModelDirection) to High(TWfcModelDirection) do
      for I := 0 to LSingle.ValueCount - 1 do
        for J := 0 to LSingle.ValueCount - 1 do
          if LCorpusModel.RelationCount(D, I, J) <>
            2 * LSingle.RelationCount(D, I, J) then
            LExactCounts := False;
    Check(LExactCounts,
      'duplicate samples retain two exact copies of every observed count');
    Check((LCorpusModel.SampleShapeAt(0).Width = 3) and
      (LCorpusModel.SampleShapeAt(1).Width = 3) and
      (LCorpusModel.RelationCount(wmdEast, 0, 0) = 0) and
      (LCorpusModel.RelationCount(wmdWest, 0, 0) = 0),
      'duplicate open samples retain both shapes without an invented seam');
  finally
    LSingle.Free;
    LCorpusModel.Free;
  end;
end;

procedure TestModelSampleShapes;
var
  LCopy: TWfcModelSampleShapes;
  LModel: TWfcModel;
  LRaised: Boolean;
  LSamples: TWfcLearnSamples;
  LShapes: TWfcModelSampleShapes;
begin
  SetLength(LSamples, 2);
  LSamples[0] := MakeLearnSample1D(Tokens('A', 'B', 'A'));
  LSamples[1] := MakeLearnSample1D(Tokens('C', 'D'));
  LModel := LearnModel1DCorpus(LSamples, wmbWrap);
  try
    LSamples[0].Width := 99;
    LSamples[1].Height := 99;
    Check((LModel.SampleShapeAt(0).Width = 3) and
      (LModel.SampleShapeAt(1).Height = 1),
      'model construction deep-copies corpus sample shapes');

    LCopy := LModel.CopySampleShapes;
    LCopy[0].Width := 77;
    LCopy[1].Height := 77;
    Check((LModel.SampleShapeAt(0).Width = 3) and
      (LModel.SampleShapeAt(1).Height = 1),
      'CopySampleShapes does not expose mutable model storage');
    Check((LModel.SampleWidth = 3) and (LModel.SampleHeight = 1),
      'legacy sample dimensions remain aliases for the first corpus shape');

    LRaised := False;
    try
      LModel.SampleShapeAt(-1);
    except
      on E: Exception do LRaised := True;
    end;
    Check(LRaised, 'SampleShapeAt rejects a negative sample index');
    LRaised := False;
    try
      LModel.SampleShapeAt(LModel.SampleCount);
    except
      on E: Exception do LRaised := True;
    end;
    Check(LRaised, 'SampleShapeAt rejects an index at SampleCount');
  finally
    LModel.Free;
  end;

  SetLength(LShapes, 0);
  Check(ShapeConstructorRejected(1, LShapes),
    'the model constructor rejects an empty sample-shape corpus');
  SetLength(LShapes, 2);
  LShapes[0] := MakeWfcModelSampleShape(3, 1);
  LShapes[1] := MakeWfcModelSampleShape(0, 1);
  Check(ShapeConstructorRejected(1, LShapes),
    'the model constructor validates every corpus shape dimension');
  LShapes[1] := MakeWfcModelSampleShape(2, 2);
  Check(ShapeConstructorRejected(1, LShapes),
    'the model constructor requires height one for every rank-1 shape');
end;

procedure TestModelMerging;
var
  LDirect: TWfcModel;
  LFirst: TWfcModel;
  LMerged: TWfcModel;
  LModels: TWfcModels;
  LSamples: TWfcLearnSamples;
  LSecond: TWfcModel;
begin
  Check(WFC_MODEL_MERGE_ALGORITHM_VERSION = 1,
    'the model merger publishes replay version 1');
  SetLength(LSamples, 2);
  LSamples[0] := MakeLearnSample1D(Tokens('A', 'B', 'A'));
  LSamples[1] := MakeLearnSample1D(Tokens('C', 'D'));
  LDirect := LearnModel1DCorpus(LSamples, wmbWrap);
  LFirst := LearnModel1D(LSamples[0].Tokens, wmbWrap);
  LSecond := LearnModel1D(LSamples[1].Tokens, wmbWrap);
  LMerged := nil;
  try
    SetLength(LModels, 2);
    LModels[0] := LFirst;
    LModels[1] := LSecond;
    LMerged := MergeWfcModels(LModels);
    Check(EncodeWfcModelText(LMerged) = EncodeWfcModelText(LDirect),
      'direct corpus learning and ordered model merging encode identically');

    LFirst.Free;
    LFirst := nil;
    LSecond.Free;
    LSecond := nil;
    Check(EncodeWfcModelText(LMerged) = GOLDEN_WRAP_CORPUS,
      'a merged model owns all data independently from its source models');
  finally
    LMerged.Free;
    LSecond.Free;
    LFirst.Free;
    LDirect.Free;
  end;

  SetLength(LModels, 0);
  Check(MergeRejected(LModels), 'model merging rejects an empty model list');

  LFirst := LearnModel1D(Tokens('A'), wmbWrap);
  try
    SetLength(LModels, 2);
    LModels[0] := LFirst;
    LModels[1] := nil;
    Check(MergeRejected(LModels),
      'model merging rejects an unassigned model entry');
  finally
    LFirst.Free;
  end;

  LFirst := LearnModel1D(Tokens('A'), wmbOpen);
  LSecond := LearnModel1D(Tokens('A'), wmbWrap);
  try
    LModels[0] := LFirst;
    LModels[1] := LSecond;
    Check(MergeRejected(LModels),
      'model merging rejects incompatible boundary policies');
  finally
    LSecond.Free;
    LFirst.Free;
  end;

  LFirst := LearnModel1D(Tokens('A'), wmbWrap);
  LSecond := LearnModel2D(Tokens('A'), 1, 1, wmbWrap, wmsNone);
  try
    LModels[0] := LFirst;
    LModels[1] := LSecond;
    Check(MergeRejected(LModels),
      'model merging rejects incompatible ranks and direction sets');
  finally
    LSecond.Free;
    LFirst.Free;
  end;

  LFirst := LearnModel2D(Tokens('A'), 1, 1, wmbWrap, wmsNone);
  LSecond := LearnModel2D(Tokens('A'), 1, 1, wmbWrap, wmsD4);
  try
    LModels[0] := LFirst;
    LModels[1] := LSecond;
    Check(MergeRejected(LModels),
      'model merging rejects incompatible symmetry policies');
  finally
    LSecond.Free;
    LFirst.Free;
  end;

  LFirst := MakeSingleTokenCountModel(High(Integer), 0);
  LSecond := MakeSingleTokenCountModel(1, 0);
  try
    LModels[0] := LFirst;
    LModels[1] := LSecond;
    Check(MergeRejected(LModels),
      'model merging rejects accumulated weight overflow');
  finally
    LSecond.Free;
    LFirst.Free;
  end;

  LFirst := MakeSingleTokenCountModel(1, High(Integer));
  LSecond := MakeSingleTokenCountModel(1, 1);
  try
    LModels[0] := LFirst;
    LModels[1] := LSecond;
    Check(MergeRejected(LModels),
      'model merging rejects accumulated relation-count overflow');
  finally
    LSecond.Free;
    LFirst.Free;
  end;
end;

procedure TestModelMergeAlgebraAndOwnership;
var
  LA: TWfcModel;
  LAB: TWfcModel;
  LB: TWfcModel;
  LBC: TWfcModel;
  LC: TWfcModel;
  LExpectedText: String;
  LFlat: TWfcModel;
  LFlatText: String;
  LLeft: TWfcModel;
  LModels: TWfcModels;
  LReverse: TWfcModel;
  LRight: TWfcModel;
  LSingleMerged: TWfcModel;
  LSource: TWfcModel;
begin
  LSource := nil;
  LSingleMerged := nil;
  try
    LSource := LearnModel1D(Tokens('Z', 'A', 'Z'), wmbOpen);
    LExpectedText := EncodeWfcModelText(LSource);
    SetLength(LModels, 1);
    LModels[0] := LSource;
    LSingleMerged := MergeWfcModels(LModels);
    Check((LSingleMerged <> LSource) and
      (EncodeWfcModelText(LSingleMerged) = LExpectedText),
      'a one-input merge returns an equivalent distinct model');

    LSource.Free;
    LSource := nil;
    LModels[0] := nil;
    Check(EncodeWfcModelText(LSingleMerged) = LExpectedText,
      'a one-input merged model remains valid after its source is freed');
  finally
    LSingleMerged.Free;
    LSource.Free;
  end;

  LA := nil;
  LAB := nil;
  LB := nil;
  LBC := nil;
  LC := nil;
  LFlat := nil;
  LLeft := nil;
  LReverse := nil;
  LRight := nil;
  try
    LA := LearnModel1D(Tokens('Z', 'A', 'Z', 'B'), wmbWrap);
    LB := LearnModel1D(Tokens('M', 'Z'), wmbWrap);
    LC := LearnModel1D(Tokens('A', 'Q', 'M'), wmbWrap);

    SetLength(LModels, 3);
    LModels[0] := LA;
    LModels[1] := LB;
    LModels[2] := LC;
    LFlat := MergeWfcModels(LModels);

    SetLength(LModels, 2);
    LModels[0] := LA;
    LModels[1] := LB;
    LAB := MergeWfcModels(LModels);
    LModels[0] := LAB;
    LModels[1] := LC;
    LLeft := MergeWfcModels(LModels);

    LModels[0] := LB;
    LModels[1] := LC;
    LBC := MergeWfcModels(LModels);
    LModels[0] := LA;
    LModels[1] := LBC;
    LRight := MergeWfcModels(LModels);

    SetLength(LModels, 3);
    LModels[0] := LC;
    LModels[1] := LB;
    LModels[2] := LA;
    LReverse := MergeWfcModels(LModels);
    LFlatText := EncodeWfcModelText(LFlat);

    LA.Free;
    LA := nil;
    LAB.Free;
    LAB := nil;
    LB.Free;
    LB := nil;
    LBC.Free;
    LBC := nil;
    LC.Free;
    LC := nil;
    SetLength(LModels, 0);

    Check((EncodeWfcModelText(LLeft) = LFlatText) and
      (EncodeWfcModelText(LRight) = LFlatText),
      'flat, left-grouped, and right-grouped merges encode identically');
    Check((LFlat.TokenAt(0) = 'Z') and (LFlat.TokenAt(1) = 'A') and
      (LFlat.TokenAt(2) = 'B') and (LFlat.TokenAt(3) = 'M') and
      (LFlat.TokenAt(4) = 'Q') and
      (LFlat.SampleShapeAt(0).Width = 4) and
      (LFlat.SampleShapeAt(1).Width = 2) and
      (LFlat.SampleShapeAt(2).Width = 3),
      'merge grouping preserves flattened first-seen token and shape order');
    Check((EncodeWfcModelText(LReverse) <> LFlatText) and
      (LReverse.TokenAt(0) = 'A') and (LReverse.TokenAt(1) = 'Q') and
      (LReverse.TokenAt(2) = 'M') and (LReverse.TokenAt(3) = 'Z') and
      (LReverse.TokenAt(4) = 'B') and
      (LReverse.SampleShapeAt(0).Width = 3) and
      (LReverse.SampleShapeAt(1).Width = 2) and
      (LReverse.SampleShapeAt(2).Width = 4),
      'reversing merge input preserves its distinct token and shape order');
  finally
    LRight.Free;
    LReverse.Free;
    LLeft.Free;
    LFlat.Free;
    LC.Free;
    LBC.Free;
    LB.Free;
    LAB.Free;
    LA.Free;
  end;
end;

procedure TestCorpusTextCodec;
var
  LDecoded: TWfcModel;
  LEncoded: String;
  LModel: TWfcModel;
  LSamples: TWfcLearnSamples;
begin
  SetLength(LSamples, 2);
  LSamples[0] := MakeLearnSample1D(Tokens('A', 'B', 'A'));
  LSamples[1] := MakeLearnSample1D(Tokens('C', 'D'));
  LModel := LearnModel1DCorpus(LSamples, wmbWrap);
  try
    LEncoded := EncodeWfcModelText(LModel);
    Check(LEncoded = GOLDEN_WRAP_CORPUS,
      'a two-sample corpus encodes to the exact canonical wfcm=2 document');
    LDecoded := DecodeWfcModelText(LEncoded);
    try
      Check((EncodeWfcModelText(LDecoded) = LEncoded) and
        (LDecoded.SampleCount = 2) and
        (LDecoded.SampleShapeAt(0).Width = 3) and
        (LDecoded.SampleShapeAt(1).Width = 2),
        'wfcm=2 round-trips every ordered sample shape byte-for-byte');
    finally
      LDecoded.Free;
    end;
  finally
    LModel.Free;
  end;

  CheckDecodeRejected(ReplaceOnce(GOLDEN_WRAP_CORPUS,
    'samples=2', 'samples=1'),
    'wfcm=2 rejects a singleton sample declaration');
  CheckDecodeRejected(ReplaceOnce(GOLDEN_WRAP_CORPUS,
    'samples=2', 'samples=3'),
    'wfcm=2 rejects an incomplete sample-shape record set');
  CheckDecodeRejected(ReplaceOnce(GOLDEN_WRAP_CORPUS,
    's=0,3,1', 's=1,3,1'),
    'wfcm=2 rejects a sample-shape index out of canonical order');
  CheckDecodeRejected(ReplaceOnce(GOLDEN_WRAP_CORPUS,
    's=0,3,1', 's=0,03,1'),
    'wfcm=2 rejects a noncanonical sample dimension');
  CheckDecodeRejected(ReplaceOnce(GOLDEN_WRAP_CORPUS,
    's=0,3,1', 's=0,0,1'),
    'wfcm=2 rejects a nonpositive sample dimension');
  CheckDecodeRejected(ReplaceOnce(GOLDEN_WRAP_CORPUS,
    's=0,3,1', 's=0,3'),
    'wfcm=2 rejects a sample-shape record with a missing field');
  CheckDecodeRejected(ReplaceOnce(GOLDEN_WRAP_CORPUS,
    's=0,3,1', 's=0,3,1,9'),
    'wfcm=2 rejects a sample-shape record with an extra field');
  CheckDecodeRejected(ReplaceOnce(GOLDEN_WRAP_CORPUS,
    's=0,3,1'#10's=1,2,1', 's=1,2,1'#10's=0,3,1'),
    'wfcm=2 rejects reordered sample-shape records');
  CheckDecodeRejected(ReplaceOnce(GOLDEN_WRAP_CORPUS,
    'samples=2', 'samples=02'),
    'wfcm=2 rejects a noncanonical sample count');
  CheckDecodeRejected(ReplaceOnce(GOLDEN_WRAP_CORPUS,
    'samples=2', 'samples=2147483648'),
    'wfcm=2 rejects a sample count outside the Integer range');
  CheckDecodeRejected(ReplaceOnce(GOLDEN_WRAP_CORPUS,
    's=1,2,1'#10'boundary=wrap',
    's=1,2,1'#10's=2,1,1'#10'boundary=wrap'),
    'wfcm=2 rejects an undeclared extra sample-shape record');
  CheckDecodeRejected(ReplaceOnce(GOLDEN_WRAP_CORPUS,
    's=1,2,1', 's=1,2,2'),
    'wfcm=2 enforces rank-1 height on every sample shape');
  CheckDecodeRejected(ReplaceOnce(GOLDEN_WRAP_CORPUS,
    'samples=2'#10's=0,3,1'#10's=1,2,1',
    'width=3'#10'height=1'),
    'wfcm=2 rejects legacy singular shape fields');
end;

procedure TestTextCodec;
var
  LDecoded: TWfcModel;
  LEncoded: String;
  LModel: TWfcModel;
  LNoteModel: TWfcModel;
  LPunctuationModel: TWfcModel;
begin
  Check(WFC_MODEL_TEXT_VERSION = 2,
    'the canonical model text format publishes version 2');
  LModel := LearnModel1D(Tokens('A', 'B', 'A'), wmbWrap);
  try
    LEncoded := EncodeWfcModelText(LModel);
    Check(LEncoded = GOLDEN_WRAP_ABA,
      'wrapped A-B-A encodes to the exact canonical golden document');
    LDecoded := DecodeWfcModelText(LEncoded);
    try
      Check(EncodeWfcModelText(LDecoded) = LEncoded,
        'canonical model text decodes and re-encodes byte-for-byte');
      Check((LDecoded.ValueCount = 2) and
        (LDecoded.WeightAt(0) = 2) and
        (LDecoded.RelationCount(wmdEast, 0, 0) = 1),
        'decoding reconstructs model frequencies and relations');
    finally
      LDecoded.Free;
    end;
  finally
    LModel.Free;
  end;

  LPunctuationModel := LearnModel1D(Tokens('a b', ',', '%'), wmbWrap);
  try
    LEncoded := EncodeWfcModelText(LPunctuationModel);
    Check((Pos('a%20b', LEncoded) > 0) and
      (Pos('%2C', LEncoded) > 0) and (Pos('%25', LEncoded) > 0),
      'tokens percent-encode space, comma, and percent canonically');
    CheckDecodeRejected(ReplaceOnce(LEncoded, '%2C', '%2c'),
      'the decoder rejects lowercase percent escapes');
  finally
    LPunctuationModel.Free;
  end;

  LNoteModel := LearnModel1D(Tokens(MusicalNoteToken), wmbWrap);
  try
    LEncoded := EncodeWfcModelText(LNoteModel);
    Check(Pos('%E2%99%AB', LEncoded) > 0,
      'the musical-note token uses canonical UTF-8 percent bytes');
    LDecoded := DecodeWfcModelText(LEncoded);
    try
      Check((LDecoded.TokenAt(0) = MusicalNoteToken) and
        (EncodeWfcModelText(LDecoded) = LEncoded),
        'Unicode tokens round-trip identically through the text codec');
    finally
      LDecoded.Free;
    end;
  finally
    LNoteModel.Free;
  end;

  CheckDecodeRejected(ReplaceOnce(GOLDEN_WRAP_ABA, #10, #13#10),
    'the decoder rejects CRLF input');
  CheckDecodeRejected(Copy(GOLDEN_WRAP_ABA, 1,
    Length(GOLDEN_WRAP_ABA) - 1),
    'the decoder requires a final LF');
  CheckDecodeRejected(ReplaceOnce(GOLDEN_WRAP_ABA,
    'rank=1'#10'width=3', 'width=3'#10'rank=1'),
    'the decoder rejects reordered required fields');
  CheckDecodeRejected(ReplaceOnce(GOLDEN_WRAP_ABA,
    'rank=1', 'rank=01'),
    'the decoder rejects leading-zero decimal fields');
  CheckDecodeRejected(ReplaceOnce(GOLDEN_WRAP_ABA,
    'v=0,2,A', 'v=0,2,%41'),
    'the decoder rejects unnecessary percent escapes');
  CheckDecodeRejected(ReplaceOnce(GOLDEN_WRAP_ABA,
    'v=0,2,A', 'v=0,2,%FF'),
    'the decoder rejects malformed UTF-8 tokens');
  CheckDecodeRejected(ReplaceOnce(GOLDEN_WRAP_ABA,
    'r=E,0,0,1'#10'r=E,0,1,1',
    'r=E,0,1,1'#10'r=E,0,0,1'),
    'the decoder rejects noncanonical relation ordering');
  CheckDecodeRejected(ReplaceOnce(GOLDEN_WRAP_ABA,
    'relations=6', 'relations=5'),
    'the decoder rejects a mismatched relation record count');
  CheckDecodeRejected(ReplaceOnce(GOLDEN_WRAP_ABA,
    'r=E,0,0,1', 'r=E,0,0,0'),
    'the decoder rejects a nonpositive relation count');
  CheckDecodeRejected(ReplaceOnce(GOLDEN_WRAP_ABA,
    'end'#10, 'end.'#10),
    'the decoder rejects a noncanonical end marker');
  CheckDecodeRejected(GOLDEN_WRAP_ABA + 'extra'#10,
    'the decoder rejects data after the end marker');
end;

begin
  WriteLn('WFC model-learning conformance suite');
  WriteLn('====================================');
  RunTest('1D bounded observations', @TestLearn1DOpen);
  RunTest('1D wrapped observations', @TestLearn1DWrap);
  RunTest('2D cardinal observations', @TestLearn2D);
  RunTest('explicit D4 observations', @TestD4Learning);
  RunTest('learning input guards', @TestInvalidLearningInputs);
  RunTest('immutable model and constructor guards',
    @TestModelImmutabilityAndGuards);
  RunTest('graph adapter boundaries and solve', @TestGraphAdapter);
  RunTest('asymmetric graph adapter orientation',
    @TestAsymmetricGraphAdapter);
  RunTest('multi-sample corpus observations', @TestCorpusLearning);
  RunTest('corpus D4 and duplicate-sample invariants',
    @TestCorpusD4AndDuplicateSamples);
  RunTest('model sample-shape immutability and guards',
    @TestModelSampleShapes);
  RunTest('deterministic model merging and guards', @TestModelMerging);
  RunTest('model merge algebra and ownership',
    @TestModelMergeAlgebraAndOwnership);
  RunTest('canonical corpus text codec', @TestCorpusTextCodec);
  RunTest('canonical model text codec', @TestTextCodec);
  WriteLn('====================================');
  WriteLn(Format('%d checks, %d failures',
    [GCheckCount, GFailureCount]));

  if GFailureCount > 0 then
  {$IFDEF PAS2JS}
    raise Exception.CreateFmt('%d model-learning checks failed',
      [GFailureCount]);
  {$ELSE}
    Halt(1);
  {$ENDIF}
end.
