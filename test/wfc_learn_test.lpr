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
  LRaised: Boolean;
begin
  LModel := nil;
  LRaised := False;
  try
    try
      LModel := DecodeWfcModelText(AText);
    except
      on E: Exception do
        LRaised := True;
    end;
  finally
    LModel.Free;
  end;
  Check(LRaised, AMessage);
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

procedure TestTextCodec;
var
  LDecoded: TWfcModel;
  LEncoded: String;
  LModel: TWfcModel;
  LNoteModel: TWfcModel;
  LPunctuationModel: TWfcModel;
begin
  Check(WFC_MODEL_TEXT_VERSION = 1,
    'the canonical model text format publishes version 1');
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
