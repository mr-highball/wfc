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
program wfc_pattern2d_test;

{$mode delphi}{$H+}

uses
  SysUtils,
  wfc,
  wfc_model,
  wfc_learn,
  wfc_pattern2d,
  wfc_pattern2d_learn,
  wfc_pattern2d_text;

type
  TTestProcedure = procedure;

const
  GOLDEN_PUNCTUATION_UNICODE =
    'wfcp=1'#10 +
    'rank=2'#10 +
    'samples=1'#10 +
    's=0,2,1'#10 +
    'footprint=2,1'#10 +
    'boundary=wrap'#10 +
    'symmetry=none'#10 +
    'directions=N,E,S,W'#10 +
    'palette=2'#10 +
    't=0,%2C'#10 +
    't=1,%E2%99%AB'#10 +
    'patterns=2'#10 +
    'p=0,1,0,1'#10 +
    'p=1,1,1,0'#10 +
    'relations=12'#10 +
    'r=N,0,0'#10 +
    'r=N,0,1'#10 +
    'r=N,1,0'#10 +
    'r=N,1,1'#10 +
    'r=E,0,1'#10 +
    'r=E,1,0'#10 +
    'r=S,0,0'#10 +
    'r=S,0,1'#10 +
    'r=S,1,0'#10 +
    'r=S,1,1'#10 +
    'r=W,0,1'#10 +
    'r=W,1,0'#10 +
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

function TokensOf(const AValues: array of TWfcModelToken):
  TWfcModelTokens;
var
  I: Integer;
begin
  Result := nil;
  SetLength(Result, Length(AValues));
  for I := 0 to Length(AValues) - 1 do
    Result[I] := AValues[I];
end;

function PayloadOf(const AValues: array of Integer):
  TWfcPattern2DPayload;
var
  I: Integer;
begin
  Result := nil;
  SetLength(Result, Length(AValues));
  for I := 0 to Length(AValues) - 1 do
    Result[I] := AValues[I];
end;

function PatternIndicesOf(const AValues: array of Integer):
  TWfcPatternIndices;
var
  I: Integer;
begin
  Result := nil;
  SetLength(Result, Length(AValues));
  for I := 0 to Length(AValues) - 1 do
    Result[I] := AValues[I];
end;

function IntegersOf(const AValues: array of Integer):
  TWfcModelIntegerArray;
var
  I: Integer;
begin
  Result := nil;
  SetLength(Result, Length(AValues));
  for I := 0 to Length(AValues) - 1 do
    Result[I] := AValues[I];
end;

function PayloadMatches(const AModel: TWfcOverlappingModel2D;
  const APattern: Integer; const AExpected: array of Integer): Boolean;
var
  X: Integer;
  Y: Integer;
begin
  if Length(AExpected) <> AModel.PatternWidth * AModel.PatternHeight then
    Exit(False);
  for Y := 0 to AModel.PatternHeight - 1 do
    for X := 0 to AModel.PatternWidth - 1 do
      if AModel.PatternPaletteIndexAt(APattern, X, Y) <>
          AExpected[Y * AModel.PatternWidth + X] then
        Exit(False);
  Result := True;
end;

function TokensMatch(const AActual: TWfcModelTokens;
  const AExpected: array of TWfcModelToken): Boolean;
var
  I: Integer;
begin
  if Length(AActual) <> Length(AExpected) then
    Exit(False);
  for I := 0 to Length(AExpected) - 1 do
    if AActual[I] <> AExpected[I] then
      Exit(False);
  Result := True;
end;

function IndependentCompatible(const AModel: TWfcOverlappingModel2D;
  const ASource, ATarget: Integer;
  const ADirection: TWfcModelDirection): Boolean;
var
  X: Integer;
  Y: Integer;
begin
  case ADirection of
    wmdNorth:
      for Y := 0 to AModel.PatternHeight - 2 do
        for X := 0 to AModel.PatternWidth - 1 do
          if AModel.PatternPaletteIndexAt(ASource, X, Y) <>
              AModel.PatternPaletteIndexAt(ATarget, X, Y + 1) then
            Exit(False);
    wmdEast:
      for Y := 0 to AModel.PatternHeight - 1 do
        for X := 1 to AModel.PatternWidth - 1 do
          if AModel.PatternPaletteIndexAt(ASource, X, Y) <>
              AModel.PatternPaletteIndexAt(ATarget, X - 1, Y) then
            Exit(False);
    wmdSouth:
      for Y := 1 to AModel.PatternHeight - 1 do
        for X := 0 to AModel.PatternWidth - 1 do
          if AModel.PatternPaletteIndexAt(ASource, X, Y) <>
              AModel.PatternPaletteIndexAt(ATarget, X, Y - 1) then
            Exit(False);
    wmdWest:
      for Y := 0 to AModel.PatternHeight - 1 do
        for X := 0 to AModel.PatternWidth - 2 do
          if AModel.PatternPaletteIndexAt(ASource, X, Y) <>
              AModel.PatternPaletteIndexAt(ATarget, X + 1, Y) then
            Exit(False);
  end;
  Result := True;
end;

function ModelTokenAsGraphValue(const AToken: TWfcModelToken):
  TGraphValue;
begin
  {$IFDEF PAS2JS}
  Result := TGraphValue(AToken);
  {$ELSE}
  Result := TGraphValue(UTF8Decode(AToken));
  {$ENDIF}
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

procedure CheckDecodeRejected(const AText, AMessage: String);
var
  LDecoded: TWfcOverlappingModel2D;
  LRaised: Boolean;
begin
  LDecoded := nil;
  LRaised := False;
  try
    try
      LDecoded := DecodeWfcPattern2DText(AText);
    except
      on E: EConvertError do LRaised := True;
    end;
  finally
    LDecoded.Free;
  end;
  Check(LRaised, AMessage);
end;

function PatternModelLimitRejected(const APatternWidth,
  APatternHeight: Integer; const AShapes: TWfcModelSampleShapes;
  const APalette: TWfcModelTokens;
  const APatterns: TWfcPattern2DPayloads;
  const AWeights: TWfcModelIntegerArray;
  const AExpectedMessage: String): Boolean;
var
  LMessage: String;
  LModel: TWfcOverlappingModel2D;
begin
  Result := False;
  LMessage := '';
  LModel := nil;
  try
    try
      LModel := TWfcOverlappingModel2D.Create(APatternWidth,
        APatternHeight, wmbWrap, wmsNone, AShapes, APalette,
        APatterns, AWeights);
    except
      on E: EWfcModel do
      begin
        Result := True;
        LMessage := E.Message;
      end;
    end;
  finally
    LModel.Free;
  end;
  Result := Result and (Pos(AExpectedMessage, LMessage) > 0);
end;

function PatternLearningLimitRejected(const ASamples: TWfcLearnSamples;
  const APatternWidth, APatternHeight: Integer;
  const AExpectedMessage: String): Boolean;
var
  LMessage: String;
  LModel: TWfcOverlappingModel2D;
begin
  Result := False;
  LMessage := '';
  LModel := nil;
  try
    try
      LModel := LearnOverlappingModel2DCorpus(ASamples,
        APatternWidth, APatternHeight, wmbWrap, wmsNone);
    except
      on E: EWfcModel do
      begin
        Result := True;
        LMessage := E.Message;
      end;
    end;
  finally
    LModel.Free;
  end;
  Result := Result and (Pos(AExpectedMessage, LMessage) > 0);
end;

function OpenNineModel: TWfcOverlappingModel2D;
begin
  Result := LearnOverlappingModel2D(TokensOf([
    'A', 'B', 'C',
    'D', 'E', 'F',
    'G', 'H', 'I']), 3, 3, 2, 2, wmbOpen, wmsNone);
end;

function WrappedFourModel: TWfcOverlappingModel2D;
begin
  Result := LearnOverlappingModel2D(TokensOf([
    'A', 'B',
    'C', 'D']), 2, 2, 2, 2, wmbWrap, wmsNone);
end;

procedure TestDeterministicLearning;
var
  LModel: TWfcOverlappingModel2D;
begin
  LModel := LearnOverlappingModel2D(TokensOf([
    'A', 'B', 'A', 'B',
    'C', 'D', 'C', 'D']), 4, 2, 2, 2, wmbOpen, wmsNone);
  try
    Check((LModel.PatternWidth = 2) and (LModel.PatternHeight = 2)
      and (LModel.SourceBoundary = wmbOpen)
      and (LModel.Symmetry = wmsNone),
      'learning retains the exact overlapping-model metadata');
    Check((LModel.SourceCount = 1)
      and (LModel.SourceShapeAt(0).Width = 4)
      and (LModel.SourceShapeAt(0).Height = 2),
      'learning retains the ordered source shape');
    Check((LModel.PaletteCount = 4)
      and (LModel.PaletteTokenAt(0) = 'A')
      and (LModel.PaletteTokenAt(1) = 'B')
      and (LModel.PaletteTokenAt(2) = 'C')
      and (LModel.PaletteTokenAt(3) = 'D'),
      'palette indices follow deterministic first-observation order');
    Check((LModel.PatternCount = 2)
      and PayloadMatches(LModel, 0, [0, 1, 2, 3])
      and PayloadMatches(LModel, 1, [1, 0, 3, 2]),
      'patterns follow deterministic transformed-origin order');
    Check((LModel.PatternWeightAt(0) = 2)
      and (LModel.PatternWeightAt(1) = 1),
      'pattern weights are unnormalized observation frequencies');
  finally
    LModel.Free;
  end;
end;

procedure TestStructuralCompatibility;
var
  D: TWfcModelDirection;
  I: Integer;
  J: Integer;
  LExpected: Boolean;
  LExact: Boolean;
  LModel: TWfcOverlappingModel2D;
begin
  LModel := OpenNineModel;
  try
    LExact := True;
    for D := Low(TWfcModelDirection) to High(TWfcModelDirection) do
      for I := 0 to LModel.PatternCount - 1 do
        for J := 0 to LModel.PatternCount - 1 do
        begin
          LExpected := IndependentCompatible(LModel, I, J, D);
          if LModel.PatternsCompatible(I, J, D) <> LExpected then
            LExact := False;
          if (LModel.CompiledModel.RelationCount(D, I, J) <> 0) <>
              LExpected then
            LExact := False;
          if LModel.CompiledModel.RelationCount(D, I, J) > 1 then
            LExact := False;
        end;
    Check(LExact,
      'compiled N/E/S/W relations exactly equal independent footprint overlap');
    Check(LModel.PatternsCompatible(0, 1, wmdEast)
      and LModel.PatternsCompatible(1, 0, wmdWest)
      and LModel.PatternsCompatible(0, 2, wmdSouth)
      and LModel.PatternsCompatible(2, 0, wmdNorth),
      'known horizontal and vertical neighbors use reciprocal directions');
    Check((not LModel.PatternsCompatible(0, 0, wmdEast))
      and (not LModel.PatternsCompatible(0, 3, wmdSouth)),
      'nonmatching overlaps are absent even when both patterns were observed');
  finally
    LModel.Free;
  end;
end;

procedure TestCorpusIsolationAndGeneralization;
var
  LModel: TWfcOverlappingModel2D;
  LSamples: TWfcLearnSamples;
begin
  SetLength(LSamples, 2);
  LSamples[0] := MakeLearnSample2D(TokensOf([
    'A', 'B',
    'C', 'D']), 2, 2);
  LSamples[1] := MakeLearnSample2D(TokensOf([
    'B', 'E',
    'D', 'F']), 2, 2);
  LModel := LearnOverlappingModel2DCorpus(LSamples,
    2, 2, wmbOpen, wmsNone);
  try
    Check((LModel.SourceCount = 2)
      and (LModel.SourceShapeAt(0).Width = 2)
      and (LModel.SourceShapeAt(1).Width = 2),
      'a corpus retains separate ordered sample shapes');
    Check((LModel.PatternCount = 2)
      and PayloadMatches(LModel, 0, [0, 1, 2, 3])
      and PayloadMatches(LModel, 1, [1, 4, 3, 5]),
      'extraction never creates a pattern across a sample boundary');
    Check((LModel.PaletteCount = 6)
      and (LModel.PaletteTokenAt(4) = 'E')
      and (LModel.PaletteTokenAt(5) = 'F'),
      'corpus palette order is sample-major and cell-major');
    Check(LModel.PatternsCompatible(0, 1, wmdEast)
      and LModel.PatternsCompatible(1, 0, wmdWest),
      'separately observed patterns may generalize when overlaps truly match');
    Check(not LModel.PatternsCompatible(1, 0, wmdEast),
      'structural generalization does not invent a mismatching reverse edge');
  finally
    LModel.Free;
  end;
end;

procedure TestOpenAndWrappedExtraction;
var
  LModel: TWfcOverlappingModel2D;
  LRaised: Boolean;
begin
  LModel := LearnOverlappingModel2D(TokensOf(['A', 'B']),
    2, 1, 3, 2, wmbWrap, wmsNone);
  try
    Check((LModel.PatternCount = 2)
      and PayloadMatches(LModel, 0, [0, 1, 0, 0, 1, 0])
      and PayloadMatches(LModel, 1, [1, 0, 1, 1, 0, 1]),
      'wrapped extraction supports footprints larger than the source');
    Check((LModel.PatternWeightAt(0) = 1)
      and (LModel.PatternWeightAt(1) = 1),
      'wrapped origins are counted once per source cell');
  finally
    LModel.Free;
  end;

  LRaised := False;
  try
    LModel := LearnOverlappingModel2D(TokensOf(['A', 'B']),
      2, 1, 3, 2, wmbOpen, wmsNone);
    LModel.Free;
  except
    on E: EWfcOverlapping2D do LRaised := True;
  end;
  Check(LRaised,
    'open extraction rejects a footprint larger than its source');
end;

procedure TestD4Closure;
const
  EXPECTED: array[0..7, 0..3] of Integer = (
    (0, 1, 2, 3),
    (2, 0, 3, 1),
    (3, 2, 1, 0),
    (1, 3, 0, 2),
    (1, 0, 3, 2),
    (3, 1, 2, 0),
    (2, 3, 0, 1),
    (0, 2, 1, 3));
var
  I: Integer;
  J: Integer;
  LExact: Boolean;
  LModel: TWfcOverlappingModel2D;
  LSymmetric: TWfcOverlappingModel2D;
begin
  LModel := LearnOverlappingModel2D(TokensOf([
    'A', 'B',
    'C', 'D']), 2, 2, 2, 2, wmbOpen, wmsD4);
  try
    LExact := LModel.PatternCount = 8;
    for I := 0 to LModel.PatternCount - 1 do
    begin
      if LModel.PatternWeightAt(I) <> 1 then
        LExact := False;
      for J := 0 to 3 do
        if LModel.PatternPaletteIndexAt(I, J mod 2, J div 2) <>
            EXPECTED[I, J] then
          LExact := False;
    end;
    Check(LExact,
      'D4 extraction has a deterministic complete transform orbit');
    Check((LModel.Symmetry = wmsD4)
      and (LModel.CopyPatternWeights[0] = 1),
      'D4 orbit members retain equal raw weights');
  finally
    LModel.Free;
  end;

  LSymmetric := LearnOverlappingModel2D(TokensOf([
    'A', 'A',
    'A', 'A']), 2, 2, 2, 2, wmbOpen, wmsD4);
  try
    Check((LSymmetric.PatternCount = 1)
      and (LSymmetric.PatternWeightAt(0) = 8),
      'D4 stabilizers accumulate all eight transformed observations');
  finally
    LSymmetric.Free;
  end;
end;

procedure TestImmutabilityAndGuards;
var
  LCopyPalette: TWfcModelTokens;
  LCopyPattern: TWfcPattern2DPayload;
  LCopyPatterns: TWfcPattern2DPayloads;
  LCopyShapes: TWfcModelSampleShapes;
  LCopyWeights: TWfcModelIntegerArray;
  LGrid: TWfcPatternGrid2D;
  LGridValues: TWfcPatternIndices;
  LModel: TWfcOverlappingModel2D;
  LPalette: TWfcModelTokens;
  LPatterns: TWfcPattern2DPayloads;
  LRaised: Boolean;
  LShapes: TWfcModelSampleShapes;
  LWeights: TWfcModelIntegerArray;
begin
  SetLength(LShapes, 1);
  LShapes[0] := MakeWfcModelSampleShape(2, 2);
  LPalette := TokensOf(['A', 'B', 'C', 'D']);
  SetLength(LPatterns, 1);
  LPatterns[0] := PayloadOf([0, 1, 2, 3]);
  LWeights := IntegersOf([1]);
  LModel := TWfcOverlappingModel2D.Create(2, 2, wmbOpen, wmsNone,
    LShapes, LPalette, LPatterns, LWeights);
  try
    LShapes[0].Width := 99;
    LPalette[0] := 'changed';
    LPatterns[0][0] := 3;
    LWeights[0] := 99;
    Check((LModel.SourceShapeAt(0).Width = 2)
      and (LModel.PaletteTokenAt(0) = 'A')
      and (LModel.PatternPaletteIndexAt(0, 0, 0) = 0)
      and (LModel.PatternWeightAt(0) = 1),
      'the constructor deep-copies every caller-owned array');

    LCopyShapes := LModel.CopySourceShapes;
    LCopyPalette := LModel.CopyPalette;
    LCopyPattern := LModel.CopyPattern(0);
    LCopyPatterns := LModel.CopyPatterns;
    LCopyWeights := LModel.CopyPatternWeights;
    LCopyShapes[0].Width := 88;
    LCopyPalette[0] := 'copy';
    LCopyPattern[0] := 2;
    LCopyPatterns[0][0] := 1;
    LCopyWeights[0] := 77;
    Check((LModel.SourceShapeAt(0).Width = 2)
      and (LModel.PaletteTokenAt(0) = 'A')
      and (LModel.PatternPaletteIndexAt(0, 0, 0) = 0)
      and (LModel.PatternWeightAt(0) = 1),
      'all overlapping-model copy accessors return detached storage');

    LRaised := False;
    try
      LModel.PatternPaletteIndexAt(0, 2, 0);
    except
      on E: ERangeError do LRaised := True;
    end;
    Check(LRaised, 'pattern coordinate access is range checked');
    LRaised := False;
    try
      LModel.PatternWeightAt(1);
    except
      on E: ERangeError do LRaised := True;
    end;
    Check(LRaised, 'pattern index access is range checked');
  finally
    LModel.Free;
  end;

  LGridValues := PatternIndicesOf([3, 4]);
  LGrid := MakeWfcPatternGrid2D(2, 1, wmbOpen, LGridValues);
  LGridValues[0] := 99;
  Check((LGrid.Patterns[0] = 3) and (LGrid.Patterns[1] = 4),
    'pattern-grid construction copies its assignment array');
  LRaised := False;
  try
    LGrid := MakeWfcPatternGrid2D(2, 2, wmbOpen,
      PatternIndicesOf([0]));
  except
    on E: EWfcOverlapping2D do LRaised := True;
  end;
  Check(LRaised, 'pattern-grid construction rejects a mismatched cell count');

  SetLength(LShapes, 1);
  LShapes[0] := MakeWfcModelSampleShape(2, 2);
  LPalette := TokensOf(['A', 'A']);
  SetLength(LPatterns, 1);
  LPatterns[0] := PayloadOf([0, 0, 0, 0]);
  LWeights := IntegersOf([1]);
  LRaised := False;
  try
    LModel := TWfcOverlappingModel2D.Create(2, 2, wmbOpen, wmsNone,
      LShapes, LPalette, LPatterns, LWeights);
    LModel.Free;
  except
    on E: EWfcOverlapping2D do LRaised := True;
  end;
  Check(LRaised, 'the constructor rejects duplicate palette tokens');

  LPalette := TokensOf(['A', 'B', 'C', 'D']);
  LPatterns[0] := PayloadOf([0, 1, 2, 3]);
  LWeights[0] := 8;
  LRaised := False;
  try
    LModel := TWfcOverlappingModel2D.Create(2, 2, wmbOpen, wmsD4,
      LShapes, LPalette, LPatterns, LWeights);
    LModel.Free;
  except
    on E: EWfcOverlapping2D do LRaised := True;
  end;
  Check(LRaised, 'the constructor rejects an incomplete D4 orbit');
end;

procedure TestPrivateStableKeys;
var
  I: Integer;
  LExact: Boolean;
  LModel: TWfcOverlappingModel2D;
begin
  LModel := OpenNineModel;
  try
    LExact := True;
    for I := 0 to LModel.PatternCount - 1 do
      if (LModel.PatternKeyAt(I) <> TWfcModelToken('@p' + IntToStr(I)))
          or (LModel.FindPatternKey(LModel.PatternKeyAt(I)) <> I)
          or (LModel.CompiledModel.TokenAt(I) <> LModel.PatternKeyAt(I)) then
        LExact := False;
    Check(LExact,
      'private pattern-domain keys are stable ASCII indices');
    Check((LModel.FindPatternKey('@p00') = -1)
      and (LModel.FindPatternKey('@p4') = -1)
      and (LModel.FindPatternKey('p0') = -1)
      and (LModel.FindPatternKey('@p-1') = -1),
      'private key parsing rejects aliases and out-of-range values');
  finally
    LModel.Free;
  end;
end;

procedure TestAssignmentValidationAndProjection;
var
  LGrid: TWfcPatternGrid2D;
  LModel: TWfcOverlappingModel2D;
  LOutput: TWfcTokenGrid2D;
  LReport: TWfcOverlapping2DValidationReport;
begin
  LModel := OpenNineModel;
  try
    LGrid := MakeWfcPatternGrid2D(2, 2, wmbOpen,
      PatternIndicesOf([0, 1, 2, 3]));
    Check(ValidateOverlappingPatternGrid2D(LModel, LGrid, LReport)
      and LReport.Valid and (LReport.CheckedPatterns = 4)
      and (LReport.CheckedRelations = 4)
      and (LReport.Issue.Kind = woikNone),
      'independent assignment validation accepts the exact source tiling');
    Check(TryProjectOverlappingPatternGrid2D(LModel, LGrid,
      LOutput, LReport)
      and (LOutput.Width = 3) and (LOutput.Height = 3)
      and TokensMatch(LOutput.Tokens, [
        'A', 'B', 'C',
        'D', 'E', 'F',
        'G', 'H', 'I'])
      and (LReport.CheckedProjectionCells = 16),
      'open projection reconstructs the complete bounded token grid');
    Check(ValidateOverlappingProjection2D(LModel, LGrid,
      LOutput, LReport) and (LReport.CheckedProjectionCells = 16),
      'the projection validator independently checks every pattern write');

    LGrid.Patterns[1] := 0;
    Check((not ValidateOverlappingPatternGrid2D(LModel, LGrid, LReport))
      and (LReport.Issue.Kind = woikOverlap)
      and (LReport.Issue.X = 0) and (LReport.Issue.Y = 0)
      and (LReport.Issue.NeighborX = 1)
      and LReport.Issue.HasDirection
      and (LReport.Issue.Direction = wmdEast)
      and (LReport.Issue.PatternIndex = 0)
      and (LReport.Issue.RelatedPatternIndex = 0),
      'overlap tampering returns the exact first failing edge');
    LGrid.Patterns[1] := LModel.PatternCount;
    Check((not ValidateOverlappingPatternGrid2D(LModel, LGrid, LReport))
      and (LReport.Issue.Kind = woikPatternIndex)
      and (LReport.Issue.X = 1) and (LReport.Issue.Y = 0)
      and (LReport.Issue.PatternIndex = LModel.PatternCount),
      'pattern-index tampering returns its exact cell and value');
  finally
    LModel.Free;
  end;
end;

procedure TestWrappedProjectionAndTamperReports;
var
  LGrid: TWfcPatternGrid2D;
  LModel: TWfcOverlappingModel2D;
  LOutput: TWfcTokenGrid2D;
  LReport: TWfcOverlapping2DValidationReport;
begin
  LModel := WrappedFourModel;
  try
    LGrid := MakeWfcPatternGrid2D(2, 2, wmbWrap,
      PatternIndicesOf([0, 1, 2, 3]));
    Check(TryProjectOverlappingPatternGrid2D(LModel, LGrid,
      LOutput, LReport)
      and (LOutput.Width = 2) and (LOutput.Height = 2)
      and TokensMatch(LOutput.Tokens, ['A', 'B', 'C', 'D'])
      and (LReport.CheckedProjectionCells = 16),
      'wrapped projection folds every footprint write onto the output torus');

    LOutput.Tokens[0] := 'B';
    Check((not ValidateOverlappingProjection2D(LModel, LGrid,
      LOutput, LReport))
      and (LReport.Issue.Kind = woikProjectionToken)
      and (LReport.Issue.X = 0) and (LReport.Issue.Y = 0)
      and (LReport.Issue.PatternOffsetX = 0)
      and (LReport.Issue.PatternOffsetY = 0)
      and (LReport.Issue.ExpectedPaletteIndex = 0)
      and (LReport.Issue.ActualPaletteIndex = 1),
      'projection tampering reports the anchor, offset, and palette values');
    LOutput.Width := 3;
    Check((not ValidateOverlappingProjection2D(LModel, LGrid,
      LOutput, LReport))
      and (LReport.Issue.Kind = woikProjectionShape),
      'projection shape tampering is distinguished from token tampering');
    Check(Pos('invalid shape', DescribeOverlapping2DIssue(
      LReport.Issue)) > 0,
      'tamper reports have a stable human-readable description');
  finally
    LModel.Free;
  end;
end;

procedure TestGraphApplySolveAndCapture;
var
  LGraph: TGraph;
  LGrid: TWfcPatternGrid2D;
  LModel: TWfcOverlappingModel2D;
  LOptions: TGraphSolveOptions;
  LReport: TWfcOverlapping2DValidationReport;
  LSolveReport: TGraphSolveReport;
  LValid: Boolean;
begin
  LModel := WrappedFourModel;
  LGraph := TGraph.Create;
  try
    LGraph.Seed := 24680;
    LGraph.Reshape(4, 4, 1);
    LGraph.WrapNeighbors := True;
    ApplyOverlappingModel2DToGraph(LModel, LGraph);
    Check((LGraph.RuleGroups.Count = LModel.PatternCount)
      and (LGraph.Rules[ModelTokenAsGraphValue(
        LModel.PatternKeyAt(0))].Weight = LModel.PatternWeightAt(0)),
      'the adapter registers the latent patterns and raw weights');
    LOptions := DefaultGraphSolveOptions;
    LOptions.MaxBacktracks := 4096;
    Check(LGraph.TrySolve(LOptions, LSolveReport),
      'a periodic overlapping-pattern graph solves through the public API');
    LValid := CaptureSolvedPatternGrid2D(LModel, LGraph, 0,
      LGrid, LReport);
    Check(LValid and LReport.Valid
      and (LGrid.Width = 4) and (LGrid.Height = 4)
      and (LGrid.Boundary = wmbWrap)
      and (LReport.CheckedPatterns = 16)
      and ValidateOverlappingPatternGrid2D(LModel, LGrid, LReport),
      'capture maps solved private keys back to a valid pattern assignment');
  finally
    LGraph.Free;
    LModel.Free;
  end;

  LModel := WrappedFourModel;
  LGraph := TGraph.Create;
  try
    LGraph.Reshape(1, 1, 1);
    LGraph.WrapNeighbors := False;
    Check((not CaptureSolvedPatternGrid2D(LModel, LGraph, 0,
      LGrid, LReport))
      and (LReport.Issue.Kind = woikEmptyGraphCell)
      and (LReport.Issue.X = 0) and (LReport.Issue.Y = 0),
      'capture reports an empty graph cell without inventing a pattern');
    LGraph.Entry[0, 0, 0].Value := 'outside';
    Check((not CaptureSolvedPatternGrid2D(LModel, LGraph, 0,
      LGrid, LReport))
      and (LReport.Issue.Kind = woikUnknownPatternKey)
      and (LReport.Issue.X = 0) and (LReport.Issue.Y = 0),
      'capture reports a value outside the private pattern domain');
  finally
    LGraph.Free;
    LModel.Free;
  end;
end;

procedure TestAdapterOpenBoundarySupport;
var
  LBoundaryGraph: TGraph;
  LGraph: TGraph;
  LGrid: TWfcPatternGrid2D;
  LModel: TWfcOverlappingModel2D;
  LOptions: TGraphSolveOptions;
  LOutput: TWfcTokenGrid2D;
  LReport: TWfcOverlapping2DValidationReport;
  LSolveReport: TGraphSolveReport;
begin
  Check((WFC_MODEL_GRAPH_ADAPTER_VERSION = 1) and
    (WFC_GRAPH_MODEL_VERSION = 1),
    'the pattern adapter uses version-1 finite graph support');
  LModel := OpenNineModel;
  LGraph := TGraph.Create;
  try
    LGraph.Seed := 0;
    LGraph.Reshape(2, 2, 1);
    LGraph.WrapNeighbors := False;
    ApplyOverlappingModel2DToGraph(LModel, LGraph);
    Check((LGraph.RuleGroups.Count = LModel.PatternCount)
      and (LGraph.Rules[ModelTokenAsGraphValue(
        LModel.PatternKeyAt(0))].DeniedDirections = [gdNorth, gdEast])
      and (LGraph.Rules[ModelTokenAsGraphValue(
        LModel.PatternKeyAt(3))].DeniedDirections = [gdSouth, gdWest])
      and LGraph.Rules[ModelTokenAsGraphValue(
        LModel.PatternKeyAt(0))].Denied[gdNorth]
      and LGraph.Rules[ModelTokenAsGraphValue(
        LModel.PatternKeyAt(0))].Denied[gdEast]
      and LGraph.Rules[ModelTokenAsGraphValue(
        LModel.PatternKeyAt(3))].Denied[gdSouth]
      and LGraph.Rules[ModelTokenAsGraphValue(
        LModel.PatternKeyAt(3))].Denied[gdWest],
      'open pattern endpoints retain every structurally empty support row');
    Check((not LGraph.Rules[ModelTokenAsGraphValue(
        LModel.PatternKeyAt(0))].Denied[gdUp])
      and (not LGraph.Rules[ModelTokenAsGraphValue(
        LModel.PatternKeyAt(0))].Denied[gdDown]),
      'directions outside the rank-2 model remain graph wildcards');
    LOptions := DefaultGraphSolveOptions;
    LOptions.MaxBacktracks := 4096;
    Check(LGraph.TrySolve(LOptions, LSolveReport),
      'the finite open pattern model solves on its bounded support');
    Check(CaptureSolvedPatternGrid2D(LModel, LGraph, 0,
      LGrid, LReport)
      and ValidateOverlappingPatternGrid2D(LModel, LGrid, LReport)
      and TryProjectOverlappingPatternGrid2D(LModel, LGrid,
        LOutput, LReport)
      and (LOutput.Width = 3) and (LOutput.Height = 3)
      and TokensMatch(LOutput.Tokens, [
        'A', 'B', 'C',
        'D', 'E', 'F',
        'G', 'H', 'I']),
      'the bounded solve projects back to the exact open source sample');
  finally
    LGraph.Free;
  end;

  LBoundaryGraph := TGraph.Create;
  try
    LBoundaryGraph.Seed := 0;
    LBoundaryGraph.Reshape(1, 1, 1);
    LBoundaryGraph.WrapNeighbors := False;
    ApplyOverlappingModel2DToGraph(LModel, LBoundaryGraph);
    LBoundaryGraph.Entry[0, 0, 0].Value := ModelTokenAsGraphValue(
      LModel.PatternKeyAt(0));
    LOptions := DefaultGraphSolveOptions;
    Check(LBoundaryGraph.TrySolve(LOptions, LSolveReport),
      'empty support does not constrain absent pattern-grid neighbors');
  finally
    LBoundaryGraph.Free;
    LModel.Free;
  end;
end;

procedure TestCanonicalPatternTextCodec;
var
  LD4Corrupt: String;
  LD4Model: TWfcOverlappingModel2D;
  LD4Text: String;
  LDecoded: TWfcOverlappingModel2D;
  LEncoded: String;
  LModel: TWfcOverlappingModel2D;
  LSamples: TWfcLearnSamples;
begin
  LModel := LearnOverlappingModel2D(TokensOf([',', MusicalNoteToken]),
    2, 1, 2, 1, wmbWrap, wmsNone);
  try
    LEncoded := EncodeWfcPattern2DText(LModel);
    Check(LEncoded = GOLDEN_PUNCTUATION_UNICODE,
      'wfcp=1 encoding matches the exact canonical golden artifact');
    Check((Pos('%2C', LEncoded) > 0)
      and (Pos('%E2%99%AB', LEncoded) > 0),
      'punctuation and Unicode palette tokens use canonical UTF-8 escapes');
    Check(Pos('@p', LEncoded) = 0,
      'serialization never leaks private graph-domain pattern keys');

    LDecoded := DecodeWfcPattern2DText(GOLDEN_PUNCTUATION_UNICODE);
    try
      Check((LDecoded.PatternWidth = 2)
        and (LDecoded.PatternHeight = 1)
        and (LDecoded.SourceBoundary = wmbWrap)
        and (LDecoded.PaletteTokenAt(0) = ',')
        and (LDecoded.PaletteTokenAt(1) = MusicalNoteToken)
        and PayloadMatches(LDecoded, 0, [0, 1])
        and PayloadMatches(LDecoded, 1, [1, 0]),
        'decoding restores punctuation, Unicode, metadata, and payloads');
      Check(EncodeWfcPattern2DText(LDecoded) =
        GOLDEN_PUNCTUATION_UNICODE,
        'decode then encode is byte-for-byte canonical');
    finally
      LDecoded.Free;
    end;
  finally
    LModel.Free;
  end;

  CheckDecodeRejected(ReplaceOnce(ReplaceOnce(
    GOLDEN_PUNCTUATION_UNICODE, 'relations=12', 'relations=11'),
    'r=N,0,0'#10, ''),
    'the decoder rejects a missing structural relation');
  CheckDecodeRejected(ReplaceOnce(GOLDEN_PUNCTUATION_UNICODE,
    'r=E,0,1', 'r=E,0,0'),
    'the decoder rejects an invented nonstructural relation');
  CheckDecodeRejected(ReplaceOnce(GOLDEN_PUNCTUATION_UNICODE,
    'r=N,0,0'#10'r=N,0,1'#10,
    'r=N,0,1'#10'r=N,0,0'#10),
    'the decoder rejects reordered relation records');

  CheckDecodeRejected(ReplaceOnce(GOLDEN_PUNCTUATION_UNICODE,
    't=0,%2C'#10't=1,%E2%99%AB'#10,
    't=1,%E2%99%AB'#10't=0,%2C'#10),
    'the decoder rejects reordered palette records');
  CheckDecodeRejected(ReplaceOnce(GOLDEN_PUNCTUATION_UNICODE,
    't=1,%E2%99%AB', 't=1,%2C'),
    'the decoder rejects duplicate palette values');
  CheckDecodeRejected(ReplaceOnce(GOLDEN_PUNCTUATION_UNICODE,
    'p=0,1,0,1'#10'p=1,1,1,0'#10,
    'p=1,1,1,0'#10'p=0,1,0,1'#10),
    'the decoder rejects reordered pattern records');
  CheckDecodeRejected(ReplaceOnce(GOLDEN_PUNCTUATION_UNICODE,
    'p=0,1,0,1', 'p=0,1,0,2'),
    'the decoder rejects an out-of-range pattern palette index');
  CheckDecodeRejected(ReplaceOnce(GOLDEN_PUNCTUATION_UNICODE,
    'p=0,1,0,1', 'p=0,2,0,1'),
    'the decoder rejects corrupted raw pattern weights');
  CheckDecodeRejected(ReplaceOnce(GOLDEN_PUNCTUATION_UNICODE,
    's=0,2,1', 's=0,3,1'),
    'the decoder rejects source-shape metadata inconsistent with weights');
  CheckDecodeRejected(ReplaceOnce(GOLDEN_PUNCTUATION_UNICODE,
    'footprint=2,1', 'footprint=3,1'),
    'the decoder rejects footprint metadata inconsistent with payloads');

  CheckDecodeRejected(ReplaceOnce(GOLDEN_PUNCTUATION_UNICODE,
    'symmetry=none', 'symmetry=d4'),
    'the decoder rejects non-square D4 metadata');
  SetLength(LSamples, 2);
  LSamples[0] := MakeLearnSample2D(TokensOf([
    'A', 'B',
    'C', 'D']), 2, 2);
  LSamples[1] := MakeLearnSample2D(TokensOf([
    'A', 'B',
    'C', 'D']), 2, 2);
  LD4Model := LearnOverlappingModel2DCorpus(LSamples,
    2, 2, wmbOpen, wmsD4);
  try
    LD4Text := EncodeWfcPattern2DText(LD4Model);
  finally
    LD4Model.Free;
  end;
  LD4Corrupt := ReplaceOnce(LD4Text,
    'p=0,2,0,1,2,3', 'p=0,3,0,1,2,3');
  LD4Corrupt := ReplaceOnce(LD4Corrupt,
    'p=1,2,2,0,3,1', 'p=1,1,2,0,3,1');
  CheckDecodeRejected(LD4Corrupt,
    'the decoder rejects unequal weights within a complete D4 orbit');

  CheckDecodeRejected(ReplaceOnce(GOLDEN_PUNCTUATION_UNICODE,
    'rank=2', 'rank=02'),
    'the decoder rejects noncanonical leading-zero integers');
  CheckDecodeRejected(ReplaceOnce(GOLDEN_PUNCTUATION_UNICODE,
    '%E2%99%AB', '%e2%99%AB'),
    'the decoder rejects lowercase percent escapes');
  CheckDecodeRejected(ReplaceOnce(GOLDEN_PUNCTUATION_UNICODE,
    #10, #13#10),
    'the decoder rejects CRLF in place of canonical LF');
  CheckDecodeRejected(Copy(GOLDEN_PUNCTUATION_UNICODE, 1,
    Length(GOLDEN_PUNCTUATION_UNICODE) - 1),
    'the decoder requires a final LF');
end;

procedure TestVersionedResourceLimits;
var
  LPalette: TWfcModelTokens;
  LPatterns: TWfcPattern2DPayloads;
  LSamples: TWfcLearnSamples;
  LShapes: TWfcModelSampleShapes;
  LWeights: TWfcModelIntegerArray;
begin
  Check((WFC_PATTERN_2D_LIMITS_VERSION = 1) and
    (WFC_PATTERN_2D_MAX_SOURCE_COUNT = 65536) and
    (WFC_PATTERN_2D_MAX_SOURCE_DIMENSION = 4194304) and
    (WFC_PATTERN_2D_MAX_SOURCE_CELL_COUNT = 4194304) and
    (WFC_PATTERN_2D_MAX_TOTAL_SOURCE_CELL_COUNT = 4194304) and
    (WFC_PATTERN_2D_MAX_FOOTPRINT_DIMENSION = 4096) and
    (WFC_PATTERN_2D_MAX_FOOTPRINT_CELL_COUNT = 4096) and
    (WFC_PATTERN_2D_MAX_PALETTE_COUNT = 4096) and
    (WFC_PATTERN_2D_MAX_PATTERN_COUNT = 1024) and
    (WFC_PATTERN_2D_MAX_TOTAL_PATTERN_CELL_COUNT = 4194304) and
    (WFC_PATTERN_2D_MAX_RELATION_SLOT_COUNT = 4194304) and
    (WFC_PATTERN_2D_MAX_ENCODED_TEXT_LENGTH = 16777216) and
    (WFC_PATTERN_2D_MAX_TEXT_LINE_COUNT = 262144),
    'version-one pattern cardinality, dimension, dense-slot, and envelope limits are exact');

  SetLength(LShapes, 1);
  LShapes[0] := MakeWfcModelSampleShape(1, 1);
  LPalette := TokensOf(['A']);
  SetLength(LPatterns, 1);
  LPatterns[0] := PayloadOf([0]);
  LWeights := IntegersOf([1]);

  Check(PatternModelLimitRejected(
    WFC_PATTERN_2D_MAX_FOOTPRINT_DIMENSION + 1, 1,
    LShapes, LPalette, LPatterns, LWeights,
    'pattern dimension exceeds the version-1 limit'),
    'an oversized footprint dimension is rejected before multiplication');
  Check(PatternModelLimitRejected(65, 64,
    LShapes, LPalette, LPatterns, LWeights,
    'pattern cells exceed the version-1 limit'),
    'an oversized footprint area is rejected before multiplication');

  SetLength(LShapes, WFC_PATTERN_2D_MAX_SOURCE_COUNT + 1);
  Check(PatternModelLimitRejected(1, 1,
    LShapes, LPalette, LPatterns, LWeights,
    'source count exceeds the version-1 limit'),
    'source cardinality is rejected before inspecting or copying shapes');

  SetLength(LShapes, 1);
  LShapes[0] := MakeWfcModelSampleShape(
    WFC_PATTERN_2D_MAX_SOURCE_DIMENSION + 1, 1);
  Check(PatternModelLimitRejected(1, 1,
    LShapes, LPalette, LPatterns, LWeights,
    'source dimension exceeds the version-1 limit'),
    'an oversized source dimension is rejected before multiplication');
  LShapes[0] := MakeWfcModelSampleShape(2049, 2048);
  Check(PatternModelLimitRejected(1, 1,
    LShapes, LPalette, LPatterns, LWeights,
    'source cells exceed the version-1 limit'),
    'an oversized source area is rejected before multiplication');
  SetLength(LShapes, 2);
  LShapes[0] := MakeWfcModelSampleShape(2097153, 1);
  LShapes[1] := MakeWfcModelSampleShape(2097153, 1);
  Check(PatternModelLimitRejected(1, 1,
    LShapes, LPalette, LPatterns, LWeights,
    'aggregate overlapping source cells exceed the version-1 limit'),
    'aggregate source cells are bounded before pattern validation work');

  SetLength(LShapes, 1);
  LShapes[0] := MakeWfcModelSampleShape(1, 1);
  SetLength(LPalette, WFC_PATTERN_2D_MAX_PALETTE_COUNT + 1);
  Check(PatternModelLimitRejected(1, 1,
    LShapes, LPalette, LPatterns, LWeights,
    'palette count exceeds the version-1 limit'),
    'palette cardinality is rejected before quadratic uniqueness work');

  LPalette := TokensOf(['A']);
  SetLength(LPatterns, WFC_PATTERN_2D_MAX_PATTERN_COUNT + 1);
  Check(PatternModelLimitRejected(1, 1,
    LShapes, LPalette, LPatterns, nil,
    'pattern count exceeds the version-1 limit'),
    'pattern cardinality is rejected before dense relation allocation');

  SetLength(LSamples, 1);
  LSamples[0].Width := WFC_PATTERN_2D_MAX_SOURCE_DIMENSION + 1;
  LSamples[0].Height := 1;
  LSamples[0].Tokens := nil;
  Check(PatternLearningLimitRejected(LSamples, 1, 1,
    'sample dimension exceeds the version-1 limit'),
    'the overlapping learner applies source limits before owned allocation');

  CheckDecodeRejected(StringOfChar('x',
    WFC_PATTERN_2D_MAX_ENCODED_TEXT_LENGTH + 1),
    'oversized pattern text is rejected before line splitting');
  CheckDecodeRejected(StringOfChar(#10,
    WFC_PATTERN_2D_MAX_TEXT_LINE_COUNT + 1),
    'excessive pattern-text newlines are rejected before line splitting');
  CheckDecodeRejected(ReplaceOnce(GOLDEN_PUNCTUATION_UNICODE,
    'samples=1', 'samples=65537'),
    'serialized source cardinality is bounded before shape allocation');
  CheckDecodeRejected(ReplaceOnce(GOLDEN_PUNCTUATION_UNICODE,
    's=0,2,1', 's=0,4194305,1'),
    'serialized source dimensions are bounded before multiplication');
  CheckDecodeRejected(ReplaceOnce(GOLDEN_PUNCTUATION_UNICODE,
    's=0,2,1', 's=0,2049,2048'),
    'serialized source cells are bounded before multiplication');
  CheckDecodeRejected(ReplaceOnce(GOLDEN_PUNCTUATION_UNICODE,
    'samples=1'#10's=0,2,1',
    'samples=2'#10's=0,2097153,1'#10's=1,2097153,1'),
    'serialized aggregate source cells are bounded before later allocations');
  CheckDecodeRejected(ReplaceOnce(GOLDEN_PUNCTUATION_UNICODE,
    'footprint=2,1', 'footprint=4097,1'),
    'serialized footprint dimensions are bounded before multiplication');
  CheckDecodeRejected(ReplaceOnce(GOLDEN_PUNCTUATION_UNICODE,
    'footprint=2,1', 'footprint=65,64'),
    'serialized footprint cells are bounded before payload allocation');
  CheckDecodeRejected(ReplaceOnce(GOLDEN_PUNCTUATION_UNICODE,
    'palette=2', 'palette=4097'),
    'serialized palette cardinality is bounded before allocation');
  CheckDecodeRejected(ReplaceOnce(GOLDEN_PUNCTUATION_UNICODE,
    'patterns=2', 'patterns=1025'),
    'serialized pattern cardinality is bounded before dense allocation');
end;

begin
  WriteLn('WFC overlapping-pattern 2D conformance suite');
  WriteLn('============================================');
  RunTest('deterministic palette, pattern, and weight order',
    @TestDeterministicLearning);
  RunTest('exact structural cardinal compatibility',
    @TestStructuralCompatibility);
  RunTest('corpus isolation and structural generalization',
    @TestCorpusIsolationAndGeneralization);
  RunTest('open and wrapped extraction boundaries',
    @TestOpenAndWrappedExtraction);
  RunTest('D4 transform closure and raw frequencies', @TestD4Closure);
  RunTest('immutability and constructor guards', @TestImmutabilityAndGuards);
  RunTest('private stable graph-domain keys', @TestPrivateStableKeys);
  RunTest('assignment validation and open projection',
    @TestAssignmentValidationAndProjection);
  RunTest('wrapped projection and tamper reports',
    @TestWrappedProjectionAndTamperReports);
  RunTest('graph apply, solve, and capture', @TestGraphApplySolveAndCapture);
  RunTest('adapter open-boundary support', @TestAdapterOpenBoundarySupport);
  RunTest('versioned resource limits', @TestVersionedResourceLimits);
  RunTest('canonical wfcp=1 text codec', @TestCanonicalPatternTextCodec);
  WriteLn('============================================');
  WriteLn(Format('%d checks, %d failures',
    [GCheckCount, GFailureCount]));

  if GFailureCount > 0 then
  {$IFDEF PAS2JS}
    raise Exception.CreateFmt('%d overlapping-pattern checks failed',
      [GFailureCount]);
  {$ELSE}
    Halt(1);
  {$ENDIF}
end.
