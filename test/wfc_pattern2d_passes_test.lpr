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
program wfc_pattern2d_passes_test;

{$mode delphi}{$H+}

uses
  {$IFDEF PAS2JS}wfc_browser_test_host,{$ENDIF}
  SysUtils,
  wfc,
  wfc_model,
  wfc_pattern2d,
  wfc_pattern2d_learn,
  wfc_pattern2d_graph;

type
  TTestProcedure = procedure;

  TRejectingPatternPipeline = class(TWfcPattern2DPassPipeline)
  strict private
    FRejectNext: Boolean;
    FRejectAtWrappedProjection: Boolean;
  strict protected
    function DoValidateProjection(const APatterns: TWfcPatternGrid2D;
      const AProjection: TWfcTokenGrid2D;
      out AReport: TWfcOverlapping2DValidationReport): Boolean; override;
  public
    property RejectNext: Boolean read FRejectNext write FRejectNext;
    property RejectAtWrappedProjection: Boolean
      read FRejectAtWrappedProjection write FRejectAtWrappedProjection;
  end;

const
  CHECKER_WIDTH = 4;
  CHECKER_HEIGHT = 4;
  CHECKER_SEED = TGraphSeed(246813579);
  NEXT_CHECKER_SEED = TGraphSeed(324508639);
  EXPECTED_CHECKER_SIGNATURE = '413365F3';

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

function CheckerModel: TWfcOverlappingModel2D;
begin
  Result := LearnOverlappingModel2D(TokensOf([
    'A', 'B',
    'B', 'A']), 2, 2, 2, 2, wmbWrap, wmsNone);
end;

function WrongPatternModel: TWfcOverlappingModel2D;
begin
  Result := LearnOverlappingModel2D(TokensOf([
    'A', 'A',
    'B', 'B']), 2, 2, 2, 2, wmbWrap, wmsNone);
end;

function CoverageGapModel: TWfcOverlappingModel2D;
begin
  Result := LearnOverlappingModel2D(TokensOf([
    'A', 'B',
    'C', 'D']), 2, 2, 2, 2, wmbOpen, wmsNone);
end;

function ReservedPaletteModel: TWfcOverlappingModel2D;
begin
  Result := LearnOverlappingModel2D(TokensOf([
    '@p0', 'B',
    'B', '@p0']), 2, 2, 2, 2, wmbWrap, wmsNone);
end;

function AliasedTorusModel: TWfcOverlappingModel2D;
begin
  { The three-cell footprint is wider than the two-cell graph used below.
    Its first and third coordinates therefore read the same wrapped anchor,
    while some retained patterns deliberately store different tokens there. }
  Result := LearnOverlappingModel2D(TokensOf([
    'A', 'B', 'A', 'A']), 4, 1, 3, 1, wmbWrap, wmsNone);
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

function GraphValuesEqual(const A, B: TGraphValues): Boolean;
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

function PatternGridsEqual(const A, B: TWfcPatternGrid2D): Boolean;
var
  I: Integer;
begin
  if (A.Width <> B.Width) or (A.Height <> B.Height) or
      (A.Boundary <> B.Boundary) or
      (Length(A.Patterns) <> Length(B.Patterns)) then
    Exit(False);
  for I := 0 to Length(A.Patterns) - 1 do
    if A.Patterns[I] <> B.Patterns[I] then
      Exit(False);
  Result := True;
end;

function TokenGridsEqual(const A, B: TWfcTokenGrid2D): Boolean;
var
  I: Integer;
begin
  if (A.Width <> B.Width) or (A.Height <> B.Height) or
      (Length(A.Tokens) <> Length(B.Tokens)) then
    Exit(False);
  for I := 0 to Length(A.Tokens) - 1 do
    if A.Tokens[I] <> B.Tokens[I] then
      Exit(False);
  Result := True;
end;

function WrapCoordinate(const AValue, ASize: Integer): Integer;
begin
  Result := AValue mod ASize;
  if Result < 0 then
    Inc(Result, ASize);
end;

function NewSourceAndTarget(const AModel: TWfcOverlappingModel2D;
  const AWidth, AHeight, ADepth: Integer;
  const AWrap, AOverlayTarget: Boolean): TGraph;
begin
  Result := TGraph.Create;
  try
    Result.Reshape(AWidth, AHeight, ADepth);
    Result.WrapNeighbors := AWrap;
    Result.Seed := CHECKER_SEED;
    Result.CurrentPass := 'patterns';
    Result.PassMode := gpmOverlay;
    Result.ClearDependencies;
    ApplyOverlappingModel2DToGraph(AModel, Result);
    Result.SwitchToPass('terrain');
    if AOverlayTarget then
    begin
      Result.PassMode := gpmOverlay;
      Result.ClearDependencies;
    end;
  except
    Result.Free;
    raise;
  end;
end;

function NewCompleteGraph(const AModel: TWfcOverlappingModel2D): TGraph;
begin
  Result := NewSourceAndTarget(AModel, CHECKER_WIDTH,
    CHECKER_HEIGHT, 1, True, True);
  try
    ApplyOverlappingProjectionFromPass2D(AModel, Result, 'patterns');
    Result.SwitchToPass('foliage');
    Result.PassMode := gpmOverlay;
    Result.ClearDependencies;
    Result.AddValue('tree');
    Result.AddValue('reed');
    Result.Rules['tree'].RequireFromPass('terrain', 'A');
    Result.Rules['reed'].RequireFromPass('terrain', 'B');
    Result.SwitchToPass('patterns');
  except
    Result.Free;
    raise;
  end;
end;

function TRejectingPatternPipeline.DoValidateProjection(
  const APatterns: TWfcPatternGrid2D;
  const AProjection: TWfcTokenGrid2D;
  out AReport: TWfcOverlapping2DValidationReport): Boolean;
begin
  Result := inherited DoValidateProjection(APatterns, AProjection,
    AReport);
  if Result and FRejectNext then
  begin
    FRejectNext := False;
    AReport.Valid := False;
    AReport.Issue.Kind := woikProjectionToken;
    if FRejectAtWrappedProjection then
    begin
      FRejectAtWrappedProjection := False;
      AReport.Issue.X := Width - 1;
      AReport.Issue.Y := Height - 1;
      AReport.Issue.PatternOffsetX := 1;
      AReport.Issue.PatternOffsetY := 1;
    end
    else
    begin
      { Negative offsets from the inherited initialized report identify a
        custom hook whose X,Y already name its failed public entry. }
      AReport.Issue.X := 0;
      AReport.Issue.Y := 0;
    end;
    Result := False;
  end;
end;

procedure ExpectBridgeRejected(const AGraph: TGraph;
  const AModel: TWfcOverlappingModel2D; const ASourcePass,
  AMessage: String);
var
  LBeforeDependencies: Integer;
  LBeforeHasDefinition: Boolean;
  LBeforePass: String;
  LBeforePassIndex: Integer;
  LBeforeRuleCount: Integer;
  LBeforeValues: TGraphValues;
  LError: String;
  LRaised: Boolean;
begin
  LBeforeDependencies := AGraph.DependencyCount;
  LBeforeHasDefinition := AGraph.HasDefinition;
  LBeforePass := AGraph.CurrentPass;
  LBeforePassIndex := AGraph.CurrentPassIndex;
  LBeforeRuleCount := AGraph.RuleGroups.Count;
  LBeforeValues := AGraph.CopyRegisteredValues;
  LError := '';
  LRaised := False;
  try
    ApplyOverlappingProjectionFromPass2D(AModel, AGraph, ASourcePass);
  except
    on E: Exception do
    begin
      LRaised := True;
      LError := E.Message;
    end;
  end;
  Check(LRaised and (Pos('@p', LError) = 0) and
    (AGraph.DependencyCount = LBeforeDependencies) and
    (AGraph.HasDefinition = LBeforeHasDefinition) and
    (AGraph.CurrentPass = LBeforePass) and
    (AGraph.CurrentPassIndex = LBeforePassIndex) and
    (AGraph.RuleGroups.Count = LBeforeRuleCount) and
    GraphValuesEqual(AGraph.CopyRegisteredValues, LBeforeValues), AMessage);
end;

procedure TestPreflightAndAtomicity;
var
  LBeforeDependencies: Integer;
  LBeforeValues: TGraphValues;
  LGap: TWfcOverlappingModel2D;
  LGraph: TGraph;
  LModel: TWfcOverlappingModel2D;
  LReserved: TWfcOverlappingModel2D;
  LRaised: Boolean;
  LWrong: TWfcOverlappingModel2D;
begin
  LModel := CheckerModel;
  LWrong := WrongPatternModel;
  LGap := CoverageGapModel;
  LReserved := ReservedPaletteModel;
  try
    LGraph := NewSourceAndTarget(LModel, 4, 4, 1, True, True);
    try
      LBeforeDependencies := LGraph.DependencyCount;
      LBeforeValues := LGraph.CopyRegisteredValues;
      ValidateOverlappingProjectionFromPass2D(LModel, LGraph,
        'patterns');
      Check((LGraph.DependencyCount = LBeforeDependencies) and
        GraphValuesEqual(LGraph.CopyRegisteredValues, LBeforeValues) and
        (not LGraph.HasDefinition),
        'successful preflight is read-only');
      ApplyOverlappingProjectionFromPass2D(LModel, LGraph, 'patterns');
      Check(LGraph.HasDefinition and (LGraph.DependencyCount = 1) and
        (LGraph.DependencyIndex[0] = 0),
        'apply materializes a defined target and declares its source');
    finally
      LGraph.Free;
    end;

    LGraph := NewSourceAndTarget(LModel, 4, 4, 1, False, True);
    try
      ExpectBridgeRejected(LGraph, LModel, 'patterns',
        'non-wrapped topology is rejected atomically');
    finally
      LGraph.Free;
    end;

    LGraph := NewSourceAndTarget(LModel, 4, 4, 2, True, True);
    try
      ExpectBridgeRejected(LGraph, LModel, 'patterns',
        'non-plane topology is rejected atomically');
    finally
      LGraph.Free;
    end;

    LGraph := NewSourceAndTarget(LModel, 4, 4, 1, True, False);
    try
      ExpectBridgeRejected(LGraph, LModel, 'patterns',
        'a non-overlay target is rejected atomically');
    finally
      LGraph.Free;
    end;

    LGraph := NewSourceAndTarget(LModel, 4, 4, 1, True, True);
    try
      LGraph.AddValue('existing');
      ExpectBridgeRejected(LGraph, LModel, 'patterns',
        'a nonempty target is rejected atomically');
    finally
      LGraph.Free;
    end;

    LGraph := TGraph.Create;
    try
      LGraph.Reshape(4, 4, 1);
      LGraph.WrapNeighbors := True;
      LGraph.CurrentPass := 'terrain';
      LGraph.PassMode := gpmOverlay;
      LGraph.ClearDependencies;
      ExpectBridgeRejected(LGraph, LModel, 'missing',
        'a missing source pass is rejected atomically');
    finally
      LGraph.Free;
    end;

    LGraph := NewSourceAndTarget(LModel, 4, 4, 1, True, True);
    try
      ExpectBridgeRejected(LGraph, LWrong, 'patterns',
        'a mismatched applied source model is rejected atomically');
    finally
      LGraph.Free;
    end;

    LGraph := NewSourceAndTarget(LGap, 4, 4, 1, True, True);
    try
      ExpectBridgeRejected(LGraph, LGap, 'patterns',
        'incomplete per-offset palette coverage is rejected atomically');
    finally
      LGraph.Free;
    end;

    LGraph := NewSourceAndTarget(LReserved, 4, 4, 1, True, True);
    try
      ExpectBridgeRejected(LGraph, LReserved, 'patterns',
        'reserved @pN palette syntax is rejected atomically');
    finally
      LGraph.Free;
    end;

    LGraph := NewSourceAndTarget(LModel, 4, 4, 1, True, True);
    try
      LGraph.SwitchToPass('patterns');
      LGraph.DependsOn('terrain');
      LGraph.SwitchToPass('terrain');
      ExpectBridgeRejected(LGraph, LModel, 'patterns',
        'a dependency cycle is rejected atomically');
    finally
      LGraph.Free;
    end;

    LGraph := NewSourceAndTarget(LModel, 4, 4, 1, True, True);
    try
      LRaised := False;
      try
        ValidateOverlappingProjectionFromPass2D(nil, LGraph, 'patterns');
      except
        on E: EArgumentNilException do LRaised := True;
      end;
      Check(LRaised and (not LGraph.HasDefinition) and
        (LGraph.DependencyCount = 0),
        'nil model preflight fails before target mutation');
    finally
      LGraph.Free;
    end;

    LRaised := False;
    try
      ValidateOverlappingProjectionFromPass2D(LModel, nil, 'patterns');
    except
      on E: EArgumentNilException do LRaised := True;
    end;
    Check(LRaised, 'nil graph preflight is rejected explicitly');
  finally
    LReserved.Free;
    LGap.Free;
    LWrong.Free;
    LModel.Free;
  end;
end;

procedure TestProjectionAndDownstreamSemantics;
var
  LAllContributions: Boolean;
  LAnchorPattern: Integer;
  LAnchorX: Integer;
  LAnchorY: Integer;
  LExpected: TWfcModelToken;
  LFoliageCorrect: Boolean;
  LGraph: TGraph;
  LMaterialized: TWfcTokenGrid2D;
  LModel: TWfcOverlappingModel2D;
  LOptions: TGraphSolveOptions;
  LPatterns: TWfcPatternGrid2D;
  LPrivateSafe: Boolean;
  LProjected: TWfcTokenGrid2D;
  LReport: TGraphSolveReport;
  LSeamContributions: Integer;
  LValidation: TWfcOverlapping2DValidationReport;
  LValues: TGraphValues;
  PX: Integer;
  PY: Integer;
  X: Integer;
  Y: Integer;
begin
  LModel := CheckerModel;
  LGraph := NewCompleteGraph(LModel);
  try
    LValues := LGraph.PassGraph[1].CopyRegisteredValues;
    Check((Length(LValues) = 2) and
      (LValues[0] = ModelTokenAsGraphValue('A')) and
      (LValues[1] = ModelTokenAsGraphValue('B')) and
      (LGraph.PassGraph[1].Rules[LValues[0]].Weight =
        WFC_DEFAULT_VALUE_WEIGHT) and
      (LGraph.PassGraph[1].Rules[LValues[1]].Weight =
        WFC_DEFAULT_VALUE_WEIGHT),
      'projection registers the ordered public palette at unit weight');
    Check((LGraph.PassGraph[2].DependencyCount = 1) and
      (LGraph.PassGraph[2].DependencyIndex[0] = 1),
      'a semantic consumer depends on the public projection only');

    LOptions := DefaultGraphSolveOptions;
    Check(LGraph.TrySolve(LOptions, LReport),
      'patterns, public terrain, and downstream foliage solve together');
    LGraph.SwitchToPass('foliage');
    Check(CaptureSolvedOverlappingProjectionPass2D(LModel,
      LGraph.PassGraph[0], LGraph.PassGraph[1], LPatterns,
      LMaterialized, LValidation) and
      (LGraph.CurrentPass = 'foliage'),
      'capture validates exact pass objects without changing selection');
    Check(LValidation.Valid and
      (LValidation.CheckedPatterns = CHECKER_WIDTH * CHECKER_HEIGHT) and
      (LValidation.CheckedRelations = 2 * CHECKER_WIDTH * CHECKER_HEIGHT) and
      (LValidation.CheckedProjectionCells = CHECKER_WIDTH * CHECKER_HEIGHT *
        LModel.PatternWidth * LModel.PatternHeight),
      'independent validation checks every pattern, relation, and footprint write');

    LAllContributions := True;
    LSeamContributions := 0;
    for Y := 0 to CHECKER_HEIGHT - 1 do
      for X := 0 to CHECKER_WIDTH - 1 do
        for PY := 0 to LModel.PatternHeight - 1 do
          for PX := 0 to LModel.PatternWidth - 1 do
          begin
            LAnchorX := WrapCoordinate(X - PX, CHECKER_WIDTH);
            LAnchorY := WrapCoordinate(Y - PY, CHECKER_HEIGHT);
            LAnchorPattern := LPatterns.Patterns[
              LAnchorY * CHECKER_WIDTH + LAnchorX];
            LExpected := LModel.PaletteTokenAt(
              LModel.PatternPaletteIndexAt(LAnchorPattern, PX, PY));
            if LMaterialized.Tokens[Y * CHECKER_WIDTH + X] <>
                LExpected then
              LAllContributions := False;
            if (X - PX < 0) or (Y - PY < 0) then
              Inc(LSeamContributions);
          end;
    Check(LAllContributions and (LSeamContributions = 15),
      'every signed footprint clause, including all wrapped seam clauses, agrees');

    Check(TryProjectOverlappingPatternGrid2D(LModel, LPatterns,
      LProjected, LValidation) and
      TokenGridsEqual(LProjected, LMaterialized),
      'materialized terrain equals the standalone overlapping projector');

    LFoliageCorrect := True;
    for Y := 0 to CHECKER_HEIGHT - 1 do
      for X := 0 to CHECKER_WIDTH - 1 do
        if ((LMaterialized.Tokens[Y * CHECKER_WIDTH + X] = 'A') and
            (LGraph.PassGraph[2].Entry[X, Y, 0].Value <> 'tree')) or
           ((LMaterialized.Tokens[Y * CHECKER_WIDTH + X] = 'B') and
            (LGraph.PassGraph[2].Entry[X, Y, 0].Value <> 'reed')) then
          LFoliageCorrect := False;
    Check(LFoliageCorrect,
      'downstream semantics consume public terrain tokens at each coordinate');

    LPrivateSafe := True;
    for X := 0 to Length(LValues) - 1 do
      if Pos('@p', LValues[X]) > 0 then
        LPrivateSafe := False;
    for X := 0 to Length(LMaterialized.Tokens) - 1 do
      if Pos('@p', String(LMaterialized.Tokens[X])) > 0 then
        LPrivateSafe := False;
    Check(LPrivateSafe,
      'the public registered domain and captured result contain no private keys');

    LGraph.PassGraph[0].Entry[0, 0, 0].Value := '@p999';
    Check(not CaptureSolvedOverlappingProjectionPass2D(LModel,
      LGraph.PassGraph[0], LGraph.PassGraph[1], LPatterns,
      LMaterialized, LValidation) and
      (LValidation.Issue.Kind = woikUnknownPatternKey) and
      (LValidation.Issue.Value = '') and
      (Length(LPatterns.Patterns) = 0) and
      (Length(LMaterialized.Tokens) = 0),
      'failed capture is atomic and scrubs a malformed private key');
  finally
    LGraph.Free;
    LModel.Free;
  end;
end;

procedure TestCapturePassIdentity;
var
  LFirst: TGraph;
  LGrid: TWfcPatternGrid2D;
  LModel: TWfcOverlappingModel2D;
  LOptions: TGraphSolveOptions;
  LProjection: TWfcTokenGrid2D;
  LRaised: Boolean;
  LReport: TGraphSolveReport;
  LSecond: TGraph;
  LValidation: TWfcOverlapping2DValidationReport;
begin
  LModel := CheckerModel;
  LFirst := NewCompleteGraph(LModel);
  LSecond := NewCompleteGraph(LModel);
  try
    LOptions := DefaultGraphSolveOptions;
    Check(LFirst.TrySolve(LOptions, LReport) and
      LSecond.TrySolve(LOptions, LReport),
      'independent identity fixtures solve');
    LRaised := False;
    try
      CaptureSolvedOverlappingProjectionPass2D(LModel,
        LFirst.PassGraph[0], LSecond.PassGraph[1], LGrid,
        LProjection, LValidation);
    except
      on E: EWfcPattern2DGraph do LRaised := True;
    end;
    Check(LRaised and (Length(LGrid.Patterns) = 0) and
      (Length(LProjection.Tokens) = 0),
      'capture rejects pass objects from different pipelines');
  finally
    LSecond.Free;
    LFirst.Free;
    LModel.Free;
  end;
end;

procedure TestTinyWrappedOffsetAliasing;
const
  TINY_WIDTH = 2;
  TINY_HEIGHT = 1;
var
  LAliasedContributions: Boolean;
  LConflictingPattern: Integer;
  LGraph: TGraph;
  LMaterialized: TWfcTokenGrid2D;
  LModel: TWfcOverlappingModel2D;
  LOptions: TGraphSolveOptions;
  LPattern: Integer;
  LPatterns: TWfcPatternGrid2D;
  LProjected: TWfcTokenGrid2D;
  LReport: TGraphSolveReport;
  LValidation: TWfcOverlapping2DValidationReport;
  X: Integer;
begin
  LModel := AliasedTorusModel;
  LGraph := NewSourceAndTarget(LModel, TINY_WIDTH, TINY_HEIGHT,
    1, True, True);
  try
    ApplyOverlappingProjectionFromPass2D(LModel, LGraph, 'patterns');
    LConflictingPattern := -1;
    for LPattern := 0 to LModel.PatternCount - 1 do
      if LModel.PatternPaletteIndexAt(LPattern, 0, 0) <>
          LModel.PatternPaletteIndexAt(LPattern, 2, 0) then
      begin
        LConflictingPattern := LPattern;
        Break;
      end;
    Check((LModel.PatternWidth > TINY_WIDTH) and
      (WrapCoordinate(0, TINY_WIDTH) =
        WrapCoordinate(-2, TINY_WIDTH)) and
      (LConflictingPattern >= 0),
      'tiny fixture aliases distinct footprint offsets on one wrapped anchor');

    LOptions := DefaultGraphSolveOptions;
    Check(LGraph.TrySolve(LOptions, LReport),
      'the tiny aliased-offset projection solves deterministically');
    Check(CaptureSolvedOverlappingProjectionPass2D(LModel,
      LGraph.PassGraph[0], LGraph.PassGraph[1], LPatterns,
      LMaterialized, LValidation) and LValidation.Valid and
      (LValidation.CheckedProjectionCells = TINY_WIDTH * TINY_HEIGHT *
        LModel.PatternWidth * LModel.PatternHeight),
      'independent capture checks every aliased footprint contribution');

    LAliasedContributions := True;
    for X := 0 to TINY_WIDTH - 1 do
    begin
      LPattern := LPatterns.Patterns[X];
      if (LMaterialized.Tokens[X] <>
          LModel.PaletteTokenAt(LModel.PatternPaletteIndexAt(
            LPattern, 0, 0))) or
         (LMaterialized.Tokens[X] <>
          LModel.PaletteTokenAt(LModel.PatternPaletteIndexAt(
            LPattern, 2, 0))) then
        LAliasedContributions := False;
    end;
    Check(LAliasedContributions and
      TryProjectOverlappingPatternGrid2D(LModel, LPatterns,
        LProjected, LValidation) and
      TokenGridsEqual(LProjected, LMaterialized),
      'both aliased clauses agree with the independently materialized ' +
      'projection');

    { Regenerate only the public pass from a registered latent payload whose
      two aliased cells disagree. Pooling the two supports would allow a token;
      preserving both AND clauses makes public entry zero impossible. }
    LGraph.PassGraph[0].Entry[0, 0, 0].Value :=
      ModelTokenAsGraphValue(LModel.PatternKeyAt(LConflictingPattern));
    Check((not LGraph.TryRegenerateFrom('terrain', LOptions, LReport)) and
      (LReport.Status = gssContradiction) and
      (LReport.FailedPassIndex = 1) and
      (LReport.Contradiction.Kind = gckPassDependency) and
      (LReport.Contradiction.PassIndex = 1) and
      (LReport.Contradiction.DependencyPassIndex = 0) and
      (LReport.Contradiction.EntryIndex = 0),
      'distinct aliased offsets remain conjunctive instead of pooling support');
  finally
    LGraph.Free;
    LModel.Free;
  end;
end;

procedure TestOwnerRollbackAndReplay;
var
  LAfterFailure: TWfcPattern2DComposition;
  LBaselineA: TWfcPattern2DComposition;
  LBaselineB: TWfcPattern2DComposition;
  LConfig: TWfcPattern2DPassConfig;
  LDetached: TWfcTokenGrid2D;
  LDetachedPatterns: TWfcPatternGrid2D;
  LFailed: TWfcPattern2DComposition;
  LModel: TWfcOverlappingModel2D;
  LNextA: TWfcPattern2DComposition;
  LNextB: TWfcPattern2DComposition;
  LPipelineA: TRejectingPatternPipeline;
  LPipelineB: TRejectingPatternPipeline;
  LReport: TWfcPattern2DPassReport;
  LValidation: TWfcOverlapping2DValidationReport;
  LOriginalPattern: Integer;
  LOriginalToken: TWfcModelToken;
  LSeedChangedCopy: TWfcPattern2DComposition;
begin
  LModel := CheckerModel;
  LConfig := DefaultWfcPattern2DPassConfig(LModel, CHECKER_WIDTH,
    CHECKER_HEIGHT, CHECKER_SEED);
  LPipelineA := TRejectingPatternPipeline.Create(LConfig);
  LPipelineB := TRejectingPatternPipeline.Create(LConfig);
  LBaselineA := nil;
  LBaselineB := nil;
  LFailed := nil;
  LAfterFailure := nil;
  LNextA := nil;
  LNextB := nil;
  LSeedChangedCopy := nil;
  try
    Check((not LPipelineA.TryCopyCommitted(LSeedChangedCopy,
      LValidation)) and (not Assigned(LSeedChangedCopy)),
      'copy reports no committed composition before the first success');
    Check(LPipelineA.TryGenerate(LBaselineA, LReport) and
      (LReport.Status = wpppsCompleted) and
      LPipelineA.Validate(LBaselineA, LValidation),
      'the specialized owner publishes an independently validated composition');
    LPipelineA.RejectNext := True;
    Check((not LPipelineA.Validate(LBaselineA, LValidation)) and
      (LValidation.Issue.Kind = woikProjectionToken),
      'public immutable-result validation uses the owner extension hook');
    Check(LPipelineB.TryGenerate(LBaselineB, LReport) and
      (LBaselineA.Signature = LBaselineB.Signature) and
      PatternGridsEqual(LBaselineA.CopyPatternGrid,
        LBaselineB.CopyPatternGrid) and
      TokenGridsEqual(LBaselineA.CopyProjection,
        LBaselineB.CopyProjection),
      'same model, dimensions, and seed replay exactly');

    LPipelineA.Seed := NEXT_CHECKER_SEED;
    LPipelineB.Seed := NEXT_CHECKER_SEED;
    Check(LPipelineA.TryCopyCommitted(LSeedChangedCopy, LValidation) and
      (LSeedChangedCopy.Seed = CHECKER_SEED) and
      (LSeedChangedCopy.Signature = LBaselineA.Signature) and
      PatternGridsEqual(LSeedChangedCopy.CopyPatternGrid,
        LBaselineA.CopyPatternGrid) and
      TokenGridsEqual(LSeedChangedCopy.CopyProjection,
        LBaselineA.CopyProjection),
      'changing the next seed does not relabel the committed composition');

    LDetachedPatterns := LBaselineA.CopyPatternGrid;
    LOriginalPattern := LDetachedPatterns.Patterns[0];
    LDetachedPatterns.Patterns[0] :=
      (LOriginalPattern + 1) mod LModel.PatternCount;
    LDetached := LBaselineA.CopyProjection;
    LOriginalToken := LDetached.Tokens[0];
    LDetached.Tokens[0] := 'tampered-copy';
    LDetachedPatterns := LBaselineA.CopyPatternGrid;
    LDetached := LBaselineA.CopyProjection;
    Check((LDetachedPatterns.Patterns[0] = LOriginalPattern) and
      (LDetached.Tokens[0] = LOriginalToken) and
      (LBaselineA.Signature =
        CalculateWfcPattern2DCompositionSignature(LModel, LBaselineA)),
      'pattern and projection copies are detached and the signature ' +
      'recomputes exactly');

    LPipelineA.RejectAtWrappedProjection := True;
    LPipelineA.RejectNext := True;
    Check((not LPipelineA.TryGenerate(LFailed, LReport)) and
      (not Assigned(LFailed)) and
      (LReport.Status = wpppsValidationFailed) and
      (LReport.FailedLayer = wpplProjection) and
      (LReport.Validation.Issue.X = CHECKER_WIDTH - 1) and
      (LReport.Validation.Issue.Y = CHECKER_HEIGHT - 1) and
      (LReport.Validation.Issue.PatternOffsetX = 1) and
      (LReport.Validation.Issue.PatternOffsetY = 1) and
      (LReport.Solve.FailedPassIndex = Ord(wpplProjection)) and
      (LReport.Solve.Contradiction.Kind = gckFinalValidation) and
      (LReport.Solve.Contradiction.PassIndex = Ord(wpplProjection)) and
      (LReport.Solve.Contradiction.EntryIndex = 0),
      'a seam-crossing projection rejection reports the exact public entry');
    Check(LPipelineA.TryCopyCommitted(LAfterFailure, LValidation) and
      (LAfterFailure.Seed = CHECKER_SEED) and
      (LAfterFailure.Signature = LBaselineA.Signature) and
      PatternGridsEqual(LAfterFailure.CopyPatternGrid,
        LBaselineA.CopyPatternGrid) and
      TokenGridsEqual(LAfterFailure.CopyProjection,
        LBaselineA.CopyProjection),
      'a failed new-seed attempt preserves committed state and seed identity');

    Check(LPipelineA.TryGenerate(LNextA, LReport) and
      LPipelineB.TryGenerate(LNextB, LReport) and
      (LNextA.Seed = NEXT_CHECKER_SEED) and
      (LNextB.Seed = NEXT_CHECKER_SEED) and
      (LNextA.Signature = LNextB.Signature) and
      PatternGridsEqual(LNextA.CopyPatternGrid, LNextB.CopyPatternGrid) and
      TokenGridsEqual(LNextA.CopyProjection, LNextB.CopyProjection),
      'failure restores every pass RNG stream for exact retry replay');

    WriteLn('  [INFO] checker signature ',
      WfcPattern2DCompositionSignatureHex(LBaselineA.Signature));
    Check(WfcPattern2DCompositionSignatureHex(LBaselineA.Signature) =
      EXPECTED_CHECKER_SIGNATURE,
      'the composition has the fixed cross-runtime signature golden');
    Check((Length(WfcPattern2DCompositionSignatureHex(
      LBaselineA.Signature)) = 8) and
      (Pos('@p', WfcPattern2DCompositionSignatureHex(
        LBaselineA.Signature)) = 0),
      'public signature text is fixed-width and private-key-free');
  finally
    LSeedChangedCopy.Free;
    LNextB.Free;
    LNextA.Free;
    LAfterFailure.Free;
    LFailed.Free;
    LBaselineB.Free;
    LBaselineA.Free;
    LPipelineB.Free;
    LPipelineA.Free;
    LModel.Free;
  end;
end;

begin
  WriteLn('WFC pattern-projected pass conformance suite');
  WriteLn('============================================');
  RunTest('preflight and atomic adapter application',
    TestPreflightAndAtomicity);
  RunTest('projection, seams, and downstream semantics',
    TestProjectionAndDownstreamSemantics);
  RunTest('capture pass identity', TestCapturePassIdentity);
  RunTest('tiny wrapped offset aliasing', TestTinyWrappedOffsetAliasing);
  RunTest('owner rollback, state, RNG, and signature replay',
    TestOwnerRollbackAndReplay);
  WriteLn('============================================');
  WriteLn('Checks: ', GCheckCount, '  Failures: ', GFailureCount);
  if GFailureCount <> 0 then
  begin
    {$IFDEF PAS2JS}
    raise Exception.CreateFmt('%d pattern-pass checks failed',
      [GFailureCount]);
    {$ELSE}
    Halt(1);
    {$ENDIF}
  end;
end.
