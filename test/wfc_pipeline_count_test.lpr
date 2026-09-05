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
program wfc_pipeline_count_test;

{$mode delphi}{$H+}

uses
  {$IFDEF PAS2JS}wfc_browser_test_host,{$ENDIF}
  SysUtils,
  wfc,
  wfc_model,
  wfc_rule_model,
  wfc_rule_text,
  wfc_sequence,
  wfc_pipeline_model,
  wfc_pipeline_text,
  wfc_pipeline_compile;

type
  TTestProcedure = procedure;

  TGraphValidationAccess = class(TGraph)
  public
    class function InvokeCommitValidation(const AGraph: TGraph;
      out AFailedPassIndex, AFailedEntryIndex: Integer): Boolean; static;
  end;

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

class function TGraphValidationAccess.InvokeCommitValidation(
  const AGraph: TGraph;
  out AFailedPassIndex, AFailedEntryIndex: Integer): Boolean;
begin
  { Dispatch the existing virtual hook on the actual graph. Do not add an
    instance method to a cast-only descendant: pas2js objects do not gain it. }
  Result := TGraphValidationAccess(AGraph).DoValidateCommit(
    AFailedPassIndex, AFailedEntryIndex);
end;

function RuleText(const AValues: TWfcModelTokens): String;
var
  I: Integer;
  LModel: TWfcRuleModel;
  LRows: TWfcRuleRows;
  LWeights: TWfcModelIntegerArray;
begin
  LRows := nil;
  SetLength(LWeights, Length(AValues));
  for I := 0 to Length(LWeights) - 1 do
    LWeights[I] := 1;
  LModel := TWfcRuleModel.Create(1, AValues, LWeights, LRows);
  try
    Result := EncodeWfcRuleText(LModel);
  finally
    LModel.Free;
  end;
end;

function Terms3(const AFirstAllowed, AMiddleAllowed,
  ALastAllowed: TWfcModelToken): TWfcPipelineRequirementTerms;
begin
  Result := nil;
  SetLength(Result, 3);
  Result[0] := MakeWfcPipelineRequirementTerm(-1, 0, 0,
    TokensOf([AFirstAllowed]));
  Result[1] := MakeWfcPipelineRequirementTerm(0, 0, 0,
    TokensOf([AMiddleAllowed]));
  Result[2] := MakeWfcPipelineRequirementTerm(1, 0, 0,
    TokensOf([ALastAllowed]));
end;

function NewRecipe(const ARequirement: TWfcPipelineRequirement;
  const AWrap: Boolean): TWfcPipelineModel;
var
  LBridges: TWfcPipelineBridges;
  LDependencies: TWfcPipelineDependencies;
  LPasses: TWfcPipelinePasses;
  LRequirements: TWfcPipelineRequirements;
  LResources: TWfcPipelineResources;
begin
  LBridges := nil;
  SetLength(LResources, 2);
  LResources[0] := MakeWfcPipelineResource('provider-rules', wprkRules,
    RuleText(TokensOf(['A', 'B'])), 'count provider fixture', 'MIT',
    'pipeline-count:provider:v1');
  LResources[1] := MakeWfcPipelineResource('consumer-rules', wprkRules,
    RuleText(TokensOf(['C', 'D'])), 'count consumer fixture', 'MIT',
    'pipeline-count:consumer:v1');

  SetLength(LPasses, 2);
  LPasses[0] := MakeWfcPipelinePass('provider', wppvPublic,
    gpmOverlay, WFC_PIPELINE_NO_INDEX, wpakRules, 0, False, wseWhole);
  LPasses[1] := MakeWfcPipelinePass('consumer', wppvPublic,
    gpmOverlay, WFC_PIPELINE_NO_INDEX, wpakRules, 1, False, wseWhole);
  SetLength(LDependencies, 1);
  LDependencies[0] := MakeWfcPipelineDependency(1, 0);
  SetLength(LRequirements, 1);
  LRequirements[0] := ARequirement;
  Result := TWfcPipelineModel.Create(
    MakeWfcPipelineMetadata('Count requirement fixture', 'MIT',
      'project-authored count fixture', 'pipeline-count:v1'),
    1, AWrap, rmBottomUp, LResources, LPasses, LDependencies,
    LBridges, LRequirements);
end;

function NewCountRecipe(const AWrap: Boolean;
  const ATerms: TWfcPipelineRequirementTerms;
  const AMinimum, AMaximum: Integer;
  const AMode: TGraphPassCountMode): TWfcPipelineModel;
begin
  Result := NewRecipe(MakeWfcPipelineCountRequirement(1, 'C', 0,
    ATerms, AMinimum, AMaximum, AMode), AWrap);
end;

function ModelRejected(const ARequirement: TWfcPipelineRequirement;
  const AMessageFragment: String): Boolean;
var
  LModel: TWfcPipelineModel;
begin
  Result := False;
  LModel := nil;
  try
    try
      LModel := NewRecipe(ARequirement, False);
    except
      on E: EWfcPipelineModel do
        Result := Pos(AMessageFragment, E.Message) > 0;
    end;
  finally
    LModel.Free;
  end;
end;

function DecodeRejected(const AText,
  AMessageFragment: String): Boolean;
var
  LModel: TWfcPipelineModel;
begin
  Result := False;
  LModel := nil;
  try
    try
      LModel := DecodeWfcPipelineModelText(AText);
    except
      on E: EConvertError do
        Result := Pos(AMessageFragment, E.Message) > 0;
    end;
  finally
    LModel.Free;
  end;
end;

function ReplaceOnce(const AText, AOld, ANew: String): String;
var
  LPosition: Integer;
begin
  LPosition := Pos(AOld, AText);
  if LPosition = 0 then
    raise Exception.Create('replacement fixture was not found: ' + AOld);
  Result := Copy(AText, 1, LPosition - 1) + ANew +
    Copy(AText, LPosition + Length(AOld),
      Length(AText) - LPosition - Length(AOld) + 1);
end;

{$PUSH}{$R-}
function InvalidCountMode: TGraphPassCountMode;
var
  LValue: Integer;
begin
  LValue := Ord(High(TGraphPassCountMode)) + 1;
  Result := TGraphPassCountMode(LValue);
end;
{$POP}

procedure TestImmutableModel;
var
  LAccessor: TWfcPipelineRequirement;
  LCleanExact: TWfcPipelineRequirement;
  LCleanModel: TWfcPipelineModel;
  LCount: TWfcPipelineRequirement;
  LCountModel: TWfcPipelineModel;
  LModeModel: TWfcPipelineModel;
  LPoisonedExact: TWfcPipelineRequirement;
  LPoisonedModel: TWfcPipelineModel;
  LTerms: TWfcPipelineRequirementTerms;
begin
  Check((WFC_PASS_COUNT_VERSION = 1) and
    (Ord(wprqExact) = 0) and (Ord(wprqAny) = 1) and
    (Ord(wprqCount) = 2),
    'count support is independently versioned and appends the IR kind');

  LTerms := Terms3('A', 'B', 'A');
  LCount := MakeWfcPipelineCountRequirement(1, 'C', 0, LTerms,
    1, 2, gpcmMatchingTerms);
  LCountModel := NewRecipe(LCount, True);
  try
    LAccessor := LCountModel.RequirementAt(0);
    Check((LAccessor.Kind = wprqCount) and
      (LAccessor.CountMode = gpcmMatchingTerms) and
      (LAccessor.MinimumCount = 1) and
      (LAccessor.MaximumCount = 2) and
      (Length(LAccessor.Terms) = 3),
      'immutable IR retains explicit count mode and inclusive bounds');
    LTerms[0].OffsetX := 99;
    LCount.Terms[1].AllowedProviderTokens[0] := 'A';
    Check((LCountModel.RequirementAt(0).Terms[0].OffsetX = -1) and
      (LCountModel.RequirementAt(0).Terms[1].AllowedProviderTokens[0] = 'B'),
      'constructor detaches caller-owned count terms and token sets');
    LAccessor.MinimumCount := 0;
    LAccessor.Terms[0].AllowedProviderTokens[0] := 'B';
    LAccessor := LCountModel.RequirementAt(0);
    Check((LAccessor.MinimumCount = 1) and
      (LAccessor.Terms[0].AllowedProviderTokens[0] = 'A'),
      'requirement accessors return detached count records');

    LModeModel := NewCountRecipe(True, Terms3('A', 'B', 'A'),
      1, 2, gpcmDistinctCells);
    try
      Check(LModeModel.Signature <> LCountModel.Signature,
        'count mode contributes to recipe identity');
    finally
      LModeModel.Free;
    end;
    LModeModel := NewCountRecipe(True, Terms3('A', 'B', 'A'),
      0, 2, gpcmMatchingTerms);
    try
      Check(LModeModel.Signature <> LCountModel.Signature,
        'count bounds contribute to recipe identity');
    finally
      LModeModel.Free;
    end;
    Check(WfcPipelineSignatureHex(LCountModel.Signature) = 'CC378AC5',
      'canonical count recipe identity is pinned');
  finally
    LCountModel.Free;
  end;

  SetLength(LTerms, 1);
  LTerms[0] := MakeWfcPipelineRequirementTerm(0, 0, 0,
    TokensOf(['A']));
  LCleanExact := MakeWfcPipelineRequirement(1, 'C', 0,
    wprqExact, LTerms);
  LPoisonedExact := LCleanExact;
  LPoisonedExact.CountMode := InvalidCountMode;
  LPoisonedExact.MinimumCount := Low(Integer);
  LPoisonedExact.MaximumCount := High(Integer);
  LCleanModel := NewRecipe(LCleanExact, False);
  try
    LPoisonedModel := NewRecipe(LPoisonedExact, False);
    try
      LAccessor := LPoisonedModel.RequirementAt(0);
      Check((LAccessor.CountMode = gpcmMatchingTerms) and
        (LAccessor.MinimumCount = 0) and
        (LAccessor.MaximumCount = 0),
        'legacy kinds normalize unused raw count fields before storage');
      Check((LPoisonedModel.Signature = LCleanModel.Signature) and
        (EncodeWfcPipelineModelText(LPoisonedModel) =
          EncodeWfcPipelineModelText(LCleanModel)),
        'unused count fields cannot change legacy signatures or wire text');
    finally
      LPoisonedModel.Free;
    end;
  finally
    LCleanModel.Free;
  end;
end;

procedure TestModelValidation;
var
  LRejected: Boolean;
  LRequirement: TWfcPipelineRequirement;
  LTerms: TWfcPipelineRequirementTerms;
begin
  LRejected := False;
  try
    MakeWfcPipelineRequirement(1, 'C', 0, wprqCount,
      Terms3('A', 'A', 'A'));
  except
    on E: EWfcPipelineModel do
      LRejected := Pos('MakeWfcPipelineCountRequirement', E.Message) > 0;
  end;
  Check(LRejected,
    'the generic helper rejects count records without explicit bounds');

  LTerms := nil;
  LRequirement := MakeWfcPipelineCountRequirement(1, 'C', 0,
    LTerms, 0, 0, gpcmMatchingTerms);
  Check(ModelRejected(LRequirement, 'at least one term'),
    'count requirements reject an empty declared term set');

  LTerms := Terms3('A', 'A', 'A');
  LRequirement := MakeWfcPipelineCountRequirement(1, 'C', 0,
    LTerms, -1, 2, gpcmMatchingTerms);
  Check(ModelRejected(LRequirement, '0 <= minimum'),
    'count requirements reject negative lower bounds');
  LRequirement := MakeWfcPipelineCountRequirement(1, 'C', 0,
    LTerms, 2, 1, gpcmMatchingTerms);
  Check(ModelRejected(LRequirement, '0 <= minimum'),
    'count requirements reject inverted bounds');
  LRequirement := MakeWfcPipelineCountRequirement(1, 'C', 0,
    LTerms, 0, 4, gpcmMatchingTerms);
  Check(ModelRejected(LRequirement, 'maximum <= 3'),
    'both modes bound maxima by the distinct declared offset count');
  LRequirement := MakeWfcPipelineCountRequirement(1, 'C', 0,
    LTerms, 0, 1, InvalidCountMode);
  Check(ModelRejected(LRequirement, 'count mode is unknown'),
    'unknown count modes fail closed in immutable construction');

  LTerms[1] := LTerms[0];
  LRequirement := MakeWfcPipelineCountRequirement(1, 'C', 0,
    LTerms, 0, 1, gpcmMatchingTerms);
  Check(ModelRejected(LRequirement, 'strict X/Y/Z order'),
    'count terms require canonical unique ordered offsets');

  LTerms := Terms3('A', 'A', 'A');
  LTerms[1].OffsetY := 1;
  LRequirement := MakeWfcPipelineCountRequirement(1, 'C', 0,
    LTerms, 0, 1, gpcmMatchingTerms);
  Check(ModelRejected(LRequirement, 'inactive rank-1 axis'),
    'count terms reject inactive rank axes');

  LTerms := Terms3('A', 'A', 'A');
  LTerms[1].AllowedProviderTokens := TokensOf(['B', 'A']);
  LRequirement := MakeWfcPipelineCountRequirement(1, 'C', 0,
    LTerms, 0, 1, gpcmMatchingTerms);
  Check(ModelRejected(LRequirement, 'provider-vocabulary order'),
    'count token sets retain strict provider-vocabulary ordering');
end;

procedure TestCanonicalText;
var
  LCellsModel: TWfcPipelineModel;
  LDecoded: TWfcPipelineModel;
  LModel: TWfcPipelineModel;
  LText: String;
begin
  LModel := NewCountRecipe(True, Terms3('A', 'B', 'A'),
    1, 2, gpcmMatchingTerms);
  try
    LText := EncodeWfcPipelineModelText(LModel);
    Check(Pos('requirement=0,1,C,0,count-terms-v1,3'#10 +
      'count=0,1,2'#10 + 'term=0,0,-1,0,0,1'#10, LText) > 0,
      'term-count wire kind carries one immediate indexed bounds record');
    LDecoded := DecodeWfcPipelineModelText(LText);
    try
      Check((LDecoded.Signature = LModel.Signature) and
        (EncodeWfcPipelineModelText(LDecoded) = LText),
        'count recipe text round-trips byte identically');
    finally
      LDecoded.Free;
    end;

    Check(DecodeRejected(ReplaceOnce(LText, 'count-terms-v1',
      'count-terms-v2'), 'unknown kind'),
      'unknown count wire revisions fail closed');
    Check(DecodeRejected(ReplaceOnce(LText, 'count=0,1,2'#10, ''),
      'requirement count record'),
      'a count requirement must carry exactly one immediate count record');
    Check(DecodeRejected(ReplaceOnce(LText, 'count=0,1,2'#10,
      'count=0,1,2'#10 + 'count=0,1,2'#10), 'term record'),
      'a count requirement rejects a duplicate bounds record');
    Check(DecodeRejected(ReplaceOnce(LText, 'count=0,1,2',
      'count=1,1,2'), 'parent index'),
      'count record parent indices are canonical and contiguous');
    Check(DecodeRejected(ReplaceOnce(LText, 'count=0,1,2',
      'count=0,01,2'), 'minimum count'),
      'count bounds reject noncanonical integer syntax');
    Check(DecodeRejected(ReplaceOnce(LText, 'count=0,1,2',
      'count=0,1,4'), 'maximum <= 3'),
      'decoded count bounds are revalidated against declared offsets');
  finally
    LModel.Free;
  end;

  LCellsModel := NewCountRecipe(True, Terms3('A', 'B', 'A'),
    1, 2, gpcmDistinctCells);
  try
    Check(Pos(',count-cells-v1,3'#10 + 'count=0,1,2'#10,
      EncodeWfcPipelineModelText(LCellsModel)) > 0,
      'distinct-cell mode has a separate versioned wire kind');
  finally
    LCellsModel.Free;
  end;

  LModel := NewRecipe(MakeWfcPipelineRequirement(1, 'C', 0,
    wprqExact, Copy(Terms3('A', 'A', 'A'), 1, 1)), False);
  try
    LText := EncodeWfcPipelineModelText(LModel);
    Check(DecodeRejected(ReplaceOnce(LText,
      'term=0,0,0,0,0,1'#10,
      'count=0,0,1'#10 + 'term=0,0,0,0,0,1'#10), 'term record'),
      'legacy wire kinds cannot carry a count record');
  finally
    LModel.Free;
  end;
end;

procedure SetPassValues(const AGraph: TGraph;
  const APassIndex: Integer; const AValues: array of TGraphValue);
var
  X: Integer;
begin
  for X := 0 to Length(AValues) - 1 do
    AGraph.PassGraph[APassIndex].SetAllowedValues(X, 0, 0, AValues[X]);
end;

function LockedSolve(const ARecipe: TWfcPipelineModel;
  const AProvider, AConsumer: array of TGraphValue;
  out AReport: TGraphSolveReport;
  out AValidation: TWfcPipelineCommitValidation): Boolean;
var
  LCompiled: TWfcCompiledPipeline;
  LOptions: TGraphSolveOptions;
begin
  Result := False;
  LCompiled := CompileWfcPipeline(ARecipe, Length(AProvider), 1, 1);
  try
    SetPassValues(LCompiled.Graph, 0, AProvider);
    SetPassValues(LCompiled.Graph, 1, AConsumer);
    LCompiled.Graph.Seed := 0;
    LOptions := DefaultGraphSolveOptions;
    LOptions.MaxBacktracks := 32;
    Result := LCompiled.Graph.TrySolve(LOptions, AReport);
    AValidation := LCompiled.LastValidation;
  finally
    LCompiled.Free;
  end;
end;

procedure TestCompiledSemantics;
var
  LModel: TWfcPipelineModel;
  LReport: TGraphSolveReport;
  LValidation: TWfcPipelineCommitValidation;
begin
  LModel := NewCountRecipe(True, Terms3('A', 'A', 'A'),
    2, 2, gpcmMatchingTerms);
  try
    Check(LockedSolve(LModel, ['A', 'A', 'B'], ['C', 'C', 'C'],
      LReport, LValidation) and (LValidation.Kind = wpcvkNone),
      'compiled term mode accepts an inclusive exact count');
    Check(not LockedSolve(LModel, ['A', 'A', 'A'], ['C', 'C', 'C'],
      LReport, LValidation),
      'compiled upper count rejects excess matching terms');
  finally
    LModel.Free;
  end;

  LModel := NewCountRecipe(True, Terms3('A', 'A', 'A'),
    3, 3, gpcmMatchingTerms);
  try
    Check(LockedSolve(LModel, ['A'], ['C'], LReport, LValidation),
      'term mode counts all declared offsets after wrapped aliasing');
  finally
    LModel.Free;
  end;

  LModel := NewCountRecipe(True, Terms3('B', 'A', 'A'),
    1, 1, gpcmDistinctCells);
  try
    Check(LockedSolve(LModel, ['A'], ['C'], LReport, LValidation),
      'cell mode unions matching aliases and ignores a nonmatching alias');
  finally
    LModel.Free;
  end;

  LModel := NewCountRecipe(True, Terms3('A', 'A', 'A'),
    3, 3, gpcmDistinctCells);
  try
    Check(LockedSolve(LModel, ['A', 'A', 'A'], ['C', 'C', 'C'],
      LReport, LValidation) and (LValidation.Kind = wpcvkNone),
      'independent cell mode counts several different provider cells');
  finally
    LModel.Free;
  end;

  LModel := NewCountRecipe(True, Terms3('A', 'A', 'A'),
    2, 3, gpcmDistinctCells);
  try
    Check(not LockedSolve(LModel, ['A'], ['C'], LReport, LValidation),
      'wrapped aliasing can make a valid declared lower bound unsatisfiable');
  finally
    LModel.Free;
  end;

  LModel := NewCountRecipe(False, Terms3('A', 'A', 'A'),
    2, 2, gpcmMatchingTerms);
  try
    Check(LockedSolve(LModel, ['A', 'A', 'A'], ['C', 'D', 'D'],
      LReport, LValidation),
      'open out-of-bounds terms count as false at an edge');
  finally
    LModel.Free;
  end;
end;

procedure ReplaceConsumerGroup(const AGraph: TGraph;
  const AValue: TGraphValue);
var
  LReplacement: TGraphRuleGroup;
begin
  AGraph.RuleGroups.Remove(AValue);
  LReplacement := TGraphRuleGroup.Create(AValue);
  try
    AGraph.RuleGroups.Add(AValue, LReplacement);
    LReplacement := nil;
  finally
    LReplacement.Free;
  end;
end;

procedure TestIndependentUpperValidation;
var
  LCompiled: TWfcCompiledPipeline;
  LFailedEntryIndex: Integer;
  LFailedPassIndex: Integer;
  LModel: TWfcPipelineModel;
  LOptions: TGraphSolveOptions;
  LReport: TGraphSolveReport;
begin
  LModel := NewCountRecipe(True, Terms3('A', 'A', 'A'),
    0, 2, gpcmMatchingTerms);
  try
    LCompiled := CompileWfcPipeline(LModel, 1, 1, 1);
    try
      { Corrupt only the compiled core surface. The immutable recipe must
        still reject the candidate at the rollback-capable boundary. }
      ReplaceConsumerGroup(LCompiled.Graph.PassGraph[1], 'C');
      SetPassValues(LCompiled.Graph, 0, ['A']);
      SetPassValues(LCompiled.Graph, 1, ['C']);
      LCompiled.Graph.Seed := 0;
      LOptions := DefaultGraphSolveOptions;
      LOptions.MaxBacktracks := 8;
      Check((not LCompiled.Graph.TrySolve(LOptions, LReport)) and
        (LReport.Contradiction.Kind = gckFinalValidation) and
        (LReport.FailedPassIndex = 1) and
        (LReport.Contradiction.EntryIndex = 0) and
        (LCompiled.LastValidation.Kind = wpcvkRequirement) and
        (LCompiled.LastValidation.RequirementIndex = 0),
        'independent commit validation rejects a corrupted upper-count clause');
      Check(LCompiled.Graph.PassGraph[1].Entry[0, 0, 0].Empty,
        'independent count rejection rolls back the generated consumer');
    finally
      LCompiled.Free;
    end;
  finally
    LModel.Free;
  end;

  LModel := NewCountRecipe(True, Terms3('A', 'A', 'A'),
    0, 2, gpcmDistinctCells);
  try
    LCompiled := CompileWfcPipeline(LModel, 3, 1, 1);
    try
      ReplaceConsumerGroup(LCompiled.Graph.PassGraph[1], 'C');
      SetPassValues(LCompiled.Graph, 0, ['A', 'A', 'A']);
      SetPassValues(LCompiled.Graph, 1, ['C', 'C', 'C']);
      LOptions := DefaultGraphSolveOptions;
      LOptions.MaxBacktracks := 8;
      Check((not LCompiled.Graph.TrySolve(LOptions, LReport)) and
        (LReport.Contradiction.Kind = gckFinalValidation) and
        (LCompiled.LastValidation.Kind = wpcvkRequirement),
        'independent cell upper bound rejects three distinct matches');
      Check(LCompiled.Graph.PassGraph[1].Entry[0, 0, 0].Empty and
        LCompiled.Graph.PassGraph[1].Entry[1, 0, 0].Empty and
        LCompiled.Graph.PassGraph[1].Entry[2, 0, 0].Empty,
        'independent distinct-cell rejection rolls back every consumer');
    finally
      LCompiled.Free;
    end;
  finally
    LModel.Free;
  end;

  LModel := NewCountRecipe(False, Terms3('A', 'A', 'A'),
    1, 3, gpcmMatchingTerms);
  try
    LCompiled := CompileWfcPipeline(LModel, 1, 1, 1);
    try
      LCompiled.Graph.PassGraph[1].Entry[0, 0, 0].Value := 'C';
      Check((not TGraphValidationAccess.InvokeCommitValidation(
          LCompiled.Graph, LFailedPassIndex, LFailedEntryIndex)) and
        (LFailedPassIndex = 1) and (LFailedEntryIndex = 0) and
        (LCompiled.LastValidation.Kind = wpcvkRequirement),
        'independent count evaluation treats an empty provider entry as false');
    finally
      LCompiled.Free;
    end;
  finally
    LModel.Free;
  end;
end;

begin
  WriteLn('WFC portable pipeline count-requirement suite');
  WriteLn('=============================================');
  RunTest('immutable IR and legacy normalization', @TestImmutableModel);
  RunTest('count model validation', @TestModelValidation);
  RunTest('canonical count text', @TestCanonicalText);
  RunTest('compiled count semantics', @TestCompiledSemantics);
  RunTest('independent upper-count validation',
    @TestIndependentUpperValidation);
  WriteLn('=============================================');
  WriteLn(GCheckCount, ' checks, ', GFailureCount, ' failures');
  if GFailureCount <> 0 then
  begin
    {$IFDEF PAS2JS}
    raise Exception.CreateFmt('%d pipeline-count checks failed',
      [GFailureCount]);
    {$ELSE}
    Halt(1);
    {$ENDIF}
  end;
end.
