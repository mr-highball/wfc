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
program wfc_pipeline_compile_test;

{$mode delphi}{$H+}

uses
  {$IFDEF PAS2JS}wfc_browser_test_host,{$ENDIF}
  SysUtils,
  wfc,
  wfc_model,
  wfc_model_text,
  wfc_learn,
  wfc_rule_model,
  wfc_rule_text,
  wfc_pattern2d,
  wfc_pattern2d_learn,
  wfc_pattern2d_text,
  wfc_sequence,
  wfc_sequence_learn,
  wfc_sequence_text,
  wfc_pipeline_model,
  wfc_pipeline_compile;

type
  TTestProcedure = procedure;

  TRecipeInputs = record
    Metadata: TWfcPipelineMetadata;
    Rank: Integer;
    WrapNeighbors: Boolean;
    RunMode: TGraphRunMode;
    Resources: TWfcPipelineResources;
    Passes: TWfcPipelinePasses;
    Dependencies: TWfcPipelineDependencies;
    Bridges: TWfcPipelineBridges;
    Requirements: TWfcPipelineRequirements;
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

function TokensOf(const AValues: array of TWfcModelToken): TWfcModelTokens;
var
  I: Integer;
begin
  Result := nil;
  SetLength(Result, Length(AValues));
  for I := 0 to Length(AValues) - 1 do
    Result[I] := AValues[I];
end;

function IntegersOf(const AValues: array of Integer): TWfcModelIntegerArray;
var
  I: Integer;
begin
  Result := nil;
  SetLength(Result, Length(AValues));
  for I := 0 to Length(AValues) - 1 do
    Result[I] := AValues[I];
end;

function NewRecipe(const AInputs: TRecipeInputs): TWfcPipelineModel;
begin
  Result := TWfcPipelineModel.Create(AInputs.Metadata,
    AInputs.Rank, AInputs.WrapNeighbors, AInputs.RunMode,
    AInputs.Resources, AInputs.Passes, AInputs.Dependencies,
    AInputs.Bridges, AInputs.Requirements);
end;

procedure BuildResourceDocuments(out AModelText, ARuleText,
  APatternText, ASequenceText: String);
var
  LModel: TWfcModel;
  LPattern: TWfcOverlappingModel2D;
  LRows: TWfcRuleRows;
  LRuleModel: TWfcRuleModel;
  LSequence: TWfcSequenceModel;
begin
  LModel := LearnModel2D(TokensOf([
    TWfcModelToken('empty'), TWfcModelToken('house'),
    TWfcModelToken('empty'), TWfcModelToken('house')]),
    2, 2, wmbWrap, wmsNone);
  try
    AModelText := EncodeWfcModelText(LModel);
  finally
    LModel.Free;
  end;

  LRows := nil;
  LRuleModel := TWfcRuleModel.Create(2,
    TokensOf([TWfcModelToken('grass'), TWfcModelToken('tree')]),
    IntegersOf([3, 1]), LRows);
  try
    ARuleText := EncodeWfcRuleText(LRuleModel);
  finally
    LRuleModel.Free;
  end;

  LPattern := LearnOverlappingModel2D(TokensOf([
    TWfcModelToken('land'), TWfcModelToken('water'),
    TWfcModelToken('land'), TWfcModelToken('water')]),
    2, 2, 1, 1, wmbWrap, wmsNone);
  try
    APatternText := EncodeWfcPattern2DText(LPattern);
  finally
    LPattern.Free;
  end;

  LSequence := LearnSequenceModel(TokensOf([
    TWfcModelToken('A'), TWfcModelToken('B'),
    TWfcModelToken('A')]), 2);
  try
    ASequenceText := EncodeWfcSequenceText(LSequence);
  finally
    LSequence.Free;
  end;
end;

function BuildPatternRecipeInputs: TRecipeInputs;
var
  LModelText: String;
  LPatternText: String;
  LRuleText: String;
  LSequenceText: String;
  LTerms: TWfcPipelineRequirementTerms;
begin
  Result := Default(TRecipeInputs);
  BuildResourceDocuments(LModelText, LRuleText, LPatternText,
    LSequenceText);
  Result.Metadata := MakeWfcPipelineMetadata('Compiled pattern fixture',
    'MIT', 'project-authored compiler fixture', 'compile:pattern:v1');
  Result.Rank := 2;
  Result.WrapNeighbors := True;
  Result.RunMode := rmBottomUp;

  SetLength(Result.Resources, 3);
  Result.Resources[0] := MakeWfcPipelineResource('patterns',
    wprkPattern2D, LPatternText, 'pattern fixture', 'MIT', 'pattern:v1');
  Result.Resources[1] := MakeWfcPipelineResource('foliage-rules',
    wprkRules, LRuleText, 'rule fixture', 'MIT', 'rules:v1');
  Result.Resources[2] := MakeWfcPipelineResource('structure-model',
    wprkModel, LModelText, 'model fixture', 'MIT', 'model:v1');

  SetLength(Result.Passes, 4);
  Result.Passes[0] := MakeWfcPipelinePass('patterns', wppvPrivate,
    gpmOverlay, WFC_PIPELINE_NO_INDEX, wpakPattern2D, 0, False, wseWhole);
  Result.Passes[1] := MakeWfcPipelinePass('terrain', wppvPublic,
    gpmOverlay, WFC_PIPELINE_NO_INDEX, wpakEmpty,
    WFC_PIPELINE_NO_INDEX, False, wseWhole);
  Result.Passes[2] := MakeWfcPipelinePass('foliage', wppvPublic,
    gpmOverlay, WFC_PIPELINE_NO_INDEX, wpakRules, 1, False, wseWhole);
  Result.Passes[3] := MakeWfcPipelinePass('structure', wppvPublic,
    gpmOverlay, WFC_PIPELINE_NO_INDEX, wpakModel, 2, False, wseWhole);

  SetLength(Result.Dependencies, 3);
  Result.Dependencies[0] := MakeWfcPipelineDependency(1, 0);
  Result.Dependencies[1] := MakeWfcPipelineDependency(2, 1);
  Result.Dependencies[2] := MakeWfcPipelineDependency(3, 1);

  SetLength(Result.Bridges, 1);
  Result.Bridges[0] := MakeWfcPipelineBridge(
    wpbkPattern2DProjection, 0, 1);

  SetLength(Result.Requirements, 2);
  SetLength(LTerms, 1);
  LTerms[0] := MakeWfcPipelineRequirementTerm(0, 0, 0,
    TokensOf([TWfcModelToken('land')]));
  Result.Requirements[0] := MakeWfcPipelineRequirement(2, 'tree', 1,
    wprqExact, LTerms);

  SetLength(LTerms, 2);
  LTerms[0] := MakeWfcPipelineRequirementTerm(-1, 0, 0,
    TokensOf([TWfcModelToken('land'), TWfcModelToken('water')]));
  LTerms[1] := MakeWfcPipelineRequirementTerm(1, 0, 0,
    TokensOf([TWfcModelToken('land')]));
  Result.Requirements[1] := MakeWfcPipelineRequirement(3, 'house', 1,
    wprqAny, LTerms);
end;

function BuildSequenceRecipeInputs: TRecipeInputs;
var
  LModelText: String;
  LPatternText: String;
  LRuleText: String;
  LSequenceText: String;
begin
  Result := Default(TRecipeInputs);
  BuildResourceDocuments(LModelText, LRuleText, LPatternText,
    LSequenceText);
  Result.Metadata := MakeWfcPipelineMetadata('Compiled sequence fixture',
    'MIT', '', 'compile:sequence:v1');
  Result.Rank := 1;
  Result.WrapNeighbors := False;
  Result.RunMode := rmTopDown;
  SetLength(Result.Resources, 1);
  Result.Resources[0] := MakeWfcPipelineResource('words', wprkSequence,
    LSequenceText, 'sequence fixture', 'MIT', 'sequence:v1');
  SetLength(Result.Passes, 3);
  Result.Passes[0] := MakeWfcPipelinePass('states', wppvPrivate,
    gpmOverlay, WFC_PIPELINE_NO_INDEX, wpakSequence, 0, True, wseWhole);
  Result.Passes[1] := MakeWfcPipelinePass('tokens', wppvPublic,
    gpmOverlay, WFC_PIPELINE_NO_INDEX, wpakEmpty,
    WFC_PIPELINE_NO_INDEX, False, wseWhole);
  Result.Passes[2] := MakeWfcPipelinePass('copy', wppvPublic,
    gpmTransform, 1, wpakEmpty, WFC_PIPELINE_NO_INDEX, False, wseWhole);
  SetLength(Result.Dependencies, 2);
  Result.Dependencies[0] := MakeWfcPipelineDependency(1, 0);
  Result.Dependencies[1] := MakeWfcPipelineDependency(2, 1);
  SetLength(Result.Bridges, 1);
  Result.Bridges[0] := MakeWfcPipelineBridge(
    wpbkSequenceProjection, 0, 1);
end;

function BuildLegacyRecipeInputs: TRecipeInputs;
var
  LDocument: String;
  LModel: TWfcRuleModel;
  LRows: TWfcRuleRows;
begin
  Result := Default(TRecipeInputs);
  LRows := nil;
  LModel := TWfcRuleModel.Create(1,
    TokensOf([TWfcModelToken('only')]), IntegersOf([1]), LRows);
  try
    LDocument := EncodeWfcRuleText(LModel);
  finally
    LModel.Free;
  end;
  Result.Metadata := MakeWfcPipelineMetadata('Compiled legacy fixture',
    'MIT', '', 'compile:legacy:v1');
  Result.Rank := 1;
  Result.WrapNeighbors := False;
  Result.RunMode := rmBottomUp;
  SetLength(Result.Resources, 1);
  Result.Resources[0] := MakeWfcPipelineResource('single-rule', wprkRules,
    LDocument, 'legacy rule fixture', 'MIT', 'rules:legacy:v1');
  SetLength(Result.Passes, 2);
  Result.Passes[0] := MakeWfcPipelinePass('first', wppvPublic,
    gpmLegacy, WFC_PIPELINE_NO_INDEX, wpakRules, 0, False, wseWhole);
  Result.Passes[1] := MakeWfcPipelinePass('second', wppvPublic,
    gpmLegacy, WFC_PIPELINE_NO_INDEX, wpakRules, 0, False, wseWhole);
  SetLength(Result.Dependencies, 1);
  Result.Dependencies[0] := MakeWfcPipelineDependency(1, 0);
end;

function BuildRequirementRecipeInputs(
  const AWrap: Boolean): TRecipeInputs;
var
  LConsumerDocument: String;
  LConsumerModel: TWfcRuleModel;
  LProviderDocument: String;
  LProviderModel: TWfcRuleModel;
  LRows: TWfcRuleRows;
  LTerms: TWfcPipelineRequirementTerms;
begin
  Result := Default(TRecipeInputs);
  LRows := nil;
  LProviderModel := TWfcRuleModel.Create(1,
    TokensOf([TWfcModelToken('A'), TWfcModelToken('B')]),
    IntegersOf([1, 1]), LRows);
  try
    LProviderDocument := EncodeWfcRuleText(LProviderModel);
  finally
    LProviderModel.Free;
  end;
  LConsumerModel := TWfcRuleModel.Create(1,
    TokensOf([TWfcModelToken('C'), TWfcModelToken('D')]),
    IntegersOf([1, 1]), LRows);
  try
    LConsumerDocument := EncodeWfcRuleText(LConsumerModel);
  finally
    LConsumerModel.Free;
  end;

  Result.Metadata := MakeWfcPipelineMetadata('Requirement validator fixture',
    'MIT', '', 'compile:requirements:v1');
  Result.Rank := 1;
  Result.WrapNeighbors := AWrap;
  Result.RunMode := rmBottomUp;
  SetLength(Result.Resources, 2);
  Result.Resources[0] := MakeWfcPipelineResource('provider-rules', wprkRules,
    LProviderDocument, 'requirement provider fixture', 'MIT', 'provider:v1');
  Result.Resources[1] := MakeWfcPipelineResource('consumer-rules', wprkRules,
    LConsumerDocument, 'requirement consumer fixture', 'MIT', 'consumer:v1');
  SetLength(Result.Passes, 2);
  Result.Passes[0] := MakeWfcPipelinePass('provider', wppvPublic,
    gpmOverlay, WFC_PIPELINE_NO_INDEX, wpakRules, 0, False, wseWhole);
  Result.Passes[1] := MakeWfcPipelinePass('consumer', wppvPublic,
    gpmOverlay, WFC_PIPELINE_NO_INDEX, wpakRules, 1, False, wseWhole);
  SetLength(Result.Dependencies, 1);
  Result.Dependencies[0] := MakeWfcPipelineDependency(1, 0);

  SetLength(Result.Requirements, 2);
  SetLength(LTerms, 1);
  LTerms[0] := MakeWfcPipelineRequirementTerm(-1, 0, 0,
    TokensOf([TWfcModelToken('A')]));
  Result.Requirements[0] := MakeWfcPipelineRequirement(1, 'C', 0,
    wprqExact, LTerms);
  SetLength(LTerms, 2);
  LTerms[0] := MakeWfcPipelineRequirementTerm(-1, 0, 0,
    TokensOf([TWfcModelToken('A')]));
  LTerms[1] := MakeWfcPipelineRequirementTerm(1, 0, 0,
    TokensOf([TWfcModelToken('B')]));
  Result.Requirements[1] := MakeWfcPipelineRequirement(1, 'D', 0,
    wprqAny, LTerms);
end;

function CompileRejected(const ARecipe: TWfcPipelineModel;
  const AWidth, AHeight, ADepth: Integer;
  const AStage: TWfcPipelineCompileStage;
  const AMessageFragment: String): Boolean;
var
  LCompiled: TWfcCompiledPipeline;
begin
  Result := False;
  LCompiled := nil;
  try
    try
      LCompiled := CompileWfcPipeline(ARecipe, AWidth, AHeight, ADepth);
    except
      on E: EWfcPipelineCompile do
      begin
        Result := (E.Stage = AStage) and
          (Pos(AMessageFragment, E.Message) > 0);
        if not Result then
          WriteLn('    unexpected compiler error: ', E.Message);
      end;
    end;
  finally
    LCompiled.Free;
  end;
end;

function HasDependency(const AGraph: TGraph;
  const AProvider: Integer): Boolean;
var
  I: Integer;
begin
  for I := 0 to AGraph.DependencyCount - 1 do
    if AGraph.DependencyIndex[I] = AProvider then
      Exit(True);
  Result := False;
end;

procedure TestCompilerTopologyAndAdapters;
var
  LCompiled: TWfcCompiledPipeline;
  LInputs: TRecipeInputs;
  LRecipe: TWfcPipelineModel;
begin
  LInputs := BuildPatternRecipeInputs;
  LRecipe := NewRecipe(LInputs);
  try
    LCompiled := CompileWfcPipeline(LRecipe, 4, 3, 1);
    try
      Check((LCompiled.Recipe = LRecipe) and Assigned(LCompiled.Graph),
        'compiled owner retains the explicit borrowed recipe and graph boundary');
      Check((LCompiled.Graph.Dimension.Width = 4) and
        (LCompiled.Graph.Dimension.Height = 3) and
        (LCompiled.Graph.Dimension.Depth = 1) and
        LCompiled.Graph.WrapNeighbors and
        (LCompiled.Graph.Mode = rmBottomUp),
        'compiler applies the validated shape and topology');
      Check((LCompiled.Graph.TotalPassCount = 4) and
        (LCompiled.Graph.CurrentPassIndex = 0) and
        (LCompiled.Graph.PassGraph[0].CurrentPass = 'patterns') and
        (LCompiled.Graph.PassGraph[3].CurrentPass = 'structure'),
        'compiler preserves contiguous pass identity and selects pass zero');
      Check((LCompiled.Graph.PassGraph[0].PassMode = gpmOverlay) and
        (LCompiled.Graph.PassGraph[1].PassMode = gpmOverlay) and
        (LCompiled.Graph.PassGraph[2].PassMode = gpmOverlay) and
        (LCompiled.Graph.PassGraph[3].PassMode = gpmOverlay),
        'compiler removes compatibility modes from declared overlay passes');
      Check((LCompiled.Graph.PassGraph[0].DependencyCount = 0) and
        (LCompiled.Graph.PassGraph[1].DependencyCount = 1) and
        HasDependency(LCompiled.Graph.PassGraph[1], 0) and
        HasDependency(LCompiled.Graph.PassGraph[2], 1) and
        HasDependency(LCompiled.Graph.PassGraph[3], 1),
        'constructed dependency surface exactly follows the recipe DAG');
      Check(LCompiled.Graph.PassGraph[0].RuleGroups.Count =
        LRecipe.BorrowPattern2DResource(0).PatternCount,
        'pattern2d adapter materializes every private pattern state');
      Check((LCompiled.Graph.PassGraph[1].RuleGroups.Count = 2) and
        (LCompiled.Graph.PassGraph[1].Rules['land'].Weight =
          WFC_DEFAULT_VALUE_WEIGHT),
        'pattern bridge materializes the public palette at neutral weight');
      Check((LCompiled.Graph.PassGraph[2].RuleGroups.Count = 2) and
        (LCompiled.Graph.PassGraph[2].Rules['grass'].Weight = 3) and
        (LCompiled.Graph.PassGraph[2].Rules['tree'].Weight = 1),
        'rules adapter preserves its complete weighted definition');
      Check((LCompiled.Graph.PassGraph[3].RuleGroups.Count = 2) and
        (LCompiled.Graph.PassGraph[3].Rules['house'].Weight = 2),
        'generic model adapter preserves learned weights');
    finally
      LCompiled.Free;
    end;
  finally
    LRecipe.Free;
  end;
end;

procedure TestPatternSolveAndRequirements;
var
  I: Integer;
  LCompiled: TWfcCompiledPipeline;
  LInputs: TRecipeInputs;
  LOptions: TGraphSolveOptions;
  LRecipe: TWfcPipelineModel;
  LReport: TGraphSolveReport;
  LTerrain: String;
begin
  LInputs := BuildPatternRecipeInputs;
  LRecipe := NewRecipe(LInputs);
  try
    LCompiled := CompileWfcPipeline(LRecipe, 4, 2, 1);
    try
      LCompiled.Graph.Seed := 0;
      LOptions := DefaultGraphSolveOptions;
      LOptions.MaxBacktracks := 64;
      Check(LCompiled.Graph.TrySolve(LOptions, LReport) and
        (LReport.Status = gssSolved),
        'compiled mixed-adapter pattern pipeline solves transactionally');
      Check(LCompiled.LastValidation.Kind = wpcvkNone,
        'successful solve passes every typed commit validator');
      Check((Length(LReport.ExecutionOrder) = 4) and
        (LReport.ExecutionOrder[0] = 0) and
        (LReport.ExecutionOrder[1] = 1) and
        (LReport.ExecutionOrder[2] = 2) and
        (LReport.ExecutionOrder[3] = 3),
        'stable topological execution uses pass-index tie breaking');
      for I := 0 to 7 do
      begin
        LTerrain := LCompiled.Graph.PassGraph[1].Entry[
          TGraphCoordinate(I mod 4), TGraphCoordinate(I div 4), 0].Value;
        Check((LTerrain = 'land') or (LTerrain = 'water'),
          'pattern projection publishes only public palette token ' +
          IntToStr(I));
        if LCompiled.Graph.PassGraph[2].Entry[
            TGraphCoordinate(I mod 4), TGraphCoordinate(I div 4), 0].Value =
            'tree' then
          Check(LTerrain = 'land',
            'compiled exact requirement constrains tree at cell ' +
            IntToStr(I));
      end;
    finally
      LCompiled.Free;
    end;
  finally
    LRecipe.Free;
  end;
end;

procedure TestSequenceBridgeAndTransform;
var
  I: Integer;
  LCompiled: TWfcCompiledPipeline;
  LInputs: TRecipeInputs;
  LOptions: TGraphSolveOptions;
  LRecipe: TWfcPipelineModel;
  LReport: TGraphSolveReport;
  LExpected: array[0..2] of String;
begin
  LExpected[0] := 'A';
  LExpected[1] := 'B';
  LExpected[2] := 'A';
  LInputs := BuildSequenceRecipeInputs;
  LRecipe := NewRecipe(LInputs);
  try
    LCompiled := CompileWfcPipeline(LRecipe, 3, 1, 1);
    try
      Check((LCompiled.Graph.Mode = rmTopDown) and
        (LCompiled.Graph.PassGraph[0].RuleGroups.Count =
          LRecipe.BorrowSequenceResource(0).StateCount),
        'sequence adapter materializes its private state model');
      Check((LCompiled.Graph.PassGraph[1].RuleGroups.Count = 2) and
        (LCompiled.Graph.PassGraph[1].Rules['A'].Weight = 1) and
        (LCompiled.Graph.PassGraph[1].Rules['B'].Weight = 1),
        'sequence projection bridge uses unit public-token weights');
      Check((LCompiled.Graph.PassGraph[2].PassMode = gpmTransform) and
        (LCompiled.Graph.PassGraph[2].TransformSourceIndex = 1) and
        (LCompiled.Graph.PassGraph[2].DependencyCount = 1) and
        HasDependency(LCompiled.Graph.PassGraph[2], 1) and
        (not LCompiled.Graph.PassGraph[2].HasDefinition),
        'definitionless transform retains exactly its declared source');

      LCompiled.Graph.Seed := 0;
      LOptions := DefaultGraphSolveOptions;
      LOptions.MaxBacktracks := 32;
      Check(LCompiled.Graph.TrySolve(LOptions, LReport),
        'compiled sequence bridge pipeline solves');
      for I := 0 to 2 do
        Check((LCompiled.Graph.PassGraph[1].Entry[I, 0, 0].Value =
          LExpected[I]) and
          (LCompiled.Graph.PassGraph[2].Entry[I, 0, 0].Value =
          LExpected[I]),
          'sequence bridge and transform publish exact token ' + IntToStr(I));
      Check(LCompiled.LastValidation.Kind = wpcvkNone,
        'sequence path and projection validators accept the committed result');
    finally
      LCompiled.Free;
    end;
  finally
    LRecipe.Free;
  end;
end;

function TransformDomainIsRejected(const AGraph: TGraph): Boolean;
begin
  Result := False;
  try
    AGraph.SwitchToPass(2);
    AGraph.SetAllowedValues(0, 0, 0, 'A');
  except
    on E: Exception do
      Result := Pos('unknown pass value', E.Message) > 0;
  end;
end;

procedure TestTransformInputBoundary;
var
  LCompiled: TWfcCompiledPipeline;
  LInputs: TRecipeInputs;
  LOptions: TGraphSolveOptions;
  LRecipe: TWfcPipelineModel;
  LReport: TGraphSolveReport;
begin
  LInputs := BuildSequenceRecipeInputs;
  LRecipe := NewRecipe(LInputs);
  try
    LCompiled := CompileWfcPipeline(LRecipe, 3, 1, 1);
    try
      Check(TransformDomainIsRejected(LCompiled.Graph),
        'core domain API exposes no registered vocabulary on a definitionless transform');

      { The source sequence is deterministically A,B,A. A conflicting target
        lock used to override transform copying because definitionless staging
        preserves caller locks before consulting the source. }
      LCompiled.Graph.PassGraph[2].Entry[0, 0, 0].Value := 'B';
      LCompiled.Graph.Seed := 0;
      LOptions := DefaultGraphSolveOptions;
      LOptions.MaxBacktracks := 32;
      Check((not LCompiled.Graph.TrySolve(LOptions, LReport)) and
        (LReport.Contradiction.Kind = gckFinalValidation) and
        (LReport.FailedPassIndex = 2) and
        (LReport.Contradiction.EntryIndex = 0),
        'commit validation prevents a transform lock from overriding source-copy semantics');
      Check((LCompiled.LastValidation.Kind = wpcvkTransform) and
        (LCompiled.LastValidation.PassIndex = 2) and
        (LCompiled.LastValidation.BridgeIndex = -1) and
        (LCompiled.LastValidation.EntryIndex = 0),
        'transform-copy rejection has stable compiler diagnostics');
      Check((not LCompiled.Graph.PassGraph[2].Entry[0, 0, 0].Empty) and
        (not LCompiled.Graph.PassGraph[2].Entry[0, 0, 0].Generated) and
        (LCompiled.Graph.PassGraph[2].Entry[0, 0, 0].Value = 'B'),
        'transform validation failure restores the original caller lock');
    finally
      LCompiled.Free;
    end;
  finally
    LRecipe.Free;
  end;
end;

procedure TestLegacyCompatibilityMode;
var
  LCompiled: TWfcCompiledPipeline;
  LInputs: TRecipeInputs;
  LOptions: TGraphSolveOptions;
  LRecipe: TWfcPipelineModel;
  LReport: TGraphSolveReport;
begin
  LInputs := BuildLegacyRecipeInputs;
  LRecipe := NewRecipe(LInputs);
  try
    LCompiled := CompileWfcPipeline(LRecipe, 2, 1, 1);
    try
      Check((LCompiled.Graph.PassGraph[0].PassMode = gpmLegacy) and
        (LCompiled.Graph.PassGraph[0].DependencyCount = 0) and
        (LCompiled.Graph.PassGraph[1].PassMode = gpmLegacy) and
        (LCompiled.Graph.PassGraph[1].DependencyCount = 1) and
        HasDependency(LCompiled.Graph.PassGraph[1], 0),
        'compiler reconstructs the legacy predecessor edge exactly');
      LCompiled.Graph.Seed := 0;
      LOptions := DefaultGraphSolveOptions;
      Check(LCompiled.Graph.TrySolve(LOptions, LReport) and
        (LCompiled.Graph.PassGraph[0].Entry[0, 0, 0].Value = 'only') and
        (LCompiled.Graph.PassGraph[1].Entry[0, 0, 0].Value = 'only'),
        'resource-backed legacy passes remain executable definitions');
    finally
      LCompiled.Free;
    end;
  finally
    LRecipe.Free;
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

procedure SetPassDomainEverywhere(const AGraph: TGraph;
  const APassIndex: Integer; const AValue: TGraphValue);
var
  X: Integer;
begin
  for X := 0 to Integer(AGraph.Dimension.Width) - 1 do
    AGraph.PassGraph[APassIndex].SetAllowedValues(X, 0, 0, AValue);
end;

procedure TestIndependentRequirementValidation;
var
  LCompiled: TWfcCompiledPipeline;
  LInputs: TRecipeInputs;
  LOptions: TGraphSolveOptions;
  LRecipe: TWfcPipelineModel;
  LReport: TGraphSolveReport;
begin
  LOptions := DefaultGraphSolveOptions;
  LOptions.MaxBacktracks := 32;

  { Remove only the core-private clause after compilation. The independent
    validator must still apply the immutable recipe and reject the open-edge
    exact requirement while restoring the generated candidate. }
  LInputs := BuildRequirementRecipeInputs(False);
  LRecipe := NewRecipe(LInputs);
  try
    LCompiled := CompileWfcPipeline(LRecipe, 3, 1, 1);
    try
      ReplaceConsumerGroup(LCompiled.Graph.PassGraph[1], 'C');
      SetPassDomainEverywhere(LCompiled.Graph, 0, 'A');
      SetPassDomainEverywhere(LCompiled.Graph, 1, 'C');
      LCompiled.Graph.Seed := 0;
      Check((not LCompiled.Graph.TrySolve(LOptions, LReport)) and
        (LReport.Contradiction.Kind = gckFinalValidation) and
        (LReport.FailedPassIndex = 1) and
        (LReport.Contradiction.EntryIndex = 0) and
        (LCompiled.LastValidation.Kind = wpcvkRequirement) and
        (LCompiled.LastValidation.RequirementIndex = 0),
        'independent exact validation treats an open negative offset as unsupported');
      Check(LCompiled.Graph.PassGraph[1].Entry[0, 0, 0].Empty,
        'exact requirement rejection rolls back the generated consumer grid');
    finally
      LCompiled.Free;
    end;
  finally
    LRecipe.Free;
  end;

  { The identical offset wraps from x=0 to the final provider cell. Stripping
    the core clause makes success evidence belong to the independent check. }
  LInputs := BuildRequirementRecipeInputs(True);
  LRecipe := NewRecipe(LInputs);
  try
    LCompiled := CompileWfcPipeline(LRecipe, 3, 1, 1);
    try
      ReplaceConsumerGroup(LCompiled.Graph.PassGraph[1], 'C');
      SetPassDomainEverywhere(LCompiled.Graph, 0, 'A');
      SetPassDomainEverywhere(LCompiled.Graph, 1, 'C');
      LCompiled.Graph.Seed := 0;
      Check(LCompiled.Graph.TrySolve(LOptions, LReport) and
        (LCompiled.LastValidation.Kind = wpcvkNone) and
        (LCompiled.Graph.PassGraph[1].Entry[0, 0, 0].Value = 'C'),
        'independent exact validation resolves the same negative offset through wrapping');
    finally
      LCompiled.Free;
    end;
  finally
    LRecipe.Free;
  end;

  { Remove the any clause and extend the provider after compilation. Neither
    wrapped neighbor emits an allowed token, so the recipe check must reject
    requirement 1 even though the mutable graph surface no longer contains it. }
  LInputs := BuildRequirementRecipeInputs(True);
  LRecipe := NewRecipe(LInputs);
  try
    LCompiled := CompileWfcPipeline(LRecipe, 3, 1, 1);
    try
      ReplaceConsumerGroup(LCompiled.Graph.PassGraph[1], 'D');
      LCompiled.Graph.PassGraph[0].AddValue('rogue');
      SetPassDomainEverywhere(LCompiled.Graph, 0, 'rogue');
      SetPassDomainEverywhere(LCompiled.Graph, 1, 'D');
      LCompiled.Graph.Seed := 0;
      Check((not LCompiled.Graph.TrySolve(LOptions, LReport)) and
        (LReport.Contradiction.Kind = gckFinalValidation) and
        (LReport.FailedPassIndex = 1) and
        (LReport.Contradiction.EntryIndex = 0) and
        (LCompiled.LastValidation.Kind = wpcvkRequirement) and
        (LCompiled.LastValidation.RequirementIndex = 1),
        'independent any validation requires at least one wrapped term to match');
      Check(LCompiled.Graph.PassGraph[1].Entry[0, 0, 0].Empty,
        'any requirement rejection rolls back the generated consumer grid');
    finally
      LCompiled.Free;
    end;
  finally
    LRecipe.Free;
  end;
end;

procedure TestFreshCompilationAndFailures;
var
  LFirst: TWfcCompiledPipeline;
  LInputs: TRecipeInputs;
  LRecipe: TWfcPipelineModel;
  LSecond: TWfcCompiledPipeline;
begin
  Check(CompileRejected(nil, 1, 1, 1, wpcsPreflight,
    'recipe cannot be nil'),
    'nil recipes fail with deterministic preflight attribution');

  LInputs := BuildSequenceRecipeInputs;
  LRecipe := NewRecipe(LInputs);
  try
    Check(CompileRejected(LRecipe, 3, 2, 1, wpcsPreflight,
      'rank-1 pipeline'),
      'rank-incompatible dimensions fail before graph publication');
    Check(CompileRejected(LRecipe, 0, 1, 1, wpcsPreflight,
      'must be positive'),
      'zero dimensions fail before graph allocation');
    Check(CompileRejected(LRecipe, High(Integer), 2, 1,
      wpcsPreflight, 'compiler limit'),
      'oversized dense shapes fail before multiplication or allocation');

    LFirst := CompileWfcPipeline(LRecipe, 3, 1, 1);
    LSecond := CompileWfcPipeline(LRecipe, 3, 1, 1);
    try
      Check((LFirst.Graph <> LSecond.Graph) and
        (LFirst.Graph.PassGraph[0] <> LSecond.Graph.PassGraph[0]),
        'each compilation owns a fresh graph and pass set');
      LFirst.Graph.PassGraph[0].Entry[0, 0, 0].Value :=
        LFirst.Graph.PassGraph[0].CopyRegisteredValues[0];
      Check(LSecond.Graph.PassGraph[0].Entry[0, 0, 0].Empty,
        'caller state never leaks between separately compiled graphs');
    finally
      LSecond.Free;
      LFirst.Free;
    end;
  finally
    LRecipe.Free;
  end;
end;

procedure TestCommitValidationRollback;
var
  LCompiled: TWfcCompiledPipeline;
  LInputs: TRecipeInputs;
  LOptions: TGraphSolveOptions;
  LRecipe: TWfcPipelineModel;
  LReport: TGraphSolveReport;
  LSolved: Boolean;
begin
  LInputs := BuildPatternRecipeInputs;
  SetLength(LInputs.Passes, 2);
  SetLength(LInputs.Dependencies, 1);
  SetLength(LInputs.Resources, 1);
  LInputs.Requirements := nil;
  LRecipe := NewRecipe(LInputs);
  try
    LCompiled := CompileWfcPipeline(LRecipe, 4, 2, 1);
    try
      LCompiled.Graph.SwitchToPass(1);
      LCompiled.Graph.AddValue('rogue');
      LCompiled.Graph.SetAllowedValues(0, 0, 0, 'rogue');
      LCompiled.Graph.Seed := 0;
      LOptions := DefaultGraphSolveOptions;
      LOptions.MaxBacktracks := 64;
      LSolved := LCompiled.Graph.TrySolve(LOptions, LReport);
      Check((not LSolved) and (LReport.Status = gssContradiction) and
        (LReport.Contradiction.Kind = gckFinalValidation) and
        (LReport.FailedPassIndex = 1) and
        (LReport.Contradiction.EntryIndex = 0),
        'typed projection rejection is attributed inside the commit boundary');
      Check((LCompiled.LastValidation.Kind = wpcvkPatternBridge) and
        (LCompiled.LastValidation.PassIndex = 1) and
        (LCompiled.LastValidation.BridgeIndex = 0) and
        (LCompiled.LastValidation.EntryIndex = 0),
        'compiled owner retains deterministic typed-validator diagnostics');
      Check(LCompiled.Graph.PassGraph[1].Entry[0, 0, 0].Empty,
        'failed final validation restores the pre-solve entry snapshot');
    finally
      LCompiled.Free;
    end;
  finally
    LRecipe.Free;
  end;
end;

begin
  WriteLn('WFC portable pipeline compiler suite');
  WriteLn('====================================');
  RunTest('topology and all pass adapters', @TestCompilerTopologyAndAdapters);
  RunTest('pattern solve and requirements', @TestPatternSolveAndRequirements);
  RunTest('sequence bridge and transform', @TestSequenceBridgeAndTransform);
  RunTest('transform run-input boundary', @TestTransformInputBoundary);
  RunTest('legacy compatibility mode', @TestLegacyCompatibilityMode);
  RunTest('independent requirement validation',
    @TestIndependentRequirementValidation);
  RunTest('fresh compilation and preflight failures',
    @TestFreshCompilationAndFailures);
  RunTest('commit validation rollback', @TestCommitValidationRollback);
  WriteLn('====================================');
  WriteLn(GCheckCount, ' checks, ', GFailureCount, ' failures');
  if GFailureCount <> 0 then
  begin
    {$IFDEF PAS2JS}
    raise Exception.CreateFmt('%d pipeline-compiler checks failed',
      [GFailureCount]);
    {$ELSE}
    Halt(1);
    {$ENDIF}
  end;
end.
