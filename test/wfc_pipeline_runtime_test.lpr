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
program wfc_pipeline_runtime_test;

{$mode delphi}{$H+}

uses
  SysUtils,
  wfc,
  wfc_model,
  wfc_rule_model,
  wfc_rule_text,
  wfc_sequence,
  wfc_pipeline_model,
  wfc_pipeline_run,
  wfc_pipeline_result,
  wfc_pipeline_runtime;

type
  TTestProcedure = procedure;

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

function BuildAliasRecipe(const AFingerprint: TWfcModelToken):
  TWfcPipelineModel;
var
  LDependencies: TWfcPipelineDependencies;
  LDocument: String;
  LMetadata: TWfcPipelineMetadata;
  LPasses: TWfcPipelinePasses;
  LResources: TWfcPipelineResources;
  LRows: TWfcRuleRows;
  LRules: TWfcRuleModel;
begin
  LRows := nil;
  LRules := TWfcRuleModel.Create(1,
    TokensOf([TWfcModelToken('A'), TWfcModelToken('B')]),
    IntegersOf([1, 1]), LRows);
  try
    LDocument := EncodeWfcRuleText(LRules);
  finally
    LRules.Free;
  end;

  LMetadata := MakeWfcPipelineMetadata('Runtime alias fixture', 'MIT',
    'project-authored runtime fixture', AFingerprint);
  SetLength(LResources, 1);
  LResources[0] := MakeWfcPipelineResource('rules', wprkRules,
    LDocument, 'runtime rules', 'MIT', 'runtime:rules:v1');
  SetLength(LPasses, 3);
  LPasses[0] := MakeWfcPipelinePass('source', wppvPublic,
    gpmOverlay, WFC_PIPELINE_NO_INDEX, wpakRules, 0, False, wseWhole);
  LPasses[1] := MakeWfcPipelinePass('copy-one', wppvPublic,
    gpmTransform, 0, wpakEmpty, WFC_PIPELINE_NO_INDEX, False, wseWhole);
  LPasses[2] := MakeWfcPipelinePass('copy-two', wppvPublic,
    gpmTransform, 1, wpakEmpty, WFC_PIPELINE_NO_INDEX, False, wseWhole);
  SetLength(LDependencies, 2);
  LDependencies[0] := MakeWfcPipelineDependency(1, 0);
  LDependencies[1] := MakeWfcPipelineDependency(2, 1);
  Result := TWfcPipelineModel.Create(LMetadata, 1, False, rmBottomUp,
    LResources, LPasses, LDependencies, nil, nil);
end;

function BuildLongTokenRecipe: TWfcPipelineModel;
var
  LDocument: String;
  LMetadata: TWfcPipelineMetadata;
  LPasses: TWfcPipelinePasses;
  LResources: TWfcPipelineResources;
  LRows: TWfcRuleRows;
  LRules: TWfcRuleModel;
  LToken: TWfcModelToken;
begin
  LRows := nil;
  LToken := TWfcModelToken(StringOfChar('x', 100));
  LRules := TWfcRuleModel.Create(1, TokensOf([LToken]),
    IntegersOf([1]), LRows);
  try
    LDocument := EncodeWfcRuleText(LRules);
  finally
    LRules.Free;
  end;
  LMetadata := MakeWfcPipelineMetadata('Runtime budget fixture', 'MIT',
    '', 'runtime:encoded-budget:v1');
  SetLength(LResources, 1);
  LResources[0] := MakeWfcPipelineResource('long-rules', wprkRules,
    LDocument, 'runtime budget rules', 'MIT', 'runtime:long-rules:v1');
  SetLength(LPasses, 1);
  LPasses[0] := MakeWfcPipelinePass('output', wppvPublic,
    gpmOverlay, WFC_PIPELINE_NO_INDEX, wpakRules, 0, False, wseWhole);
  Result := TWfcPipelineModel.Create(LMetadata, 1, False, rmBottomUp,
    LResources, LPasses, nil, nil, nil);
end;

function BuildPassBudgetRecipe: TWfcPipelineModel;
var
  I: Integer;
  LDocument: String;
  LMetadata: TWfcPipelineMetadata;
  LPasses: TWfcPipelinePasses;
  LResources: TWfcPipelineResources;
  LRows: TWfcRuleRows;
  LRules: TWfcRuleModel;
  LVisibility: TWfcPipelinePassVisibility;
begin
  LRows := nil;
  LRules := TWfcRuleModel.Create(1, TokensOf(['A']),
    IntegersOf([1]), LRows);
  try
    LDocument := EncodeWfcRuleText(LRules);
  finally
    LRules.Free;
  end;
  LMetadata := MakeWfcPipelineMetadata('Runtime pass budget fixture',
    'MIT', '', 'runtime:pass-budget:v1');
  SetLength(LResources, 1);
  LResources[0] := MakeWfcPipelineResource('rules', wprkRules,
    LDocument, 'runtime pass budget rules', 'MIT', 'runtime:budget-rules:v1');
  SetLength(LPasses, 6);
  for I := 0 to Length(LPasses) - 1 do
  begin
    if I = 0 then
      LVisibility := wppvPublic
    else
      LVisibility := wppvPrivate;
    LPasses[I] := MakeWfcPipelinePass(
      TWfcModelToken('layer-' + IntToStr(I)), LVisibility,
      gpmOverlay, WFC_PIPELINE_NO_INDEX, wpakRules, 0, False, wseWhole);
  end;
  Result := TWfcPipelineModel.Create(LMetadata, 1, False, rmBottomUp,
    LResources, LPasses, nil, nil, nil);
end;

function BuildNegotiationRecipe: TWfcPipelineModel;
var
  LDependencies: TWfcPipelineDependencies;
  LMetadata: TWfcPipelineMetadata;
  LPasses: TWfcPipelinePasses;
  LProviderDocument: String;
  LProviderRules: TWfcRuleModel;
  LResources: TWfcPipelineResources;
  LRows: TWfcRuleRows;
  LConsumerDocument: String;
  LConsumerRules: TWfcRuleModel;
  LRequirementTerms: TWfcPipelineRequirementTerms;
  LRequirements: TWfcPipelineRequirements;
begin
  LRows := nil;
  LProviderRules := TWfcRuleModel.Create(1,
    TokensOf(['marsh', 'meadow']), IntegersOf([1, 1]), LRows);
  try
    LProviderDocument := EncodeWfcRuleText(LProviderRules);
  finally
    LProviderRules.Free;
  end;
  LConsumerRules := TWfcRuleModel.Create(1, TokensOf(['cottage']),
    IntegersOf([1]), LRows);
  try
    LConsumerDocument := EncodeWfcRuleText(LConsumerRules);
  finally
    LConsumerRules.Free;
  end;

  LMetadata := MakeWfcPipelineMetadata('Runtime negotiation fixture',
    'MIT', 'project-authored negotiation fixture',
    'runtime:negotiation-search:v1');
  SetLength(LResources, 2);
  LResources[0] := MakeWfcPipelineResource('terrain-rules', wprkRules,
    LProviderDocument, 'runtime terrain rules', 'MIT',
    'runtime:terrain-rules:v1');
  LResources[1] := MakeWfcPipelineResource('housing-rules', wprkRules,
    LConsumerDocument, 'runtime housing rules', 'MIT',
    'runtime:housing-rules:v1');
  SetLength(LPasses, 2);
  LPasses[0] := MakeWfcPipelinePass('terrain', wppvPublic,
    gpmOverlay, WFC_PIPELINE_NO_INDEX, wpakRules, 0, False, wseWhole);
  LPasses[1] := MakeWfcPipelinePass('housing', wppvPublic,
    gpmOverlay, WFC_PIPELINE_NO_INDEX, wpakRules, 1, False, wseWhole);
  SetLength(LDependencies, 1);
  LDependencies[0] := MakeWfcPipelineDependency(1, 0);
  SetLength(LRequirementTerms, 1);
  LRequirementTerms[0] := MakeWfcPipelineRequirementTerm(0, 0, 0,
    TokensOf(['meadow']));
  SetLength(LRequirements, 1);
  LRequirements[0] := MakeWfcPipelineRequirement(1, 'cottage', 0,
    wprqExact, LRequirementTerms);
  Result := TWfcPipelineModel.Create(LMetadata, 1, False, rmBottomUp,
    LResources, LPasses, LDependencies, nil, LRequirements);
end;

function BuildReversedAliasRecipe: TWfcPipelineModel;
var
  LDependencies: TWfcPipelineDependencies;
  LDocument: String;
  LMetadata: TWfcPipelineMetadata;
  LPasses: TWfcPipelinePasses;
  LResources: TWfcPipelineResources;
  LRows: TWfcRuleRows;
  LRules: TWfcRuleModel;
begin
  LRows := nil;
  LRules := TWfcRuleModel.Create(1, TokensOf(['A', 'B']),
    IntegersOf([1, 1]), LRows);
  try
    LDocument := EncodeWfcRuleText(LRules);
  finally
    LRules.Free;
  end;
  LMetadata := MakeWfcPipelineMetadata('Runtime reversed alias fixture',
    'MIT', '', 'runtime:reversed-alias:v1');
  SetLength(LResources, 1);
  LResources[0] := MakeWfcPipelineResource('rules', wprkRules,
    LDocument, 'runtime reversed alias rules', 'MIT',
    'runtime:reversed-rules:v1');
  SetLength(LPasses, 4);
  LPasses[0] := MakeWfcPipelinePass('copy-high', wppvPublic,
    gpmTransform, 3, wpakEmpty, WFC_PIPELINE_NO_INDEX, False, wseWhole);
  LPasses[1] := MakeWfcPipelinePass('copy-low', wppvPublic,
    gpmTransform, 2, wpakEmpty, WFC_PIPELINE_NO_INDEX, False, wseWhole);
  LPasses[2] := MakeWfcPipelinePass('low-source', wppvPublic,
    gpmOverlay, WFC_PIPELINE_NO_INDEX, wpakRules, 0, False, wseWhole);
  LPasses[3] := MakeWfcPipelinePass('high-source', wppvPublic,
    gpmOverlay, WFC_PIPELINE_NO_INDEX, wpakRules, 0, False, wseWhole);
  SetLength(LDependencies, 2);
  LDependencies[0] := MakeWfcPipelineDependency(0, 3);
  LDependencies[1] := MakeWfcPipelineDependency(1, 2);
  Result := TWfcPipelineModel.Create(LMetadata, 1, False, rmBottomUp,
    LResources, LPasses, LDependencies, nil, nil);
end;

function NewRun(const ARecipe: TWfcPipelineModel;
  const AWidth: Integer; const AStrategy: TWfcPipelineSolveStrategy;
  const ATrace: Boolean; const ALocks: TWfcPipelineCellLocks;
  const ADomains: TWfcPipelineCellDomains): TWfcPipelineRun;
var
  LPassLimit: Integer;
begin
  if AStrategy = wpssNegotiated then
    LPassLimit := 4
  else
    LPassLimit := 0;
  Result := TWfcPipelineRun.Create(ARecipe, AWidth, 1, 1,
    Cardinal(4294967295), AStrategy, 16, LPassLimit, ATrace,
    ALocks, ADomains);
end;

procedure CheckLayer(const AResult: TWfcPipelineResult;
  const ALayerIndex: Integer; const AExpected: array of TWfcModelToken;
  const AMessage: String);
var
  I: Integer;
  LLayer: TWfcPipelineResultLayer;
  LMatches: Boolean;
begin
  LLayer := AResult.LayerAt(ALayerIndex);
  LMatches := Length(LLayer.Tokens) = Length(AExpected);
  if LMatches then
    for I := 0 to Length(AExpected) - 1 do
      if LLayer.Tokens[I] <> AExpected[I] then
      begin
        LMatches := False;
        Break;
      end;
  Check(LMatches, AMessage);
end;

procedure TestOneWayTransformInputsAndReplay;
var
  I: Integer;
  LLocks: TWfcPipelineCellLocks;
  LRecipe: TWfcPipelineModel;
  LResult: TWfcPipelineResult;
  LReplay: TWfcPipelineResult;
  LRun: TWfcPipelineRun;
  LRuntime: TWfcPipelineRuntime;
begin
  LRecipe := BuildAliasRecipe('runtime:one-way:v1');
  try
    SetLength(LLocks, 3);
    LLocks[0] := MakeWfcPipelineCellLock(0, 0, 0, 0, 'A');
    LLocks[1] := MakeWfcPipelineCellLock(1, 1, 0, 0, 'B');
    LLocks[2] := MakeWfcPipelineCellLock(2, 2, 0, 0, 'A');
    LRun := NewRun(LRecipe, 3, wpssOneWay, True, LLocks, nil);
    try
      LRuntime := TWfcPipelineRuntime.Create(LRecipe, LRun);
      try
        LResult := LRuntime.Execute;
        try
          Check(LResult.Status = wprsSolved,
            'one-way execution produces a solved terminal result');
          Check(LResult.LayerCount = 3,
            'every public transform is captured as a result layer');
          for I := 0 to 2 do
            CheckLayer(LResult, I, ['A', 'B', 'A'],
              'transform layer ' + IntToStr(I) +
              ' exactly copies the constrained materialized source');
          Check(LResult.Seed = High(Cardinal),
            'the complete unsigned seed range reaches the result');
          Check(LResult.EvidenceKind = wpekTrace,
            'trace-enabled one-way runs retain trace evidence');

          LReplay := LRuntime.Execute;
          try
            Check(LReplay.Status = wprsSolved,
              'a prepared runtime can replay its invocation');
            Check(LReplay.Signature = LResult.Signature,
              'repeated execution is byte-semantically deterministic');
          finally
            LReplay.Free;
          end;
        finally
          LResult.Free;
        end;
      finally
        LRuntime.Free;
      end;
    finally
      LRun.Free;
    end;
  finally
    LRecipe.Free;
  end;
end;

procedure TestAliasDomainIntersection;
var
  LDomains: TWfcPipelineCellDomains;
  LLocks: TWfcPipelineCellLocks;
  LRecipe: TWfcPipelineModel;
  LResult: TWfcPipelineResult;
  LRun: TWfcPipelineRun;
begin
  LRecipe := BuildAliasRecipe('runtime:domain:v1');
  try
    SetLength(LLocks, 2);
    LLocks[0] := MakeWfcPipelineCellLock(0, 0, 0, 0, 'B');
    LLocks[1] := MakeWfcPipelineCellLock(1, 0, 0, 0, 'B');
    SetLength(LDomains, 3);
    LDomains[0] := MakeWfcPipelineCellDomain(0, 0, 0, 0,
      TokensOf(['A', 'B']));
    LDomains[1] := MakeWfcPipelineCellDomain(1, 0, 0, 0,
      TokensOf(['B']));
    LDomains[2] := MakeWfcPipelineCellDomain(2, 0, 0, 0,
      TokensOf(['B']));
    LRun := NewRun(LRecipe, 1, wpssOneWay, False, LLocks, LDomains);
    try
      LResult := ExecuteWfcPipeline(LRecipe, LRun);
      try
        Check(LResult.Status = wprsSolved,
          'identical alias locks and compatible domains solve normally');
        CheckLayer(LResult, 0, ['B'],
          'alias domains intersect on the materialized source');
        CheckLayer(LResult, 2, ['B'],
          'the domain-constrained value propagates through the transform chain');
        Check(LResult.EvidenceKind = wpekNone,
          'trace-disabled one-way runs do not synthesize evidence');
      finally
        LResult.Free;
      end;
    finally
      LRun.Free;
    end;
  finally
    LRecipe.Free;
  end;
end;

procedure TestEmptyAliasIntersectionIsContradiction;
var
  LDomains: TWfcPipelineCellDomains;
  LRecipe: TWfcPipelineModel;
  LResult: TWfcPipelineResult;
  LRun: TWfcPipelineRun;
begin
  LRecipe := BuildAliasRecipe('runtime:empty-domain:v1');
  try
    SetLength(LDomains, 2);
    LDomains[0] := MakeWfcPipelineCellDomain(0, 0, 0, 0,
      TokensOf(['A']));
    LDomains[1] := MakeWfcPipelineCellDomain(1, 0, 0, 0,
      TokensOf(['B']));
    LRun := NewRun(LRecipe, 1, wpssOneWay, True, nil, LDomains);
    try
      LResult := ExecuteWfcPipeline(LRecipe, LRun);
      try
        Check(LResult.Status = wprsContradiction,
          'an empty alias-domain intersection is a terminal contradiction');
        Check(LResult.LayerCount = 0,
          'failed executions never publish partial public layers');
        Check(LResult.CopyFailure.Kind = gckEntryDomain,
          'the core reports the effective empty caller domain');
      finally
        LResult.Free;
      end;
    finally
      LRun.Free;
    end;
  finally
    LRecipe.Free;
  end;
end;

procedure TestExplicitEmptyTransformDomain;
var
  LDomains: TWfcPipelineCellDomains;
  LRecipe: TWfcPipelineModel;
  LResult: TWfcPipelineResult;
  LRun: TWfcPipelineRun;
begin
  LRecipe := BuildAliasRecipe('runtime:explicit-empty:v1');
  try
    SetLength(LDomains, 1);
    LDomains[0] := MakeWfcPipelineCellDomain(2, 0, 0, 0, nil);
    LRun := NewRun(LRecipe, 1, wpssOneWay, False, nil, LDomains);
    try
      LResult := ExecuteWfcPipeline(LRecipe, LRun);
      try
        Check((LResult.Status = wprsContradiction) and
          (LResult.LayerCount = 0),
          'an explicit empty transform domain constrains its source');
      finally
        LResult.Free;
      end;
    finally
      LRun.Free;
    end;
  finally
    LRecipe.Free;
  end;
end;

procedure TestConflictingAliasLocksRejected;
var
  LCaught: Boolean;
  LLocks: TWfcPipelineCellLocks;
  LRecipe: TWfcPipelineModel;
  LRun: TWfcPipelineRun;
  LRuntime: TWfcPipelineRuntime;
begin
  LRecipe := BuildAliasRecipe('runtime:lock-conflict:v1');
  try
    SetLength(LLocks, 2);
    LLocks[0] := MakeWfcPipelineCellLock(0, 0, 0, 0, 'A');
    LLocks[1] := MakeWfcPipelineCellLock(2, 0, 0, 0, 'B');
    LRun := NewRun(LRecipe, 1, wpssOneWay, False, LLocks, nil);
    try
      LCaught := False;
      LRuntime := nil;
      try
        LRuntime := TWfcPipelineRuntime.Create(LRecipe, LRun);
      except
        on E: EWfcPipelineRuntime do
          LCaught := Pos('conflict after transform resolution',
            E.Message) > 0;
      end;
      LRuntime.Free;
      Check(LCaught,
        'conflicting locks on transform aliases fail before graph publication');
    finally
      LRun.Free;
    end;
  finally
    LRecipe.Free;
  end;
end;

procedure TestAliasLockDomainConflictRejected;
var
  LCaught: Boolean;
  LDomains: TWfcPipelineCellDomains;
  LLocks: TWfcPipelineCellLocks;
  LRecipe: TWfcPipelineModel;
  LRun: TWfcPipelineRun;
  LRuntime: TWfcPipelineRuntime;
begin
  LRecipe := BuildAliasRecipe('runtime:cross-conflict:v1');
  try
    SetLength(LLocks, 1);
    LLocks[0] := MakeWfcPipelineCellLock(0, 0, 0, 0, 'A');
    SetLength(LDomains, 1);
    LDomains[0] := MakeWfcPipelineCellDomain(2, 0, 0, 0,
      TokensOf(['B']));
    LRun := NewRun(LRecipe, 1, wpssOneWay, False, LLocks, LDomains);
    try
      LCaught := False;
      LRuntime := nil;
      try
        LRuntime := TWfcPipelineRuntime.Create(LRecipe, LRun);
      except
        on E: EWfcPipelineRuntime do
          LCaught := Pos('excluded by the effective domain', E.Message) > 0;
      end;
      LRuntime.Free;
      Check(LCaught,
        'lock/domain conflicts across aliases fail before graph publication');
    finally
      LRun.Free;
    end;
  finally
    LRecipe.Free;
  end;
end;

procedure TestNegotiatedExecution;
var
  LLocks: TWfcPipelineCellLocks;
  LLayer: TWfcPipelineResultLayer;
  LRecipe: TWfcPipelineModel;
  LResult: TWfcPipelineResult;
  LRun: TWfcPipelineRun;
begin
  LRecipe := BuildAliasRecipe('runtime:negotiated:v1');
  try
    SetLength(LLocks, 1);
    LLocks[0] := MakeWfcPipelineCellLock(2, 0, 0, 0, 'B');
    LRun := NewRun(LRecipe, 2, wpssNegotiated, False, LLocks, nil);
    try
      LResult := ExecuteWfcPipeline(LRecipe, LRun);
      try
        Check(LResult.Status = wprsSolved,
          'negotiated invocations use the negotiated solver path');
        Check(LResult.PassBacktracks = 0,
          'a first-attempt negotiated solution reports no pass backtracks');
        Check(LResult.EvidenceKind = wpekNegotiationTranscript,
          'negotiated results retain transcript evidence');
        LLayer := LResult.LayerAt(2);
        Check((Length(LLayer.Tokens) = 2) and
          (LLayer.Tokens[0] = 'B'),
          'negotiated execution honors a transform-targeted lock');
        CheckLayer(LResult, 2, LResult.LayerAt(0).Tokens,
          'negotiated transforms exactly copy their materialized source');
      finally
        LResult.Free;
      end;
    finally
      LRun.Free;
    end;
  finally
    LRecipe.Free;
  end;
end;

procedure TestNegotiatedOuterBacktrackingAndLimit;
var
  LLimitedResult: TWfcPipelineResult;
  LLimitedRun: TWfcPipelineRun;
  LRecipe: TWfcPipelineModel;
  LReplay: TWfcPipelineResult;
  LResult: TWfcPipelineResult;
  LRun: TWfcPipelineRun;
  LRuntime: TWfcPipelineRuntime;
begin
  LRecipe := BuildNegotiationRecipe;
  try
    LRun := TWfcPipelineRun.Create(LRecipe, 1, 1, 1, 0,
      wpssNegotiated, 1, 1, True, nil, nil);
    try
      LRuntime := TWfcPipelineRuntime.Create(LRecipe, LRun);
      try
        LResult := LRuntime.Execute;
        try
          Check((LResult.Status = wprsSolved) and
            (LResult.PassBacktracks = 1),
            'negotiation reopens an incompatible provider assignment once');
          CheckLayer(LResult, 0, ['meadow'],
            'outer pass backtracking publishes the supported provider value');
          CheckLayer(LResult, 1, ['cottage'],
            'the dependent pass publishes after provider repair');
          LReplay := LRuntime.Execute;
          try
            Check(LReplay.Signature = LResult.Signature,
              'an outer-backtracked negotiated run replays deterministically');
          finally
            LReplay.Free;
          end;
        finally
          LResult.Free;
        end;
      finally
        LRuntime.Free;
      end;
    finally
      LRun.Free;
    end;

    LLimitedRun := TWfcPipelineRun.Create(LRecipe, 1, 1, 1, 0,
      wpssNegotiated, 1, 0, False, nil, nil);
    try
      LRuntime := TWfcPipelineRuntime.Create(LRecipe, LLimitedRun);
      try
        LLimitedResult := LRuntime.Execute;
        try
          Check((LLimitedResult.Status = wprsPassBacktrackLimit) and
            (LLimitedResult.PassBacktracks = 0),
            'a zero outer budget maps to the pass-backtrack-limit result');
          Check((LLimitedResult.LayerCount = 0) and
            (LLimitedResult.EvidenceKind = wpekNegotiationTranscript),
            'pass-limit results retain transcript evidence without layers');
          LReplay := LRuntime.Execute;
          try
            Check(LReplay.Signature = LLimitedResult.Signature,
              'a pass-limit result replays deterministically');
          finally
            LReplay.Free;
          end;
        finally
          LLimitedResult.Free;
        end;
      finally
        LRuntime.Free;
      end;
    finally
      LLimitedRun.Free;
    end;
  finally
    LRecipe.Free;
  end;
end;

procedure TestReversedAliasOrderConflict;
var
  LCaught: Boolean;
  LLocks: TWfcPipelineCellLocks;
  LRecipe: TWfcPipelineModel;
  LRun: TWfcPipelineRun;
  LRuntime: TWfcPipelineRuntime;
begin
  LRecipe := BuildReversedAliasRecipe;
  try
    SetLength(LLocks, 3);
    LLocks[0] := MakeWfcPipelineCellLock(0, 0, 0, 0, 'A');
    LLocks[1] := MakeWfcPipelineCellLock(1, 0, 0, 0, 'B');
    LLocks[2] := MakeWfcPipelineCellLock(2, 0, 0, 0, 'A');
    LRun := NewRun(LRecipe, 1, wpssOneWay, False, LLocks, nil);
    try
      LCaught := False;
      LRuntime := nil;
      try
        LRuntime := TWfcPipelineRuntime.Create(LRecipe, LRun);
      except
        on E: EWfcPipelineRuntime do
          LCaught := Pos('conflict after transform resolution',
            E.Message) > 0;
      end;
      LRuntime.Free;
      Check(LCaught,
        'effective-cell sorting detects conflicts after reversed alias mapping');
    finally
      LRun.Free;
    end;
  finally
    LRecipe.Free;
  end;
end;

procedure TestDetachedResultLifetime;
var
  LLocks: TWfcPipelineCellLocks;
  LRecipe: TWfcPipelineModel;
  LResult: TWfcPipelineResult;
  LRun: TWfcPipelineRun;
  LRuntime: TWfcPipelineRuntime;
begin
  LRecipe := BuildAliasRecipe('runtime:detached-result:v1');
  SetLength(LLocks, 1);
  LLocks[0] := MakeWfcPipelineCellLock(2, 0, 0, 0, 'B');
  LRun := NewRun(LRecipe, 1, wpssOneWay, False, LLocks, nil);
  LRuntime := TWfcPipelineRuntime.Create(LRecipe, LRun);
  LResult := LRuntime.Execute;
  LRuntime.Free;
  LRun.Free;
  LRecipe.Free;
  try
    Check((LResult.Status = wprsSolved) and
      (LResult.LayerAt(2).Tokens[0] = 'B') and
      (LResult.RecipeSignature <> 0) and (LResult.RunSignature <> 0),
      'results remain self-contained after recipe, run, and runtime release');
  finally
    LResult.Free;
  end;
end;

procedure TestRecipeMismatchRejected;
var
  LCaught: Boolean;
  LRecipeOne: TWfcPipelineModel;
  LRecipeTwo: TWfcPipelineModel;
  LRun: TWfcPipelineRun;
  LRuntime: TWfcPipelineRuntime;
begin
  LRecipeOne := BuildAliasRecipe('runtime:recipe-one:v1');
  try
    LRecipeTwo := BuildAliasRecipe('runtime:recipe-two:v1');
    try
      LRun := NewRun(LRecipeOne, 1, wpssOneWay, False, nil, nil);
      try
        LCaught := False;
        LRuntime := nil;
        try
          LRuntime := TWfcPipelineRuntime.Create(LRecipeTwo, LRun);
        except
          on E: EWfcPipelineRuntime do
            LCaught := Pos('does not match the run provenance',
              E.Message) > 0;
        end;
        LRuntime.Free;
        Check(LCaught,
          'runtime construction rejects a recipe/run provenance mismatch');
      finally
        LRun.Free;
      end;
    finally
      LRecipeTwo.Free;
    end;
  finally
    LRecipeOne.Free;
  end;
end;

procedure TestOutputBudgetRejectedBeforeCompilation;
var
  LCaught: Boolean;
  LRecipe: TWfcPipelineModel;
  LRun: TWfcPipelineRun;
  LRuntime: TWfcPipelineRuntime;
begin
  LRecipe := BuildAliasRecipe('runtime:budget:v1');
  try
    LRun := NewRun(LRecipe, 1398102, wpssOneWay, False, nil, nil);
    try
      LCaught := False;
      LRuntime := nil;
      try
        LRuntime := TWfcPipelineRuntime.Create(LRecipe, LRun);
      except
        on E: EWfcPipelineRuntime do
          LCaught := Pos('public output exceeds the result cell limit',
            E.Message) > 0;
      end;
      LRuntime.Free;
      Check(LCaught,
        'result allocation limits are checked before graph compilation');
    finally
      LRun.Free;
    end;
  finally
    LRecipe.Free;
  end;
end;

procedure TestEncodedOutputBudgetRejectedBeforeCompilation;
var
  LCaught: Boolean;
  LRecipe: TWfcPipelineModel;
  LRun: TWfcPipelineRun;
  LRuntime: TWfcPipelineRuntime;
begin
  LRecipe := BuildLongTokenRecipe;
  try
    LRun := NewRun(LRecipe, 700000, wpssOneWay, False, nil, nil);
    try
      LCaught := False;
      LRuntime := nil;
      try
        LRuntime := TWfcPipelineRuntime.Create(LRecipe, LRun);
      except
        on E: EWfcPipelineRuntime do
          LCaught := Pos('can exceed the result encoded-token budget',
            E.Message) > 0;
      end;
      LRuntime.Free;
      Check(LCaught,
        'worst-case encoded result size is checked before graph compilation');
    finally
      LRun.Free;
    end;
  finally
    LRecipe.Free;
  end;
end;

procedure TestAggregatePassBudgetRejectedBeforeCompilation;
var
  LCaught: Boolean;
  LRecipe: TWfcPipelineModel;
  LRun: TWfcPipelineRun;
  LRuntime: TWfcPipelineRuntime;
begin
  LRecipe := BuildPassBudgetRecipe;
  try
    LRun := NewRun(LRecipe, 3000000, wpssOneWay, False, nil, nil);
    try
      LCaught := False;
      LRuntime := nil;
      try
        LRuntime := TWfcPipelineRuntime.Create(LRecipe, LRun);
      except
        on E: EWfcPipelineRuntime do
          LCaught := Pos('runtime pass-cell limit', E.Message) > 0;
      end;
      LRuntime.Free;
      Check(LCaught,
        'aggregate private and public pass cells are bounded before allocation');
    finally
      LRun.Free;
    end;
  finally
    LRecipe.Free;
  end;
end;

begin
  RunTest('one-way transform inputs and replay',
    TestOneWayTransformInputsAndReplay);
  RunTest('alias-domain intersection', TestAliasDomainIntersection);
  RunTest('empty alias-domain intersection',
    TestEmptyAliasIntersectionIsContradiction);
  RunTest('explicit empty transform domain',
    TestExplicitEmptyTransformDomain);
  RunTest('conflicting alias locks', TestConflictingAliasLocksRejected);
  RunTest('alias lock/domain conflict', TestAliasLockDomainConflictRejected);
  RunTest('negotiated execution', TestNegotiatedExecution);
  RunTest('negotiated outer backtracking and limit',
    TestNegotiatedOuterBacktrackingAndLimit);
  RunTest('reversed alias ordering', TestReversedAliasOrderConflict);
  RunTest('detached result lifetime', TestDetachedResultLifetime);
  RunTest('recipe mismatch', TestRecipeMismatchRejected);
  RunTest('output budget', TestOutputBudgetRejectedBeforeCompilation);
  RunTest('encoded output budget',
    TestEncodedOutputBudgetRejectedBeforeCompilation);
  RunTest('aggregate pass budget',
    TestAggregatePassBudgetRejectedBeforeCompilation);
  WriteLn('Checks: ', GCheckCount, ', Failures: ', GFailureCount);
  if GFailureCount <> 0 then
  begin
    {$IFDEF PAS2JS}
    raise Exception.CreateFmt('%d pipeline runtime checks failed',
      [GFailureCount]);
    {$ELSE}
    Halt(1);
    {$ENDIF}
  end;
end.
