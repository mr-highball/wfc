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
unit learned_pattern_world_bundle;

{$mode delphi}{$H+}

interface

uses
  SysUtils,
  wfc,
  wfc_pipeline_model,
  wfc_pipeline_run;

const
  LEARNED_PATTERN_WORLD_BUNDLE_VERSION = 1;
  LEARNED_PATTERN_WORLD_BUNDLE_WIDTH = 8;
  LEARNED_PATTERN_WORLD_BUNDLE_HEIGHT = 6;
  LEARNED_PATTERN_WORLD_BUNDLE_DEPTH = 1;
  LEARNED_PATTERN_WORLD_BUNDLE_SEED = TGraphSeed(0);
  LEARNED_PATTERN_WORLD_BUNDLE_MAX_BACKTRACKS = 65536;
  LEARNED_PATTERN_WORLD_BUNDLE_MAX_PASS_BACKTRACKS = 0;

  LEARNED_PATTERN_WORLD_PATTERN_BYTES = 2130;
  LEARNED_PATTERN_WORLD_FOLIAGE_RULE_BYTES = 104;
  LEARNED_PATTERN_WORLD_STRUCTURE_RULE_BYTES = 90;
  LEARNED_PATTERN_WORLD_FOLIAGE_RULE_SIGNATURE = Cardinal($EC4261C3);
  LEARNED_PATTERN_WORLD_STRUCTURE_RULE_SIGNATURE = Cardinal($2E18E099);

  LEARNED_PATTERN_WORLD_RECIPE_BYTES = 6186;
  LEARNED_PATTERN_WORLD_RUN_BYTES = 327;
  LEARNED_PATTERN_WORLD_RESULT_BYTES = 3000;
  LEARNED_PATTERN_WORLD_RECIPE_SIGNATURE = Cardinal($DC2030BE);
  LEARNED_PATTERN_WORLD_RUN_SIGNATURE = Cardinal($AA80D2AA);
  LEARNED_PATTERN_WORLD_RESULT_SIGNATURE = Cardinal($5329DB78);
  LEARNED_PATTERN_WORLD_TERRAIN_HASH = Cardinal($EBBC9390);
  LEARNED_PATTERN_WORLD_FOLIAGE_HASH = Cardinal($92D4BC87);
  LEARNED_PATTERN_WORLD_STRUCTURE_HASH = Cardinal($8FA9D854);

type
  ELearnedPatternWorldBundle = class(Exception);

function CreateLearnedPatternWorldBundleRecipe: TWfcPipelineModel;
function CreateLearnedPatternWorldBundleRun(
  const ARecipe: TWfcPipelineModel): TWfcPipelineRun;

implementation

uses
  wfc_model,
  wfc_rule_model,
  wfc_rule_text,
  wfc_pattern2d,
  wfc_pattern2d_text,
  wfc_sequence,
  learned_pattern_world_demo;

function TokensOf(const AValues: array of TWfcModelToken): TWfcModelTokens;
var
  I: Integer;
begin
  Result := nil;
  SetLength(Result, Length(AValues));
  for I := 0 to Length(AValues) - 1 do
    Result[I] := AValues[I];
end;

function WeightsOf(const AValues: array of Integer): TWfcModelIntegerArray;
var
  I: Integer;
begin
  Result := nil;
  SetLength(Result, Length(AValues));
  for I := 0 to Length(AValues) - 1 do
    Result[I] := AValues[I];
end;

procedure RequireArtifact(const ACondition: Boolean;
  const AMessage: String);
begin
  if not ACondition then
    raise ELearnedPatternWorldBundle.Create(AMessage);
end;

function BuildPatternDocument: String;
var
  LPattern: TWfcOverlappingModel2D;
begin
  LPattern := LearnLearnedPatternWorldModel;
  try
    RequireArtifact(LPattern.PatternCount = 17,
      'the learned bundle pattern count changed');
    Result := EncodeWfcPattern2DText(LPattern);
    RequireArtifact(Length(Result) = LEARNED_PATTERN_WORLD_PATTERN_BYTES,
      'the learned bundle pattern artifact size changed');
  finally
    LPattern.Free;
  end;
end;

function BuildRuleDocument(const ATokens: TWfcModelTokens;
  const AWeights: TWfcModelIntegerArray; const AExpectedBytes: Integer;
  const AExpectedSignature: Cardinal; const ALabel: String): String;
var
  LModel: TWfcRuleModel;
  LRows: TWfcRuleRows;
begin
  LRows := nil;
  LModel := TWfcRuleModel.Create(2, ATokens, AWeights, LRows);
  try
    RequireArtifact(LModel.Signature = AExpectedSignature,
      ALabel + ' rule signature changed');
    Result := EncodeWfcRuleText(LModel);
    RequireArtifact(Length(Result) = AExpectedBytes,
      ALabel + ' rule artifact size changed');
  finally
    LModel.Free;
  end;
end;

function ExactRequirement(const AConsumerPassIndex: Integer;
  const AConsumerToken: TWfcModelToken;
  const AProviderToken: TWfcModelToken): TWfcPipelineRequirement;
var
  LTerms: TWfcPipelineRequirementTerms;
begin
  SetLength(LTerms, 1);
  LTerms[0] := MakeWfcPipelineRequirementTerm(0, 0, 0,
    TokensOf([AProviderToken]));
  Result := MakeWfcPipelineRequirement(AConsumerPassIndex,
    AConsumerToken, 1, wprqExact, LTerms);
end;

function CreateLearnedPatternWorldBundleRecipe: TWfcPipelineModel;
var
  LBridges: TWfcPipelineBridges;
  LDependencies: TWfcPipelineDependencies;
  LFoliageDocument: String;
  LMetadata: TWfcPipelineMetadata;
  LPasses: TWfcPipelinePasses;
  LPatternDocument: String;
  LRequirements: TWfcPipelineRequirements;
  LResources: TWfcPipelineResources;
  LStructureDocument: String;
begin
  LPatternDocument := BuildPatternDocument;
  LFoliageDocument := BuildRuleDocument(TokensOf([
    TWfcModelToken('reeds'), TWfcModelToken('grass'),
    TWfcModelToken('tree'), TWfcModelToken('moss')]),
    WeightsOf([2, 3, 1, 1]),
    LEARNED_PATTERN_WORLD_FOLIAGE_RULE_BYTES,
    LEARNED_PATTERN_WORLD_FOLIAGE_RULE_SIGNATURE, 'foliage');
  LStructureDocument := BuildRuleDocument(TokensOf([
    TWfcModelToken('dock'), TWfcModelToken('hut'),
    TWfcModelToken('mine')]), WeightsOf([1, 1, 1]),
    LEARNED_PATTERN_WORLD_STRUCTURE_RULE_BYTES,
    LEARNED_PATTERN_WORLD_STRUCTURE_RULE_SIGNATURE, 'structure');

  LMetadata := MakeWfcPipelineMetadata(
    'LearnedPatternWorld portable bundle', 'MIT',
    'project-authored bundle of the embedded LearnedPatternWorld corpus',
    'LearnedPatternWorld/pipeline-bundle-v1');

  SetLength(LResources, 3);
  LResources[0] := MakeWfcPipelineResource('terrain-patterns',
    wprkPattern2D, LPatternDocument,
    'two embedded wrapped terrain samples; footprint 2x2; symmetry D4',
    'MIT', 'LearnedPatternWorld/pattern-resource-v1');
  LResources[1] := MakeWfcPipelineResource('foliage-rules',
    wprkRules, LFoliageDocument,
    'project-authored terrain-compatible foliage vocabulary and weights',
    'MIT', 'LearnedPatternWorld/foliage-rules-v1');
  LResources[2] := MakeWfcPipelineResource('structure-rules',
    wprkRules, LStructureDocument,
    'project-authored terrain-compatible structure vocabulary',
    'MIT', 'LearnedPatternWorld/structure-rules-v1');

  SetLength(LPasses, 4);
  LPasses[0] := MakeWfcPipelinePass('patterns', wppvPrivate,
    gpmOverlay, WFC_PIPELINE_NO_INDEX, wpakPattern2D, 0,
    False, wseWhole);
  LPasses[1] := MakeWfcPipelinePass('terrain', wppvPublic,
    gpmOverlay, WFC_PIPELINE_NO_INDEX, wpakEmpty,
    WFC_PIPELINE_NO_INDEX, False, wseWhole);
  LPasses[2] := MakeWfcPipelinePass('foliage', wppvPublic,
    gpmOverlay, WFC_PIPELINE_NO_INDEX, wpakRules, 1,
    False, wseWhole);
  LPasses[3] := MakeWfcPipelinePass('structure', wppvPublic,
    gpmOverlay, WFC_PIPELINE_NO_INDEX, wpakRules, 2,
    False, wseWhole);

  SetLength(LDependencies, 3);
  LDependencies[0] := MakeWfcPipelineDependency(1, 0);
  LDependencies[1] := MakeWfcPipelineDependency(2, 1);
  LDependencies[2] := MakeWfcPipelineDependency(3, 1);

  SetLength(LBridges, 1);
  LBridges[0] := MakeWfcPipelineBridge(
    wpbkPattern2DProjection, 0, 1);

  SetLength(LRequirements, 7);
  LRequirements[0] := ExactRequirement(2, 'reeds', '~');
  LRequirements[1] := ExactRequirement(2, 'grass', '.');
  LRequirements[2] := ExactRequirement(2, 'tree', '.');
  LRequirements[3] := ExactRequirement(2, 'moss', '#');
  LRequirements[4] := ExactRequirement(3, 'dock', '~');
  LRequirements[5] := ExactRequirement(3, 'hut', '.');
  LRequirements[6] := ExactRequirement(3, 'mine', '#');

  Result := TWfcPipelineModel.Create(LMetadata, 2, True,
    rmBottomUp, LResources, LPasses, LDependencies,
    LBridges, LRequirements);
end;

function CreateLearnedPatternWorldBundleRun(
  const ARecipe: TWfcPipelineModel): TWfcPipelineRun;
var
  LLocks: TWfcPipelineCellLocks;
begin
  SetLength(LLocks, 8);
  LLocks[0] := MakeWfcPipelineCellLock(1, 0, 0, 0, '~');
  LLocks[1] := MakeWfcPipelineCellLock(1, 1, 0, 0, '~');
  LLocks[2] := MakeWfcPipelineCellLock(1, 0, 1, 0, '~');
  LLocks[3] := MakeWfcPipelineCellLock(1, 1, 1, 0, '.');
  LLocks[4] := MakeWfcPipelineCellLock(1, 4, 3, 0, '#');
  LLocks[5] := MakeWfcPipelineCellLock(1, 5, 3, 0, '.');
  LLocks[6] := MakeWfcPipelineCellLock(1, 4, 4, 0, '.');
  LLocks[7] := MakeWfcPipelineCellLock(1, 5, 4, 0, '.');
  Result := TWfcPipelineRun.Create(ARecipe,
    LEARNED_PATTERN_WORLD_BUNDLE_WIDTH,
    LEARNED_PATTERN_WORLD_BUNDLE_HEIGHT,
    LEARNED_PATTERN_WORLD_BUNDLE_DEPTH,
    LEARNED_PATTERN_WORLD_BUNDLE_SEED, wpssOneWay,
    LEARNED_PATTERN_WORLD_BUNDLE_MAX_BACKTRACKS,
    LEARNED_PATTERN_WORLD_BUNDLE_MAX_PASS_BACKTRACKS,
    False, LLocks, nil);
end;

end.
