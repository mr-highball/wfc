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
program wfc_pipeline_model_test;

{$mode delphi}{$H+}

uses
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
  wfc_pipeline_model;

type
  TTestProcedure = procedure;

  TRecipeInputs = record
    Metadata: TWfcPipelineMetadata;
    Versions: TWfcPipelineVersions;
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

function MusicalNoteToken: TWfcModelToken;
begin
  {$IFDEF PAS2JS}
  Result := TWfcModelToken(Chr($266B));
  {$ELSE}
  Result := TWfcModelToken(UTF8Encode(
    UnicodeString(WideChar($266B))));
  {$ENDIF}
end;

function NewRecipe(const AInputs: TRecipeInputs): TWfcPipelineModel;
begin
  Result := TWfcPipelineModel.Create(AInputs.Metadata, AInputs.Versions,
    AInputs.Rank, AInputs.WrapNeighbors, AInputs.RunMode,
    AInputs.Resources, AInputs.Passes, AInputs.Dependencies,
    AInputs.Bridges, AInputs.Requirements);
end;

function RecipeRejected(const AInputs: TRecipeInputs;
  const AExpectedFragment: String): Boolean;
var
  LRecipe: TWfcPipelineModel;
begin
  Result := False;
  LRecipe := nil;
  try
    try
      LRecipe := NewRecipe(AInputs);
    except
      on E: Exception do
      begin
        Result := (AExpectedFragment = '') or
          (Pos(AExpectedFragment, E.Message) > 0);
        if not Result then
          WriteLn('    unexpected error: ', E.Message);
      end;
    end;
  finally
    LRecipe.Free;
  end;
end;

function InvalidResourceKind: TWfcPipelineResourceKind;
var
  LOrdinal: Integer;
begin
  LOrdinal := 99;
  {$PUSH}{$R-}
  Result := TWfcPipelineResourceKind(LOrdinal);
  {$POP}
end;

function InvalidAdapterKind: TWfcPipelineAdapterKind;
var
  LOrdinal: Integer;
begin
  LOrdinal := 99;
  {$PUSH}{$R-}
  Result := TWfcPipelineAdapterKind(LOrdinal);
  {$POP}
end;

function InvalidRunMode: TGraphRunMode;
var
  LOrdinal: Integer;
begin
  LOrdinal := 99;
  {$PUSH}{$R-}
  Result := TGraphRunMode(LOrdinal);
  {$POP}
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

function ReservedPatternKeyDocument: String;
var
  LPattern: TWfcOverlappingModel2D;
begin
  LPattern := LearnOverlappingModel2D(
    TokensOf([TWfcModelToken('@p0')]), 1, 1, 1, 1,
    wmbWrap, wmsNone);
  try
    Result := EncodeWfcPattern2DText(LPattern);
  finally
    LPattern.Free;
  end;
end;

function MaximumStateSequenceDocument: String;
var
  I: Integer;
  LModel: TWfcSequenceModel;
  LTokens: TWfcModelTokens;
begin
  SetLength(LTokens, WFC_SEQUENCE_MAX_STATE_COUNT);
  for I := 0 to Length(LTokens) - 1 do
    LTokens[I] := TWfcModelToken('state-') + IntToStr(I);
  LModel := LearnSequenceModel(LTokens, 1);
  try
    if LModel.StateCount <> WFC_SEQUENCE_MAX_STATE_COUNT then
      raise Exception.Create('maximum-state sequence fixture is incomplete');
    Result := EncodeWfcSequenceText(LModel);
  finally
    LModel.Free;
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
  Result.Metadata := MakeWfcPipelineMetadata(
    TWfcModelToken('Learned pattern ') + MusicalNoteToken,
    'MIT', 'project-authored conformance fixture', 'fixture:v1');
  Result.Versions := CurrentWfcPipelineVersions;
  Result.Rank := 2;
  Result.WrapNeighbors := True;
  Result.RunMode := rmBottomUp;

  SetLength(Result.Resources, 4);
  Result.Resources[0] := MakeWfcPipelineResource('patterns',
    wprkPattern2D, LPatternText, 'pattern fixture', 'MIT', 'pattern:v1');
  Result.Resources[1] := MakeWfcPipelineResource('foliage-rules',
    wprkRules, LRuleText, 'rule fixture', 'MIT', 'rules:v1');
  Result.Resources[2] := MakeWfcPipelineResource('structure-model',
    wprkModel, LModelText, 'model fixture', 'MIT', 'model:v1');
  Result.Resources[3] := MakeWfcPipelineResource('sequence-unused',
    wprkSequence, LSequenceText, 'sequence fixture', 'MIT', 'sequence:v1');

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
  Result.Metadata := MakeWfcPipelineMetadata('Sequence bundle', 'MIT',
    '', '');
  Result.Versions := CurrentWfcPipelineVersions;
  Result.Rank := 1;
  Result.WrapNeighbors := False;
  Result.RunMode := rmTopDown;
  SetLength(Result.Resources, 1);
  Result.Resources[0] := MakeWfcPipelineResource('words', wprkSequence,
    LSequenceText, 'sequence fixture', 'MIT', '');
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
  Result.Requirements := nil;
end;

procedure TestCompleteRecipeAndOwnership;
const
  EXPECTED_SIGNATURE = '2B49D38A';
var
  LCopyRequirements: TWfcPipelineRequirements;
  LCopyVocabulary: TWfcModelTokens;
  LDifferent: TWfcPipelineModel;
  LInputs: TRecipeInputs;
  LRecipe: TWfcPipelineModel;
  LSecond: TWfcPipelineModel;
begin
  LInputs := BuildPatternRecipeInputs;
  LRecipe := NewRecipe(LInputs);
  try
    Check((LRecipe.ResourceCount = 4) and (LRecipe.PassCount = 4) and
      (LRecipe.DependencyCount = 3) and (LRecipe.BridgeCount = 1) and
      (LRecipe.RequirementCount = 2),
      'recipe retains every ordered definition family');
    Check((LRecipe.Rank = 2) and LRecipe.WrapNeighbors and
      (LRecipe.RunMode = rmBottomUp),
      'recipe retains portable topology metadata');
    Check(Pos(MusicalNoteToken, LRecipe.CopyMetadata.Name) > 0,
      'portable metadata retains Unicode scalar values');
    Check((LRecipe.FindResource('foliage-rules') = 1) and
      (LRecipe.FindPass('structure') = 3),
      'stable ids and labels resolve to their contiguous indices');
    Check((LRecipe.BorrowPattern2DResource(0).PaletteCount = 2) and
      (LRecipe.BorrowRuleResource(1).FindToken('tree') = 1) and
      (LRecipe.BorrowModelResource(2).FindToken('house') = 1) and
      (LRecipe.BorrowSequenceResource(3).FindPublicToken('B') = 1),
      'borrowed typed resources expose the strictly decoded immutable owners');
    Check(WfcPipelineSignatureHex(LRecipe.Signature) = EXPECTED_SIGNATURE,
      'complete semantic recipe signature is pinned [' +
      WfcPipelineSignatureHex(LRecipe.Signature) + ']');

    LInputs.Resources[0].Id := 'changed-resource';
    LInputs.Passes[1].LabelName := 'changed-pass';
    LInputs.Dependencies[0].ProviderPassIndex := 3;
    LInputs.Bridges[0].SourcePassIndex := 3;
    LInputs.Requirements[0].Terms[0].AllowedProviderTokens[0] := 'water';
    Check((LRecipe.ResourceAt(0).Id = 'patterns') and
      (LRecipe.PassAt(1).LabelName = 'terrain') and
      (LRecipe.DependencyAt(0).ProviderPassIndex = 0) and
      (LRecipe.BridgeAt(0).SourcePassIndex = 0) and
      (LRecipe.RequirementAt(0).Terms[0].AllowedProviderTokens[0] = 'land'),
      'constructor deep-copies every caller-managed input layer');

    LCopyRequirements := LRecipe.CopyRequirements;
    LCopyRequirements[0].Terms[0].AllowedProviderTokens[0] := 'water';
    LCopyVocabulary := LRecipe.CopyPublicVocabulary(1);
    LCopyVocabulary[0] := 'changed';
    Check((LRecipe.RequirementAt(0).Terms[0].AllowedProviderTokens[0] =
      'land') and (LRecipe.CopyPublicVocabulary(1)[0] = 'land'),
      'copy accessors detach nested token arrays and public vocabularies');
  finally
    LRecipe.Free;
  end;

  LInputs := BuildPatternRecipeInputs;
  LRecipe := NewRecipe(LInputs);
  LSecond := NewRecipe(LInputs);
  LInputs.Metadata.SourceFingerprint := 'fixture:v2';
  LDifferent := NewRecipe(LInputs);
  try
    Check(LRecipe.Signature = LSecond.Signature,
      'fresh reconstruction from identical declarative inputs is stable');
    Check(LRecipe.Signature <> LDifferent.Signature,
      'every semantic provenance field contributes to bundle identity');
  finally
    LDifferent.Free;
    LSecond.Free;
    LRecipe.Free;
  end;
end;

procedure TestSequenceBridgeAndTransform;
var
  LInputs: TRecipeInputs;
  LRecipe: TWfcPipelineModel;
  LTerms: TWfcPipelineRequirementTerms;
  LVocabulary: TWfcModelTokens;
begin
  LInputs := BuildSequenceRecipeInputs;
  LRecipe := NewRecipe(LInputs);
  try
    LVocabulary := LRecipe.CopyPublicVocabulary(2);
    Check((Length(LVocabulary) = 2) and (LVocabulary[0] = 'A') and
      (LVocabulary[1] = 'B'),
      'sequence bridge vocabulary flows through a public empty transform');
    Check(LRecipe.BorrowSequenceResource(0).Order = 2,
      'sequence recipe owns its decoded typed model');
  finally
    LRecipe.Free;
  end;

  LInputs := BuildSequenceRecipeInputs;
  LInputs.Passes[0].Visibility := wppvPublic;
  Check(RecipeRejected(LInputs, 'must remain private'),
    'sequence adapters cannot expose private latent state keys');

  LInputs := BuildSequenceRecipeInputs;
  LInputs.Passes[0].SequenceExtent := wseWrap;
  Check(RecipeRejected(LInputs, 'open sequence pass'),
    'open sequence recipes reject a wrapped extent');

  LInputs := BuildSequenceRecipeInputs;
  LInputs.Passes[2].TransformSourceIndex := 0;
  LInputs.Dependencies[1].ProviderPassIndex := 0;
  Check(RecipeRejected(LInputs, 'cannot expose a private source'),
    'public transforms cannot copy a private representation');

  LInputs := BuildSequenceRecipeInputs;
  SetLength(LInputs.Dependencies, 1);
  Check(RecipeRejected(LInputs, 'transform pass 2 requires dependency'),
    'transform source edges must be declared explicitly');

  LInputs := BuildSequenceRecipeInputs;
  LInputs.Requirements := nil;
  SetLength(LInputs.Requirements, 1);
  SetLength(LInputs.Requirements[0].Terms, 1);
  LInputs.Requirements[0] := MakeWfcPipelineRequirement(2, 'A', 1,
    wprqExact, LInputs.Requirements[0].Terms);
  LInputs.Requirements[0].Terms[0] :=
    MakeWfcPipelineRequirementTerm(0, 1, 0,
      TokensOf([TWfcModelToken('A')]));
  Check(RecipeRejected(LInputs, 'inactive rank-1 axis'),
    'rank-1 requirements reject nonzero Y offsets');

  LInputs := BuildSequenceRecipeInputs;
  SetLength(LTerms, 1);
  LTerms[0] := MakeWfcPipelineRequirementTerm(0, 0, 0,
    TokensOf([TWfcModelToken('A')]));
  SetLength(LInputs.Requirements, 1);
  LInputs.Requirements[0] := MakeWfcPipelineRequirement(2, 'A', 1,
    wprqExact, LTerms);
  Check(RecipeRejected(LInputs, 'has no materialized rule definition'),
    'definitionless transforms cannot consume token requirements');
end;

procedure TestResourceAndVersionGuards;
var
  I: Integer;
  LDenseSlotCount: Integer;
  LInputs: TRecipeInputs;
  LRecipe: TWfcPipelineModel;
  LResourceCount: Integer;
  LSequenceText: String;
begin
  Check((WFC_PIPELINE_PATTERN_BRIDGE_VERSION = 2) and
    (WFC_PIPELINE_SEQUENCE_BRIDGE_VERSION = 2) and
    (CurrentWfcPipelineVersions.Pattern2DBridgeVersion = 2) and
    (CurrentWfcPipelineVersions.SequenceBridgeVersion = 2),
    'new recipes advertise inverse-lowering bridge version 2');

  LInputs := BuildPatternRecipeInputs;
  LInputs.Versions.Pattern2DBridgeVersion := 1;
  LRecipe := NewRecipe(LInputs);
  try
    Check((LRecipe.CopyVersions.Pattern2DBridgeVersion = 1) and
      (LRecipe.CopyVersions.SequenceBridgeVersion = 2),
      'pattern bridge version 1 remains independently accepted');
  finally
    LRecipe.Free;
  end;

  LInputs := BuildSequenceRecipeInputs;
  LInputs.Versions.SequenceBridgeVersion := 1;
  LRecipe := NewRecipe(LInputs);
  try
    Check((LRecipe.CopyVersions.Pattern2DBridgeVersion = 2) and
      (LRecipe.CopyVersions.SequenceBridgeVersion = 1),
      'sequence bridge version 1 remains independently accepted');
  finally
    LRecipe.Free;
  end;

  LInputs := BuildPatternRecipeInputs;
  LInputs.Versions.Pattern2DBridgeVersion := 0;
  Check(RecipeRejected(LInputs, 'unsupported pattern2d bridge version'),
    'pattern bridge version zero fails closed');
  LInputs := BuildPatternRecipeInputs;
  LInputs.Versions.Pattern2DBridgeVersion := 3;
  Check(RecipeRejected(LInputs, 'unsupported pattern2d bridge version'),
    'unknown future pattern bridge versions fail closed');
  LInputs := BuildSequenceRecipeInputs;
  LInputs.Versions.SequenceBridgeVersion := 0;
  Check(RecipeRejected(LInputs, 'unsupported sequence bridge version'),
    'sequence bridge version zero fails closed');
  LInputs := BuildSequenceRecipeInputs;
  LInputs.Versions.SequenceBridgeVersion := 3;
  Check(RecipeRejected(LInputs, 'unsupported sequence bridge version'),
    'unknown future sequence bridge versions fail closed');

  LInputs := BuildPatternRecipeInputs;
  LInputs.Resources[0].Document := StringReplace(
    LInputs.Resources[0].Document, #10, #13#10, [rfReplaceAll]);
  Check(RecipeRejected(LInputs, 'cannot be decoded'),
    'embedded resources retain strict canonical line endings');

  LInputs := BuildPatternRecipeInputs;
  LInputs.Resources[0].Kind := wprkModel;
  Check(RecipeRejected(LInputs, 'declared kind'),
    'resource kind and canonical payload header cannot disagree');

  LInputs := BuildPatternRecipeInputs;
  LInputs.Resources[1].Id := LInputs.Resources[0].Id;
  Check(RecipeRejected(LInputs, 'resource ids must be unique'),
    'resource ids are unique and nonempty');

  LInputs := BuildPatternRecipeInputs;
  LInputs.Resources[0].SourceDescription := '';
  Check(RecipeRejected(LInputs, 'source description cannot be empty'),
    'every resource carries nonempty source provenance');

  LInputs := BuildPatternRecipeInputs;
  LInputs.Resources[0].Kind := InvalidResourceKind;
  Check(RecipeRejected(LInputs, 'kind is unknown'),
    'unknown resource kinds fail closed');

  LInputs := BuildPatternRecipeInputs;
  Inc(LInputs.Versions.SolverAlgorithmVersion);
  Check(RecipeRejected(LInputs, 'unsupported reference-solver version'),
    'unknown replay-relevant versions fail closed');

  LInputs := BuildPatternRecipeInputs;
  LInputs.RunMode := InvalidRunMode;
  Check(RecipeRejected(LInputs, 'run mode is unknown'),
    'unknown traversal modes fail closed');

  LInputs := BuildPatternRecipeInputs;
  SetLength(LInputs.Resources, WFC_PIPELINE_MAX_RESOURCE_COUNT + 1);
  Check(RecipeRejected(LInputs, 'resource count exceeds'),
    'resource count limits are enforced before inspecting payloads');

  LInputs := BuildPatternRecipeInputs;
  LInputs.Resources[0].Document := StringOfChar('x',
    WFC_PIPELINE_MAX_RESOURCE_PAYLOAD_LENGTH + 1);
  Check(RecipeRejected(LInputs, 'payload exceeds'),
    'individual payload limits are enforced before nested decoding');

  LInputs := BuildPatternRecipeInputs;
  LInputs.Metadata.Name := StringOfChar('x',
    WFC_PIPELINE_MAX_ENCODED_TOKEN_LENGTH + 1);
  Check(RecipeRejected(LInputs, 'raw token length exceeds'),
    'outer token lengths are bounded before Unicode validation or encoding');

  LInputs := BuildSequenceRecipeInputs;
  LSequenceText := MaximumStateSequenceDocument;
  LDenseSlotCount := 4 * WFC_SEQUENCE_MAX_STATE_COUNT *
    WFC_SEQUENCE_MAX_STATE_COUNT;
  LResourceCount :=
    (WFC_PIPELINE_MAX_TOTAL_RESOURCE_RELATION_SLOT_COUNT div
    LDenseSlotCount) + 1;
  SetLength(LInputs.Resources, LResourceCount);
  for I := 0 to LResourceCount - 1 do
    LInputs.Resources[I] := MakeWfcPipelineResource(
      TWfcModelToken('sequence-') + IntToStr(I), wprkSequence,
      LSequenceText, 'relation-slot boundary fixture', 'MIT', '');
  Check(RecipeRejected(LInputs,
    'aggregate typed-resource relation slots exceed'),
    'aggregate typed-resource expansion has a fixed version-1 boundary');
end;

procedure TestPassAndTopologyGuards;
var
  LInputs: TRecipeInputs;
begin
  LInputs := BuildPatternRecipeInputs;
  LInputs.Passes[1].LabelName := LInputs.Passes[0].LabelName;
  Check(RecipeRejected(LInputs, 'pass labels must be unique'),
    'pass labels are unique and nonempty');

  LInputs := BuildPatternRecipeInputs;
  LInputs.Passes[2].AdapterKind := InvalidAdapterKind;
  Check(RecipeRejected(LInputs, 'adapter is unknown'),
    'unknown pass adapters fail closed');

  LInputs := BuildPatternRecipeInputs;
  LInputs.Passes[2].ResourceIndex := 2;
  Check(RecipeRejected(LInputs, 'requires a rules resource'),
    'adapter and resource kinds must match exactly');

  LInputs := BuildPatternRecipeInputs;
  LInputs.Passes[2].Mode := gpmTransform;
  LInputs.Passes[2].TransformSourceIndex := 1;
  Check(RecipeRejected(LInputs, 'must use the empty adapter'),
    'transform copy semantics cannot be shadowed by a local definition');

  LInputs := BuildPatternRecipeInputs;
  LInputs.Passes[0].Visibility := wppvPublic;
  Check(RecipeRejected(LInputs, 'must remain private'),
    'pattern adapters cannot expose private pattern keys');

  LInputs := BuildPatternRecipeInputs;
  SetLength(LInputs.Dependencies, 4);
  LInputs.Dependencies[3] := MakeWfcPipelineDependency(0, 1);
  Check(RecipeRejected(LInputs, 'contain a cycle'),
    'dependency cycles are rejected before runtime construction');

  LInputs := BuildPatternRecipeInputs;
  SetLength(LInputs.Dependencies, 4);
  LInputs.Dependencies[3] := LInputs.Dependencies[0];
  Check(RecipeRejected(LInputs, 'duplicates dependency'),
    'duplicate dependency edges are rejected explicitly');

  LInputs := BuildPatternRecipeInputs;
  LInputs.Dependencies[0] := LInputs.Dependencies[1];
  LInputs.Dependencies[1] := LInputs.Dependencies[2];
  SetLength(LInputs.Dependencies, 2);
  Check(RecipeRejected(LInputs, 'bridge 0 requires dependency'),
    'bridge-implied dependencies must be declared');

  LInputs := BuildPatternRecipeInputs;
  LInputs.WrapNeighbors := False;
  Check(RecipeRejected(LInputs, 'wrapped rank-2 pipeline'),
    'pattern projection bridge rejects incompatible topology');

  LInputs := BuildPatternRecipeInputs;
  LInputs.Resources[0].Document := ReservedPatternKeyDocument;
  Check(RecipeRejected(LInputs, 'reserved latent-key syntax'),
    'pattern projection rejects a public token shaped like a private key');

  LInputs := BuildPatternRecipeInputs;
  SetLength(LInputs.Bridges, 2);
  LInputs.Bridges[1] := LInputs.Bridges[0];
  Check(RecipeRejected(LInputs, 'one vocabulary owner'),
    'a materialized public target has exactly one bridge owner');

  LInputs := BuildPatternRecipeInputs;
  LInputs.Bridges := nil;
  Check(RecipeRejected(LInputs, 'no statically known vocabulary owner'),
    'an empty public pass cannot exist without a typed vocabulary source');
end;

procedure TestRequirementGuards;
var
  I: Integer;
  J: Integer;
  LInputs: TRecipeInputs;
  LTerm: TWfcPipelineRequirementTerm;
begin
  LInputs := BuildPatternRecipeInputs;
  LInputs.Requirements[0].ConsumerPassIndex := 0;
  Check(RecipeRejected(LInputs, 'endpoints must both be public'),
    'public token requirements cannot name private passes');

  LInputs := BuildPatternRecipeInputs;
  LInputs.Requirements[0].ConsumerToken := 'unknown';
  Check(RecipeRejected(LInputs, 'outside its public vocabulary'),
    'consumer tokens resolve against static public vocabulary');

  LInputs := BuildPatternRecipeInputs;
  LInputs.Requirements[0].Terms[0].AllowedProviderTokens[0] := 'unknown';
  Check(RecipeRejected(LInputs, 'outside the provider vocabulary'),
    'provider tokens resolve against static public vocabulary');

  LInputs := BuildPatternRecipeInputs;
  SetLength(LInputs.Requirements[0].Terms, 2);
  LInputs.Requirements[0].Terms[1] := LInputs.Requirements[0].Terms[0];
  Check(RecipeRejected(LInputs, 'exactly one term'),
    'exact requirements contain one and only one term');

  LInputs := BuildPatternRecipeInputs;
  LTerm := LInputs.Requirements[1].Terms[0];
  LInputs.Requirements[1].Terms[0] := LInputs.Requirements[1].Terms[1];
  LInputs.Requirements[1].Terms[1] := LTerm;
  Check(RecipeRejected(LInputs, 'strict X/Y/Z order'),
    'any-of terms use canonical signed offset order');

  LInputs := BuildPatternRecipeInputs;
  LInputs.Requirements[1].Terms[0].AllowedProviderTokens :=
    TokensOf([TWfcModelToken('water'), TWfcModelToken('land')]);
  Check(RecipeRejected(LInputs, 'provider-vocabulary order'),
    'allowed-token sets use canonical provider vocabulary order');

  LInputs := BuildPatternRecipeInputs;
  SetLength(LInputs.Requirements, 3);
  LInputs.Requirements[2] := LInputs.Requirements[0];
  Check(RecipeRejected(LInputs, 'duplicates requirement'),
    'duplicate exact requirement keys cannot collapse into hidden OR');

  LInputs := BuildPatternRecipeInputs;
  LInputs.Requirements[0].Terms[0].OffsetZ := 1;
  Check(RecipeRejected(LInputs, 'inactive rank-2 axis'),
    'rank-2 requirements reject nonzero Z offsets');

  LInputs := BuildPatternRecipeInputs;
  LInputs.Requirements[1].Terms[0].OffsetX := Low(Integer);
  LInputs.Requirements[1].Terms[1].OffsetX := High(Integer);
  Check(not RecipeRejected(LInputs, ''),
    'active axes preserve the complete signed Integer offset range');

  LInputs := BuildPatternRecipeInputs;
  SetLength(LInputs.Requirements,
    (WFC_PIPELINE_MAX_TOTAL_REQUIREMENT_TERM_COUNT div
    WFC_PIPELINE_MAX_REQUIREMENT_TERM_COUNT) + 1);
  for I := 0 to Length(LInputs.Requirements) - 1 do
  begin
    LInputs.Requirements[I].ConsumerToken := 'tree';
    SetLength(LInputs.Requirements[I].Terms,
      WFC_PIPELINE_MAX_REQUIREMENT_TERM_COUNT);
  end;
  Check(RecipeRejected(LInputs, 'aggregate requirement-term count exceeds'),
    'aggregate requirement-term storage has a fixed version-1 boundary');

  LInputs := BuildPatternRecipeInputs;
  SetLength(LInputs.Requirements,
    (WFC_PIPELINE_MAX_TOTAL_ALLOWED_TOKEN_COUNT div
    WFC_PIPELINE_MAX_ALLOWED_TOKEN_COUNT) + 1);
  for I := 0 to Length(LInputs.Requirements) - 1 do
  begin
    LInputs.Requirements[I].ConsumerToken := 'tree';
    SetLength(LInputs.Requirements[I].Terms, 1);
    SetLength(LInputs.Requirements[I].Terms[0].AllowedProviderTokens,
      WFC_PIPELINE_MAX_ALLOWED_TOKEN_COUNT);
    for J := 0 to WFC_PIPELINE_MAX_ALLOWED_TOKEN_COUNT - 1 do
      LInputs.Requirements[I].Terms[0].AllowedProviderTokens[J] := 'land';
  end;
  Check(RecipeRejected(LInputs, 'aggregate allowed-token count exceeds'),
    'aggregate allowed-token storage has a fixed version-1 boundary');
end;

begin
  WriteLn('WFC portable pipeline recipe model suite');
  WriteLn('========================================');
  RunTest('complete recipe and ownership', @TestCompleteRecipeAndOwnership);
  RunTest('sequence bridge and transform', @TestSequenceBridgeAndTransform);
  RunTest('resource and version guards', @TestResourceAndVersionGuards);
  RunTest('pass and topology guards', @TestPassAndTopologyGuards);
  RunTest('public requirement guards', @TestRequirementGuards);
  WriteLn('========================================');
  WriteLn(GCheckCount, ' checks, ', GFailureCount, ' failures');
  if GFailureCount <> 0 then
  begin
    {$IFDEF PAS2JS}
    raise Exception.CreateFmt('%d pipeline-model checks failed',
      [GFailureCount]);
    {$ELSE}
    Halt(1);
    {$ENDIF}
  end;
end.
