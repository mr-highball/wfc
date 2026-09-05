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
program wfc_training_test;

{$mode delphi}{$H+}

uses
  SysUtils,
  wfc,
  wfc_model,
  wfc_learn,
  wfc_model_text,
  wfc_pattern2d,
  wfc_pattern2d_learn,
  wfc_pattern2d_text,
  wfc_sequence,
  wfc_sequence_learn,
  wfc_sequence_text,
  wfc_text_codec,
  wfc_pipeline_model,
  wfc_pipeline_run,
  wfc_pipeline_result,
  wfc_pipeline_runtime,
  wfc_training;

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

procedure BeginTest(const AName: String);
begin
  WriteLn('[TEST] ', AName);
end;

function TokensOf(
  const AValues: array of TWfcModelToken): TWfcModelTokens;
var
  I: Integer;
begin
  Result := nil;
  SetLength(Result, Length(AValues));
  for I := 0 to Length(AValues) - 1 do
    Result[I] := AValues[I];
end;

function MusicToken: TWfcModelToken;
begin
  {$IFDEF PAS2JS}
  Result := TWfcModelToken(Chr($266B));
  {$ELSE}
  Result := TWfcModelToken(UTF8Encode(
    UnicodeString(WideChar($266B))));
  {$ENDIF}
end;

function BasicMetadata: TWfcTrainingMetadata;
begin
  Result := MakeWfcTrainingMetadata('portable learner', 'MIT',
    'project-authored training corpus');
end;

function Samples1D: TWfcTrainingSamples;
begin
  Result := nil;
  SetLength(Result, 2);
  Result[0] := MakeWfcTrainingSample('first', 2, 1,
    TokensOf(['A', 'B']));
  Result[1] := MakeWfcTrainingSample('second', 2, 1,
    TokensOf(['C', 'D']));
end;

function DirectLearnSamples(
  const ATrainingSamples: TWfcTrainingSamples): TWfcLearnSamples;
var
  I: Integer;
begin
  Result := nil;
  SetLength(Result, Length(ATrainingSamples));
  for I := 0 to Length(ATrainingSamples) - 1 do
    Result[I] := MakeLearnSample2D(ATrainingSamples[I].Tokens,
      ATrainingSamples[I].Width, ATrainingSamples[I].Height);
end;

function DirectSequenceSamples(
  const ATrainingSamples: TWfcTrainingSamples): TWfcSequenceSamples;
var
  I: Integer;
begin
  Result := nil;
  SetLength(Result, Length(ATrainingSamples));
  for I := 0 to Length(ATrainingSamples) - 1 do
    Result[I] := MakeWfcSequenceSample(ATrainingSamples[I].Tokens);
end;

procedure TestDocumentIdentityAndImmutability;
var
  LCopy: TWfcTrainingSamples;
  LDocument: TWfcTrainingDocument;
  LMetadata: TWfcTrainingMetadata;
  LOptions: TWfcTrainingOptions;
  LSample: TWfcTrainingSample;
  LSamples: TWfcTrainingSamples;
  LTokens: TWfcModelTokens;
begin
  BeginTest('immutable training document and semantic identity');
  Check((WFC_TRAINING_VERSION = 1) and
    (WFC_TRAINING_MAX_SAMPLE_COUNT = 4096) and
    (WFC_TRAINING_MAX_TOTAL_TOKEN_COUNT = 65536) and
    (WFC_TRAINING_MAX_DIMENSION = 65536) and
    (WFC_TRAINING_MAX_ENCODED_TOKEN_LENGTH = 65536) and
    (WFC_TRAINING_MAX_TOTAL_ENCODED_TOKEN_LENGTH = 4194304) and
    (WFC_TRAINING_MAX_FOOTPRINT_CELL_COUNT = 64) and
    (WFC_TRAINING_MAX_VISIT_COUNT = 16777216) and
    (WFC_TRAINING_MAX_ORDER = 64),
    'version-one training limits are exact');

  LMetadata := BasicMetadata;
  LOptions := MakeWfcTrainingOptions(wtkAdjacency1D,
    wmbOpen, wmsNone, 0, 0, 0);
  LTokens := TokensOf(['A', 'B']);
  SetLength(LSamples, 2);
  LSamples[0] := MakeWfcTrainingSample('first', 2, 1, LTokens);
  LSamples[1] := MakeWfcTrainingSample('second', 3, 1,
    TokensOf(['B', MusicToken, 'A']));
  LDocument := TWfcTrainingDocument.Create(LMetadata, LOptions, LSamples);
  try
    Check(WfcTextEncodeToken(MusicToken, 'training test') = '%E2%99%AB',
      'Unicode training tokens have one canonical ASCII encoding (' +
      WfcTextEncodeToken(MusicToken, 'training test') + ')');
    Check((LDocument.SampleCount = 2) and
      (LDocument.TotalTokenCount = 5),
      'document retains ordered sample and token counts');
    Check(WfcTrainingSignatureHex(LDocument.Signature) = '4FCBB7E7',
      'training signature golden is pinned (' +
      WfcTrainingSignatureHex(LDocument.Signature) + ')');

    LMetadata.Name := 'mutated';
    LOptions.Kind := wtkSequence;
    LTokens[0] := 'mutated';
    LSamples[0].Name := 'mutated';
    LSamples[0].Tokens[1] := 'mutated';
    Check((LDocument.CopyMetadata.Name = 'portable learner') and
      (LDocument.CopyOptions.Kind = wtkAdjacency1D),
      'metadata and options are detached from constructor inputs');
    LSample := LDocument.SampleAt(0);
    Check((LSample.Name = 'first') and (LSample.Tokens[0] = 'A') and
      (LSample.Tokens[1] = 'B'),
      'sample data is detached from helper and constructor inputs');
    LSample.Name := 'changed';
    LSample.Tokens[0] := 'changed';
    LCopy := LDocument.CopySamples;
    LCopy[0].Tokens[1] := 'changed';
    Check((LDocument.SampleAt(0).Name = 'first') and
      (LDocument.SampleAt(0).Tokens[0] = 'A') and
      (LDocument.SampleAt(0).Tokens[1] = 'B'),
      'sample accessors return deep detached copies');
  finally
    LDocument.Free;
  end;
end;

procedure TestAdjacencyProfiles;
var
  LDirect: TWfcModel;
  LDirectSamples: TWfcLearnSamples;
  LDocument: TWfcTrainingDocument;
  LModel: TWfcModel;
  LOptions: TWfcTrainingOptions;
  LSamples: TWfcTrainingSamples;
  LText: String;
begin
  BeginTest('cardinal 1D and 2D training equivalence');
  LSamples := Samples1D;
  LOptions := MakeWfcTrainingOptions(wtkAdjacency1D,
    wmbOpen, wmsNone, 0, 0, 0);
  LDocument := TWfcTrainingDocument.Create(BasicMetadata,
    LOptions, LSamples);
  try
    Check(WfcTrainingSignatureHex(LDocument.Signature) = '7C62796B',
      'adjacency1d training identity is pinned (' +
      WfcTrainingSignatureHex(LDocument.Signature) + ')');
    LText := LearnWfcTrainingModelText(LDocument);
    SetLength(LDirectSamples, 2);
    LDirectSamples[0] := MakeLearnSample1D(LSamples[0].Tokens);
    LDirectSamples[1] := MakeLearnSample1D(LSamples[1].Tokens);
    LDirect := LearnModel1DCorpus(LDirectSamples, wmbOpen);
    try
      Check(LText = EncodeWfcModelText(LDirect),
        'adjacency1d delegates exactly to the ordered corpus learner');
    finally
      LDirect.Free;
    end;
    LModel := DecodeWfcModelText(LText);
    try
      Check((LModel.SampleCount = 2) and
        (LModel.RelationCount(wmdEast, LModel.FindToken('B'),
        LModel.FindToken('C')) = 0),
        'adjacency1d keeps corpus boundaries seam-free');
    finally
      LModel.Free;
    end;
  finally
    LDocument.Free;
  end;

  SetLength(LSamples, 2);
  LSamples[0] := MakeWfcTrainingSample('north', 2, 2,
    TokensOf(['A', 'B', 'C', 'D']));
  LSamples[1] := MakeWfcTrainingSample('south', 1, 2,
    TokensOf(['E', 'F']));
  LOptions := MakeWfcTrainingOptions(wtkAdjacency2D,
    wmbWrap, wmsD4, 0, 0, 0);
  LDocument := TWfcTrainingDocument.Create(BasicMetadata,
    LOptions, LSamples);
  try
    Check(WfcTrainingSignatureHex(LDocument.Signature) = 'F66FB2A9',
      'adjacency2d training identity is pinned (' +
      WfcTrainingSignatureHex(LDocument.Signature) + ')');
    LText := LearnWfcTrainingModelText(LDocument);
    LDirectSamples := DirectLearnSamples(LSamples);
    LDirect := LearnModel2DCorpus(LDirectSamples, wmbWrap, wmsD4);
    try
      Check(LText = EncodeWfcModelText(LDirect),
        'adjacency2d delegates exactly with wrap and D4 policy');
    finally
      LDirect.Free;
    end;
  finally
    LDocument.Free;
  end;
end;

procedure TestPatternAndSequenceProfiles;
var
  LDirectPattern: TWfcOverlappingModel2D;
  LDirectSamples: TWfcLearnSamples;
  LDirectSequence: TWfcSequenceModel;
  LDocument: TWfcTrainingDocument;
  LOptions: TWfcTrainingOptions;
  LSamples: TWfcTrainingSamples;
  LSequenceSamples: TWfcSequenceSamples;
  LText: String;
begin
  BeginTest('Pattern2D and sequence training equivalence');
  SetLength(LSamples, 2);
  LSamples[0] := MakeWfcTrainingSample('checker', 2, 2,
    TokensOf(['A', 'B', 'B', 'A']));
  LSamples[1] := MakeWfcTrainingSample('stripe', 2, 2,
    TokensOf(['A', 'A', 'B', 'B']));
  LOptions := MakeWfcTrainingOptions(wtkPattern2D,
    wmbWrap, wmsNone, 2, 2, 0);
  LDocument := TWfcTrainingDocument.Create(BasicMetadata,
    LOptions, LSamples);
  try
    Check(WfcTrainingSignatureHex(LDocument.Signature) = '5BB36038',
      'pattern2d training identity is pinned (' +
      WfcTrainingSignatureHex(LDocument.Signature) + ')');
    LText := LearnWfcTrainingModelText(LDocument);
    LDirectSamples := DirectLearnSamples(LSamples);
    LDirectPattern := LearnOverlappingModel2DCorpus(LDirectSamples,
      2, 2, wmbWrap, wmsNone);
    try
      Check(LText = EncodeWfcPattern2DText(LDirectPattern),
        'pattern2d delegates exactly to overlapping corpus extraction');
    finally
      LDirectPattern.Free;
    end;
  finally
    LDocument.Free;
  end;

  SetLength(LSamples, 2);
  LSamples[0] := MakeWfcTrainingSample('left', 3, 1,
    TokensOf(['A', 'B', 'A']));
  LSamples[1] := MakeWfcTrainingSample('right', 3, 1,
    TokensOf(['A', 'C', 'A']));
  LOptions := MakeWfcTrainingOptions(wtkSequence,
    wmbOpen, wmsNone, 0, 0, 2);
  LDocument := TWfcTrainingDocument.Create(BasicMetadata,
    LOptions, LSamples);
  try
    Check(WfcTrainingSignatureHex(LDocument.Signature) = 'D5D9B48C',
      'sequence training identity is pinned (' +
      WfcTrainingSignatureHex(LDocument.Signature) + ')');
    LText := LearnWfcTrainingModelText(LDocument);
    LSequenceSamples := DirectSequenceSamples(LSamples);
    LDirectSequence := LearnSequenceModelCorpus(LSequenceSamples, 2);
    try
      Check(LText = EncodeWfcSequenceText(LDirectSequence),
        'sequence delegates exactly with independent typed BOS boundaries');
    finally
      LDirectSequence.Free;
    end;
  finally
    LDocument.Free;
  end;
end;

function RecipeFingerprintMatches(const ARecipe: TWfcPipelineModel;
  const ADocument: TWfcTrainingDocument): Boolean;
var
  LExpected: TWfcModelToken;
  LMetadata: TWfcPipelineMetadata;
  LResource: TWfcPipelineResource;
begin
  LExpected := TWfcModelToken('wfclearn-v1/' +
    WfcTrainingSignatureHex(ADocument.Signature));
  LMetadata := ARecipe.CopyMetadata;
  LResource := ARecipe.ResourceAt(0);
  Result := (LMetadata.SourceFingerprint = LExpected) and
    (LResource.SourceFingerprint = LExpected) and
    (LMetadata.LicenseIdentifier = 'MIT') and
    (LResource.SourceLicenseIdentifier = 'MIT') and
    (Pos('project-authored training corpus | samples=',
      String(LMetadata.SourceDescription)) = 1) and
    (LMetadata.SourceDescription = LResource.SourceDescription);
end;

procedure TestRecipeTopologyAndProvenance;
var
  LBridge: TWfcPipelineBridge;
  LDocument: TWfcTrainingDocument;
  LMetadata: TWfcPipelineMetadata;
  LOptions: TWfcTrainingOptions;
  LPass: TWfcPipelinePass;
  LRecipe: TWfcPipelineModel;
  LSamples: TWfcTrainingSamples;
begin
  BeginTest('recipe topology and retained provenance');
  LSamples := Samples1D;
  LOptions := MakeWfcTrainingOptions(wtkAdjacency1D,
    wmbOpen, wmsNone, 0, 0, 0);
  LDocument := TWfcTrainingDocument.Create(BasicMetadata,
    LOptions, LSamples);
  try
    LRecipe := LearnWfcTrainingRecipe(LDocument);
    try
      LPass := LRecipe.PassAt(0);
      Check((LRecipe.Rank = 1) and (not LRecipe.WrapNeighbors) and
        (LRecipe.ResourceCount = 1) and (LRecipe.PassCount = 1) and
        (LRecipe.DependencyCount = 0) and (LRecipe.BridgeCount = 0) and
        (LPass.LabelName = 'output') and
        (LPass.Visibility = wppvPublic) and
        (LPass.AdapterKind = wpakModel) and
        RecipeFingerprintMatches(LRecipe, LDocument),
        'adjacency1d exports one public model pass with provenance');
      LMetadata := LRecipe.CopyMetadata;
      Check(Pos('0:first:2x1:', String(LMetadata.SourceDescription)) > 0,
        'recipe provenance retains ordered sample name, shape, and fingerprint');
    finally
      LRecipe.Free;
    end;
  finally
    LDocument.Free;
  end;

  SetLength(LSamples, 1);
  LSamples[0] := MakeWfcTrainingSample('checker', 2, 2,
    TokensOf(['A', 'B', 'B', 'A']));
  LOptions := MakeWfcTrainingOptions(wtkPattern2D,
    wmbWrap, wmsNone, 2, 2, 0);
  LDocument := TWfcTrainingDocument.Create(BasicMetadata,
    LOptions, LSamples);
  try
    LRecipe := LearnWfcTrainingRecipe(LDocument);
    try
      LBridge := LRecipe.BridgeAt(0);
      Check((LRecipe.Rank = 2) and LRecipe.WrapNeighbors and
        (LRecipe.PassCount = 2) and (LRecipe.DependencyCount = 1) and
        (LRecipe.BridgeCount = 1) and
        (LRecipe.PassAt(0).LabelName = 'patterns') and
        (LRecipe.PassAt(0).Visibility = wppvPrivate) and
        (LRecipe.PassAt(1).LabelName = 'output') and
        (LBridge.Kind = wpbkPattern2DProjection) and
        RecipeFingerprintMatches(LRecipe, LDocument),
        'pattern2d exports private patterns through the current public bridge');
    finally
      LRecipe.Free;
    end;
  finally
    LDocument.Free;
  end;

  SetLength(LSamples, 2);
  LSamples[0] := MakeWfcTrainingSample('left', 3, 1,
    TokensOf(['A', 'B', 'A']));
  LSamples[1] := MakeWfcTrainingSample('right', 3, 1,
    TokensOf(['A', 'C', 'A']));
  LOptions := MakeWfcTrainingOptions(wtkSequence,
    wmbOpen, wmsNone, 0, 0, 2);
  LDocument := TWfcTrainingDocument.Create(BasicMetadata,
    LOptions, LSamples);
  try
    LRecipe := LearnWfcTrainingRecipe(LDocument);
    try
      LPass := LRecipe.PassAt(0);
      LBridge := LRecipe.BridgeAt(0);
      Check((LRecipe.Rank = 1) and (not LRecipe.WrapNeighbors) and
        (LRecipe.PassCount = 2) and (LRecipe.DependencyCount = 1) and
        (LRecipe.BridgeCount = 1) and
        (LPass.LabelName = 'sequence') and
        (LPass.Visibility = wppvPrivate) and
        (LPass.AdapterKind = wpakSequence) and LPass.HasSequenceExtent and
        (LPass.SequenceExtent = wseWhole) and
        (LBridge.Kind = wpbkSequenceProjection) and
        RecipeFingerprintMatches(LRecipe, LDocument),
        'sequence exports a private whole extent through the current public bridge');
    finally
      LRecipe.Free;
    end;
  finally
    LDocument.Free;
  end;
end;

procedure TestRuntimePublicLocks;
var
  LDocument: TWfcTrainingDocument;
  LLayer: TWfcPipelineResultLayer;
  LLocks: TWfcPipelineCellLocks;
  LOptions: TWfcTrainingOptions;
  LRecipe: TWfcPipelineModel;
  LResult: TWfcPipelineResult;
  LRun: TWfcPipelineRun;
  LSamples: TWfcTrainingSamples;
begin
  BeginTest('trained recipes accept public locks');
  SetLength(LSamples, 1);
  LSamples[0] := MakeWfcTrainingSample('checker', 2, 2,
    TokensOf(['A', 'B', 'B', 'A']));
  LOptions := MakeWfcTrainingOptions(wtkPattern2D,
    wmbWrap, wmsNone, 2, 2, 0);
  LDocument := TWfcTrainingDocument.Create(BasicMetadata,
    LOptions, LSamples);
  try
    LRecipe := LearnWfcTrainingRecipe(LDocument);
    try
      SetLength(LLocks, 1);
      LLocks[0] := MakeWfcPipelineCellLock(1, 0, 0, 0, 'A');
      LRun := TWfcPipelineRun.Create(LRecipe, 2, 2, 1, 0,
        wpssOneWay, 65536, 0, False, LLocks, nil);
      try
        LResult := ExecuteWfcPipeline(LRecipe, LRun);
        try
          LLayer := LResult.LayerAt(0);
          Check((LResult.Status = wprsSolved) and
            (LResult.LayerCount = 1) and
            (LLayer.LabelName = 'output') and
            (Length(LLayer.Tokens) = 4) and
            (LLayer.Tokens[0] = 'A') and (LLayer.Tokens[1] = 'B') and
            (LLayer.Tokens[2] = 'B') and (LLayer.Tokens[3] = 'A'),
            'Pattern2D bridge inversely lowers a public output lock');
        finally
          LResult.Free;
        end;
      finally
        LRun.Free;
      end;
    finally
      LRecipe.Free;
    end;
  finally
    LDocument.Free;
  end;

  SetLength(LSamples, 2);
  LSamples[0] := MakeWfcTrainingSample('left', 3, 1,
    TokensOf(['A', 'B', 'A']));
  LSamples[1] := MakeWfcTrainingSample('right', 3, 1,
    TokensOf(['A', 'C', 'A']));
  LOptions := MakeWfcTrainingOptions(wtkSequence,
    wmbOpen, wmsNone, 0, 0, 2);
  LDocument := TWfcTrainingDocument.Create(BasicMetadata,
    LOptions, LSamples);
  try
    LRecipe := LearnWfcTrainingRecipe(LDocument);
    try
      SetLength(LLocks, 1);
      LLocks[0] := MakeWfcPipelineCellLock(1, 1, 0, 0, 'C');
      LRun := TWfcPipelineRun.Create(LRecipe, 3, 1, 1, 0,
        wpssOneWay, 65536, 0, False, LLocks, nil);
      try
        LResult := ExecuteWfcPipeline(LRecipe, LRun);
        try
          LLayer := LResult.LayerAt(0);
          Check((LResult.Status = wprsSolved) and
            (LResult.LayerCount = 1) and
            (LLayer.LabelName = 'output') and
            (Length(LLayer.Tokens) = 3) and
            (LLayer.Tokens[0] = 'A') and (LLayer.Tokens[1] = 'C') and
            (LLayer.Tokens[2] = 'A'),
            'sequence bridge inversely lowers a public output lock');
        finally
          LResult.Free;
        end;
      finally
        LRun.Free;
      end;
    finally
      LRecipe.Free;
    end;
  finally
    LDocument.Free;
  end;
end;

procedure CheckDocumentRejected(const AMetadata: TWfcTrainingMetadata;
  const AOptions: TWfcTrainingOptions;
  const ASamples: TWfcTrainingSamples; const AExpected,
  AMessage: String);
var
  LDocument: TWfcTrainingDocument;
  LMessage: String;
  LRaised: Boolean;
begin
  LDocument := nil;
  LMessage := '';
  LRaised := False;
  try
    try
      LDocument := TWfcTrainingDocument.Create(AMetadata,
        AOptions, ASamples);
    except
      on E: EWfcTraining do
      begin
        LRaised := True;
        LMessage := E.Message;
      end;
    end;
  finally
    LDocument.Free;
  end;
  Check(LRaised and (Pos(AExpected, LMessage) > 0), AMessage);
end;

procedure TestValidationAndLimits;
var
  I: Integer;
  J: Integer;
  LDocument: TWfcTrainingDocument;
  LMetadata: TWfcTrainingMetadata;
  LOptions: TWfcTrainingOptions;
  LRecipe: TWfcPipelineModel;
  LSamples: TWfcTrainingSamples;
  LText: String;
  LTokens: TWfcModelTokens;
begin
  BeginTest('training validation and pre-allocation limits');
  LMetadata := BasicMetadata;
  LOptions := MakeWfcTrainingOptions(wtkAdjacency1D,
    wmbOpen, wmsNone, 0, 0, 0);
  CheckDocumentRejected(LMetadata, LOptions, nil,
    'sample count', 'an empty corpus is rejected');

  LSamples := Samples1D;
  LSamples[1].Name := LSamples[0].Name;
  CheckDocumentRejected(LMetadata, LOptions, LSamples,
    'must be unique', 'duplicate sample names are rejected');

  LSamples := Samples1D;
  LSamples[0].Width := 3;
  CheckDocumentRejected(LMetadata, LOptions, LSamples,
    'has 2 tokens; expected 3', 'sample shape mismatch is rejected');

  LSamples := Samples1D;
  LOptions := MakeWfcTrainingOptions(wtkAdjacency1D,
    wmbOpen, wmsD4, 0, 0, 0);
  CheckDocumentRejected(LMetadata, LOptions, LSamples,
    'symmetry must be none', 'adjacency1d rejects D4 symmetry');

  LOptions := MakeWfcTrainingOptions(wtkSequence,
    wmbWrap, wmsNone, 0, 0, 2);
  CheckDocumentRejected(LMetadata, LOptions, LSamples,
    'boundary must be open', 'sequence training rejects wrapped source seams');

  LOptions := MakeWfcTrainingOptions(wtkPattern2D,
    wmbWrap, wmsD4, 2, 1, 0);
  CheckDocumentRejected(LMetadata, LOptions, LSamples,
    'square footprint', 'Pattern2D D4 requires a square footprint');

  LOptions := MakeWfcTrainingOptions(wtkAdjacency1D,
    wmbOpen, wmsNone, 0, 0, 0);
  SetLength(LTokens, WFC_MODEL_MAX_VALUE_COUNT + 1);
  for I := 0 to Length(LTokens) - 1 do
    LTokens[I] := TWfcModelToken('v' + IntToStr(I));
  SetLength(LSamples, 1);
  LSamples[0] := MakeWfcTrainingSample('wide', Length(LTokens), 1,
    LTokens);
  CheckDocumentRejected(LMetadata, LOptions, LSamples,
    'vocabulary', 'generic value capacity is rejected before learning');

  LOptions := MakeWfcTrainingOptions(wtkPattern2D,
    wmbWrap, wmsD4, 8, 8, 0);
  SetLength(LTokens, 32769);
  for I := 0 to Length(LTokens) - 1 do
    LTokens[I] := 'A';
  SetLength(LSamples, 1);
  LSamples[0] := MakeWfcTrainingSample('visits', Length(LTokens), 1,
    LTokens);
  CheckDocumentRejected(LMetadata, LOptions, LSamples,
    'visit count', 'Pattern2D transformed payload work is bounded');

  LOptions := MakeWfcTrainingOptions(wtkSequence,
    wmbOpen, wmsNone, 0, 0, 2);
  SetLength(LSamples, 33 * 33);
  I := 0;
  for J := 0 to 32 do
    while I < (J + 1) * 33 do
    begin
      LSamples[I] := MakeWfcTrainingSample(
        TWfcModelToken('pair-' + IntToStr(I)), 2, 1,
        TokensOf([TWfcModelToken('t' + IntToStr(J)),
          TWfcModelToken('t' + IntToStr(I mod 33))]));
      Inc(I);
    end;
  CheckDocumentRejected(LMetadata, LOptions, LSamples,
    'state count', 'sequence state capacity is rejected before learning');

  SetLength(LSamples, 1);
  LSamples[0] := MakeWfcTrainingSample('open-pattern', 2, 2,
    TokensOf(['A', 'B', 'B', 'A']));
  LOptions := MakeWfcTrainingOptions(wtkPattern2D,
    wmbOpen, wmsNone, 2, 2, 0);
  LDocument := TWfcTrainingDocument.Create(LMetadata, LOptions, LSamples);
  try
    LText := LearnWfcTrainingModelText(LDocument);
    Check(Pos('wfcp=1'#10, LText) = 1,
      'open Pattern2D remains a supported standalone learned model');
    LRecipe := nil;
    try
      try
        LRecipe := LearnWfcTrainingRecipe(LDocument);
        Check(False, 'open Pattern2D recipe export is rejected explicitly');
      except
        on E: EWfcTraining do
          Check(Pos('requires wrapped', E.Message) > 0,
            'open Pattern2D recipe export is rejected explicitly');
      end;
    finally
      LRecipe.Free;
    end;
  finally
    LDocument.Free;
  end;
end;

procedure TestLearnerCapacityHardening;
var
  I: Integer;
  LMessage: String;
  LModel: TWfcModel;
  LSequence: TWfcSequenceModel;
  LTokens: TWfcModelTokens;
  LRaised: Boolean;
begin
  BeginTest('public learners reject target capacities during construction');
  SetLength(LTokens, WFC_MODEL_MAX_VALUE_COUNT + 1);
  for I := 0 to Length(LTokens) - 1 do
    LTokens[I] := TWfcModelToken('v' + IntToStr(I));
  LModel := nil;
  LMessage := '';
  LRaised := False;
  try
    try
      LModel := LearnModel1D(LTokens, wmbOpen);
    except
      on E: ERangeError do
      begin
        LRaised := True;
        LMessage := E.Message;
      end;
    end;
  finally
    LModel.Free;
  end;
  Check(LRaised and (Pos('value count exceeds', LMessage) > 0),
    'generic learner stops at the version-one value limit');

  LSequence := nil;
  LMessage := '';
  LRaised := False;
  try
    try
      LSequence := LearnSequenceModel(TokensOf(['A']),
        WFC_SEQUENCE_MAX_ORDER + 1);
    except
      on E: EWfcSequence do
      begin
        LRaised := True;
        LMessage := E.Message;
      end;
    end;
  finally
    LSequence.Free;
  end;
  Check(LRaised and (Pos('order exceeds', LMessage) > 0),
    'sequence learner stops before allocating an oversized history');

  SetLength(LTokens,
    (WFC_SEQUENCE_MAX_TOTAL_HISTORY_ITEM_COUNT div 65) + 1);
  for I := 0 to Length(LTokens) - 1 do
    LTokens[I] := TWfcModelToken('h' + IntToStr(I));
  LSequence := nil;
  LMessage := '';
  LRaised := False;
  try
    try
      LSequence := LearnSequenceModel(LTokens, 66);
    except
      on E: EWfcSequence do
      begin
        LRaised := True;
        LMessage := E.Message;
      end;
    end;
  finally
    LSequence.Free;
  end;
  Check(LRaised and (Pos('state history exceeds', LMessage) > 0),
    'sequence learner stops before exceeding aggregate state history');
end;

begin
  TestDocumentIdentityAndImmutability;
  TestAdjacencyProfiles;
  TestPatternAndSequenceProfiles;
  TestRecipeTopologyAndProvenance;
  TestRuntimePublicLocks;
  TestValidationAndLimits;
  TestLearnerCapacityHardening;
  WriteLn('Checks: ', GCheckCount, ', Failures: ', GFailureCount);
  if GFailureCount <> 0 then
  begin
    {$IFDEF PAS2JS}
    raise Exception.CreateFmt('%d training checks failed', [GFailureCount]);
    {$ELSE}
    Halt(1);
    {$ENDIF}
  end;
end.
