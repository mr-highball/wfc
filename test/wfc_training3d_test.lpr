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
program wfc_training3d_test;

{$mode delphi}{$H+}

uses
  {$IFDEF PAS2JS}wfc_browser_test_host,{$ENDIF}
  SysUtils,
  wfc,
  wfc_model,
  wfc_model_text,
  wfc_pipeline_compile,
  wfc_pipeline_model,
  wfc_pipeline_result,
  wfc_pipeline_run,
  wfc_pipeline_runtime,
  wfc_training,
  wfc_training_text;

type
  TTestProcedure = procedure;

const
  SOURCE_GOLDEN =
    'wfclearn=2'#10 +
    'name=Volume'#10 +
    'license=MIT'#10 +
    'source=fixture'#10 +
    'kind=adjacency3d'#10 +
    'boundary=wrap'#10 +
    'symmetry=none'#10 +
    'footprint=0,0'#10 +
    'order=0'#10 +
    'samples=1'#10 +
    'sample=0,1,1,1,one'#10 +
    'token=0,0,A'#10 +
    'end'#10;

  MODEL_GOLDEN =
    'wfcm=3'#10 +
    'rank=3'#10 +
    'samples=1'#10 +
    's=0,1,1,1'#10 +
    'boundary=wrap'#10 +
    'symmetry=none'#10 +
    'directions=N,E,S,W,U,D'#10 +
    'values=1'#10 +
    'v=0,1,A'#10 +
    'relations=6'#10 +
    'r=N,0,0,1'#10 +
    'r=E,0,0,1'#10 +
    'r=S,0,0,1'#10 +
    'r=W,0,0,1'#10 +
    'r=U,0,0,1'#10 +
    'r=D,0,0,1'#10 +
    'end'#10;

  LEGACY_GOLDEN =
    'wfclearn=1'#10 +
    'name=Legacy'#10 +
    'license=MIT'#10 +
    'source=fixture'#10 +
    'kind=adjacency2d'#10 +
    'boundary=wrap'#10 +
    'symmetry=none'#10 +
    'footprint=0,0'#10 +
    'order=0'#10 +
    'samples=1'#10 +
    'sample=0,1,1,flat'#10 +
    'token=0,0,A'#10 +
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

function Metadata: TWfcTrainingMetadata;
begin
  Result := MakeWfcTrainingMetadata('Volume', 'MIT', 'fixture');
end;

function VolumeDocument(const ABoundary: TWfcModelBoundary;
  const ASymmetry: TWfcModelSymmetry): TWfcTrainingDocument;
var
  LOptions: TWfcTrainingOptions;
  LSamples: TWfcTrainingSamples;
begin
  SetLength(LSamples, 1);
  LSamples[0] := MakeWfcTrainingSample('one', 1, 1, 1, TokensOf(['A']));
  LOptions := MakeWfcTrainingOptions(wtkAdjacency3D, ABoundary,
    ASymmetry, 0, 0, 0);
  Result := TWfcTrainingDocument.Create(Metadata, LOptions, LSamples);
end;

function ReplaceText(const AText, AOld, ANew: String): String;
begin
  Result := StringReplace(AText, AOld, ANew, [rfReplaceAll]);
end;

procedure ExpectDecodeFailure(const AText, AMessage: String);
var
  LDocument: TWfcTrainingDocument;
  LRaised: Boolean;
begin
  LDocument := nil;
  LRaised := False;
  try
    try
      LDocument := DecodeWfcTrainingText(AText);
    except
      on E: Exception do
        LRaised := True;
    end;
  finally
    LDocument.Free;
  end;
  Check(LRaised, AMessage);
end;

procedure ExpectDocumentFailure(const AOptions: TWfcTrainingOptions;
  const ASamples: TWfcTrainingSamples; const AExpected, AMessage: String);
var
  LDocument: TWfcTrainingDocument;
  LError: String;
begin
  LDocument := nil;
  LError := '';
  try
    try
      LDocument := TWfcTrainingDocument.Create(Metadata, AOptions, ASamples);
    except
      on E: Exception do
        LError := E.Message;
    end;
  finally
    LDocument.Free;
  end;
  Check((LError <> '') and (Pos(AExpected, LError) > 0), AMessage);
end;

procedure TestSourceGoldenAndIdentity;
var
  LDecoded: TWfcTrainingDocument;
  LDocument: TWfcTrainingDocument;
  LSample: TWfcTrainingSample;
  LTokens: TWfcModelTokens;
begin
  Check((WFC_TRAINING_VERSION = 1) and
    (WFC_TRAINING_TEXT_VERSION = 4),
    'the legacy API version and latest source-text profile are pinned');
  Check((Ord(wtkAdjacency1D) = 0) and (Ord(wtkAdjacency2D) = 1) and
    (Ord(wtkPattern2D) = 2) and (Ord(wtkSequence) = 3) and
    (Ord(wtkAdjacency3D) = 4),
    'the volume kind is appended after all legacy enum ordinals');

  LTokens := TokensOf(['A']);
  LSample := MakeWfcTrainingSample('one', 1, 1, 1, LTokens);
  LTokens[0] := 'changed';
  Check((LSample.Depth = 1) and (LSample.Tokens[0] = 'A'),
    'the explicit-depth factory retains depth and detaches tokens');

  LDocument := VolumeDocument(wmbWrap, wmsNone);
  try
    Check(EncodeWfcTrainingText(LDocument) = SOURCE_GOLDEN,
      'adjacency3d source has exact canonical wfclearn=2 bytes');
    Check(WfcTrainingSignatureHex(LDocument.Signature) = 'EF40219B',
      'the volume source fingerprint golden is pinned (' +
      WfcTrainingSignatureHex(LDocument.Signature) + ')');
    LSample := LDocument.SampleAt(0);
    Check((LDocument.TotalTokenCount = 1) and (LSample.Width = 1) and
      (LSample.Height = 1) and (LSample.Depth = 1),
      'the immutable document retains its validated volume shape');
  finally
    LDocument.Free;
  end;

  LDecoded := DecodeWfcTrainingText(SOURCE_GOLDEN);
  try
    Check((LDecoded.CopyOptions.Kind = wtkAdjacency3D) and
      (LDecoded.SampleAt(0).Depth = 1),
      'wfclearn=2 decodes the volume kind and explicit depth');
    Check(EncodeWfcTrainingText(LDecoded) = SOURCE_GOLDEN,
      'wfclearn=2 round-trips byte-for-byte');
  finally
    LDecoded.Free;
  end;
end;

procedure TestLegacyNormalization;
var
  LDocument: TWfcTrainingDocument;
  LOptions: TWfcTrainingOptions;
  LSamples: TWfcTrainingSamples;
  LSignature: String;
begin
  LDocument := DecodeWfcTrainingText(LEGACY_GOLDEN);
  try
    LSignature := WfcTrainingSignatureHex(LDocument.Signature);
    Check((EncodeWfcTrainingText(LDocument) = LEGACY_GOLDEN) and
      (LDocument.SampleAt(0).Depth = 1) and
      (LSignature = '9EC95E17'),
      'legacy source bytes and fingerprint remain pinned while depth normalizes');
  finally
    LDocument.Free;
  end;

  SetLength(LSamples, 1);
  LSamples[0].Name := 'flat';
  LSamples[0].Width := 1;
  LSamples[0].Height := 1;
  LSamples[0].Tokens := TokensOf(['A']);
  LSamples[0].Depth := Low(Integer);
  LOptions := MakeWfcTrainingOptions(wtkAdjacency2D, wmbWrap,
    wmsNone, 0, 0, 0);
  LDocument := TWfcTrainingDocument.Create(
    MakeWfcTrainingMetadata('Legacy', 'MIT', 'fixture'),
    LOptions, LSamples);
  try
    Check((EncodeWfcTrainingText(LDocument) = LEGACY_GOLDEN) and
      (LDocument.SampleAt(0).Depth = 1) and
      (WfcTrainingSignatureHex(LDocument.Signature) = LSignature),
      'legacy construction ignores caller depth and preserves its fingerprint');
  finally
    LDocument.Free;
  end;

  LSamples[0] := MakeWfcTrainingSample('flat', 1, 1, TokensOf(['A']));
  Check(LSamples[0].Depth = 1,
    'the legacy sample factory initializes normalized depth one');
end;

procedure TestLearnedModelAndSymmetries;
const
  SYMMETRIES: array[0..3] of TWfcModelSymmetry =
    (wmsNone, wmsD4, wmsCubeRotations, wmsCubeFull);
  NAMES: array[0..3] of String =
    ('none', 'd4', 'cube24', 'cube48');
  COUNTS: array[0..3] of Integer = (1, 8, 24, 48);
var
  D: TWfcModelDirection;
  I: Integer;
  LDocument: TWfcTrainingDocument;
  LModel: TWfcModel;
  LText: String;
begin
  for I := Low(SYMMETRIES) to High(SYMMETRIES) do
  begin
    LDocument := VolumeDocument(wmbWrap, SYMMETRIES[I]);
    try
      Check(Pos('symmetry=' + NAMES[I] + #10,
        EncodeWfcTrainingText(LDocument)) > 0,
        'training symmetry has canonical name ' + NAMES[I]);
      LText := LearnWfcTrainingModelText(LDocument);
      if I = 0 then
        Check(LText = MODEL_GOLDEN,
          'the wrapped singleton learns exact canonical wfcm=3 bytes');
      LModel := DecodeWfcModelText(LText);
      try
        Check((LModel.Rank = 3) and (LModel.SampleDepth = 1) and
          (LModel.WeightAt(0) = COUNTS[I]),
          'the ' + NAMES[I] + ' learner retains rank and observation weight');
        for D := wmdNorth to wmdDown do
          Check(LModel.RelationCount(D, 0, 0) = COUNTS[I],
            NAMES[I] + ' retains the learned ' + IntToStr(Ord(D)) +
            ' direction count');
      finally
        LModel.Free;
      end;
    finally
      LDocument.Free;
    end;
  end;
end;

procedure TestCorpusShapesAndNoSeam;
var
  D: TWfcModelDirection;
  LDocument: TWfcTrainingDocument;
  LModel: TWfcModel;
  LOptions: TWfcTrainingOptions;
  LSamples: TWfcTrainingSamples;
  LShape: TWfcModelSampleShape;
begin
  SetLength(LSamples, 2);
  LSamples[0] := MakeWfcTrainingSample('deep', 1, 1, 2,
    TokensOf(['A', 'A']));
  LSamples[1] := MakeWfcTrainingSample('wide', 2, 1, 1,
    TokensOf(['B', 'B']));
  LOptions := MakeWfcTrainingOptions(wtkAdjacency3D, wmbOpen,
    wmsNone, 0, 0, 0);
  LDocument := TWfcTrainingDocument.Create(Metadata, LOptions, LSamples);
  try
    Check(LDocument.TotalTokenCount = 4,
      'validated training cell totals multiply width, height, and depth');
    LModel := DecodeWfcModelText(LearnWfcTrainingModelText(LDocument));
    try
      LShape := LModel.SampleShapeAt(0);
      Check((LModel.SampleCount = 2) and (LShape.Depth = 2) and
        (LModel.SampleShapeAt(1).Width = 2),
        'ordered training volume shapes reach the learned model');
      for D := wmdNorth to wmdDown do
        Check(LModel.RelationCount(D, 0, 1) = 0,
          'open corpus does not create a cross-sample seam in direction ' +
          IntToStr(Ord(D)));
    finally
      LModel.Free;
    end;
  finally
    LDocument.Free;
  end;
end;

procedure TestRecipeCompileAndRun;
var
  I: Integer;
  LCompiled: TWfcCompiledPipeline;
  LDocument: TWfcTrainingDocument;
  LLayer: TWfcPipelineResultLayer;
  LMetadata: TWfcPipelineMetadata;
  LPass: TWfcPipelinePass;
  LRecipe: TWfcPipelineModel;
  LResource: TWfcPipelineResource;
  LResult: TWfcPipelineResult;
  LRun: TWfcPipelineRun;
begin
  LDocument := VolumeDocument(wmbWrap, wmsNone);
  try
    LRecipe := LearnWfcTrainingRecipe(LDocument);
    try
      LMetadata := LRecipe.CopyMetadata;
      LResource := LRecipe.ResourceAt(0);
      LPass := LRecipe.PassAt(0);
      Check((LRecipe.Rank = 3) and LRecipe.WrapNeighbors and
        (LRecipe.ResourceCount = 1) and (LRecipe.PassCount = 1) and
        (LRecipe.DependencyCount = 0) and (LRecipe.BridgeCount = 0) and
        (LResource.Kind = wprkModel) and
        (LPass.Visibility = wppvPublic) and
        (LPass.AdapterKind = wpakModel),
        'adjacency3d exports one public rank-3 model pass');
      Check((LMetadata.SourceFingerprint = TWfcModelToken('wfclearn-v2/' +
        WfcTrainingSignatureHex(LDocument.Signature))) and
        (LMetadata.SourceFingerprint = LResource.SourceFingerprint) and
        (LMetadata.SourceDescription =
        'fixture | samples=0:one:1x1x1:C4996540'),
        'rank-3 recipe provenance uses the version-2 depth-aware fingerprint');
      Check((LRecipe.BorrowModelResource(0).Rank = 3) and
        (LRecipe.BorrowModelResource(0).SampleDepth = 1),
        'the recipe owns the decoded rank-3 model resource');

      LCompiled := CompileWfcPipeline(LRecipe, 2, 2, 2);
      try
        Check((LCompiled.Graph.Dimension.Width = 2) and
          (LCompiled.Graph.Dimension.Height = 2) and
          (LCompiled.Graph.Dimension.Depth = 2),
          'the learned recipe compiles for a true volume extent');
      finally
        LCompiled.Free;
      end;

      LRun := TWfcPipelineRun.Create(LRecipe, 2, 2, 2, 55,
        wpssOneWay, 65536, 0, False, nil, nil);
      try
        LResult := ExecuteWfcPipeline(LRecipe, LRun);
        try
          Check((LResult.Status = wprsSolved) and
            (LResult.LayerCount = 1) and (LResult.CellCount = 8),
            'the learned volume recipe solves end-to-end');
          LLayer := LResult.LayerAt(0);
          Check((LLayer.LabelName = 'output') and
            (Length(LLayer.Tokens) = 8),
            'the end-to-end result publishes one eight-cell output layer');
          for I := 0 to Length(LLayer.Tokens) - 1 do
            Check(LLayer.Tokens[I] = 'A',
              'the solved volume publishes its learned token at cell ' +
              IntToStr(I));
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

procedure TestProfileAndValidationFailures;
var
  LOptions: TWfcTrainingOptions;
  LSamples: TWfcTrainingSamples;
begin
  ExpectDecodeFailure(ReplaceText(SOURCE_GOLDEN,
    'wfclearn=2', 'wfclearn=1'),
    'wfclearn=1 rejects the volume kind');
  ExpectDecodeFailure(ReplaceText(SOURCE_GOLDEN,
    'kind=adjacency3d', 'kind=adjacency2d'),
    'wfclearn=2 rejects every legacy training kind');
  ExpectDecodeFailure(ReplaceText(SOURCE_GOLDEN,
    'sample=0,1,1,1,one', 'sample=0,1,1,one'),
    'wfclearn=2 requires explicit sample depth');
  ExpectDecodeFailure(ReplaceText(SOURCE_GOLDEN,
    'sample=0,1,1,1,one', 'sample=0,1,1,1,1,one'),
    'wfclearn=2 rejects extra sample fields');
  ExpectDecodeFailure(ReplaceText(SOURCE_GOLDEN,
    'sample=0,1,1,1,one', 'sample=0,1,1,0,one'),
    'wfclearn=2 rejects zero sample depth');
  ExpectDecodeFailure(ReplaceText(SOURCE_GOLDEN,
    'sample=0,1,1,1,one', 'sample=0,1,1,01,one'),
    'wfclearn=2 rejects noncanonical sample depth');
  ExpectDecodeFailure(ReplaceText(SOURCE_GOLDEN,
    'sample=0,1,1,1,one', 'sample=0,65536,65536,2,one'),
    'wfclearn=2 preflights volume cell overflow');
  ExpectDecodeFailure(ReplaceText(LEGACY_GOLDEN,
    'symmetry=none', 'symmetry=cube24'),
    'wfclearn=1 rejects cube symmetry');
  ExpectDecodeFailure(ReplaceText(SOURCE_GOLDEN,
    'wfclearn=2', 'wfclearn=3'),
    'unknown future source profiles are rejected');

  SetLength(LSamples, 1);
  LSamples[0] := MakeWfcTrainingSample('bad', 1, 1, 0, TokensOf(['A']));
  LOptions := MakeWfcTrainingOptions(wtkAdjacency3D, wmbOpen,
    wmsNone, 0, 0, 0);
  ExpectDocumentFailure(LOptions, LSamples, 'depth',
    'the immutable document rejects zero depth');

  LSamples[0] := MakeWfcTrainingSample('bad', 1, 1, 2, TokensOf(['A']));
  ExpectDocumentFailure(LOptions, LSamples, 'expected 2',
    'the immutable document validates volume token count');

  LSamples[0] := MakeWfcTrainingSample('bad', 65536, 65536, 2,
    TokensOf(['A']));
  ExpectDocumentFailure(LOptions, LSamples, 'cell count',
    'the immutable document rejects overflowing volume shape work');

  LSamples[0] := MakeWfcTrainingSample('bad', 1, 1, 1, TokensOf(['A']));
  LOptions := MakeWfcTrainingOptions(wtkAdjacency3D, wmbOpen,
    wmsNone, 1, 0, 0);
  ExpectDocumentFailure(LOptions, LSamples, 'footprint',
    'adjacency3d rejects a nonzero footprint');
  LOptions := MakeWfcTrainingOptions(wtkAdjacency3D, wmbOpen,
    wmsNone, 0, 0, 1);
  ExpectDocumentFailure(LOptions, LSamples, 'order',
    'adjacency3d rejects sequence order');

  LSamples[0] := MakeWfcTrainingSample('bad', 1, 1, TokensOf(['A']));
  LOptions := MakeWfcTrainingOptions(wtkAdjacency2D, wmbOpen,
    wmsCubeFull, 0, 0, 0);
  ExpectDocumentFailure(LOptions, LSamples, 'none or d4',
      'legacy adjacency training rejects cube symmetry');
end;

{$IFDEF PAS2JS}
procedure TestMalformedBrowserVolumeAxes;
const
  AXIS_NAMES: array[0..2] of String = ('width', 'height', 'depth');
var
  I: Integer;
  LOptions: TWfcTrainingOptions;
  LSamples: TWfcTrainingSamples;
begin
  LOptions := MakeWfcTrainingOptions(wtkAdjacency3D, wmbOpen,
    wmsNone, 0, 0, 0);
  for I := 0 to 2 do
  begin
    SetLength(LSamples, 1);
    LSamples[0] := MakeWfcTrainingSample('bad', 2, 2, 2,
      TokensOf(['A', 'A', 'A', 'A', 'A', 'A']));
    case I of
      0: asm LSamples[0].Width = 1.5; end;
      1: asm LSamples[0].Height = 1.5; end;
      2: asm LSamples[0].Depth = 1.5; end;
    end;
    ExpectDocumentFailure(LOptions, LSamples, 'exact integers',
      'the immutable volume document rejects fractional ' + AXIS_NAMES[I]);
  end;
end;
{$ENDIF}

begin
  WriteLn('WFC rank-3 training integration suite');
  WriteLn('=====================================');
  RunTest('source golden and identity', @TestSourceGoldenAndIdentity);
  RunTest('legacy normalization', @TestLegacyNormalization);
  RunTest('learned model and symmetries', @TestLearnedModelAndSymmetries);
  RunTest('corpus shapes and seam policy', @TestCorpusShapesAndNoSeam);
  RunTest('recipe compile and volume run', @TestRecipeCompileAndRun);
  RunTest('profile and validation failures', @TestProfileAndValidationFailures);
  {$IFDEF PAS2JS}
  RunTest('browser fractional volume axes', @TestMalformedBrowserVolumeAxes);
  {$ENDIF}
  WriteLn('=====================================');
  WriteLn('Checks: ', GCheckCount, '  Failures: ', GFailureCount);
  if GFailureCount <> 0 then
  begin
    {$IFDEF PAS2JS}
    raise Exception.CreateFmt('%d rank-3 training checks failed',
      [GFailureCount]);
    {$ELSE}
    Halt(1);
    {$ENDIF}
  end;
end.
