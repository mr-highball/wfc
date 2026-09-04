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
program wfc_pipeline_result_text_test;

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
  wfc_pipeline_result_text;

type
  TTestProcedure = procedure;

const
  EXPECTED_RECIPE_SIGNATURE = '2EB37D18';
  EXPECTED_RUN_SIGNATURE = '8346494C';
  EXPECTED_RESULT_SIGNATURE = '93EFCC36';

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

function NoteToken: TWfcModelToken;
begin
  {$IFDEF PAS2JS}
  Result := TWfcModelToken(Chr($266B));
  {$ELSE}
  Result := TWfcModelToken(UTF8Encode(
    UnicodeString(WideChar($266B))));
  {$ENDIF}
end;

function BuildRecipe: TWfcPipelineModel;
var
  LPasses: TWfcPipelinePasses;
  LResources: TWfcPipelineResources;
  LRows: TWfcRuleRows;
  LRule: TWfcRuleModel;
begin
  LRows := nil;
  LRule := TWfcRuleModel.Create(1,
    TokensOf([TWfcModelToken('A'), TWfcModelToken('x,y'), NoteToken]),
    IntegersOf([1, 2, 3]), LRows);
  try
    SetLength(LResources, 1);
    LResources[0] := MakeWfcPipelineResource('rules', wprkRules,
      EncodeWfcRuleText(LRule), 'result text tests', 'MIT', 'fixture');
  finally
    LRule.Free;
  end;
  SetLength(LPasses, 2);
  LPasses[0] := MakeWfcPipelinePass('latent', wppvPrivate,
    gpmOverlay, WFC_PIPELINE_NO_INDEX, wpakRules, 0, False, wseWhole);
  LPasses[1] := MakeWfcPipelinePass('public,x', wppvPublic,
    gpmOverlay, WFC_PIPELINE_NO_INDEX, wpakRules, 0, False, wseWhole);
  Result := TWfcPipelineModel.Create(
    MakeWfcPipelineMetadata('result text fixture', 'MIT', '', ''),
    1, False, rmBottomUp, LResources, LPasses, nil, nil, nil);
end;

function BuildRun(const ARecipe: TWfcPipelineModel;
  const AStrategy: TWfcPipelineSolveStrategy = wpssOneWay):
  TWfcPipelineRun;
var
  LMaxPassBacktracks: Integer;
begin
  if AStrategy = wpssNegotiated then
    LMaxPassBacktracks := 3
  else
    LMaxPassBacktracks := 0;
  Result := TWfcPipelineRun.Create(ARecipe, 2, 1, 1,
    High(Cardinal), AStrategy, 7, LMaxPassBacktracks, True, nil, nil);
end;

function SolvedOutcomes: TWfcPipelinePassOutcomes;
begin
  Result := nil;
  SetLength(Result, 2);
  Result[0].PassIndex := 0;
  Result[0].Decisions := 1;
  Result[0].Propagations := 2;
  Result[0].Executed := True;
  Result[0].ExecutionOrdinal := 0;
  Result[0].Disposition := gpdSolved;
  Result[1].PassIndex := 1;
  Result[1].Decisions := 3;
  Result[1].Propagations := 4;
  Result[1].Contradictions := 1;
  Result[1].Backtracks := 1;
  Result[1].ExcludedAssignments := 1;
  Result[1].Executed := True;
  Result[1].ExecutionOrdinal := 1;
  Result[1].Disposition := gpdSolved;
end;

function SolvedLayers: TWfcPipelineResultLayers;
begin
  Result := nil;
  SetLength(Result, 1);
  Result[0] := MakeWfcPipelineResultLayer(1, 'public,x',
    TokensOf([TWfcModelToken('x,y'), NoteToken]));
end;

function BuildSolvedResult(const ARecipe: TWfcPipelineModel;
  const ARun: TWfcPipelineRun): TWfcPipelineResult;
begin
  Result := TWfcPipelineResult.Create(ARecipe, ARun,
    CurrentWfcPipelineResultVersions, wprsSolved, 0, wpekTrace,
    Cardinal($89ABCDEF), EmptyWfcPipelineFailure, SolvedOutcomes,
    SolvedLayers);
end;

function ExpectedText: String;
begin
  Result :=
    'wfcpipeline-result=1'#10 +
    'recipe-signature=' + EXPECTED_RECIPE_SIGNATURE + #10 +
    'run-signature=' + EXPECTED_RUN_SIGNATURE + #10 +
    'graph-model-version=1'#10 +
    'random-algorithm-version=1'#10 +
    'solver-algorithm-version=2'#10 +
    'pipeline-algorithm-version=2'#10 +
    'trace-version=1'#10 +
    'trace-hash-version=1'#10 +
    'negotiation-algorithm-version=1'#10 +
    'negotiation-hash-version=1'#10 +
    'width=2'#10 +
    'height=1'#10 +
    'depth=1'#10 +
    'seed=4294967295'#10 +
    'strategy=one-way'#10 +
    'max-backtracks=7'#10 +
    'max-pass-backtracks=0'#10 +
    'trace=true'#10 +
    'status=solved'#10 +
    'pass-backtracks=0'#10 +
    'evidence=trace'#10 +
    'evidence-signature=89ABCDEF'#10 +
    'failure-kind=none'#10 +
    'failure-pass=-1'#10 +
    'failure-entry=-1'#10 +
    'failure-neighbor=-1'#10 +
    'failure-direction-present=false'#10 +
    'failure-direction=N'#10 +
    'failure-dependency-pass=-1'#10 +
    'passes=2'#10 +
    'pass=0,true,0,solved,1,2,0,0,0'#10 +
    'pass=1,true,1,solved,3,4,1,1,1'#10 +
    'layers=1'#10 +
    'layer=0,1,public%2Cx,2'#10 +
    'value=0,0,x%2Cy'#10 +
    'value=0,1,%E2%99%AB'#10 +
    'signature=' + EXPECTED_RESULT_SIGNATURE + #10 +
    'end'#10;
end;

function ReplaceOnce(const AText, AOld, ANew: String): String;
var
  LPosition: Integer;
begin
  LPosition := Pos(AOld, AText);
  if LPosition = 0 then
    raise Exception.Create('test replacement text was not found: ' + AOld);
  Result := Copy(AText, 1, LPosition - 1) + ANew +
    Copy(AText, LPosition + Length(AOld),
      Length(AText) - LPosition - Length(AOld) + 1);
end;

procedure ExpectDecodeRejected(const AText: String;
  const ARecipe: TWfcPipelineModel; const ARun: TWfcPipelineRun;
  const ALabel: String);
var
  LDecoded: TWfcPipelineResult;
  LRejected: Boolean;
begin
  LDecoded := nil;
  LRejected := False;
  try
    try
      LDecoded := DecodeWfcPipelineResultText(AText, ARecipe, ARun);
    except
      on EConvertError do
        LRejected := True;
    end;
  finally
    LDecoded.Free;
  end;
  Check(LRejected, ALabel);
end;

procedure TestCanonicalRoundTrip;
var
  LDecoded: TWfcPipelineResult;
  LEncoded: String;
  LLayer: TWfcPipelineResultLayer;
  LRecipe: TWfcPipelineModel;
  LResult: TWfcPipelineResult;
  LRun: TWfcPipelineRun;
begin
  LRecipe := BuildRecipe;
  LRun := nil;
  LResult := nil;
  LDecoded := nil;
  try
    LRun := BuildRun(LRecipe);
    LResult := BuildSolvedResult(LRecipe, LRun);
    LEncoded := EncodeWfcPipelineResultText(LResult);
    Check(WfcPipelineSignatureHex(LRecipe.Signature) =
      EXPECTED_RECIPE_SIGNATURE,
      'fixture recipe signature matches the portable golden');
    Check(WfcPipelineRunSignatureHex(LRun.Signature) =
      EXPECTED_RUN_SIGNATURE,
      'fixture run signature matches the portable golden');
    Check(WfcPipelineResultSignatureHex(LResult.Signature) =
      EXPECTED_RESULT_SIGNATURE,
      'fixture result signature matches the portable golden');
    Check(LEncoded = ExpectedText,
      'encoder emits the exact canonical version-1 document');
    Check(Pos('layer=0,1,public%2Cx,2'#10, LEncoded) <> 0,
      'layer labels use canonical uppercase percent escaping');
    Check(Pos('value=0,1,%E2%99%AB'#10, LEncoded) <> 0,
      'Unicode scalar tokens encode as canonical UTF-8 bytes');
    LDecoded := DecodeWfcPipelineResultText(LEncoded, LRecipe, LRun);
    Check(LDecoded.Signature = LResult.Signature,
      'decoder restores the semantic result signature');
    Check(EncodeWfcPipelineResultText(LDecoded) = LEncoded,
      'decoded results re-encode byte for byte');
    LLayer := LDecoded.LayerAt(0);
    Check((LLayer.PassIndex = 1) and (LLayer.LabelName = 'public,x') and
      (Length(LLayer.Tokens) = 2) and
      (LLayer.Tokens[0] = 'x,y') and (LLayer.Tokens[1] = NoteToken),
      'decoded result restores public layer identity and row-major values');
    Check((LDecoded.RecipeSignature = LRecipe.Signature) and
      (LDecoded.RunSignature = LRun.Signature) and
      (LDecoded.Seed = High(Cardinal)),
      'decoded result remains bound to recipe, run, and full seed range');
    WriteLn('  recipe-signature=',
      WfcPipelineSignatureHex(LRecipe.Signature));
    WriteLn('  run-signature=',
      WfcPipelineRunSignatureHex(LRun.Signature));
    WriteLn('  result-signature=',
      WfcPipelineResultSignatureHex(LResult.Signature));
  finally
    LDecoded.Free;
    LResult.Free;
    LRun.Free;
    LRecipe.Free;
  end;
end;

procedure TestEnvelopeAndScalarRejection;
var
  LEncoded: String;
  LNilRejected: Boolean;
  LRecipe: TWfcPipelineModel;
  LResult: TWfcPipelineResult;
  LRun: TWfcPipelineRun;
begin
  LRecipe := BuildRecipe;
  LRun := nil;
  LResult := nil;
  try
    LRun := BuildRun(LRecipe);
    LResult := BuildSolvedResult(LRecipe, LRun);
    LEncoded := EncodeWfcPipelineResultText(LResult);
    ExpectDecodeRejected(ReplaceOnce(LEncoded,
      'wfcpipeline-result=1', 'wfcpipeline-result=2'), LRecipe, LRun,
      'unknown artifact versions are rejected');
    ExpectDecodeRejected(ReplaceOnce(LEncoded,
      'recipe-signature=', 'recipe-signature=0'), LRecipe, LRun,
      'malformed recipe signatures are rejected');
    ExpectDecodeRejected(ReplaceOnce(LEncoded,
      WfcPipelineSignatureHex(LRecipe.Signature), '00000000'),
      LRecipe, LRun, 'results bound to another recipe are rejected');
    ExpectDecodeRejected(ReplaceOnce(LEncoded,
      WfcPipelineRunSignatureHex(LRun.Signature), '00000000'),
      LRecipe, LRun, 'results bound to another run are rejected');
    ExpectDecodeRejected(ReplaceOnce(LEncoded,
      'graph-model-version=1', 'graph-model-version=01'), LRecipe, LRun,
      'leading-zero versions are rejected');
    ExpectDecodeRejected(ReplaceOnce(LEncoded,
      'width=2', 'width=02'), LRecipe, LRun,
      'leading-zero dimensions are rejected');
    ExpectDecodeRejected(ReplaceOnce(LEncoded,
      'seed=4294967295', 'seed=4294967296'), LRecipe, LRun,
      'seeds beyond Cardinal are rejected');
    ExpectDecodeRejected(ReplaceOnce(LEncoded,
      'strategy=one-way', 'strategy=best'), LRecipe, LRun,
      'unknown strategies are rejected');
    ExpectDecodeRejected(ReplaceOnce(LEncoded,
      'trace=true', 'trace=1'), LRecipe, LRun,
      'noncanonical booleans are rejected');
    ExpectDecodeRejected(ReplaceOnce(LEncoded,
      'status=solved', 'status=maybe'), LRecipe, LRun,
      'unknown result statuses are rejected');
    ExpectDecodeRejected(ReplaceOnce(LEncoded,
      'evidence=trace', 'evidence=proof'), LRecipe, LRun,
      'unknown evidence kinds are rejected');
    ExpectDecodeRejected(ReplaceOnce(LEncoded,
      'evidence-signature=89ABCDEF', 'evidence-signature=89abcdef'),
      LRecipe, LRun, 'lowercase evidence signatures are rejected');
    ExpectDecodeRejected(ReplaceOnce(LEncoded,
      'failure-direction=N', 'failure-direction=n'), LRecipe, LRun,
      'unknown direction spellings are rejected');
    ExpectDecodeRejected(ReplaceOnce(LEncoded,
      'signature=' + WfcPipelineResultSignatureHex(LResult.Signature),
      'signature=00000000'), LRecipe, LRun,
      'semantic signature mismatches are rejected');
    ExpectDecodeRejected(ReplaceOnce(LEncoded,
      WfcPipelineResultSignatureHex(LResult.Signature),
      LowerCase(WfcPipelineResultSignatureHex(LResult.Signature))),
      LRecipe, LRun, 'lowercase result signatures are rejected');
    ExpectDecodeRejected(ReplaceOnce(LEncoded, 'end'#10, 'stop'#10),
      LRecipe, LRun, 'missing end markers are rejected');
    ExpectDecodeRejected(LEncoded + 'extra'#10, LRecipe, LRun,
      'trailing records are rejected');
    ExpectDecodeRejected(StringReplace(LEncoded, #10, #13#10,
      [rfReplaceAll]), LRecipe, LRun, 'CRLF documents are rejected');
    ExpectDecodeRejected(Copy(LEncoded, 1, Length(LEncoded) - 1),
      LRecipe, LRun, 'a missing final LF is rejected');

    LNilRejected := False;
    try
      DecodeWfcPipelineResultText(LEncoded, nil, LRun);
    except
      on EConvertError do
        LNilRejected := True;
    end;
    Check(LNilRejected, 'decoding requires the referenced recipe');
    LNilRejected := False;
    try
      DecodeWfcPipelineResultText(LEncoded, LRecipe, nil);
    except
      on EConvertError do
        LNilRejected := True;
    end;
    Check(LNilRejected, 'decoding requires the referenced run');
    LNilRejected := False;
    try
      EncodeWfcPipelineResultText(nil);
    except
      on EArgumentNilException do
        LNilRejected := True;
    end;
    Check(LNilRejected, 'encoding rejects nil result objects');
  finally
    LResult.Free;
    LRun.Free;
    LRecipe.Free;
  end;
end;

procedure TestRecordAndSemanticRejection;
var
  LEncoded: String;
  LRecipe: TWfcPipelineModel;
  LResult: TWfcPipelineResult;
  LRun: TWfcPipelineRun;
begin
  LRecipe := BuildRecipe;
  LRun := nil;
  LResult := nil;
  try
    LRun := BuildRun(LRecipe);
    LResult := BuildSolvedResult(LRecipe, LRun);
    LEncoded := EncodeWfcPipelineResultText(LResult);
    ExpectDecodeRejected(ReplaceOnce(LEncoded,
      'passes=2', 'passes=1025'), LRecipe, LRun,
      'oversized pass record counts are rejected before allocation');
    ExpectDecodeRejected(ReplaceOnce(LEncoded,
      'pass=0,true,0,solved,1,2,0,0,0',
      'pass=1,true,0,solved,1,2,0,0,0'), LRecipe, LRun,
      'pass record indices must be complete and ordered');
    ExpectDecodeRejected(ReplaceOnce(LEncoded,
      'pass=0,true,0,solved,1,2,0,0,0',
      'pass=0,true,0,solved,1,2,0,0'), LRecipe, LRun,
      'missing pass record fields are rejected');
    ExpectDecodeRejected(ReplaceOnce(LEncoded,
      'pass=0,true,0,solved,1,2,0,0,0',
      'pass=0,true,0,reused,1,2,0,0,0'), LRecipe, LRun,
      'semantically impossible reused outcomes are rejected');
    ExpectDecodeRejected(ReplaceOnce(LEncoded,
      'layers=1', 'layers=257'), LRecipe, LRun,
      'oversized public-layer counts are rejected before allocation');
    ExpectDecodeRejected(ReplaceOnce(LEncoded,
      'layer=0,1,public%2Cx,2', 'layer=1,1,public%2Cx,2'),
      LRecipe, LRun, 'public-layer indices must be complete and ordered');
    ExpectDecodeRejected(ReplaceOnce(LEncoded,
      'layer=0,1,public%2Cx,2', 'layer=0,0,public%2Cx,2'),
      LRecipe, LRun, 'private passes cannot be decoded as output layers');
    ExpectDecodeRejected(ReplaceOnce(LEncoded,
      'layer=0,1,public%2Cx,2', 'layer=0,1,public%2cx,2'),
      LRecipe, LRun, 'lowercase percent escapes are rejected');
    ExpectDecodeRejected(ReplaceOnce(LEncoded,
      'value=0,0,x%2Cy', 'value=1,0,x%2Cy'), LRecipe, LRun,
      'public value layer indices must be ordered');
    ExpectDecodeRejected(ReplaceOnce(LEncoded,
      'value=0,0,x%2Cy', 'value=0,1,x%2Cy'), LRecipe, LRun,
      'public value cell indices must be ordered');
    ExpectDecodeRejected(ReplaceOnce(LEncoded,
      'value=0,0,x%2Cy', 'value=0,0,missing'), LRecipe, LRun,
      'unknown output tokens are rejected');
    ExpectDecodeRejected(ReplaceOnce(LEncoded,
      'layer=0,1,public%2Cx,2', 'layer=0,1,public%2Cx,3'),
      LRecipe, LRun, 'incomplete public value records are rejected');
    ExpectDecodeRejected(ReplaceOnce(LEncoded,
      'value=0,0,x%2Cy', 'value=0,0,A'), LRecipe, LRun,
      'semantic result changes require a matching result signature');
    ExpectDecodeRejected(ReplaceOnce(LEncoded,
      'passes=2'#10 + 'pass=0,true,0,solved,1,2,0,0,0',
      'passes=0'#10 + 'pass=0,true,0,solved,1,2,0,0,0'),
      LRecipe, LRun, 'inconsistent record counts cannot shift parsing');
  finally
    LResult.Free;
    LRun.Free;
    LRecipe.Free;
  end;
end;

procedure TestFailureAndNegotiationRoundTrips;
var
  LDecoded: TWfcPipelineResult;
  LFailure: TWfcPipelineFailure;
  LOutcomes: TWfcPipelinePassOutcomes;
  LRecipe: TWfcPipelineModel;
  LResult: TWfcPipelineResult;
  LRun: TWfcPipelineRun;
  LText: String;
begin
  LRecipe := BuildRecipe;
  LRun := nil;
  LResult := nil;
  LDecoded := nil;
  try
    LRun := BuildRun(LRecipe);
    LFailure := EmptyWfcPipelineFailure;
    LFailure.Kind := gckAdjacency;
    LFailure.PassIndex := 0;
    LFailure.EntryIndex := 1;
    LFailure.NeighborIndex := 0;
    LFailure.HasDirection := True;
    LFailure.Direction := gdWest;
    LOutcomes := SolvedOutcomes;
    LOutcomes[0].Disposition := gpdFailed;
    LOutcomes[0].Contradictions := 1;
    LOutcomes[1] := Default(TWfcPipelinePassOutcome);
    LOutcomes[1].PassIndex := 1;
    LOutcomes[1].ExecutionOrdinal := -1;
    LOutcomes[1].Disposition := gpdNotRun;
    LResult := TWfcPipelineResult.Create(LRecipe, LRun,
      CurrentWfcPipelineResultVersions, wprsContradiction, 0,
      wpekTrace, Cardinal($10203040), LFailure, LOutcomes, nil);
    LText := EncodeWfcPipelineResultText(LResult);
    LDecoded := DecodeWfcPipelineResultText(LText, LRecipe, LRun);
    Check((LDecoded.Status = wprsContradiction) and
      (LDecoded.CopyFailure.Kind = gckAdjacency) and
      (LDecoded.LayerCount = 0),
      'failed result text round-trips structured failure without partial data');
    Check(EncodeWfcPipelineResultText(LDecoded) = LText,
      'failed result text re-encodes byte for byte');
    FreeAndNil(LDecoded);
    FreeAndNil(LResult);
    FreeAndNil(LRun);

    LRun := BuildRun(LRecipe, wpssNegotiated);
    LResult := TWfcPipelineResult.Create(LRecipe, LRun,
      CurrentWfcPipelineResultVersions, wprsSolved, 2,
      wpekNegotiationTranscript, Cardinal($50607080),
      EmptyWfcPipelineFailure, SolvedOutcomes,
      SolvedLayers);
    LText := EncodeWfcPipelineResultText(LResult);
    LDecoded := DecodeWfcPipelineResultText(LText, LRecipe, LRun);
    Check((LDecoded.Strategy = wpssNegotiated) and
      (LDecoded.PassBacktracks = 2) and
      (LDecoded.EvidenceKind = wpekNegotiationTranscript) and
      (LDecoded.EvidenceSignature = Cardinal($50607080)),
      'negotiated result text round-trips its pass search provenance');
  finally
    LDecoded.Free;
    LResult.Free;
    LRun.Free;
    LRecipe.Free;
  end;
end;

begin
  RunTest('canonical round trip', TestCanonicalRoundTrip);
  RunTest('envelope and scalar rejection', TestEnvelopeAndScalarRejection);
  RunTest('record and semantic rejection', TestRecordAndSemanticRejection);
  RunTest('failure and negotiation round trips',
    TestFailureAndNegotiationRoundTrips);
  WriteLn('Checks: ', GCheckCount, ', Failures: ', GFailureCount);
  if GFailureCount <> 0 then
  begin
    {$IFDEF PAS2JS}
    raise Exception.CreateFmt('%d pipeline-result-text checks failed',
      [GFailureCount]);
    {$ELSE}
    Halt(1);
    {$ENDIF}
  end;
end.
