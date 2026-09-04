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
program wfc_pipeline_text_test;

{$mode delphi}{$H+}

uses
  SysUtils,
  wfc,
  wfc_model,
  wfc_rule_model,
  wfc_rule_text,
  wfc_pattern2d,
  wfc_pattern2d_learn,
  wfc_pattern2d_text,
  wfc_sequence,
  wfc_pipeline_model,
  wfc_pipeline_text,
  wfc_text_codec;

type
  TTestProcedure = procedure;

const
  SIMPLE_MODEL_TEXT =
    'wfcm=1'#10 +
    'rank=1'#10 +
    'width=2'#10 +
    'height=1'#10 +
    'boundary=open'#10 +
    'symmetry=none'#10 +
    'directions=E,W'#10 +
    'values=2'#10 +
    'v=0,1,m0'#10 +
    'v=1,1,m1'#10 +
    'relations=2'#10 +
    'r=E,0,1,1'#10 +
    'r=W,1,0,1'#10 +
    'end'#10;

  PATTERN_TEXT =
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

  SEQUENCE_TEXT =
    'wfcs=1'#10 +
    'order=2'#10 +
    'samples=2'#10 +
    's=0,3'#10 +
    's=1,3'#10 +
    'tokens=3'#10 +
    't=0,A'#10 +
    't=1,B'#10 +
    't=2,C'#10 +
    'states=5'#10 +
    'q=0,2,2,0,B,E0'#10 +
    'q=1,1,0,0,T0,E1'#10 +
    'q=2,1,0,1,T1,E0'#10 +
    'q=3,1,0,0,T0,E2'#10 +
    'q=4,1,0,1,T2,E0'#10 +
    'end'#10;

  MINIMAL_PIPELINE_TEXT =
    'wfcpipeline=1'#10 +
    'name=codec'#10 +
    'license=MIT'#10 +
    'source='#10 +
    'fingerprint='#10 +
    'graph-model-version=1'#10 +
    'random-algorithm-version=1'#10 +
    'solver-algorithm-version=2'#10 +
    'pipeline-algorithm-version=2'#10 +
    'bundle-graph-adapter-version=1'#10 +
    'model-graph-adapter-version=1'#10 +
    'rules-graph-adapter-version=1'#10 +
    'pattern2d-graph-adapter-version=1'#10 +
    'sequence-graph-adapter-version=1'#10 +
    'pattern2d-bridge-version=2'#10 +
    'sequence-bridge-version=2'#10 +
    'rank=1'#10 +
    'wrap=false'#10 +
    'traversal=bottom-up'#10 +
    'resources=1'#10 +
    'resource=0,basic-rules,rules,' +
      'wfcrules%3D1%0Arank%3D1%0Avalues%3D1%0A' +
      'v%3D0%2C1%2Conly%0Arules%3D0%0A' +
      'signature%3DD83BEE6A%0Aend%0A,' +
      'pipeline%20codec%20test,MIT,'#10 +
    'passes=1'#10 +
    'pass=0,layer,public,legacy,-1,rules,0,false,whole'#10 +
    'dependencies=0'#10 +
    'bridges=0'#10 +
    'requirements=0'#10 +
    'signature=81DC669F'#10 +
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

function ReplaceOnce(const AText, AOld,
  ANew: String): String;
var
  LPosition: Integer;
begin
  LPosition := Pos(AOld, AText);
  if LPosition = 0 then
    raise Exception.Create('test fixture replacement text was not found: ' +
      AOld);
  Result := Copy(AText, 1, LPosition - 1) + ANew +
    Copy(AText, LPosition + Length(AOld),
      Length(AText) - LPosition - Length(AOld) + 1);
end;

procedure ExpectDecodeRejected(const AText, ALabel: String);
var
  LCorrectClass: Boolean;
  LDecoded: TWfcPipelineModel;
  LRaised: Boolean;
begin
  LCorrectClass := False;
  LDecoded := nil;
  LRaised := False;
  try
    try
      LDecoded := DecodeWfcPipelineModelText(AText);
    except
      on E: Exception do
      begin
        LRaised := True;
        LCorrectClass := E is EConvertError;
      end;
    end;
  finally
    LDecoded.Free;
  end;
  Check(LRaised and LCorrectClass, ALabel);
end;

procedure ExpectDecodeRejectedContaining(const AText,
  AExpectedFragment, ALabel: String);
var
  LDecoded: TWfcPipelineModel;
  LMatched: Boolean;
begin
  LDecoded := nil;
  LMatched := False;
  try
    try
      LDecoded := DecodeWfcPipelineModelText(AText);
    except
      on E: Exception do
        LMatched := (E is EConvertError) and
          (Pos(AExpectedFragment, E.Message) > 0);
    end;
  finally
    LDecoded.Free;
  end;
  Check(LMatched, ALabel);
end;

function RuleDocument(const ATokens: TWfcModelTokens;
  const AWeights: TWfcModelIntegerArray): String;
var
  LModel: TWfcRuleModel;
  LRows: TWfcRuleRows;
begin
  LRows := nil;
  LModel := TWfcRuleModel.Create(1, ATokens, AWeights, LRows);
  try
    Result := EncodeWfcRuleText(LModel);
  finally
    LModel.Free;
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

function BuildMinimalFixture: TWfcPipelineModel;
var
  LBridges: TWfcPipelineBridges;
  LDependencies: TWfcPipelineDependencies;
  LMetadata: TWfcPipelineMetadata;
  LPasses: TWfcPipelinePasses;
  LRequirements: TWfcPipelineRequirements;
  LResources: TWfcPipelineResources;
begin
  LMetadata := MakeWfcPipelineMetadata('codec', 'MIT', '', '');
  SetLength(LResources, 1);
  LResources[0] := MakeWfcPipelineResource('basic-rules', wprkRules,
    RuleDocument(TokensOf(['only']), IntegersOf([1])),
    'pipeline codec test', 'MIT', '');
  SetLength(LPasses, 1);
  LPasses[0] := MakeWfcPipelinePass('layer', wppvPublic, gpmLegacy,
    WFC_PIPELINE_NO_INDEX, wpakRules, 0, False, wseWhole);
  LDependencies := nil;
  LBridges := nil;
  LRequirements := nil;
  Result := TWfcPipelineModel.Create(LMetadata, 1, False,
    rmBottomUp, LResources, LPasses, LDependencies, LBridges,
    LRequirements);
end;

function BuildComprehensiveFixture: TWfcPipelineModel;
var
  LBridges: TWfcPipelineBridges;
  LDependencies: TWfcPipelineDependencies;
  LMetadata: TWfcPipelineMetadata;
  LPasses: TWfcPipelinePasses;
  LRequirements: TWfcPipelineRequirements;
  LResources: TWfcPipelineResources;
  LTerms: TWfcPipelineRequirementTerms;
begin
  LMetadata := MakeWfcPipelineMetadata('portable,pipeline ' + NoteToken,
    'MIT', 'source'#10'description', 'fnv:test');
  SetLength(LResources, 3);
  LResources[0] := MakeWfcPipelineResource('provider,rules', wprkRules,
    RuleDocument(TokensOf([',', NoteToken]), IntegersOf([1, 2])),
    'provider source', 'MIT', 'provider:1');
  LResources[1] := MakeWfcPipelineResource('consumer-rules', wprkRules,
    RuleDocument(TokensOf(['tree', 'house']), IntegersOf([3, 4])),
    'consumer source', 'MIT', '');
  LResources[2] := MakeWfcPipelineResource('model', wprkModel,
    SIMPLE_MODEL_TEXT, 'model source', 'MIT', 'model:1');

  SetLength(LPasses, 4);
  LPasses[0] := MakeWfcPipelinePass('provider', wppvPublic,
    gpmLegacy, WFC_PIPELINE_NO_INDEX, wpakRules, 0, False, wseWhole);
  LPasses[1] := MakeWfcPipelinePass('consumer', wppvPublic,
    gpmOverlay, WFC_PIPELINE_NO_INDEX, wpakRules, 1, False, wseWhole);
  LPasses[2] := MakeWfcPipelinePass('copy', wppvPublic,
    gpmTransform, 1, wpakEmpty, WFC_PIPELINE_NO_INDEX, False, wseWhole);
  LPasses[3] := MakeWfcPipelinePass('learned', wppvPublic,
    gpmOverlay, WFC_PIPELINE_NO_INDEX, wpakModel, 2, False, wseWhole);

  SetLength(LDependencies, 2);
  LDependencies[0] := MakeWfcPipelineDependency(1, 0);
  LDependencies[1] := MakeWfcPipelineDependency(2, 1);
  LBridges := nil;

  SetLength(LRequirements, 2);
  SetLength(LTerms, 1);
  LTerms[0] := MakeWfcPipelineRequirementTerm(Low(Integer), 0, 0,
    TokensOf([',', NoteToken]));
  LRequirements[0] := MakeWfcPipelineRequirement(1, 'tree', 0,
    wprqExact, LTerms);
  SetLength(LTerms, 2);
  LTerms[0] := MakeWfcPipelineRequirementTerm(Low(Integer) + 1, 0, 0,
    TokensOf([',']));
  LTerms[1] := MakeWfcPipelineRequirementTerm(High(Integer), 0, 0,
    TokensOf([NoteToken]));
  LRequirements[1] := MakeWfcPipelineRequirement(1, 'house', 0,
    wprqAny, LTerms);

  Result := TWfcPipelineModel.Create(LMetadata, 1, False,
    rmTopDown, LResources, LPasses, LDependencies, LBridges,
    LRequirements);
end;

function BuildPatternBridgeFixture: TWfcPipelineModel;
var
  LBridges: TWfcPipelineBridges;
  LDependencies: TWfcPipelineDependencies;
  LPasses: TWfcPipelinePasses;
  LRequirements: TWfcPipelineRequirements;
  LResources: TWfcPipelineResources;
begin
  SetLength(LResources, 1);
  LResources[0] := MakeWfcPipelineResource('pattern', wprkPattern2D,
    PATTERN_TEXT, 'pattern source', 'MIT', '');
  SetLength(LPasses, 2);
  LPasses[0] := MakeWfcPipelinePass('patterns', wppvPrivate,
    gpmLegacy, WFC_PIPELINE_NO_INDEX, wpakPattern2D, 0, False, wseWhole);
  LPasses[1] := MakeWfcPipelinePass('projection', wppvPublic,
    gpmOverlay, WFC_PIPELINE_NO_INDEX, wpakEmpty,
    WFC_PIPELINE_NO_INDEX, False, wseWhole);
  SetLength(LDependencies, 1);
  LDependencies[0] := MakeWfcPipelineDependency(1, 0);
  SetLength(LBridges, 1);
  LBridges[0] := MakeWfcPipelineBridge(wpbkPattern2DProjection, 0, 1);
  LRequirements := nil;
  Result := TWfcPipelineModel.Create(MakeWfcPipelineMetadata(
    'pattern bridge', 'MIT', '', ''), 2, True, rmBottomUp,
    LResources, LPasses, LDependencies, LBridges, LRequirements);
end;

function BuildSequenceBridgeFixture: TWfcPipelineModel;
var
  LBridges: TWfcPipelineBridges;
  LDependencies: TWfcPipelineDependencies;
  LPasses: TWfcPipelinePasses;
  LRequirements: TWfcPipelineRequirements;
  LResources: TWfcPipelineResources;
begin
  SetLength(LResources, 1);
  LResources[0] := MakeWfcPipelineResource('sequence', wprkSequence,
    SEQUENCE_TEXT, 'sequence source', 'MIT', '');
  SetLength(LPasses, 2);
  LPasses[0] := MakeWfcPipelinePass('states', wppvPrivate,
    gpmLegacy, WFC_PIPELINE_NO_INDEX, wpakSequence, 0, True, wseWhole);
  LPasses[1] := MakeWfcPipelinePass('surface', wppvPublic,
    gpmOverlay, WFC_PIPELINE_NO_INDEX, wpakEmpty,
    WFC_PIPELINE_NO_INDEX, False, wseWhole);
  SetLength(LDependencies, 1);
  LDependencies[0] := MakeWfcPipelineDependency(1, 0);
  SetLength(LBridges, 1);
  LBridges[0] := MakeWfcPipelineBridge(wpbkSequenceProjection, 0, 1);
  LRequirements := nil;
  Result := TWfcPipelineModel.Create(MakeWfcPipelineMetadata(
    'sequence bridge', 'MIT', '', ''), 1, False, rmBottomUp,
    LResources, LPasses, LDependencies, LBridges, LRequirements);
end;

procedure TestMinimalCanonicalRoundTrip;
var
  LDecoded: TWfcPipelineModel;
  LEncoded: String;
  LFixture: TWfcPipelineModel;
begin
  LFixture := BuildMinimalFixture;
  try
    LEncoded := EncodeWfcPipelineModelText(LFixture);
    Check(LEncoded = MINIMAL_PIPELINE_TEXT,
      'encoding matches the exact canonical wfcpipeline=1 golden');
    Check(Pos('wfcpipeline=1'#10, LEncoded) = 1,
      'the canonical document starts with the versioned header');
    Check(Copy(LEncoded, Length(LEncoded) - 3, 4) = 'end'#10,
      'the canonical document has one final LF after end');
    Check(Pos('signature=' +
      WfcPipelineSignatureHex(LFixture.Signature) + #10, LEncoded) > 0,
      'the document stores the complete semantic signature');
    LDecoded := DecodeWfcPipelineModelText(LEncoded);
    try
      Check((LDecoded.Signature = LFixture.Signature) and
        (LDecoded.ResourceCount = 1) and (LDecoded.PassCount = 1),
        'decode reconstructs the immutable recipe identity and counts');
      Check(EncodeWfcPipelineModelText(LDecoded) = LEncoded,
        'decode then encode is byte-for-byte identical');
    finally
      LDecoded.Free;
    end;
    WriteLn('  canonical-signature=',
      WfcPipelineSignatureHex(LFixture.Signature));
  finally
    LFixture.Free;
  end;
end;

procedure TestEveryRecordSurface;
const
  EXPECTED_SIGNATURE = '564C7ED3';
var
  LDecoded: TWfcPipelineModel;
  LEncoded: String;
  LFixture: TWfcPipelineModel;
  LRequirement: TWfcPipelineRequirement;
begin
  LFixture := BuildComprehensiveFixture;
  try
    LEncoded := EncodeWfcPipelineModelText(LFixture);
    Check(WfcPipelineSignatureHex(LFixture.Signature) = EXPECTED_SIGNATURE,
      'the complete recipe semantic signature is pinned');
    Check((Pos('traversal=top-down'#10, LEncoded) > 0) and
      (Pos('resource=0,provider%2Crules,rules,', LEncoded) > 0) and
      (Pos('pass=2,copy,public,transform,1,empty,-1,false,whole'#10,
        LEncoded) > 0),
      'metadata, typed resources, topology, and pass fields are explicit');
    Check((Pos('requirement=0,1,tree,0,exact,1'#10, LEncoded) > 0) and
      (Pos('term=0,0,-2147483648,0,0,2'#10, LEncoded) > 0) and
      (Pos('term=1,1,2147483647,0,0,1'#10, LEncoded) > 0),
      'requirements retain exact signed offset boundaries and counts');
    Check((Pos('%E2%99%AB', LEncoded) > 0) and
      (Pos('source%0Adescription', LEncoded) > 0),
      'Unicode, punctuation, and line breaks are canonical token escapes');
    LDecoded := DecodeWfcPipelineModelText(LEncoded);
    try
      Check(EncodeWfcPipelineModelText(LDecoded) = LEncoded,
        'the complete record surface round-trips byte identically');
      LRequirement := LDecoded.RequirementAt(1);
      Check((Length(LRequirement.Terms) = 2) and
        (LRequirement.Terms[0].OffsetX = Low(Integer) + 1) and
        (LRequirement.Terms[1].OffsetX = High(Integer)) and
        (LRequirement.Terms[1].AllowedProviderTokens[0] = NoteToken),
        'nested term and allowed-token arrays reconstruct exactly');
    finally
      LDecoded.Free;
    end;
    WriteLn('  comprehensive-signature=',
      WfcPipelineSignatureHex(LFixture.Signature));
  finally
    LFixture.Free;
  end;
end;

procedure TestTypedProjectionRecords;
const
  EXPECTED_PATTERN_SIGNATURE = 'DCFE0ADF';
  EXPECTED_SEQUENCE_SIGNATURE = '82728C07';
var
  LDecoded: TWfcPipelineModel;
  LEncoded: String;
  LFixture: TWfcPipelineModel;
begin
  LFixture := BuildPatternBridgeFixture;
  try
    LEncoded := EncodeWfcPipelineModelText(LFixture);
    Check(WfcPipelineSignatureHex(LFixture.Signature) =
      EXPECTED_PATTERN_SIGNATURE,
      'the pattern projection recipe signature is pinned');
    Check((Pos(',pattern2d,', LEncoded) > 0) and
      (Pos(',pattern2d-projection,0,1'#10, LEncoded) > 0),
      'pattern2d resource, adapter, and bridge names are canonical');
    LDecoded := DecodeWfcPipelineModelText(LEncoded);
    try
      Check(EncodeWfcPipelineModelText(LDecoded) = LEncoded,
        'the pattern projection recipe round-trips exactly');
    finally
      LDecoded.Free;
    end;
    WriteLn('  pattern-signature=',
      WfcPipelineSignatureHex(LFixture.Signature));
  finally
    LFixture.Free;
  end;

  LFixture := BuildSequenceBridgeFixture;
  try
    LEncoded := EncodeWfcPipelineModelText(LFixture);
    Check(WfcPipelineSignatureHex(LFixture.Signature) =
      EXPECTED_SEQUENCE_SIGNATURE,
      'the sequence projection recipe signature is pinned');
    Check((Pos(',sequence,', LEncoded) > 0) and
      (Pos(',true,whole'#10, LEncoded) > 0) and
      (Pos(',sequence-projection,0,1'#10, LEncoded) > 0),
      'sequence resource, extent, adapter, and bridge names are canonical');
    LDecoded := DecodeWfcPipelineModelText(LEncoded);
    try
      Check(EncodeWfcPipelineModelText(LDecoded) = LEncoded,
        'the sequence projection recipe round-trips exactly');
    finally
      LDecoded.Free;
    end;
    WriteLn('  sequence-signature=',
      WfcPipelineSignatureHex(LFixture.Signature));
  finally
    LFixture.Free;
  end;
end;

procedure TestEnvelopeRejections;
var
  LEncoded: String;
  LFixture: TWfcPipelineModel;
  LReplacementSignature: String;
  LSignature: String;
begin
  LFixture := BuildMinimalFixture;
  try
    LEncoded := EncodeWfcPipelineModelText(LFixture);
    LSignature := WfcPipelineSignatureHex(LFixture.Signature);
  finally
    LFixture.Free;
  end;
  ExpectDecodeRejected(ReplaceOnce(LEncoded, 'wfcpipeline=1',
    'wfcpipeline=2'), 'unknown outer versions fail closed');
  ExpectDecodeRejected(ReplaceOnce(LEncoded, #10, #13#10),
    'CRLF is rejected');
  ExpectDecodeRejected(Copy(LEncoded, 1, Length(LEncoded) - 1),
    'the final LF is mandatory');
  ExpectDecodeRejected(LEncoded + 'extra'#10,
    'trailing data after end is rejected');
  ExpectDecodeRejected(ReplaceOnce(LEncoded, 'rank=1', 'rank=01'),
    'noncanonical numeric syntax is rejected');
  ExpectDecodeRejected(ReplaceOnce(LEncoded, 'resources=1',
    'resources=2147483648'), 'overflowing counts are rejected');
  ExpectDecodeRejected(ReplaceOnce(LEncoded,
    'graph-model-version=1', 'graph-model-version=2'),
    'unknown compatibility versions fail closed');
  ExpectDecodeRejected(ReplaceOnce(LEncoded,
    'pattern2d-bridge-version=2', 'pattern2d-bridge-version=3'),
    'unknown future pattern bridge versions fail closed');
  ExpectDecodeRejected(ReplaceOnce(LEncoded,
    'pattern2d-bridge-version=2', 'pattern2d-bridge-version=0'),
    'pattern bridge version zero fails closed');
  ExpectDecodeRejected(ReplaceOnce(LEncoded,
    'sequence-bridge-version=2', 'sequence-bridge-version=3'),
    'unknown future sequence bridge versions fail closed');
  ExpectDecodeRejected(ReplaceOnce(LEncoded,
    'sequence-bridge-version=2', 'sequence-bridge-version=0'),
    'sequence bridge version zero fails closed');
  ExpectDecodeRejected(ReplaceOnce(LEncoded, ',rules,', ',callback,'),
    'unknown resource kinds fail closed');
  ExpectDecodeRejected(ReplaceOnce(LEncoded, ',public,legacy,',
    ',public,callback,'), 'unknown pass modes fail closed');
  ExpectDecodeRejected(ReplaceOnce(LEncoded, ',public,legacy,',
    ',callback,legacy,'), 'unknown visibility names fail closed');
  ExpectDecodeRejected(ReplaceOnce(LEncoded, ',rules,0,false,whole',
    ',callback,0,false,whole'),
    'unknown adapter names fail closed');
  ExpectDecodeRejected(ReplaceOnce(LEncoded, ',false,whole',
    ',false,callback'), 'unknown sequence extents fail closed');
  ExpectDecodeRejected(ReplaceOnce(LEncoded, 'traversal=bottom-up',
    'traversal=callback'), 'unknown traversal modes fail closed');
  ExpectDecodeRejected(ReplaceOnce(LEncoded, 'signature=',
    'signature=0'), 'malformed stored signatures are rejected');
  if LSignature[1] = '0' then
    LReplacementSignature := '1' + Copy(LSignature, 2, 7)
  else
    LReplacementSignature := '0' + Copy(LSignature, 2, 7);
  ExpectDecodeRejected(ReplaceOnce(LEncoded, LSignature,
    LReplacementSignature), 'corrupted semantic signatures are rejected');
end;

procedure TestIndexedAndNestedRejections;
var
  LEncoded: String;
  LFixture: TWfcPipelineModel;
begin
  LFixture := BuildComprehensiveFixture;
  try
    LEncoded := EncodeWfcPipelineModelText(LFixture);
  finally
    LFixture.Free;
  end;
  ExpectDecodeRejected(ReplaceOnce(LEncoded, 'resource=0,',
    'resource=1,'), 'resource indices must be contiguous and ordered');
  ExpectDecodeRejected(ReplaceOnce(LEncoded, 'pass=0,', 'pass=1,'),
    'pass indices must be contiguous and ordered');
  ExpectDecodeRejected(ReplaceOnce(LEncoded,
    'pass=2,copy,public,transform,1,empty,-1,false,whole',
    'pass=2,copy,public,transform,1,rules,1,false,whole'),
    'resource-backed transform passes are rejected as semantically inert');
  ExpectDecodeRejected(ReplaceOnce(LEncoded, 'dependency=0,',
    'dependency=1,'),
    'dependency indices must be contiguous and ordered');
  ExpectDecodeRejected(ReplaceOnce(LEncoded, 'requirement=0,',
    'requirement=1,'),
    'requirement indices must be contiguous and ordered');
  ExpectDecodeRejected(ReplaceOnce(LEncoded, 'term=0,0,',
    'term=0,1,'), 'term indices must be contiguous and ordered');
  ExpectDecodeRejected(ReplaceOnce(LEncoded, 'allowed=0,0,0,',
    'allowed=0,0,1,'),
    'allowed-token indices must be contiguous and ordered');
  ExpectDecodeRejected(ReplaceOnce(LEncoded, 'exact,1'#10,
    'exact,2'#10), 'term counts are authoritative');
  ExpectDecodeRejected(ReplaceOnce(LEncoded,
    'term=0,0,-2147483648,0,0,2',
    'term=0,0,-2147483648,0,0,3'),
    'allowed-token counts are authoritative');
  ExpectDecodeRejected(ReplaceOnce(LEncoded, '-2147483648',
    '-2147483649'), 'signed offset underflow is rejected');
  ExpectDecodeRejected(ReplaceOnce(LEncoded, '2147483647',
    '2147483648'), 'signed offset overflow is rejected');
  ExpectDecodeRejected(ReplaceOnce(LEncoded,
    'allowed=0,0,0,%2C'#10'allowed=0,0,1,%E2%99%AB'#10,
    'allowed=0,0,0,%E2%99%AB'#10'allowed=0,0,1,%2C'#10),
    'allowed tokens outside provider-vocabulary order are rejected');
  ExpectDecodeRejected(ReplaceOnce(LEncoded,
    'term=1,0,-2147483647,0,0,1'#10 +
      'allowed=1,0,0,%2C'#10 +
      'term=1,1,2147483647,0,0,1'#10,
    'term=1,0,2147483647,0,0,1'#10 +
      'allowed=1,0,0,%2C'#10 +
      'term=1,1,-2147483647,0,0,1'#10),
    'any terms outside strict signed X/Y/Z order are rejected');
end;

procedure TestNestedResourceRejections;
var
  LEncoded: String;
  LFixture: TWfcPipelineModel;
begin
  LFixture := BuildMinimalFixture;
  try
    LEncoded := EncodeWfcPipelineModelText(LFixture);
  finally
    LFixture.Free;
  end;
  ExpectDecodeRejected(ReplaceOnce(LEncoded, 'wfcrules%3D1',
    'wfcm%3D1'), 'a payload with the wrong typed header is rejected');
  ExpectDecodeRejected(ReplaceOnce(LEncoded, 'wfcrules%3D1',
    'wfcrules%3D2'), 'an unknown nested payload version is rejected');
  ExpectDecodeRejected(ReplaceOnce(LEncoded, 'D83BEE6A',
    'D83BEE6B'), 'a corrupted nested semantic signature is rejected');
  ExpectDecodeRejected(ReplaceOnce(LEncoded, '%0A', '%0D%0A'),
    'a nested CRLF payload is rejected');
  ExpectDecodeRejected(ReplaceOnce(LEncoded, 'only', '%6Fnly'),
    'unnecessary nested token escapes are rejected');
  ExpectDecodeRejected(ReplaceOnce(LEncoded, 'resource=0,',
    'resource=0,extra,'),
    'resource records with extra fields are rejected');
end;

procedure TestLimitsEnumsAndTokenSyntax;
var
  LComprehensive: String;
  LEncoded: String;
  LFixture: TWfcPipelineModel;
  LNewPayload: String;
  LOldPayload: String;
begin
  LFixture := BuildMinimalFixture;
  try
    LEncoded := EncodeWfcPipelineModelText(LFixture);
  finally
    LFixture.Free;
  end;
  ExpectDecodeRejected(ReplaceOnce(LEncoded, 'resources=1',
    'resources=65'), 'resource counts above the public limit are rejected');
  ExpectDecodeRejected(ReplaceOnce(LEncoded, 'passes=1',
    'passes=257'), 'pass counts above the public limit are rejected');
  ExpectDecodeRejected(ReplaceOnce(LEncoded, 'wrap=false',
    'wrap=0'), 'noncanonical Boolean spellings are rejected');
  ExpectDecodeRejected(ReplaceOnce(LEncoded, 'license=MIT',
    'license=%4DIT'), 'unnecessary outer token escapes are rejected');
  ExpectDecodeRejected(ReplaceOnce(LEncoded,
    'graph-model-version=1'#10'random-algorithm-version=1'#10,
    'random-algorithm-version=1'#10'graph-model-version=1'#10),
    'fixed fields cannot be reordered');
  ExpectDecodeRejected(ReplaceOnce(LEncoded, 'wfcpipeline=1'#10,
    'wfcpipeline=1'#10#10), 'blank lines are rejected');
  ExpectDecodeRejected(LEncoded + #10,
    'a second final LF is rejected as a blank line');
  ExpectDecodeRejectedContaining(
    StringOfChar(#10, WFC_PIPELINE_MAX_TEXT_LINE_COUNT + 1),
    'line-count limit',
    'line-count limits are enforced before splitting or copying records');

  LFixture := BuildComprehensiveFixture;
  try
    LComprehensive := EncodeWfcPipelineModelText(LFixture);
  finally
    LFixture.Free;
  end;
  ExpectDecodeRejected(ReplaceOnce(LComprehensive,
    'dependencies=2', 'dependencies=4097'),
    'dependency counts above the public limit are rejected');
  ExpectDecodeRejected(ReplaceOnce(LComprehensive,
    'requirements=2', 'requirements=4097'),
    'requirement counts above the public limit are rejected');
  ExpectDecodeRejected(ReplaceOnce(LComprehensive, 'exact,1'#10,
    'exact,257'#10),
    'term counts above the public limit are rejected');
  ExpectDecodeRejected(ReplaceOnce(LComprehensive,
    'term=0,0,-2147483648,0,0,2',
    'term=0,0,-2147483648,0,0,1025'),
    'allowed-token counts above the public limit are rejected');
  ExpectDecodeRejected(ReplaceOnce(LComprehensive, ',exact,1'#10,
    ',predicate,1'#10), 'unknown requirement kinds fail closed');
  ExpectDecodeRejected(ReplaceOnce(LComprehensive, '%E2%99%AB',
    '%e2%99%AB'), 'lowercase percent escapes are rejected');
  ExpectDecodeRejected(ReplaceOnce(LComprehensive, '%E2%99%AB',
    '%E2%28%A1'), 'invalid UTF-8 scalar encodings are rejected');
  ExpectDecodeRejected(ReplaceOnce(LComprehensive,
    'fingerprint=fnv%3Atest', 'fingerprint=fnv%'),
    'truncated percent escapes are rejected');

  LFixture := BuildPatternBridgeFixture;
  try
    LEncoded := EncodeWfcPipelineModelText(LFixture);
  finally
    LFixture.Free;
  end;
  ExpectDecodeRejected(ReplaceOnce(LEncoded,
    ',pattern2d-projection,0,1', ',callback,0,1'),
    'unknown bridge names fail closed');
  LOldPayload := WfcTextEncodeToken(TWfcModelToken(PATTERN_TEXT),
    'WFC pipeline test');
  LNewPayload := WfcTextEncodeToken(
    TWfcModelToken(ReservedPatternKeyDocument), 'WFC pipeline test');
  ExpectDecodeRejected(ReplaceOnce(LEncoded, LOldPayload, LNewPayload),
    'serialized pattern bridges reject public tokens shaped like private keys');
  ExpectDecodeRejected(ReplaceOnce(LEncoded, 'bridges=1',
    'bridges=257'), 'bridge counts above the public limit are rejected');
end;

procedure CheckBridgeVersionRoundTrip(const ACurrent: TWfcPipelineModel;
  const APatternVersion, ASequenceVersion: Integer;
  const ALabel: String);
var
  LDecoded: TWfcPipelineModel;
  LEncoded: String;
  LFixture: TWfcPipelineModel;
  LVersions: TWfcPipelineVersions;
begin
  LVersions := ACurrent.CopyVersions;
  LVersions.Pattern2DBridgeVersion := APatternVersion;
  LVersions.SequenceBridgeVersion := ASequenceVersion;
  LFixture := TWfcPipelineModel.Create(ACurrent.CopyMetadata,
    LVersions, ACurrent.Rank, ACurrent.WrapNeighbors,
    ACurrent.RunMode, ACurrent.CopyResources,
    ACurrent.CopyPasses, ACurrent.CopyDependencies,
    ACurrent.CopyBridges, ACurrent.CopyRequirements);
  try
    LEncoded := EncodeWfcPipelineModelText(LFixture);
    LDecoded := DecodeWfcPipelineModelText(LEncoded);
    try
      LVersions := LDecoded.CopyVersions;
      Check((LVersions.Pattern2DBridgeVersion = APatternVersion) and
        (LVersions.SequenceBridgeVersion = ASequenceVersion) and
        (EncodeWfcPipelineModelText(LDecoded) = LEncoded), ALabel);
    finally
      LDecoded.Free;
    end;
  finally
    LFixture.Free;
  end;
end;

procedure TestVersions;
var
  LCurrent: TWfcPipelineModel;
begin
  Check((WFC_PIPELINE_TEXT_VERSION = 1) and
    (WFC_PIPELINE_MODEL_VERSION = 1) and
    (WFC_PIPELINE_MODEL_SIGNATURE_VERSION = 1),
    'pipeline text, model, and signature contracts are version one');
  Check((WFC_PIPELINE_MAX_TEXT_LINE_COUNT = 82522) and
    (WFC_PIPELINE_MAX_ENCODED_TEXT_LENGTH = 268435456),
    'the canonical envelope has fixed public version-1 limits');
  Check((WFC_PIPELINE_MAX_TOTAL_REQUIREMENT_TERM_COUNT = 8192) and
    (WFC_PIPELINE_MAX_TOTAL_ALLOWED_TOKEN_COUNT = 65536) and
    (WFC_PIPELINE_MAX_ENCODED_TOKEN_LENGTH = 1048576) and
    (WFC_PIPELINE_MAX_TOTAL_ENCODED_TOKEN_LENGTH = 16777216) and
    (WFC_PIPELINE_MAX_TOTAL_RESOURCE_RELATION_SLOT_COUNT = 16777216),
    'the recipe owner has fixed aggregate complexity limits');
  Check((WFC_PIPELINE_PATTERN_BRIDGE_VERSION = 2) and
    (WFC_PIPELINE_SEQUENCE_BRIDGE_VERSION = 2),
    'new canonical recipes select inverse-lowering bridge version 2');

  LCurrent := BuildMinimalFixture;
  try
    CheckBridgeVersionRoundTrip(LCurrent, 1, 1,
      'canonical bridge-version-1 recipes remain byte-exact decodable');
    CheckBridgeVersionRoundTrip(LCurrent, 1, 2,
      'mixed pattern-v1 and sequence-v2 recipes round-trip exactly');
    CheckBridgeVersionRoundTrip(LCurrent, 2, 1,
      'mixed pattern-v2 and sequence-v1 recipes round-trip exactly');
  finally
    LCurrent.Free;
  end;
end;

begin
  WriteLn('WFC portable-pipeline text conformance suite');
  WriteLn('============================================');
  RunTest('minimal canonical round-trip', @TestMinimalCanonicalRoundTrip);
  RunTest('complete metadata and record surface', @TestEveryRecordSurface);
  RunTest('typed projection bridge records', @TestTypedProjectionRecords);
  RunTest('strict envelope and enum rejection', @TestEnvelopeRejections);
  RunTest('indexed nested record rejection', @TestIndexedAndNestedRejections);
  RunTest('strict nested resource rejection', @TestNestedResourceRejections);
  RunTest('limits, enum closure, and token syntax',
    @TestLimitsEnumsAndTokenSyntax);
  RunTest('public codec versions', @TestVersions);
  WriteLn('============================================');
  WriteLn('Checks: ', GCheckCount, '  Failures: ', GFailureCount);
  if GFailureCount <> 0 then
  begin
    {$IFDEF PAS2JS}
    raise Exception.CreateFmt('%d pipeline-text checks failed',
      [GFailureCount]);
    {$ELSE}
    Halt(1);
    {$ENDIF}
  end;
end.
