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
program wfc_pipeline_run_text_test;

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
  wfc_pipeline_run,
  wfc_pipeline_run_text;

type
  TTestProcedure = procedure;

const
  EXPECTED_RECIPE_SIGNATURE = '5D0886DC';
  EXPECTED_RUN_SIGNATURE = 'C6C16461';

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
  LDependencies: TWfcPipelineDependencies;
  LPasses: TWfcPipelinePasses;
  LResources: TWfcPipelineResources;
  LRows: TWfcRuleRows;
  LRule: TWfcRuleModel;
begin
  LRows := nil;
  LRule := TWfcRuleModel.Create(1,
    TokensOf([TWfcModelToken('A'), TWfcModelToken('B'),
      TWfcModelToken('x,y'), NoteToken]),
    IntegersOf([1, 2, 3, 4]), LRows);
  try
    SetLength(LResources, 1);
    LResources[0] := MakeWfcPipelineResource('rules', wprkRules,
      EncodeWfcRuleText(LRule), 'run codec tests', 'MIT', 'fixture');
  finally
    LRule.Free;
  end;
  SetLength(LPasses, 3);
  LPasses[0] := MakeWfcPipelinePass('base', wppvPublic,
    gpmLegacy, WFC_PIPELINE_NO_INDEX, wpakRules, 0, False, wseWhole);
  LPasses[1] := MakeWfcPipelinePass('latent', wppvPrivate,
    gpmOverlay, WFC_PIPELINE_NO_INDEX, wpakRules, 0, False, wseWhole);
  LPasses[2] := MakeWfcPipelinePass('copy', wppvPublic,
    gpmTransform, 0, wpakEmpty, WFC_PIPELINE_NO_INDEX, False,
    wseWhole);
  SetLength(LDependencies, 1);
  LDependencies[0] := MakeWfcPipelineDependency(2, 0);
  Result := TWfcPipelineModel.Create(
    MakeWfcPipelineMetadata('run text fixture', 'MIT', '', ''),
    1, False, rmBottomUp, LResources, LPasses,
    LDependencies, nil, nil);
end;

function BuildRun(const ARecipe: TWfcPipelineModel): TWfcPipelineRun;
var
  LDomains: TWfcPipelineCellDomains;
  LLocks: TWfcPipelineCellLocks;
begin
  SetLength(LLocks, 2);
  LLocks[0] := MakeWfcPipelineCellLock(0, 0, 0, 0, 'x,y');
  LLocks[1] := MakeWfcPipelineCellLock(2, 2, 0, 0, NoteToken);
  SetLength(LDomains, 3);
  LDomains[0] := MakeWfcPipelineCellDomain(0, 0, 0, 0,
    TokensOf([TWfcModelToken('x,y')]));
  LDomains[1] := MakeWfcPipelineCellDomain(0, 1, 0, 0, nil);
  LDomains[2] := MakeWfcPipelineCellDomain(2, 2, 0, 0,
    TokensOf([TWfcModelToken('A'), TWfcModelToken('B'), NoteToken]));
  Result := TWfcPipelineRun.Create(ARecipe, 3, 1, 1,
    High(Cardinal), wpssNegotiated, 7, 3, True,
    LLocks, LDomains);
end;

function ExpectedText: String;
begin
  Result :=
    'wfcpipeline-run=1'#10 +
    'recipe-signature=' + EXPECTED_RECIPE_SIGNATURE + #10 +
    'width=3'#10 +
    'height=1'#10 +
    'depth=1'#10 +
    'seed=4294967295'#10 +
    'strategy=negotiated'#10 +
    'max-backtracks=7'#10 +
    'max-pass-backtracks=3'#10 +
    'trace=true'#10 +
    'locks=2'#10 +
    'lock=0,0,0,0,0,x%2Cy'#10 +
    'lock=1,2,2,0,0,%E2%99%AB'#10 +
    'domains=3'#10 +
    'domain=0,0,0,0,0,1'#10 +
    'allowed=0,0,x%2Cy'#10 +
    'domain=1,0,1,0,0,0'#10 +
    'domain=2,2,2,0,0,3'#10 +
    'allowed=2,0,A'#10 +
    'allowed=2,1,B'#10 +
    'allowed=2,2,%E2%99%AB'#10 +
    'signature=' + EXPECTED_RUN_SIGNATURE + #10 +
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
  const ARecipe: TWfcPipelineModel; const ALabel: String);
var
  LRejected: Boolean;
  LRun: TWfcPipelineRun;
begin
  LRejected := False;
  LRun := nil;
  try
    try
      LRun := DecodeWfcPipelineRunText(AText, ARecipe);
    except
      on EConvertError do
        LRejected := True;
    end;
  finally
    LRun.Free;
  end;
  Check(LRejected, ALabel);
end;

procedure TestCanonicalRoundTrip;
var
  LDecoded: TWfcPipelineRun;
  LDomain: TWfcPipelineCellDomain;
  LEncoded: String;
  LRecipe: TWfcPipelineModel;
  LRun: TWfcPipelineRun;
begin
  LRecipe := BuildRecipe;
  LRun := nil;
  LDecoded := nil;
  try
    LRun := BuildRun(LRecipe);
    LEncoded := EncodeWfcPipelineRunText(LRun);
    Check(WfcPipelineSignatureHex(LRecipe.Signature) =
      EXPECTED_RECIPE_SIGNATURE,
      'fixture recipe signature matches the portable golden');
    Check(WfcPipelineRunSignatureHex(LRun.Signature) =
      EXPECTED_RUN_SIGNATURE,
      'fixture run signature matches the portable golden');
    Check(LEncoded = ExpectedText,
      'encoder emits the exact canonical version-1 document');
    Check(Pos('lock=0,0,0,0,0,x%2Cy'#10, LEncoded) <> 0,
      'tokens use canonical uppercase percent escaping');
    Check(Pos('seed=4294967295'#10, LEncoded) <> 0,
      'the complete Cardinal seed range is encoded canonically');
    LDecoded := DecodeWfcPipelineRunText(LEncoded, LRecipe);
    Check(LDecoded.Signature = LRun.Signature,
      'decoder restores the semantic run signature');
    Check(EncodeWfcPipelineRunText(LDecoded) = LEncoded,
      'decoded runs re-encode byte for byte');
    Check((LDecoded.LockCount = 2) and
      (LDecoded.LockAt(0).Token = 'x,y'),
      'decoded locks preserve public pass inputs');
    Check(LDecoded.LockAt(1).Token = NoteToken,
      'decoded locks preserve Unicode scalar tokens');
    LDomain := LDecoded.DomainAt(1);
    Check(Length(LDomain.AllowedTokens) = 0,
      'decoded empty domains remain explicitly present');
    Check((LDecoded.Strategy = wpssNegotiated) and
      (LDecoded.MaxPassBacktracks = 3) and LDecoded.CaptureTrace,
      'decoded solve options preserve their exact meaning');
    WriteLn('  recipe-signature=',
      WfcPipelineSignatureHex(LRecipe.Signature));
    WriteLn('  run-signature=',
      WfcPipelineRunSignatureHex(LRun.Signature));
  finally
    LDecoded.Free;
    LRun.Free;
    LRecipe.Free;
  end;
end;

procedure TestEnvelopeAndScalarRejection;
var
  LEncoded: String;
  LNilRejected: Boolean;
  LRecipe: TWfcPipelineModel;
  LRun: TWfcPipelineRun;
begin
  LRecipe := BuildRecipe;
  LRun := nil;
  try
    LRun := BuildRun(LRecipe);
    LEncoded := EncodeWfcPipelineRunText(LRun);
    ExpectDecodeRejected(ReplaceOnce(LEncoded,
      'wfcpipeline-run=1', 'wfcpipeline-run=2'), LRecipe,
      'unknown artifact versions are rejected');
    ExpectDecodeRejected(ReplaceOnce(LEncoded,
      'recipe-signature=', 'recipe-signature=0'), LRecipe,
      'malformed recipe signatures are rejected');
    ExpectDecodeRejected(ReplaceOnce(LEncoded,
      WfcPipelineSignatureHex(LRecipe.Signature), '00000000'), LRecipe,
      'runs bound to another recipe are rejected');
    ExpectDecodeRejected(ReplaceOnce(LEncoded,
      'width=3', 'width=03'), LRecipe,
      'leading-zero dimensions are rejected');
    ExpectDecodeRejected(ReplaceOnce(LEncoded,
      'seed=4294967295', 'seed=4294967296'), LRecipe,
      'seeds beyond Cardinal are rejected');
    ExpectDecodeRejected(ReplaceOnce(LEncoded,
      'strategy=negotiated', 'strategy=best'), LRecipe,
      'unknown strategies are rejected');
    ExpectDecodeRejected(ReplaceOnce(LEncoded,
      'trace=true', 'trace=1'), LRecipe,
      'noncanonical booleans are rejected');
    ExpectDecodeRejected(ReplaceOnce(LEncoded,
      'signature=' + WfcPipelineRunSignatureHex(LRun.Signature),
      'signature=00000000'), LRecipe,
      'semantic signature mismatches are rejected');
    ExpectDecodeRejected(ReplaceOnce(LEncoded,
      WfcPipelineRunSignatureHex(LRun.Signature),
      LowerCase(WfcPipelineRunSignatureHex(LRun.Signature))), LRecipe,
      'lowercase signature text is rejected');
    ExpectDecodeRejected(ReplaceOnce(LEncoded, 'end'#10, 'stop'#10),
      LRecipe, 'missing end markers are rejected');
    ExpectDecodeRejected(LEncoded + 'extra'#10, LRecipe,
      'trailing records are rejected');
    ExpectDecodeRejected(StringReplace(LEncoded, #10, #13#10,
      [rfReplaceAll]), LRecipe, 'CRLF documents are rejected');
    ExpectDecodeRejected(Copy(LEncoded, 1, Length(LEncoded) - 1),
      LRecipe, 'a missing final LF is rejected');

    LNilRejected := False;
    try
      DecodeWfcPipelineRunText(LEncoded, nil);
    except
      on EArgumentNilException do
        LNilRejected := True;
    end;
    Check(LNilRejected, 'decoding requires the referenced recipe');
    LNilRejected := False;
    try
      EncodeWfcPipelineRunText(nil);
    except
      on EArgumentNilException do
        LNilRejected := True;
    end;
    Check(LNilRejected, 'encoding rejects nil run objects');
  finally
    LRun.Free;
    LRecipe.Free;
  end;
end;

procedure TestRecordAndSemanticRejection;
var
  LEncoded: String;
  LRecipe: TWfcPipelineModel;
  LRun: TWfcPipelineRun;
begin
  LRecipe := BuildRecipe;
  LRun := nil;
  try
    LRun := BuildRun(LRecipe);
    LEncoded := EncodeWfcPipelineRunText(LRun);
    ExpectDecodeRejected(ReplaceOnce(LEncoded,
      'lock=0,0,0,0,0,x%2Cy', 'lock=1,0,0,0,0,x%2Cy'), LRecipe,
      'lock record indices must be contiguous');
    ExpectDecodeRejected(ReplaceOnce(LEncoded,
      'lock=0,0,0,0,0,x%2Cy', 'lock=0,1,0,0,0,x%2Cy'), LRecipe,
      'private-pass locks are rejected after decoding');
    ExpectDecodeRejected(ReplaceOnce(LEncoded,
      'lock=0,0,0,0,0,x%2Cy', 'lock=0,0,3,0,0,x%2Cy'), LRecipe,
      'out-of-range coordinates are rejected after decoding');
    ExpectDecodeRejected(ReplaceOnce(LEncoded,
      'lock=0,0,0,0,0,x%2Cy', 'lock=0,0,0,0,0,missing'), LRecipe,
      'unknown public tokens are rejected after decoding');
    ExpectDecodeRejected(ReplaceOnce(LEncoded,
      'lock=0,0,0,0,0,x%2Cy', 'lock=0,0,0,0,0,x%2cy'), LRecipe,
      'lowercase percent escapes are rejected');
    ExpectDecodeRejected(ReplaceOnce(LEncoded,
      'lock=0,0,0,0,0,x%2Cy', 'lock=0,0,0,0,x%2Cy'), LRecipe,
      'missing record fields are rejected');
    ExpectDecodeRejected(ReplaceOnce(LEncoded,
      'lock=0,0,0,0,0,x%2Cy', 'lock=0,0,0,0,0,x%2Cy,extra'), LRecipe,
      'extra record fields are rejected');
    ExpectDecodeRejected(ReplaceOnce(LEncoded,
      'domain=0,0,0,0,0,1', 'domain=1,0,0,0,0,1'), LRecipe,
      'domain record indices must be contiguous');
    ExpectDecodeRejected(ReplaceOnce(LEncoded,
      'allowed=2,0,A', 'allowed=2,1,A'), LRecipe,
      'allowed-token indices must be contiguous');
    ExpectDecodeRejected(ReplaceOnce(LEncoded,
      'allowed=2,0,A'#10 + 'allowed=2,1,B',
      'allowed=2,0,B'#10 + 'allowed=2,1,A'), LRecipe,
      'decoded domain tokens must follow vocabulary order');
    ExpectDecodeRejected(ReplaceOnce(LEncoded,
      'lock=1,2,2,0,0,%E2%99%AB',
      'lock=1,0,0,0,0,%E2%99%AB'), LRecipe,
      'decoded locks must follow strict pass/row-major order');
    ExpectDecodeRejected(ReplaceOnce(LEncoded,
      'domains=3', 'domains=262145'), LRecipe,
      'oversized record counts are rejected before allocation');
    ExpectDecodeRejected(ReplaceOnce(LEncoded,
      'strategy=negotiated'#10 + 'max-backtracks=7'#10 +
        'max-pass-backtracks=3',
      'strategy=one-way'#10 + 'max-backtracks=7'#10 +
        'max-pass-backtracks=3'), LRecipe,
      'one-way text cannot carry a pass-backtrack budget');
  finally
    LRun.Free;
    LRecipe.Free;
  end;
end;

begin
  RunTest('canonical round trip', TestCanonicalRoundTrip);
  RunTest('envelope and scalar rejection', TestEnvelopeAndScalarRejection);
  RunTest('record and semantic rejection', TestRecordAndSemanticRejection);
  WriteLn('Checks: ', GCheckCount, ', Failures: ', GFailureCount);
  if GFailureCount <> 0 then
  begin
    {$IFDEF PAS2JS}
    raise Exception.CreateFmt('%d pipeline-run-text checks failed',
      [GFailureCount]);
    {$ELSE}
    Halt(1);
    {$ENDIF}
  end;
end.
