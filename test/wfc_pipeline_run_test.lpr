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
program wfc_pipeline_run_test;

{$mode delphi}{$H+}

uses
  SysUtils,
  wfc,
  wfc_model,
  wfc_rule_model,
  wfc_rule_text,
  wfc_sequence,
  wfc_pipeline_model,
  wfc_pipeline_run;

type
  TTestProcedure = procedure;

const
  EXPECTED_RUN_SIGNATURE = '9788EDD5';

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

function BuildRecipe(const ARank: Integer): TWfcPipelineModel;
var
  LDependencies: TWfcPipelineDependencies;
  LMetadata: TWfcPipelineMetadata;
  LPasses: TWfcPipelinePasses;
  LResources: TWfcPipelineResources;
  LRows: TWfcRuleRows;
  LRuleModel: TWfcRuleModel;
  LRuleText: String;
begin
  LRows := nil;
  LRuleModel := TWfcRuleModel.Create(ARank,
    TokensOf([TWfcModelToken('A'), TWfcModelToken('B'),
      TWfcModelToken('x,y'), NoteToken]),
    IntegersOf([1, 2, 3, 4]), LRows);
  try
    LRuleText := EncodeWfcRuleText(LRuleModel);
  finally
    LRuleModel.Free;
  end;
  SetLength(LResources, 1);
  LResources[0] := MakeWfcPipelineResource('rules', wprkRules,
    LRuleText, 'run tests', 'MIT', '');
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
  LMetadata := MakeWfcPipelineMetadata('run fixture', 'MIT', '', '');
  Result := TWfcPipelineModel.Create(LMetadata, ARank, False,
    rmBottomUp, LResources, LPasses, LDependencies, nil, nil);
end;

procedure ExpectRejected(const ARecipe: TWfcPipelineModel;
  const AWidth, AHeight, ADepth: Integer;
  const AStrategy: TWfcPipelineSolveStrategy;
  const AMaxBacktracks, AMaxPassBacktracks: Integer;
  const ALocks: TWfcPipelineCellLocks;
  const ADomains: TWfcPipelineCellDomains;
  const ALabel: String);
var
  LRejected: Boolean;
  LRun: TWfcPipelineRun;
begin
  LRejected := False;
  LRun := nil;
  try
    try
      LRun := TWfcPipelineRun.Create(ARecipe, AWidth, AHeight,
        ADepth, 7, AStrategy, AMaxBacktracks,
        AMaxPassBacktracks, False, ALocks, ADomains);
    except
      on EWfcPipelineRun do
        LRejected := True;
    end;
  finally
    LRun.Free;
  end;
  Check(LRejected, ALabel);
end;

procedure TestConstructionAndCopies;
var
  LCopyDomains: TWfcPipelineCellDomains;
  LCopyLocks: TWfcPipelineCellLocks;
  LDomain: TWfcPipelineCellDomain;
  LDomains: TWfcPipelineCellDomains;
  LLocks: TWfcPipelineCellLocks;
  LRecipe: TWfcPipelineModel;
  LRun: TWfcPipelineRun;
  LRunAgain: TWfcPipelineRun;
begin
  LRecipe := BuildRecipe(1);
  LRun := nil;
  LRunAgain := nil;
  try
    SetLength(LLocks, 2);
    LLocks[0] := MakeWfcPipelineCellLock(0, 0, 0, 0, 'A');
    LLocks[1] := MakeWfcPipelineCellLock(2, 2, 0, 0, NoteToken);
    SetLength(LDomains, 2);
    LDomains[0] := MakeWfcPipelineCellDomain(0, 0, 0, 0,
      TokensOf([TWfcModelToken('A'), TWfcModelToken('B')]));
    LDomains[1] := MakeWfcPipelineCellDomain(2, 2, 0, 0,
      TokensOf([TWfcModelToken('B'), NoteToken]));
    LRun := TWfcPipelineRun.Create(LRecipe, 3, 1, 1,
      High(Cardinal), wpssNegotiated, 11, 5, True,
      LLocks, LDomains);
    LRunAgain := TWfcPipelineRun.Create(LRecipe, 3, 1, 1,
      High(Cardinal), wpssNegotiated, 11, 5, True,
      LLocks, LDomains);

    Check(LRun.RecipeSignature = LRecipe.Signature,
      'run records the exact recipe signature');
    Check((LRun.Width = 3) and (LRun.Height = 1) and
      (LRun.Depth = 1), 'run preserves its rank-compatible shape');
    Check(LRun.Seed = High(Cardinal),
      'run preserves the complete unsigned seed range');
    Check((LRun.Strategy = wpssNegotiated) and
      (LRun.MaxBacktracks = 11) and (LRun.MaxPassBacktracks = 5),
      'run preserves solve strategy and both bounded search limits');
    Check(LRun.CaptureTrace, 'run preserves trace capture intent');
    Check((LRun.LockCount = 2) and (LRun.DomainCount = 2),
      'run preserves ordered input counts');
    Check((LRun.LockAt(1).PassIndex = 2) and
      (LRun.LockAt(1).Token = NoteToken),
      'lock access preserves portable Unicode tokens');
    LDomain := LRun.DomainAt(0);
    Check((Length(LDomain.AllowedTokens) = 2) and
      (LDomain.AllowedTokens[0] = 'A') and
      (LDomain.AllowedTokens[1] = 'B'),
      'domain access returns its canonical token set');
    Check(LRun.Signature = LRunAgain.Signature,
      'equal invocations have equal semantic signatures');
    Check(WfcPipelineRunSignatureHex(LRun.Signature) =
      EXPECTED_RUN_SIGNATURE,
      'run signature matches the portable version-1 golden');
    WriteLn('  semantic-signature=',
      WfcPipelineRunSignatureHex(LRun.Signature));

    LLocks[0].Token := 'B';
    LDomains[0].AllowedTokens[0] := 'x,y';
    Check(LRun.LockAt(0).Token = 'A',
      'run detaches caller lock storage');
    Check(LRun.DomainAt(0).AllowedTokens[0] = 'A',
      'run deep-copies caller domain tokens');
    LCopyLocks := LRun.CopyLocks;
    LCopyLocks[0].Token := 'x,y';
    LCopyDomains := LRun.CopyDomains;
    LCopyDomains[0].AllowedTokens[0] := 'x,y';
    Check(LRun.LockAt(0).Token = 'A',
      'lock copies cannot mutate the immutable run');
    Check(LRun.DomainAt(0).AllowedTokens[0] = 'A',
      'domain copies cannot mutate the immutable run');
  finally
    LRunAgain.Free;
    LRun.Free;
    LRecipe.Free;
  end;
end;

procedure TestShapeAndOptionsValidation;
var
  LInvalidStrategyOrdinal: Integer;
  LRecipe1: TWfcPipelineModel;
  LRecipe2: TWfcPipelineModel;
begin
  LRecipe1 := BuildRecipe(1);
  LRecipe2 := BuildRecipe(2);
  try
    ExpectRejected(nil, 1, 1, 1, wpssOneWay, 0, 0, nil, nil,
      'nil recipes are rejected');
    ExpectRejected(LRecipe1, 0, 1, 1, wpssOneWay, 0, 0, nil, nil,
      'zero dimensions are rejected');
    ExpectRejected(LRecipe1, 1, 2, 1, wpssOneWay, 0, 0, nil, nil,
      'rank-1 height must be one');
    ExpectRejected(LRecipe1, 1, 1, 2, wpssOneWay, 0, 0, nil, nil,
      'rank-1 depth must be one');
    ExpectRejected(LRecipe2, 1, 1, 2, wpssOneWay, 0, 0, nil, nil,
      'rank-2 depth must be one');
    ExpectRejected(LRecipe2, WFC_PIPELINE_RUN_MAX_CELL_COUNT, 2, 1,
      wpssOneWay, 0, 0, nil, nil,
      'aggregate cell-count overflow is rejected before allocation');
    ExpectRejected(LRecipe1, WFC_PIPELINE_RUN_MAX_DIMENSION + 1,
      1, 1, wpssOneWay, 0, 0, nil, nil,
      'oversized dimensions are rejected');
    LInvalidStrategyOrdinal := 17;
    ExpectRejected(LRecipe1, 1, 1, 1,
      TWfcPipelineSolveStrategy(LInvalidStrategyOrdinal), 0, 0, nil, nil,
      'unknown solve strategies fail closed');
    ExpectRejected(LRecipe1, 1, 1, 1, wpssOneWay,
      -1, 0, nil, nil, 'negative local backtrack limits are rejected');
    ExpectRejected(LRecipe1, 1, 1, 1, wpssOneWay,
      WFC_PIPELINE_RUN_MAX_BACKTRACKS + 1, 0, nil, nil,
      'oversized local backtrack limits are rejected');
    ExpectRejected(LRecipe1, 1, 1, 1, wpssOneWay,
      0, 1, nil, nil,
      'one-way runs reject an active pass-backtrack budget');
    ExpectRejected(LRecipe1, 1, 1, 1, wpssNegotiated,
      0, WFC_PIPELINE_RUN_MAX_PASS_BACKTRACKS + 1, nil, nil,
      'oversized pass-backtrack limits are rejected');
  finally
    LRecipe2.Free;
    LRecipe1.Free;
  end;
end;

procedure TestInputValidation;
var
  LDomains: TWfcPipelineCellDomains;
  LLocks: TWfcPipelineCellLocks;
  LRecipe: TWfcPipelineModel;
  LRun: TWfcPipelineRun;
begin
  LRecipe := BuildRecipe(1);
  LRun := nil;
  try
    SetLength(LLocks, 1);
    LLocks[0] := MakeWfcPipelineCellLock(3, 0, 0, 0, 'A');
    ExpectRejected(LRecipe, 2, 1, 1, wpssOneWay, 0, 0,
      LLocks, nil, 'out-of-range lock passes are rejected');
    LLocks[0] := MakeWfcPipelineCellLock(1, 0, 0, 0, 'A');
    ExpectRejected(LRecipe, 2, 1, 1, wpssOneWay, 0, 0,
      LLocks, nil, 'private-pass locks are rejected');
    LLocks[0] := MakeWfcPipelineCellLock(0, 2, 0, 0, 'A');
    ExpectRejected(LRecipe, 2, 1, 1, wpssOneWay, 0, 0,
      LLocks, nil, 'out-of-range lock coordinates are rejected');
    LLocks[0] := MakeWfcPipelineCellLock(0, 0, 0, 0, 'missing');
    ExpectRejected(LRecipe, 2, 1, 1, wpssOneWay, 0, 0,
      LLocks, nil, 'lock tokens outside the public vocabulary are rejected');

    SetLength(LLocks, 2);
    LLocks[0] := MakeWfcPipelineCellLock(0, 1, 0, 0, 'A');
    LLocks[1] := MakeWfcPipelineCellLock(0, 0, 0, 0, 'B');
    ExpectRejected(LRecipe, 2, 1, 1, wpssOneWay, 0, 0,
      LLocks, nil, 'lock order is strict and deterministic');
    LLocks[1] := MakeWfcPipelineCellLock(0, 1, 0, 0, 'B');
    ExpectRejected(LRecipe, 2, 1, 1, wpssOneWay, 0, 0,
      LLocks, nil, 'duplicate lock cells are rejected');

    SetLength(LDomains, 1);
    LDomains[0] := MakeWfcPipelineCellDomain(1, 0, 0, 0, nil);
    ExpectRejected(LRecipe, 2, 1, 1, wpssOneWay, 0, 0,
      nil, LDomains, 'private-pass domains are rejected');
    LDomains[0] := MakeWfcPipelineCellDomain(0, 0, 0, 0,
      TokensOf([TWfcModelToken('B'), TWfcModelToken('A')]));
    ExpectRejected(LRecipe, 2, 1, 1, wpssOneWay, 0, 0,
      nil, LDomains, 'domain tokens must follow vocabulary order');
    LDomains[0] := MakeWfcPipelineCellDomain(0, 0, 0, 0,
      TokensOf([TWfcModelToken('A'), TWfcModelToken('A')]));
    ExpectRejected(LRecipe, 2, 1, 1, wpssOneWay, 0, 0,
      nil, LDomains, 'duplicate domain tokens are rejected');
    LDomains[0] := MakeWfcPipelineCellDomain(0, 0, 0, 0,
      TokensOf([TWfcModelToken('missing')]));
    ExpectRejected(LRecipe, 2, 1, 1, wpssOneWay, 0, 0,
      nil, LDomains, 'domain tokens outside the vocabulary are rejected');

    LDomains[0] := MakeWfcPipelineCellDomain(0, 0, 0, 0, nil);
    LRun := TWfcPipelineRun.Create(LRecipe, 2, 1, 1, 1,
      wpssOneWay, 0, 0, False, nil, LDomains);
    Check((LRun.DomainCount = 1) and
      (Length(LRun.DomainAt(0).AllowedTokens) = 0),
      'an explicit empty domain remains a valid contradiction input');
    FreeAndNil(LRun);

    SetLength(LLocks, 1);
    LLocks[0] := MakeWfcPipelineCellLock(0, 0, 0, 0, 'A');
    LDomains[0] := MakeWfcPipelineCellDomain(0, 0, 0, 0,
      TokensOf([TWfcModelToken('B')]));
    ExpectRejected(LRecipe, 2, 1, 1, wpssOneWay, 0, 0,
      LLocks, LDomains,
      'a lock excluded by its cell domain is rejected statically');
    LDomains[0] := MakeWfcPipelineCellDomain(0, 0, 0, 0,
      TokensOf([TWfcModelToken('A'), TWfcModelToken('B')]));
    LRun := TWfcPipelineRun.Create(LRecipe, 2, 1, 1, 1,
      wpssOneWay, 0, 0, False, LLocks, LDomains);
    Check(LRun.LockCount = 1,
      'compatible lock and domain constraints may share a cell');
  finally
    LRun.Free;
    LRecipe.Free;
  end;
end;

begin
  RunTest('construction and immutable copies', TestConstructionAndCopies);
  RunTest('shape and option validation', TestShapeAndOptionsValidation);
  RunTest('public input validation', TestInputValidation);
  WriteLn('Checks: ', GCheckCount, ', Failures: ', GFailureCount);
  if GFailureCount <> 0 then
  begin
    {$IFDEF PAS2JS}
    raise Exception.CreateFmt('%d pipeline-run checks failed',
      [GFailureCount]);
    {$ELSE}
    Halt(1);
    {$ENDIF}
  end;
end.
