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
program wfc_run_app_test;

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
  wfc_pipeline_text,
  wfc_pipeline_run,
  wfc_pipeline_run_text,
  wfc_pipeline_result,
  wfc_pipeline_result_text,
  wfc_run_app;

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

function Arguments(const AValues: array of String): TWfcRunArguments;
var
  I: Integer;
begin
  Result := nil;
  SetLength(Result, Length(AValues));
  for I := 0 to Length(AValues) - 1 do
    Result[I] := AValues[I];
end;

procedure CheckParseFailure(const AValues: array of String;
  const AExpectedError, AMessage: String);
var
  LArguments: TWfcRunArguments;
  LCommand: TWfcRunCommand;
  LError: String;
begin
  LArguments := Arguments(AValues);
  Check(not WfcRunParseCommand(LArguments, LCommand, LError) and
    (LError = AExpectedError), AMessage);
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

function BuildAliasRecipe(const AFingerprint: TWfcModelToken):
  TWfcPipelineModel;
var
  LDependencies: TWfcPipelineDependencies;
  LDocument: String;
  LMetadata: TWfcPipelineMetadata;
  LPasses: TWfcPipelinePasses;
  LResources: TWfcPipelineResources;
  LRules: TWfcRuleModel;
begin
  LRules := TWfcRuleModel.Create(1,
    TokensOf([TWfcModelToken('A'), TWfcModelToken('B')]),
    IntegersOf([1, 1]), nil);
  try
    LDocument := EncodeWfcRuleText(LRules);
  finally
    LRules.Free;
  end;

  LMetadata := MakeWfcPipelineMetadata('Runner transform fixture', 'MIT',
    'project-authored runner fixture', AFingerprint);
  SetLength(LResources, 1);
  LResources[0] := MakeWfcPipelineResource('rules', wprkRules,
    LDocument, 'runner rules', 'MIT', 'runner:rules:v1');
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

function BuildSolvedRun(const ARecipe: TWfcPipelineModel):
  TWfcPipelineRun;
var
  LDomains: TWfcPipelineCellDomains;
  LLocks: TWfcPipelineCellLocks;
begin
  SetLength(LLocks, 1);
  LLocks[0] := MakeWfcPipelineCellLock(2, 0, 0, 0, 'B');
  SetLength(LDomains, 1);
  LDomains[0] := MakeWfcPipelineCellDomain(1, 1, 0, 0,
    TokensOf(['A']));
  Result := TWfcPipelineRun.Create(ARecipe, 2, 1, 1,
    Cardinal($12345678), wpssOneWay, 16, 0, False,
    LLocks, LDomains);
end;

function BuildNonSolvedRun(const ARecipe: TWfcPipelineModel):
  TWfcPipelineRun;
var
  LDomains: TWfcPipelineCellDomains;
begin
  SetLength(LDomains, 1);
  LDomains[0] := MakeWfcPipelineCellDomain(2, 0, 0, 0, nil);
  Result := TWfcPipelineRun.Create(ARecipe, 2, 1, 1,
    Cardinal($12345678), wpssOneWay, 16, 0, False,
    nil, LDomains);
end;

function BuildOppositeRun(const ARecipe: TWfcPipelineModel):
  TWfcPipelineRun;
var
  LDomains: TWfcPipelineCellDomains;
  LLocks: TWfcPipelineCellLocks;
begin
  SetLength(LLocks, 1);
  LLocks[0] := MakeWfcPipelineCellLock(2, 0, 0, 0, 'A');
  SetLength(LDomains, 1);
  LDomains[0] := MakeWfcPipelineCellDomain(1, 1, 0, 0,
    TokensOf(['B']));
  Result := TWfcPipelineRun.Create(ARecipe, 2, 1, 1,
    Cardinal($12345678), wpssOneWay, 16, 0, False,
    LLocks, LDomains);
end;

function BuildConflictingRun(const ARecipe: TWfcPipelineModel):
  TWfcPipelineRun;
var
  LDomains: TWfcPipelineCellDomains;
  LLocks: TWfcPipelineCellLocks;
begin
  SetLength(LLocks, 1);
  LLocks[0] := MakeWfcPipelineCellLock(2, 0, 0, 0, 'B');
  SetLength(LDomains, 1);
  LDomains[0] := MakeWfcPipelineCellDomain(1, 0, 0, 0,
    TokensOf(['A']));
  Result := TWfcPipelineRun.Create(ARecipe, 2, 1, 1,
    Cardinal($12345678), wpssOneWay, 16, 0, False,
    LLocks, LDomains);
end;

function CorruptSignature(const AText: String): String;
var
  LIndex: Integer;
begin
  Result := AText;
  LIndex := Pos('signature=', Result);
  if LIndex = 0 then
    raise Exception.Create('fixture has no signature');
  Inc(LIndex, Length('signature='));
  if Result[LIndex] = '0' then
    Result[LIndex] := '1'
  else
    Result[LIndex] := '0';
end;

function CanonicalResultHasStatus(const AText: String;
  const ARecipe: TWfcPipelineModel; const ARun: TWfcPipelineRun;
  const AStatus: TWfcPipelineResultStatus): Boolean;
var
  LResult: TWfcPipelineResult;
begin
  Result := False;
  LResult := nil;
  try
    try
      LResult := DecodeWfcPipelineResultText(AText, ARecipe, ARun);
      Result := Assigned(LResult) and (LResult.Status = AStatus) and
        (EncodeWfcPipelineResultText(LResult) = AText);
    except
      Result := False;
    end;
  finally
    LResult.Free;
  end;
end;

function SolvedResultHasTransformLayers(const AText: String;
  const ARecipe: TWfcPipelineModel; const ARun: TWfcPipelineRun;
  const AFirstToken, ASecondToken: TWfcModelToken): Boolean;
var
  I: Integer;
  LLayer: TWfcPipelineResultLayer;
  LResult: TWfcPipelineResult;
begin
  Result := False;
  LResult := nil;
  try
    try
      LResult := DecodeWfcPipelineResultText(AText, ARecipe, ARun);
      if (LResult.Status <> wprsSolved) or (LResult.LayerCount <> 3) then
        Exit;
      for I := 0 to LResult.LayerCount - 1 do
      begin
        LLayer := LResult.LayerAt(I);
        if (LLayer.PassIndex <> I) or (Length(LLayer.Tokens) <> 2) or
            (LLayer.Tokens[0] <> AFirstToken) or
            (LLayer.Tokens[1] <> ASecondToken) then
          Exit;
      end;
      Result := True;
    except
      Result := False;
    end;
  finally
    LResult.Free;
  end;
end;

procedure TestCommandParsing;
var
  LArguments: TWfcRunArguments;
  LCommand: TWfcRunCommand;
  LError: String;
begin
  LArguments := Arguments(['--help']);
  Check(WfcRunParseCommand(LArguments, LCommand, LError) and
    (LCommand.Kind = wrckHelp) and (LError = ''),
    'the sole --help argument selects help');

  LArguments := Arguments(['--version']);
  Check(WfcRunParseCommand(LArguments, LCommand, LError) and
    (LCommand.Kind = wrckVersion) and (LError = ''),
    'the sole --version argument selects version output');

  LArguments := Arguments(['recipe.wfcpipeline', 'run.wfcrun']);
  Check(WfcRunParseCommand(LArguments, LCommand, LError) and
    (LCommand.Kind = wrckExecute) and
    (LCommand.OutputMode = wromCanonical) and
    (LCommand.RecipePath = 'recipe.wfcpipeline') and
    (LCommand.RunPath = 'run.wfcrun'),
    'two paths select execution with canonical result output');

  LArguments := Arguments(['--quiet', 'recipe', 'run']);
  Check(WfcRunParseCommand(LArguments, LCommand, LError) and
    (LCommand.OutputMode = wromQuiet),
    '--quiet suppresses successful and non-solved result output');

  LArguments := Arguments(['-', 'run']);
  Check(WfcRunParseCommand(LArguments, LCommand, LError) and
    (LCommand.RecipePath = '-') and (LCommand.RunPath = 'run'),
    'RECIPE may explicitly use standard input');

  LArguments := Arguments(['recipe', '-']);
  Check(WfcRunParseCommand(LArguments, LCommand, LError) and
    (LCommand.RecipePath = 'recipe') and (LCommand.RunPath = '-'),
    'RUN may explicitly use standard input');

  LArguments := Arguments(['--', '--recipe', '--run']);
  Check(WfcRunParseCommand(LArguments, LCommand, LError) and
    (LCommand.RecipePath = '--recipe') and
    (LCommand.RunPath = '--run'),
    '-- permits both paths to begin with a dash');

  CheckParseFailure([], 'RECIPE and RUN are required',
    'omitted paths are a usage error');
  CheckParseFailure(['recipe'],
    'execution requires one RECIPE and one RUN',
    'one path is insufficient');
  CheckParseFailure(['recipe', 'run', 'extra'],
    'execution accepts exactly one RECIPE and one RUN',
    'more than two paths are rejected');
  CheckParseFailure(['', 'run'],
    'RECIPE and RUN paths cannot be empty',
    'an empty recipe path is rejected');
  CheckParseFailure(['recipe', ''],
    'RECIPE and RUN paths cannot be empty',
    'an empty run path is rejected');
  CheckParseFailure(['--unknown', 'recipe', 'run'],
    'unknown option; use -- before a path beginning with -',
    'unknown options are rejected');
  CheckParseFailure(['--quiet', '--quiet', 'recipe', 'run'],
    '--quiet was specified more than once',
    'duplicate quiet options are rejected');
  CheckParseFailure(['recipe', '--quiet', 'run'],
    '--quiet must precede RECIPE and RUN',
    'options after a path are rejected');
  CheckParseFailure(['recipe', '--', 'run'],
    'end-of-options marker must precede RECIPE and RUN',
    'the end-of-options marker cannot follow a path');
  CheckParseFailure(['-', '-'],
    'only one of RECIPE and RUN may be standard input',
    'standard input cannot supply both artifacts');
  CheckParseFailure(['--help', 'extra'],
    '--help does not accept additional arguments',
    'help rejects extra arguments');
  CheckParseFailure(['--version', 'extra'],
    '--version does not accept additional arguments',
    'version rejects extra arguments');
end;

procedure TestHelpAndVersion;
var
  LCommand: TWfcRunCommand;
  LError: String;
  LOutput: String;
  LStatus: Integer;
begin
  LCommand.Kind := wrckHelp;
  LCommand.OutputMode := wromCanonical;
  LCommand.RecipePath := '';
  LCommand.RunPath := '';
  LStatus := WfcRunExecuteTexts(LCommand, '', '', LOutput, LError);
  Check((LStatus = WFC_RUN_EXIT_SOLVED) and (LError = '') and
    (LOutput = WfcRunHelpText) and
    (Pos('[--] RECIPE RUN'#10, LOutput) > 0) and
    (Pos('one may be - to read standard input.'#10, LOutput) > 0),
    'help is exact LF text and explains the dual-input contract');

  LCommand.Kind := wrckVersion;
  LStatus := WfcRunExecuteTexts(LCommand, 'ignored', 'ignored',
    LOutput, LError);
  Check((LStatus = WFC_RUN_EXIT_SOLVED) and (LError = '') and
    (LOutput = 'wfc-run 1 (wfcpipeline=1,2,3, wfcpipeline-run=1, ' +
      'wfcpipeline-result=1, runtime=2)'#10),
    'version identifies every portable artifact and runtime contract');
end;

procedure TestExecution;
var
  LCommand: TWfcRunCommand;
  LConflictRun: TWfcPipelineRun;
  LConflictText: String;
  LError: String;
  LMalformed: String;
  LNonSolvedRun: TWfcPipelineRun;
  LNonSolvedText: String;
  LOppositeOutput: String;
  LOppositeRun: TWfcPipelineRun;
  LOppositeText: String;
  LOtherRecipe: TWfcPipelineModel;
  LOtherRecipeText: String;
  LOutput: String;
  LRecipe: TWfcPipelineModel;
  LRecipeText: String;
  LRepeatOutput: String;
  LSolvedRun: TWfcPipelineRun;
  LSolvedText: String;
  LStatus: Integer;
begin
  LRecipe := BuildAliasRecipe('runner:recipe:v1');
  LOtherRecipe := nil;
  LSolvedRun := nil;
  LNonSolvedRun := nil;
  LOppositeRun := nil;
  LConflictRun := nil;
  try
    LRecipeText := EncodeWfcPipelineModelText(LRecipe);
    LSolvedRun := BuildSolvedRun(LRecipe);
    LSolvedText := EncodeWfcPipelineRunText(LSolvedRun);
    LNonSolvedRun := BuildNonSolvedRun(LRecipe);
    LNonSolvedText := EncodeWfcPipelineRunText(LNonSolvedRun);
    LOppositeRun := BuildOppositeRun(LRecipe);
    LOppositeText := EncodeWfcPipelineRunText(LOppositeRun);
    LConflictRun := BuildConflictingRun(LRecipe);
    LConflictText := EncodeWfcPipelineRunText(LConflictRun);

    LCommand.Kind := wrckExecute;
    LCommand.OutputMode := wromCanonical;
    LCommand.RecipePath := 'unused-by-pure-logic';
    LCommand.RunPath := 'unused-by-pure-logic';

    LStatus := WfcRunExecuteTexts(LCommand, LRecipeText, LSolvedText,
      LOutput, LError);
    Check((LStatus = WFC_RUN_EXIT_SOLVED) and (LError = '') and
      (Pos('wfcpipeline-result=1'#10, LOutput) = 1) and
      CanonicalResultHasStatus(LOutput, LRecipe, LSolvedRun, wprsSolved),
      'canonical recipe and run decode, execute, and emit a canonical result');
    Check(SolvedResultHasTransformLayers(LOutput, LRecipe, LSolvedRun,
      'B', 'A'),
      'public transform-targeted inputs materialize in every result layer');

    LStatus := WfcRunExecuteTexts(LCommand, LRecipeText, LSolvedText,
      LRepeatOutput, LError);
    Check((LStatus = WFC_RUN_EXIT_SOLVED) and
      (LRepeatOutput = LOutput) and (LError = ''),
      'the encoded solved result is deterministic for the pinned seed');

    LStatus := WfcRunExecuteTexts(LCommand, LRecipeText, LOppositeText,
      LOppositeOutput, LError);
    Check((LStatus = WFC_RUN_EXIT_SOLVED) and (LError = '') and
      (LOppositeOutput <> LOutput) and
      SolvedResultHasTransformLayers(LOppositeOutput, LRecipe,
        LOppositeRun, 'A', 'B'),
      'opposite transform inputs override the same-seed counterfactual');

    LCommand.OutputMode := wromQuiet;
    LStatus := WfcRunExecuteTexts(LCommand, LRecipeText, LSolvedText,
      LOutput, LError);
    Check((LStatus = WFC_RUN_EXIT_SOLVED) and
      (LOutput = '') and (LError = ''),
      'quiet solved execution suppresses the result artifact');

    LCommand.OutputMode := wromCanonical;
    LStatus := WfcRunExecuteTexts(LCommand, LRecipeText, LNonSolvedText,
      LOutput, LError);
    Check((LStatus = WFC_RUN_EXIT_NOT_SOLVED) and (LError = '') and
      (Pos('wfcpipeline-result=1'#10, LOutput) = 1) and
      CanonicalResultHasStatus(LOutput, LRecipe, LNonSolvedRun,
        wprsContradiction),
      'a valid contradiction emits a canonical result and returns exit 4');

    LCommand.OutputMode := wromQuiet;
    LStatus := WfcRunExecuteTexts(LCommand, LRecipeText, LNonSolvedText,
      LOutput, LError);
    Check((LStatus = WFC_RUN_EXIT_NOT_SOLVED) and
      (LOutput = '') and (LError = ''),
      'quiet non-solved execution retains exit 4 without output');

    LCommand.OutputMode := wromCanonical;
    LMalformed := CorruptSignature(LRecipeText);
    LStatus := WfcRunExecuteTexts(LCommand, LMalformed, LSolvedText,
      LOutput, LError);
    Check((LStatus = WFC_RUN_EXIT_INVALID_INVOCATION) and
      (LOutput = '') and
      (Pos('wfc-run: invalid recipe: invalid WFC pipeline text: ',
        LError) = 1),
      'a malformed recipe is distinguished from runtime failure');

    LMalformed := CorruptSignature(LSolvedText);
    LStatus := WfcRunExecuteTexts(LCommand, LRecipeText, LMalformed,
      LOutput, LError);
    Check((LStatus = WFC_RUN_EXIT_INVALID_INVOCATION) and
      (LOutput = '') and
      (Pos('wfc-run: invalid run: invalid WFC pipeline run text: ',
        LError) = 1),
      'a malformed run is distinguished from runtime failure');

    LOtherRecipe := BuildAliasRecipe('runner:other-recipe:v1');
    LOtherRecipeText := EncodeWfcPipelineModelText(LOtherRecipe);
    LStatus := WfcRunExecuteTexts(LCommand, LOtherRecipeText, LSolvedText,
      LOutput, LError);
    Check((LStatus = WFC_RUN_EXIT_INVALID_INVOCATION) and
      (LOutput = '') and
      (Pos('wfc-run: invalid run: ', LError) = 1),
      'recipe/run provenance mismatch is an invalid run invocation');

    LStatus := WfcRunExecuteTexts(LCommand, LRecipeText, LConflictText,
      LOutput, LError);
    Check((LStatus = WFC_RUN_EXIT_INVALID_INVOCATION) and
      (LOutput = '') and
      (LError = 'wfc-run: invalid invocation: lock 0 is excluded by the ' +
        'effective domain at pass 0 [0, 0, 0]'#10),
      'valid artifacts with incompatible effective inputs are rejected');

    LMalformed := StringReplace(LRecipeText, #10, #13#10, []);
    LStatus := WfcRunExecuteTexts(LCommand, LMalformed, LSolvedText,
      LOutput, LError);
    Check((LStatus = WFC_RUN_EXIT_INVALID_INVOCATION) and
      (LOutput = '') and (Pos('wfc-run: invalid recipe: ', LError) = 1),
      'noncanonical recipe line endings are rejected');

    LMalformed := StringReplace(LSolvedText, #10, #13#10, []);
    LStatus := WfcRunExecuteTexts(LCommand, LRecipeText, LMalformed,
      LOutput, LError);
    Check((LStatus = WFC_RUN_EXIT_INVALID_INVOCATION) and
      (LOutput = '') and (Pos('wfc-run: invalid run: ', LError) = 1),
      'noncanonical run line endings are rejected');

    LMalformed := LRecipeText + #$80;
    LStatus := WfcRunExecuteTexts(LCommand, LMalformed, LSolvedText,
      LOutput, LError);
    Check((LStatus = WFC_RUN_EXIT_INVALID_INVOCATION) and
      (LOutput = '') and
      (Pos('wfc-run: invalid recipe: document contains a non-ASCII byte ',
        LError) = 1),
      'raw non-ASCII recipe bytes are rejected before decoding');

    LMalformed := LSolvedText + #$80;
    LStatus := WfcRunExecuteTexts(LCommand, LRecipeText, LMalformed,
      LOutput, LError);
    Check((LStatus = WFC_RUN_EXIT_INVALID_INVOCATION) and
      (LOutput = '') and
      (Pos('wfc-run: invalid run: document contains a non-ASCII byte ',
        LError) = 1),
      'raw non-ASCII run bytes are rejected before target-specific decoding');

    LStatus := WfcRunExecuteTexts(LCommand, '', LSolvedText,
      LOutput, LError);
    Check((LStatus = WFC_RUN_EXIT_INVALID_INVOCATION) and
      (LOutput = '') and (Pos('wfc-run: invalid recipe: ', LError) = 1),
      'empty recipe input is an artifact failure');

    LStatus := WfcRunExecuteTexts(LCommand, LRecipeText, '',
      LOutput, LError);
    Check((LStatus = WFC_RUN_EXIT_INVALID_INVOCATION) and
      (LOutput = '') and (Pos('wfc-run: invalid run: ', LError) = 1),
      'empty run input is an artifact failure');
  finally
    LConflictRun.Free;
    LOppositeRun.Free;
    LNonSolvedRun.Free;
    LSolvedRun.Free;
    LOtherRecipe.Free;
    LRecipe.Free;
  end;
end;

procedure TestStatusAndFailureContracts;
begin
  Check((WFC_RUN_EXIT_SOLVED = 0) and
    (WFC_RUN_EXIT_INVALID_INVOCATION = 1) and
    (WFC_RUN_EXIT_USAGE = 2) and
    (WFC_RUN_EXIT_IO = 3) and
    (WFC_RUN_EXIT_NOT_SOLVED = 4) and
    (WFC_RUN_EXIT_INTERNAL = 70),
    'public exit statuses have their fixed values');
  Check((WfcRunFailureExitCode(wrfkInvalidRecipe) = 1) and
    (WfcRunFailureExitCode(wrfkInvalidRun) = 1) and
    (WfcRunFailureExitCode(wrfkInvalidInvocation) = 1) and
    (WfcRunFailureExitCode(wrfkUsage) = 2) and
    (WfcRunFailureExitCode(wrfkIo) = 3) and
    (WfcRunFailureExitCode(wrfkInternal) = 70),
    'artifact, invocation, usage, I/O, and internal failures remain distinct');
  Check(WfcRunFormatFailure(wrfkUsage, 'bad'#10'arguments') =
    'wfc-run: usage error: bad arguments'#10,
    'failure messages are one exact LF-terminated line');
  Check(WfcRunFormatFailure(wrfkIo, '') =
    'wfc-run: I/O error: unspecified failure'#10,
    'empty host failures retain an actionable class');
  Check((WFC_RUN_CLI_VERSION = 1) and
    (WFC_RUN_MAX_RECIPE_INPUT_LENGTH = 268435456) and
    (WFC_RUN_MAX_RUN_INPUT_LENGTH = 67108864),
    'the CLI and per-artifact bounded input contracts are public');
end;

begin
  WriteLn('WFC pipeline-runner application conformance suite');
  WriteLn('================================================');
  TestCommandParsing;
  TestHelpAndVersion;
  TestExecution;
  TestStatusAndFailureContracts;
  WriteLn('================================================');
  WriteLn('Checks: ', GCheckCount, '  Failures: ', GFailureCount);
  if GFailureCount <> 0 then
  begin
    {$IFDEF PAS2JS}
    raise Exception.CreateFmt('%d runner-app checks failed',
      [GFailureCount]);
    {$ELSE}
    Halt(1);
    {$ENDIF}
  end;
end.
