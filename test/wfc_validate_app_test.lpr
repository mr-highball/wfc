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
program wfc_validate_app_test;

{$mode delphi}{$H+}

uses
  {$IFDEF PAS2JS}wfc_browser_test_host,{$ENDIF}
  SysUtils,
  wfc_validate_app;

const
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
    'pattern2d-bridge-version=1'#10 +
    'sequence-bridge-version=1'#10 +
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
    'signature=9B25EECF'#10 +
    'end'#10;

  EXPECTED_SUMMARY =
    'valid canonical wfcpipeline=1 signature=9B25EECF' +
    ' resources=1 passes=1 dependencies=0 bridges=0 requirements=0'#10;

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

function Arguments(const AValues: array of String): TWfcValidateArguments;
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
  LArguments: TWfcValidateArguments;
  LCommand: TWfcValidateCommand;
  LError: String;
begin
  LArguments := Arguments(AValues);
  Check(not WfcValidateParseCommand(LArguments, LCommand, LError) and
    (LError = AExpectedError), AMessage);
end;

procedure TestCommandParsing;
var
  LArguments: TWfcValidateArguments;
  LCommand: TWfcValidateCommand;
  LError: String;
begin
  LArguments := Arguments(['--help']);
  Check(WfcValidateParseCommand(LArguments, LCommand, LError) and
    (LCommand.Kind = wvckHelp) and (LError = ''),
    'the sole --help argument selects help');

  LArguments := Arguments(['--version']);
  Check(WfcValidateParseCommand(LArguments, LCommand, LError) and
    (LCommand.Kind = wvckVersion) and (LError = ''),
    'the sole --version argument selects version output');

  LArguments := Arguments(['recipe', 'fixture.wfcpipeline']);
  Check(WfcValidateParseCommand(LArguments, LCommand, LError) and
    (LCommand.Kind = wvckRecipe) and
    (LCommand.OutputMode = wvomSummary) and
    (LCommand.InputPath = 'fixture.wfcpipeline'),
    'recipe defaults to deterministic summary output');

  LArguments := Arguments(['recipe', '--quiet', '-']);
  Check(WfcValidateParseCommand(LArguments, LCommand, LError) and
    (LCommand.OutputMode = wvomQuiet) and (LCommand.InputPath = '-'),
    'literal - explicitly selects standard input');

  LArguments := Arguments(['recipe', '--emit-canonical', 'fixture']);
  Check(WfcValidateParseCommand(LArguments, LCommand, LError) and
    (LCommand.OutputMode = wvomCanonical) and
    (LCommand.InputPath = 'fixture'),
    '--emit-canonical selects exact artifact output');

  LArguments := Arguments(['recipe', '--', '--fixture']);
  Check(WfcValidateParseCommand(LArguments, LCommand, LError) and
    (LCommand.InputPath = '--fixture'),
    '-- permits an input path beginning with a dash');

  CheckParseFailure([], 'a command is required',
    'an omitted command is a usage error');
  CheckParseFailure(['unknown'],
    'the command must be recipe, --help, or --version',
    'unknown commands are rejected');
  CheckParseFailure(['--help', 'extra'],
    '--help does not accept additional arguments',
    'help rejects extra arguments');
  CheckParseFailure(['--version', 'extra'],
    '--version does not accept additional arguments',
    'version rejects extra arguments');
  CheckParseFailure(['recipe'],
    'recipe validation requires one INPUT',
    'recipe requires explicit file or standard input');
  CheckParseFailure(['recipe', '--'],
    'recipe validation requires one INPUT',
    'an end-of-options marker is not an input');
  CheckParseFailure(['recipe', ''], 'INPUT cannot be empty',
    'empty input paths are rejected');
  CheckParseFailure(['recipe', 'one', 'two'],
    'recipe validation accepts exactly one INPUT',
    'multiple recipe inputs are rejected');
  CheckParseFailure(['recipe', '--unknown', 'fixture'],
    'unknown recipe option; use -- before a path beginning with -',
    'unknown options are rejected');
  CheckParseFailure(['recipe', '--quiet', '--quiet', 'fixture'],
    '--quiet was specified more than once',
    'duplicate quiet options are rejected');
  CheckParseFailure(['recipe', '--emit-canonical',
    '--emit-canonical', 'fixture'],
    '--emit-canonical was specified more than once',
    'duplicate canonical-output options are rejected');
  CheckParseFailure(['recipe', '--quiet', '--emit-canonical', 'fixture'],
    '--quiet and --emit-canonical are mutually exclusive',
    'quiet and canonical output are mutually exclusive');
  CheckParseFailure(['recipe', 'fixture', '--quiet'],
    'recipe options must precede INPUT',
    'options after input are rejected');
  CheckParseFailure(['recipe', 'fixture', '--'],
    'end-of-options marker must precede INPUT',
    'the end-of-options marker cannot follow input');
end;

procedure TestHelpAndVersion;
var
  LCommand: TWfcValidateCommand;
  LError: String;
  LOutput: String;
  LStatus: Integer;
begin
  LCommand.Kind := wvckHelp;
  LCommand.OutputMode := wvomSummary;
  LCommand.InputPath := '';
  LStatus := WfcValidateExecuteText(LCommand, '', LOutput, LError);
  Check((LStatus = WFC_VALIDATE_EXIT_SUCCESS) and (LError = '') and
    (LOutput = WfcValidateHelpText) and
    (Pos(' [--] INPUT'#10, LOutput) > 0) and
    (Pos('does not solve it.'#10, LOutput) > 0),
    'help is exact LF text and states the recipe-only claim');

  LCommand.Kind := wvckVersion;
  LStatus := WfcValidateExecuteText(LCommand, 'ignored', LOutput, LError);
  Check((LStatus = WFC_VALIDATE_EXIT_SUCCESS) and (LError = '') and
    (LOutput = 'wfc-validate 1 (wfcpipeline=1)'#10),
    'version identifies both CLI and artifact contracts');
end;

procedure TestRecipeExecution;
var
  LCommand: TWfcValidateCommand;
  LError: String;
  LMalformed: String;
  LOutput: String;
  LStatus: Integer;
begin
  LCommand.Kind := wvckRecipe;
  LCommand.InputPath := 'ignored-by-pure-logic';

  LCommand.OutputMode := wvomSummary;
  LStatus := WfcValidateExecuteText(LCommand, MINIMAL_PIPELINE_TEXT,
    LOutput, LError);
  Check((LStatus = WFC_VALIDATE_EXIT_SUCCESS) and
    (LOutput = EXPECTED_SUMMARY) and (LError = ''),
    'a canonical recipe produces the exact deterministic summary');

  LCommand.OutputMode := wvomQuiet;
  LStatus := WfcValidateExecuteText(LCommand, MINIMAL_PIPELINE_TEXT,
    LOutput, LError);
  Check((LStatus = WFC_VALIDATE_EXIT_SUCCESS) and
    (LOutput = '') and (LError = ''),
    'quiet validation has no successful output');

  LCommand.OutputMode := wvomCanonical;
  LStatus := WfcValidateExecuteText(LCommand, MINIMAL_PIPELINE_TEXT,
    LOutput, LError);
  Check((LStatus = WFC_VALIDATE_EXIT_SUCCESS) and
    (LOutput = MINIMAL_PIPELINE_TEXT) and (LError = ''),
    'canonical output is byte-identical to accepted input');

  LMalformed := StringReplace(MINIMAL_PIPELINE_TEXT,
    'signature=9B25EECF', 'signature=9B25EECE', []);
  LStatus := WfcValidateExecuteText(LCommand, LMalformed,
    LOutput, LError);
  Check((LStatus = WFC_VALIDATE_EXIT_INVALID_ARTIFACT) and
    (LOutput = '') and
    (LError = 'wfc-validate: invalid recipe: invalid WFC pipeline text: ' +
      'pipeline signature does not match its semantic recipe'#10),
    'a semantic signature mismatch is an exact artifact failure');

  LMalformed := StringReplace(MINIMAL_PIPELINE_TEXT, #10, #13#10, []);
  LStatus := WfcValidateExecuteText(LCommand, LMalformed,
    LOutput, LError);
  Check((LStatus = WFC_VALIDATE_EXIT_INVALID_ARTIFACT) and
    (LOutput = '') and (Pos('wfc-validate: invalid recipe: ', LError) = 1),
    'CRLF input is rejected without standard output');

  LMalformed := MINIMAL_PIPELINE_TEXT + #$80;
  LStatus := WfcValidateExecuteText(LCommand, LMalformed,
    LOutput, LError);
  Check((LStatus = WFC_VALIDATE_EXIT_INVALID_ARTIFACT) and
    (LOutput = '') and
    (Pos('document contains a non-ASCII byte at offset ', LError) > 0),
    'raw non-ASCII bytes are rejected before target-specific decoding');

  LStatus := WfcValidateExecuteText(LCommand, '', LOutput, LError);
  Check((LStatus = WFC_VALIDATE_EXIT_INVALID_ARTIFACT) and
    (LOutput = '') and (Pos('wfc-validate: invalid recipe: ', LError) = 1),
    'empty input is an artifact error rather than a usage error');
end;

procedure TestStatusAndFailureContracts;
begin
  Check((WFC_VALIDATE_EXIT_SUCCESS = 0) and
    (WFC_VALIDATE_EXIT_INVALID_ARTIFACT = 1) and
    (WFC_VALIDATE_EXIT_USAGE = 2) and
    (WFC_VALIDATE_EXIT_IO = 3) and
    (WFC_VALIDATE_EXIT_NOT_SOLVED = 4) and
    (WFC_VALIDATE_EXIT_INTERNAL = 70),
    'public exit statuses have their fixed values');
  Check((WfcValidateFailureExitCode(wvfkInvalidArtifact) = 1) and
    (WfcValidateFailureExitCode(wvfkUsage) = 2) and
    (WfcValidateFailureExitCode(wvfkIo) = 3) and
    (WfcValidateFailureExitCode(wvfkInternal) = 70),
    'failure kinds remain distinguishable');
  Check(WfcValidateFormatFailure(wvfkUsage, 'bad'#10'arguments') =
    'wfc-validate: usage error: bad arguments'#10,
    'failure messages are one exact LF-terminated line');
  Check(WfcValidateFormatFailure(wvfkIo, '') =
    'wfc-validate: I/O error: unspecified failure'#10,
    'empty host failures retain an actionable class');
  Check((WFC_VALIDATE_CLI_VERSION = 1) and
    (WFC_VALIDATE_MAX_INPUT_LENGTH = 268435456),
    'the CLI and bounded recipe input contracts are public');
end;

begin
  WriteLn('WFC recipe-validator application conformance suite');
  WriteLn('=================================================');
  TestCommandParsing;
  TestHelpAndVersion;
  TestRecipeExecution;
  TestStatusAndFailureContracts;
  WriteLn('=================================================');
  WriteLn('Checks: ', GCheckCount, '  Failures: ', GFailureCount);
  if GFailureCount <> 0 then
  begin
    {$IFDEF PAS2JS}
    raise Exception.CreateFmt('%d validator-app checks failed',
      [GFailureCount]);
    {$ELSE}
    Halt(1);
    {$ENDIF}
  end;
end.
