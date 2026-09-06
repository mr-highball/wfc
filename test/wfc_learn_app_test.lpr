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
program wfc_learn_app_test;

{$mode delphi}{$H+}

uses
  {$IFDEF PAS2JS}wfc_browser_test_host,{$ENDIF}
  SysUtils,
  wfc_learn_app;

const
  ADJACENCY_1D_TEXT =
    'wfclearn=1'#10 +
    'name=alternation'#10 +
    'license=MIT'#10 +
    'source=project-authored'#10 +
    'kind=adjacency1d'#10 +
    'boundary=wrap'#10 +
    'symmetry=none'#10 +
    'footprint=0,0'#10 +
    'order=0'#10 +
    'samples=1'#10 +
    'sample=0,2,1,first'#10 +
    'token=0,0,A'#10 +
    'token=0,1,B'#10 +
    'end'#10;

  ADJACENCY_2D_TEXT =
    'wfclearn=1'#10 +
    'name=checkerboard'#10 +
    'license=MIT'#10 +
    'source=project-authored'#10 +
    'kind=adjacency2d'#10 +
    'boundary=wrap'#10 +
    'symmetry=none'#10 +
    'footprint=0,0'#10 +
    'order=0'#10 +
    'samples=1'#10 +
    'sample=0,2,2,first'#10 +
    'token=0,0,A'#10 +
    'token=0,1,B'#10 +
    'token=0,2,B'#10 +
    'token=0,3,A'#10 +
    'end'#10;

  PATTERN_2D_TEXT =
    'wfclearn=1'#10 +
    'name=checker-patterns'#10 +
    'license=MIT'#10 +
    'source=project-authored'#10 +
    'kind=pattern2d'#10 +
    'boundary=wrap'#10 +
    'symmetry=none'#10 +
    'footprint=2,2'#10 +
    'order=0'#10 +
    'samples=1'#10 +
    'sample=0,2,2,first'#10 +
    'token=0,0,A'#10 +
    'token=0,1,B'#10 +
    'token=0,2,B'#10 +
    'token=0,3,A'#10 +
    'end'#10;

  OPEN_PATTERN_2D_TEXT =
    'wfclearn=1'#10 +
    'name=open-patterns'#10 +
    'license=MIT'#10 +
    'source=project-authored'#10 +
    'kind=pattern2d'#10 +
    'boundary=open'#10 +
    'symmetry=none'#10 +
    'footprint=2,2'#10 +
    'order=0'#10 +
    'samples=1'#10 +
    'sample=0,2,2,first'#10 +
    'token=0,0,A'#10 +
    'token=0,1,B'#10 +
    'token=0,2,B'#10 +
    'token=0,3,A'#10 +
    'end'#10;

  SEQUENCE_TEXT =
    'wfclearn=1'#10 +
    'name=bounded-sequence'#10 +
    'license=MIT'#10 +
    'source=project-authored'#10 +
    'kind=sequence'#10 +
    'boundary=open'#10 +
    'symmetry=none'#10 +
    'footprint=0,0'#10 +
    'order=2'#10 +
    'samples=1'#10 +
    'sample=0,3,1,first'#10 +
    'token=0,0,A'#10 +
    'token=0,1,B'#10 +
    'token=0,2,A'#10 +
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

function Arguments(const AValues: array of String): TWfcLearnArguments;
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
  LArguments: TWfcLearnArguments;
  LCommand: TWfcLearnCommand;
  LError: String;
begin
  LArguments := Arguments(AValues);
  Check(not WfcLearnParseCommand(LArguments, LCommand, LError) and
    (LError = AExpectedError), AMessage);
end;

procedure TestCommandParsing;
var
  LArguments: TWfcLearnArguments;
  LCommand: TWfcLearnCommand;
  LError: String;
begin
  LArguments := Arguments(['--help']);
  Check(WfcLearnParseCommand(LArguments, LCommand, LError) and
    (LCommand.Kind = wlckHelp) and (LError = ''),
    'the sole --help argument selects help');

  LArguments := Arguments(['--version']);
  Check(WfcLearnParseCommand(LArguments, LCommand, LError) and
    (LCommand.Kind = wlckVersion) and (LError = ''),
    'the sole --version argument selects version output');

  LArguments := Arguments(['fixture.wfclearn']);
  Check(WfcLearnParseCommand(LArguments, LCommand, LError) and
    (LCommand.Kind = wlckLearn) and
    (LCommand.OutputMode = wlomRecipe) and
    (LCommand.InputPath = 'fixture.wfclearn'),
    'one input defaults to canonical recipe output');

  LArguments := Arguments(['--model', '-']);
  Check(WfcLearnParseCommand(LArguments, LCommand, LError) and
    (LCommand.OutputMode = wlomModel) and (LCommand.InputPath = '-'),
    '--model accepts standard input and selects model output');

  LArguments := Arguments(['--quiet', 'fixture']);
  Check(WfcLearnParseCommand(LArguments, LCommand, LError) and
    (LCommand.OutputMode = wlomQuiet) and
    (LCommand.InputPath = 'fixture'),
    '--quiet selects checked output suppression');

  LArguments := Arguments(['--', '--fixture']);
  Check(WfcLearnParseCommand(LArguments, LCommand, LError) and
    (LCommand.InputPath = '--fixture'),
    '-- permits an input path beginning with a dash');

  CheckParseFailure([], 'training requires one INPUT',
    'an omitted input is a usage error');
  CheckParseFailure(['--help', 'extra'],
    '--help does not accept additional arguments',
    'help rejects extra arguments');
  CheckParseFailure(['--version', 'extra'],
    '--version does not accept additional arguments',
    'version rejects extra arguments');
  CheckParseFailure(['--'], 'training requires one INPUT',
    'an end-of-options marker is not an input');
  CheckParseFailure([''], 'INPUT cannot be empty',
    'empty input paths are rejected');
  CheckParseFailure(['one', 'two'],
    'training accepts exactly one INPUT',
    'multiple training inputs are rejected');
  CheckParseFailure(['--unknown', 'fixture'],
    'unknown option; use -- before a path beginning with -',
    'unknown options are rejected');
  CheckParseFailure(['--model', '--model', 'fixture'],
    '--model was specified more than once',
    'duplicate model options are rejected');
  CheckParseFailure(['--quiet', '--quiet', 'fixture'],
    '--quiet was specified more than once',
    'duplicate quiet options are rejected');
  CheckParseFailure(['--model', '--quiet', 'fixture'],
    '--model and --quiet are mutually exclusive',
    'model and quiet output are mutually exclusive');
  CheckParseFailure(['fixture', '--model'],
    'options must precede INPUT',
    'options after input are rejected');
  CheckParseFailure(['fixture', '--'],
    'end-of-options marker must precede INPUT',
    'the end-of-options marker cannot follow input');
end;

procedure TestHelpAndVersion;
var
  LCommand: TWfcLearnCommand;
  LError: String;
  LOutput: String;
  LStatus: Integer;
begin
  LCommand.Kind := wlckHelp;
  LCommand.OutputMode := wlomRecipe;
  LCommand.InputPath := '';
  LStatus := WfcLearnExecuteText(LCommand, '', LOutput, LError);
  Check((LStatus = WFC_LEARN_EXIT_SUCCESS) and (LError = '') and
    (LOutput = WfcLearnHelpText) and
    (Pos('[--model | --quiet] [--] INPUT'#10, LOutput) > 0) and
    (Pos('without emitting output.'#10, LOutput) > 0),
    'help is exact LF text and states all output modes');

  LCommand.Kind := wlckVersion;
  LStatus := WfcLearnExecuteText(LCommand, 'ignored', LOutput, LError);
  Check((LStatus = WFC_LEARN_EXIT_SUCCESS) and (LError = '') and
    (LOutput = 'wfc-learn 2 (wfclearn=1,2,3)'#10),
    'version identifies the CLI and training-text contracts');
end;

procedure CheckTrainingKind(const AText, AExpectedModelHeader,
  ALabel: String);
var
  LCommand: TWfcLearnCommand;
  LError: String;
  LOutput: String;
  LStatus: Integer;
begin
  LCommand.Kind := wlckLearn;
  LCommand.InputPath := 'ignored-by-pure-logic';

  LCommand.OutputMode := wlomRecipe;
  LStatus := WfcLearnExecuteText(LCommand, AText, LOutput, LError);
  Check((LStatus = WFC_LEARN_EXIT_SUCCESS) and (LError = '') and
    (Pos('wfcpipeline=1'#10, LOutput) = 1) and
    (Copy(LOutput, Length(LOutput) - 3, 4) = 'end'#10),
    ALabel + ' emits a canonical pipeline recipe');

  LCommand.OutputMode := wlomModel;
  LStatus := WfcLearnExecuteText(LCommand, AText, LOutput, LError);
  Check((LStatus = WFC_LEARN_EXIT_SUCCESS) and (LError = '') and
    (Pos(AExpectedModelHeader, LOutput) = 1) and
    (Copy(LOutput, Length(LOutput) - 3, 4) = 'end'#10),
    ALabel + ' emits its standalone canonical model');

  LCommand.OutputMode := wlomQuiet;
  LStatus := WfcLearnExecuteText(LCommand, AText, LOutput, LError);
  Check((LStatus = WFC_LEARN_EXIT_SUCCESS) and
    (LOutput = '') and (LError = ''),
    ALabel + ' quiet mode still compiles successfully');
end;

procedure TestTrainingExecution;
var
  LCommand: TWfcLearnCommand;
  LError: String;
  LMalformed: String;
  LOutput: String;
  LStatus: Integer;
begin
  CheckTrainingKind(ADJACENCY_1D_TEXT, 'wfcm=1'#10,
    'cardinal one-dimensional training');
  CheckTrainingKind(ADJACENCY_2D_TEXT, 'wfcm=1'#10,
    'cardinal two-dimensional training');
  LMalformed := StringReplace(ADJACENCY_1D_TEXT, 'wfclearn=1',
    'wfclearn=2', []);
  LMalformed := StringReplace(LMalformed, 'kind=adjacency1d',
    'kind=adjacency3d', []);
  LMalformed := StringReplace(LMalformed, 'sample=0,2,1,first',
    'sample=0,1,1,2,first', []);
  CheckTrainingKind(LMalformed, 'wfcm=3'#10,
    'six-direction volume training');
  CheckTrainingKind(PATTERN_2D_TEXT, 'wfcp=1'#10,
    'overlapping-pattern training');
  CheckTrainingKind(SEQUENCE_TEXT, 'wfcs=1'#10,
    'bounded sequence training');

  LCommand.Kind := wlckLearn;
  LCommand.InputPath := 'ignored-by-pure-logic';
  LCommand.OutputMode := wlomModel;
  LStatus := WfcLearnExecuteText(LCommand, OPEN_PATTERN_2D_TEXT,
    LOutput, LError);
  Check((LStatus = WFC_LEARN_EXIT_SUCCESS) and (LError = '') and
    (Pos('wfcp=1'#10, LOutput) = 1),
    'model-only mode supports an open Pattern2D source');

  LCommand.OutputMode := wlomRecipe;
  LStatus := WfcLearnExecuteText(LCommand, OPEN_PATTERN_2D_TEXT,
    LOutput, LError);
  Check((LStatus = WFC_LEARN_EXIT_INVALID_TRAINING) and
    (LOutput = '') and
    (Pos('wfc-learn: invalid training: ', LError) = 1),
    'recipe mode rejects unsupported open Pattern2D projection');

  LMalformed := StringReplace(ADJACENCY_1D_TEXT,
    'kind=adjacency1d', 'kind=unknown', []);
  LStatus := WfcLearnExecuteText(LCommand, LMalformed,
    LOutput, LError);
  Check((LStatus = WFC_LEARN_EXIT_INVALID_TRAINING) and
    (LOutput = '') and
    (Pos('wfc-learn: invalid training: ', LError) = 1),
    'unknown training kinds are exact invalid-training failures');

  LMalformed := StringReplace(ADJACENCY_1D_TEXT, #10, #13#10, []);
  LStatus := WfcLearnExecuteText(LCommand, LMalformed,
    LOutput, LError);
  Check((LStatus = WFC_LEARN_EXIT_INVALID_TRAINING) and
    (LOutput = '') and
    (Pos('wfc-learn: invalid training: ', LError) = 1),
    'CRLF training input is rejected without standard output');

  LMalformed := ADJACENCY_1D_TEXT + #$80;
  LStatus := WfcLearnExecuteText(LCommand, LMalformed,
    LOutput, LError);
  Check((LStatus = WFC_LEARN_EXIT_INVALID_TRAINING) and
    (LOutput = '') and
    (Pos('document contains a non-ASCII byte at offset ', LError) > 0),
    'raw non-ASCII bytes are rejected before target-specific decoding');

  LStatus := WfcLearnExecuteText(LCommand, '', LOutput, LError);
  Check((LStatus = WFC_LEARN_EXIT_INVALID_TRAINING) and
    (LOutput = '') and
    (Pos('wfc-learn: invalid training: ', LError) = 1),
    'empty input is an invalid-training error rather than usage error');
end;

procedure TestStatusAndFailureContracts;
begin
  Check((WFC_LEARN_EXIT_SUCCESS = 0) and
    (WFC_LEARN_EXIT_INVALID_TRAINING = 1) and
    (WFC_LEARN_EXIT_USAGE = 2) and
    (WFC_LEARN_EXIT_IO = 3) and
    (WFC_LEARN_EXIT_INTERNAL = 70),
    'public exit statuses have their fixed values');
  Check((WfcLearnFailureExitCode(wlfkInvalidTraining) = 1) and
    (WfcLearnFailureExitCode(wlfkUsage) = 2) and
    (WfcLearnFailureExitCode(wlfkIo) = 3) and
    (WfcLearnFailureExitCode(wlfkInternal) = 70),
    'failure kinds remain distinguishable');
  Check(WfcLearnFormatFailure(wlfkUsage, 'bad'#10'arguments') =
    'wfc-learn: usage error: bad arguments'#10,
    'failure messages are one exact LF-terminated line');
  Check(WfcLearnFormatFailure(wlfkIo, '') =
    'wfc-learn: I/O error: unspecified failure'#10,
    'empty host failures retain an actionable class');
  Check((WFC_LEARN_CLI_VERSION = 2) and
    (WFC_LEARN_MAX_INPUT_LENGTH = 8388608),
    'the CLI and bounded training-input contracts are public');
end;

begin
  WriteLn('WFC training application conformance suite');
  WriteLn('==========================================');
  TestCommandParsing;
  TestHelpAndVersion;
  TestTrainingExecution;
  TestStatusAndFailureContracts;
  WriteLn('==========================================');
  WriteLn('Checks: ', GCheckCount, '  Failures: ', GFailureCount);
  if GFailureCount <> 0 then
  begin
    {$IFDEF PAS2JS}
    raise Exception.CreateFmt('%d learner-app checks failed',
      [GFailureCount]);
    {$ELSE}
    Halt(1);
    {$ENDIF}
  end;
end.
