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
unit wfc_run_app;

{$mode delphi}{$H+}

interface

uses
  wfc_pipeline_text,
  wfc_pipeline_run_text;

const
  WFC_RUN_CLI_VERSION = 1;

  WFC_RUN_EXIT_SOLVED = 0;
  WFC_RUN_EXIT_INVALID_INVOCATION = 1;
  WFC_RUN_EXIT_USAGE = 2;
  WFC_RUN_EXIT_IO = 3;
  WFC_RUN_EXIT_NOT_SOLVED = 4;
  WFC_RUN_EXIT_INTERNAL = 70;

  WFC_RUN_MAX_RECIPE_INPUT_LENGTH =
    WFC_PIPELINE_MAX_ENCODED_TEXT_LENGTH;
  WFC_RUN_MAX_RUN_INPUT_LENGTH =
    WFC_PIPELINE_RUN_MAX_ENCODED_TEXT_LENGTH;

type
  TWfcRunArguments = array of String;

  TWfcRunCommandKind = (
    wrckHelp,
    wrckVersion,
    wrckExecute
  );

  TWfcRunOutputMode = (
    wromCanonical,
    wromQuiet
  );

  TWfcRunFailureKind = (
    wrfkInvalidRecipe,
    wrfkInvalidRun,
    wrfkInvalidInvocation,
    wrfkUsage,
    wrfkIo,
    wrfkInternal
  );

  TWfcRunCommand = record
    Kind: TWfcRunCommandKind;
    OutputMode: TWfcRunOutputMode;
    RecipePath: String;
    RunPath: String;
  end;

function WfcRunParseCommand(const AArguments: TWfcRunArguments;
  out ACommand: TWfcRunCommand; out AError: String): Boolean;

function WfcRunExecuteTexts(const ACommand: TWfcRunCommand;
  const ARecipeText, ARunText: String; out AStandardOutput,
  AStandardError: String): Integer;

function WfcRunHelpText: String;
function WfcRunVersionText: String;
function WfcRunFailureExitCode(const AKind: TWfcRunFailureKind): Integer;
function WfcRunFormatFailure(const AKind: TWfcRunFailureKind;
  const AMessage: String): String;

implementation

uses
  SysUtils,
  wfc_pipeline_model,
  wfc_pipeline_run,
  wfc_pipeline_compile,
  wfc_pipeline_result,
  wfc_pipeline_result_text,
  wfc_pipeline_runtime;

function WfcRunHelpText: String;
begin
  Result :=
    'Usage:'#10 +
    '  wfc-run [--quiet] [--] RECIPE RUN'#10 +
    '  wfc-run --help'#10 +
    '  wfc-run --version'#10 +
    #10 +
    'RECIPE and RUN are canonical artifact files; one may be - to read ' +
    'standard input.'#10 +
    'Solved and non-solved invocations emit an exact canonical result ' +
    'unless --quiet is used.'#10;
end;

function WfcRunVersionText: String;
begin
  Result := 'wfc-run ' + IntToStr(WFC_RUN_CLI_VERSION) +
    ' (wfcpipeline=' + IntToStr(WFC_PIPELINE_TEXT_VERSION) + ',' +
    IntToStr(WFC_PIPELINE_VALUE_QUOTA_TEXT_VERSION) + ',' +
    IntToStr(WFC_PIPELINE_CONNECTIVITY_TEXT_VERSION) + ',' +
    IntToStr(WFC_PIPELINE_MAX_SUPPORTED_TEXT_VERSION) +
    ', wfcpipeline-run=' + IntToStr(WFC_PIPELINE_RUN_TEXT_VERSION) +
    ', wfcpipeline-result=' +
    IntToStr(WFC_PIPELINE_RESULT_TEXT_VERSION) +
    ', runtime=' + IntToStr(WFC_PIPELINE_RUNTIME_VERSION) + ')'#10;
end;

function WfcRunFailureExitCode(const AKind: TWfcRunFailureKind): Integer;
begin
  Result := WFC_RUN_EXIT_INTERNAL;
  case AKind of
    wrfkInvalidRecipe,
    wrfkInvalidRun,
    wrfkInvalidInvocation:
      Result := WFC_RUN_EXIT_INVALID_INVOCATION;
    wrfkUsage:
      Result := WFC_RUN_EXIT_USAGE;
    wrfkIo:
      Result := WFC_RUN_EXIT_IO;
    wrfkInternal:
      Result := WFC_RUN_EXIT_INTERNAL;
  end;
end;

function OneLineMessage(const AMessage: String): String;
var
  I: Integer;
begin
  Result := AMessage;
  if Result = '' then
    Exit('unspecified failure');
  for I := 1 to Length(Result) do
    if (Result[I] = #10) or (Result[I] = #13) then
      Result[I] := ' ';
end;

function WfcRunFormatFailure(const AKind: TWfcRunFailureKind;
  const AMessage: String): String;
var
  LLabel: String;
begin
  LLabel := 'internal error';
  case AKind of
    wrfkInvalidRecipe:
      LLabel := 'invalid recipe';
    wrfkInvalidRun:
      LLabel := 'invalid run';
    wrfkInvalidInvocation:
      LLabel := 'invalid invocation';
    wrfkUsage:
      LLabel := 'usage error';
    wrfkIo:
      LLabel := 'I/O error';
    wrfkInternal:
      LLabel := 'internal error';
  end;
  Result := 'wfc-run: ' + LLabel + ': ' +
    OneLineMessage(AMessage) + #10;
end;

procedure InitializeCommand(out ACommand: TWfcRunCommand);
begin
  ACommand.Kind := wrckExecute;
  ACommand.OutputMode := wromCanonical;
  ACommand.RecipePath := '';
  ACommand.RunPath := '';
end;

function ParseExecutionCommand(const AArguments: TWfcRunArguments;
  out ACommand: TWfcRunCommand; out AError: String): Boolean;
var
  I: Integer;
  LEndOfOptions: Boolean;
  LHasQuiet: Boolean;
  LPathCount: Integer;
begin
  Result := False;
  AError := '';
  InitializeCommand(ACommand);
  LEndOfOptions := False;
  LHasQuiet := False;
  LPathCount := 0;

  for I := 0 to Length(AArguments) - 1 do
  begin
    if not LEndOfOptions and (AArguments[I] = '--') then
    begin
      if LPathCount <> 0 then
      begin
        AError := 'end-of-options marker must precede RECIPE and RUN';
        Exit;
      end;
      LEndOfOptions := True;
      Continue;
    end;

    if not LEndOfOptions and (AArguments[I] = '--quiet') then
    begin
      if LPathCount <> 0 then
      begin
        AError := '--quiet must precede RECIPE and RUN';
        Exit;
      end;
      if LHasQuiet then
      begin
        AError := '--quiet was specified more than once';
        Exit;
      end;
      LHasQuiet := True;
      ACommand.OutputMode := wromQuiet;
      Continue;
    end;

    if not LEndOfOptions and (AArguments[I] <> '-') and
        (AArguments[I] <> '') and (AArguments[I][1] = '-') then
    begin
      AError := 'unknown option; use -- before a path beginning with -';
      Exit;
    end;

    if AArguments[I] = '' then
    begin
      AError := 'RECIPE and RUN paths cannot be empty';
      Exit;
    end;
    if LPathCount = 0 then
      ACommand.RecipePath := AArguments[I]
    else if LPathCount = 1 then
      ACommand.RunPath := AArguments[I]
    else
    begin
      AError := 'execution accepts exactly one RECIPE and one RUN';
      Exit;
    end;
    Inc(LPathCount);
  end;

  if LPathCount <> 2 then
  begin
    AError := 'execution requires one RECIPE and one RUN';
    Exit;
  end;
  if (ACommand.RecipePath = '-') and (ACommand.RunPath = '-') then
  begin
    AError := 'only one of RECIPE and RUN may be standard input';
    Exit;
  end;
  Result := True;
end;

function WfcRunParseCommand(const AArguments: TWfcRunArguments;
  out ACommand: TWfcRunCommand; out AError: String): Boolean;
begin
  InitializeCommand(ACommand);
  AError := '';
  if Length(AArguments) = 0 then
  begin
    AError := 'RECIPE and RUN are required';
    Exit(False);
  end;

  if AArguments[0] = '--help' then
  begin
    if Length(AArguments) <> 1 then
    begin
      AError := '--help does not accept additional arguments';
      Exit(False);
    end;
    ACommand.Kind := wrckHelp;
    Exit(True);
  end;

  if AArguments[0] = '--version' then
  begin
    if Length(AArguments) <> 1 then
    begin
      AError := '--version does not accept additional arguments';
      Exit(False);
    end;
    ACommand.Kind := wrckVersion;
    Exit(True);
  end;

  Result := ParseExecutionCommand(AArguments, ACommand, AError);
end;

function FindNonAsciiByte(const AText: String): Integer;
var
  I: Integer;
begin
  for I := 1 to Length(AText) do
    if Ord(AText[I]) > 127 then
      Exit(I - 1);
  Result := -1;
end;

function FailureOutcome(const AKind: TWfcRunFailureKind;
  const AMessage: String; out AStandardOutput,
  AStandardError: String): Integer;
begin
  AStandardOutput := '';
  AStandardError := WfcRunFormatFailure(AKind, AMessage);
  Result := WfcRunFailureExitCode(AKind);
end;

function WfcRunExecuteTexts(const ACommand: TWfcRunCommand;
  const ARecipeText, ARunText: String; out AStandardOutput,
  AStandardError: String): Integer;
var
  LBadByte: Integer;
  LRecipe: TWfcPipelineModel;
  LResult: TWfcPipelineResult;
  LRun: TWfcPipelineRun;
begin
  AStandardOutput := '';
  AStandardError := '';
  case ACommand.Kind of
    wrckHelp:
      begin
        AStandardOutput := WfcRunHelpText;
        Exit(WFC_RUN_EXIT_SOLVED);
      end;
    wrckVersion:
      begin
        AStandardOutput := WfcRunVersionText;
        Exit(WFC_RUN_EXIT_SOLVED);
      end;
    wrckExecute:
      ;
  end;

  if Length(ARecipeText) > WFC_RUN_MAX_RECIPE_INPUT_LENGTH then
    Exit(FailureOutcome(wrfkInvalidRecipe,
      'document exceeds the version-1 encoded length limit',
      AStandardOutput, AStandardError));
  LBadByte := FindNonAsciiByte(ARecipeText);
  if LBadByte >= 0 then
    Exit(FailureOutcome(wrfkInvalidRecipe,
      'document contains a non-ASCII byte at offset ' +
      IntToStr(LBadByte), AStandardOutput, AStandardError));

  if Length(ARunText) > WFC_RUN_MAX_RUN_INPUT_LENGTH then
    Exit(FailureOutcome(wrfkInvalidRun,
      'document exceeds the version-1 encoded length limit',
      AStandardOutput, AStandardError));
  LBadByte := FindNonAsciiByte(ARunText);
  if LBadByte >= 0 then
    Exit(FailureOutcome(wrfkInvalidRun,
      'document contains a non-ASCII byte at offset ' +
      IntToStr(LBadByte), AStandardOutput, AStandardError));

  LRecipe := nil;
  LRun := nil;
  LResult := nil;
  try
    try
      LRecipe := DecodeWfcPipelineModelText(ARecipeText);
      if LRecipe = nil then
        raise Exception.Create('recipe decoder returned no model');
    except
      on E: EConvertError do
        Exit(FailureOutcome(wrfkInvalidRecipe, E.Message,
          AStandardOutput, AStandardError));
      on E: EWfcPipelineModel do
        Exit(FailureOutcome(wrfkInvalidRecipe, E.Message,
          AStandardOutput, AStandardError));
      on E: Exception do
        Exit(FailureOutcome(wrfkInternal,
          E.ClassName + ': ' + E.Message,
          AStandardOutput, AStandardError));
    end;

    try
      LRun := DecodeWfcPipelineRunText(ARunText, LRecipe);
      if LRun = nil then
        raise Exception.Create('run decoder returned no invocation');
    except
      on E: EConvertError do
        Exit(FailureOutcome(wrfkInvalidRun, E.Message,
          AStandardOutput, AStandardError));
      on E: EWfcPipelineRun do
        Exit(FailureOutcome(wrfkInvalidRun, E.Message,
          AStandardOutput, AStandardError));
      on E: Exception do
        Exit(FailureOutcome(wrfkInternal,
          E.ClassName + ': ' + E.Message,
          AStandardOutput, AStandardError));
    end;

    try
      LResult := ExecuteWfcPipeline(LRecipe, LRun);
      if LResult = nil then
        raise Exception.Create('pipeline runtime returned no result');
    except
      on E: EWfcPipelineRuntime do
        Exit(FailureOutcome(wrfkInvalidInvocation, E.Message,
          AStandardOutput, AStandardError));
      on E: EWfcPipelineCompile do
        Exit(FailureOutcome(wrfkInvalidInvocation, E.Message,
          AStandardOutput, AStandardError));
      on E: Exception do
        Exit(FailureOutcome(wrfkInternal,
          E.ClassName + ': ' + E.Message,
          AStandardOutput, AStandardError));
    end;

    try
      AStandardOutput := EncodeWfcPipelineResultText(LResult);
    except
      on E: Exception do
        Exit(FailureOutcome(wrfkInternal,
          E.ClassName + ': ' + E.Message,
          AStandardOutput, AStandardError));
    end;
    if ACommand.OutputMode = wromQuiet then
      AStandardOutput := '';
    if LResult.Status = wprsSolved then
      Result := WFC_RUN_EXIT_SOLVED
    else
      Result := WFC_RUN_EXIT_NOT_SOLVED;
  finally
    LResult.Free;
    LRun.Free;
    LRecipe.Free;
  end;
end;

end.
