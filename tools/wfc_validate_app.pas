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
unit wfc_validate_app;

{$mode delphi}{$H+}

interface

uses
  SysUtils,
  wfc_pipeline_text,
  wfc_artifact_document;

const
  WFC_VALIDATE_CLI_VERSION = 2;

  WFC_VALIDATE_EXIT_SUCCESS = 0;
  WFC_VALIDATE_EXIT_INVALID_ARTIFACT = 1;
  WFC_VALIDATE_EXIT_USAGE = 2;
  WFC_VALIDATE_EXIT_IO = 3;
  { Retained for source compatibility. Validation, including exact replay of
    a valid non-solved result, never returns this execution-only status. }
  WFC_VALIDATE_EXIT_NOT_SOLVED = 4;
  WFC_VALIDATE_EXIT_INTERNAL = 70;

  WFC_VALIDATE_MAX_INPUT_LENGTH =
    WFC_PIPELINE_MAX_ENCODED_TEXT_LENGTH;

type
  TWfcValidateArguments = array of String;

  { New artifact families extend this closed command kind instead of changing
    the meaning of recipe validation. }
  TWfcValidateCommandKind = (
    wvckHelp,
    wvckVersion,
    wvckRecipe,
    wvckRules,
    wvckModel,
    wvckPattern2D,
    wvckSequence,
    wvckTraining,
    wvckRun,
    wvckResult,
    wvckPattern3D
  );

  TWfcValidateOutputMode = (
    wvomSummary,
    wvomQuiet,
    wvomCanonical
  );

  TWfcValidateFailureKind = (
    wvfkInvalidArtifact,
    wvfkUsage,
    wvfkIo,
    wvfkInternal
  );

  TWfcValidateCommand = record
    Kind: TWfcValidateCommandKind;
    OutputMode: TWfcValidateOutputMode;
    InputPath: String;
    RecipePath: String;
    RunPath: String;
    ReplayResult: Boolean;
  end;

function WfcValidateParseCommand(
  const AArguments: TWfcValidateArguments;
  out ACommand: TWfcValidateCommand;
  out AError: String): Boolean;

function WfcValidateExecuteText(const ACommand: TWfcValidateCommand;
  const AInputText: String; out AStandardOutput,
  AStandardError: String): Integer; overload;
function WfcValidateExecuteText(const ACommand: TWfcValidateCommand;
  const AInputText, ARecipeText, ARunText: String;
  out AStandardOutput, AStandardError: String): Integer; overload;
function WfcValidateCommandArtifactKind(
  const AKind: TWfcValidateCommandKind): TWfcArtifactKind;

function WfcValidateHelpText: String;
function WfcValidateVersionText: String;
function WfcValidateFailureExitCode(
  const AKind: TWfcValidateFailureKind): Integer;
function WfcValidateFormatFailure(const AKind: TWfcValidateFailureKind;
  const AMessage: String): String;
function WfcValidateFormatCommandFailure(const ACommand: TWfcValidateCommand;
  const AKind: TWfcValidateFailureKind; const AMessage: String): String;
function WfcValidateOneLineMessage(const AMessage: String): String;

implementation

uses
  wfc_pipeline_model;

function WfcValidateHelpText: String;
begin
  Result :=
    'Usage:'#10 +
    '  wfc-validate recipe [--quiet | --emit-canonical] [--] INPUT'#10 +
    '  wfc-validate rules|model|pattern2d|pattern3d|sequence|training [--quiet | --emit-canonical] [--] INPUT'#10 +
    '  wfc-validate run [--quiet | --emit-canonical] [--] RECIPE RUN'#10 +
    '  wfc-validate result [--replay] [--quiet | --emit-canonical] [--] RECIPE RUN RESULT'#10 +
    '  wfc-validate --help'#10 +
    '  wfc-validate --version'#10 +
    #10 +
    'Each path is a file, or - for standard input (at most one per command).'#10 +
    'Validation checks the canonical recipe and its static contracts; ' +
    'it does not solve it.'#10 +
    'Result validation checks stored artifact contracts, not generation history.'#10 +
    'Only result --replay executes the run and requires exact result bytes.'#10 +
    'Valid stored or replayed non-solved results succeed with exit code 0.'#10;
end;

function WfcValidateVersionText: String;
begin
  Result := 'wfc-validate ' + IntToStr(WFC_VALIDATE_CLI_VERSION) +
    ' (wfcpipeline=' + IntToStr(WFC_PIPELINE_TEXT_VERSION) + ',' +
    IntToStr(WFC_PIPELINE_VALUE_QUOTA_TEXT_VERSION) + ',' +
    IntToStr(WFC_PIPELINE_CONNECTIVITY_TEXT_VERSION) + ',' +
    IntToStr(WFC_PIPELINE_PATTERN_3D_TEXT_VERSION) + ',' +
    IntToStr(WFC_PIPELINE_MAX_SUPPORTED_TEXT_VERSION) + ')'#10;
end;

function WfcValidateFailureExitCode(
  const AKind: TWfcValidateFailureKind): Integer;
begin
  Result := WFC_VALIDATE_EXIT_INTERNAL;
  case AKind of
    wvfkInvalidArtifact:
      Result := WFC_VALIDATE_EXIT_INVALID_ARTIFACT;
    wvfkUsage:
      Result := WFC_VALIDATE_EXIT_USAGE;
    wvfkIo:
      Result := WFC_VALIDATE_EXIT_IO;
    wvfkInternal:
      Result := WFC_VALIDATE_EXIT_INTERNAL;
  end;
end;

function WfcValidateOneLineMessage(const AMessage: String): String;
var
  I: Integer;
begin
  { Diagnostics must not inject terminal controls, extra lines, or unbounded
    attacker-controlled text. Ordinary legacy diagnostics remain unchanged. }
  Result := Copy(AMessage, 1, 4096);
  if Result = '' then
    Exit('unspecified failure');
  for I := 1 to Length(Result) do
    if (Ord(Result[I]) < 32) or (Ord(Result[I]) = 127) then
      Result[I] := ' ';
end;

function WfcValidateFormatFailure(const AKind: TWfcValidateFailureKind;
  const AMessage: String): String;
var
  LLabel: String;
begin
  LLabel := 'internal error';
  case AKind of
    wvfkInvalidArtifact:
      LLabel := 'invalid recipe';
    wvfkUsage:
      LLabel := 'usage error';
    wvfkIo:
      LLabel := 'I/O error';
    wvfkInternal:
      LLabel := 'internal error';
  end;
  Result := 'wfc-validate: ' + LLabel + ': ' +
    WfcValidateOneLineMessage(AMessage) + #10;
end;

function WfcValidateCommandArtifactKind(
  const AKind: TWfcValidateCommandKind): TWfcArtifactKind;
begin
  case AKind of
    wvckRecipe: Result := wakRecipe;
    wvckRules: Result := wakRules;
    wvckModel: Result := wakModel;
    wvckPattern2D: Result := wakPattern2D;
    wvckPattern3D: Result := wakPattern3D;
    wvckSequence: Result := wakSequence;
    wvckTraining: Result := wakTraining;
    wvckRun: Result := wakRun;
    wvckResult: Result := wakResult;
  else
    raise EConvertError.Create('command does not select an artifact family');
  end;
end;

function WfcValidateFormatCommandFailure(const ACommand: TWfcValidateCommand;
  const AKind: TWfcValidateFailureKind; const AMessage: String): String;
begin
  if (AKind = wvfkInvalidArtifact) and (ACommand.Kind <> wvckRecipe) then
    Result := 'wfc-validate: invalid ' +
      WfcArtifactKindName(WfcValidateCommandArtifactKind(ACommand.Kind)) +
      ': ' + WfcValidateOneLineMessage(AMessage) + #10
  else Result := WfcValidateFormatFailure(AKind, AMessage);
end;

procedure InitializeCommand(out ACommand: TWfcValidateCommand);
begin
  ACommand.Kind := wvckRecipe;
  ACommand.OutputMode := wvomSummary;
  ACommand.InputPath := '';
  ACommand.RecipePath := '';
  ACommand.RunPath := '';
  ACommand.ReplayResult := False;
end;

function ParseRecipeCommand(const AArguments: TWfcValidateArguments;
  out ACommand: TWfcValidateCommand; out AError: String): Boolean;
var
  I: Integer;
  LEndOfOptions: Boolean;
  LHasCanonical: Boolean;
  LHasInput: Boolean;
  LHasQuiet: Boolean;
begin
  Result := False;
  AError := '';
  ACommand.Kind := wvckRecipe;
  ACommand.OutputMode := wvomSummary;
  ACommand.InputPath := '';
  LEndOfOptions := False;
  LHasCanonical := False;
  LHasInput := False;
  LHasQuiet := False;

  for I := 1 to Length(AArguments) - 1 do
  begin
    if not LEndOfOptions and (AArguments[I] = '--') then
    begin
      if LHasInput then
      begin
        AError := 'end-of-options marker must precede INPUT';
        Exit;
      end;
      LEndOfOptions := True;
      Continue;
    end;

    if not LEndOfOptions and (AArguments[I] = '--quiet') then
    begin
      if LHasInput then
      begin
        AError := 'recipe options must precede INPUT';
        Exit;
      end;
      if LHasQuiet then
      begin
        AError := '--quiet was specified more than once';
        Exit;
      end;
      if LHasCanonical then
      begin
        AError := '--quiet and --emit-canonical are mutually exclusive';
        Exit;
      end;
      LHasQuiet := True;
      ACommand.OutputMode := wvomQuiet;
      Continue;
    end;

    if not LEndOfOptions and
        (AArguments[I] = '--emit-canonical') then
    begin
      if LHasInput then
      begin
        AError := 'recipe options must precede INPUT';
        Exit;
      end;
      if LHasCanonical then
      begin
        AError := '--emit-canonical was specified more than once';
        Exit;
      end;
      if LHasQuiet then
      begin
        AError := '--quiet and --emit-canonical are mutually exclusive';
        Exit;
      end;
      LHasCanonical := True;
      ACommand.OutputMode := wvomCanonical;
      Continue;
    end;

    if not LEndOfOptions and (AArguments[I] <> '-') and
        (AArguments[I] <> '') and (AArguments[I][1] = '-') then
    begin
      AError := 'unknown recipe option; use -- before a path beginning with -';
      Exit;
    end;

    if LHasInput then
    begin
      AError := 'recipe validation accepts exactly one INPUT';
      Exit;
    end;
    if AArguments[I] = '' then
    begin
      AError := 'INPUT cannot be empty';
      Exit;
    end;
    ACommand.InputPath := AArguments[I];
    LHasInput := True;
  end;

  if not LHasInput then
  begin
    AError := 'recipe validation requires one INPUT';
    Exit;
  end;
  Result := True;
end;

function WfcValidateParseCommand(
  const AArguments: TWfcValidateArguments;
  out ACommand: TWfcValidateCommand;
  out AError: String): Boolean;
var
  I, LCount, LRequired, LStdinCount: Integer;
  LEnd, LHasMode: Boolean;
  LFamily, LArg: String;
  LPaths: array[0..2] of String;
begin
  InitializeCommand(ACommand);
  AError := '';
  if Length(AArguments) = 0 then
  begin
    AError := 'a command is required';
    Exit(False);
  end;

  if AArguments[0] = '--help' then
  begin
    if Length(AArguments) <> 1 then
    begin
      AError := '--help does not accept additional arguments';
      Exit(False);
    end;
    ACommand.Kind := wvckHelp;
    Exit(True);
  end;

  if AArguments[0] = '--version' then
  begin
    if Length(AArguments) <> 1 then
    begin
      AError := '--version does not accept additional arguments';
      Exit(False);
    end;
    ACommand.Kind := wvckVersion;
    Exit(True);
  end;

  if AArguments[0] = 'recipe' then
    Exit(ParseRecipeCommand(AArguments, ACommand, AError));
  if AArguments[0] = 'rules' then ACommand.Kind := wvckRules
  else if AArguments[0] = 'model' then ACommand.Kind := wvckModel
  else if AArguments[0] = 'pattern2d' then ACommand.Kind := wvckPattern2D
  else if AArguments[0] = 'pattern3d' then ACommand.Kind := wvckPattern3D
  else if AArguments[0] = 'sequence' then ACommand.Kind := wvckSequence
  else if AArguments[0] = 'training' then ACommand.Kind := wvckTraining
  else if AArguments[0] = 'run' then ACommand.Kind := wvckRun
  else if AArguments[0] = 'result' then ACommand.Kind := wvckResult
  else
  begin
    AError := 'the command must be rules, model, pattern2d, sequence, training, recipe, run, result, --help, or --version';
    Exit(False);
  end;
  LFamily := AArguments[0];
  LRequired := 1;
  if ACommand.Kind = wvckRun then LRequired := 2
  else if ACommand.Kind = wvckResult then LRequired := 3;
  LCount := 0; LStdinCount := 0; LEnd := False; LHasMode := False;
  for I := 1 to High(AArguments) do
  begin
    LArg := AArguments[I];
    if not LEnd and (LArg = '--') then
    begin
      if LCount <> 0 then
      begin AError := 'end-of-options marker must precede paths'; Exit(False); end;
      LEnd := True;
      Continue;
    end;
    if not LEnd and (LArg <> '-') and (LArg <> '') and (LArg[1] = '-') then
    begin
      if LCount <> 0 then
      begin AError := LFamily + ' options must precede paths'; Exit(False); end;
      if (LArg = '--quiet') or (LArg = '--emit-canonical') then
      begin
        if LHasMode then
        begin AError := '--quiet and --emit-canonical may be specified only once and are mutually exclusive'; Exit(False); end;
        LHasMode := True;
        if LArg = '--quiet' then ACommand.OutputMode := wvomQuiet
        else ACommand.OutputMode := wvomCanonical;
      end
      else if (LArg = '--replay') and (ACommand.Kind = wvckResult) then
      begin
        if ACommand.ReplayResult then
        begin AError := '--replay was specified more than once'; Exit(False); end;
        ACommand.ReplayResult := True;
      end
      else
      begin AError := 'unknown ' + LFamily + ' option; use -- before a path beginning with -'; Exit(False); end;
      Continue;
    end;
    if LArg = '' then
    begin AError := 'input paths cannot be empty'; Exit(False); end;
    if LCount = LRequired then
    begin AError := LFamily + ' requires exactly ' + IntToStr(LRequired) + ' input path(s)'; Exit(False); end;
    if LArg = '-' then
    begin
      Inc(LStdinCount);
      if LStdinCount > 1 then
      begin AError := 'at most one input path may read standard input'; Exit(False); end;
    end;
    LPaths[LCount] := LArg;
    Inc(LCount);
  end;
  if LCount <> LRequired then
  begin AError := LFamily + ' requires exactly ' + IntToStr(LRequired) + ' input path(s)'; Exit(False); end;
  ACommand.InputPath := LPaths[LRequired - 1];
  if LRequired >= 2 then ACommand.RecipePath := LPaths[0];
  if LRequired = 3 then ACommand.RunPath := LPaths[1];
  Result := True;
end;

function RecipeSummary(const AModel: TWfcPipelineModel): String;
begin
  Result := 'valid canonical wfcpipeline=' +
    IntToStr(WfcPipelineModelTextVersion(AModel)) + ' signature=' +
    WfcPipelineSignatureHex(AModel.Signature) +
    ' resources=' + IntToStr(AModel.ResourceCount) +
    ' passes=' + IntToStr(AModel.PassCount) +
    ' dependencies=' + IntToStr(AModel.DependencyCount) +
    ' bridges=' + IntToStr(AModel.BridgeCount) +
    ' requirements=' + IntToStr(AModel.RequirementCount);
  if AModel.ValueQuotaCount > 0 then
    Result := Result + ' value-quotas=' + IntToStr(AModel.ValueQuotaCount);
  if AModel.ConnectivityCount > 0 then
    Result := Result + ' connectivities=' + IntToStr(AModel.ConnectivityCount);
  Result := Result + #10;
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

function WfcValidateExecuteText(const ACommand: TWfcValidateCommand;
  const AInputText: String; out AStandardOutput,
  AStandardError: String): Integer;
begin
  Result := WfcValidateExecuteText(ACommand, AInputText, '', '',
    AStandardOutput, AStandardError);
end;

function WfcValidateExecuteText(const ACommand: TWfcValidateCommand;
  const AInputText, ARecipeText, ARunText: String;
  out AStandardOutput, AStandardError: String): Integer;
var
  LBadByte: Integer;
  LDocument: TWfcArtifactDocument;
begin
  AStandardOutput := '';
  AStandardError := '';
  case ACommand.Kind of
    wvckHelp:
      begin
        AStandardOutput := WfcValidateHelpText;
        Exit(WFC_VALIDATE_EXIT_SUCCESS);
      end;
    wvckVersion:
      begin
        AStandardOutput := WfcValidateVersionText;
        Exit(WFC_VALIDATE_EXIT_SUCCESS);
      end;
    wvckRecipe, wvckRules, wvckModel, wvckPattern2D, wvckPattern3D, wvckSequence,
    wvckTraining, wvckRun, wvckResult:
      ;
  else
    AStandardError := WfcValidateFormatFailure(wvfkUsage, 'unknown command kind');
    Exit(WFC_VALIDATE_EXIT_USAGE);
  end;

  { A closed case is also strict in pas2js; set membership alone accepts
    string property names such as '0' supplied by an untyped JS caller. }
  case ACommand.OutputMode of
    wvomSummary, wvomQuiet, wvomCanonical: ;
  else
    AStandardError := WfcValidateFormatFailure(wvfkUsage, 'unknown output mode');
    Exit(WFC_VALIDATE_EXIT_USAGE);
  end;
  {$IFDEF PAS2JS}
  if ACommand.Kind = wvckResult then
    if (ACommand.ReplayResult <> False) and (ACommand.ReplayResult <> True) then
    begin
      AStandardError := WfcValidateFormatFailure(wvfkUsage,
        'result replay flag must be Boolean');
      Exit(WFC_VALIDATE_EXIT_USAGE);
    end;
  {$ENDIF}
  if Length(AInputText) > WfcArtifactInputLimit(
      WfcValidateCommandArtifactKind(ACommand.Kind)) then
  begin
    AStandardError := WfcValidateFormatCommandFailure(ACommand, wvfkInvalidArtifact,
      'document exceeds the version-1 encoded length limit');
    Exit(WFC_VALIDATE_EXIT_INVALID_ARTIFACT);
  end;
  LBadByte := FindNonAsciiByte(AInputText);
  if LBadByte >= 0 then
  begin
    AStandardError := WfcValidateFormatCommandFailure(ACommand, wvfkInvalidArtifact,
      'document contains a non-ASCII byte at offset ' +
      IntToStr(LBadByte));
    Exit(WFC_VALIDATE_EXIT_INVALID_ARTIFACT);
  end;

  LDocument := nil;
  try
    try
      LDocument := TWfcArtifactDocument.Create(
        WfcValidateCommandArtifactKind(ACommand.Kind), AInputText, ARecipeText, ARunText);
      { Earlier callers may initialize only the original recipe fields. Never
        inspect the appended replay flag except for the new result command. }
      if ACommand.Kind = wvckResult then
        if ACommand.ReplayResult then LDocument.RequireReplay;
      case ACommand.OutputMode of
        wvomSummary:
          if ACommand.Kind = wvckRecipe then
            AStandardOutput := RecipeSummary(LDocument.Recipe)
          else AStandardOutput := LDocument.Summary;
        wvomQuiet:
          AStandardOutput := '';
        wvomCanonical:
          { The strict decoder has already proved byte-for-byte re-encode
            identity. Reusing the caller's string avoids another large copy. }
          AStandardOutput := AInputText;
      end;
      Result := WFC_VALIDATE_EXIT_SUCCESS;
    except
      on E: EConvertError do
      begin
        AStandardOutput := '';
        AStandardError := WfcValidateFormatCommandFailure(ACommand,
          wvfkInvalidArtifact, E.Message);
        Result := WFC_VALIDATE_EXIT_INVALID_ARTIFACT;
      end;
      on E: EWfcArtifactDocument do
      begin
        AStandardOutput := '';
        AStandardError := WfcValidateFormatCommandFailure(ACommand,
          wvfkInvalidArtifact, E.Message);
        Result := WFC_VALIDATE_EXIT_INVALID_ARTIFACT;
      end;
      on E: Exception do
      begin
        AStandardOutput := '';
        AStandardError := WfcValidateFormatFailure(wvfkInternal,
          E.ClassName + ': ' + E.Message);
        Result := WFC_VALIDATE_EXIT_INTERNAL;
      end;
    end;
  finally
    LDocument.Free;
  end;
end;

end.
