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
  wfc_pipeline_text;

const
  WFC_VALIDATE_CLI_VERSION = 1;

  WFC_VALIDATE_EXIT_SUCCESS = 0;
  WFC_VALIDATE_EXIT_INVALID_ARTIFACT = 1;
  WFC_VALIDATE_EXIT_USAGE = 2;
  WFC_VALIDATE_EXIT_IO = 3;
  { Reserved for a future valid execution that produces a canonical
    non-solved result artifact. Recipe validation never returns this code. }
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
    wvckRecipe
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
  end;

function WfcValidateParseCommand(
  const AArguments: TWfcValidateArguments;
  out ACommand: TWfcValidateCommand;
  out AError: String): Boolean;

function WfcValidateExecuteText(const ACommand: TWfcValidateCommand;
  const AInputText: String; out AStandardOutput,
  AStandardError: String): Integer;

function WfcValidateHelpText: String;
function WfcValidateVersionText: String;
function WfcValidateFailureExitCode(
  const AKind: TWfcValidateFailureKind): Integer;
function WfcValidateFormatFailure(const AKind: TWfcValidateFailureKind;
  const AMessage: String): String;

implementation

uses
  wfc_pipeline_model;

function WfcValidateHelpText: String;
begin
  Result :=
    'Usage:'#10 +
    '  wfc-validate recipe [--quiet | --emit-canonical] [--] INPUT'#10 +
    '  wfc-validate --help'#10 +
    '  wfc-validate --version'#10 +
    #10 +
    'INPUT is one file path, or - to read standard input.'#10 +
    'Validation checks the canonical recipe and its static contracts; ' +
    'it does not solve it.'#10;
end;

function WfcValidateVersionText: String;
begin
  Result := 'wfc-validate ' + IntToStr(WFC_VALIDATE_CLI_VERSION) +
    ' (wfcpipeline=' + IntToStr(WFC_PIPELINE_TEXT_VERSION) + ')'#10;
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
    OneLineMessage(AMessage) + #10;
end;

procedure InitializeCommand(out ACommand: TWfcValidateCommand);
begin
  ACommand.Kind := wvckRecipe;
  ACommand.OutputMode := wvomSummary;
  ACommand.InputPath := '';
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

  if AArguments[0] <> 'recipe' then
  begin
    AError := 'the command must be recipe, --help, or --version';
    Exit(False);
  end;
  Result := ParseRecipeCommand(AArguments, ACommand, AError);
end;

function RecipeSummary(const AModel: TWfcPipelineModel): String;
begin
  Result := 'valid canonical wfcpipeline=' +
    IntToStr(WFC_PIPELINE_TEXT_VERSION) + ' signature=' +
    WfcPipelineSignatureHex(AModel.Signature) +
    ' resources=' + IntToStr(AModel.ResourceCount) +
    ' passes=' + IntToStr(AModel.PassCount) +
    ' dependencies=' + IntToStr(AModel.DependencyCount) +
    ' bridges=' + IntToStr(AModel.BridgeCount) +
    ' requirements=' + IntToStr(AModel.RequirementCount) + #10;
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
var
  LBadByte: Integer;
  LModel: TWfcPipelineModel;
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
    wvckRecipe:
      ;
  end;

  if Length(AInputText) > WFC_VALIDATE_MAX_INPUT_LENGTH then
  begin
    AStandardError := WfcValidateFormatFailure(wvfkInvalidArtifact,
      'document exceeds the version-1 encoded length limit');
    Exit(WFC_VALIDATE_EXIT_INVALID_ARTIFACT);
  end;
  LBadByte := FindNonAsciiByte(AInputText);
  if LBadByte >= 0 then
  begin
    AStandardError := WfcValidateFormatFailure(wvfkInvalidArtifact,
      'document contains a non-ASCII byte at offset ' +
      IntToStr(LBadByte));
    Exit(WFC_VALIDATE_EXIT_INVALID_ARTIFACT);
  end;

  LModel := nil;
  try
    try
      LModel := DecodeWfcPipelineModelText(AInputText);
      if LModel = nil then
        raise Exception.Create('recipe decoder returned no model');
      case ACommand.OutputMode of
        wvomSummary:
          AStandardOutput := RecipeSummary(LModel);
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
        AStandardError := WfcValidateFormatFailure(
          wvfkInvalidArtifact, E.Message);
        Result := WFC_VALIDATE_EXIT_INVALID_ARTIFACT;
      end;
      on E: EWfcPipelineModel do
      begin
        AStandardOutput := '';
        AStandardError := WfcValidateFormatFailure(
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
    LModel.Free;
  end;
end;

end.
