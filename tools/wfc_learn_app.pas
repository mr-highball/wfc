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
unit wfc_learn_app;

{$mode delphi}{$H+}

interface

uses
  SysUtils,
  wfc_training_text;

const
  WFC_LEARN_CLI_VERSION = 3;

  WFC_LEARN_EXIT_SUCCESS = 0;
  WFC_LEARN_EXIT_INVALID_TRAINING = 1;
  WFC_LEARN_EXIT_USAGE = 2;
  WFC_LEARN_EXIT_IO = 3;
  WFC_LEARN_EXIT_INTERNAL = 70;

  WFC_LEARN_MAX_INPUT_LENGTH =
    WFC_TRAINING_MAX_ENCODED_TEXT_LENGTH;

type
  TWfcLearnArguments = array of String;

  TWfcLearnCommandKind = (
    wlckHelp,
    wlckVersion,
    wlckLearn
  );

  TWfcLearnOutputMode = (
    wlomRecipe,
    wlomModel,
    wlomQuiet
  );

  TWfcLearnFailureKind = (
    wlfkInvalidTraining,
    wlfkUsage,
    wlfkIo,
    wlfkInternal
  );

  TWfcLearnCommand = record
    Kind: TWfcLearnCommandKind;
    OutputMode: TWfcLearnOutputMode;
    InputPath: String;
  end;

function WfcLearnParseCommand(const AArguments: TWfcLearnArguments;
  out ACommand: TWfcLearnCommand; out AError: String): Boolean;

function WfcLearnExecuteText(const ACommand: TWfcLearnCommand;
  const AInputText: String; out AStandardOutput,
  AStandardError: String): Integer;

function WfcLearnHelpText: String;
function WfcLearnVersionText: String;
function WfcLearnFailureExitCode(
  const AKind: TWfcLearnFailureKind): Integer;
function WfcLearnFormatFailure(const AKind: TWfcLearnFailureKind;
  const AMessage: String): String;

implementation

uses
  Classes,
  wfc_model,
  wfc_pipeline_model,
  wfc_pipeline_text,
  wfc_training;

function WfcLearnHelpText: String;
begin
  Result :=
    'Usage:'#10 +
    '  wfc-learn [--model | --quiet] [--] INPUT'#10 +
    '  wfc-learn --help'#10 +
    '  wfc-learn --version'#10 +
    #10 +
    'INPUT is one canonical wfclearn=1, wfclearn=2, wfclearn=3, or wfclearn=4 file path, or - to read standard ' +
      'input.'#10 +
    'By default, training emits a canonical wfcpipeline=1 recipe, or ' +
      'wfcpipeline=2 for value quotas, or wfcpipeline=3 for authored connectivity.'#10 +
    '--model emits the learned standalone wfcm, wfcp, or wfcs model; ' +
      'it rejects sources with value quotas or connectivity because standalone models cannot retain either policy.'#10 +
    '--quiet validates and compiles the recipe without emitting output.'#10;
end;

function WfcLearnVersionText: String;
begin
  Result := 'wfc-learn ' + IntToStr(WFC_LEARN_CLI_VERSION) +
    ' (wfclearn=1,2,3,4)'#10;
end;

function WfcLearnFailureExitCode(
  const AKind: TWfcLearnFailureKind): Integer;
begin
  Result := WFC_LEARN_EXIT_INTERNAL;
  case AKind of
    wlfkInvalidTraining:
      Result := WFC_LEARN_EXIT_INVALID_TRAINING;
    wlfkUsage:
      Result := WFC_LEARN_EXIT_USAGE;
    wlfkIo:
      Result := WFC_LEARN_EXIT_IO;
    wlfkInternal:
      Result := WFC_LEARN_EXIT_INTERNAL;
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

function WfcLearnFormatFailure(const AKind: TWfcLearnFailureKind;
  const AMessage: String): String;
var
  LLabel: String;
begin
  LLabel := 'internal error';
  case AKind of
    wlfkInvalidTraining:
      LLabel := 'invalid training';
    wlfkUsage:
      LLabel := 'usage error';
    wlfkIo:
      LLabel := 'I/O error';
    wlfkInternal:
      LLabel := 'internal error';
  end;
  Result := 'wfc-learn: ' + LLabel + ': ' +
    OneLineMessage(AMessage) + #10;
end;

procedure InitializeCommand(out ACommand: TWfcLearnCommand);
begin
  ACommand.Kind := wlckLearn;
  ACommand.OutputMode := wlomRecipe;
  ACommand.InputPath := '';
end;

function ParseLearnCommand(const AArguments: TWfcLearnArguments;
  out ACommand: TWfcLearnCommand; out AError: String): Boolean;
var
  I: Integer;
  LEndOfOptions: Boolean;
  LHasInput: Boolean;
  LHasModel: Boolean;
  LHasQuiet: Boolean;
begin
  Result := False;
  AError := '';
  ACommand.Kind := wlckLearn;
  ACommand.OutputMode := wlomRecipe;
  ACommand.InputPath := '';
  LEndOfOptions := False;
  LHasInput := False;
  LHasModel := False;
  LHasQuiet := False;

  for I := 0 to Length(AArguments) - 1 do
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

    if not LEndOfOptions and (AArguments[I] = '--model') then
    begin
      if LHasInput then
      begin
        AError := 'options must precede INPUT';
        Exit;
      end;
      if LHasModel then
      begin
        AError := '--model was specified more than once';
        Exit;
      end;
      if LHasQuiet then
      begin
        AError := '--model and --quiet are mutually exclusive';
        Exit;
      end;
      LHasModel := True;
      ACommand.OutputMode := wlomModel;
      Continue;
    end;

    if not LEndOfOptions and (AArguments[I] = '--quiet') then
    begin
      if LHasInput then
      begin
        AError := 'options must precede INPUT';
        Exit;
      end;
      if LHasQuiet then
      begin
        AError := '--quiet was specified more than once';
        Exit;
      end;
      if LHasModel then
      begin
        AError := '--model and --quiet are mutually exclusive';
        Exit;
      end;
      LHasQuiet := True;
      ACommand.OutputMode := wlomQuiet;
      Continue;
    end;

    if not LEndOfOptions and (AArguments[I] <> '-') and
        (AArguments[I] <> '') and (AArguments[I][1] = '-') then
    begin
      AError := 'unknown option; use -- before a path beginning with -';
      Exit;
    end;

    if LHasInput then
    begin
      AError := 'training accepts exactly one INPUT';
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
    AError := 'training requires one INPUT';
    Exit;
  end;
  Result := True;
end;

function WfcLearnParseCommand(const AArguments: TWfcLearnArguments;
  out ACommand: TWfcLearnCommand; out AError: String): Boolean;
begin
  InitializeCommand(ACommand);
  AError := '';

  if (Length(AArguments) > 0) and
      (AArguments[0] = '--help') then
  begin
    if Length(AArguments) <> 1 then
    begin
      AError := '--help does not accept additional arguments';
      Exit(False);
    end;
    ACommand.Kind := wlckHelp;
    Exit(True);
  end;

  if (Length(AArguments) > 0) and
      (AArguments[0] = '--version') then
  begin
    if Length(AArguments) <> 1 then
    begin
      AError := '--version does not accept additional arguments';
      Exit(False);
    end;
    ACommand.Kind := wlckVersion;
    Exit(True);
  end;

  Result := ParseLearnCommand(AArguments, ACommand, AError);
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

function WfcLearnExecuteText(const ACommand: TWfcLearnCommand;
  const AInputText: String; out AStandardOutput,
  AStandardError: String): Integer;
var
  LBadByte: Integer;
  LDocument: TWfcTrainingDocument;
  LRecipe: TWfcPipelineModel;
begin
  AStandardOutput := '';
  AStandardError := '';
  case ACommand.Kind of
    wlckHelp:
      begin
        AStandardOutput := WfcLearnHelpText;
        Exit(WFC_LEARN_EXIT_SUCCESS);
      end;
    wlckVersion:
      begin
        AStandardOutput := WfcLearnVersionText;
        Exit(WFC_LEARN_EXIT_SUCCESS);
      end;
    wlckLearn:
      ;
  end;

  if Length(AInputText) > WFC_LEARN_MAX_INPUT_LENGTH then
  begin
    AStandardError := WfcLearnFormatFailure(wlfkInvalidTraining,
      'document exceeds the version-1 encoded length limit');
    Exit(WFC_LEARN_EXIT_INVALID_TRAINING);
  end;
  LBadByte := FindNonAsciiByte(AInputText);
  if LBadByte >= 0 then
  begin
    AStandardError := WfcLearnFormatFailure(wlfkInvalidTraining,
      'document contains a non-ASCII byte at offset ' +
      IntToStr(LBadByte));
    Exit(WFC_LEARN_EXIT_INVALID_TRAINING);
  end;

  LDocument := nil;
  LRecipe := nil;
  try
    try
      LDocument := DecodeWfcTrainingText(AInputText);
      if LDocument = nil then
        raise EWfcTraining.Create(
          'training decoder returned no document');

      case ACommand.OutputMode of
        wlomRecipe,
        wlomQuiet:
          begin
            LRecipe := LearnWfcTrainingRecipe(LDocument);
            if LRecipe = nil then
              raise EWfcTraining.Create(
                'training compiler returned no recipe');
            if ACommand.OutputMode = wlomRecipe then
              AStandardOutput := EncodeWfcPipelineModelText(LRecipe)
            else
              AStandardOutput := '';
          end;
        wlomModel:
          AStandardOutput := LearnWfcTrainingModelText(LDocument);
      end;
      Result := WFC_LEARN_EXIT_SUCCESS;
    except
      on E: EWfcTraining do
      begin
        AStandardOutput := '';
        AStandardError := WfcLearnFormatFailure(
          wlfkInvalidTraining, E.Message);
        Result := WFC_LEARN_EXIT_INVALID_TRAINING;
      end;
      on E: EWfcPipelineModel do
      begin
        AStandardOutput := '';
        AStandardError := WfcLearnFormatFailure(
          wlfkInvalidTraining, E.Message);
        Result := WFC_LEARN_EXIT_INVALID_TRAINING;
      end;
      on E: EWfcModel do
      begin
        AStandardOutput := '';
        AStandardError := WfcLearnFormatFailure(
          wlfkInvalidTraining, E.Message);
        Result := WFC_LEARN_EXIT_INVALID_TRAINING;
      end;
      on E: EConvertError do
      begin
        AStandardOutput := '';
        AStandardError := WfcLearnFormatFailure(
          wlfkInvalidTraining, E.Message);
        Result := WFC_LEARN_EXIT_INVALID_TRAINING;
      end;
      on E: ERangeError do
      begin
        AStandardOutput := '';
        AStandardError := WfcLearnFormatFailure(
          wlfkInvalidTraining, E.Message);
        Result := WFC_LEARN_EXIT_INVALID_TRAINING;
      end;
      on E: EInvalidOperation do
      begin
        AStandardOutput := '';
        AStandardError := WfcLearnFormatFailure(
          wlfkInvalidTraining, E.Message);
        Result := WFC_LEARN_EXIT_INVALID_TRAINING;
      end;
      on E: Exception do
      begin
        AStandardOutput := '';
        AStandardError := WfcLearnFormatFailure(wlfkInternal,
          E.ClassName + ': ' + E.Message);
        Result := WFC_LEARN_EXIT_INTERNAL;
      end;
    end;
  finally
    LRecipe.Free;
    LDocument.Free;
  end;
end;

end.
