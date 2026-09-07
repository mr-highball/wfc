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
unit wfc_inspect_app;

{$mode delphi}{$H+}

interface

uses wfc_validate_app;

const
  WFC_INSPECT_CLI_VERSION = 1;
  WFC_INSPECT_DEFAULT_DETAIL_LIMIT = 256;

type
  TWfcInspectArguments = TWfcValidateArguments;
  TWfcInspectCommand = record
    Kind: TWfcValidateCommandKind;
    InputPath, RecipePath, RunPath: String;
    DetailLimit: Integer;
  end;

function WfcInspectParseCommand(const AArguments: TWfcInspectArguments;
  out ACommand: TWfcInspectCommand; out AError: String): Boolean;
function WfcInspectExecuteText(const ACommand: TWfcInspectCommand;
  const AInputText: String; out AStandardOutput,
  AStandardError: String): Integer; overload;
function WfcInspectExecuteText(const ACommand: TWfcInspectCommand;
  const AInputText, ARecipeText, ARunText: String;
  out AStandardOutput, AStandardError: String): Integer; overload;
function WfcInspectHelpText: String;
function WfcInspectVersionText: String;
function WfcInspectFormatFailure(const ACommand: TWfcInspectCommand;
  const AKind: TWfcValidateFailureKind; const AMessage: String): String;

implementation

uses SysUtils, wfc_text_codec, wfc_artifact_document, wfc_artifact_inspect;

function WfcInspectHelpText: String;
begin
  Result := 'Usage:'#10 +
    '  wfc-inspect rules|model|pattern2d|pattern3d|sequence|training|recipe [--limit N] [--] INPUT'#10 +
    '  wfc-inspect run [--limit N] [--] RECIPE RUN'#10 +
    '  wfc-inspect result [--limit N] [--] RECIPE RUN RESULT'#10 +
    '  wfc-inspect --help'#10 + '  wfc-inspect --version'#10 + #10 +
    'Each path is a file, or - for standard input (at most one per command).'#10 +
    'N is a canonical nonnegative integer; default 256, zero means summary only.'#10 +
    'One total detail-record budget applies across the entire report.'#10 +
    'Inspection validates stored contracts; it never executes or replays a run.'#10;
end;

function WfcInspectVersionText: String;
begin
  Result := 'wfc-inspect ' + IntToStr(WFC_INSPECT_CLI_VERSION) + #10;
end;

function WfcInspectFormatFailure(const ACommand: TWfcInspectCommand;
  const AKind: TWfcValidateFailureKind; const AMessage: String): String;
var C: TWfcValidateCommand;
begin
  C := Default(TWfcValidateCommand);
  C.Kind := ACommand.Kind;
  Result := WfcValidateFormatCommandFailure(C, AKind, AMessage);
  Result := 'wfc-inspect:' + Copy(Result, Length('wfc-validate:') + 1, Length(Result));
end;

function WfcInspectParseCommand(const AArguments: TWfcInspectArguments;
  out ACommand: TWfcInspectCommand; out AError: String): Boolean;
var I, N: Integer; LEnd, LPath, LLimit: Boolean;
  LArguments: TWfcValidateArguments; C: TWfcValidateCommand; S: String;
begin
  ACommand := Default(TWfcInspectCommand);
  ACommand.Kind := wvckRecipe;
  ACommand.DetailLimit := WFC_INSPECT_DEFAULT_DETAIL_LIMIT;
  AError := '';
  LArguments := nil;
  if (Length(AArguments) = 0) or (AArguments[0] = '--help') or
      (AArguments[0] = '--version') then
    LArguments := AArguments
  else
  begin
    SetLength(LArguments, Length(AArguments));
    LArguments[0] := AArguments[0]; N := 1; I := 1;
    LEnd := False; LPath := False; LLimit := False;
    while I < Length(AArguments) do
    begin
      S := AArguments[I];
      if not LEnd and (S = '--limit') then
      begin
        if LPath then
        begin AError := 'inspection options must precede paths'; Exit(False); end;
        if LLimit then
        begin AError := '--limit was specified more than once'; Exit(False); end;
        LLimit := True; Inc(I);
        if I = Length(AArguments) then
        begin AError := '--limit requires a canonical nonnegative integer'; Exit(False); end;
        try
          ACommand.DetailLimit := WfcTextParseCanonicalInteger(AArguments[I],
            '--limit', 'inspection');
        except
          on E: EConvertError do begin AError := E.Message; Exit(False); end;
        end;
      end
      else
      begin
        if not LEnd and (S <> '-') and (S <> '') and (S[1] = '-') then
        begin
          if S <> '--' then
          begin AError := 'unknown inspection option; use -- before a path beginning with -'; Exit(False); end;
          LEnd := True;
        end
        else LPath := True;
        LArguments[N] := S; Inc(N);
      end;
      Inc(I);
    end;
    SetLength(LArguments, N);
  end;
  Result := WfcValidateParseCommand(LArguments, C, AError);
  if not Result then Exit;
  ACommand.Kind := C.Kind; ACommand.InputPath := C.InputPath;
  ACommand.RecipePath := C.RecipePath; ACommand.RunPath := C.RunPath;
end;

function WfcInspectExecuteText(const ACommand: TWfcInspectCommand;
  const AInputText: String; out AStandardOutput, AStandardError: String): Integer;
begin
  Result := WfcInspectExecuteText(ACommand, AInputText, '', '',
    AStandardOutput, AStandardError);
end;

function WfcInspectExecuteText(const ACommand: TWfcInspectCommand;
  const AInputText, ARecipeText, ARunText: String;
  out AStandardOutput, AStandardError: String): Integer;
var D: TWfcArtifactDocument;
begin
  AStandardOutput := ''; AStandardError := '';
  case ACommand.Kind of
    wvckHelp: begin AStandardOutput := WfcInspectHelpText; Exit(0); end;
    wvckVersion: begin AStandardOutput := WfcInspectVersionText; Exit(0); end;
    wvckRules, wvckModel, wvckPattern2D, wvckPattern3D, wvckSequence, wvckTraining,
    wvckRecipe, wvckRun, wvckResult: ;
  else
    AStandardError := WfcInspectFormatFailure(ACommand, wvfkUsage, 'unknown command kind');
    Exit(WFC_VALIDATE_EXIT_USAGE);
  end;
  if (ACommand.DetailLimit < 0) or (ACommand.DetailLimit > High(Integer))
    {$IFDEF PAS2JS}or (ACommand.DetailLimit <> Trunc(ACommand.DetailLimit)){$ENDIF} then
  begin
    AStandardError := WfcInspectFormatFailure(ACommand, wvfkUsage,
      'detail limit must be a canonical nonnegative integer');
    Exit(WFC_VALIDATE_EXIT_USAGE);
  end;
  D := nil;
  try
    try
      D := TWfcArtifactDocument.Create(WfcValidateCommandArtifactKind(ACommand.Kind),
        AInputText, ARecipeText, ARunText);
      AStandardOutput := WfcInspectArtifact(D, ACommand.DetailLimit);
      Result := WFC_VALIDATE_EXIT_SUCCESS;
    except
      on E: EConvertError do
      begin
        AStandardOutput := '';
        AStandardError := WfcInspectFormatFailure(ACommand, wvfkInvalidArtifact, E.Message);
        Result := WFC_VALIDATE_EXIT_INVALID_ARTIFACT;
      end;
      on E: EWfcArtifactDocument do
      begin
        AStandardOutput := '';
        AStandardError := WfcInspectFormatFailure(ACommand, wvfkInvalidArtifact, E.Message);
        Result := WFC_VALIDATE_EXIT_INVALID_ARTIFACT;
      end;
      on E: Exception do
      begin
        AStandardOutput := '';
        AStandardError := WfcInspectFormatFailure(ACommand, wvfkInternal, E.ClassName + ': ' + E.Message);
        Result := WFC_VALIDATE_EXIT_INTERNAL;
      end;
    end;
  finally D.Free; end;
end;

end.
