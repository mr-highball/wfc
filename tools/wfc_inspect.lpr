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
program wfc_inspect;

{$mode delphi}{$H+}

uses SysUtils, wfc_validate_app, wfc_inspect_app, wfc_artifact_cli_io;

function Run: Integer;
var
  Args: TWfcInspectArguments; Command: TWfcInspectCommand;
  InputText, RecipeText, RunText, OutputText, ErrorText, ReadingRole: String;
begin
  Args := WfcArtifactNativeArguments;
  if not WfcInspectParseCommand(Args, Command, ErrorText) then
    Exit(WfcArtifactEmitOutcome('wfc-inspect', WFC_VALIDATE_EXIT_USAGE, '',
      WfcInspectFormatFailure(Command, wvfkUsage, ErrorText)));
  try
    WfcArtifactReadInputs(Command.Kind, Command.InputPath, Command.RecipePath,
      Command.RunPath, InputText, RecipeText, RunText, ReadingRole);
  except
    on E: EWfcArtifactInputLimit do
      Exit(WfcArtifactEmitOutcome('wfc-inspect', WFC_VALIDATE_EXIT_INVALID_ARTIFACT, '',
        WfcInspectFormatFailure(Command, wvfkInvalidArtifact, E.Message)));
    on E: EOutOfMemory do
      Exit(WfcArtifactEmitOutcome('wfc-inspect', WFC_VALIDATE_EXIT_INTERNAL, '',
        WfcInspectFormatFailure(Command, wvfkInternal, E.ClassName + ': ' + E.Message)));
    on E: Exception do
      Exit(WfcArtifactEmitOutcome('wfc-inspect', WFC_VALIDATE_EXIT_IO, '',
        WfcInspectFormatFailure(Command, wvfkIo, 'cannot read ' + ReadingRole + ': ' + E.Message)));
  end;
  Result := WfcInspectExecuteText(Command, InputText, RecipeText, RunText,
    OutputText, ErrorText);
  Result := WfcArtifactEmitOutcome('wfc-inspect', Result, OutputText, ErrorText);
end;

var Status: Integer;
begin
  try Status := Run;
  except
    on E: Exception do
    begin
      WfcArtifactTryWriteStandardError('wfc-inspect: internal error: ' +
        WfcValidateOneLineMessage(E.ClassName + ': ' + E.Message) + #10);
      Status := WFC_VALIDATE_EXIT_INTERNAL;
    end;
  end;
  Halt(Status);
end.
