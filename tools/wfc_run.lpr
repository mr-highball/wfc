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
program wfc_run;

{$mode delphi}{$H+}

uses
  Classes,
  SysUtils,
  wfc_run_app;

const
  WFC_RUN_IO_CHUNK_SIZE = 65536;
  {$IFDEF MSWINDOWS}
  { A drained redirected standard-input pipe reports this terminal condition
    through GetLastOSError instead of returning a zero-byte read. }
  WFC_RUN_WINDOWS_ERROR_BROKEN_PIPE = 109;
  {$ENDIF}

type
  EWfcRunInputLimit = class(Exception);

function CommandArguments: TWfcRunArguments;
var
  I: Integer;
begin
  Result := nil;
  SetLength(Result, ParamCount);
  for I := 1 to ParamCount do
    Result[I - 1] := ParamStr(I);
end;

procedure EnsureInputCapacity(var AText: String;
  const ARequiredLength, AMaximumLength: Integer);
var
  LCapacity: Integer;
begin
  if ARequiredLength <= Length(AText) then
    Exit;
  if ARequiredLength > AMaximumLength then
    raise EWfcRunInputLimit.Create(
      'document exceeds the version-1 encoded length limit');

  LCapacity := Length(AText);
  if LCapacity < WFC_RUN_IO_CHUNK_SIZE then
    LCapacity := WFC_RUN_IO_CHUNK_SIZE;
  while LCapacity < ARequiredLength do
  begin
    if LCapacity > AMaximumLength div 2 then
      LCapacity := AMaximumLength
    else
      LCapacity := LCapacity * 2;
  end;
  SetLength(AText, LCapacity);
end;

function ReadBoundedHandle(const AHandle: THandle;
  const AMaximumLength: Integer): String;
var
  LBuffer: array[0..WFC_RUN_IO_CHUNK_SIZE - 1] of Byte;
  LErrorCode: Integer;
  LErrorMessage: String;
  LRead: LongInt;
  LRequest: LongInt;
  LUsed: Integer;
begin
  Result := '';
  LUsed := 0;
  repeat
    LRequest := AMaximumLength - LUsed;
    if LRequest >= SizeOf(LBuffer) then
      LRequest := SizeOf(LBuffer)
    else
      Inc(LRequest);
    LRead := FileRead(AHandle, LBuffer[0], LRequest);
    if LRead < 0 then
    begin
      LErrorCode := GetLastOSError;
      {$IFDEF MSWINDOWS}
      if LErrorCode = WFC_RUN_WINDOWS_ERROR_BROKEN_PIPE then
        Break;
      {$ENDIF}
      if LErrorCode = 0 then
        LErrorMessage := 'unknown operating-system error'
      else
        LErrorMessage := SysErrorMessage(LErrorCode);
      raise EReadError.Create('input handle read failed: ' +
        LErrorMessage);
    end;
    if LRead = 0 then
      Break;
    if LUsed > AMaximumLength - LRead then
      raise EWfcRunInputLimit.Create(
        'document exceeds the version-1 encoded length limit');
    EnsureInputCapacity(Result, LUsed + LRead, AMaximumLength);
    Move(LBuffer[0], Result[LUsed + 1], LRead);
    Inc(LUsed, LRead);
  until False;
  SetLength(Result, LUsed);
end;

function ReadInput(const APath: String;
  const AMaximumLength: Integer): String;
var
  LStream: TFileStream;
begin
  if APath = '-' then
    Exit(ReadBoundedHandle(StdInputHandle, AMaximumLength));
  LStream := TFileStream.Create(APath,
    fmOpenRead or fmShareDenyNone);
  try
    if LStream.Size > AMaximumLength then
      raise EWfcRunInputLimit.Create(
        'document exceeds the version-1 encoded length limit');
    Result := ReadBoundedHandle(LStream.Handle, AMaximumLength);
  finally
    LStream.Free;
  end;
end;

procedure WriteHandleExact(const AHandle: THandle;
  const AText: String);
var
  LStream: THandleStream;
begin
  if AText = '' then
    Exit;
  LStream := THandleStream.Create(AHandle);
  try
    LStream.WriteBuffer(AText[1], Length(AText));
  finally
    LStream.Free;
  end;
end;

procedure TryWriteStandardError(const AText: String);
begin
  try
    WriteHandleExact(StdErrorHandle, AText);
  except
    { There is nowhere else to report a standard-error failure. }
  end;
end;

function EmitOutcome(const AStatus: Integer;
  const AStandardOutput, AStandardError: String): Integer;
begin
  try
    WriteHandleExact(StdOutputHandle, AStandardOutput);
    WriteHandleExact(StdErrorHandle, AStandardError);
    Result := AStatus;
  except
    on E: Exception do
    begin
      TryWriteStandardError(WfcRunFormatFailure(wrfkIo,
        'cannot write process output: ' + E.Message));
      Result := WFC_RUN_EXIT_IO;
    end;
  end;
end;

function ReadArtifact(const APath, ALabel: String;
  const AMaximumLength: Integer; const AInvalidKind: TWfcRunFailureKind;
  out AText: String; out AExitCode: Integer): Boolean;
begin
  Result := False;
  AText := '';
  AExitCode := WFC_RUN_EXIT_INTERNAL;
  try
    AText := ReadInput(APath, AMaximumLength);
    Exit(True);
  except
    on E: EWfcRunInputLimit do
      AExitCode := EmitOutcome(WfcRunFailureExitCode(AInvalidKind), '',
        WfcRunFormatFailure(AInvalidKind, E.Message));
    on E: EOutOfMemory do
      AExitCode := EmitOutcome(WFC_RUN_EXIT_INTERNAL, '',
        WfcRunFormatFailure(wrfkInternal,
          E.ClassName + ': ' + E.Message));
    on E: Exception do
      AExitCode := EmitOutcome(WFC_RUN_EXIT_IO, '',
        WfcRunFormatFailure(wrfkIo,
          'cannot read ' + ALabel + ': ' + E.Message));
  end;
end;

function Run: Integer;
var
  LArguments: TWfcRunArguments;
  LCommand: TWfcRunCommand;
  LError: String;
  LExitCode: Integer;
  LOutput: String;
  LRecipeText: String;
  LRunText: String;
begin
  LArguments := CommandArguments;
  if not WfcRunParseCommand(LArguments, LCommand, LError) then
    Exit(EmitOutcome(WFC_RUN_EXIT_USAGE, '',
      WfcRunFormatFailure(wrfkUsage, LError)));

  LRecipeText := '';
  LRunText := '';
  if LCommand.Kind = wrckExecute then
  begin
    if not ReadArtifact(LCommand.RecipePath, 'RECIPE',
        WFC_RUN_MAX_RECIPE_INPUT_LENGTH, wrfkInvalidRecipe,
        LRecipeText, LExitCode) then
      Exit(LExitCode);
    if not ReadArtifact(LCommand.RunPath, 'RUN',
        WFC_RUN_MAX_RUN_INPUT_LENGTH, wrfkInvalidRun,
        LRunText, LExitCode) then
      Exit(LExitCode);
  end;

  Result := WfcRunExecuteTexts(LCommand, LRecipeText, LRunText,
    LOutput, LError);
  Result := EmitOutcome(Result, LOutput, LError);
end;

var
  LExitCode: Integer;
begin
  try
    LExitCode := Run;
  except
    on E: Exception do
    begin
      TryWriteStandardError(WfcRunFormatFailure(wrfkInternal,
        E.ClassName + ': ' + E.Message));
      LExitCode := WFC_RUN_EXIT_INTERNAL;
    end;
  end;
  Halt(LExitCode);
end.
