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
program wfc_validate;

{$mode delphi}{$H+}

uses
  Classes,
  SysUtils,
  wfc_validate_app;

const
  WFC_VALIDATE_IO_CHUNK_SIZE = 65536;
  WFC_VALIDATE_OUTPUT_CHUNK_SIZE = 4096;
  {$IFDEF MSWINDOWS}
  { A drained redirected standard-input pipe reports this terminal condition
    through GetLastOSError instead of returning a zero-byte read. }
  WFC_VALIDATE_WINDOWS_ERROR_BROKEN_PIPE = 109;
  {$ENDIF}

type
  EWfcValidateInputLimit = class(Exception);

function CommandArguments: TWfcValidateArguments;
var
  I: Integer;
begin
  Result := nil;
  SetLength(Result, ParamCount);
  for I := 1 to ParamCount do
    Result[I - 1] := ParamStr(I);
end;

procedure EnsureInputCapacity(var AText: String;
  const ARequiredLength: Integer);
var
  LCapacity: Integer;
begin
  if ARequiredLength <= Length(AText) then
    Exit;
  if ARequiredLength > WFC_VALIDATE_MAX_INPUT_LENGTH then
    raise EWfcValidateInputLimit.Create(
      'document exceeds the version-1 encoded length limit');

  LCapacity := Length(AText);
  if LCapacity < WFC_VALIDATE_IO_CHUNK_SIZE then
    LCapacity := WFC_VALIDATE_IO_CHUNK_SIZE;
  while LCapacity < ARequiredLength do
  begin
    if LCapacity > WFC_VALIDATE_MAX_INPUT_LENGTH div 2 then
      LCapacity := WFC_VALIDATE_MAX_INPUT_LENGTH
    else
      LCapacity := LCapacity * 2;
  end;
  SetLength(AText, LCapacity);
end;

function ReadBoundedHandle(const AHandle: THandle): String;
var
  LBuffer: array[0..WFC_VALIDATE_IO_CHUNK_SIZE - 1] of Byte;
  LErrorCode: Integer;
  LErrorMessage: String;
  LRead: LongInt;
  LRequest: LongInt;
  LUsed: Integer;
begin
  Result := '';
  LUsed := 0;
  repeat
    LRequest := WFC_VALIDATE_MAX_INPUT_LENGTH - LUsed;
    if LRequest >= SizeOf(LBuffer) then
      LRequest := SizeOf(LBuffer)
    else
      Inc(LRequest);
    LRead := FileRead(AHandle, LBuffer[0], LRequest);
    if LRead < 0 then
    begin
      LErrorCode := GetLastOSError;
      {$IFDEF MSWINDOWS}
      if LErrorCode = WFC_VALIDATE_WINDOWS_ERROR_BROKEN_PIPE then
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
    if LUsed > WFC_VALIDATE_MAX_INPUT_LENGTH - LRead then
      raise EWfcValidateInputLimit.Create(
        'document exceeds the version-1 encoded length limit');
    EnsureInputCapacity(Result, LUsed + LRead);
    Move(LBuffer[0], Result[LUsed + 1], LRead);
    Inc(LUsed, LRead);
  until False;
  SetLength(Result, LUsed);
end;

function ReadInput(const APath: String): String;
var
  LStream: TFileStream;
begin
  if APath = '-' then
    Exit(ReadBoundedHandle(StdInputHandle));
  LStream := TFileStream.Create(APath,
    fmOpenRead or fmShareDenyNone);
  try
    if LStream.Size > WFC_VALIDATE_MAX_INPUT_LENGTH then
      raise EWfcValidateInputLimit.Create(
        'document exceeds the version-1 encoded length limit');
    Result := ReadBoundedHandle(LStream.Handle);
  finally
    LStream.Free;
  end;
end;

procedure WriteHandleExact(const AHandle: THandle;
  const AText: String);
var
  LErrorCode: Integer;
  LErrorMessage: String;
  LRequest: LongInt;
  LWritten: LongInt;
  LTotal: Integer;
begin
  if AText = '' then
    Exit;
  LTotal := 0;
  while LTotal < Length(AText) do
  begin
    LRequest := Length(AText) - LTotal;
    if LRequest > WFC_VALIDATE_OUTPUT_CHUNK_SIZE then
      LRequest := WFC_VALIDATE_OUTPUT_CHUNK_SIZE;
    LWritten := FileWrite(AHandle, AText[LTotal + 1],
      LRequest);
    if LWritten < 0 then
    begin
      LErrorCode := GetLastOSError;
      if LErrorCode = 0 then
        LErrorMessage := 'unknown operating-system error'
      else
        LErrorMessage := SysErrorMessage(LErrorCode);
      raise EWriteError.Create('output handle write failed: ' +
        LErrorMessage);
    end;
    if LWritten = 0 then
      raise EWriteError.Create(
        'output handle write made no progress');
    Inc(LTotal, LWritten);
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
      TryWriteStandardError(WfcValidateFormatFailure(wvfkIo,
        'cannot write process output: ' + E.Message));
      Result := WFC_VALIDATE_EXIT_IO;
    end;
  end;
end;

function Run: Integer;
var
  LArguments: TWfcValidateArguments;
  LCommand: TWfcValidateCommand;
  LError: String;
  LInput: String;
  LOutput: String;
begin
  LArguments := CommandArguments;
  if not WfcValidateParseCommand(LArguments, LCommand, LError) then
    Exit(EmitOutcome(WFC_VALIDATE_EXIT_USAGE, '',
      WfcValidateFormatFailure(wvfkUsage, LError)));

  LInput := '';
  if LCommand.Kind = wvckRecipe then
  begin
    try
      LInput := ReadInput(LCommand.InputPath);
    except
      on E: EWfcValidateInputLimit do
        Exit(EmitOutcome(WFC_VALIDATE_EXIT_INVALID_ARTIFACT, '',
          WfcValidateFormatFailure(wvfkInvalidArtifact, E.Message)));
      on E: EOutOfMemory do
        Exit(EmitOutcome(WFC_VALIDATE_EXIT_INTERNAL, '',
          WfcValidateFormatFailure(wvfkInternal,
            E.ClassName + ': ' + E.Message)));
      on E: Exception do
        Exit(EmitOutcome(WFC_VALIDATE_EXIT_IO, '',
          WfcValidateFormatFailure(wvfkIo,
            'cannot read INPUT: ' + E.Message)));
    end;
  end;

  Result := WfcValidateExecuteText(LCommand, LInput, LOutput, LError);
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
      TryWriteStandardError(WfcValidateFormatFailure(wvfkInternal,
        E.ClassName + ': ' + E.Message));
      LExitCode := WFC_VALIDATE_EXIT_INTERNAL;
    end;
  end;
  Halt(LExitCode);
end.
