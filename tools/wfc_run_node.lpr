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
program wfc_run_node;

{$mode delphi}{$H+}
{$modeswitch externalclass}

{$IFNDEF PAS2JS}
  {$FATAL This host must be compiled with pas2js for Node.js}
{$ENDIF}

uses
  SysUtils,
  JS,
  NodeJSApp,
  NodeJS,
  wfc_run_app;

const
  WFC_RUN_IO_CHUNK_SIZE = 65536;

type
  EWfcRunInputLimit = class(Exception);
  EWfcRunHostInternal = class(Exception);

  { The pinned pas2js Node binding omits fs.readSync. These declarations expose
    only the built-in synchronous byte primitives needed by the thin host. }
  TWfcRunNodeBuffer = class external name 'Buffer' (TJSObject)
  strict private
    FLength: NativeInt; external name 'length';
  public
    class function allocUnsafe(const ASize: NativeInt):
      TWfcRunNodeBuffer;
    class function concat(const ABuffers: TJSArray;
      const ATotalLength: NativeInt): TWfcRunNodeBuffer;
    class function from(const AText, AEncoding: String):
      TWfcRunNodeBuffer;
    function subarray(const AStart, AEnd: NativeInt):
      TWfcRunNodeBuffer;
    function AsText(const AEncoding: String): String;
      external name 'toString';
    property Length: NativeInt read FLength;
  end;

  TWfcRunNodeBuffers = array of TWfcRunNodeBuffer;

  TWfcRunNodeFileSystem = class external name 'Object' (TJSObject)
  public
    function openSync(const APath, AFlags: String): NativeInt;
    procedure closeSync(const AFileDescriptor: NativeInt);
    function readSync(const AFileDescriptor: NativeInt;
      const ABuffer: TWfcRunNodeBuffer; const AOffset,
      ALength: NativeInt; const APosition: JSValue): NativeInt;
    function writeSync(const AFileDescriptor: NativeInt;
      const ABuffer: TWfcRunNodeBuffer; const AOffset,
      ALength: NativeInt; const APosition: JSValue): NativeInt;
  end;

var
  GFileSystem: TWfcRunNodeFileSystem;

function JavaScriptExceptionMessage(const AException: JSValue): String;
begin
  if AException is TJSError then
    Result := TJSError(AException).Message
  else if JS.isObject(AException) and
      TJSObject(AException).hasOwnProperty('message') then
    Result := String(TJSObject(AException)['message'])
  else
    Result := String(AException);
  if Result = '' then
    Result := 'JavaScript host operation failed';
end;

function CommandArguments: TWfcRunArguments;
var
  I: Integer;
begin
  Result := nil;
  SetLength(Result, ParamCount);
  for I := 1 to ParamCount do
    Result[I - 1] := ParamStr(I);
end;

procedure AppendBuffer(var ABuffers: TWfcRunNodeBuffers;
  var ACount: Integer; const ABuffer: TWfcRunNodeBuffer);
var
  LCapacity: Integer;
begin
  if ACount = Length(ABuffers) then
  begin
    LCapacity := Length(ABuffers);
    if LCapacity = 0 then
      LCapacity := 16
    else
      LCapacity := LCapacity * 2;
    SetLength(ABuffers, LCapacity);
  end;
  ABuffers[ACount] := ABuffer;
  Inc(ACount);
end;

function ReadFileDescriptor(const AFileDescriptor,
  AMaximumLength: NativeInt): String;
var
  LBuffer: TWfcRunNodeBuffer;
  LBuffers: TWfcRunNodeBuffers;
  LCount: Integer;
  LRead: NativeInt;
  LRequest: NativeInt;
  LTotal: NativeInt;
begin
  Result := '';
  LBuffers := nil;
  LCount := 0;
  LTotal := 0;
  repeat
    LRequest := AMaximumLength - LTotal;
    if LRequest >= WFC_RUN_IO_CHUNK_SIZE then
      LRequest := WFC_RUN_IO_CHUNK_SIZE
    else
      Inc(LRequest);
    LBuffer := TWfcRunNodeBuffer.allocUnsafe(LRequest);
    LRead := GFileSystem.readSync(AFileDescriptor, LBuffer, 0,
      LRequest, JS.Null);
    if (LRead < 0) or (LRead > LRequest) then
      raise EWfcRunHostInternal.Create(
        'fs.readSync returned an invalid byte count');
    if LRead = 0 then
      Break;
    if LTotal > AMaximumLength - LRead then
      raise EWfcRunInputLimit.Create(
        'document exceeds the version-1 encoded length limit');
    if LRead <> LBuffer.Length then
      LBuffer := LBuffer.subarray(0, LRead);
    AppendBuffer(LBuffers, LCount, LBuffer);
    Inc(LTotal, LRead);
  until False;

  if LTotal = 0 then
    Exit('');
  SetLength(LBuffers, LCount);
  LBuffer := TWfcRunNodeBuffer.concat(TJSArray(LBuffers), LTotal);
  Result := LBuffer.AsText('latin1');
end;

function ReadInput(const APath: String;
  const AMaximumLength: NativeInt): String;
var
  LFileDescriptor: NativeInt;
  LOwnsFileDescriptor: Boolean;
begin
  LOwnsFileDescriptor := APath <> '-';
  if LOwnsFileDescriptor then
    LFileDescriptor := GFileSystem.openSync(APath, 'r')
  else
    LFileDescriptor := 0;
  try
    Result := ReadFileDescriptor(LFileDescriptor, AMaximumLength);
  finally
    if LOwnsFileDescriptor then
      GFileSystem.closeSync(LFileDescriptor);
  end;
end;

procedure WriteFileDescriptor(const AFileDescriptor: NativeInt;
  const AText: String);
var
  LBuffer: TWfcRunNodeBuffer;
  LOffset: NativeInt;
  LWritten: NativeInt;
begin
  if AText = '' then
    Exit;
  LBuffer := TWfcRunNodeBuffer.from(AText, 'utf8');
  LOffset := 0;
  while LOffset < LBuffer.Length do
  begin
    LWritten := GFileSystem.writeSync(AFileDescriptor, LBuffer,
      LOffset, LBuffer.Length - LOffset, JS.Null);
    if (LWritten <= 0) or
        (LWritten > LBuffer.Length - LOffset) then
      raise EWfcRunHostInternal.Create(
        'fs.writeSync returned an invalid byte count');
    Inc(LOffset, LWritten);
  end;
end;

procedure TryWriteStandardError(const AText: String);
begin
  try
    WriteFileDescriptor(2, AText);
  except
    { There is nowhere else to report a standard-error failure. }
  end;
end;

function EmitOutcome(const AStatus: Integer;
  const AStandardOutput, AStandardError: String): Integer;
begin
  try
    WriteFileDescriptor(1, AStandardOutput);
    WriteFileDescriptor(2, AStandardError);
    Result := AStatus;
  except
    on E: EWfcRunHostInternal do
    begin
      TryWriteStandardError(WfcRunFormatFailure(wrfkInternal,
        E.Message));
      Result := WFC_RUN_EXIT_INTERNAL;
    end;
    on E: Exception do
    begin
      TryWriteStandardError(WfcRunFormatFailure(wrfkIo,
        'cannot write process output: ' + E.Message));
      Result := WFC_RUN_EXIT_IO;
    end;
  else
    TryWriteStandardError(WfcRunFormatFailure(wrfkIo,
      'cannot write process output: ' +
      JavaScriptExceptionMessage(JS.JSExceptValue)));
    Result := WFC_RUN_EXIT_IO;
  end;
end;

function ReadArtifact(const APath, ALabel: String;
  const AMaximumLength: NativeInt;
  const AInvalidKind: TWfcRunFailureKind; out AText: String;
  out AExitCode: Integer): Boolean;
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
    on E: EWfcRunHostInternal do
      AExitCode := EmitOutcome(WFC_RUN_EXIT_INTERNAL, '',
        WfcRunFormatFailure(wrfkInternal, E.Message));
    on E: EOutOfMemory do
      AExitCode := EmitOutcome(WFC_RUN_EXIT_INTERNAL, '',
        WfcRunFormatFailure(wrfkInternal,
          E.ClassName + ': ' + E.Message));
    on E: Exception do
      AExitCode := EmitOutcome(WFC_RUN_EXIT_IO, '',
        WfcRunFormatFailure(wrfkIo,
          'cannot read ' + ALabel + ': ' + E.Message));
  else
    AExitCode := EmitOutcome(WFC_RUN_EXIT_IO, '',
      WfcRunFormatFailure(wrfkIo,
        'cannot read ' + ALabel + ': ' +
        JavaScriptExceptionMessage(JS.JSExceptValue)));
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
    GFileSystem := TWfcRunNodeFileSystem(Require('fs'));
    LExitCode := Run;
  except
    on E: Exception do
    begin
      TryWriteStandardError(WfcRunFormatFailure(wrfkInternal,
        E.ClassName + ': ' + E.Message));
      LExitCode := WFC_RUN_EXIT_INTERNAL;
    end;
  else
    TryWriteStandardError(WfcRunFormatFailure(wrfkInternal,
      JavaScriptExceptionMessage(JS.JSExceptValue)));
    LExitCode := WFC_RUN_EXIT_INTERNAL;
  end;
  TNJSProcess.exitCode := LExitCode;
end.
