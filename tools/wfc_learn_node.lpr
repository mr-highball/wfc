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
program wfc_learn_node;

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
  wfc_learn_app;

const
  WFC_LEARN_IO_CHUNK_SIZE = 65536;

type
  EWfcLearnInputLimit = class(Exception);
  EWfcLearnHostInternal = class(Exception);

  { The pinned pas2js Node binding omits fs.readSync. These declarations expose
    only the built-in synchronous byte primitives needed by the thin host. }
  TWfcNodeBuffer = class external name 'Buffer' (TJSObject)
  strict private
    FLength: NativeInt; external name 'length';
  public
    class function allocUnsafe(const ASize: NativeInt):
      TWfcNodeBuffer;
    class function concat(const ABuffers: TJSArray;
      const ATotalLength: NativeInt): TWfcNodeBuffer;
    class function from(const AText, AEncoding: String):
      TWfcNodeBuffer;
    function CopyTo(const ATarget: TWfcNodeBuffer;
      const ATargetStart, ASourceStart,
      ASourceEnd: NativeInt): NativeInt; external name 'copy';
    function AsText(const AEncoding: String): String;
      external name 'toString';
    property Length: NativeInt read FLength;
  end;

  TWfcNodeBuffers = array of TWfcNodeBuffer;

  TWfcNodeFileSystem = class external name 'Object' (TJSObject)
  public
    function openSync(const APath, AFlags: String): NativeInt;
    procedure closeSync(const AFileDescriptor: NativeInt);
    function readSync(const AFileDescriptor: NativeInt;
      const ABuffer: TWfcNodeBuffer; const AOffset,
      ALength: NativeInt; const APosition: JSValue): NativeInt;
    function writeSync(const AFileDescriptor: NativeInt;
      const ABuffer: TWfcNodeBuffer; const AOffset,
      ALength: NativeInt; const APosition: JSValue): NativeInt;
  end;

var
  GFileSystem: TWfcNodeFileSystem;

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

function CommandArguments: TWfcLearnArguments;
var
  I: Integer;
begin
  Result := nil;
  SetLength(Result, ParamCount);
  for I := 1 to ParamCount do
    Result[I - 1] := ParamStr(I);
end;

procedure AppendBuffer(var ABuffers: TWfcNodeBuffers;
  var ACount: Integer; const ABuffer: TWfcNodeBuffer);
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

function ReadFileDescriptor(const AFileDescriptor: NativeInt): String;
var
  LBuffer: TWfcNodeBuffer;
  LBuffers: TWfcNodeBuffers;
  LChunk: TWfcNodeBuffer;
  LCopied: NativeInt;
  LCount: Integer;
  LRead: NativeInt;
  LRequest: NativeInt;
  LTotal: NativeInt;
begin
  Result := '';
  LBuffers := nil;
  LCount := 0;
  LTotal := 0;
  LBuffer := TWfcNodeBuffer.allocUnsafe(WFC_LEARN_IO_CHUNK_SIZE);
  repeat
    LRequest := WFC_LEARN_MAX_INPUT_LENGTH - LTotal;
    if LRequest >= WFC_LEARN_IO_CHUNK_SIZE then
      LRequest := WFC_LEARN_IO_CHUNK_SIZE
    else
      Inc(LRequest);
    LRead := GFileSystem.readSync(AFileDescriptor, LBuffer, 0,
      LRequest, JS.Null);
    if (LRead < 0) or (LRead > LRequest) then
      raise EWfcLearnHostInternal.Create(
        'fs.readSync returned an invalid byte count');
    if LRead = 0 then
      Break;
    if LTotal > WFC_LEARN_MAX_INPUT_LENGTH - LRead then
      raise EWfcLearnInputLimit.Create(
        'document exceeds the version-1 encoded length limit');
    LChunk := TWfcNodeBuffer.allocUnsafe(LRead);
    LCopied := LBuffer.CopyTo(LChunk, 0, 0, LRead);
    if LCopied <> LRead then
      raise EWfcLearnHostInternal.Create(
        'Buffer.copy returned an invalid byte count');
    AppendBuffer(LBuffers, LCount, LChunk);
    Inc(LTotal, LRead);
  until False;

  if LTotal = 0 then
    Exit('');
  SetLength(LBuffers, LCount);
  LBuffer := TWfcNodeBuffer.concat(TJSArray(LBuffers), LTotal);
  Result := LBuffer.AsText('latin1');
end;

function ReadInput(const APath: String): String;
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
    Result := ReadFileDescriptor(LFileDescriptor);
  finally
    if LOwnsFileDescriptor then
      GFileSystem.closeSync(LFileDescriptor);
  end;
end;

procedure WriteFileDescriptor(const AFileDescriptor: NativeInt;
  const AText: String);
var
  LBuffer: TWfcNodeBuffer;
  LOffset: NativeInt;
  LWritten: NativeInt;
begin
  if AText = '' then
    Exit;
  LBuffer := TWfcNodeBuffer.from(AText, 'utf8');
  LOffset := 0;
  while LOffset < LBuffer.Length do
  begin
    LWritten := GFileSystem.writeSync(AFileDescriptor, LBuffer,
      LOffset, LBuffer.Length - LOffset, JS.Null);
    if (LWritten <= 0) or
        (LWritten > LBuffer.Length - LOffset) then
      raise EWfcLearnHostInternal.Create(
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
    on E: EWfcLearnHostInternal do
    begin
      TryWriteStandardError(WfcLearnFormatFailure(wlfkInternal,
        E.Message));
      Result := WFC_LEARN_EXIT_INTERNAL;
    end;
    on E: Exception do
    begin
      TryWriteStandardError(WfcLearnFormatFailure(wlfkIo,
        'cannot write process output: ' + E.Message));
      Result := WFC_LEARN_EXIT_IO;
    end;
  else
    TryWriteStandardError(WfcLearnFormatFailure(wlfkIo,
      'cannot write process output: ' +
      JavaScriptExceptionMessage(JS.JSExceptValue)));
    Result := WFC_LEARN_EXIT_IO;
  end;
end;

function Run: Integer;
var
  LArguments: TWfcLearnArguments;
  LCommand: TWfcLearnCommand;
  LError: String;
  LInput: String;
  LOutput: String;
begin
  LArguments := CommandArguments;
  if not WfcLearnParseCommand(LArguments, LCommand, LError) then
    Exit(EmitOutcome(WFC_LEARN_EXIT_USAGE, '',
      WfcLearnFormatFailure(wlfkUsage, LError)));

  LInput := '';
  if LCommand.Kind = wlckLearn then
  begin
    try
      LInput := ReadInput(LCommand.InputPath);
    except
      on E: EWfcLearnInputLimit do
        Exit(EmitOutcome(WFC_LEARN_EXIT_INVALID_TRAINING, '',
          WfcLearnFormatFailure(wlfkInvalidTraining, E.Message)));
      on E: EWfcLearnHostInternal do
        Exit(EmitOutcome(WFC_LEARN_EXIT_INTERNAL, '',
          WfcLearnFormatFailure(wlfkInternal, E.Message)));
      on E: EOutOfMemory do
        Exit(EmitOutcome(WFC_LEARN_EXIT_INTERNAL, '',
          WfcLearnFormatFailure(wlfkInternal,
            E.ClassName + ': ' + E.Message)));
      on E: Exception do
        Exit(EmitOutcome(WFC_LEARN_EXIT_IO, '',
          WfcLearnFormatFailure(wlfkIo,
            'cannot read INPUT: ' + E.Message)));
    else
      Exit(EmitOutcome(WFC_LEARN_EXIT_IO, '',
        WfcLearnFormatFailure(wlfkIo,
          'cannot read INPUT: ' +
          JavaScriptExceptionMessage(JS.JSExceptValue))));
    end;
  end;

  Result := WfcLearnExecuteText(LCommand, LInput, LOutput, LError);
  Result := EmitOutcome(Result, LOutput, LError);
end;

var
  LExitCode: Integer;
begin
  try
    GFileSystem := TWfcNodeFileSystem(Require('fs'));
    LExitCode := Run;
  except
    on E: Exception do
    begin
      TryWriteStandardError(WfcLearnFormatFailure(wlfkInternal,
        E.ClassName + ': ' + E.Message));
      LExitCode := WFC_LEARN_EXIT_INTERNAL;
    end;
  else
    TryWriteStandardError(WfcLearnFormatFailure(wlfkInternal,
      JavaScriptExceptionMessage(JS.JSExceptValue)));
    LExitCode := WFC_LEARN_EXIT_INTERNAL;
  end;
  TNJSProcess.exitCode := LExitCode;
end.
