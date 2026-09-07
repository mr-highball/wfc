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
unit wfc_artifact_cli_io;

{$mode delphi}{$H+}
{$IFDEF PAS2JS}{$FATAL artifact CLI I/O requires native FPC}{$ENDIF}

interface

uses SysUtils, wfc_validate_app;

type EWfcArtifactInputLimit = class(Exception);

function WfcArtifactNativeArguments: TWfcValidateArguments;
procedure WfcArtifactReadInputs(const AKind: TWfcValidateCommandKind;
  const AInputPath, ARecipePath, ARunPath: String;
  out AInput, ARecipe, ARun, AReadingRole: String);
function WfcArtifactEmitOutcome(const AProgramName: String; const AStatus: Integer;
  const AStandardOutput, AStandardError: String): Integer;
procedure WfcArtifactTryWriteStandardError(const AText: String);

implementation

uses Classes, wfc_artifact_document{$IFDEF UNIX}, BaseUnix{$ENDIF};

const IO_CHUNK_SIZE = 65536; OUTPUT_CHUNK_SIZE = 4096;

function WfcArtifactNativeArguments: TWfcValidateArguments;
var I: Integer;
begin
  Result := nil;
  SetLength(Result, ParamCount);
  for I := 1 to ParamCount do Result[I - 1] := ParamStr(I);
end;

procedure InputLimit;
begin
  { Preserve the original recipe host's diagnostic; each role nevertheless
    uses its own format limit rather than the largest recipe allowance. }
  raise EWfcArtifactInputLimit.Create('document exceeds the version-1 encoded length limit');
end;

procedure EnsureCapacity(var AText: String; const ARequired, ALimit: Integer);
var N: Integer;
begin
  if ARequired <= Length(AText) then Exit;
  if ARequired > ALimit then InputLimit;
  N := Length(AText);
  if N < IO_CHUNK_SIZE then N := IO_CHUNK_SIZE;
  if N > ALimit then N := ALimit;
  while N < ARequired do
    if N > ALimit div 2 then N := ALimit else N := N * 2;
  SetLength(AText, N);
end;

function ReadBoundedHandle(const AHandle: THandle; const ALimit: Integer): String;
var Buffer: array[0..IO_CHUNK_SIZE - 1] of Byte;
  N, Request, Used, ErrorCode: Integer;
begin
  Result := ''; Used := 0;
  repeat
    Request := ALimit - Used;
    if Request >= SizeOf(Buffer) then Request := SizeOf(Buffer) else Inc(Request);
    N := FileRead(AHandle, Buffer[0], Request);
    if N < 0 then
    begin
      ErrorCode := GetLastOSError;
      {$IFDEF UNIX}if ErrorCode = ESysEINTR then Continue;{$ENDIF}
      {$IFDEF MSWINDOWS}if ErrorCode = 109 then Break;{$ENDIF}
      raise EReadError.Create('input handle read failed: ' + SysErrorMessage(ErrorCode));
    end;
    if N = 0 then Break;
    if Used > ALimit - N then InputLimit;
    EnsureCapacity(Result, Used + N, ALimit);
    Move(Buffer[0], Result[Used + 1], N);
    Inc(Used, N);
  until False;
  SetLength(Result, Used);
end;

function ReadInput(const APath: String; const ALimit: Integer): String;
var F: TFileStream;
begin
  if APath = '-' then Exit(ReadBoundedHandle(StdInputHandle, ALimit));
  F := TFileStream.Create(APath, fmOpenRead or fmShareDenyNone);
  try
    if F.Size > ALimit then InputLimit;
    Result := ReadBoundedHandle(F.Handle, ALimit);
  finally F.Free; end;
end;

procedure WfcArtifactReadInputs(const AKind: TWfcValidateCommandKind;
  const AInputPath, ARecipePath, ARunPath: String;
  out AInput, ARecipe, ARun, AReadingRole: String);
begin
  AInput := ''; ARecipe := ''; ARun := ''; AReadingRole := '';
  if AKind in [wvckHelp, wvckVersion] then Exit;
  if AKind in [wvckRun, wvckResult] then
  begin
    AReadingRole := 'RECIPE';
    ARecipe := ReadInput(ARecipePath, WfcArtifactInputLimit(wakRecipe));
  end;
  if AKind = wvckResult then
  begin
    AReadingRole := 'RUN';
    ARun := ReadInput(ARunPath, WfcArtifactInputLimit(wakRun));
  end;
  AReadingRole := 'INPUT';
  if AKind = wvckRun then AReadingRole := 'RUN'
  else if AKind = wvckResult then AReadingRole := 'RESULT';
  AInput := ReadInput(AInputPath,
    WfcArtifactInputLimit(WfcValidateCommandArtifactKind(AKind)));
end;

procedure WriteHandleExact(const AHandle: THandle; const AText: String);
var N, Request, Used, ErrorCode: Integer;
begin
  Used := 0;
  while Used < Length(AText) do
  begin
    Request := Length(AText) - Used;
    if Request > OUTPUT_CHUNK_SIZE then Request := OUTPUT_CHUNK_SIZE;
    N := FileWrite(AHandle, AText[Used + 1], Request);
    if N < 0 then
    begin
      ErrorCode := GetLastOSError;
      {$IFDEF UNIX}if ErrorCode = ESysEINTR then Continue;{$ENDIF}
      raise EWriteError.Create('output handle write failed: ' + SysErrorMessage(ErrorCode));
    end;
    if N = 0 then raise EWriteError.Create('output handle write made no progress');
    Inc(Used, N);
  end;
end;

procedure WfcArtifactTryWriteStandardError(const AText: String);
begin
  try WriteHandleExact(StdErrorHandle, AText);
  except { No other destination exists for a standard-error write failure. } end;
end;

function WfcArtifactEmitOutcome(const AProgramName: String; const AStatus: Integer;
  const AStandardOutput, AStandardError: String): Integer;
begin
  try
    WriteHandleExact(StdOutputHandle, AStandardOutput);
    WriteHandleExact(StdErrorHandle, AStandardError);
    Result := AStatus;
  except
    on E: Exception do
    begin
      WfcArtifactTryWriteStandardError(AProgramName + ': I/O error: ' +
        WfcValidateOneLineMessage('cannot write process output: ' + E.Message) + #10);
      Result := WFC_VALIDATE_EXIT_IO;
    end;
  end;
end;

end.
