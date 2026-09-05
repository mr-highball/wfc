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
unit wfc_atomic_new_file;

{$mode delphi}{$H+}
{$IFDEF PAS2JS}{$FATAL wfc_atomic_new_file is a native filesystem tool}{$ENDIF}

interface

uses Classes, SysUtils;

const
  WFC_ATOMIC_NEW_FILE_VERSION = 1;
  WFC_ATOMIC_NEW_FILE_BLOCK_BYTES = 65536;

type
  EWfcAtomicNewFile = class(EWriteError);

  { Caller-owned sequential publication of one NEW native file. Construction
    validates an explicit path and creates only a unique exclusive sibling.
    Publish flushes and closes that sibling, then uses an OS no-replace move
    (Windows) or hard-link creation (Unix, including Darwin). A destination
    appearing during writing is never replaced; there is no overwrite fallback.

    WriteBytes consumes its borrowed array synchronously in bounded chunks.
    ByteCount counts successful writes; an OS failure may have written a prefix.
    I/O errors set Failed and preserve the original destination. Cancel and
    destruction close the handle and attempt removal of only the exact owned
    partial path, never a wildcard or destination. Cancel is terminal. Publish
    is idempotent once successful; destruction never publishes implicitly.

    A published file is complete and flushed, but this is not a crash-durable
    directory transaction: no directory fsync is promised. Unix publication
    requires sibling hard-link support. Use a trusted parent directory; this
    helper does not sandbox hostile concurrent directory/partial-name changes.
    Cleanup failure is retained in CleanupError and never hides the triggering
    exception. After a successful Unix link, failure to unlink the sibling is
    cleanup-only: Published remains True and destruction retries cleanup. }
  TWfcAtomicNewFile = class
  private
    FHandle: THandle;
    FOutputPath, FTemporaryPath, FCloseError, FTemporaryError: String;
    FOwnsTemporary, FPublished, FCancelled, FFailed: Boolean;
    FByteCount: Int64;
    procedure RequireWritable;
    procedure CloseFile(const ACheckErrors: Boolean);
    procedure CleanupTemporary;
    function GetCleanupError: String;
  public
    constructor Create(const AOutputPath: String);
    destructor Destroy; override;
    procedure WriteBytes(const ABytes: array of Byte);
    procedure Publish;
    procedure Cancel;
    property OutputPath: String read FOutputPath;
    property TemporaryPath: String read FTemporaryPath;
    property Published: Boolean read FPublished;
    property Cancelled: Boolean read FCancelled;
    property Failed: Boolean read FFailed;
    property ByteCount: Int64 read FByteCount;
    property CleanupError: String read GetCleanupError;
  end;

implementation

uses
  {$IFDEF MSWINDOWS}Windows{$ELSE}BaseUnix{$ENDIF};

function NativeError: Integer;
begin
  {$IFDEF MSWINDOWS}Result := GetLastError;{$ELSE}Result := fpGetErrno;{$ENDIF}
end;

procedure FileError(const AOperation: String; const ACode: Integer);
begin
  raise EWfcAtomicNewFile.Create(AOperation + ': ' + SysErrorMessage(ACode));
end;

function AnyPathExists(const APath: String): Boolean;
{$IFNDEF MSWINDOWS}var LStat: Stat;{$ENDIF}
begin
  {$IFDEF MSWINDOWS}
  Result := GetFileAttributes(PChar(APath)) <> INVALID_FILE_ATTRIBUTES;
  {$ELSE}
  { lstat includes dangling links, which are still existing destinations. }
  Result := fpLStat(PChar(APath), LStat) = 0;
  {$ENDIF}
end;

constructor TWfcAtomicNewFile.Create(const AOutputPath: String);
var I, LError: Integer; LCandidate, LPrefix: String;
begin
  inherited Create;
  FHandle := THandle(-1);
  if (AOutputPath = '') or (Pos(#0, AOutputPath) <> 0) then
    raise EWfcAtomicNewFile.Create('output must name a new file without NUL');
  FOutputPath := ExpandFileName(AOutputPath);
  if not DirectoryExists(ExtractFilePath(FOutputPath)) then
    raise EWfcAtomicNewFile.Create('output parent directory must already exist');
  if AnyPathExists(FOutputPath) then
    raise EWfcAtomicNewFile.Create('refusing to overwrite existing output');
  LPrefix := FOutputPath + '.partial-' + IntToHex(GetProcessID, 8) +
    '-' + IntToHex(GetTickCount64, 16) + '-';
  for I := 0 to 999 do
  begin
    LCandidate := LPrefix + IntToStr(I);
    {$IFDEF MSWINDOWS}
    FHandle := CreateFile(PChar(LCandidate), GENERIC_WRITE, 0, nil,
      CREATE_NEW, FILE_ATTRIBUTE_NORMAL, 0);
    {$ELSE}
    FHandle := fpOpen(PChar(LCandidate), O_WRONLY or O_CREAT or O_EXCL,
      S_IRUSR or S_IWUSR);
    {$ENDIF}
    if FHandle <> THandle(-1) then
    begin
      FTemporaryPath := LCandidate;
      FOwnsTemporary := True;
      Exit;
    end;
    LError := NativeError;
    {$IFDEF MSWINDOWS}
    if (LError <> ERROR_FILE_EXISTS) and (LError <> ERROR_ALREADY_EXISTS) then
    {$ELSE}
    if LError <> ESysEEXIST then
    {$ENDIF}
      FileError('cannot create exclusive output sibling', LError);
  end;
  raise EWfcAtomicNewFile.Create('exclusive temporary output names are exhausted');
end;

procedure TWfcAtomicNewFile.RequireWritable;
begin
  if FFailed then raise EWfcAtomicNewFile.Create('a previous file operation failed');
  if FCancelled then raise EWfcAtomicNewFile.Create('output has been cancelled');
  if FPublished then raise EWfcAtomicNewFile.Create('output has already been published');
  if FHandle = THandle(-1) then raise EWfcAtomicNewFile.Create('output handle is closed');
end;

procedure TWfcAtomicNewFile.WriteBytes(const ABytes: array of Byte);
var LOffset, LCount, LWritten, LError: Integer;
begin
  RequireWritable;
  if Length(ABytes) > High(Integer) then
    raise EWfcAtomicNewFile.Create('borrowed byte array exceeds Integer indexing');
  if Length(ABytes) > High(Int64) - FByteCount then
    raise EWfcAtomicNewFile.Create('file byte count exceeds Int64');
  LOffset := 0;
  try
    while LOffset < Length(ABytes) do
    begin
      LCount := Length(ABytes) - LOffset;
      if LCount > WFC_ATOMIC_NEW_FILE_BLOCK_BYTES then
        LCount := WFC_ATOMIC_NEW_FILE_BLOCK_BYTES;
      LWritten := FileWrite(FHandle, ABytes[LOffset], LCount);
      if LWritten <= 0 then
      begin
        LError := NativeError;
        {$IFNDEF MSWINDOWS}
        if (LWritten < 0) and (LError = ESysEINTR) then Continue;
        {$ENDIF}
        FileError('output write failed', LError);
      end;
      Inc(LOffset, LWritten);
      Inc(FByteCount, LWritten);
    end;
  except
    FFailed := True;
    raise;
  end;
end;

procedure TWfcAtomicNewFile.CloseFile(const ACheckErrors: Boolean);
var LHandle: THandle; LOk: Boolean; LError: Integer;
begin
  if FHandle = THandle(-1) then Exit;
  LHandle := FHandle;
  FHandle := THandle(-1);
  {$IFDEF MSWINDOWS}LOk := CloseHandle(LHandle);{$ELSE}LOk := fpClose(LHandle) = 0;{$ENDIF}
  if not LOk then
  begin
    LError := NativeError;
    if ACheckErrors then FileError('output close failed', LError)
    else FCloseError := 'output close failed: ' + SysErrorMessage(LError);
  end;
end;

procedure TWfcAtomicNewFile.CleanupTemporary;
var LOk: Boolean;
begin
  if not FOwnsTemporary then Exit;
  {$IFDEF MSWINDOWS}
  LOk := Windows.DeleteFile(PChar(FTemporaryPath));
  {$ELSE}
  LOk := fpUnlink(PChar(FTemporaryPath)) = 0;
  {$ENDIF}
  if LOk then
  begin
    FOwnsTemporary := False;
    FTemporaryError := '';
  end
  else FTemporaryError := 'cannot remove owned temporary output ' +
    FTemporaryPath + ': ' + SysErrorMessage(NativeError);
end;

function TWfcAtomicNewFile.GetCleanupError: String;
begin
  Result := FCloseError;
  if FTemporaryError <> '' then
  begin
    if Result <> '' then Result := Result + '; ';
    Result := Result + FTemporaryError;
  end;
end;

procedure TWfcAtomicNewFile.Publish;
begin
  if FPublished then Exit;
  RequireWritable;
  try
    if not FileFlush(FHandle) then FileError('output flush failed', NativeError);
    CloseFile(True);
    {$IFDEF MSWINDOWS}
    if not MoveFile(PChar(FTemporaryPath), PChar(FOutputPath)) then
      FileError('cannot publish without replacing an existing path', NativeError);
    FPublished := True;
    FOwnsTemporary := False;
    {$ELSE}
    if fpLink(PChar(FTemporaryPath), PChar(FOutputPath)) <> 0 then
      FileError('cannot publish without replacing an existing path', NativeError);
    FPublished := True;
    CleanupTemporary;
    {$ENDIF}
  except
    FFailed := True;
    raise;
  end;
end;

procedure TWfcAtomicNewFile.Cancel;
begin
  if FPublished then
  begin CleanupTemporary; Exit; end;
  FCancelled := True;
  CloseFile(False);
  CleanupTemporary;
end;

destructor TWfcAtomicNewFile.Destroy;
begin
  CloseFile(False);
  CleanupTemporary;
  inherited Destroy;
end;

end.
