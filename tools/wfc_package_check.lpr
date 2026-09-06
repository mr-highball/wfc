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
program wfc_package_check;

{$mode delphi}{$H+}
{$IFDEF PAS2JS}{$FATAL package filesystem checks require native FPC}{$ENDIF}

uses Classes, SysUtils, wfc_package_check_app,
  {$IFDEF MSWINDOWS}Windows{$ELSE}BaseUnix{$ENDIF};

const
  WFC_PACKAGE_CHECK_MAX_TOTAL_SOURCE_BYTES = 33554432;
  WFC_PACKAGE_CHECK_MAX_DIRECTORY_ENTRIES = 65536;
  WFC_PACKAGE_CHECK_EXIT_INVALID = 1;
  WFC_PACKAGE_CHECK_EXIT_USAGE = 2;
  WFC_PACKAGE_CHECK_EXIT_IO = 3;

type
  EWfcPackageHostInvalid = class(Exception);
  EWfcPackageHostUsage = class(Exception);

function PathKind(const APath: String): Integer;
{$IFDEF MSWINDOWS}
var LAttributes: DWORD;
begin
  LAttributes := GetFileAttributes(PChar(APath));
  if LAttributes = INVALID_FILE_ATTRIBUTES then Exit(0);
  if (LAttributes and FILE_ATTRIBUTE_REPARSE_POINT) <> 0 then Exit(-1);
  if (LAttributes and FILE_ATTRIBUTE_DEVICE) <> 0 then Exit(-1);
  if (LAttributes and FILE_ATTRIBUTE_DIRECTORY) <> 0 then Result := 2
  else Result := 1;
end;
{$ELSE}
var LStat: Stat;
begin
  if fpLStat(PChar(APath), LStat) <> 0 then Exit(0);
  if fpS_ISLNK(LStat.st_mode) then Exit(-1);
  if fpS_ISDIR(LStat.st_mode) then Result := 2
  else if fpS_ISREG(LStat.st_mode) then Result := 1
  else Result := -1;
end;
{$ENDIF}

procedure RequireOrdinaryDirectory(const APath: String);
var I, LFirst, LKind: Integer; LPart: String;
begin
  {$IFDEF MSWINDOWS}
  if (Length(APath) < 3) or (APath[2] <> ':') or
    (APath[3] <> DirectorySeparator) then
    raise EWfcPackageHostInvalid.Create('root must use an ordinary local drive path');
  LFirst := 4;
  {$ELSE}
  if (APath = '') or (APath[1] <> DirectorySeparator) then
    raise EWfcPackageHostInvalid.Create('root must resolve to an absolute directory');
  LFirst := 2;
  {$ENDIF}
  for I := LFirst to Length(APath) do
    if APath[I] = DirectorySeparator then
    begin
      LPart := Copy(APath, 1, I - 1); LKind := PathKind(LPart);
      if LKind = 0 then raise EInOutError.Create('required directory is unavailable');
      if LKind <> 2 then
        raise EWfcPackageHostInvalid.Create('directory contains a link, reparse point or non-directory');
    end;
  LKind := PathKind(APath);
  if LKind = 0 then raise EInOutError.Create('required directory is unavailable');
  if LKind <> 2 then
    raise EWfcPackageHostInvalid.Create('expected an ordinary directory without links or reparse points');
end;

function ReadBoundedFile(const APath: String): String;
var LStream: TFileStream; LBuffer: array[0..65535] of Byte;
  LRead, LUsed, LRequest, LKind: Integer;
begin
  Result := ''; LKind := PathKind(APath);
  if LKind = 0 then raise EInOutError.Create('required file is unavailable: ' + ExtractFileName(APath));
  if LKind <> 1 then
    raise EWfcPackageHostInvalid.Create('expected an ordinary file: ' + ExtractFileName(APath));
  LStream := TFileStream.Create(APath, fmOpenRead or fmShareDenyNone);
  try
    if LStream.Size > WFC_PACKAGE_CHECK_MAX_TEXT_LENGTH then
      raise EWfcPackageHostInvalid.Create('file exceeds the text limit: ' + ExtractFileName(APath));
    LUsed := 0;
    repeat
      LRequest := WFC_PACKAGE_CHECK_MAX_TEXT_LENGTH - LUsed;
      if LRequest >= SizeOf(LBuffer) then LRequest := SizeOf(LBuffer)
      else Inc(LRequest);
      LRead := LStream.Read(LBuffer[0], LRequest);
      if LRead < 0 then raise EReadError.Create('file read failed: ' + ExtractFileName(APath));
      if LRead = 0 then Break;
      if LRead > WFC_PACKAGE_CHECK_MAX_TEXT_LENGTH - LUsed then
        raise EWfcPackageHostInvalid.Create('file exceeds the text limit: ' + ExtractFileName(APath));
      SetLength(Result, LUsed + LRead);
      Move(LBuffer[0], Result[LUsed + 1], LRead); Inc(LUsed, LRead);
    until False;
  finally LStream.Free; end;
end;

function CanonicalUnitFile(const AName: String): Boolean;
var I, N: Integer; LBase: String;
begin
  Result := False; N := Length(AName) - 4;
  if (N < 1) or (Copy(AName, N + 1, 4) <> '.pas') then Exit;
  if not (AName[1] in ['a'..'z', '_']) then Exit;
  for I := 2 to N do
    if not (AName[I] in ['a'..'z', '0'..'9', '_']) then Exit;
  { A Pascal extension does not stop Windows from interpreting these basenames
    as devices. Reject them before any source handle is opened, on every OS. }
  LBase := Copy(AName, 1, N);
  if (LBase = 'con') or (LBase = 'prn') or (LBase = 'aux') or (LBase = 'nul') then Exit;
  if (N = 4) and (LBase[4] in ['0'..'9']) and
    ((Copy(LBase, 1, 3) = 'com') or (Copy(LBase, 1, 3) = 'lpt')) then Exit;
  Result := True;
end;

function SourceUnits(const ARoot: String): TWfcPackageSourceUnits;
var LDirectory, LText, LName: String; LSearch: TSearchRec;
  LNames: TStringList; I, LCode, LEntries, LTotal, LScanError: Integer;
begin
  Result := nil;
  LDirectory := IncludeTrailingPathDelimiter(ARoot) + 'src';
  RequireOrdinaryDirectory(LDirectory);
  LNames := TStringList.Create;
  try
    LNames.CaseSensitive := True; LNames.Sorted := True;
    LNames.Duplicates := dupError; LEntries := 0;
    LScanError := 0;
    {$IFNDEF MSWINDOWS}fpSetErrno(0);{$ENDIF}
    LCode := FindFirst(IncludeTrailingPathDelimiter(LDirectory) + '*', faAnyFile, LSearch);
    {$IFNDEF MSWINDOWS}if LCode <> 0 then LScanError := fpGetErrno;{$ENDIF}
    if LCode = 0 then
    try
      repeat
        Inc(LEntries);
        if LEntries > WFC_PACKAGE_CHECK_MAX_DIRECTORY_ENTRIES then
          raise EWfcPackageHostInvalid.Create('source directory entry limit exceeded');
        if LowerCase(ExtractFileExt(LSearch.Name)) = '.pas' then
        begin
          if not CanonicalUnitFile(LSearch.Name) then
            raise EWfcPackageHostInvalid.Create('source filename is not a canonical lowercase unit: ' + LSearch.Name);
          if PathKind(IncludeTrailingPathDelimiter(LDirectory) + LSearch.Name) <> 1 then
            raise EWfcPackageHostInvalid.Create('source unit is not an ordinary file: ' + LSearch.Name);
          if LNames.Count >= WFC_PACKAGE_CHECK_MAX_UNITS then
            raise EWfcPackageHostInvalid.Create('source unit limit exceeded');
          LNames.Add(LSearch.Name);
        end;
        {$IFNDEF MSWINDOWS}fpSetErrno(0);{$ENDIF}
        LCode := FindNext(LSearch);
        {$IFNDEF MSWINDOWS}if LCode <> 0 then LScanError := fpGetErrno;{$ENDIF}
      until LCode <> 0;
    finally SysUtils.FindClose(LSearch); end;
    {$IFDEF MSWINDOWS}
    if not (LCode in [ERROR_FILE_NOT_FOUND, ERROR_NO_MORE_FILES]) then
      LScanError := LCode;
    {$ENDIF}
    if LScanError <> 0 then raise EInOutError.Create('source directory enumeration failed');
    if LNames.Count = 0 then
      raise EWfcPackageHostInvalid.Create('source inventory has no Pascal units');
    SetLength(Result, LNames.Count); LTotal := 0;
    for I := 0 to LNames.Count - 1 do
    begin
      LText := ReadBoundedFile(IncludeTrailingPathDelimiter(LDirectory) + LNames[I]);
      if Length(LText) > WFC_PACKAGE_CHECK_MAX_TOTAL_SOURCE_BYTES - LTotal then
        raise EWfcPackageHostInvalid.Create('aggregate source text limit exceeded');
      Inc(LTotal, Length(LText));
      LName := WfcPackageDeclaredUnitName(LText);
      Result[I] := Copy(LNames[I], 1, Length(LNames[I]) - 4);
      if LName <> Result[I] then
        raise EWfcPackageHostInvalid.Create('unit declaration does not match source filename: ' + LNames[I]);
    end;
  finally LNames.Free; end;
end;

procedure WriteExact(const AHandle: THandle; const AText: String);
var LOffset, LWritten: Integer;
begin
  LOffset := 0;
  while LOffset < Length(AText) do
  begin
    LWritten := FileWrite(AHandle, AText[LOffset + 1], Length(AText) - LOffset);
    if LWritten <= 0 then raise EWriteError.Create('output handle write failed');
    Inc(LOffset, LWritten);
  end;
end;

procedure Diagnostic(const AText: String);
var I: Integer; LText: String;
begin
  LText := Copy(AText, 1, WFC_PACKAGE_CHECK_MAX_DIAGNOSTIC_LENGTH);
  for I := 1 to Length(LText) do
    if (Ord(LText[I]) < 32) or (Ord(LText[I]) > 126) then LText[I] := '?';
  try WriteExact(StdErrorHandle, 'wfc_package_check: ' + LText + LineEnding);
  except on E: Exception do begin end; end;
end;

function Execute: Integer;
var LRoot: String; LUnits: TWfcPackageSourceUnits; LResult: TWfcPackageCheckResult;
  LFpmake, LLazarus, LPackage: String;
begin
  if (ParamCount = 1) and (ParamStr(1) = '--help') then
  begin
    WriteExact(StdOutputHandle, 'Usage: wfc_package_check --root DIRECTORY' + LineEnding +
      'Read-only src/*.pas inventory against fpmake.pp, wfc.lpk and wfc_package.pas.' + LineEnding +
      'Exit status: 0 valid, 1 invalid repository, 2 usage, 3 I/O.' + LineEnding);
    Exit(0);
  end;
  if (ParamCount = 1) and (ParamStr(1) = '--version') then
  begin
    WriteExact(StdOutputHandle, 'wfc_package_check ' + IntToStr(WFC_PACKAGE_CHECK_VERSION) + LineEnding);
    Exit(0);
  end;
  if (ParamCount <> 2) or (ParamStr(1) <> '--root') or
    (ParamStr(2) = '') or (Pos('--', ParamStr(2)) = 1) or
    (Pos(#0, ParamStr(2)) <> 0) then
    raise EWfcPackageHostUsage.Create('expected --root DIRECTORY exactly once, or --help/--version alone');
  LRoot := ExcludeTrailingPathDelimiter(ExpandFileName(ParamStr(2)));
  RequireOrdinaryDirectory(LRoot);
  LUnits := SourceUnits(LRoot);
  LFpmake := ReadBoundedFile(IncludeTrailingPathDelimiter(LRoot) + 'fpmake.pp');
  LLazarus := ReadBoundedFile(IncludeTrailingPathDelimiter(LRoot) + 'wfc.lpk');
  LPackage := ReadBoundedFile(IncludeTrailingPathDelimiter(LRoot) + 'wfc_package.pas');
  LResult := CheckWfcPackageManifests(LUnits, LFpmake, LLazarus, LPackage);
  if not LResult.Passed then
  begin Diagnostic(LResult.Diagnostic); Exit(WFC_PACKAGE_CHECK_EXIT_INVALID); end;
  WriteExact(StdOutputHandle, 'Package manifests complete: ' +
    IntToStr(LResult.SourceUnitCount) + ' source units; fpmake=' +
    IntToStr(LResult.FpmUnitCount) + ', lazarus=' + IntToStr(LResult.LazarusUnitCount) +
    ', package=' + IntToStr(LResult.PackageUnitCount) + '.' + LineEnding);
  Result := 0;
end;

var ExitCodeValue: Integer;
begin
  try ExitCodeValue := Execute;
  except
    on E: EWfcPackageHostUsage do begin Diagnostic(E.Message); ExitCodeValue := WFC_PACKAGE_CHECK_EXIT_USAGE; end;
    on E: EWfcPackageHostInvalid do begin Diagnostic(E.Message); ExitCodeValue := WFC_PACKAGE_CHECK_EXIT_INVALID; end;
    on E: EWfcPackageCheck do begin Diagnostic(E.Message); ExitCodeValue := WFC_PACKAGE_CHECK_EXIT_INVALID; end;
    on E: Exception do begin Diagnostic(E.Message); ExitCodeValue := WFC_PACKAGE_CHECK_EXIT_IO; end;
  end;
  Halt(ExitCodeValue);
end.
