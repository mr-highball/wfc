{ SPDX-License-Identifier: MIT
  Copyright (c) 2021 mr-highball
  Read-only FPC host over project-owned inventory and SHA-256 semantics. }
program wfc_asset_check;

{$mode delphi}{$H+}
{$IFDEF PAS2JS}{$FATAL asset filesystem checks require native FPC}{$ENDIF}

uses Classes, SysUtils, wfc_asset_manifest, wfc_asset_check_app, wfc_sha256,
  {$IFDEF MSWINDOWS}Windows{$ELSE}BaseUnix{$ENDIF};

type EWfcAssetHostIO = class(Exception);

const
  READ_CHUNK = 65536;
  WRITE_CHUNK = 4096;
  MAX_SHA256_BYTES: Int64 = High(Int64) div 4;

function PathKind(const Path: String): Integer;
{$IFDEF MSWINDOWS}
var Attributes: DWORD;
begin
  Attributes := GetFileAttributes(PChar(Path));
  if Attributes = INVALID_FILE_ATTRIBUTES then Exit(0);
  if (Attributes and (FILE_ATTRIBUTE_REPARSE_POINT or FILE_ATTRIBUTE_DEVICE)) <> 0 then Exit(-1);
  if (Attributes and FILE_ATTRIBUTE_DIRECTORY) <> 0 then Result := 2 else Result := 1;
end;
{$ELSE}
var Info: Stat;
begin
  if fpLStat(PChar(Path), Info) <> 0 then Exit(0);
  if fpS_ISLNK(Info.st_mode) then Exit(-1);
  if fpS_ISDIR(Info.st_mode) then Result := 2
  else if fpS_ISREG(Info.st_mode) then Result := 1 else Result := -1;
end;
{$ENDIF}

function AbsolutePath(const Path: String): String;
begin
  if (Path = '') or (Pos(#0, Path) <> 0) then
    raise EWfcAssetHostIO.Create('empty or invalid native input path');
  Result := ExpandFileName(Path);
end;

procedure RequireDirectory(const Path: String);
var I, First: Integer;
begin
  {$IFDEF MSWINDOWS}
  if (Length(Path) < 3) or not (Path[1] in ['A'..'Z','a'..'z']) or
    (Path[2] <> ':') or (Path[3] <> DirectorySeparator) then
    raise EWfcAssetHostIO.Create('root must use an ordinary local drive path');
  First := 4;
  if PathKind(Copy(Path, 1, 3)) <> 2 then
    raise EWfcAssetHostIO.Create('drive root is unavailable or not an ordinary directory');
  {$ELSE}
  if (Path = '') or (Path[1] <> DirectorySeparator) then
    raise EWfcAssetHostIO.Create('directory must resolve to an absolute path');
  First := 2;
  {$ENDIF}
  for I := First to Length(Path) do
    if Path[I] = DirectorySeparator then
      if PathKind(Copy(Path, 1, I - 1)) <> 2 then
        raise EWfcAssetHostIO.Create('directory is missing or contains a link, reparse point or non-directory');
  if PathKind(Path) <> 2 then
    raise EWfcAssetHostIO.Create('expected an ordinary directory without links or reparse points');
end;

procedure RequireFile(const Path: String);
begin
  RequireDirectory(ExtractFileDir(Path));
  if PathKind(Path) <> 1 then
    raise EWfcAssetHostIO.Create('required input is missing or is not an ordinary file');
end;

function RootedAssetPath(const Root, RelativePath: String): String;
var LocalPath: String; I: Integer;
begin
  ValidateWfcAssetPath(RelativePath);
  LocalPath := RelativePath;
  for I := 1 to Length(LocalPath) do
    if LocalPath[I] = '/' then LocalPath[I] := DirectorySeparator;
  Result := IncludeTrailingPathDelimiter(Root) + LocalPath;
  { Validated paths have no root, alternate stream, . or .. segments. Parent
    checks reject links; this is a trusted non-mutating tree, not a race-proof
    hostile filesystem sandbox. }
  RequireFile(Result);
end;

procedure WriteExact(const Handle: THandle; const Text: String);
var Used, Request, N: Integer;
begin
  Used := 0;
  while Used < Length(Text) do
  begin
    Request := Length(Text) - Used;
    if Request > WRITE_CHUNK then Request := WRITE_CHUNK;
    N := FileWrite(Handle, Text[Used + 1], Request);
    if N < 0 then
    begin
      {$IFDEF UNIX}if GetLastOSError = ESysEINTR then Continue;{$ENDIF}
      raise EWfcAssetHostIO.Create('process output write failed');
    end;
    if N = 0 then raise EWfcAssetHostIO.Create('process output made no progress');
    Inc(Used, N);
  end;
end;

procedure OutputLine(const Text: String);
begin WriteExact(StdOutputHandle, Text + #10); end;

function ReadTextFile(const Path: String; const MaxBytes: Integer): String;
var Stream: TFileStream; Buffer: array[0..READ_CHUNK - 1] of Byte;
  Used, Capacity, Request, N, Required, NewCapacity: Integer;
begin
  RequireFile(Path);
  Result := ''; Used := 0; Capacity := 0;
  Stream := TFileStream.Create(Path, fmOpenRead or fmShareDenyWrite);
  try
    if Stream.Size > MaxBytes then
      raise EWfcAssetManifest.Create('input text exceeds its byte allowance');
    repeat
      Request := MaxBytes - Used;
      if Request >= SizeOf(Buffer) then Request := SizeOf(Buffer) else Inc(Request);
      N := Stream.Read(Buffer[0], Request);
      if N < 0 then raise EWfcAssetHostIO.Create('input text read failed');
      if N = 0 then Break;
      if N > MaxBytes - Used then
        raise EWfcAssetManifest.Create('input text exceeds its byte allowance');
      Required := Used + N;
      if Required > Capacity then
      begin
        NewCapacity := Capacity;
        if NewCapacity < READ_CHUNK then NewCapacity := READ_CHUNK;
        if NewCapacity > MaxBytes then NewCapacity := MaxBytes;
        while NewCapacity < Required do
          if NewCapacity > MaxBytes div 2 then NewCapacity := MaxBytes
          else NewCapacity := NewCapacity * 2;
        SetLength(Result, NewCapacity); Capacity := NewCapacity;
      end;
      Move(Buffer[0], Result[Used + 1], N); Used := Required;
    until False;
    SetLength(Result, Used);
  finally Stream.Free; end;
end;

function CheckAssetBytes(const Path: String; const Entry: TWfcAssetEntry): Boolean;
var Stream: TFileStream; Context: TWfcSha256Context; Buffer: TWfcSha256Bytes;
  ActualBytes, InitialSize: Int64; N: Integer; Digest: String;
begin
  Result := False; ActualBytes := 0;
  RequireFile(Path);
  Stream := TFileStream.Create(Path, fmOpenRead or fmShareDenyWrite);
  try
    InitialSize := Stream.Size;
    if (InitialSize < 0) or (InitialSize > MAX_SHA256_BYTES) then
      raise EWfcAssetHostIO.Create('asset size exceeds the SHA-256 message length representation');
    SetLength(Buffer, READ_CHUNK);
    Context := TWfcSha256Context.Create;
    try
      repeat
        N := Stream.Read(Buffer[0], Length(Buffer));
        if N < 0 then raise EWfcAssetHostIO.Create('asset read failed');
        if N = 0 then Break;
        if ActualBytes > MAX_SHA256_BYTES - N then
          raise EWfcAssetHostIO.Create('asset exceeds the SHA-256 message length representation');
        Context.Update(Buffer, 0, N); Inc(ActualBytes, N);
      until False;
      if (ActualBytes <> InitialSize) or (Stream.Size <> InitialSize) then
        raise EWfcAssetHostIO.Create('asset size changed during the read');
      Digest := WfcSha256DigestHex(Context.Finish);
      Result := (IntToStr(ActualBytes) = Entry.ByteSizeText) and (Digest = Entry.SHA256);
      if IntToStr(ActualBytes) <> Entry.ByteSizeText then
        OutputLine('size-mismatch=' + Entry.Path);
      if Digest <> Entry.SHA256 then OutputLine('sha256-mismatch=' + Entry.Path);
    finally Context.Free; end;
  finally Stream.Free; end;
end;

procedure VerifyEvidence(const Path: String);
var Stream: TFileStream;
begin
  RequireFile(Path);
  Stream := TFileStream.Create(Path, fmOpenRead or fmShareDenyWrite);
  try
    { Opening proves readable ordinary evidence, not the truth of its contents. }
  finally Stream.Free; end;
end;

function Execute(const Command: TWfcAssetCommand): Integer;
var Manifest: TWfcAssetManifest; Files: TWfcAssetFileList;
  Report: TWfcAssetInventoryReport; Entry: TWfcAssetEntry;
  ManifestText, FileListText, Root, Path: String; I, J, Matches, Mismatches: Integer;
begin
  Result := 0;
  case Command.Kind of
    wackHelp: begin WriteExact(StdOutputHandle, WfcAssetCommandHelp); Exit; end;
    wackVersion: begin OutputLine('wfc_asset_check 1'); Exit; end;
    wackInventory:
      begin
        FileListText := ReadTextFile(AbsolutePath(Command.RawInventoryPath),
          Command.FileListLimits.MaxEncodedBytes);
        WriteExact(StdOutputHandle, ConvertWfcAssetPathLines(FileListText, Command.FileListLimits));
        Exit;
      end;
  end;
  Manifest := nil; Files := nil;
  try
    ManifestText := ReadTextFile(AbsolutePath(Command.ManifestPath), Command.ManifestLimits.MaxEncodedBytes);
    FileListText := ReadTextFile(AbsolutePath(Command.FileListPath), Command.FileListLimits.MaxEncodedBytes);
    Manifest := DecodeWfcAssetManifest(ManifestText, Command.ManifestLimits);
    Files := DecodeWfcAssetFileList(FileListText, Command.FileListLimits);
    Report := CheckWfcAssetInventory(Manifest, Files);
    OutputLine('scope=supplied-file-inventory; classification=suffix-profile-1');
    OutputLine('inventory-files=' + IntToStr(Files.Count));
    OutputLine('assets=' + IntToStr(Manifest.Count));
    OutputLine('documented=' + IntToStr(Report.DocumentedCount));
    OutputLine('unresolved=' + IntToStr(Report.UnresolvedCount));
    OutputLine('provenance=declarations-not-independent-legal-verification');
    for I := 0 to High(Report.MissingRequiredEntries) do
      OutputLine('missing-asset-record=' + Report.MissingRequiredEntries[I]);
    for I := 0 to High(Report.OrphanManifestRows) do
      OutputLine('orphan-asset-record=' + Report.OrphanManifestRows[I]);
    for I := 0 to High(Report.MissingEvidencePaths) do
      OutputLine('missing-evidence=' + Report.MissingEvidencePaths[I].AssetPath +
        ':' + Report.MissingEvidencePaths[I].EvidencePath);
    if not Report.MatchesSuppliedInventory then Exit(1);
    Root := AbsolutePath(Command.RootPath);
    while (Length(Root) > 3) and (Root[Length(Root)] = DirectorySeparator) do
      Delete(Root, Length(Root), 1);
    RequireDirectory(Root);
    Matches := 0; Mismatches := 0;
    for I := 0 to Manifest.Count - 1 do
    begin
      Entry := Manifest.EntryAt(I);
      Path := RootedAssetPath(Root, Entry.Path);
      if CheckAssetBytes(Path, Entry) then Inc(Matches) else Inc(Mismatches);
      for J := 0 to High(Entry.EvidencePaths) do
        VerifyEvidence(RootedAssetPath(Root, Entry.EvidencePaths[J]));
    end;
    OutputLine('matched-assets=' + IntToStr(Matches));
    OutputLine('mismatched-assets=' + IntToStr(Mismatches));
    if Mismatches <> 0 then Exit(1);
    if Command.RequireReviewed and (Manifest.UnresolvedCount <> 0) then
    begin OutputLine('required-review=incomplete'); Exit(1); end;
    if Command.RequireReviewed then OutputLine('required-review=documented-declarations');
    OutputLine('byte-inventory-check=passed');
  finally Files.Free; Manifest.Free; end;
end;

var Arguments: TWfcAssetArguments; Command: TWfcAssetCommand; I, ExitStatus: Integer;
begin
  ExitStatus := 4;
  try
    SetLength(Arguments, ParamCount);
    for I := 1 to ParamCount do Arguments[I - 1] := ParamStr(I);
    Command := ParseWfcAssetCommand(Arguments);
    ExitStatus := Execute(Command);
  except
    on E: Exception do
    begin
      if E is EWfcAssetUsage then ExitStatus := 2
      else if E is EWfcAssetManifest then ExitStatus := 3
      else ExitStatus := 4;
      try WriteExact(StdErrorHandle, 'wfc_asset_check: ' + WfcAssetOneLine(E.Message) + #10);
      except { No alternate output destination is authorized. } end;
    end;
  end;
  Halt(ExitStatus);
end.
