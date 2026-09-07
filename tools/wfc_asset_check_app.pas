{ SPDX-License-Identifier: MIT
  Copyright (c) 2021 mr-highball
  Shared command and raw inventory conversion semantics; no filesystem I/O. }
unit wfc_asset_check_app;

{$mode delphi}{$H+}

interface

uses SysUtils, wfc_asset_manifest;

type
  EWfcAssetUsage = class(Exception);
  TWfcAssetArguments = array of String;
  TWfcAssetCommandKind = (wackHelp, wackVersion, wackInventory, wackCheck);
  TWfcAssetCommand = record
    Kind: TWfcAssetCommandKind;
    RootPath, ManifestPath, FileListPath, RawInventoryPath: String;
    RequireReviewed: Boolean;
    ManifestLimits: TWfcAssetManifestLimits;
    FileListLimits: TWfcAssetFileListLimits;
  end;

function ParseWfcAssetCommand(const Arguments: TWfcAssetArguments): TWfcAssetCommand;
function ConvertWfcAssetPathLines(const Text: String;
  const Limits: TWfcAssetFileListLimits): String;
function WfcAssetOneLine(const Text: String): String;
function WfcAssetCommandHelp: String;

implementation

type
  TOption = (opRoot, opManifest, opFiles, opInventory, opReviewed,
    opManifestBytes, opListBytes, opAssets, opFilesLimit, opEvidence);
  TOptions = set of TOption;

procedure Usage;
begin
  raise EWfcAssetUsage.Create('invalid arguments; use --help for the two explicit modes');
end;

function NonnegativeOption(const Text: String): Integer;
var I, Digit: Integer;
begin
  if (Text = '') or ((Length(Text) > 1) and (Text[1] = '0')) then Usage;
  Result := 0;
  for I := 1 to Length(Text) do
  begin
    if not (Text[I] in ['0'..'9']) then Usage;
    Digit := Ord(Text[I]) - Ord('0');
    if Result > (High(Integer) - Digit) div 10 then Usage;
    Result := Result * 10 + Digit;
  end;
end;

function ParseWfcAssetCommand(const Arguments: TWfcAssetArguments): TWfcAssetCommand;
var I: Integer; Seen: TOptions; Option: TOption; Value: String;
  {$IFDEF PAS2JS}Valid: Boolean;{$ENDIF}
begin
  {$IFDEF PAS2JS}
  { Inspect passive own slots before Pascal reads them. This does not claim a
    sandbox against Proxy traps or replacement of global JS intrinsics. }
  asm
    Valid = Array.isArray(Arguments) && Arguments.length <= 2147483647;
    if (Valid) {
      for (var n = 0; n < Arguments.length; n++) {
        var d = Object.getOwnPropertyDescriptor(Arguments, String(n));
        if (!d || !Object.prototype.hasOwnProperty.call(d, 'value') ||
            typeof d.value !== 'string' || d.value.length > 2147483647) {
          Valid = false;
          break;
        }
      }
    }
  end;
  if not Valid then Usage;
  {$ENDIF}
  if Length(Arguments) > High(Integer) then Usage;
  for I := 0 to Length(Arguments) - 1 do
    if Length(Arguments[I]) > High(Integer) then Usage;
  Result := Default(TWfcAssetCommand);
  Result.ManifestLimits.Version := 1;
  Result.ManifestLimits.MaxAssets := 100000;
  Result.ManifestLimits.MaxEvidencePaths := 1000000;
  Result.ManifestLimits.MaxEncodedBytes := 1048576;
  Result.FileListLimits.Version := 1;
  Result.FileListLimits.MaxFiles := 1000000;
  Result.FileListLimits.MaxEncodedBytes := 33554432;
  if Length(Arguments) = 0 then Usage;
  if (Length(Arguments) = 1) and (Arguments[0] = '--help') then
  begin Result.Kind := wackHelp; Exit; end;
  if (Length(Arguments) = 1) and (Arguments[0] = '--version') then
  begin Result.Kind := wackVersion; Exit; end;
  Seen := []; I := 0;
  while I < Length(Arguments) do
  begin
    if Arguments[I] = '--root' then Option := opRoot
    else if Arguments[I] = '--manifest' then Option := opManifest
    else if Arguments[I] = '--files' then Option := opFiles
    else if Arguments[I] = '--inventory-from' then Option := opInventory
    else if Arguments[I] = '--require-reviewed' then Option := opReviewed
    else if Arguments[I] = '--max-manifest-bytes' then Option := opManifestBytes
    else if Arguments[I] = '--max-file-list-bytes' then Option := opListBytes
    else if Arguments[I] = '--max-assets' then Option := opAssets
    else if Arguments[I] = '--max-files' then Option := opFilesLimit
    else if Arguments[I] = '--max-evidence-paths' then Option := opEvidence
    else begin Usage; Exit; end;
    if Option in Seen then Usage;
    Include(Seen, Option); Inc(I);
    if Option = opReviewed then
    begin Result.RequireReviewed := True; Continue; end;
    if I >= Length(Arguments) then Usage;
    Value := Arguments[I]; Inc(I);
    if Value = '' then Usage;
    if Copy(Value, 1, 2) = '--' then Usage;
    case Option of
      opRoot: Result.RootPath := Value;
      opManifest: Result.ManifestPath := Value;
      opFiles: Result.FileListPath := Value;
      opInventory: Result.RawInventoryPath := Value;
      opManifestBytes: Result.ManifestLimits.MaxEncodedBytes := NonnegativeOption(Value);
      opListBytes: Result.FileListLimits.MaxEncodedBytes := NonnegativeOption(Value);
      opAssets: Result.ManifestLimits.MaxAssets := NonnegativeOption(Value);
      opFilesLimit: Result.FileListLimits.MaxFiles := NonnegativeOption(Value);
      opEvidence: Result.ManifestLimits.MaxEvidencePaths := NonnegativeOption(Value);
    end;
  end;
  if opInventory in Seen then
  begin
    if (Seen - [opInventory, opListBytes, opFilesLimit]) <> [] then Usage;
    Result.Kind := wackInventory;
  end
  else
  begin
    if not ([opRoot, opManifest, opFiles] <= Seen) then Usage;
    Result.Kind := wackCheck;
  end;
end;

procedure SortPaths(var Paths: TWfcAssetPaths);
var Work: TWfcAssetPaths; Width, Left, Midpoint, Finish, I, J, K, N: Integer;
begin
  N := Length(Paths);
  if N < 2 then Exit;
  SetLength(Work, N); Width := 1;
  while Width < N do
  begin
    Left := 0;
    while Left < N do
    begin
      if Width > N - Left then Midpoint := N else Midpoint := Left + Width;
      if Width > N - Midpoint then Finish := N else Finish := Midpoint + Width;
      I := Left; J := Midpoint;
      for K := Left to Finish - 1 do
      begin
        if (I < Midpoint) and ((J >= Finish) or (Paths[I] <= Paths[J])) then
        begin Work[K] := Paths[I]; Inc(I); end
        else begin Work[K] := Paths[J]; Inc(J); end;
      end;
      Left := Finish;
    end;
    for I := 0 to N - 1 do Paths[I] := Work[I];
    if Width > N div 2 then Width := N else Width := Width * 2;
  end;
end;

function ConvertWfcAssetPathLines(const Text: String;
  const Limits: TWfcAssetFileListLimits): String;
var Paths: TWfcAssetPaths; List, EmptyList: TWfcAssetFileList;
  I, First, Count, Finish: Integer; Path: String; HasTail: Boolean;
  {$IFDEF PAS2JS}Valid: Boolean;{$ENDIF}
begin
  {$IFDEF PAS2JS}
  asm
    Valid = typeof Text === 'string' && Text.length <= 2147483647;
  end;
  if not Valid then raise EWfcAssetManifest.Create('invalid raw inventory text');
  {$ENDIF}
  { The owner validates the complete caller policy even for an empty input. }
  EmptyList := TWfcAssetFileList.Create(nil, Limits);
  EmptyList.Free;
  if Length(Text) > Limits.MaxEncodedBytes then
    raise EWfcAssetManifest.Create('raw file inventory exceeds byte allowance');
  Count := 0;
  for I := 1 to Length(Text) do
  begin
    if not (Ord(Text[I]) in [10, 13, 32..126]) then
      raise EWfcAssetManifest.Create('raw inventory must contain ASCII paths and line endings');
    if Text[I] = #10 then Inc(Count);
    if (Text[I] = #13) and ((I = Length(Text)) or (Text[I + 1] <> #10)) then
      raise EWfcAssetManifest.Create('raw inventory contains a bare carriage return');
  end;
  if (Text <> '') and (Text[Length(Text)] <> #10) then Inc(Count);
  if Count > Limits.MaxFiles then
    raise EWfcAssetManifest.Create('raw inventory exceeds file count allowance');
  SetLength(Paths, Count); Count := 0; First := 1; HasTail := Text <> '';
  for I := 1 to Length(Text) do
    if Text[I] = #10 then
    begin
      Finish := I;
      if (Finish > First) and (Text[Finish - 1] = #13) then Dec(Finish);
      Path := Copy(Text, First, Finish - First);
      ValidateWfcAssetPath(Path);
      Paths[Count] := Path; Inc(Count);
      HasTail := I < Length(Text);
      if HasTail then First := I + 1;
    end;
  if HasTail then
  begin
    Path := Copy(Text, First, Length(Text) - First + 1);
    ValidateWfcAssetPath(Path); Paths[Count] := Path;
  end;
  SortPaths(Paths);
  List := TWfcAssetFileList.Create(Paths, Limits);
  try Result := EncodeWfcAssetFileList(List, Limits);
  finally List.Free; end;
end;

function WfcAssetOneLine(const Text: String): String;
var I, InspectCount: Integer; Piece: String;
  {$IFDEF PAS2JS}Valid: Boolean;{$ENDIF}
begin
  {$IFDEF PAS2JS}
  asm Valid = typeof Text === 'string'; end;
  if not Valid then Exit('[invalid diagnostic text]');
  {$ENDIF}
  Result := '';
  { The diagnostic is bounded even if a native 64-bit string exceeds the
    shared Integer index range. At least one output byte is used per input. }
  if Length(Text) > 512 then InspectCount := 512 else InspectCount := Length(Text);
  for I := 1 to InspectCount do
  begin
    if Ord(Text[I]) in [32..126] then Piece := Text[I]
    else Piece := '\x' + IntToHex(Ord(Text[I]), 2);
    if Length(Result) + Length(Piece) > 509 then
    begin Result := Result + '...'; Exit; end;
    Result := Result + Piece;
  end;
end;

function WfcAssetCommandHelp: String;
begin
  Result :=
    'WFC asset inventory checker / MIT / FPC' + #10 +
    '  wfc_asset_check --root DIR --manifest FILE --files FILE [--require-reviewed]' + #10 +
    '  wfc_asset_check --inventory-from FILE' + #10 +
    '  wfc_asset_check --help | --version' + #10 +
    'Inventory conversion writes canonical LF text to stdout; inputs are never changed.' + #10 +
    'Allowances: --max-manifest-bytes 1048576, --max-file-list-bytes 33554432,' + #10 +
    '  --max-assets 100000, --max-files 1000000, --max-evidence-paths 1000000.' + #10 +
    'Conversion accepts only file-list byte/count allowances. All are adjustable.' + #10 +
    'A path starting with -- must be written with ./ or an absolute prefix.' + #10 +
    'The supplied inventory determines scope; version 1 classifies assets by suffix.' + #10 +
    'Hashes verify bytes, not ownership. Documented review is a declaration, not legal proof.' + #10 +
    'Exit: 0 matches; 1 mismatch/unresolved when required; 2 usage; 3 metadata/policy; 4 I/O.' + #10;
end;

end.
