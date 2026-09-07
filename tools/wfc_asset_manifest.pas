{ SPDX-License-Identifier: MIT
  Copyright (c) 2021 mr-highball
  Canonical asset declarations and explicitly supplied inventory checks.
  Declarations and byte identity never establish legal ownership or permission. }
unit wfc_asset_manifest;
{$mode delphi}{$H+}
interface
uses SysUtils;
type
  EWfcAssetManifest = class(Exception);
  TWfcAssetReview = (warUnresolved, warDocumented);
  TWfcAssetPaths = array of String;
  TWfcAssetEntry = record
    Path,ByteSizeText,SHA256,Origin,Author,Version,LicenseText,Modifications: String;
    Review: TWfcAssetReview;
    EvidencePaths: TWfcAssetPaths;
  end;
  TWfcAssetEntries = array of TWfcAssetEntry;
  TWfcAssetManifestLimits = record
    Version,MaxAssets,MaxEvidencePaths,MaxEncodedBytes: Integer;
  end;
  TWfcAssetFileListLimits = record
    Version,MaxFiles,MaxEncodedBytes: Integer;
  end;
  TWfcAssetManifest = class
  strict private
    FEntries: TWfcAssetEntries;
    FLimits: TWfcAssetManifestLimits;
    FDocumentedCount,FUnresolvedCount,FEncodedBytes: Integer;
    function GetCount: Integer;
  public
    constructor Create(const Entries: TWfcAssetEntries; const Limits: TWfcAssetManifestLimits);
    function EntryAt(const Index: Integer): TWfcAssetEntry;
    function CopyLimits: TWfcAssetManifestLimits;
    property Count: Integer read GetCount;
    property DocumentedCount: Integer read FDocumentedCount;
    property UnresolvedCount: Integer read FUnresolvedCount;
    property EncodedBytes: Integer read FEncodedBytes;
  end;
  TWfcAssetFileList = class
  strict private
    FPaths: TWfcAssetPaths;
    FLimits: TWfcAssetFileListLimits;
    FEncodedBytes: Integer;
    function GetCount: Integer;
  public
    constructor Create(const Paths: TWfcAssetPaths; const Limits: TWfcAssetFileListLimits);
    function PathAt(const Index: Integer): String;
    function CopyPaths: TWfcAssetPaths;
    function CopyLimits: TWfcAssetFileListLimits;
    property Count: Integer read GetCount;
    property EncodedBytes: Integer read FEncodedBytes;
  end;
  TWfcAssetMissingEvidence = record
    AssetPath,EvidencePath: String;
  end;
  TWfcAssetMissingEvidencePaths = array of TWfcAssetMissingEvidence;
  TWfcAssetInventoryReport = record
    MissingRequiredEntries,OrphanManifestRows: TWfcAssetPaths;
    MissingEvidencePaths: TWfcAssetMissingEvidencePaths;
    DocumentedCount,UnresolvedCount: Integer;
    MatchesSuppliedInventory: Boolean;
  end;

procedure ValidateWfcAssetPath(const Path: String);
function WfcAssetPathRequiresManifest(const Path: String): Boolean;
function EncodeWfcAssetManifest(const Manifest: TWfcAssetManifest;
  const Limits: TWfcAssetManifestLimits): String;
function DecodeWfcAssetManifest(const Text: String;
  const Limits: TWfcAssetManifestLimits): TWfcAssetManifest;
function EncodeWfcAssetFileList(const FileList: TWfcAssetFileList;
  const Limits: TWfcAssetFileListLimits): String;
function DecodeWfcAssetFileList(const Text: String;
  const Limits: TWfcAssetFileListLimits): TWfcAssetFileList;
function CheckWfcAssetInventory(const Manifest: TWfcAssetManifest;
  const FileList: TWfcAssetFileList): TWfcAssetInventoryReport;

implementation
uses wfc_text_codec, wfc_model{$IFDEF PAS2JS},JS{$ENDIF};

type
  TAssetIndices = array of Integer;

procedure AssetError(const Message: String);
begin
  raise EWfcAssetManifest.Create('invalid asset declaration: '+Message);
end;

procedure RequireString(const Value: String);
{$IFDEF PAS2JS}var Valid: Boolean;{$ENDIF}
begin
  {$IFDEF PAS2JS}
  asm Valid=typeof Value==='string'; end;
  if not Valid then AssetError('string value required');
  {$ENDIF}
end;

procedure RequireIndex(const Index,Count: Integer);
{$IFDEF PAS2JS}var Valid: Boolean;{$ENDIF}
begin
  {$IFDEF PAS2JS}
  asm Valid=typeof Index==='number' && Number.isInteger(Index); end;
  if not Valid then AssetError('integer index required');
  {$ENDIF}
  if (Index<0) or (Index>=Count) then AssetError('index outside owner');
end;

procedure ManifestLimits(const Limits: TWfcAssetManifestLimits);
{$IFDEF PAS2JS}var Valid: Boolean;{$ENDIF}
begin
  {$IFDEF PAS2JS}
  asm
    function field(o,k) {
      if(o===null || typeof o!=='object' || Array.isArray(o)) return undefined;
      let d;
      while(o!==null && !(d=Object.getOwnPropertyDescriptor(o,k))) o=Object.getPrototypeOf(o);
      return d && Object.prototype.hasOwnProperty.call(d,'value') ? d.value : undefined;
    }
    function nonnegative(v) { return typeof v==='number' && Number.isInteger(v) && v>=0 && v<=2147483647; }
    Valid=field(Limits,'Version')===1 && nonnegative(field(Limits,'MaxAssets')) &&
      nonnegative(field(Limits,'MaxEvidencePaths')) && nonnegative(field(Limits,'MaxEncodedBytes'));
  end;
  if not Valid then AssetError('manifest limits require passive version-one integer fields');
  {$ENDIF}
  if (Limits.Version<>1) or (Limits.MaxAssets<0) or
    (Limits.MaxEvidencePaths<0) or (Limits.MaxEncodedBytes<0) then
    AssetError('invalid manifest limits');
end;

procedure FileListLimits(const Limits: TWfcAssetFileListLimits);
{$IFDEF PAS2JS}var Valid: Boolean;{$ENDIF}
begin
  {$IFDEF PAS2JS}
  asm
    function field(o,k) {
      if(o===null || typeof o!=='object' || Array.isArray(o)) return undefined;
      let d;
      while(o!==null && !(d=Object.getOwnPropertyDescriptor(o,k))) o=Object.getPrototypeOf(o);
      return d && Object.prototype.hasOwnProperty.call(d,'value') ? d.value : undefined;
    }
    function nonnegative(v) { return typeof v==='number' && Number.isInteger(v) && v>=0 && v<=2147483647; }
    Valid=field(Limits,'Version')===1 && nonnegative(field(Limits,'MaxFiles')) &&
      nonnegative(field(Limits,'MaxEncodedBytes'));
  end;
  if not Valid then AssetError('file-list limits require passive version-one integer fields');
  {$ENDIF}
  if (Limits.Version<>1) or (Limits.MaxFiles<0) or (Limits.MaxEncodedBytes<0) then
    AssetError('invalid file-list limits');
end;

procedure RequirePaths(const Paths: TWfcAssetPaths; const Maximum: Integer);
{$IFDEF PAS2JS}var Valid: Boolean;{$ENDIF}
begin
  {$IFDEF PAS2JS}
  asm
    Valid=Array.isArray(Paths) && Paths.length<=Maximum;
    if(Valid) for(let i=0;i<Paths.length;i++) {
      const d=Object.getOwnPropertyDescriptor(Paths,String(i));
      if(!d || !Object.prototype.hasOwnProperty.call(d,'value') || typeof d.value!=='string') {Valid=false;break;}
    }
  end;
  if not Valid then AssetError('bounded dense passive string array required');
  {$ENDIF}
  if Length(Paths)>Maximum then AssetError('path count exceeds limit');
end;

procedure RequireEntries(const Entries: TWfcAssetEntries;
  const Limits: TWfcAssetManifestLimits);
var I,Total: Integer;
{$IFDEF PAS2JS}Valid: Boolean;{$ENDIF}
begin
  {$IFDEF PAS2JS}
  asm
    function field(o,k) {
      if(o===null || typeof o!=='object' || Array.isArray(o)) return undefined;
      let d;
      while(o!==null && !(d=Object.getOwnPropertyDescriptor(o,k))) o=Object.getPrototypeOf(o);
      return d && Object.prototype.hasOwnProperty.call(d,'value') ? d.value : undefined;
    }
    const keys=['Path','ByteSizeText','SHA256','Origin','Author','Version','LicenseText','Modifications'];
    Valid=Array.isArray(Entries) && Entries.length<=Limits.MaxAssets;
    let count=0;
    if(Valid) for(let i=0;i<Entries.length;i++) {
      const d=Object.getOwnPropertyDescriptor(Entries,String(i));
      if(!d || !Object.prototype.hasOwnProperty.call(d,'value')) {Valid=false;break;}
      const e=d.value;
      for(let k=0;k<keys.length;k++) if(typeof field(e,keys[k])!=='string') {Valid=false;break;}
      if(!Valid) break;
      const review=field(e,'Review'), paths=field(e,'EvidencePaths');
      if((review!==0 && review!==1) || !Array.isArray(paths) || paths.length>Limits.MaxEvidencePaths-count) {Valid=false;break;}
      count+=paths.length;
      for(let j=0;j<paths.length;j++) {
        const p=Object.getOwnPropertyDescriptor(paths,String(j));
        if(!p || !Object.prototype.hasOwnProperty.call(p,'value') || typeof p.value!=='string') {Valid=false;break;}
      }
      if(!Valid) break;
    }
  end;
  if not Valid then AssetError('bounded dense passive asset records required');
  {$ENDIF}
  if Length(Entries)>Limits.MaxAssets then AssetError('asset count exceeds limit');
  Total:=0;
  for I:=0 to Length(Entries)-1 do
  begin
    RequirePaths(Entries[I].EvidencePaths,Limits.MaxEvidencePaths-Total);
    Total:=Total+Length(Entries[I].EvidencePaths);
  end;
end;

function FoldChar(const Value: Char): Char;
begin
  Result:=Value;
  if (Value>='a') and (Value<='z') then Result:=Chr(Ord(Value)-32);
end;

function ComparePath(const Left,Right: String; const Fold: Boolean): Integer;
var I,N: Integer; A,B: Char;
begin
  N:=Length(Left);
  if Length(Right)<N then N:=Length(Right);
  for I:=1 to N do
  begin
    A:=Left[I]; B:=Right[I];
    if Fold then begin A:=FoldChar(A); B:=FoldChar(B); end;
    if A<B then Exit(-1);
    if A>B then Exit(1);
  end;
  if Length(Left)<Length(Right) then Exit(-1);
  if Length(Left)>Length(Right) then Exit(1);
  Result:=0;
end;

procedure ValidateWfcAssetPath(const Path: String);
var I,N,Start,Finish,DotAt,J: Integer; Segment,Base: String;
begin
  RequireString(Path);
  if Path='' then AssetError('empty path');
  if Length(Path)>High(Integer) then AssetError('path exceeds portable index range');
  N:=Length(Path); Start:=0;
  for I:=0 to N do
  begin
    if I<N then
      if not (Path[I+1] in ['A'..'Z','a'..'z','0'..'9',' ','_','-','.','/']) then
        AssetError('path contains a disallowed character');
    if (I=N) or (Path[I+1]='/') then
    begin
      Finish:=I;
      if Finish<=Start then AssetError('empty path segment');
      Segment:=Copy(Path,Start+1,Finish-Start);
      if (Segment='.') or (Segment='..') then AssetError('relative traversal segment');
      if Segment[Length(Segment)] in ['.',' '] then AssetError('path segment has a trailing dot or space');
      DotAt:=0;
      while (DotAt<Length(Segment)) and (Segment[DotAt+1]<>'.') do Inc(DotAt);
      while (DotAt>0) and (Segment[DotAt]=' ') do Dec(DotAt);
      Base:='';
      if DotAt<=4 then
      begin
        Base:=Copy(Segment,1,DotAt);
        for J:=1 to Length(Base) do Base[J]:=FoldChar(Base[J]);
      end;
      if (Base='CON') or (Base='PRN') or (Base='AUX') or (Base='NUL') or
        ((Length(Base)=4) and ((Copy(Base,1,3)='COM') or (Copy(Base,1,3)='LPT')) and
        (Base[4] in ['0'..'9'])) then AssetError('reserved portable device basename');
      if I<N then Start:=I+1;
    end;
  end;
end;

procedure ValidatePathOrder(const Paths: TWfcAssetPaths);
var Indices,Scratch,Swap: TAssetIndices;
  I,N,Width,Start,Middle,Finish,L,R,K: Integer;
begin
  N:=Length(Paths);
  for I:=0 to N-1 do
  begin
    ValidateWfcAssetPath(Paths[I]);
    if (I>0) and (ComparePath(Paths[I-1],Paths[I],False)>=0) then
      AssetError('paths are not strictly ordinal sorted');
  end;
  { Folded duplicates need not be adjacent in the required ordinal order.
    Sort bounded private indices, never the supplied array or its methods. }
  SetLength(Indices,N); SetLength(Scratch,N);
  for I:=0 to N-1 do Indices[I]:=I;
  Width:=1;
  while Width<N do
  begin
    Start:=0;
    while Start<N do
    begin
      Middle:=Start;
      if Width<N-Start then Middle:=Start+Width else Middle:=N;
      if Width<N-Middle then Finish:=Middle+Width else Finish:=N;
      L:=Start; R:=Middle; K:=Start;
      while K<Finish do
      begin
        if (L<Middle) and ((R>=Finish) or
          (ComparePath(Paths[Indices[L]],Paths[Indices[R]],True)<=0)) then
        begin Scratch[K]:=Indices[L]; Inc(L); end
        else begin Scratch[K]:=Indices[R]; Inc(R); end;
        Inc(K);
      end;
      Start:=Finish;
    end;
    Swap:=Indices; Indices:=Scratch; Scratch:=Swap;
    if Width>N div 2 then Width:=N else Width:=Width*2;
  end;
  for I:=1 to N-1 do
    if ComparePath(Paths[Indices[I-1]],Paths[Indices[I]],True)=0 then
      AssetError('case-folded path collision');
end;

procedure Charge(var Used: Integer; const Amount,Maximum: Integer);
begin
  if (Amount<0) or (Used>Maximum) or (Amount>Maximum-Used) then
    AssetError('encoded byte limit exceeded');
  Used:=Used+Amount;
end;

function EncodeToken(const Value: String; const Maximum: Integer): String;
var Token: TWfcModelToken;
begin
  RequireString(Value);
  if Length(Value)>Maximum then AssetError('encoded byte limit exceeded');
  { Native descriptive String fields contain UTF-8 bytes. An implicit or
    explicit String/UTF8String cast can transcode through the Windows locale;
    copy bytes into the codec's tagged token without any such conversion. }
  {$IFDEF PAS2JS}
  Token:=Value;
  {$ELSE}
  SetLength(Token,Length(Value));
  if Value<>'' then Move(Value[1],Token[1],Length(Value));
  {$ENDIF}
  try Result:=WfcTextEncodeToken(Token,'asset declaration');
  except
    on E: EConvertError do AssetError('invalid Unicode token');
    on E: ERangeError do AssetError('token exceeds portable codec range');
  end;
  if Length(Result)>Maximum then AssetError('encoded byte limit exceeded');
end;

function DecodeToken(const Value: String): String;
var Token: TWfcModelToken;
begin
  try
    Token:=WfcTextDecodeToken(Value,'asset declaration');
    if WfcTextEncodeToken(Token,'asset declaration')<>Value then
      AssetError('noncanonical encoded token');
    {$IFDEF PAS2JS}
    Result:=Token;
    {$ELSE}
    SetLength(Result,Length(Token));
    if Token<>'' then Move(Token[1],Result[1],Length(Token));
    {$ENDIF}
  except
    on E: EConvertError do AssetError('invalid canonical token');
    on E: ERangeError do AssetError('token exceeds portable codec range');
  end;
end;

procedure ChargeLine(var Used: Integer; const Prefix,Value: String;
  const Maximum: Integer);
begin
  Charge(Used,Length(Prefix),Maximum);
  Charge(Used,Length(Value),Maximum);
  Charge(Used,1,Maximum);
end;

procedure ValidateSize(const Value: String);
const Maximum='9223372036854775807';
var I: Integer;
begin
  if (Value='') or (Length(Value)>19) then AssetError('invalid exact byte size');
  if (Length(Value)>1) and (Value[1]='0') then AssetError('noncanonical byte size');
  for I:=1 to Length(Value) do
    if not (Value[I] in ['0'..'9']) then AssetError('nondecimal byte size');
  if (Length(Value)=19) and (ComparePath(Value,Maximum,False)>0) then
    AssetError('byte size exceeds signed 64-bit range');
end;

function ReviewText(const Value: TWfcAssetReview): String;
begin
  case Value of
    warUnresolved: Result:='unresolved';
    warDocumented: Result:='documented';
  else AssetError('invalid review value'); end;
end;

function ValidateEntries(const Entries: TWfcAssetEntries;
  const Limits: TWfcAssetManifestLimits): Integer;
var I,J: Integer; Prefix,Token: String; Paths: TWfcAssetPaths;
  procedure Metadata(const Name,Value: String);
  begin
    if Value='' then AssetError('descriptive fields must not be empty');
    if (Entries[I].Review=warDocumented) and (Value='unknown') then
      AssetError('documented asset contains unresolved metadata');
    Token:=EncodeToken(Value,Limits.MaxEncodedBytes-Result);
    ChargeLine(Result,Prefix+Name+'=',Token,Limits.MaxEncodedBytes);
  end;
begin
  ManifestLimits(Limits); RequireEntries(Entries,Limits);
  Result:=0;
  ChargeLine(Result,'wfcassets=','1',Limits.MaxEncodedBytes);
  ChargeLine(Result,'assets=',IntToStr(Length(Entries)),Limits.MaxEncodedBytes);
  SetLength(Paths,Length(Entries));
  for I:=0 to Length(Entries)-1 do
  begin
    Paths[I]:=Entries[I].Path;
    Prefix:='asset.'+IntToStr(I)+'.';
    Token:=EncodeToken(Entries[I].Path,Limits.MaxEncodedBytes-Result);
    ChargeLine(Result,Prefix+'path=',Token,Limits.MaxEncodedBytes);
    ValidateSize(Entries[I].ByteSizeText);
    ChargeLine(Result,Prefix+'bytes=',Entries[I].ByteSizeText,Limits.MaxEncodedBytes);
    if Length(Entries[I].SHA256)<>64 then AssetError('SHA256 requires 64 uppercase hex digits');
    for J:=1 to 64 do
      if not (Entries[I].SHA256[J] in ['0'..'9','A'..'F']) then AssetError('SHA256 requires uppercase hex');
    ChargeLine(Result,Prefix+'sha256=',Entries[I].SHA256,Limits.MaxEncodedBytes);
    ChargeLine(Result,Prefix+'review=',ReviewText(Entries[I].Review),Limits.MaxEncodedBytes);
    Metadata('origin',Entries[I].Origin); Metadata('author',Entries[I].Author);
    Metadata('version',Entries[I].Version); Metadata('license',Entries[I].LicenseText);
    Metadata('modifications',Entries[I].Modifications);
    if (Entries[I].Review=warDocumented) and (Length(Entries[I].EvidencePaths)=0) then
      AssetError('documented asset requires evidence');
    ChargeLine(Result,Prefix+'evidence.count=',IntToStr(Length(Entries[I].EvidencePaths)),Limits.MaxEncodedBytes);
    ValidatePathOrder(Entries[I].EvidencePaths);
    for J:=0 to Length(Entries[I].EvidencePaths)-1 do
    begin
      if ComparePath(Entries[I].Path,Entries[I].EvidencePaths[J],True)=0 then
        AssetError('asset evidence cannot reference itself');
      Token:=EncodeToken(Entries[I].EvidencePaths[J],Limits.MaxEncodedBytes-Result);
      ChargeLine(Result,Prefix+'evidence.'+IntToStr(J)+'=',Token,Limits.MaxEncodedBytes);
    end;
  end;
  ValidatePathOrder(Paths);
end;

function ValidateFiles(const Paths: TWfcAssetPaths;
  const Limits: TWfcAssetFileListLimits): Integer;
var I: Integer; Token: String;
begin
  FileListLimits(Limits); RequirePaths(Paths,Limits.MaxFiles);
  Result:=0;
  ChargeLine(Result,'wfcfiles=','1',Limits.MaxEncodedBytes);
  ChargeLine(Result,'files=',IntToStr(Length(Paths)),Limits.MaxEncodedBytes);
  for I:=0 to Length(Paths)-1 do
  begin
    Token:=EncodeToken(Paths[I],Limits.MaxEncodedBytes-Result);
    ChargeLine(Result,'file.'+IntToStr(I)+'=',Token,Limits.MaxEncodedBytes);
  end;
  ValidatePathOrder(Paths);
end;

function ClonePaths(const Paths: TWfcAssetPaths): TWfcAssetPaths;
var I: Integer;
begin
  SetLength(Result,Length(Paths));
  for I:=0 to Length(Paths)-1 do Result[I]:=Paths[I];
end;

function CloneEntry(const Entry: TWfcAssetEntry): TWfcAssetEntry;
begin
  Result.Path:=Entry.Path; Result.ByteSizeText:=Entry.ByteSizeText;
  Result.SHA256:=Entry.SHA256; Result.Origin:=Entry.Origin;
  Result.Author:=Entry.Author; Result.Version:=Entry.Version;
  Result.LicenseText:=Entry.LicenseText; Result.Modifications:=Entry.Modifications;
  Result.Review:=Entry.Review; Result.EvidencePaths:=ClonePaths(Entry.EvidencePaths);
end;

constructor TWfcAssetManifest.Create(const Entries: TWfcAssetEntries;
  const Limits: TWfcAssetManifestLimits);
var I: Integer;
begin
  inherited Create;
  FEncodedBytes:=ValidateEntries(Entries,Limits);
  FLimits.Version:=Limits.Version; FLimits.MaxAssets:=Limits.MaxAssets;
  FLimits.MaxEvidencePaths:=Limits.MaxEvidencePaths; FLimits.MaxEncodedBytes:=Limits.MaxEncodedBytes;
  SetLength(FEntries,Length(Entries));
  for I:=0 to Length(Entries)-1 do
  begin
    FEntries[I]:=CloneEntry(Entries[I]);
    if Entries[I].Review=warDocumented then Inc(FDocumentedCount) else Inc(FUnresolvedCount);
  end;
end;

function TWfcAssetManifest.GetCount: Integer;
begin Result:=Length(FEntries); end;
function TWfcAssetManifest.EntryAt(const Index: Integer): TWfcAssetEntry;
begin RequireIndex(Index,Count); Result:=CloneEntry(FEntries[Index]); end;
function TWfcAssetManifest.CopyLimits: TWfcAssetManifestLimits;
begin Result:=FLimits; end;

constructor TWfcAssetFileList.Create(const Paths: TWfcAssetPaths;
  const Limits: TWfcAssetFileListLimits);
begin
  inherited Create;
  FEncodedBytes:=ValidateFiles(Paths,Limits);
  FLimits.Version:=Limits.Version; FLimits.MaxFiles:=Limits.MaxFiles;
  FLimits.MaxEncodedBytes:=Limits.MaxEncodedBytes;
  FPaths:=ClonePaths(Paths);
end;
function TWfcAssetFileList.GetCount: Integer;
begin Result:=Length(FPaths); end;
function TWfcAssetFileList.PathAt(const Index: Integer): String;
begin RequireIndex(Index,Count); Result:=FPaths[Index]; end;
function TWfcAssetFileList.CopyPaths: TWfcAssetPaths;
begin Result:=ClonePaths(FPaths); end;
function TWfcAssetFileList.CopyLimits: TWfcAssetFileListLimits;
begin Result:=FLimits; end;

function EncodeWfcAssetManifest(const Manifest: TWfcAssetManifest;
  const Limits: TWfcAssetManifestLimits): String;
var Entries: TWfcAssetEntries; Lines: TWfcTextLines;
  I,J,Count,Cursor,ExpectedBytes: Integer; Prefix: String;
  procedure Line(const Key,Value: String);
  begin
    Lines[Cursor]:=Key+Value; Inc(Cursor);
  end;
  procedure TokenLine(const Key,Value: String);
  begin Line(Key,EncodeToken(Value,Limits.MaxEncodedBytes)); end;
begin
  ManifestLimits(Limits);
  if Manifest=nil then AssetError('manifest owner required');
  if Manifest.Count>Limits.MaxAssets then AssetError('asset count exceeds limit');
  if Manifest.EncodedBytes>Limits.MaxEncodedBytes then AssetError('encoded byte limit exceeded');
  Count:=2;
  SetLength(Entries,Manifest.Count);
  for I:=0 to Manifest.Count-1 do
  begin
    Entries[I]:=Manifest.EntryAt(I);
    Charge(Count,10,Limits.MaxEncodedBytes);
    Charge(Count,Length(Entries[I].EvidencePaths),Limits.MaxEncodedBytes);
  end;
  ExpectedBytes:=ValidateEntries(Entries,Limits);
  SetLength(Lines,Count); Cursor:=0;
  Line('wfcassets=','1'); Line('assets=',IntToStr(Length(Entries)));
  for I:=0 to Length(Entries)-1 do
  begin
    Prefix:='asset.'+IntToStr(I)+'.';
    TokenLine(Prefix+'path=',Entries[I].Path);
    Line(Prefix+'bytes=',Entries[I].ByteSizeText);
    Line(Prefix+'sha256=',Entries[I].SHA256);
    Line(Prefix+'review=',ReviewText(Entries[I].Review));
    TokenLine(Prefix+'origin=',Entries[I].Origin);
    TokenLine(Prefix+'author=',Entries[I].Author);
    TokenLine(Prefix+'version=',Entries[I].Version);
    TokenLine(Prefix+'license=',Entries[I].LicenseText);
    TokenLine(Prefix+'modifications=',Entries[I].Modifications);
    Line(Prefix+'evidence.count=',IntToStr(Length(Entries[I].EvidencePaths)));
    for J:=0 to Length(Entries[I].EvidencePaths)-1 do
      TokenLine(Prefix+'evidence.'+IntToStr(J)+'=',Entries[I].EvidencePaths[J]);
  end;
  Result:=WfcTextJoinCanonicalLines(Lines,'asset declaration');
  if Length(Result)<>ExpectedBytes then AssetError('canonical size invariant failed');
end;

function EncodeWfcAssetFileList(const FileList: TWfcAssetFileList;
  const Limits: TWfcAssetFileListLimits): String;
var Paths: TWfcAssetPaths; Lines: TWfcTextLines;
  I,LineCount,ExpectedBytes: Integer;
begin
  FileListLimits(Limits);
  if FileList=nil then AssetError('file-list owner required');
  if FileList.Count>Limits.MaxFiles then AssetError('file count exceeds limit');
  if FileList.EncodedBytes>Limits.MaxEncodedBytes then AssetError('encoded byte limit exceeded');
  Paths:=FileList.CopyPaths;
  ExpectedBytes:=ValidateFiles(Paths,Limits);
  LineCount:=2; Charge(LineCount,Length(Paths),Limits.MaxEncodedBytes);
  SetLength(Lines,LineCount);
  Lines[0]:='wfcfiles=1'; Lines[1]:='files='+IntToStr(Length(Paths));
  for I:=0 to Length(Paths)-1 do
    Lines[I+2]:='file.'+IntToStr(I)+'='+EncodeToken(Paths[I],Limits.MaxEncodedBytes);
  Result:=WfcTextJoinCanonicalLines(Lines,'asset file list');
  if Length(Result)<>ExpectedBytes then AssetError('canonical size invariant failed');
end;

procedure TextPreflight(const Text: String; const Maximum: Integer;
  out LineCount: Integer);
var I: Integer; PreviousLF: Boolean;
begin
  RequireString(Text);
  if (Length(Text)=0) or (Length(Text)>Maximum) then AssetError('empty or oversized canonical text');
  if Text[Length(Text)]<>#10 then AssetError('canonical text requires final LF');
  LineCount:=0; PreviousLF:=True;
  for I:=1 to Length(Text) do
  begin
    if Text[I]=#10 then
    begin
      if PreviousLF then AssetError('blank canonical line');
      Inc(LineCount); PreviousLF:=True;
    end
    else
    begin
      if (Ord(Text[I])<32) or (Ord(Text[I])>126) then AssetError('canonical text requires ASCII and LF');
      PreviousLF:=False;
    end;
  end;
end;

function NextLine(const Text: String; var Cursor: Integer): String;
var Start: Integer;
begin
  if Cursor>=Length(Text) then AssetError('missing canonical line');
  Start:=Cursor;
  while (Cursor<Length(Text)) and (Text[Cursor+1]<>#10) do Inc(Cursor);
  if Cursor>=Length(Text) then AssetError('unterminated canonical line');
  Result:=Copy(Text,Start+1,Cursor-Start); Inc(Cursor);
end;

function NextValue(const Text: String; var Cursor: Integer;
  const Prefix: String): String;
var Line: String;
begin
  Line:=NextLine(Text,Cursor);
  if Copy(Line,1,Length(Prefix))<>Prefix then AssetError('unexpected canonical field or order');
  Result:=Copy(Line,Length(Prefix)+1,Length(Line)-Length(Prefix));
end;

function CanonicalCount(const Text: String): Integer;
begin
  try Result:=WfcTextParseCanonicalInteger(Text,'count','asset declaration');
  except on E: EConvertError do AssetError('invalid canonical count'); end;
end;

function DecodeWfcAssetManifest(const Text: String;
  const Limits: TWfcAssetManifestLimits): TWfcAssetManifest;
var Entries: TWfcAssetEntries;
  Cursor,LineCount,Count,I,J,Evidence,TotalEvidence,RemainingLines: Integer;
  Prefix,Review: String;
begin
  ManifestLimits(Limits); TextPreflight(Text,Limits.MaxEncodedBytes,LineCount);
  Cursor:=0;
  if NextLine(Text,Cursor)<>'wfcassets=1' then AssetError('unsupported manifest envelope');
  Count:=CanonicalCount(NextValue(Text,Cursor,'assets='));
  if Count>Limits.MaxAssets then AssetError('asset count exceeds limit');
  if Count>(LineCount-2) div 10 then AssetError('asset count exceeds available canonical lines');
  { Parsing scratch is bounded by both declared limits and physically supplied
    lines. Retained owner storage is allocated only after complete validation. }
  SetLength(Entries,Count); TotalEvidence:=0; RemainingLines:=LineCount-2;
  for I:=0 to Count-1 do
  begin
    Prefix:='asset.'+IntToStr(I)+'.';
    Entries[I].Path:=DecodeToken(NextValue(Text,Cursor,Prefix+'path='));
    Entries[I].ByteSizeText:=NextValue(Text,Cursor,Prefix+'bytes=');
    Entries[I].SHA256:=NextValue(Text,Cursor,Prefix+'sha256=');
    Review:=NextValue(Text,Cursor,Prefix+'review=');
    if Review='unresolved' then Entries[I].Review:=warUnresolved
    else if Review='documented' then Entries[I].Review:=warDocumented
    else AssetError('unsupported review value');
    Entries[I].Origin:=DecodeToken(NextValue(Text,Cursor,Prefix+'origin='));
    Entries[I].Author:=DecodeToken(NextValue(Text,Cursor,Prefix+'author='));
    Entries[I].Version:=DecodeToken(NextValue(Text,Cursor,Prefix+'version='));
    Entries[I].LicenseText:=DecodeToken(NextValue(Text,Cursor,Prefix+'license='));
    Entries[I].Modifications:=DecodeToken(NextValue(Text,Cursor,Prefix+'modifications='));
    Evidence:=CanonicalCount(NextValue(Text,Cursor,Prefix+'evidence.count='));
    RemainingLines:=RemainingLines-10;
    if (Evidence>Limits.MaxEvidencePaths-TotalEvidence) or (Evidence>RemainingLines) then
      AssetError('evidence count exceeds limit or available lines');
    TotalEvidence:=TotalEvidence+Evidence;
    SetLength(Entries[I].EvidencePaths,Evidence);
    for J:=0 to Evidence-1 do
      Entries[I].EvidencePaths[J]:=DecodeToken(NextValue(Text,Cursor,Prefix+'evidence.'+IntToStr(J)+'='));
    RemainingLines:=RemainingLines-Evidence;
  end;
  if Cursor<Length(Text) then AssetError('extra canonical lines');
  Result:=TWfcAssetManifest.Create(Entries,Limits);
end;

function DecodeWfcAssetFileList(const Text: String;
  const Limits: TWfcAssetFileListLimits): TWfcAssetFileList;
var Paths: TWfcAssetPaths; Cursor,LineCount,Count,I: Integer;
begin
  FileListLimits(Limits); TextPreflight(Text,Limits.MaxEncodedBytes,LineCount);
  Cursor:=0;
  if NextLine(Text,Cursor)<>'wfcfiles=1' then AssetError('unsupported file-list envelope');
  Count:=CanonicalCount(NextValue(Text,Cursor,'files='));
  if Count>Limits.MaxFiles then AssetError('file count exceeds limit');
  if Count<>LineCount-2 then AssetError('file count does not match canonical lines');
  SetLength(Paths,Count);
  for I:=0 to Count-1 do
    Paths[I]:=DecodeToken(NextValue(Text,Cursor,'file.'+IntToStr(I)+'='));
  if Cursor<Length(Text) then AssetError('extra canonical lines');
  Result:=TWfcAssetFileList.Create(Paths,Limits);
end;

function WfcAssetPathRequiresManifest(const Path: String): Boolean;
const Suffixes='|PNG|JPG|JPEG|GIF|BMP|ICO|RES|ZIP|7Z|TAR|GZ|FBX|OBJ|GLB|GLTF|BLEND|STL|SVG|WAV|WAVE|MP3|OGG|FLAC|MID|MIDI|TTF|OTF|WOFF|WOFF2|PDF|';
var I,DotAt: Integer; Suffix: String;
begin
  ValidateWfcAssetPath(Path);
  DotAt:=0;
  for I:=1 to Length(Path) do
    if Path[I]='/' then DotAt:=0 else if Path[I]='.' then DotAt:=I;
  if DotAt=0 then Exit(False);
  if Length(Path)-DotAt>5 then Exit(False);
  Suffix:=Copy(Path,DotAt+1,Length(Path)-DotAt);
  for I:=1 to Length(Suffix) do Suffix[I]:=FoldChar(Suffix[I]);
  Result:=Pos('|'+Suffix+'|',Suffixes)>0;
end;

function HasPath(const Paths: TWfcAssetPaths; const Path: String): Boolean;
var Left,Right,Middle,Comparison: Integer;
begin
  Left:=0; Right:=Length(Paths)-1;
  while Left<=Right do
  begin
    Middle:=Left+(Right-Left) div 2;
    Comparison:=ComparePath(Paths[Middle],Path,False);
    if Comparison=0 then Exit(True);
    if Comparison<0 then Left:=Middle+1 else Right:=Middle-1;
  end;
  Result:=False;
end;

function CheckWfcAssetInventory(const Manifest: TWfcAssetManifest;
  const FileList: TWfcAssetFileList): TWfcAssetInventoryReport;
var Files,Assets: TWfcAssetPaths; Entry: TWfcAssetEntry;
  I,J,MissingCount,OrphanCount,EvidenceCount,Pass: Integer;
begin
  if (Manifest=nil) or (FileList=nil) then AssetError('manifest and file-list owners required');
  Files:=FileList.CopyPaths;
  SetLength(Assets,Manifest.Count);
  for I:=0 to Manifest.Count-1 do
  begin Entry:=Manifest.EntryAt(I); Assets[I]:=Entry.Path; end;
  Result.DocumentedCount:=Manifest.DocumentedCount;
  Result.UnresolvedCount:=Manifest.UnresolvedCount;
  { Count, then allocate the exact detached report. The maximum sizes follow
    the already validated owner counts; no hidden inventory cap is inferred. }
  for Pass:=0 to 1 do
  begin
    MissingCount:=0; OrphanCount:=0; EvidenceCount:=0;
    for I:=0 to Length(Files)-1 do
      if WfcAssetPathRequiresManifest(Files[I]) and not HasPath(Assets,Files[I]) then
      begin
        if Pass=1 then Result.MissingRequiredEntries[MissingCount]:=Files[I];
        Inc(MissingCount);
      end;
    for I:=0 to Manifest.Count-1 do
    begin
      Entry:=Manifest.EntryAt(I);
      if not HasPath(Files,Entry.Path) then
      begin
        if Pass=1 then Result.OrphanManifestRows[OrphanCount]:=Entry.Path;
        Inc(OrphanCount);
      end;
      for J:=0 to Length(Entry.EvidencePaths)-1 do
        if not HasPath(Files,Entry.EvidencePaths[J]) then
        begin
          if Pass=1 then
          begin
            Result.MissingEvidencePaths[EvidenceCount].AssetPath:=Entry.Path;
            Result.MissingEvidencePaths[EvidenceCount].EvidencePath:=Entry.EvidencePaths[J];
          end;
          Inc(EvidenceCount);
        end;
    end;
    if Pass=0 then
    begin
      SetLength(Result.MissingRequiredEntries,MissingCount);
      SetLength(Result.OrphanManifestRows,OrphanCount);
      SetLength(Result.MissingEvidencePaths,EvidenceCount);
    end;
  end;
  Result.MatchesSuppliedInventory:=(MissingCount=0) and (OrphanCount=0) and (EvidenceCount=0);
end;

end.
