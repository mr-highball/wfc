{ SPDX-License-Identifier: MIT
  Copyright (c) 2021 mr-highball
  Portable asset declarations: complete canonical bytes, ownership and scope.
  No filesystem access, permission inference, forged owners or Proxy sandbox. }
program wfc_asset_manifest_test;
{$mode delphi}{$H+}
uses {$IFDEF PAS2JS}wfc_browser_test_host, JS,{$ENDIF}
  SysUtils, wfc_asset_manifest;

const
  ZERO_HASH='0000000000000000000000000000000000000000000000000000000000000000';
  EMPTY_MANIFEST='wfcassets=1'#10'assets=0'#10;
  EMPTY_FILES='wfcfiles=1'#10'files=0'#10;
  ONE_MANIFEST='wfcassets=1'#10'assets=1'#10+
    'asset.0.path=assets%2FA%20file.png'#10+
    'asset.0.bytes=9223372036854775807'#10+
    'asset.0.sha256='+ZERO_HASH+#10+
    'asset.0.review=unresolved'#10+
    'asset.0.origin=unknown'#10'asset.0.author=unknown'#10+
    'asset.0.version=unknown'#10'asset.0.license=unknown'#10+
    'asset.0.modifications=unknown'#10'asset.0.evidence.count=1'#10+
    'asset.0.evidence.0=docs%2Fasset%20notes.md'#10;
  TWO_FILES='wfcfiles=1'#10'files=2'#10+
    'file.0=assets%2FA%20file.png'#10'file.1=docs%2Fasset%20notes.md'#10;
var Checks: Integer;

procedure Check(const OK:Boolean; const Name:String);
begin Inc(Checks); if not OK then raise Exception.Create('asset manifest test: '+Name); end;

function ML(const Assets:Integer=64; const Evidence:Integer=128;
  const Bytes:Integer=65536):TWfcAssetManifestLimits;
begin Result.Version:=1;Result.MaxAssets:=Assets;Result.MaxEvidencePaths:=Evidence;Result.MaxEncodedBytes:=Bytes;end;
function FL(const Files:Integer=128; const Bytes:Integer=65536):TWfcAssetFileListLimits;
begin Result.Version:=1;Result.MaxFiles:=Files;Result.MaxEncodedBytes:=Bytes;end;
function Paths(const Values:array of String):TWfcAssetPaths;
var I:Integer;
begin SetLength(Result,Length(Values));for I:=0 to High(Values) do Result[I]:=Values[I];end;
function Entry(const Path:String):TWfcAssetEntry;
begin
  Result.Path:=Path;Result.ByteSizeText:='0';Result.SHA256:=ZERO_HASH;
  Result.Review:=warUnresolved;Result.Origin:='unknown';Result.Author:='unknown';
  Result.Version:='unknown';Result.LicenseText:='unknown';
  Result.Modifications:='unknown';Result.EvidencePaths:=nil;
end;
function One:TWfcAssetEntries;
begin
  SetLength(Result,1);Result[0]:=Entry('assets/A file.png');
  Result[0].ByteSizeText:='9223372036854775807';
  Result[0].EvidencePaths:=Paths(['docs/asset notes.md']);
end;
procedure Documented(var E:TWfcAssetEntry);
begin
  E.Review:=warDocumented;E.Origin:='local source';E.Author:='Named author';
  E.Version:='v1';E.LicenseText:='recorded terms';E.Modifications:='none';
end;
procedure SameEntry(const A,B:TWfcAssetEntry; const Name:String);
var I:Integer;
begin
  Check(A.Path=B.Path,Name+' path');Check(A.ByteSizeText=B.ByteSizeText,Name+' exact decimal');
  Check(A.SHA256=B.SHA256,Name+' digest');Check(A.Review=B.Review,Name+' review');
  Check(A.Origin=B.Origin,Name+' origin');Check(A.Author=B.Author,Name+' author');
  Check(A.Version=B.Version,Name+' version');Check(A.LicenseText=B.LicenseText,Name+' license');
  Check(A.Modifications=B.Modifications,Name+' modifications');
  Check(Length(A.EvidencePaths)=Length(B.EvidencePaths),Name+' evidence count');
  for I:=0 to High(A.EvidencePaths) do Check(A.EvidencePaths[I]=B.EvidencePaths[I],Name+' evidence row');
end;
procedure TypedError(const ClassName,Detail,Name:String);
var I:Integer;Safe:Boolean;
begin
  Check(ClassName='EWfcAssetManifest',Name+' concrete exception: '+ClassName);
  Check((Length(Detail)>0) and (Length(Detail)<=512),Name+' bounded nonempty diagnostic');
  Safe:=True;for I:=1 to Length(Detail) do
    if (Ord(Detail[I])<32) or (Ord(Detail[I])=127) then Safe:=False;
  Check(Safe,Name+' terminal-safe diagnostic');
end;
procedure RejectManifest(const E:TWfcAssetEntries; const L:TWfcAssetManifestLimits;
  const Name:String);
var M:TWfcAssetManifest;C,D:String;
begin
  M:=nil;C:='';D:='';
  try
    try M:=TWfcAssetManifest.Create(E,L);except on X:Exception do begin C:=X.ClassName;D:=X.Message;end;end;
    TypedError(C,D,Name);Check(M=nil,Name+' no partial owner');
  finally M.Free;end;
end;
procedure RejectFiles(const P:TWfcAssetPaths; const L:TWfcAssetFileListLimits;
  const Name:String);
var F:TWfcAssetFileList;C,D:String;
begin
  F:=nil;C:='';D:='';
  try
    try F:=TWfcAssetFileList.Create(P,L);except on X:Exception do begin C:=X.ClassName;D:=X.Message;end;end;
    TypedError(C,D,Name);Check(F=nil,Name+' no partial owner');
  finally F.Free;end;
end;
procedure RejectText(const Text:String; const IsManifest:Boolean; const Name:String);
var M:TWfcAssetManifest;F:TWfcAssetFileList;C,D:String;
begin
  M:=nil;F:=nil;C:='';D:='';
  try
    try if IsManifest then M:=DecodeWfcAssetManifest(Text,ML)
      else F:=DecodeWfcAssetFileList(Text,FL);
    except on X:Exception do begin C:=X.ClassName;D:=X.Message;end;end;
    TypedError(C,D,Name);Check((M=nil) and (F=nil),Name+' no decoded partial owner');
  finally F.Free;M.Free;end;
end;
function Change(const Text,FromValue,ToValue:String):String;
begin
  Check(Pos(FromValue,Text)>0,'mutation names an actual field');
  Result:=StringReplace(Text,FromValue,ToValue,[]);
end;

procedure TestCanonicalAndOwnership;
var E:TWfcAssetEntries;P,Q:TWfcAssetPaths;M,N:TWfcAssetManifest;
  F,G:TWfcAssetFileList;R:TWfcAssetInventoryReport;A,B:TWfcAssetEntry;
  L:TWfcAssetManifestLimits;K:TWfcAssetFileListLimits;S:String;
begin
  M:=TWfcAssetManifest.Create(nil,ML(0,0,Length(EMPTY_MANIFEST)));
  F:=TWfcAssetFileList.Create(nil,FL(0,Length(EMPTY_FILES)));
  try
    Check(M.Count=0,'empty manifest count');Check(F.Count=0,'empty file-list count');
    Check(M.DocumentedCount=0,'empty documented count');Check(M.UnresolvedCount=0,'empty unresolved count');
    Check(EncodeWfcAssetManifest(M,ML)=EMPTY_MANIFEST,'independent complete empty manifest');
    Check(EncodeWfcAssetFileList(F,FL)=EMPTY_FILES,'independent complete empty file list');
    Check(M.EncodedBytes=Length(EMPTY_MANIFEST),'empty encoded byte allowance');
    Check(F.EncodedBytes=Length(EMPTY_FILES),'empty file encoded byte allowance');
    R:=CheckWfcAssetInventory(M,F);Check(R.MatchesSuppliedInventory,'empty supplied inventory matches');
    Check((Length(R.MissingRequiredEntries)=0) and (Length(R.OrphanManifestRows)=0) and
      (Length(R.MissingEvidencePaths)=0),'empty complete mismatch lists');
  finally F.Free;M.Free;end;
  E:=One;P:=Paths(['assets/A file.png','docs/asset notes.md']);
  M:=TWfcAssetManifest.Create(E,ML);F:=TWfcAssetFileList.Create(P,FL);
  N:=nil;G:=nil;
  try
    Check(EncodeWfcAssetManifest(M,ML)=ONE_MANIFEST,'independent full manifest bytes');
    Check(EncodeWfcAssetFileList(F,FL)=TWO_FILES,'independent full inventory bytes');
    Check(M.EncodedBytes=Length(ONE_MANIFEST),'all manifest envelope bytes counted');
    Check(F.EncodedBytes=Length(TWO_FILES),'all file-list envelope bytes counted');
    N:=DecodeWfcAssetManifest(ONE_MANIFEST,ML);G:=DecodeWfcAssetFileList(TWO_FILES,FL);
    SameEntry(E[0],N.EntryAt(0),'roundtrip complete entry');
    Check(EncodeWfcAssetManifest(N,ML)=ONE_MANIFEST,'canonical manifest reencode');
    Check(EncodeWfcAssetFileList(G,FL)=TWO_FILES,'canonical file-list reencode');
    E[0].Path:='changed.png';E[0].EvidencePaths[0]:='changed.md';E[0].Origin:='changed';
    P[0]:='changed.png';SetLength(E,0);SetLength(P,0);
    Check(EncodeWfcAssetManifest(M,ML)=ONE_MANIFEST,'constructor detaches all caller rows');
    Check(EncodeWfcAssetFileList(F,FL)=TWO_FILES,'constructor detaches caller paths');
    A:=M.EntryAt(0);B:=M.EntryAt(0);A.Path:='other.png';A.EvidencePaths[0]:='other.md';
    Check(B.EvidencePaths[0]='docs/asset notes.md','entry copies do not alias one another');
    Check(EncodeWfcAssetManifest(M,ML)=ONE_MANIFEST,'entry copy cannot mutate owner');
    Q:=F.CopyPaths;Q[0]:='other.png';Check(F.PathAt(0)='assets/A file.png','path-array copy detached');
    L:=M.CopyLimits;L.MaxAssets:=0;Check(M.CopyLimits.MaxAssets=64,'manifest limits copy detached');
    K:=F.CopyLimits;K.MaxFiles:=0;Check(F.CopyLimits.MaxFiles=128,'file limits copy detached');
    R:=CheckWfcAssetInventory(M,F);Check(R.MatchesSuppliedInventory,'matching unresolved inventory');
    Check((R.UnresolvedCount=1) and (R.DocumentedCount=0),'matching does not imply reviewed');
    A:=M.EntryAt(0);Q:=F.CopyPaths;S:=EncodeWfcAssetManifest(M,ML);
  finally G.Free;N.Free;F.Free;M.Free;end;
  Check(A.Path='assets/A file.png','entry survives owner destruction');
  Check(A.EvidencePaths[0]='docs/asset notes.md','evidence survives owner destruction');
  Check(Q[1]='docs/asset notes.md','paths survive owner destruction');
  Check(S=ONE_MANIFEST,'complete text survives owner destruction');
end;

procedure TestLimits;
var E:TWfcAssetEntries;P:TWfcAssetPaths;M,N:TWfcAssetManifest;F,G:TWfcAssetFileList;
  L:TWfcAssetManifestLimits;K:TWfcAssetFileListLimits;I:Integer;C,D,S:String;
begin
  E:=One;P:=Paths(['assets/A file.png','docs/asset notes.md']);
  M:=TWfcAssetManifest.Create(E,ML(1,1,Length(ONE_MANIFEST)));
  F:=TWfcAssetFileList.Create(P,FL(2,Length(TWO_FILES)));
  try
    Check(EncodeWfcAssetManifest(M,ML(1,1,Length(ONE_MANIFEST)))=ONE_MANIFEST,'all exact manifest limits');
    Check(EncodeWfcAssetFileList(F,FL(2,Length(TWO_FILES)))=TWO_FILES,'all exact file limits');
    for I:=0 to 2 do begin
      L:=ML(1,1,Length(ONE_MANIFEST));
      case I of 0:L.MaxAssets:=0;1:L.MaxEvidencePaths:=0;2:Dec(L.MaxEncodedBytes);end;
      RejectManifest(E,L,'one-less constructor manifest allowance '+IntToStr(I));
      C:='';D:='';try S:=EncodeWfcAssetManifest(M,L);except on X:Exception do begin C:=X.ClassName;D:=X.Message;end;end;
      TypedError(C,D,'encoder reapplies complete manifest allowance');
      N:=nil;try
        C:='';D:='';try N:=DecodeWfcAssetManifest(ONE_MANIFEST,L);except on X:Exception do begin C:=X.ClassName;D:=X.Message;end;end;
        TypedError(C,D,'decoder manifest allowance');Check(N=nil,'decoder retains no refused manifest');
      finally N.Free;end;
    end;
    for I:=0 to 1 do begin
      K:=FL(2,Length(TWO_FILES));if I=0 then K.MaxFiles:=1 else Dec(K.MaxEncodedBytes);
      RejectFiles(P,K,'one-less constructor file allowance');
      C:='';D:='';try S:=EncodeWfcAssetFileList(F,K);except on X:Exception do begin C:=X.ClassName;D:=X.Message;end;end;
      TypedError(C,D,'encoder reapplies complete file allowance');
      G:=nil;try
        C:='';D:='';try G:=DecodeWfcAssetFileList(TWO_FILES,K);except on X:Exception do begin C:=X.ClassName;D:=X.Message;end;end;
        TypedError(C,D,'decoder file allowance');Check(G=nil,'decoder retains no refused file-list');
      finally G.Free;end;
    end;
    Check(EncodeWfcAssetManifest(M,ML)=ONE_MANIFEST,'all failed policies preserve existing owner');
  finally F.Free;M.Free;end;
  RejectManifest(nil,ML(0,0,Length(EMPTY_MANIFEST)-1),'empty still charges its full header');
  RejectFiles(nil,FL(0,Length(EMPTY_FILES)-1),'empty file list still charges its header');
  for I:=0 to 4 do begin
    L:=ML;case I of 0:L.Version:=0;1:L.Version:=2;2:L.MaxAssets:=-1;3:L.MaxEvidencePaths:=-1;4:L.MaxEncodedBytes:=-1;end;
    RejectManifest(nil,L,'invalid manifest policy '+IntToStr(I));
  end;
  for I:=0 to 3 do begin
    K:=FL;case I of 0:K.Version:=0;1:K.Version:=2;2:K.MaxFiles:=-1;3:K.MaxEncodedBytes:=-1;end;
    RejectFiles(nil,K,'invalid file policy '+IntToStr(I));
  end;
  SetLength(E,2);E[0]:=Entry('a.png');E[1]:=Entry('b.png');
  E[0].EvidencePaths:=Paths(['proof.md']);E[1].EvidencePaths:=Paths(['proof.md']);
  RejectManifest(E,ML(2,1),'evidence limit is total across entries, not per entry');
end;

procedure RejectPath(const Value,Name:String);
var C,D:String;B:Boolean;
begin
  C:='';D:='';try ValidateWfcAssetPath(Value);except on X:Exception do begin C:=X.ClassName;D:=X.Message;end;end;
  TypedError(C,D,Name);
  C:='';D:='';try B:=WfcAssetPathRequiresManifest(Value);except on X:Exception do begin C:=X.ClassName;D:=X.Message;end;end;
  TypedError(C,D,Name+' classifier validates path');
end;
procedure TestPathsAndClassification;
const Suffixes:array[0..30] of String=('png','jpg','jpeg','gif','bmp','ico','res','zip','7z','tar','gz','fbx','obj','glb','gltf','blend','stl','svg','wav','wave','mp3','ogg','flac','mid','midi','ttf','otf','woff','woff2','pdf','PNG');
var Bad,Good:TWfcAssetPaths;I:Integer;E:TWfcAssetEntries;M:TWfcAssetManifest;F:TWfcAssetFileList;
begin
  Good:=Paths(['A B/file_1-2.png','.hidden','dir.v1/file','ordinary .png','x/a..b','x/.gitignore','conifer.txt','COM10.png','LPT10.txt']);
  for I:=0 to High(Good) do begin ValidateWfcAssetPath(Good[I]);Check(True,'accepted portable path '+IntToStr(I));end;
  Bad:=Paths(['','/a.png','a//b.png','a/','./a','../a','a/../b','a/./b','a\b','C:/a','a:b','a?b','a*b','a|b','a<b','a>b','a"b','a'+#9+'b','a'+#10+'b','a'+#27+'b','a'+#127+'b','a'+#128+'b','a.','a ','a./b','a /b','CON','con.txt','PrN.png','AUX.wav','NUL.tar.gz','x/COM0.txt','x/COM9','LPT0.png','lpt9.foo','CON .png','COM1 .txt']);
  for I:=0 to High(Bad) do begin
    RejectPath(Bad[I],'invalid portable path '+IntToStr(I));
    SetLength(E,1);E[0]:=Entry(Bad[I]);RejectManifest(E,ML,'entry path '+IntToStr(I));
    RejectFiles(Paths([Bad[I]]),FL,'file-list path '+IntToStr(I));
  end;
  for I:=0 to High(Suffixes) do Check(WfcAssetPathRequiresManifest('some/file.'+Suffixes[I]),'required suffix '+Suffixes[I]);
  Check(WfcAssetPathRequiresManifest('.svg'),'dotfile suffix classified');
  Check(not WfcAssetPathRequiresManifest('picture.png.txt'),'only final suffix classified');
  Check(not WfcAssetPathRequiresManifest('picture.png/readme'),'directory suffix irrelevant');
  Check(not WfcAssetPathRequiresManifest('blob.bin'),'unknown binary detection not claimed');
  Check(not WfcAssetPathRequiresManifest('source.pas'),'source licensing not inferred');
  RejectFiles(Paths(['b.png','a.png']),FL,'unsorted file paths');
  RejectFiles(Paths(['a.png','a.png']),FL,'duplicate file paths');
  RejectFiles(Paths(['A.png','B.png','a.png']),FL,'nonadjacent file case-fold collision');
  SetLength(E,3);E[0]:=Entry('A.png');E[1]:=Entry('B.png');E[2]:=Entry('a.png');
  RejectManifest(E,ML,'nonadjacent asset case-fold collision');
  SetLength(E,2);E[0]:=Entry('b.png');E[1]:=Entry('a.png');RejectManifest(E,ML,'unsorted asset paths');
  E[0]:=Entry('a.png');RejectManifest(E,ML,'duplicate asset paths');
  SetLength(E,1);E[0]:=Entry('x.png');E[0].EvidencePaths:=Paths(['A.md','B.md','a.md']);
  RejectManifest(E,ML,'nonadjacent evidence case-fold collision');
  E[0].EvidencePaths:=Paths(['b.md','a.md']);RejectManifest(E,ML,'unsorted evidence paths');
  E[0].EvidencePaths:=Paths(['x.png']);RejectManifest(E,ML,'evidence cannot be asset itself');
  E[0].EvidencePaths:=Paths(['X.PNG']);RejectManifest(E,ML,'case-fold self evidence forbidden');
  E[0].EvidencePaths:=Paths(['../proof']);RejectManifest(E,ML,'evidence obeys path profile');
  E[0].EvidencePaths:=nil;M:=TWfcAssetManifest.Create(E,ML);F:=TWfcAssetFileList.Create(Paths(['x.png']),FL);
  try Check(CheckWfcAssetInventory(M,F).MatchesSuppliedInventory,'unresolved entry needs no invented evidence');finally F.Free;M.Free;end;
end;

function UnicodeMetadata:String;
begin
  {$IFDEF PAS2JS}Result:='caf'+#$E9+' '+#$D83C#$DFB5;
  {$ELSE}Result:='caf'+#$C3#$A9+' '+#$F0#$9F#$8E#$B5;{$ENDIF}
end;
procedure TestMetadataAndSizes;
const GoodSizes:array[0..5] of String=('0','1','9007199254740991','9007199254740992','9007199254740993','9223372036854775807');
  BadSizes:array[0..11] of String=('','00','01','-1','+1',' 1','1 ','1.0','1e3','9223372036854775808','99999999999999999999','1'+#10);
var E:TWfcAssetEntries;M,N:TWfcAssetManifest;S,Expected:String;I,J:Integer;
begin
  E:=One;
  for I:=0 to High(GoodSizes) do begin
    E[0].ByteSizeText:=GoodSizes[I];M:=TWfcAssetManifest.Create(E,ML);N:=nil;
    try
      S:=EncodeWfcAssetManifest(M,ML);Expected:=Change(ONE_MANIFEST,'bytes=9223372036854775807','bytes='+GoodSizes[I]);
      Check(S=Expected,'huge decimal preserved in full bytes '+IntToStr(I));
      N:=DecodeWfcAssetManifest(S,ML);Check(N.EntryAt(0).ByteSizeText=GoodSizes[I],'huge decimal is not a JS number');
    finally N.Free;M.Free;end;
  end;
  for I:=0 to High(BadSizes) do begin E:=One;E[0].ByteSizeText:=BadSizes[I];RejectManifest(E,ML,'bad canonical byte size '+IntToStr(I));end;
  for I:=0 to 3 do begin
    E:=One;case I of 0:E[0].SHA256:=Copy(ZERO_HASH,1,63);1:E[0].SHA256:=ZERO_HASH+'0';2:E[0].SHA256:='a'+Copy(ZERO_HASH,2,63);3:E[0].SHA256:='G'+Copy(ZERO_HASH,2,63);end;
    RejectManifest(E,ML,'bad exact digest '+IntToStr(I));
  end;
  E:=One;E[0].Origin:=UnicodeMetadata;E[0].Author:='A & B';E[0].Version:='v=1';
  E[0].LicenseText:='terms: ~';E[0].Modifications:='100% retained';
  Expected:=Change(ONE_MANIFEST,'asset.0.origin=unknown','asset.0.origin=caf%C3%A9%20%F0%9F%8E%B5');
  Expected:=Change(Expected,'asset.0.author=unknown','asset.0.author=A%20%26%20B');
  Expected:=Change(Expected,'asset.0.version=unknown','asset.0.version=v%3D1');
  Expected:=Change(Expected,'asset.0.license=unknown','asset.0.license=terms%3A%20~');
  Expected:=Change(Expected,'asset.0.modifications=unknown','asset.0.modifications=100%25%20retained');
  M:=TWfcAssetManifest.Create(E,ML);N:=nil;
  try
    Check(EncodeWfcAssetManifest(M,ML)=Expected,'independent UTF-8/percent metadata envelope');
    N:=DecodeWfcAssetManifest(Expected,ML);SameEntry(E[0],N.EntryAt(0),'all Unicode metadata roundtrips');
  finally N.Free;M.Free;end;
  for J:=0 to 1 do for I:=0 to 4 do begin
    E:=One;if J=1 then Documented(E[0]);
    case I of 0:E[0].Origin:='';1:E[0].Author:='';2:E[0].Version:='';3:E[0].LicenseText:='';4:E[0].Modifications:='';end;
    RejectManifest(E,ML,'empty required descriptive field '+IntToStr(J)+'/'+IntToStr(I));
  end;
  for I:=0 to 4 do begin
    E:=One;Documented(E[0]);
    case I of 0:E[0].Origin:='unknown';1:E[0].Author:='unknown';2:E[0].Version:='unknown';3:E[0].LicenseText:='unknown';4:E[0].Modifications:='unknown';end;
    RejectManifest(E,ML,'documented exact unknown field '+IntToStr(I));
  end;
  E:=One;Documented(E[0]);E[0].EvidencePaths:=nil;RejectManifest(E,ML,'documented requires evidence');
  E:=One;Documented(E[0]);E[0].Origin:='Unknown';E[0].LicenseText:='not a known identifier';
  M:=TWfcAssetManifest.Create(E,ML);
  try Check((M.DocumentedCount=1) and (M.UnresolvedCount=0),'exact sentinel only, no legal inference');finally M.Free;end;
end;

procedure TestTextRefusals;
var I:Integer;S:String;
begin
  for I:=0 to 20 do begin
    case I of
      0:S:='';1:S:=Copy(ONE_MANIFEST,1,Length(ONE_MANIFEST)-1);
      2:S:=#13+ONE_MANIFEST;3:S:=ONE_MANIFEST+#10;
      4:S:=Change(ONE_MANIFEST,'wfcassets=1','wfcassets=2');
      5:S:=Change(ONE_MANIFEST,'assets=1'#10,'assets=01'#10);
      6:S:=Change(ONE_MANIFEST,'assets=1'#10,'assets=2147483648'#10);
      7:S:=Change(ONE_MANIFEST,'assets=1'#10,'assets=2'#10);
      8:S:=Change(ONE_MANIFEST,'asset.0.path=','asset.1.path=');
      9:S:=Change(ONE_MANIFEST,'asset.0.path=assets%2FA%20file.png','asset.0.path=assets/A%20file.png');
      10:S:=Change(ONE_MANIFEST,'%2F','%2f');
      11:S:=Change(ONE_MANIFEST,'unknown','%75nknown');
      12:S:=Change(ONE_MANIFEST,'unknown','%C0%AF');
      13:S:=Change(ONE_MANIFEST,'unknown','%ED%A0%80');
      14:S:=Change(ONE_MANIFEST,'unknown','%F4%90%80%80');
      15:S:=Change(ONE_MANIFEST,'unresolved','Unresolved');
      16:S:=Change(ONE_MANIFEST,'asset.0.evidence.count=1','asset.0.evidence.count=-1');
      17:S:=ONE_MANIFEST+'ignored=yes'#10;
      18:S:=Change(ONE_MANIFEST,'asset.0.author=unknown'#10,'asset.0.author=unknown'#10'asset.0.author=unknown'#10);
      19:S:=Change(ONE_MANIFEST,'asset.0.origin=unknown'#10'asset.0.author=unknown'#10,'asset.0.author=unknown'#10'asset.0.origin=unknown'#10);
      20:S:=Change(ONE_MANIFEST,'unknown','%');
    end;
    RejectText(S,True,'manifest grammar mutation '+IntToStr(I));
  end;
  for I:=0 to 11 do begin
    case I of
      0:S:='';1:S:=Copy(TWO_FILES,1,Length(TWO_FILES)-1);2:S:=TWO_FILES+#10;
      3:S:=Change(TWO_FILES,'wfcfiles=1','wfcfiles=0');
      4:S:=Change(TWO_FILES,'files=2','files=02');
      5:S:=Change(TWO_FILES,'files=2','files=2147483648');
      6:S:=Change(TWO_FILES,'file.1=','file.0=');
      7:S:=Change(TWO_FILES,'file.0=assets%2FA%20file.png','file.0=../escape.png');
      8:S:=Change(TWO_FILES,'%2F','%2f');
      9:S:=Change(TWO_FILES,'file.1=docs%2Fasset%20notes.md','file.1=assets%2FA%20file.png');
      10:S:=Change(TWO_FILES,'files=2','files=1');11:S:=#13+TWO_FILES;
    end;
    RejectText(S,False,'file grammar mutation '+IntToStr(I));
  end;
  RejectText('wfcassets=1'#10'assets=0'#10'# comment'#10,True,'manifest comments forbidden');
  RejectText('wfcfiles=1'#10'files=0'#10'# comment'#10,False,'file comments forbidden');
  {$IFDEF PAS2JS}S:=#$FEFF;{$ELSE}S:=#$EF#$BB#$BF;{$ENDIF}
  RejectText(S+ONE_MANIFEST,True,'manifest BOM forbidden');
  RejectText(S+TWO_FILES,False,'file-list BOM forbidden');
end;

{$IFNDEF PAS2JS}
procedure TestNativeUtf8Bytes;
const Bad: array[0..6] of String = (
  'caf'+#233, #128, #195, #192#175, #237#160#128,
  #244#144#128#128, #240#159#142);
var E: TWfcAssetEntries; I: Integer;
begin
  { These native raw bytes are invalid UTF-8, not ANSI to be auto-transcoded.
    The existing independent Unicode envelope checks valid UTF-8 plus a
    supplementary character in both native and browser representations. }
  for I:=0 to High(Bad) do
  begin
    E:=One; E[0].Author:=Bad[I];
    RejectManifest(E,ML,'native malformed UTF-8 bytes '+IntToStr(I));
  end;
end;
{$ENDIF}

procedure TestIndices;
var M:TWfcAssetManifest;F:TWfcAssetFileList;I,J:Integer;C,D,S:String;
begin
  M:=TWfcAssetManifest.Create(One,ML);F:=TWfcAssetFileList.Create(Paths(['a.png']),FL);
  try
    for I:=-1 to 1 do if I<>0 then for J:=0 to 1 do begin
      C:='';D:='';try if J=0 then M.EntryAt(I) else S:=F.PathAt(I);
      except on X:Exception do begin C:=X.ClassName;D:=X.Message;end;end;
      TypedError(C,D,'shared negative/past-end index');
    end;
  finally F.Free;M.Free;end;
end;

procedure TestInventoryReports;
var E:TWfcAssetEntries;M:TWfcAssetManifest;F:TWfcAssetFileList;
  R,S:TWfcAssetInventoryReport;
begin
  SetLength(E,3);E[0]:=Entry('a.png');E[1]:=Entry('z.obj');E[2]:=Entry('z.txt');
  E[0].EvidencePaths:=Paths(['LICENSE','docs/missing.md']);
  Documented(E[1]);E[1].EvidencePaths:=Paths(['LICENSE','docs/missing.md']);
  Documented(E[2]);E[2].EvidencePaths:=Paths(['also.md','docs/else.md']);
  M:=TWfcAssetManifest.Create(E,ML);
  F:=TWfcAssetFileList.Create(Paths(['A.png','B.PNG','LICENSE','docs/evidence.txt','unused.zip']),FL);
  try
    R:=CheckWfcAssetInventory(M,F);Check(not R.MatchesSuppliedInventory,'all incomplete categories reported');
    Check((R.DocumentedCount=2) and (R.UnresolvedCount=1),'counts include orphan declarations');
    Check(Length(R.MissingRequiredEntries)=3,'complete missing required count');
    Check(R.MissingRequiredEntries[0]='A.png','case-different required asset stays missing');
    Check(R.MissingRequiredEntries[1]='B.PNG','missing report file-list order');
    Check(R.MissingRequiredEntries[2]='unused.zip','unmanifested archive reported without member inspection');
    Check(Length(R.OrphanManifestRows)=3,'all orphan declarations retained');
    Check(R.OrphanManifestRows[0]='a.png','exact-case mismatch produces orphan too');
    Check((R.OrphanManifestRows[1]='z.obj') and (R.OrphanManifestRows[2]='z.txt'),'orphan order follows manifest');
    Check(Length(R.MissingEvidencePaths)=4,'complete missing evidence count across assets');
    Check((R.MissingEvidencePaths[0].AssetPath='a.png') and (R.MissingEvidencePaths[0].EvidencePath='docs/missing.md'),'first missing reference');
    Check((R.MissingEvidencePaths[1].AssetPath='z.obj') and (R.MissingEvidencePaths[1].EvidencePath='docs/missing.md'),'repeated missing evidence keeps each owner');
    Check((R.MissingEvidencePaths[2].AssetPath='z.txt') and (R.MissingEvidencePaths[2].EvidencePath='also.md'),'third missing reference');
    Check(R.MissingEvidencePaths[3].EvidencePath='docs/else.md','evidence row order retained');
    S:=CheckWfcAssetInventory(M,F);R.MissingRequiredEntries[0]:='changed';
    R.OrphanManifestRows[0]:='changed';R.MissingEvidencePaths[0].EvidencePath:='changed';
    Check(S.MissingRequiredEntries[0]='A.png','report required arrays detached');
    Check(S.OrphanManifestRows[0]='a.png','report orphan arrays detached');
    Check(S.MissingEvidencePaths[0].EvidencePath='docs/missing.md','report evidence records detached');
    R:=CheckWfcAssetInventory(M,F);Check(R.MissingRequiredEntries[0]='A.png','report edits cannot change owner');
  finally F.Free;M.Free;end;
  Check(S.MissingEvidencePaths[1].AssetPath='z.obj','full report survives both owner lifetimes');
  SetLength(E,1);E[0]:=Entry('a.png');Documented(E[0]);E[0].EvidencePaths:=Paths(['LICENSE']);
  M:=TWfcAssetManifest.Create(E,ML);F:=TWfcAssetFileList.Create(Paths(['a.png','license']),FL);
  try
    R:=CheckWfcAssetInventory(M,F);Check(Length(R.MissingRequiredEntries)=0,'actual case-exact asset matched');
    Check(Length(R.OrphanManifestRows)=0,'no spurious orphan');
    Check((Length(R.MissingEvidencePaths)=1) and (R.MissingEvidencePaths[0].EvidencePath='LICENSE'),'evidence also matches exact case only');
  finally F.Free;M.Free;end;
  E[0]:=Entry('notes.txt');M:=TWfcAssetManifest.Create(E,ML);F:=TWfcAssetFileList.Create(Paths(['notes.txt']),FL);
  try Check(CheckWfcAssetInventory(M,F).MatchesSuppliedInventory,'additional non-asset suffix declaration allowed');finally F.Free;M.Free;end;
end;

{$IFDEF PAS2JS}
procedure RawManifestCase(const Which:Integer);
var E:TWfcAssetEntries;L:TWfcAssetManifestLimits;Reads:Integer;
begin
  { One fresh activation per case; never $assign into a prior null/plain record. }
  E:=One;L:=ML;Reads:=0;
  case Which of
    0:asm E=null;end;
    1:asm E={0:E[0],length:1};end;
    2:asm E=new Uint8Array(1);end;
    3:asm delete E[0];end;
    4:asm Object.defineProperty(E,'0',{get:function(){Reads++;throw new Error('slot');}});end;
    5:asm const p=Object.create(Array.prototype);p[0]=E[0];delete E[0];Object.setPrototypeOf(E,p);end;
    6:asm E[0]=null;end;
    7:asm E[0]=[];end;
    8:asm E[0]={};end;
    9:asm E[0].Path=1;end;
    10:asm E[0].ByteSizeText=9223372036854775807;end;
    11:asm E[0].SHA256=new String(E[0].SHA256);end;
    12:asm E[0].Review=0.5;end;
    13:asm E[0].Review='0';end;
    14:asm E[0].Origin={toString:function(){Reads++;return 'unknown';}};end;
    15:asm E[0].Author=undefined;end;
    16:asm E[0].Version=Symbol('v1');end;
    17:asm E[0].LicenseText=null;end;
    18:asm E[0].Modifications=true;end;
    19:asm Object.defineProperty(E[0],'EvidencePaths',{get:function(){Reads++;return [];}});end;
    20:asm Object.defineProperty(E[0],'Path',{get:function(){Reads++;return 'safe.png';}});end;
    21:asm E[0].EvidencePaths={0:'proof',length:1};end;
    22:asm delete E[0].EvidencePaths[0];end;
    23:asm Object.defineProperty(E[0].EvidencePaths,'0',{get:function(){Reads++;throw new Error('evidence');}});end;
    24:asm const p=Object.create(Array.prototype);p[0]='proof';delete E[0].EvidencePaths[0];Object.setPrototypeOf(E[0].EvidencePaths,p);end;
    25:asm E[0].EvidencePaths[0]={toString:function(){Reads++;return 'proof';}};end;
    26:asm L=null;end;
    27:asm L=[];end;
    28:asm L={};end;
    29:asm L.Version=NaN;end;
    30:asm L.MaxAssets=Infinity;end;
    31:asm L.MaxEvidencePaths='1';end;
    32:asm L.MaxEncodedBytes=1.5;end;
    33:asm Object.defineProperty(L,'MaxAssets',{get:function(){Reads++;return 1;}});end;
    34:asm const p={Version:1,MaxAssets:64,MaxEvidencePaths:128};Object.defineProperty(p,'MaxEncodedBytes',{get:function(){Reads++;return 65536;}});L=Object.create(p);end;
    35:asm E[0].Origin='\ud800';end;
    36:asm E[0].Path='caf\u00e9.png';end;
    37:asm E=new Array(2147483648);end;
    38:asm E[0].EvidencePaths=new Array(2147483648);end;
    39:asm E[0].Review=2;end;
  end;
  RejectManifest(E,L,'raw manifest '+IntToStr(Which));Check(Reads=0,'raw manifest getter/coercion never executed');
end;
procedure RawFilesCase(const Which:Integer);
var P:TWfcAssetPaths;L:TWfcAssetFileListLimits;Reads:Integer;
begin
  P:=Paths(['a.png']);L:=FL;Reads:=0;
  case Which of
    0:asm P=null;end;1:asm P={0:'a.png',length:1};end;
    2:asm P=new Uint8Array(1);end;3:asm delete P[0];end;
    4:asm Object.defineProperty(P,'0',{get:function(){Reads++;return 'a.png';}});end;
    5:asm const p=Object.create(Array.prototype);p[0]='a.png';delete P[0];Object.setPrototypeOf(P,p);end;
    6:asm P[0]=new String('a.png');end;
    7:asm P[0]={toString:function(){Reads++;return 'a.png';}};end;
    8:asm L=null;end;9:asm L=[];end;10:asm L.Version=true;end;
    11:asm L.MaxFiles=NaN;end;12:asm L.MaxEncodedBytes=Infinity;end;
    13:asm Object.defineProperty(L,'MaxFiles',{get:function(){Reads++;return 128;}});end;
    14:asm P=new Array(2147483648);end;
  end;
  RejectFiles(P,L,'raw file list '+IntToStr(Which));Check(Reads=0,'raw file getter/coercion never executed');
end;
procedure RawPositiveCase(const Which:Integer);
var E:TWfcAssetEntries;P,Q:TWfcAssetPaths;L:TWfcAssetManifestLimits;K:TWfcAssetFileListLimits;
  M:TWfcAssetManifest;F:TWfcAssetFileList;Reads:Integer;A:TWfcAssetEntry;
begin
  E:=One;P:=Paths(['assets/A file.png','docs/asset notes.md']);L:=ML;K:=FL;Reads:=0;
  case Which of
    0:asm
      Object.defineProperty(E,'slice',{get:function(){Reads++;throw new Error('caller slice');}});
      Object.defineProperty(E[0].EvidencePaths,'slice',{get:function(){Reads++;throw new Error('evidence slice');}});
      Object.defineProperty(P,'slice',{get:function(){Reads++;throw new Error('path slice');}});
    end;
    1:asm
      E.slice=function(){Reads++;return E;};E[0].EvidencePaths.slice=function(){Reads++;return this;};
      P.slice=function(){Reads++;return P;};
      E.sort=function(){Reads++;throw new Error('caller sort');};P.sort=E.sort;
    end;
    2:asm E.slice=null;E[0].EvidencePaths.slice=null;P.slice=null;end;
    3:asm Object.freeze(E[0].EvidencePaths);Object.freeze(E[0]);Object.freeze(E);Object.freeze(P);Object.freeze(L);Object.freeze(K);end;
    4:asm E[0]=Object.assign({},E[0]);L=Object.assign({},L);K=Object.assign({},K);end;
    5:asm E[0]=Object.create(Object.assign({},E[0]));L=Object.create(Object.assign({},L));K=Object.create(Object.assign({},K));end;
  end;
  M:=TWfcAssetManifest.Create(E,L);F:=nil;
  try
    F:=TWfcAssetFileList.Create(P,K);
    Check(EncodeWfcAssetManifest(M,ML)=ONE_MANIFEST,'positive raw canonical manifest');
    Check(EncodeWfcAssetFileList(F,FL)=TWO_FILES,'positive raw canonical file list');
    A:=M.EntryAt(0);Q:=F.CopyPaths;A.EvidencePaths[0]:='other.md';Q[0]:='other.png';
    Check(EncodeWfcAssetManifest(M,ML)=ONE_MANIFEST,'sanitized returned evidence copy');
    Check(EncodeWfcAssetFileList(F,FL)=TWO_FILES,'sanitized returned paths copy');
    if Which<>3 then begin
      asm E[0].Origin='changed';E[0].EvidencePaths[0]='changed.md';P[0]='changed.png';end;
      Check(EncodeWfcAssetManifest(M,ML)=ONE_MANIFEST,'caller array cannot alias retained manifest');
      Check(EncodeWfcAssetFileList(F,FL)=TWO_FILES,'caller array cannot alias retained paths');
    end;
    Check(Reads=0,'no caller-provided slice/sort/getter invoked');
  finally F.Free;M.Free;end;
end;
procedure RawTextCase(const Which:Integer);
var Text:String;Reads:Integer;
begin
  Reads:=0;
  asm Text=[null,undefined,NaN,1,true,[],{},new String('wfcfiles=1'),Symbol('text'),
    {toString:function(){Reads++;return 'wfcfiles=1';}}][Which];end;
  RejectText(Text,True,'raw manifest text is not coerced');
  RejectText(Text,False,'raw file-list text is not coerced');
  Check(Reads=0,'raw decoder never calls attacker conversion');
end;
procedure TestHostileJS;
var I,J,Bad:Integer;M:TWfcAssetManifest;F:TWfcAssetFileList;C,D,S:String;
begin
  for I:=0 to 39 do RawManifestCase(I);
  for I:=0 to 14 do RawFilesCase(I);
  for I:=0 to 5 do RawPositiveCase(I);
  for I:=0 to 9 do RawTextCase(I);
  M:=TWfcAssetManifest.Create(One,ML);F:=TWfcAssetFileList.Create(Paths(['a.png']),FL);
  try
    for I:=0 to 9 do begin
      asm Bad=[NaN,Infinity,-Infinity,0.5,'0',null,undefined,true,{},[]][I];end;
      for J:=0 to 1 do begin
        C:='';D:='';try if J=0 then M.EntryAt(Bad) else S:=F.PathAt(Bad);
        except on X:Exception do begin C:=X.ClassName;D:=X.Message;end;end;
        TypedError(C,D,'raw index validated before access');
      end;
    end;
  finally F.Free;M.Free;end;
end;
{$ENDIF}

begin
  TestCanonicalAndOwnership;TestLimits;TestPathsAndClassification;
  TestMetadataAndSizes;TestTextRefusals;TestInventoryReports;TestIndices;
  {$IFNDEF PAS2JS}TestNativeUtf8Bytes;{$ENDIF}
  {$IFDEF PAS2JS}TestHostileJS;{$ENDIF}
  WriteLn('Asset manifest checks: ',Checks,' passed');
end.
