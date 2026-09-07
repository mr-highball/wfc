{ SPDX-License-Identifier: MIT
  Copyright (c) 2021 mr-highball
  Shared CLI grammar and explicit raw-inventory conversion, without asset I/O. }
program wfc_asset_check_app_test;
{$mode delphi}{$H+}
uses {$IFDEF PAS2JS}wfc_browser_test_host, JS,{$ENDIF}
  SysUtils, wfc_asset_manifest, wfc_asset_check_app;

const
  EMPTY_FILES='wfcfiles=1'#10'files=0'#10;
  SORTED_FILES='wfcfiles=1'#10'files=4'#10+
    'file.0=A.png'#10'file.1=B.PNG'#10+
    'file.2=a%20folder%2Fz.svg'#10'file.3=notes.txt'#10;
var Checks:Integer;

procedure Check(const OK:Boolean;const Name:String);
begin Inc(Checks);if not OK then raise Exception.Create('asset app test: '+Name);end;
function Args(const Values:array of String):TWfcAssetArguments;
var I:Integer;
begin SetLength(Result,Length(Values));for I:=0 to High(Values) do Result[I]:=Values[I];end;
function ValidArgs:TWfcAssetArguments;
begin Result:=Args(['--root','release root','--manifest','assets.wfcassets','--files','release.wfcfiles']);end;
function Limits(const Files:Integer=100;const Bytes:Integer=65536):TWfcAssetFileListLimits;
begin Result.Version:=1;Result.MaxFiles:=Files;Result.MaxEncodedBytes:=Bytes;end;
procedure ErrorDetails(const Actual,Expected,Detail,Name:String);
var I:Integer;Safe:Boolean;
begin
  Check(Actual=Expected,Name+' expected '+Expected+', got '+Actual);
  Check((Length(Detail)>0) and (Length(Detail)<=512),Name+' nonempty bounded message');
  Safe:=True;for I:=1 to Length(Detail) do if not (Ord(Detail[I]) in [32..126]) then Safe:=False;
  Check(Safe,Name+' printable one-line diagnostic');
end;
procedure RejectArgs(const A:TWfcAssetArguments;const Name:String);
var C:TWfcAssetCommand;ClassName,Detail:String;Returned:Boolean;
begin
  ClassName:='';Detail:='';Returned:=False;
  try C:=ParseWfcAssetCommand(A);Returned:=True;
  except on E:Exception do begin ClassName:=E.ClassName;Detail:=E.Message;end;end;
  ErrorDetails(ClassName,'EWfcAssetUsage',Detail,Name);Check(not Returned,Name+' no returned command');
end;
procedure RejectRaw(const Text:String;const L:TWfcAssetFileListLimits;const Name:String);
var Output,ClassName,Detail:String;
begin
  Output:='unchanged sentinel';ClassName:='';Detail:='';
  try Output:=ConvertWfcAssetPathLines(Text,L);
  except on E:Exception do begin ClassName:=E.ClassName;Detail:=E.Message;end;end;
  ErrorDetails(ClassName,'EWfcAssetManifest',Detail,Name);
  Check(Output='unchanged sentinel',Name+' no partial encoded return');
end;
procedure CheckDefaults(const C:TWfcAssetCommand);
begin
  Check(C.ManifestLimits.Version=1,'default manifest version');
  Check(C.ManifestLimits.MaxAssets=100000,'default manifest asset allowance');
  Check(C.ManifestLimits.MaxEvidencePaths=1000000,'default total evidence allowance');
  Check(C.ManifestLimits.MaxEncodedBytes=1048576,'default manifest byte allowance');
  Check(C.FileListLimits.Version=1,'default file-list version');
  Check(C.FileListLimits.MaxFiles=1000000,'default file-list count allowance');
  Check(C.FileListLimits.MaxEncodedBytes=33554432,'default file-list byte allowance');
end;
procedure TestModes;
var A:TWfcAssetArguments;C,D:TWfcAssetCommand;Help:String;
begin
  C:=ParseWfcAssetCommand(Args(['--help']));Check(C.Kind=wackHelp,'help is its own mode');
  CheckDefaults(C);Check((C.RootPath='') and (C.ManifestPath='') and (C.FileListPath='') and
    (C.RawInventoryPath='') and not C.RequireReviewed,'help has no accidental input paths');
  C:=ParseWfcAssetCommand(Args(['--version']));Check(C.Kind=wackVersion,'version is its own mode');
  CheckDefaults(C);
  A:=ValidArgs;C:=ParseWfcAssetCommand(A);Check(C.Kind=wackCheck,'three explicit inputs select check');
  Check((C.RootPath='release root') and (C.ManifestPath='assets.wfcassets') and
    (C.FileListPath='release.wfcfiles') and (C.RawInventoryPath=''),'check preserves all paths');
  Check(not C.RequireReviewed,'review requirement is not silently selected');CheckDefaults(C);
  A[1]:='changed root';A[3]:='changed manifest';A[5]:='changed inventory';
  Check(C.RootPath='release root','command strings detached from argument vector');
  Check((C.ManifestPath='assets.wfcassets') and (C.FileListPath='release.wfcfiles'),'all command paths detached');
  D:=C;D.ManifestLimits.MaxAssets:=0;D.FileListLimits.MaxFiles:=0;
  Check((C.ManifestLimits.MaxAssets=100000) and (C.FileListLimits.MaxFiles=1000000),'nested command limits copy independent');
  C:=ParseWfcAssetCommand(Args(['--require-reviewed','--files','f','--root','r','--manifest','m']));
  Check(C.Kind=wackCheck,'order-independent check grammar');Check(C.RequireReviewed,'explicit require-reviewed');
  C:=ParseWfcAssetCommand(Args(['--inventory-from','raw paths.txt']));
  Check(C.Kind=wackInventory,'inventory conversion is disjoint mode');
  Check((C.RawInventoryPath='raw paths.txt') and (C.RootPath='') and (C.ManifestPath='') and
    (C.FileListPath='') and not C.RequireReviewed,'conversion has only explicit raw input');CheckDefaults(C);
  C:=ParseWfcAssetCommand(Args(['--max-files','17','--inventory-from','list','--max-file-list-bytes','999']));
  Check((C.Kind=wackInventory) and (C.FileListLimits.MaxFiles=17) and
    (C.FileListLimits.MaxEncodedBytes=999),'conversion adjustable policy');
  C:=ParseWfcAssetCommand(Args(['--root','D:\Folder With Spaces','--manifest','M file',
    '--files','/tmp/files','--max-assets','0','--max-evidence-paths','1',
    '--max-manifest-bytes','2147483647','--max-files','2','--max-file-list-bytes','3']));
  Check(C.RootPath='D:\Folder With Spaces','OS root is an opaque argument, not an asset-relative path');
  Check((C.ManifestLimits.MaxAssets=0) and (C.ManifestLimits.MaxEvidencePaths=1) and
    (C.ManifestLimits.MaxEncodedBytes=High(Integer)),'check accepts exact explicit manifest policy');
  Check((C.FileListLimits.MaxFiles=2) and (C.FileListLimits.MaxEncodedBytes=3),'check accepts independent file policy');
  C:=ParseWfcAssetCommand(Args(['--inventory-from','raw','--max-files','0','--max-file-list-bytes','0']));
  Check((C.FileListLimits.MaxFiles=0) and (C.FileListLimits.MaxEncodedBytes=0),'zero allowance parses; conversion validates actual needed bytes');
  C:=ParseWfcAssetCommand(Args(['--root','./--release','--manifest','./--manifest','--files','/tmp/--files']));
  Check((C.RootPath='./--release') and (C.ManifestPath='./--manifest') and
    (C.FileListPath='/tmp/--files'),'explicit path prefix permits a dash-leading filename');
  C:=ParseWfcAssetCommand(Args(['--inventory-from','./--raw-list']));
  Check(C.RawInventoryPath='./--raw-list','conversion accepts explicitly prefixed dash-leading filename');
  Help:=WfcAssetCommandHelp;
  Check((Pos('--root DIR --manifest FILE --files FILE',Help)>0) and
    (Pos('--inventory-from FILE',Help)>0),'help names both actual modes');
  Check((Pos('All are adjustable',Help)>0) and (Pos('1048576',Help)>0) and
    (Pos('33554432',Help)>0),'help documents operational allowances');
  Check((Pos('not ownership',Help)>0) and (Pos('not legal proof',Help)>0) and
    (Pos('supplied inventory determines scope',Help)>0),'help preserves inventory and review non-claims');
  Check((Length(Help)>0) and (Help[Length(Help)]=#10) and (Pos(#13,Help)=0),'help has complete LF text');
end;

procedure TestArgumentRefusals;
const OptionNames:array[0..8] of String=('--root','--manifest','--files','--inventory-from',
  '--max-manifest-bytes','--max-file-list-bytes','--max-assets','--max-files','--max-evidence-paths');
  NumericNames:array[0..4] of String=('--max-manifest-bytes','--max-file-list-bytes',
    '--max-assets','--max-files','--max-evidence-paths');
  BadDecimals:array[0..13] of String=('','00','01','-1','+1',' 1','1 ','1.0','1e3',
    '2147483648','999999999999999999999','1'+#10,'0x10','-0');
var I,J:Integer;A:TWfcAssetArguments;
begin
  RejectArgs(nil,'no arguments');RejectArgs(Args(['--help','--version']),'help/version cannot combine');
  RejectArgs(Args(['--help','extra']),'help has no trailing operands');
  RejectArgs(Args(['--version','extra']),'version has no trailing operands');
  RejectArgs(Args(['--unknown']),'unknown option');RejectArgs(Args(['--']),'unsupported delimiter');
  RejectArgs(Args(['--root=r','--manifest','m','--files','f']),'unsupported equals syntax');
  RejectArgs(Args(['--ROOT','r','--manifest','m','--files','f']),'option names are exact case');
  RejectArgs(Args(['--manifest','m','--files','f']),'missing root');
  RejectArgs(Args(['--root','r','--files','f']),'missing manifest');
  RejectArgs(Args(['--root','r','--manifest','m']),'missing files');
  for I:=0 to 2 do begin
    A:=ValidArgs;A[1+I*2]:='';RejectArgs(A,'empty required check input '+IntToStr(I));
  end;
  RejectArgs(Args(['--inventory-from','']),'empty raw inventory filename');
  RejectArgs(Args(['--require-reviewed']),'review flag does not supply a mode');
  RejectArgs(Args(['--root','r','--manifest','m','--files','f','--require-reviewed','true']),'review flag accepts no separate boolean operand');
  RejectArgs(Args(['--root','r','--manifest','m','--files','f','--require-reviewed','--require-reviewed']),'duplicate review flag');
  RejectArgs(Args(['--inventory-from','raw','--inventory-from','raw']),'duplicate conversion selector');
  for I:=0 to High(OptionNames) do begin
    A:=ValidArgs;SetLength(A,7);A[6]:=OptionNames[I];RejectArgs(A,'missing trailing value '+OptionNames[I]);
    A:=ValidArgs;SetLength(A,8);A[6]:=OptionNames[I];A[7]:='';RejectArgs(A,'empty option value '+OptionNames[I]);
  end;
  for I:=0 to 2 do begin
    A:=ValidArgs;SetLength(A,8);A[6]:=OptionNames[I];A[7]:='again';RejectArgs(A,'duplicate check input '+OptionNames[I]);
  end;
  for I:=0 to High(NumericNames) do begin
    A:=ValidArgs;SetLength(A,10);A[6]:=NumericNames[I];A[7]:='1';A[8]:=NumericNames[I];A[9]:='2';
    RejectArgs(A,'duplicate numeric policy '+NumericNames[I]);
    for J:=0 to High(BadDecimals) do begin
      A:=ValidArgs;SetLength(A,8);A[6]:=NumericNames[I];A[7]:=BadDecimals[J];
      RejectArgs(A,'canonical integer refusal '+IntToStr(I)+'/'+IntToStr(J));
    end;
  end;
  RejectArgs(Args(['--inventory-from','raw','--root','r']),'conversion forbids root');
  RejectArgs(Args(['--inventory-from','raw','--manifest','m']),'conversion forbids manifest');
  RejectArgs(Args(['--inventory-from','raw','--files','f']),'conversion forbids canonical input');
  RejectArgs(Args(['--inventory-from','raw','--require-reviewed']),'conversion forbids review requirement');
  RejectArgs(Args(['--inventory-from','raw','--max-assets','1']),'conversion forbids asset policy');
  RejectArgs(Args(['--inventory-from','raw','--max-evidence-paths','1']),'conversion forbids evidence policy');
  RejectArgs(Args(['--inventory-from','raw','--max-manifest-bytes','1']),'conversion forbids manifest bytes');
  RejectArgs(Args(['--root','r','--manifest','m','--files','f','--inventory-from','raw']),'complete check cannot be mixed with conversion');
  RejectArgs(Args(['--inventory-from','raw','--help']),'help is not an in-mode flag');
  RejectArgs(Args(['--root','r','--manifest','m','--files','f','--version']),'version is not an in-mode flag');
  RejectArgs(Args(['--root','--help','--manifest','m','--files','f']),'option-looking root value is refused');
  RejectArgs(Args(['--root','r','--manifest','--help','--files','f']),'option-looking manifest value is refused');
  RejectArgs(Args(['--root','r','--manifest','m','--files','--version']),'option-looking file-list value is refused');
  RejectArgs(Args(['--inventory-from','--raw-list']),'option-looking raw filename is refused');
  RejectArgs(Args(['--unknown'+#27+#10+'secret']),'unknown option never echoed unsafely');
end;

procedure TestRawConversion;
var S,T:String;L:TWfcAssetFileListLimits;F:TWfcAssetFileList;I,J,N:Integer;
begin
  Check(ConvertWfcAssetPathLines('',Limits(0,Length(EMPTY_FILES)))=EMPTY_FILES,'empty raw inventory canonical complete output');
  S:='notes.txt'#10'a folder/z.svg'#10'B.PNG'#10'A.png';
  Check(ConvertWfcAssetPathLines(S,Limits)=SORTED_FILES,'independently expected exact ordinal output');
  Check(ConvertWfcAssetPathLines(S+#10,Limits)=SORTED_FILES,'optional final LF');
  Check(ConvertWfcAssetPathLines(StringReplace(S,#10,#13#10,[rfReplaceAll]),Limits)=SORTED_FILES,'CRLF without final delimiter');
  Check(ConvertWfcAssetPathLines(StringReplace(S,#10,#13#10,[rfReplaceAll])+#13#10,Limits)=SORTED_FILES,'complete CRLF input');
  Check(ConvertWfcAssetPathLines('notes.txt'#13#10'a folder/z.svg'#10'B.PNG'#13#10'A.png'#10,Limits)=SORTED_FILES,'each actual LF/CRLF delimiter accepted');
  Check(ConvertWfcAssetPathLines(S,Limits(4,Length(SORTED_FILES)))=SORTED_FILES,'exact count and complete encoded byte allowance');
  RejectRaw(S,Limits(3),'one less raw file count');
  RejectRaw(S,Limits(4,Length(SORTED_FILES)-1),'one less complete encoded byte allowance');
  RejectRaw('',Limits(0,Length(EMPTY_FILES)-1),'empty output still needs its envelope allowance');
  RejectRaw(S,Limits(4,Length(S)-1),'raw byte allowance applies before conversion');
  for I:=0 to 3 do begin
    L:=Limits;case I of 0:L.Version:=0;1:L.Version:=2;2:L.MaxFiles:=-1;3:L.MaxEncodedBytes:=-1;end;
    RejectRaw('',L,'empty input still validates complete policy '+IntToStr(I));
  end;
  Check(ConvertWfcAssetPathLines('UPPER.PNG',Limits)='wfcfiles=1'#10'files=1'#10'file.0=UPPER.PNG'#10,'case is preserved, never normalized');
  Check(ConvertWfcAssetPathLines('folder/a b_c.txt',Limits)='wfcfiles=1'#10'files=1'#10'file.0=folder%2Fa%20b_c.txt'#10,'path percent envelope is exact');
  { Exercise merge widths across powers of two using an independent numbered
    expected inventory, not the production sort as its own oracle. }
  for J:=0 to 6 do begin
    case J of 0:N:=1;1:N:=2;2:N:=3;3:N:=7;4:N:=8;5:N:=9;else N:=17;end;
    S:='';T:='wfcfiles=1'#10'files='+IntToStr(N)+#10;
    for I:=N-1 downto 0 do S:=S+'p'+IntToStr(100+I)+'.png'+#10;
    for I:=0 to N-1 do T:=T+'file.'+IntToStr(I)+'=p'+IntToStr(100+I)+'.png'+#10;
    Check(ConvertWfcAssetPathLines(S,Limits)=T,'complete independent merge boundary '+IntToStr(N));
    F:=DecodeWfcAssetFileList(T,Limits);
    try Check(F.Count=N,'converter output accepted by separate canonical decoder');finally F.Free;end;
  end;
end;

procedure TestRawRefusals;
var Bad:TWfcAssetArguments;I:Integer;S:String;
begin
  Bad:=Args([#10,#13#10,'a.png'#10#10,#10'a.png','a.png'#13,'a.png'#13'b.png',
    'a.png'#10'a.png','a.png'#13#10'a.png'#13#10,'a.png'#10'B.png'#10'A.png',
    ' ','.','..','./a.png','../a.png','/a.png','a//b.png','a/','a/../b.png',
    'a\b.png','C:/a.png','a:b.png','a?b.png','a*b.png','a|b.png','a~b.png',
    'a.png ','a.png.','a /b.png','CON','con.PNG','COM0.bin','LPT9.png',
    'CON .png','COM1 .txt','a'+#9+'.png','a'+#27+'.png','a'+#127+'.png','a'+#0+'.png']);
  for I:=0 to High(Bad) do RejectRaw(Bad[I],Limits,'raw path/line refusal '+IntToStr(I));
  {$IFDEF PAS2JS}S:='caf'+#$E9+'.png';{$ELSE}S:='caf'+#$C3#$A9+'.png';{$ENDIF}
  RejectRaw(S,Limits,'raw paths are explicitly ASCII, not Unicode');
  {$IFDEF PAS2JS}S:=#$FEFF;{$ELSE}S:=#$EF#$BB#$BF;{$ENDIF}
  RejectRaw(S+'a.png',Limits,'raw BOM rejected');
  Check(ConvertWfcAssetPathLines('a .png',Limits)='wfcfiles=1'#10'files=1'#10'file.0=a%20.png'#10,'ordinary nondevice spaced basename is preserved');
  Check(ConvertWfcAssetPathLines('COM10.png',Limits)='wfcfiles=1'#10'files=1'#10'file.0=COM10.png'#10,'device profile does not reject COM10');
end;

procedure TestDiagnostics;
var S,T:String;I:Integer;Safe:Boolean;
begin
  Check(WfcAssetOneLine('')='','empty diagnostic');
  Check(WfcAssetOneLine('ordinary / - " quoted')='ordinary / - " quoted','printable ASCII retained');
  Check(WfcAssetOneLine(#0#9#10#13#27#127)='\x00\x09\x0A\x0D\x1B\x7F','exact terminal control escaping');
  Check(WfcAssetOneLine(StringOfChar('x',509))=StringOfChar('x',509),'exact no-truncation boundary');
  Check(WfcAssetOneLine(StringOfChar('x',510))=StringOfChar('x',509)+'...','bounded truncation marker');
  Check(WfcAssetOneLine(StringOfChar('x',508)+#10)=StringOfChar('x',508)+'...','truncation never splits an escape');
  {$IFDEF PAS2JS}S:='caf'+#$E9+' '+#$D83C#$DFB5;{$ELSE}S:='caf'+#$C3#$A9+' '+#$F0#$9F#$8E#$B5;{$ENDIF}
  T:=WfcAssetOneLine(S);Safe:=True;
  for I:=1 to Length(T) do if not (Ord(T[I]) in [32..126]) then Safe:=False;
  Check(Safe and (Length(T)<=512),'Unicode diagnostic becomes bounded printable ASCII');
  Check((Pos('caf',T)=1) and (Pos('\x',T)>0),'Unicode diagnostic is visibly escaped, not silently lost');
  T:=WfcAssetOneLine(StringOfChar(#1,10000));
  Check((Length(T)<=512) and (Copy(T,Length(T)-2,3)='...'),'large unsafe diagnostic bounded');
end;

{$IFDEF PAS2JS}
procedure RawArgumentsCase(const Which:Integer);
var A:TWfcAssetArguments;Reads:Integer;
begin
  A:=ValidArgs;Reads:=0;
  case Which of
    0:asm A=null;end;
    1:asm A={0:'--help',length:1};end;
    2:asm A='--help';end;
    3:asm A=new Uint8Array(2);end;
    4:asm delete A[1];end;
    5:asm Object.defineProperty(A,'0',{get:function(){Reads++;return '--root';}});end;
    6:asm Object.defineProperty(A,'1',{get:function(){Reads++;return 'r';}});end;
    7:asm const p=Object.create(Array.prototype);p[0]='--root';delete A[0];Object.setPrototypeOf(A,p);end;
    8:asm A[1]=undefined;end;
    9:asm A[1]=null;end;
    10:asm A[1]=1;end;
    11:asm A[1]=true;end;
    12:asm A[1]=[];end;
    13:asm A[1]={};end;
    14:asm A[1]=new String('r');end;
    15:asm A[1]=Symbol('r');end;
    16:asm A[0]={toString:function(){Reads++;return '--root';}};end;
    17:asm A[1]={toString:function(){Reads++;return 'r';}};end;
    18:asm A=['--max-files',NaN,'--inventory-from','raw'];end;
    19:asm A=['--max-files',Infinity,'--inventory-from','raw'];end;
    20:asm A=['--max-files',0,'--inventory-from','raw'];end;
    21:asm A=new Array(2147483648);end;
  end;
  RejectArgs(A,'raw command boundary '+IntToStr(Which));
  Check(Reads=0,'command rejects passive-shape violations without getter/coercion');
end;
procedure RawConversionCase(const Which:Integer);
var Text:String;L:TWfcAssetFileListLimits;Reads:Integer;
begin
  Text:='a.png';L:=Limits;Reads:=0;
  case Which of
    0:asm Text=null;end;1:asm Text=undefined;end;2:asm Text=12;end;
    3:asm Text=[];end;4:asm Text=new String('a.png');end;
    5:asm Text={toString:function(){Reads++;return 'a.png';}};end;
    6:asm Text={};Object.defineProperty(Text,'length',{get:function(){Reads++;return 1;}});end;
    7:asm Text=Symbol('paths');end;
    8:asm L=null;end;9:asm L=[];end;10:asm L={};end;
    11:asm L.MaxFiles=NaN;end;12:asm L.MaxEncodedBytes='100';end;
    13:asm Object.defineProperty(L,'MaxFiles',{get:function(){Reads++;return 100;}});end;
    14:asm const p={Version:1,MaxFiles:100};Object.defineProperty(p,'MaxEncodedBytes',{get:function(){Reads++;return 65536;}});L=Object.create(p);end;
  end;
  RejectRaw(Text,L,'raw conversion boundary '+IntToStr(Which));
  Check(Reads=0,'conversion rejects malformed input without getter/coercion');
end;
procedure RawPositiveCase(const Which:Integer);
var A:TWfcAssetArguments;C:TWfcAssetCommand;L:TWfcAssetFileListLimits;Reads:Integer;
begin
  A:=ValidArgs;L:=Limits;Reads:=0;
  case Which of
    0:asm Object.defineProperty(A,'slice',{get:function(){Reads++;throw new Error('slice');}});end;
    1:asm A.slice=function(){Reads++;return A;};A.sort=function(){Reads++;throw new Error('sort');};end;
    2:asm Object.freeze(A);Object.freeze(L);end;
    3:asm L=Object.create(Object.assign({},L));end;
  end;
  C:=ParseWfcAssetCommand(A);
  Check((C.Kind=wackCheck) and (C.RootPath='release root'),'valid passive/frozen arguments accepted');
  Check(ConvertWfcAssetPathLines('B.PNG'#10'A.png',L)='wfcfiles=1'#10'files=2'#10'file.0=A.png'#10'file.1=B.PNG'#10,'valid passive/frozen policy accepted');
  if Which<>2 then begin A[1]:='changed';Check(C.RootPath='release root','raw argument container not retained');end;
  Check(Reads=0,'parser does not dispatch caller slice/sort');
end;
procedure RawDiagnosticCase(const Which:Integer);
var Text,Output:String;Reads:Integer;
begin
  Reads:=0;
  asm Text=[null,undefined,NaN,Infinity,1,true,[],{},new String('text'),Symbol('text'),
    {toString:function(){Reads++;return 'text';}},
    Object.defineProperty({},'length',{get:function(){Reads++;throw new Error('length');}})][Which];end;
  Output:=WfcAssetOneLine(Text);
  Check(Output='[invalid diagnostic text]','diagnostic raw type uses exact safe fallback');
  Check(Reads=0,'diagnostic fallback never invokes getter/coercion');
end;
procedure TestHostileJS;
var I:Integer;
begin
  for I:=0 to 21 do RawArgumentsCase(I);
  for I:=0 to 14 do RawConversionCase(I);
  for I:=0 to 3 do RawPositiveCase(I);
  for I:=0 to 11 do RawDiagnosticCase(I);
end;
{$ENDIF}

begin
  TestModes;TestArgumentRefusals;TestRawConversion;TestRawRefusals;TestDiagnostics;
  {$IFDEF PAS2JS}TestHostileJS;{$ENDIF}
  WriteLn('Asset check app checks: ',Checks,' passed');
end.
