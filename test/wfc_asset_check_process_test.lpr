{ SPDX-License-Identifier: MIT
  Copyright (c) 2021 mr-highball
  Actual native child processes, independent asset bytes and canonical texts.
  This harness retains its explicitly requested fresh fixture directory. }
program wfc_asset_check_process_test;
{$mode delphi}{$H+}
{$IFDEF PAS2JS}{$FATAL asset filesystem process tests require native FPC}{$ENDIF}

uses Classes, SysUtils, Process, Pipes, wfc_process_test_support, wfc_browser_socket,
  {$IFDEF MSWINDOWS}Windows{$ELSE}BaseUnix{$ENDIF};

const
  ABC_HASH='BA7816BF8F01CFEA414140DE5DAE2223B00361A396177A9CB410FF61F20015AD';
  EMPTY_HASH='E3B0C44298FC1C149AFBF4C8996FB92427AE41E4649B934CA495991B7852B855';
  ZERO_HASH='0000000000000000000000000000000000000000000000000000000000000000';
  EMPTY_MANIFEST='wfcassets=1'#10'assets=0'#10;
  EMPTY_FILES='wfcfiles=1'#10'files=0'#10;
  ONE_FILE='wfcfiles=1'#10'files=1'#10'file.0=assets%2Fa.png'#10;
  TWO_FILES='wfcfiles=1'#10'files=2'#10+
    'file.0=assets%2Fa.png'#10'file.1=docs%2Fproof.txt'#10;
  PROOF_TEXT='Fixture declaration only; not an independent legal opinion.'#10;
  OUTPUT_LIMIT=65536;
  PROCESS_TIMEOUT=5000;

type
  TOwnedInput=record Path,Bytes:String; end;
  TOwnedInputs=array of TOwnedInput;
  TStringArray=array of String;
var
  Checker,FixtureParent,Elsewhere,LogRoot,CaseBase,Repo,ManifestPath,FilesPath:String;
  Inputs:TOwnedInputs;
  Directories,AllowedPaths:TStringArray;
  Checks,Cases,CaseSequence,LinkRuns,LinkSkips:Integer;
  LastOut,LastError:String;

procedure Check(const Condition:Boolean; const Message:String);
begin
  Inc(Checks);
  if not Condition then raise Exception.Create(Message);
end;

function InsideOwned(const Path:String):String;
begin
  Result:=ExpandFileName(Path);
  if Pos(IncludeTrailingPathDelimiter(FixtureParent),Result)<>1 then
    raise Exception.Create('fixture operation escaped the explicit fresh parent');
end;

procedure AddAllowed(const Path:String);
var N:Integer;
begin N:=Length(AllowedPaths);SetLength(AllowedPaths,N+1);AllowedPaths[N]:=Path;end;

procedure MakeDirectory(const Path:String);
var P:String;N:Integer;
begin
  P:=InsideOwned(Path);
  Check(CreateDir(P),'create exact fresh fixture directory');
  N:=Length(Directories);SetLength(Directories,N+1);Directories[N]:=P;
  AddAllowed(P);
end;

procedure WriteOwned(const Path,Bytes:String);
var Stream:TFileStream;P:String;N:Integer;
begin
  P:=InsideOwned(Path);
  Check(not FileExists(P) and not DirectoryExists(P),'fixture writes are exclusive to new paths');
  Stream:=TFileStream.Create(P,fmCreate);
  try if Bytes<>'' then Stream.WriteBuffer(Bytes[1],Length(Bytes));
  finally Stream.Free;end;
  N:=Length(Inputs);SetLength(Inputs,N+1);Inputs[N].Path:=P;Inputs[N].Bytes:=Bytes;
  AddAllowed(P);
end;

function ReadOwned(const Path:String):String;
var Stream:TFileStream;
begin
  Stream:=TFileStream.Create(InsideOwned(Path),fmOpenRead or fmShareDenyWrite);
  try
    Check((Stream.Size>=0) and (Stream.Size<=2097152),'bounded fixture read');
    SetLength(Result,Stream.Size);
    if Result<>'' then Stream.ReadBuffer(Result[1],Length(Result));
  finally Stream.Free;end;
end;

function IsAllowed(const Path:String):Boolean;
var I:Integer;
begin
  for I:=0 to High(AllowedPaths) do if AllowedPaths[I]=Path then Exit(True);
  Result:=False;
end;

procedure CheckOwnedUnchanged;
var I,N,Code:Integer;Search:TSearchRec;Path:String;
begin
  for I:=0 to High(Inputs) do
    Check(ReadOwned(Inputs[I].Path)=Inputs[I].Bytes,'complete input bytes remain unchanged');
  { Enumerate only directories explicitly created by this harness. Never
    recursively follow newly discovered directories or symbolic links. }
  for I:=0 to High(Directories) do
  begin
    N:=0;
    Code:=FindFirst(IncludeTrailingPathDelimiter(Directories[I])+'*',faAnyFile,Search);
    if Code=0 then
    try
      repeat
        Inc(N);Check(N<=512,'bounded owned-directory entries');
        if (Search.Name<>'.') and (Search.Name<>'..') then
        begin
          Path:=IncludeTrailingPathDelimiter(Directories[I])+Search.Name;
          Check(IsAllowed(Path),'checker created an unrequested filesystem artifact');
        end;
        Code:=FindNext(Search);
      until Code<>0;
    finally SysUtils.FindClose(Search);end;
  end;
end;

procedure Drain(const Pipe:TInputPipeStream; var Text:String);
var Buffer:array[0..4095] of Byte;Available,N,OldLength:Integer;
begin
  Available:=Pipe.NumBytesAvailable;
  while Available>0 do
  begin
    if Available>SizeOf(Buffer) then Available:=SizeOf(Buffer);
    N:=Pipe.Read(Buffer[0],Available);
    if N<=0 then raise Exception.Create('child pipe did not make progress');
    if N>OUTPUT_LIMIT-Length(Text) then raise Exception.Create('child output exceeds fixture bound');
    OldLength:=Length(Text);SetLength(Text,OldLength+N);Move(Buffer[0],Text[OldLength+1],N);
    Available:=Pipe.NumBytesAvailable;
  end;
end;

function WaitForOwnedExit(const Child:TProcess;const Milliseconds:DWord):Boolean;
var Started,Current:QWord;
begin
  Started:=WfcBrowserTickCount64;
  repeat
    if not Child.Running then Exit(True);
    Current:=WfcBrowserTickCount64;
    if (Current<Started) or (Current-Started>=Milliseconds) then Break;
    Sleep(1);
  until False;
  Result:=not Child.Running;
end;

procedure RunCheck(const Arguments:array of String;const ExpectedExit:Integer;
  const Name:String;const ExpectedOutput:String='';const ExactOutput:Boolean=False);
var Child:TProcess;I,ActualExit:Integer;Started,Current:QWord;Before,LogPrefix,ArgumentText:String;
begin
  Inc(Cases);Before:=GetCurrentDir;LastOut:='';LastError:='';
  CheckOwnedUnchanged;
  ActualExit:=-1;LogPrefix:=LogRoot+DirectorySeparator+IntToStr(Cases);
  ArgumentText:=Checker+#10;
  for I:=0 to High(Arguments) do ArgumentText:=ArgumentText+Arguments[I]+#10;
  WriteOwned(LogPrefix+'.argv',ArgumentText);
  Child:=TProcess.Create(nil);
  try
    Child.Executable:=Checker;Child.CurrentDirectory:=Elsewhere;
    for I:=0 to High(Arguments) do Child.Parameters.Add(Arguments[I]);
    Child.Options:=[poUsePipes,poNoConsole];Child.Execute;Child.CloseInput;
    Started:=WfcBrowserTickCount64;
    while Child.Running do
    begin
      Drain(Child.Output,LastOut);Drain(Child.Stderr,LastError);
      Current:=WfcBrowserTickCount64;
      if (Current<Started) or (Current-Started>=PROCESS_TIMEOUT) then
        raise Exception.Create(Name+': checker exceeded bounded process deadline');
      Sleep(1);
    end;
    Drain(Child.Output,LastOut);Drain(Child.Stderr,LastError);
    ActualExit:=WfcProcessExitCode(Child);
    Check(ActualExit=ExpectedExit,Name+': exit mismatch; stderr='+LastError);
    Check((Pos(#13,LastOut)=0) and (Pos(#13,LastError)=0),Name+': exact LF output');
    if ExpectedExit in [0,1] then
      Check(LastError='',Name+': normal verdict has no exception diagnostic')
    else
    begin
      Check((Pos('wfc_asset_check: ',LastError)=1) and (Length(LastError)<=550),Name+': bounded host diagnostic');
      Check(Pos('byte-inventory-check=passed',LastOut)=0,Name+': refused check never publishes success');
      for I:=1 to Length(LastError)-1 do
        Check(Ord(LastError[I]) in [32..126],Name+': diagnostic is terminal-safe');
    end;
    if ExactOutput then Check(LastOut=ExpectedOutput,Name+': complete exact stdout')
    else if ExpectedOutput<>'' then Check(Pos(ExpectedOutput,LastOut)>0,Name+': required actual report');
    if ExpectedExit=1 then Check(Pos('byte-inventory-check=passed',LastOut)=0,Name+': mismatch is not success');
    Check(GetCurrentDir=Before,Name+': caller current directory unchanged');
    CheckOwnedUnchanged;
    WriteLn('Asset process case ',Cases,': ',Name,' [exit ',ExpectedExit,']');
  finally
    try
      if Child.Running then
      begin
        {$IFDEF MSWINDOWS}Child.Terminate(1);{$ELSE}fpKill(Child.ProcessID,SIGKILL);{$ENDIF}
        Check(WaitForOwnedExit(Child,PROCESS_TIMEOUT),'only owned timed-out child terminated');
      end;
    finally
      Child.Free;
      WriteOwned(LogPrefix+'.stdout',LastOut);
      WriteOwned(LogPrefix+'.stderr',LastError);
      WriteOwned(LogPrefix+'.exit',IntToStr(ActualExit)+#10);
    end;
  end;
end;

function AssetManifest(const EncodedPath,SizeText,Hash,Review:String;
  const Evidence:Boolean):String;
var Meta:String;
begin
  if Review='documented' then Meta:='declared' else Meta:='unknown';
  Result:='wfcassets=1'#10'assets=1'#10+
    'asset.0.path='+EncodedPath+#10+'asset.0.bytes='+SizeText+#10+
    'asset.0.sha256='+Hash+#10+'asset.0.review='+Review+#10+
    'asset.0.origin='+Meta+#10+'asset.0.author='+Meta+#10+
    'asset.0.version='+Meta+#10+'asset.0.license='+Meta+#10+
    'asset.0.modifications='+Meta+#10;
  if Evidence then Result:=Result+'asset.0.evidence.count=1'#10'asset.0.evidence.0=docs%2Fproof.txt'#10
  else Result:=Result+'asset.0.evidence.count=0'#10;
end;

function BaseManifest:String;
begin Result:=AssetManifest('assets%2Fa.png','3',ABC_HASH,'unresolved',True);end;

procedure NewCase(const ManifestText,FileText,AssetBytes:String;
  const MakeAsset:Boolean=True;const MakeEvidence:Boolean=True);
begin
  Inc(CaseSequence);
  CaseBase:=FixtureParent+DirectorySeparator+'case-'+IntToStr(CaseSequence);
  MakeDirectory(CaseBase);Repo:=CaseBase+DirectorySeparator+'release with spaces';MakeDirectory(Repo);
  MakeDirectory(Repo+DirectorySeparator+'assets');MakeDirectory(Repo+DirectorySeparator+'docs');
  ManifestPath:=CaseBase+DirectorySeparator+'assets.manifest';FilesPath:=CaseBase+DirectorySeparator+'files.inventory';
  WriteOwned(ManifestPath,ManifestText);WriteOwned(FilesPath,FileText);
  if MakeAsset then WriteOwned(Repo+DirectorySeparator+'assets'+DirectorySeparator+'a.png',AssetBytes);
  if MakeEvidence then WriteOwned(Repo+DirectorySeparator+'docs'+DirectorySeparator+'proof.txt',PROOF_TEXT);
end;

procedure CheckRelease(const Expected:Integer;const Name,Output:String;
  const Reviewed:Boolean=False);
begin
  if Reviewed then RunCheck(['--root',Repo,'--manifest',ManifestPath,'--files',FilesPath,'--require-reviewed'],Expected,Name,Output)
  else RunCheck(['--root',Repo,'--manifest',ManifestPath,'--files',FilesPath],Expected,Name,Output);
end;

procedure TestBytesAndReview;
var M:String;
begin
  NewCase(BaseManifest,TWO_FILES,'abc');
  CheckRelease(0,'ordinary unresolved bytes accepted','byte-inventory-check=passed');
  Check(Pos('unresolved=1'#10,LastOut)>0,'unresolved default is explicit');
  Check(Pos('scope=supplied-file-inventory; classification=suffix-profile-1'#10,LastOut)>0,'inventory scope is explicit');
  Check(Pos('provenance=declarations-not-independent-legal-verification'#10,LastOut)>0,'no legal verification inference');
  CheckRelease(1,'explicit review refuses unresolved','required-review=incomplete',True);
  RunCheck(['--root',Repo,'--manifest',ManifestPath,'--files',FilesPath,
    '--max-manifest-bytes',IntToStr(Length(BaseManifest)),'--max-file-list-bytes',IntToStr(Length(TWO_FILES)),
    '--max-assets','1','--max-files','2','--max-evidence-paths','1'],0,'all exact explicit limits','byte-inventory-check=passed');
  RunCheck(['--root',Repo,'--manifest',ManifestPath,'--files',FilesPath,
    '--max-manifest-bytes',IntToStr(Length(BaseManifest)-1)],3,'one-less manifest read allowance');
  RunCheck(['--root',Repo,'--manifest',ManifestPath,'--files',FilesPath,
    '--max-file-list-bytes',IntToStr(Length(TWO_FILES)-1)],3,'one-less inventory read allowance');
  RunCheck(['--root',Repo,'--manifest',ManifestPath,'--files',FilesPath,'--max-assets','0'],3,'one-less asset row allowance');
  RunCheck(['--root',Repo,'--manifest',ManifestPath,'--files',FilesPath,'--max-files','1'],3,'one-less inventory row allowance');
  RunCheck(['--root',Repo,'--manifest',ManifestPath,'--files',FilesPath,'--max-evidence-paths','0'],3,'one-less evidence allowance');
  M:=AssetManifest('assets%2Fa.png','3',ABC_HASH,'documented',True);
  NewCase(M,TWO_FILES,'abc');CheckRelease(0,'documented declaration and readable evidence','required-review=documented-declarations',True);
  NewCase(AssetManifest('assets%2Fa.png','0',EMPTY_HASH,'unresolved',False),ONE_FILE,'');
  CheckRelease(0,'actual empty asset hash','matched-assets=1');
  { Exact independent RFC 6234 byte-oriented vector, not text normalization. }
  NewCase(AssetManifest('assets%2Fa.png','1',
    '68AA2E2EE5DFF96E3355E6C7EE373E3D6A4E17F75F9518D843709C0C9BC3E3D4',
    'unresolved',False),ONE_FILE,#25);
  CheckRelease(0,'actual binary asset byte','matched-assets=1');
  { NIST million-a vector crosses the native host fixed 64KiB read buffer. }
  NewCase(AssetManifest('assets%2Fa.png','1000000',
    'CDC76E5C9914FB9281A1C7E284D73E67F1809A48A497200E046D39CCC7112CD0',
    'unresolved',False),ONE_FILE,StringOfChar('a',1000000));
  CheckRelease(0,'multi-buffer exact streamed asset','matched-assets=1');
  NewCase(EMPTY_MANIFEST,EMPTY_FILES,'',False,False);CheckRelease(0,'empty supplied release','matched-assets=0',True);
  NewCase(BaseManifest,TWO_FILES,'abd');CheckRelease(1,'same-length changed real bytes','sha256-mismatch=assets/a.png');
  NewCase(AssetManifest('assets%2Fa.png','4',ABC_HASH,'unresolved',True),TWO_FILES,'abc');
  CheckRelease(1,'exact size mismatch','size-mismatch=assets/a.png');
  Check(Pos('sha256-mismatch=',LastOut)=0,'size-only mismatch retains independent matching digest');
  NewCase(AssetManifest('assets%2Fa.png','3',ZERO_HASH,'unresolved',True),TWO_FILES,'abc');
  CheckRelease(1,'declared digest mismatch','sha256-mismatch=assets/a.png');
  NewCase(BaseManifest,TWO_FILES,'abc',False,True);CheckRelease(4,'missing physical asset','');
  NewCase(BaseManifest,TWO_FILES,'abc',True,False);CheckRelease(4,'missing physical evidence','');
  NewCase(BaseManifest,TWO_FILES,'abc');
  WriteOwned(Repo+DirectorySeparator+'unlisted.png','not part of this supplied inventory');
  CheckRelease(0,'unlisted files are not silently discovered','byte-inventory-check=passed');
end;

procedure TestInventoryAndMetadata;
begin
  NewCase(EMPTY_MANIFEST,TWO_FILES,'abc');CheckRelease(1,'required asset declaration missing','missing-asset-record=assets/a.png');
  NewCase(BaseManifest,'wfcfiles=1'#10'files=1'#10'file.0=docs%2Fproof.txt'#10,'abc');
  CheckRelease(1,'orphan declaration reported','orphan-asset-record=assets/a.png');
  NewCase(BaseManifest,ONE_FILE,'abc');CheckRelease(1,'evidence absent from supplied inventory','missing-evidence=assets/a.png:docs/proof.txt');
  NewCase(StringReplace(BaseManifest,'assets%2Fa.png','assets%2FA.png',[]),TWO_FILES,'abc');
  CheckRelease(1,'case-only inventory mismatch remains exact','orphan-asset-record=assets/A.png');
  Check(Pos('missing-asset-record=assets/a.png',LastOut)>0,'case mismatch reports both sides');
  NewCase(StringReplace(BaseManifest,'assets%2Fa.png','..%2Foutside.png',[]),TWO_FILES,'abc',False,False);
  CheckRelease(3,'unsafe declared traversal rejected before asset reads','');
  NewCase(StringReplace(BaseManifest,#10,#13#10,[]),TWO_FILES,'abc',False,False);
  CheckRelease(3,'noncanonical CRLF manifest refused before asset reads','');
  NewCase(StringReplace(BaseManifest,'review=unresolved','review=documented',[]),TWO_FILES,'abc');
  CheckRelease(3,'documented unknown metadata rejected','');
  NewCase(AssetManifest('assets%2Fa.png','3',ABC_HASH,'documented',False),ONE_FILE,'abc');
  CheckRelease(3,'documented without evidence rejected','');
  NewCase(BaseManifest,TWO_FILES,'abc',False,True);
  MakeDirectory(Repo+DirectorySeparator+'assets'+DirectorySeparator+'a.png');
  CheckRelease(4,'directory cannot be hashed as asset','');
  NewCase(BaseManifest,TWO_FILES,'abc',True,False);
  MakeDirectory(Repo+DirectorySeparator+'docs'+DirectorySeparator+'proof.txt');
  CheckRelease(4,'directory cannot be evidence','');
  NewCase(BaseManifest,TWO_FILES,'abc');
  RunCheck(['--root',Repo+DirectorySeparator+'absent','--manifest',ManifestPath,'--files',FilesPath],4,'missing root');
  RunCheck(['--root',Repo,'--manifest',CaseBase+DirectorySeparator+'absent','--files',FilesPath],4,'missing metadata input');
  RunCheck(['--root',Repo,'--manifest',Repo,'--files',FilesPath],4,'directory cannot be manifest input');
end;

procedure TestArgumentsAndConversion;
var Raw,Expected:String;I:Integer;
  procedure Convert(const Text,Name:String;const ExpectedExit:Integer;const Output:String);
  begin
    Inc(CaseSequence);Raw:=FixtureParent+DirectorySeparator+'raw-'+IntToStr(CaseSequence)+'.txt';
    WriteOwned(Raw,Text);
    RunCheck(['--inventory-from',Raw],ExpectedExit,Name,Output,ExpectedExit=0);
    if ExpectedExit<>0 then Check(LastOut='',Name+': failed conversion publishes no partial canonical output');
  end;
begin
  RunCheck([],2,'required explicit mode');RunCheck(['--unknown'],2,'unknown argument');
  RunCheck(['--help'],0,'standalone help','WFC asset inventory checker');
  RunCheck(['--version'],0,'standalone version','wfc_asset_check 1'#10,True);
  RunCheck(['--help','--version'],2,'standalone modes cannot mix');
  NewCase(BaseManifest,TWO_FILES,'abc');
  RunCheck(['--root',Repo,'--manifest',ManifestPath,'--files',FilesPath,'--root',Repo],2,'duplicate path option');
  RunCheck(['--root',Repo,'--manifest',ManifestPath,'--files',FilesPath,'--require-reviewed','--require-reviewed'],2,'duplicate review option');
  RunCheck(['--root','--help','--manifest',ManifestPath,'--files',FilesPath],2,'option cannot become a root value');
  RunCheck(['--root',Repo,'--manifest','--version','--files',FilesPath],2,'option cannot become a manifest value');
  RunCheck(['--root',Repo,'--manifest',ManifestPath,'--files',FilesPath,'--max-assets','01'],2,'noncanonical numeric option');
  Convert('docs/proof.txt'#13#10'assets/a.png'#13#10,'CRLF sorted into exact canonical LF',0,TWO_FILES);
  Convert('docs/proof.txt'#10'assets/a.png','optional final raw terminator',0,TWO_FILES);
  Convert('','empty raw supplied inventory',0,EMPTY_FILES);
  Convert('assets/a.png'#13,'bare CR refused',3,'');
  Convert('assets/a.png'#10#10,'blank raw row refused',3,'');
  Convert(#239#187#191'assets/a.png'#10,'raw BOM refused',3,'');
  Convert('assets/a.png'#10'assets/a.png'#10,'duplicate raw path refused',3,'');
  Convert('assets/A.png'#10'assets/a.png'#10,'raw case collision refused',3,'');
  Convert('assets\a.png'#10,'raw backslash is not normalized',3,'');
  Convert('CON .png'#10,'raw portable device spelling refused',3,'');
  Convert('ordinary .png'#10,'ordinary spaced basename remains exact',0,
    'wfcfiles=1'#10'files=1'#10'file.0=ordinary%20.png'#10);
  Raw:=Elsewhere+DirectorySeparator+'--inventory.txt';WriteOwned(Raw,'assets/a.png'#10);
  RunCheck(['--inventory-from','.'+DirectorySeparator+'--inventory.txt'],0,'prefixed literal option-looking filename',ONE_FILE,True);
  RunCheck(['--inventory-from',Raw,'--root',Repo],2,'disjoint inventory and check modes');
  RunCheck(['--inventory-from',Raw,'--require-reviewed'],2,'review flag irrelevant to conversion');
  RunCheck(['--inventory-from',Raw,'--max-manifest-bytes','1024'],2,'manifest policy irrelevant to conversion');
  RunCheck(['--inventory-from',Raw,'--max-files','1','--max-file-list-bytes',IntToStr(Length(ONE_FILE))],0,'conversion exact output allowance',ONE_FILE,True);
  RunCheck(['--inventory-from',Raw,'--max-file-list-bytes',IntToStr(Length(ONE_FILE)-1)],3,'conversion one-less output allowance');
  Check(LastOut='','oversized canonical output is never partially published');
  RunCheck(['--inventory-from',Raw,'--max-files','0'],3,'conversion one-less row allowance');
  Check(LastOut='','row refusal is never partially published');
  RunCheck(['--inventory-from',CaseBase],4,'directory cannot be raw inventory');
  RunCheck(['--inventory-from',CaseBase+DirectorySeparator+'absent'],4,'missing raw inventory');
  Expected:=ReadOwned(Raw);Check(Expected='assets/a.png'#10,'literal-option source bytes retained');
  for I:=1 to Length(Expected) do Check(Ord(Expected[I])<=127,'fixture remains explicit ASCII');
end;

function CreateFixtureLink(const LinkPath,Target:String;const IsDirectory:Boolean):Boolean;
{$IFDEF MSWINDOWS}
type TCreateSymbolicLink=function(LinkPath,Target:PChar;Flags:DWORD):Byte;stdcall;
var CreateLink:TCreateSymbolicLink;Flags:DWORD;
{$ENDIF}
begin
  InsideOwned(LinkPath);InsideOwned(Target);
  {$IFDEF MSWINDOWS}
  CreateLink:=TCreateSymbolicLink(GetProcAddress(GetModuleHandle('kernel32.dll'),'CreateSymbolicLinkA'));
  Flags:=2;if IsDirectory then Flags:=Flags or 1;
  Result:=Assigned(CreateLink) and (CreateLink(PChar(LinkPath),PChar(Target),Flags)<>0);
  {$ELSE}Result:=fpSymlink(PChar(Target),PChar(LinkPath))=0;{$ENDIF}
  if Result then AddAllowed(LinkPath)
  else
  begin
    Inc(LinkSkips);
    WriteLn('SKIP symbolic-link fixture: host privilege/capability unavailable; no link refusal proof claimed.');
  end;
end;

procedure TestLinks;
var Target,LinkPath:String;
begin
  Target:=FixtureParent+DirectorySeparator+'outside-target.png';WriteOwned(Target,'abc');
  NewCase(BaseManifest,TWO_FILES,'abc',False,True);
  LinkPath:=Repo+DirectorySeparator+'assets'+DirectorySeparator+'a.png';
  if CreateFixtureLink(LinkPath,Target,False) then
  begin CheckRelease(4,'asset symlink refused','');Inc(LinkRuns);end;
  NewCase(BaseManifest,TWO_FILES,'abc',True,False);
  LinkPath:=Repo+DirectorySeparator+'docs'+DirectorySeparator+'proof.txt';
  if CreateFixtureLink(LinkPath,Target,False) then
  begin CheckRelease(4,'evidence symlink refused','');Inc(LinkRuns);end;
  NewCase(BaseManifest,TWO_FILES,'abc');
  LinkPath:=CaseBase+DirectorySeparator+'root-link';
  if CreateFixtureLink(LinkPath,Repo,True) then
  begin
    RunCheck(['--root',LinkPath,'--manifest',ManifestPath,'--files',FilesPath],4,'root directory symlink refused');Inc(LinkRuns);
  end;
  LinkPath:=CaseBase+DirectorySeparator+'manifest-link.txt';
  if CreateFixtureLink(LinkPath,ManifestPath,False) then
  begin
    RunCheck(['--root',Repo,'--manifest',LinkPath,'--files',FilesPath],4,'manifest symlink refused');Inc(LinkRuns);
  end;
  LinkPath:=CaseBase+DirectorySeparator+'inventory-link.txt';
  if CreateFixtureLink(LinkPath,FilesPath,False) then
  begin RunCheck(['--inventory-from',LinkPath],4,'raw inventory symlink refused');Inc(LinkRuns);end;
  NewCase(StringReplace(BaseManifest,'assets%2Fa.png','linked%2Fa.png',[]),
    'wfcfiles=1'#10'files=2'#10'file.0=docs%2Fproof.txt'#10'file.1=linked%2Fa.png'#10,'abc');
  LinkPath:=Repo+DirectorySeparator+'linked';Target:=Repo+DirectorySeparator+'assets';
  if CreateFixtureLink(LinkPath,Target,True) then
  begin CheckRelease(4,'intermediate directory symlink refused','');Inc(LinkRuns);end;
end;

begin
  try
    if ParamCount<>2 then raise Exception.Create('expected CHECKER_EXE and a new nonexistent FIXTURE_DIRECTORY');
    Checker:=ExpandFileName(ParamStr(1));FixtureParent:=ExcludeTrailingPathDelimiter(ExpandFileName(ParamStr(2)));
    if not FileExists(Checker) then raise Exception.Create('checker executable unavailable');
    if (ParamStr(2)='') or FileExists(FixtureParent) or DirectoryExists(FixtureParent) or
      (ExtractFileName(FixtureParent)='') then raise Exception.Create('fixture directory must be explicit and nonexistent');
    Check(CreateDir(FixtureParent),'create new explicitly owned fixture parent');
    SetLength(Directories,1);Directories[0]:=FixtureParent;
    Elsewhere:=FixtureParent+DirectorySeparator+'independent working directory';MakeDirectory(Elsewhere);
    LogRoot:=FixtureParent+DirectorySeparator+'process-evidence';MakeDirectory(LogRoot);
    TestBytesAndReview;TestInventoryAndMetadata;TestArgumentsAndConversion;TestLinks;
    CheckOwnedUnchanged;
    WriteLn('Asset checker process cases: ',Cases);
    WriteLn('Asset checker symbolic-link cases: ',LinkRuns,' executed, ',LinkSkips,' explicitly skipped');
    WriteLn('Asset checker process checks: ',Checks,' passed');
    WriteLn('Retained explicit fixture directory: ',FixtureParent);
  except
    on E:Exception do
    begin WriteLn(StdErr,'wfc_asset_check_process_test: ',E.Message);Halt(1);end;
  end;
end.
