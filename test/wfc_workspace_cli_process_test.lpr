{ SPDX-License-Identifier: MIT
  Copyright (c) 2021 mr-highball
  Native included-workspace CLI lifecycle and exclusive-publication checks. }
program wfc_workspace_cli_process_test;
{$mode delphi}{$H+}
{$IFDEF PAS2JS}{$FATAL workspace CLI process tests require native FPC}{$ENDIF}
uses
  {$IFDEF UNIX}cthreads,BaseUnix,{$ENDIF}
  Classes,SysUtils,Process,Pipes,wfc_process_test_support,wfc_browser_socket,
  wfc_atomic_new_file;
const
  CHILD_TIMEOUT=15000;
  CLEANUP_TIMEOUT=5000;
  CAPTURE_LIMIT=2097152;
type
  TOutcome=record
    Code: Integer;
    OutputText,ErrorText: String;
  end;
  TFileSnapshot=record Path,Bytes: String; end;
  TFileSnapshots=array of TFileSnapshot;
var
  Checks,Cases,CliCases,HelperCases: Integer;
  Cli,Fixture,WorkRoot,FixtureRoot,LogRoot: String;
  OriginalInputs: TFileSnapshots;

procedure Check(const Condition: Boolean; const Detail: String);
begin
  Inc(Checks);
  if not Condition then raise Exception.Create(Detail);
end;

function Elapsed(const Started: QWord): QWord;
var Current: QWord;
begin
  Current:=WfcBrowserTickCount64;
  if Current>=Started then Result:=Current-Started
  else Result:=(High(QWord)-Started)+Current+1;
end;

function OwnedPath(const Parent,Name: String): String;
var Prefix: String;
begin
  Prefix:=IncludeTrailingPathDelimiter(Parent);
  Result:=ExpandFileName(Prefix+Name);
  if Pos(Prefix,Result)<>1 then raise Exception.Create('test path escaped its explicit directory');
end;

function DataPath(const Name: String): String;
begin Result:=OwnedPath(FixtureRoot,Name); end;

procedure WriteNew(const Path,Text: String);
var F: TWfcAtomicNewFile; Bytes: array of Byte; Offset,Count,I: Integer;
begin
  F:=TWfcAtomicNewFile.Create(Path);
  try
    Offset:=0;
    while Offset<Length(Text) do
    begin
      Count:=Length(Text)-Offset; if Count>65536 then Count:=65536;
      SetLength(Bytes,Count);
      for I:=0 to Count-1 do Bytes[I]:=Ord(Text[Offset+I+1]);
      F.WriteBytes(Bytes); Inc(Offset,Count);
    end;
    F.Publish;
    Check(F.CleanupError='','exclusive evidence file has no cleanup error');
  finally F.Free; end;
end;

function ReadBytes(const Path: String): String;
var F: TFileStream;
begin
  F:=TFileStream.Create(Path,fmOpenRead or fmShareDenyWrite);
  try
    Check((F.Size>=0) and (F.Size<=CAPTURE_LIMIT),'fixture bytes remain inside process-test bound');
    SetLength(Result,Integer(F.Size));
    if Length(Result)>0 then F.ReadBuffer(Result[1],Length(Result));
  finally F.Free; end;
end;

procedure ReadAvailable(const Pipe: TInputPipeStream; var Text: String);
var Buffer: array[0..4095] of Byte; Count,Offset: Integer;
begin
  while Pipe.NumBytesAvailable>0 do
  begin
    Count:=Pipe.NumBytesAvailable; if Count>SizeOf(Buffer) then Count:=SizeOf(Buffer);
    Count:=Pipe.Read(Buffer[0],Count); if Count<=0 then Exit;
    if Length(Text)>CAPTURE_LIMIT-Count then raise Exception.Create('child output exceeds two MiB');
    Offset:=Length(Text); SetLength(Text,Offset+Count);
    Move(Buffer[0],Text[Offset+1],Count);
  end;
end;

function WaitStopped(const Child: TProcess): Boolean;
var Started: QWord;
begin
  Started:=WfcBrowserTickCount64;
  repeat
    if not Child.Running then Exit(True);
    Sleep(2);
  until Elapsed(Started)>=CLEANUP_TIMEOUT;
  Result:=not Child.Running;
end;

function Invoke(const Name,Executable: String; const Args: array of String): TOutcome;
var Child: TProcess; Started: QWord; I: Integer; LogName,ArgumentsText: String;
begin
  Inc(Cases); Result:=Default(TOutcome); Result.Code:=-1;
  LogName:=IntToStr(Cases)+'-'+Name; ArgumentsText:=Executable+#10;
  Child:=TProcess.Create(nil);
  try
    Child.Executable:=Executable; Child.CurrentDirectory:=WorkRoot;
    for I:=0 to High(Args) do
    begin Child.Parameters.Add(Args[I]); ArgumentsText:=ArgumentsText+Args[I]+#10; end;
    Child.Options:=[poUsePipes,poNoConsole];
    Child.Execute;
    { Commands are file-only. Never materialize Child.Input: CloseInput then
      closes its descriptor directly on both stable and trunk FCL. }
    Child.CloseInput; Started:=WfcBrowserTickCount64;
    repeat
      ReadAvailable(Child.Output,Result.OutputText);
      ReadAvailable(Child.Stderr,Result.ErrorText);
      if not Child.Running then Break;
      if Elapsed(Started)>=CHILD_TIMEOUT then
        raise Exception.Create(Name+' exceeded the finite child deadline');
      Sleep(2);
    until False;
    ReadAvailable(Child.Output,Result.OutputText);
    ReadAvailable(Child.Stderr,Result.ErrorText);
    Result.Code:=WfcProcessExitCode(Child);
    WriteLn('case ',Cases,': ',Name,' exit=',Result.Code);
  finally
    try
      if Child.Running then
      begin
        {$IFDEF UNIX}fpKill(Child.ProcessID,SIGKILL);{$ELSE}Child.Terminate(1);{$ENDIF}
        Check(WaitStopped(Child),'owned child stopped within cleanup deadline');
      end;
    finally
      Child.Free;
      { New work/log directories make each evidence filename exclusive. Even
        failed invocations retain their captured prefix and actual exit state. }
      WriteNew(OwnedPath(LogRoot,LogName+'.stdout'),Result.OutputText);
      WriteNew(OwnedPath(LogRoot,LogName+'.stderr'),Result.ErrorText);
      WriteNew(OwnedPath(LogRoot,LogName+'.argv'),ArgumentsText);
      WriteNew(OwnedPath(LogRoot,LogName+'.exit'),IntToStr(Result.Code)+#10);
    end;
  end;
end;

function RunCase(const Name: String; const Expected: Integer;
  const Args: array of String; const RefusedPreview: Boolean=False): String;
var O: TOutcome;
begin
  Inc(CliCases); O:=Invoke(Name,Cli,Args);
  Check(O.Code=Expected,Name+': expected '+IntToStr(Expected)+', observed '+IntToStr(O.Code)+'; '+O.ErrorText);
  if (Expected in [0,10]) or RefusedPreview then
  begin
    Check(O.ErrorText='',Name+': accepted operation/preview has empty stderr');
    Check(O.OutputText<>'',Name+': actual stdout report exists');
  end
  else
  begin
    Check(O.OutputText='',Name+': rejected operation emits no successful stdout');
    Check((Pos('workspace: ',O.ErrorText)=1) and (Length(O.ErrorText)<=4096),Name+': bounded explicit rejection diagnostic');
  end;
  Result:=O.OutputText;
end;

function RunFixture(const Name: String; const Args: array of String): String;
var O: TOutcome;
begin
  Inc(HelperCases); O:=Invoke(Name,Fixture,Args);
  Check(O.Code=0,Name+': independent fixture exited normally; '+O.ErrorText);
  Check(O.ErrorText='',Name+': fixture stderr empty');
  Result:=O.OutputText;
end;

procedure Contains(const Text,Expected,Detail: String);
begin Check(Pos(Expected,Text)>0,Detail+': actual output contains '+Expected); end;

procedure RequireNewJournal(const Name: String);
var Text: String; I: Integer; Canonical: Boolean;
begin
  Check(FileExists(DataPath(Name)),Name+': accepted output exists');
  Text:=ReadBytes(DataPath(Name));
  Check((Length(Text)>0) and (Text[Length(Text)]=#10),Name+': complete LF-terminated document');
  Canonical:=True;
  for I:=1 to Length(Text) do
    if not ((Text[I]=#10) or (Text[I] in [#32..#126])) then Canonical:=False;
  Check(Canonical,Name+': complete canonical ASCII bytes, not text-mode CRLF');
end;

procedure CaptureOriginalInputs;
const Names: array[0..8] of String=('recipe0.wfc','recipe1.wfc','run0.wfc',
  'run1.wfc','run2.wfc','run3.wfc','run4.wfc','direct.journal','forged.journal');
var I: Integer;
begin
  SetLength(OriginalInputs,Length(Names));
  for I:=0 to High(Names) do
  begin OriginalInputs[I].Path:=DataPath(Names[I]); OriginalInputs[I].Bytes:=ReadBytes(OriginalInputs[I].Path); end;
end;

procedure CheckOriginalInputs;
var I: Integer;
begin
  for I:=0 to High(OriginalInputs) do
    Check(ReadBytes(OriginalInputs[I].Path)=OriginalInputs[I].Bytes,'every original recipe/run/journal input byte unchanged');
end;

procedure CheckNoPartials(const Directory: String);
var Entry: TSearchRec; Code: Integer;
begin
  Code:=FindFirst(IncludeTrailingPathDelimiter(Directory)+'*.partial-*',faAnyFile,Entry);
  if Code=0 then
  begin
    FindClose(Entry);
    Check(False,'owned partial output remains in '+Directory);
  end;
  Check(Code<>0,'no owned partial output remains in '+Directory);
end;

procedure TestCases;
var Text,Sentinel: String; Bytes: Integer;
begin
  RunFixture('fixture-make',['--make',FixtureRoot]); CaptureOriginalInputs;
  RunCase('help',0,['--help']);
  RunCase('bad-roots-before-io',2,['preview','--input',DataPath('missing'),'--run',DataPath('missing'),'--roots','1,0']);
  RunCase('duplicate',2,['inspect','--input','a','--input','b']);
  RunCase('irrelevant',2,['inspect','--input','a','--output','b']);
  RunCase('bad-limit',2,['inspect','--input','a','--limit','actions=00']);
  RunCase('missing-input',5,['inspect','--input',DataPath('missing')]);
  Text:=RunCase('inspect-forged',0,['inspect','--input',DataPath('forged.journal')]);
  Contains(Text,'solver-executed=0','graph-free inspect'); Contains(Text,'verification=unverified-claims','opaque evidence remains a claim');
  RunCase('replay-forged',4,['replay','--input',DataPath('forged.journal')]);
  Bytes:=Length(ReadBytes(DataPath('direct.journal')));
  RunCase('exact-bytes',0,['inspect','--input',DataPath('direct.journal'),'--limit','journal-bytes='+IntToStr(Bytes)]);
  RunCase('short-bytes',3,['inspect','--input',DataPath('direct.journal'),'--limit','journal-bytes='+IntToStr(Bytes-1)]);
  RunCase('begin',0,['begin','--recipe',DataPath('recipe0.wfc'),'--run',DataPath('run0.wfc'),'--output',DataPath('step0.journal')]);
  RequireNewJournal('step0.journal');
  RunCase('initial',0,['initial','--input',DataPath('step0.journal'),'--output',DataPath('step1.journal')]);
  RunCase('lock',0,['edit','--input',DataPath('step1.journal'),'--run',DataPath('run1.wfc'),'--output',DataPath('step2.journal')]);
  RunCase('clear',0,['edit','--input',DataPath('step2.journal'),'--run',DataPath('run0.wfc'),'--output',DataPath('step3.journal')]);
  Text:=RunCase('narrow-preview',4,['preview','--input',DataPath('step3.journal'),'--run',DataPath('run0.wfc'),'--roots','3'],True);
  Contains(Text,'can-execute=0','insufficient explicit scope preview');
  RunCase('narrow-repair',4,['repair','--input',DataPath('step3.journal'),'--run',DataPath('run0.wfc'),'--roots','3','--output',DataPath('refused.journal')]);
  Check(not FileExists(DataPath('refused.journal')),'refused repair publishes no output');
  RunCase('broader',0,['repair','--input',DataPath('step3.journal'),'--run',DataPath('run0.wfc'),'--roots','1','--output',DataPath('step4.journal')]);
  RunCase('impossible',0,['edit','--input',DataPath('step4.journal'),'--run',DataPath('run2.wfc'),'--output',DataPath('step5.journal')]);
  Text:=RunCase('failed-repair',10,['repair','--input',DataPath('step5.journal'),'--run',DataPath('run2.wfc'),'--roots','0','--output',DataPath('step6.journal')]);
  Contains(Text,'current-output=0','normal failed repair currentness'); Contains(Text,'successful-baseline=1','normal failed repair retains baseline');
  RequireNewJournal('step6.journal');
  RunCase('replay-failed',0,['replay','--input',DataPath('step6.journal')]);
  Text:=RunFixture('compare-failed',['--compare',DataPath('step6.journal'),'7']);
  Contains(Text,'CLI direct-history checks: 53','complete seven-action direct history comparator');
  RunCase('clear-domain',0,['edit','--input',DataPath('step6.journal'),'--run',DataPath('run0.wfc'),'--output',DataPath('step7.journal')]);
  RunCase('recover',0,['repair','--input',DataPath('step7.journal'),'--run',DataPath('run0.wfc'),'--roots','0','--output',DataPath('step8.journal')]);
  RunCase('new-epoch',0,['begin','--input',DataPath('step8.journal'),'--recipe',DataPath('recipe1.wfc'),'--run',DataPath('run3.wfc'),'--output',DataPath('step9.journal')]);
  RunCase('preinitial-edit',0,['edit','--input',DataPath('step9.journal'),'--run',DataPath('run4.wfc'),'--output',DataPath('step10.journal')]);
  RunCase('second-initial',0,['initial','--input',DataPath('step10.journal'),'--output',DataPath('step11.journal')]);
  RunCase('final-repair',0,['repair','--input',DataPath('step11.journal'),'--run',DataPath('run4.wfc'),'--roots','1','--output',DataPath('step12.journal')]);
  RequireNewJournal('step12.journal');
  RunCase('replay-final',0,['replay','--input',DataPath('step12.journal')]);
  Text:=RunFixture('compare-final',['--compare',DataPath('step12.journal'),'13']);
  Contains(Text,'CLI direct-history checks: 85','complete thirteen-action direct history comparator');
  RunCase('failed-initial-begin',0,['begin','--recipe',DataPath('recipe0.wfc'),'--run',DataPath('run2.wfc'),'--output',DataPath('failed-initial-begin.journal')]);
  Text:=RunCase('failed-initial',10,['initial','--input',DataPath('failed-initial-begin.journal'),'--output',DataPath('failed-initial.journal')]);
  Contains(Text,'successful-baseline=0','failed initial does not acquire baseline');
  Contains(Text,'current-output=0','failed initial never becomes current output');
  RequireNewJournal('failed-initial.journal');
  RunCase('replay-failed-initial',0,['replay','--input',DataPath('failed-initial.journal')]);
  Text:=RunCase('failed-initial-preview',4,['preview','--input',DataPath('failed-initial.journal'),'--run',DataPath('run2.wfc'),'--roots','0'],True);
  Contains(Text,'missing-baseline=1','repair preview reports missing initial baseline');
  RunCase('failed-initial-repair',4,['repair','--input',DataPath('failed-initial.journal'),'--run',DataPath('run2.wfc'),'--roots','0','--output',DataPath('no-baseline-refused.journal')]);
  Check(not FileExists(DataPath('no-baseline-refused.journal')),'missing-baseline refusal never publishes');
  Sentinel:=ReadBytes(DataPath('step0.journal'));
  RunCase('same-output',5,['initial','--input',DataPath('step0.journal'),'--output',DataPath('step0.journal')]);
  Check(ReadBytes(DataPath('step0.journal'))=Sentinel,'all existing input/output bytes preserved');
  RunCase('missing-parent',5,['initial','--input',DataPath('step0.journal'),'--output',DataPath('missing-parent/output.journal')]);
  Check(not DirectoryExists(DataPath('missing-parent')),'missing parent is not silently created');
  RunCase('directory-output',5,['initial','--input',DataPath('step0.journal'),'--output',FixtureRoot]);
  Check(DirectoryExists(FixtureRoot),'directory output refusal preserves fixture directory');
  CheckOriginalInputs;
  CheckNoPartials(FixtureRoot); CheckNoPartials(LogRoot); CheckNoPartials(WorkRoot);
  Check((CliCases=35) and (HelperCases=3) and (Cases=38),'all original33 CLI cases plus two baseline refusals and three oracle processes ran');
end;

procedure Main;
begin
  Check(ParamCount=3,'usage: wfc_workspace_cli_process_test WORKSPACE_CLI FIXTURE NEW_WORK_DIRECTORY');
  Cli:=ExpandFileName(ParamStr(1)); Fixture:=ExpandFileName(ParamStr(2));
  WorkRoot:=ExcludeTrailingPathDelimiter(ExpandFileName(ParamStr(3)));
  Check(FileExists(Cli) and FileExists(Fixture),'both included executables exist');
  Check(not FileExists(WorkRoot) and not DirectoryExists(WorkRoot),'test directory must be exclusively new');
  Check(DirectoryExists(ExtractFileDir(WorkRoot)),'test directory parent must already exist');
  Check(CreateDir(WorkRoot),'create exactly the requested test directory');
  FixtureRoot:=OwnedPath(WorkRoot,'fixture with spaces'); LogRoot:=OwnedPath(WorkRoot,'logs');
  Check(CreateDir(FixtureRoot) and CreateDir(LogRoot),'create owned fixture and log directories');
  WriteLn('Owned workspace CLI evidence: ',WorkRoot);
  TestCases;
end;

begin
  try
    Main;
    WriteLn('Workspace CLI invocations: ',CliCases,'; fixture invocations: ',HelperCases);
    WriteLn('Workspace CLI process cases: ',Cases,'; checks: ',Checks,'/',Checks);
  except on E: Exception do
    begin WriteLn(StdErr,'wfc_workspace_cli_process_test: ',E.ClassName,': ',E.Message); Halt(1); end;
  end;
end.
