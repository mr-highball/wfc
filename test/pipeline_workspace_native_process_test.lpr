{ SPDX-License-Identifier: MIT
  Copyright (c) 2021 mr-highball
  Native FPC child-process conformance. No servers or alternate runtimes. }
program pipeline_workspace_native_process_test;
{$mode delphi}{$H+}
uses {$IFDEF UNIX}cthreads,BaseUnix,{$ENDIF}
  Classes,SysUtils,Process,Pipes,wfc_process_test_support,wfc_atomic_new_file;
const CHILD_TIMEOUT=60000; CLEANUP_TIMEOUT=5000; CAPTURE_LIMIT=2097152;
type
  TOutcome=record Code:Integer;OutputText,ErrorText:String;end;
  TSnapshot=record Path,Bytes:String;end;
  TSnapshots=array of TSnapshot;
var HostExe,WorkspaceExe,FixtureExe,WorkRoot,LogRoot:String;
  Checks,Cases,HostCases,FixtureCases,WorkspaceCases:Integer;
procedure Check(const Ok:Boolean;const Detail:String);
begin Inc(Checks);if not Ok then raise Exception.Create(Detail);end;
function At(const Directory,Name:String):String;
begin Result:=IncludeTrailingPathDelimiter(Directory)+Name;end;
function Data(const Name:String):String;
begin Result:=At(WorkRoot,Name);end;
procedure WriteNew(const Path,Text:String);
var F:TWfcAtomicNewFile;B:array of Byte;Used,N,I:Integer;
begin
  F:=TWfcAtomicNewFile.Create(Path);
  try
    Used:=0;
    while Used<Length(Text) do
    begin N:=Length(Text)-Used;if N>65536 then N:=65536;SetLength(B,N);
      for I:=0 to N-1 do B[I]:=Ord(Text[Used+I+1]);F.WriteBytes(B);Inc(Used,N);end;
    F.Publish;Check(F.CleanupError='','evidence file cleanup');
  finally F.Free;end;
end;
function ReadText(const Path:String):String;
var F:TFileStream;
begin
  F:=TFileStream.Create(Path,fmOpenRead or fmShareDenyWrite);
  try
    Check((F.Size>=0) and (F.Size<=67108864),'retained artifact bound');
    SetLength(Result,Integer(F.Size));if Length(Result)>0 then F.ReadBuffer(Result[1],Length(Result));
  finally F.Free;end;
end;
procedure Drain(const Pipe:TInputPipeStream;var Text:String);
var B:array[0..4095]of Byte;N,Offset:Integer;
begin
  while Pipe.NumBytesAvailable>0 do
  begin
    N:=Pipe.NumBytesAvailable;if N>SizeOf(B) then N:=SizeOf(B);
    N:=Pipe.Read(B[0],N);if N<=0 then Exit;
    if Length(Text)>CAPTURE_LIMIT-N then raise Exception.Create('child output capture bound exceeded');
    Offset:=Length(Text);SetLength(Text,Offset+N);Move(B[0],Text[Offset+1],N);
  end;
end;
function Elapsed(const Started:QWord):QWord;
var NowValue:QWord;
begin
  NowValue:=GetTickCount64;
  if NowValue>=Started then Result:=NowValue-Started else Result:=(High(QWord)-Started)+NowValue+1;
end;
function Invoke(const Name,Exe:String;const Args:array of String):TOutcome;
var P:TProcess;Started:QWord;I:Integer;Prefix,ArgumentText:String;
begin
  Inc(Cases);Result:=Default(TOutcome);Result.Code:=-1;
  Prefix:=At(LogRoot,IntToStr(Cases)+'-'+Name);ArgumentText:=Exe+#10;P:=TProcess.Create(nil);
  try
    P.Executable:=Exe;P.CurrentDirectory:=WorkRoot;P.Options:=[poUsePipes,poNoConsole];
    for I:=0 to High(Args) do begin P.Parameters.Add(Args[I]);ArgumentText:=ArgumentText+Args[I]+#10;end;
    P.Execute;P.CloseInput;Started:=GetTickCount64;
    repeat
      Drain(P.Output,Result.OutputText);Drain(P.Stderr,Result.ErrorText);
      if not P.Running then Break;
      if Elapsed(Started)>=CHILD_TIMEOUT then raise Exception.Create(Name+' child exceeded60 seconds');
      Sleep(2);
    until False;
    Drain(P.Output,Result.OutputText);Drain(P.Stderr,Result.ErrorText);
    Result.Code:=WfcProcessExitCode(P);WriteLn('case ',Cases,': ',Name,' exit=',Result.Code);
  finally
    try
      if P.Running then
      begin
        {$IFDEF UNIX}fpKill(P.ProcessID,SIGKILL);{$ELSE}P.Terminate(1);{$ENDIF}
        Started:=GetTickCount64;
        while P.Running and (Elapsed(Started)<CLEANUP_TIMEOUT) do Sleep(2);
        Check(not P.Running,'exact owned child terminal after cleanup');
      end;
    finally
      P.Free;
      WriteNew(Prefix+'.argv',ArgumentText);WriteNew(Prefix+'.stdout',Result.OutputText);
      WriteNew(Prefix+'.stderr',Result.ErrorText);WriteNew(Prefix+'.exit',IntToStr(Result.Code)+#10);
    end;
  end;
end;
function Host(const Name:String;const Expected:Integer;const Args:array of String):String;
var O:TOutcome;
begin
  Inc(HostCases);O:=Invoke(Name,HostExe,Args);
  Check(O.Code=Expected,Name+' expected exit'+IntToStr(Expected)+' actual'+IntToStr(O.Code));
  if (Expected=0) or (Expected=10) then Check(O.ErrorText='','accepted host stderr empty')
  else Check(O.ErrorText<>'','rejected host diagnostic is present');
  Result:=O.OutputText;
end;
procedure Verify(const Directory,Kind:String);
var O:TOutcome;
begin
  Inc(FixtureCases);O:=Invoke('verify-'+Kind,FixtureExe,['--verify',Directory,Kind]);
  Check((O.Code=0) and (O.ErrorText=''),'independent decode/replay/SVG fixture passed');
  Check(Pos('Native workspace fixture checks: ',O.OutputText)>0,'actual fixture check summary');
end;
procedure Workspace(const Name:String;const Args:array of String);
var O:TOutcome;
begin
  Inc(WorkspaceCases);O:=Invoke(Name,WorkspaceExe,Args);
  Check((O.Code=0) and (O.ErrorText=''),'existing workspace CLI accepted '+Name);
end;
function Snapshot(const Directory:String):TSnapshots;
const Names:array[0..3]of String=('recipe.wfc','run.wfc','journal.wfc','selected-pass.svg');
var I:Integer;
begin
  SetLength(Result,4);
  for I:=0 to 3 do begin Result[I].Path:=At(Directory,Names[I]);Result[I].Bytes:=ReadText(Result[I].Path);end;
end;
procedure Unchanged(const S:TSnapshots);
var I:Integer;
begin for I:=0 to High(S) do Check(ReadText(S[I].Path)=S[I].Bytes,'original bytes preserved '+S[I].Path);end;
procedure NoPartials(const Directory:String);
var F:TSearchRec;Code:Integer;
begin
  Code:=FindFirst(At(Directory,'*'),faAnyFile,F);
  try
    while Code=0 do
    begin
      if (F.Name<>'.') and (F.Name<>'..') then
      begin
        Check(Pos('.partial-',F.Name)=0,'no owned partial file remains');
        if (F.Attr and faDirectory)<>0 then NoPartials(At(Directory,F.Name));
      end;
      Code:=FindNext(F);
    end;
  finally FindClose(F);end;
end;
procedure Rejections;
var I:Integer;OutDir:String;
begin
  for I:=0 to 15 do
  begin
    OutDir:=Data('rejected-'+IntToStr(I));
    case I of
      0:Host('unknown-option',2,['--preset','landscape','--output-dir',OutDir,'--unknown','1']);
      1:Host('duplicate-seed',2,['--preset','landscape','--output-dir',OutDir,'--seed','7','--seed','8']);
      2:Host('noncanonical-seed',2,['--preset','landscape','--output-dir',OutDir,'--seed','07']);
      3:Host('noncanonical-origin',2,['--preset','landscape','--output-dir',OutDir,'--pass','0,2,8,6,1,+0,0,0,4,4,1,0']);
      4:Host('duplicate-pass',2,['--preset','landscape','--output-dir',OutDir,'--pass','0,2,8,6,1,0,0,0,4,4,1,0','--pass','0,2,8,6,1,0,0,0,4,4,1,0']);
      5:Host('private-view',2,['--preset','sequence','--output-dir',OutDir,'--view','1,0,0,0,1,1']);
      6:Host('low-context-policy',3,['--preset','landscape','--output-dir',OutDir,'--limit','context-bytes=1']);
      7:Host('low-action-policy',4,['--preset','landscape','--output-dir',OutDir,'--limit','actions=1']);
      8:Host('low-view-policy',2,['--preset','landscape','--output-dir',OutDir,'--limit','view-cells=1']);
      9:Host('low-svg-policy',3,['--preset','landscape','--output-dir',OutDir,'--limit','svg-bytes=1']);
      10:Host('duplicate-limit',2,['--preset','landscape','--output-dir',OutDir,'--limit','view-cells=8','--limit','view-cells=16']);
      11:Host('wrong-preset-option',2,['--preset','sequence','--output-dir',OutDir,'--weights','1,1,1,1,1,1']);
      12:Host('alias-shape-mismatch',3,['--preset','sequence','--output-dir',OutDir,'--pass','3,1,11,1,1,11,-3,5,3,2,1,0']);
      13:Host('window-not-clipped',2,['--preset','landscape','--output-dir',OutDir,'--view','2,0,0,0,4,2']);
      14:Host('invalid-trace',2,['--preset','landscape','--output-dir',OutDir,'--trace','2']);
      15:Host('import-override-refused',2,['--recipe',At(Data('landscape'),'recipe.wfc'),'--run',At(Data('landscape'),'run.wfc'),'--output-dir',OutDir,'--seed','8']);
    end;
    Check(not DirectoryExists(OutDir) and not FileExists(OutDir),'rejected request did not publish output directory');
  end;
end;
procedure Run;
var A,B:TSnapshots;O:TOutcome;Text:String;I:Integer;
begin
  Text:=Host('help',0,['--help']);Check(Pos('NOT atomic',Text)>0,'help discloses multi-file boundary');
  Text:=Host('landscape',0,['--preset','landscape','--output-dir',Data('landscape'),'--view','2,0,0,0,3,2']);
  Check(Pos('solved=1 current=1 baseline=1',Text)>0,'actual solved host status');
  Verify(Data('landscape'),'landscape');A:=Snapshot(Data('landscape'));
  Host('sequence',0,['--preset','sequence','--output-dir',Data('sequence'),'--view','2,0,0,0,12,1']);
  Verify(Data('sequence'),'sequence');
  Host('larger-signed',0,['--preset','landscape','--output-dir',Data('larger'),'--seed','55',
    '--pass','0,2,16,12,1,-32,-24,0,4,4,1,0','--pass','1,2,64,48,1,-32,-24,0,1,1,1,0',
    '--pass','2,2,7,5,1,-28,-20,0,8,8,1,0','--view','2,0,0,0,7,5','--local-backtracks','256','--pass-backtracks','32']);
  Verify(Data('larger'),'larger');
  Host('long-sequence',0,['--preset','sequence','--output-dir',Data('long sequence'),
    '--pass','1,1,64,1,1,11,-3,5,3,2,1,0','--pass','2,1,64,1,1,11,-3,5,3,2,1,0',
    '--pass','3,1,64,1,1,11,-3,5,3,2,1,0','--view','2,0,0,0,32,1']);
  Verify(Data('long sequence'),'long-sequence');
  Host('import',0,['--recipe',At(Data('landscape'),'recipe.wfc'),'--run',At(Data('landscape'),'run.wfc'),
    '--output-dir',Data('imported'),'--view','2,0,0,0,3,2']);
  Verify(Data('imported'),'landscape');B:=Snapshot(Data('imported'));
  for I:=0 to 3 do Check(A[I].Bytes=B[I].Bytes,'full imported canonical/artifact bytes equal original '+IntToStr(I));
  Workspace('inspect-landscape',['inspect','--input',At(Data('landscape'),'journal.wfc')]);
  Workspace('replay-landscape',['replay','--input',At(Data('landscape'),'journal.wfc')]);
  Workspace('replay-sequence',['replay','--input',At(Data('sequence'),'journal.wfc')]);
  Inc(FixtureCases);O:=Invoke('make-unsolved',FixtureExe,['--make-unsolved',Data('landscape'),Data('unsolved inputs')]);
  Check((O.Code=0) and (O.ErrorText=''),'actual typed empty-domain fixture constructed');
  Text:=Host('unsolved',10,['--recipe',At(Data('unsolved inputs'),'recipe.wfc'),
    '--run',At(Data('unsolved inputs'),'run.wfc'),'--output-dir',Data('unsolved'),'--view','2,0,0,0,3,2']);
  Check(Pos('solved=0 current=0 baseline=0',Text)>0,'normal failed initial has no current or baseline');
  Verify(Data('unsolved'),'unsolved');
  Workspace('replay-unsolved',['replay','--input',At(Data('unsolved'),'journal.wfc')]);
  Host('existing-output-refused',2,['--preset','sequence','--output-dir',Data('landscape')]);Unchanged(A);
  WriteNew(Data('output-is-file'),'do not replace this file'+#10);
  Host('existing-file-refused',2,['--preset','landscape','--output-dir',Data('output-is-file')]);
  Check(ReadText(Data('output-is-file'))='do not replace this file'+#10,'existing output file preserved');
  Host('zero-budgets',0,['--preset','landscape','--output-dir',Data('zero budgets'),
    '--local-backtracks','0','--pass-backtracks','0','--view','2,0,0,0,3,2']);
  Rejections;Unchanged(A);Unchanged(B);NoPartials(WorkRoot);
end;
begin
  if ParamCount<>4 then raise Exception.Create('HOST_EXE WORKSPACE_CLI_EXE FIXTURE_EXE NEW_WORK_DIRECTORY required');
  HostExe:=ExpandFileName(ParamStr(1));WorkspaceExe:=ExpandFileName(ParamStr(2));FixtureExe:=ExpandFileName(ParamStr(3));
  WorkRoot:=ExcludeTrailingPathDelimiter(ExpandFileName(ParamStr(4)));
  Check(FileExists(HostExe) and FileExists(WorkspaceExe) and FileExists(FixtureExe),'three explicit native executables exist');
  Check(not DirectoryExists(WorkRoot) and not FileExists(WorkRoot),'new exact process evidence directory');
  Check(DirectoryExists(ExtractFileDir(WorkRoot)),'process evidence parent exists');
  Check(CreateDir(WorkRoot),'create exact work directory');LogRoot:=At(WorkRoot,'logs');Check(CreateDir(LogRoot),'create evidence log directory');
  Run;
  WriteLn('Native workspace process cases: ',Cases,'; host: ',HostCases,'; fixture: ',FixtureCases,
    '; workspace: ',WorkspaceCases,'; checks: ',Checks);
end.
