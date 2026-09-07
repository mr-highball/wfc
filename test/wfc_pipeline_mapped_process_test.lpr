{ SPDX-License-Identifier: MIT
  Copyright (c) 2021 mr-highball }
program wfc_pipeline_mapped_process_test;
{$mode delphi}{$H+}
{$IFDEF PAS2JS}{$FATAL mapped process tests require native FPC}{$ENDIF}
uses
  {$IFDEF UNIX}cthreads, BaseUnix,{$ENDIF}
  Classes, SysUtils, Process, Pipes, wfc_process_test_support, wfc_browser_socket,
  wfc, wfc_model, wfc_rule_model, wfc_rule_text, wfc_sequence, wfc_lattice,
  wfc_pipeline_layout, wfc_pipeline_model, wfc_pipeline_text,
  wfc_pipeline_run, wfc_pipeline_run_text, wfc_pipeline_result,
  wfc_pipeline_result_text;

const
  CHILD_TIMEOUT = 15000;
  CAPTURE_LIMIT = 2097152;
type
  {$IF DECLARED(TIODescriptor)}
  TDescriptorAccess = class(TIODescriptor)
  public function HasOwnHandle: Boolean;
  end;
  {$ENDIF}
  TInputWriter = class(TThread)
  private
    FChild: TProcess;
    FText, FError: String;
  protected procedure Execute; override;
  public
    constructor Create(const Child: TProcess; const Text: String);
    property ErrorText: String read FError;
  end;
  TOutcome = record
    Code: Integer;
    OutputText, ErrorText: String;
  end;
var
  Checks, Cases: Integer;
  Runner, Validator, Inspector, WorkRoot: String;
  RecipePath, RecipeText, OtherRecipePath: String;
  RunPaths, ResultPaths, RunTexts, ResultTexts: array[0..1] of String;

procedure Check(const Condition: Boolean; const Detail: String);
begin
  Inc(Checks);
  if not Condition then raise Exception.Create(Detail);
end;

function Elapsed(const Started: QWord): QWord;
var Now: QWord;
begin
  Now := WfcBrowserTickCount64;
  if Now >= Started then Result := Now-Started
  else Result := (High(QWord)-Started)+Now+1;
end;

constructor TInputWriter.Create(const Child: TProcess; const Text: String);
begin
  inherited Create(True); FreeOnTerminate := False;
  FChild := Child; FText := Text; Start;
end;

{$IF DECLARED(TIODescriptor)}
function TDescriptorAccess.HasOwnHandle: Boolean;
begin Result := OurHandle <> THandle(-1); end;
{$ENDIF}

procedure TInputWriter.Execute;
var Offset, Count, Written: Integer;
begin
  try
    try
      Offset := 0;
      while Offset < Length(FText) do
      begin
        Count := Length(FText)-Offset; if Count>4096 then Count:=4096;
        Written := FChild.Input.Write(FText[Offset+1],Count);
        if Written<=0 then raise EWriteError.Create('stdin write made no progress');
        Inc(Offset,Written);
      end;
    finally
      {$IF DECLARED(TIODescriptor)}
      { FCL 3.3.1 transfers the descriptor to the stream on first access.
        Close that actual owner exactly once before CloseInput supplies EOF. }
      if FChild.Input<>nil then
        if not TDescriptorAccess(FChild.InputDescriptor).HasOwnHandle then
        begin FChild.Input.DontClose:=True; FileClose(FChild.Input.Handle); end;
      {$ENDIF}
      FChild.CloseInput;
    end;
  except on E: Exception do FError:=E.Message; end;
end;

procedure ReadAvailable(const Pipe: TInputPipeStream; var Text: String);
var Buffer: array[0..4095] of Byte; Count, Offset: Integer;
begin
  while Pipe.NumBytesAvailable>0 do
  begin
    Count:=Pipe.NumBytesAvailable; if Count>SizeOf(Buffer) then Count:=SizeOf(Buffer);
    Count:=Pipe.Read(Buffer[0],Count); if Count<=0 then Exit;
    if Length(Text)>CAPTURE_LIMIT-Count then raise Exception.Create('child capture exceeds two MiB');
    Offset:=Length(Text); SetLength(Text,Offset+Count);
    Move(Buffer[0],Text[Offset+1],Count);
  end;
end;

function WaitWriter(const Writer: TInputWriter): Boolean;
var Started: QWord;
begin
  Started:=WfcBrowserTickCount64;
  repeat
    if Writer.Finished then Exit(True);
    Sleep(2);
  until Elapsed(Started)>=5000;
  Result:=Writer.Finished;
end;

function WaitChild(const Child: TProcess): Boolean;
var Started: QWord;
begin
  Started:=WfcBrowserTickCount64;
  repeat
    if not Child.Running then Exit(True);
    Sleep(2);
  until Elapsed(Started)>=5000;
  Result:=not Child.Running;
end;

function Invoke(const Executable: String; const Args: array of String;
  const Input: String=''): TOutcome;
var Child: TProcess; Writer: TInputWriter; Started: QWord; I: Integer;
begin
  Inc(Cases); Result:=Default(TOutcome); Result.Code:=-1;
  Child:=TProcess.Create(nil); Writer:=nil;
  try
    Child.Executable:=Executable; Child.CurrentDirectory:=WorkRoot;
    for I:=0 to High(Args) do Child.Parameters.Add(Args[I]);
    Child.Options:=[poUsePipes,poNoConsole]; Child.Execute;
    Writer:=TInputWriter.Create(Child,Input); Started:=WfcBrowserTickCount64;
    repeat
      ReadAvailable(Child.Output,Result.OutputText); ReadAvailable(Child.Stderr,Result.ErrorText);
      if not Child.Running then Break;
      if Elapsed(Started)>=CHILD_TIMEOUT then
        raise Exception.Create('case '+IntToStr(Cases)+' exceeded its finite process deadline');
      Sleep(2);
    until False;
    ReadAvailable(Child.Output,Result.OutputText); ReadAvailable(Child.Stderr,Result.ErrorText);
    Result.Code:=WfcProcessExitCode(Child);
    Check(WaitWriter(Writer),'bounded stdin writer completion');
    if Result.Code in [0,4] then Check(Writer.ErrorText='','successful artifact invocation consumes stdin');
    WriteLn('case ',Cases,': ',ExtractFileName(Executable),' exit=',Result.Code);
  finally
    if Child.Running then
    begin
      {$IFDEF UNIX}fpKill(Child.ProcessID,SIGKILL);{$ELSE}Child.Terminate(1);{$ENDIF}
      Check(WaitChild(Child),'owned child stopped within cleanup deadline');
    end;
    if Writer<>nil then
    begin
      if not WaitWriter(Writer) then raise Exception.Create('owned stdin writer did not stop');
      Writer.Free;
    end;
    Child.Free;
  end;
end;

function Success(const Executable: String; const Args: array of String;
  const Input, Detail: String; const ExpectedCode: Integer=0): String;
var Outcome: TOutcome; I: Integer; CanonicalBytes: Boolean;
begin
  Outcome:=Invoke(Executable,Args,Input);
  Check(Outcome.Code=ExpectedCode,Detail+': expected exit '+IntToStr(ExpectedCode)+
    ', got '+IntToStr(Outcome.Code)+'; '+Outcome.ErrorText);
  Check(Outcome.ErrorText='',Detail+': successful stderr empty');
  CanonicalBytes:=True;
  for I:=1 to Length(Outcome.OutputText) do
    if not ((Outcome.OutputText[I]=#10) or (Outcome.OutputText[I] in [#32..#126])) then
      CanonicalBytes:=False;
  Check(CanonicalBytes,Detail+': exact ASCII/LF process bytes');
  Result:=Outcome.OutputText;
end;

procedure Failure(const Executable: String; const Args: array of String;
  const Input, Prefix, Detail: String; const ExpectedCode: Integer=1);
var Outcome: TOutcome;
begin
  Outcome:=Invoke(Executable,Args,Input);
  Check(Outcome.Code=ExpectedCode,Detail+': expected user-input exit '+IntToStr(ExpectedCode)+
    ', got '+IntToStr(Outcome.Code)+'; '+Outcome.ErrorText);
  Check(Outcome.OutputText='',Detail+': invalid input publishes zero stdout bytes');
  Check((Pos(Prefix,Outcome.ErrorText)=1) and (Length(Outcome.ErrorText)<=2048),
    Detail+': bounded user-input diagnostic, not internal error');
  Check((Length(Outcome.ErrorText)>0) and (Outcome.ErrorText[Length(Outcome.ErrorText)]=#10)
    and (Pos(#13,Outcome.ErrorText)=0),Detail+': LF-terminated diagnostic');
end;

function OwnedPath(const Name: String): String;
begin
  Result:=ExpandFileName(IncludeTrailingPathDelimiter(WorkRoot)+Name);
  if Pos(IncludeTrailingPathDelimiter(WorkRoot),Result)<>1 then
    raise Exception.Create('fixture path escaped the newly owned directory');
end;

function WriteOwned(const Name, Text: String): String;
var Stream: TFileStream;
begin
  Result:=OwnedPath(Name);
  Check(not FileExists(Result) and not DirectoryExists(Result),'fixture output is exclusively new');
  Stream:=TFileStream.Create(Result,fmCreate);
  try if Text<>'' then Stream.WriteBuffer(Text[1],Length(Text)); finally Stream.Free; end;
end;

function ReadBytes(const Path: String): String;
var Stream: TFileStream;
begin
  Stream:=TFileStream.Create(Path,fmOpenRead or fmShareDenyNone);
  try
    Check(Stream.Size<=CAPTURE_LIMIT,'read fixture remains within test containment');
    SetLength(Result,Stream.Size); if Result<>'' then Stream.ReadBuffer(Result[1],Length(Result));
  finally Stream.Free; end;
end;

function OneToken(const Token: TWfcModelToken): TWfcModelTokens;
begin Result:=nil; SetLength(Result,1); Result[0]:=Token; end;

function NewRecipe(const Name: TWfcModelToken): TWfcPipelineModel;
const Names: array[0..2] of String=('terrain','foliage','housing');
  Values: array[0..2] of String=('land','clear','house');
  Pitches: array[0..2] of Integer=(8,1,4);
var Resources: TWfcPipelineResources; Passes: TWfcPipelinePasses;
  Edges: TWfcPipelineDependencies; Requirements: TWfcPipelineRequirements;
  Topologies: TWfcPipelinePassTopologies; Query: TWfcPipelineMappedQuery;
  Rules: TWfcRuleModel; Weights: TWfcModelIntegerArray; I: Integer;
begin
  SetLength(Resources,3); SetLength(Passes,3); SetLength(Topologies,3);
  SetLength(Weights,1); Weights[0]:=1;
  for I:=0 to 2 do
  begin
    Rules:=TWfcRuleModel.Create(2,OneToken(TWfcModelToken(Values[I])),Weights,nil);
    try Resources[I]:=MakeWfcPipelineResource(TWfcModelToken(Names[I]),wprkRules,
      EncodeWfcRuleText(Rules),'project-authored mapped process fixture','MIT','mapped-process-v1');
    finally Rules.Free; end;
    Passes[I]:=MakeWfcPipelinePass(TWfcModelToken(Names[I]),wppvPublic,gpmOverlay,-1,
      wpakRules,I,False,wseWhole);
    Topologies[I]:=MakeWfcPipelinePassTopology(2,MakeWfcLatticeVector(0,0,0),
      MakeWfcLatticeVector(Pitches[I],Pitches[I],1),False);
  end;
  SetLength(Edges,2); SetLength(Requirements,2);
  for I:=0 to 1 do
  begin
    Edges[I]:=MakeWfcPipelineDependency(I+1,I);
    Query:=Default(TWfcPipelineMappedQuery); Query.Kind:=gpmkCellCoverage; Query.Match:=gpmmAll;
    Query.AllowedProviderTokens:=OneToken(TWfcModelToken(Values[I]));
    Requirements[I]:=MakeWfcPipelineMappedRequirement(I+1,TWfcModelToken(Values[I+1]),I,Query);
  end;
  Result:=TWfcPipelineModel.Create(MakeWfcPipelineMetadata(Name,'MIT',
    'same recipe, independently requested extents','mapped-process-v1'),
    CurrentWfcPipelineVersions,2,False,rmBottomUp,Resources,Passes,Edges,nil,
    Requirements,nil,nil,1,Topologies);
end;

function NewRun(const Recipe: TWfcPipelineModel; const Width, Height: Integer;
  const NonSolved: Boolean=False): TWfcPipelineRun;
var Extents: TWfcPipelinePassExtents; Domains: TWfcPipelineCellDomains;
begin
  SetLength(Extents,3);
  Extents[0]:=MakeWfcLatticeVector(Width,Height,1);
  Extents[1]:=MakeWfcLatticeVector(8*Width,8*Height,1);
  Extents[2]:=MakeWfcLatticeVector(2*Width,2*Height,1);
  Domains:=nil;
  if NonSolved then
  begin
    SetLength(Domains,1);
    Domains[0]:=MakeWfcPipelineCellDomain(2,2*Width-1,2*Height-1,0,nil);
  end;
  Result:=TWfcPipelineRun.Create(Recipe,Extents,9,wpssOneWay,256,0,True,nil,Domains);
end;

procedure CheckOutput(const Recipe: TWfcPipelineModel; const Run: TWfcPipelineRun;
  const Text: String; const Width, Height: Integer);
const Values: array[0..2] of String=('land','clear','house');
var Output: TWfcPipelineResult; I,J: Integer;
begin
  Check(Pos('wfcpipeline-result=2'#10,Text)=1,'real runner emits result2');
  Output:=DecodeWfcPipelineResultText(Text,Recipe,Run);
  try
    Check(Output.Status=wprsSolved,'decoded real CLI result solved');
    Check((Output.TotalCellCount=69*Width*Height) and (Output.LayerCount=3),
      'CLI materializes actual per-pass total without a padded grid');
    for I:=0 to 2 do
    begin
      Check(SameWfcLatticeLayout(Output.LayerLayoutAt(I),Run.PassLayoutAt(I)),
        'CLI layer layout exactly binds requested extent');
      Check(Length(Output.LayerAt(I).Tokens)=Run.PassCellCount(I),'complete local layer returned');
      for J:=0 to Run.PassCellCount(I)-1 do
        Check(Output.LayerAt(I).Tokens[J]=Values[I],'real public token at every local cell');
    end;
    Check(EncodeWfcPipelineResultText(Output)=Text,'real emitted result canonical byte round trip');
  finally Output.Free; end;
end;

procedure TestExtent(const Recipe: TWfcPipelineModel; const Index, Width, Height: Integer);
var Run: TWfcPipelineRun; Report: String; LastIndex: Integer;
begin
  Run:=NewRun(Recipe,Width,Height);
  try
    RunTexts[Index]:=EncodeWfcPipelineRunText(Run);
    Check(Pos('wfcpipeline-run=2'#10,RunTexts[Index])=1,'explicit caller dimensions emit run2');
    RunPaths[Index]:=WriteOwned('run '+IntToStr(Index)+'.wfcrun',RunTexts[Index]);
    ResultTexts[Index]:=Success(Runner,[RecipePath,RunPaths[Index]],'','mapped generation');
    CheckOutput(Recipe,Run,ResultTexts[Index],Width,Height);
    ResultPaths[Index]:=WriteOwned('result '+IntToStr(Index)+'.wfcresult',ResultTexts[Index]);
    Check(Success(Runner,['-',RunPaths[Index]],RecipeText,'recipe stdin generation')=ResultTexts[Index],
      'recipe stdin and file generation bytes agree');
    Check(Success(Runner,[RecipePath,'-'],RunTexts[Index],'run stdin generation')=ResultTexts[Index],
      'run stdin and file generation bytes agree');
    Check(Success(Validator,['run','--emit-canonical',RecipePath,'-'],RunTexts[Index],
      'run canonical stdin')=RunTexts[Index],'validator preserves requested run bytes');
    Check(Success(Validator,['result','--replay','--emit-canonical',RecipePath,RunPaths[Index],
      ResultPaths[Index]],'','complete saved replay')=ResultTexts[Index],
      'fresh full-solution replay returns byte-identical saved result');
    Check(Success(Validator,['result','--emit-canonical',RecipePath,RunPaths[Index],'-'],
      ResultTexts[Index],'result canonical stdin')=ResultTexts[Index],
      'canonical result stdin survives full binding');
    Report:=Success(Inspector,['result','--limit','1000',RecipePath,RunPaths[Index],ResultPaths[Index]],'','inspect real result');
    Check(Pos('execution=not-run'#10,Report)>0,'inspection does not pretend to execute replay');
    Check(Pos('result-layout pass=1 rank=2 cells='+IntToStr(8*Width)+','+
      IntToStr(8*Height)+',1',Report)>0,'inspector uses foliage shape rather than terrain shape');
    Check(Pos('solved-public-mapped-policies=checked',Report)>0,'inspection discloses mapped validation scope');
    LastIndex:=4*Width*Height-1;
    Check(Pos('cell layer=2 index='+IntToStr(LastIndex)+' xyz='+IntToStr(2*Width-1)+','+
      IntToStr(2*Height-1)+',0 token=house world-min='+IntToStr(8*Width-4)+','+
      IntToStr(8*Height-4)+',0 world-max-exclusive='+IntToStr(8*Width)+','+
      IntToStr(8*Height)+',1',Report)>0,'last requested house has correct local and world bounds');
    Check(Success(Inspector,['result','--limit','1000',RecipePath,RunPaths[Index],'-'],ResultTexts[Index],
      'inspect result stdin')=Report,'inspection stdin/file exact byte parity');
  finally Run.Free; end;
end;

function Change(const Text, OldText, NewText: String): String;
begin
  Check(Pos(OldText,Text)>0,'malformed fixture anchor exists: '+OldText);
  Result:=StringReplace(Text,OldText,NewText,[]);
end;

procedure TestInvalidInputs;
var Text, BadRecipe, BadRun: String; I: Integer;
begin
  Failure(Runner,[OtherRecipePath,RunPaths[0]],'','wfc-run: invalid run: ','wrong recipe binding');
  Failure(Validator,['run',OtherRecipePath,RunPaths[0]],'',
    'wfc-validate: invalid run: ','validator wrong recipe');
  Failure(Inspector,['run',OtherRecipePath,RunPaths[0]],'',
    'wfc-inspect: invalid run: ','inspector wrong recipe');
  Failure(Validator,['result','--replay',RecipePath,RunPaths[1],ResultPaths[0]],'',
    'wfc-validate: invalid result: ','saved result rejects changed extent invocation');
  Failure(Inspector,['result',RecipePath,RunPaths[1],ResultPaths[0]],'',
    'wfc-inspect: invalid result: ','inspector rejects wrong extent invocation');
  for I:=0 to 1 do
  begin
    if I=0 then Text:=Change(RecipeText,'pass-topology=0,2,0,0,0,8,8,1,false',
      'pass-topology=0,2,0,0,0,0,8,1,false')
    else Text:=Change(RecipeText,'mapped=0,cell,all,0,0,0,0,0,0,0,0,1',
      'mapped=0,region,all,0,0,0,0,0,0,0,0,1');
    BadRecipe:=WriteOwned('bad recipe '+IntToStr(I)+'.wfcpipeline',Text);
    Failure(Runner,[BadRecipe,RunPaths[0]],'','wfc-run: invalid recipe: ','bad topology/query generation');
    Failure(Validator,['recipe','-'],Text,'wfc-validate: invalid recipe: ','bad topology/query stdin');
    Failure(Inspector,['recipe',BadRecipe],'','wfc-inspect: invalid recipe: ','bad topology/query inspection');
  end;
  Text:=Change(RunTexts[0],'extent=1,8,8,1','extent=1,0,8,1');
  BadRun:=WriteOwned('bad extent.wfcrun',Text);
  Failure(Runner,[RecipePath,BadRun],'','wfc-run: invalid run: ','invalid extent generation');
  Failure(Validator,['run',RecipePath,'-'],Text,'wfc-validate: invalid run: ','invalid extent stdin');
  Failure(Inspector,['run',RecipePath,BadRun],'','wfc-inspect: invalid run: ','invalid extent inspection');
  Check(ReadBytes(RecipePath)=RecipeText,'all failed commands preserve input recipe');
  for I:=0 to 1 do
  begin
    Check(ReadBytes(RunPaths[I])=RunTexts[I],'all failed commands preserve input run');
    Check(ReadBytes(ResultPaths[I])=ResultTexts[I],'all failed commands preserve saved result');
  end;
end;

procedure TestNonSolved(const Recipe: TWfcPipelineModel);
var Run: TWfcPipelineRun; Output: TWfcPipelineResult; RunText,RunPath,Text,Path,Report: String;
begin
  Run:=NewRun(Recipe,1,1,True);
  try
    RunText:=EncodeWfcPipelineRunText(Run); RunPath:=WriteOwned('nonsolved run.wfcrun',RunText);
    Text:=Success(Runner,[RecipePath,RunPath],'','valid contradictory invocation',4);
    Output:=DecodeWfcPipelineResultText(Text,Recipe,Run);
    try
      Check(Output.Status<>wprsSolved,'actual non-solved result remains non-solved');
      Check((Output.LayerCount=0) and (Output.PassCount=3),'non-solved artifact retains layouts, not stale layers');
    finally Output.Free; end;
    Path:=WriteOwned('nonsolved result.wfcresult',Text);
    Check(Success(Validator,['result','--replay','--emit-canonical',RecipePath,RunPath,Path],'',
      'valid non-solved replay')=Text,'valid non-solved replay succeeds with complete canonical bytes');
    Check(Success(Validator,['result','--replay','--quiet',RecipePath,RunPath,'-'],Text,
      'non-solved replay stdin quiet')='','valid non-solved replay is not a validation error');
    Report:=Success(Inspector,['result',RecipePath,RunPath,Path],'','inspect non-solved result');
    Check(Pos('solved-public-mapped-policies=not-applicable',Report)>0,
      'non-solved inspection does not claim absent solved policies');
  finally Run.Free; end;
end;

procedure TestOwnedFileCollision;
var Rejected: Boolean;
begin
  { The included CLIs publish stdout only; this is fixture containment, not a
    claim that the CLIs implement an output-file publication API. }
  Rejected:=False;
  try WriteOwned('result 0.wfcresult','must not replace saved CLI output');
  except on E: Exception do Rejected:=True; end;
  Check(Rejected,'test-owned saved output rejects an existing fixture path');
  Check(ReadBytes(ResultPaths[0])=ResultTexts[0],
    'test-owned collision leaves complete saved CLI bytes unchanged');
end;

procedure Main;
var Recipe, OtherRecipe: TWfcPipelineModel; Report: String;
begin
  {$IFDEF UNIX}fpSignal(SIGPIPE,SignalHandler(SIG_IGN));{$ENDIF}
  if ParamCount<>4 then raise Exception.Create(
    'usage: wfc_pipeline_mapped_process_test RUNNER VALIDATOR INSPECTOR NEW_WORK_DIRECTORY');
  Runner:=ExpandFileName(ParamStr(1)); Validator:=ExpandFileName(ParamStr(2));
  Inspector:=ExpandFileName(ParamStr(3)); WorkRoot:=ExcludeTrailingPathDelimiter(ExpandFileName(ParamStr(4)));
  Check(FileExists(Runner) and FileExists(Validator) and FileExists(Inspector),'all three included CLI executables exist');
  Check(not FileExists(WorkRoot) and not DirectoryExists(WorkRoot),'supplied work directory is exclusively new');
  Check(DirectoryExists(ExtractFileDir(WorkRoot)),'new work directory parent exists');
  Check(CreateDir(WorkRoot),'create exactly the requested fresh test directory');
  WriteLn('Owned mapped process evidence: ',WorkRoot);
  Recipe:=NewRecipe('Mapped process composition'); OtherRecipe:=nil;
  try
    RecipeText:=EncodeWfcPipelineModelText(Recipe);
    Check(Pos('wfcpipeline=5'#10,RecipeText)=1,'recipe uses spatial version5');
    RecipePath:=WriteOwned('recipe data.wfcpipeline',RecipeText);
    OtherRecipe:=NewRecipe('Different recipe identity');
    OtherRecipePath:=WriteOwned('other recipe.wfcpipeline',EncodeWfcPipelineModelText(OtherRecipe));
    Check(Success(Validator,['recipe','--emit-canonical','-'],RecipeText,'recipe canonical stdin')=RecipeText,
      'validator preserves recipe5 bytes');
    Report:=Success(Inspector,['recipe',RecipePath],'','inspect mapped recipe');
    Check(Pos('pass-topology pass=0 rank=2 origin=0,0,0 pitch=8,8,1 wrap=false',Report)>0,
      'real inspector exposes coarse terrain geometry');
    Check(Pos('mapped-requirement index=1 kind=cell consumer=2 token=house provider=1 match=all',Report)>0,
      'real inspector exposes the public mapped policy');
    TestExtent(Recipe,0,1,1); TestExtent(Recipe,1,3,2);
    Check((RunTexts[0]<>RunTexts[1]) and (ResultTexts[0]<>ResultTexts[1]),
      'independent 1x1 and 3x2 requests change invocation and output identities');
    Check(ReadBytes(RecipePath)=RecipeText,'one recipe stays byte-identical across dimensions');
    TestInvalidInputs; TestNonSolved(Recipe); TestOwnedFileCollision;
  finally OtherRecipe.Free; Recipe.Free; end;
end;

begin
  try
    Main;
    WriteLn('Mapped pipeline CLI process cases: ',Cases,'; checks: ',Checks,'/',Checks);
  except on E: Exception do
    begin WriteLn(StdErr,'wfc_pipeline_mapped_process_test: ',E.ClassName,': ',E.Message); Halt(1); end;
  end;
end.
