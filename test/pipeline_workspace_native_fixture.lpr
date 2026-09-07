{ SPDX-License-Identifier: MIT
  Copyright (c) 2021 mr-highball
  Independent native artifact/replay/SVG verifier; no demo-unit imports. }
program pipeline_workspace_native_fixture;
{$mode delphi}{$H+}
uses Classes,SysUtils,wfc,wfc_model,wfc_lattice,wfc_pipeline_model,
  wfc_pipeline_run,wfc_pipeline_text,wfc_pipeline_run_text,wfc_pipeline_session,
  wfc_pipeline_workspace_journal,wfc_pipeline_workspace_journal_text,
  wfc_pipeline_workspace_replay,wfc_text_codec,wfc_atomic_new_file;
const MAX_FILE_BYTES=67108864;
var Checks: Integer;
procedure Check(const Ok: Boolean; const Detail: String);
begin Inc(Checks); if not Ok then raise Exception.Create(Detail); end;
function PathAt(const Directory,Name: String): String;
begin Result:=IncludeTrailingPathDelimiter(Directory)+Name; end;
function ReadText(const Path: String): String;
var F:TFileStream;
begin
  F:=TFileStream.Create(Path,fmOpenRead or fmShareDenyWrite);
  try
    Check((F.Size>=0) and (F.Size<=MAX_FILE_BYTES),'fixture file byte bound');
    SetLength(Result,Integer(F.Size)); if Length(Result)>0 then F.ReadBuffer(Result[1],Length(Result));
  finally F.Free; end;
end;
procedure WriteNew(const Path,Text: String);
var F:TWfcAtomicNewFile; B:array of Byte; Used,N,I:Integer;
begin
  F:=TWfcAtomicNewFile.Create(Path);
  try
    Used:=0;
    while Used<Length(Text) do
    begin
      N:=Length(Text)-Used; if N>65536 then N:=65536; SetLength(B,N);
      for I:=0 to N-1 do B[I]:=Ord(Text[Used+I+1]); F.WriteBytes(B); Inc(Used,N);
    end;
    F.Publish; Check(F.CleanupError='','fixture publication cleanup');
  finally F.Free; end;
end;
function VerificationPolicy:TWfcPipelineWorkspacePolicy;
begin
  Result:=Default(TWfcPipelineWorkspacePolicy); Result.Version:=1;
  with Result.Journal do begin Version:=1;MaxRecipes:=16;MaxRuns:=64;MaxContextTextBytes:=67108864;
    MaxActions:=64;MaxRootReferences:=256;MaxEvidenceTextBytes:=67108864;MaxEncodedTextBytes:=67108864;end;
  with Result.Replacement do begin Version:=1;MaxRetainedCellRecords:=1048576;
    MaxRetainedValueItems:=4194304;MaxCandidateVisits:=64000000;end;
  with Result.Outcome do begin Version:=1;MaxPublicCellRecords:=1048576;MaxEncodedTokenBytes:=16777216;
    MaxReportPassRecords:=65536;MaxTraceEvents:=1048576;MaxExcludedAssignmentItems:=4194304;end;
  with Result.Evidence do begin Version:=1;MaxTextBytes:=33554432;MaxLines:=1048576;end;
  with Result.Replay do begin Version:=1;MaxEpochs:=16;MaxSolveActions:=64;
    MaxInstantiatedCellRecords:=16777216;MaxEvidenceTextBytes:=67108864;end;
end;
function Vec(const V:TWfcLatticeVector):String;
begin Result:=IntToStr(V.X)+','+IntToStr(V.Y)+','+IntToStr(V.Z);end;
function Bit(const V:Boolean):String;
begin if V then Result:='1' else Result:='0';end;
procedure Attribute(const Text,Name,Value:String);
begin Check(Pos(Name+'="'+Value+'"',Text)>0,'exact SVG attribute '+Name+'='+Value);end;
function Occurrences(const Text,Needle:String):Integer;
var Start,P:Integer;
begin
  Result:=0;Start:=1;
  repeat
    P:=Pos(Needle,Copy(Text,Start,Length(Text)));
    if P=0 then Exit; Inc(Result); Start:=Start+P-1+Length(Needle);
  until False;
end;
procedure Verify(const Directory,Kind:String);
var R:TWfcPipelineModel; U,Replayed:TWfcPipelineRun; J:TWfcPipelineWorkspaceJournal;
  E:TWfcPipelineWorkspaceExecution; State:TWfcPipelineSessionPublicState;
  P:TWfcPipelineWorkspacePolicy; L:TWfcLatticeLayout; Layer:TWfcPipelineSessionLayer;
  Cell:TWfcPipelineSessionCell; RecipeText,RunText,JournalText,Svg,Group,ExpectedStatus:String;
  ExpectedCells,ExpectedPasses,Width,Height,PassIndex,X,Y,Index,I,Start,Stop:Integer;
  Minimum,Maximum:TWfcLatticeVector; Found,Solved:Boolean;
begin
  P:=VerificationPolicy; R:=nil;U:=nil;J:=nil;E:=nil;State:=nil;Replayed:=nil;
  RecipeText:=ReadText(PathAt(Directory,'recipe.wfc'));RunText:=ReadText(PathAt(Directory,'run.wfc'));
  JournalText:=ReadText(PathAt(Directory,'journal.wfc'));Svg:=ReadText(PathAt(Directory,'selected-pass.svg'));
  ExpectedPasses:=3;ExpectedCells:=822;PassIndex:=2;Width:=3;Height:=2;Solved:=Kind<>'unsolved';
  if Kind='larger' then begin ExpectedCells:=3299;Width:=7;Height:=5;end
  else if Kind='sequence' then begin ExpectedPasses:=4;ExpectedCells:=51;Width:=12;Height:=1;end
  else if Kind='long-sequence' then begin ExpectedPasses:=4;ExpectedCells:=207;Width:=32;Height:=1;end
  else Check((Kind='landscape') or (Kind='unsolved'),'known independent verification case');
  try
    R:=DecodeWfcPipelineModelText(RecipeText);U:=DecodeWfcPipelineRunText(RunText,R);
    Check(EncodeWfcPipelineModelText(R)=RecipeText,'canonical complete recipe');
    Check(EncodeWfcPipelineRunText(U)=RunText,'canonical complete run');
    Check((R.PassCount=ExpectedPasses) and (U.TotalCellCount=ExpectedCells),'independent expected geometry inventory');
    Check(R.HasPassMapping and (U.FormatVersion=2),'explicit spatial artifacts retained');
    L:=U.PassLayoutAt(PassIndex);
    if Kind='larger' then
    begin
      Check((L.Cells.X=7) and (L.Cells.Y=5) and (L.Origin.X=-28) and (L.Origin.Y=-20),
        'larger signed housing layout');
      Check((U.PassLayoutAt(0).Cells.X=16) and (U.PassLayoutAt(0).Cells.Y=12) and
        (U.PassLayoutAt(1).Cells.X=64) and (U.PassLayoutAt(1).Cells.Y=48),'unlike provider grids');
      Check((U.Seed=55) and (U.MaxBacktracks=256) and (U.MaxPassBacktracks=32),'explicit invocation settings');
    end
    else Check(U.Seed=7,'default/imported seed retained');
    if (Kind='sequence') or (Kind='long-sequence') then
    begin
      Check((R.ResourceCount=2) and (R.BridgeCount=1) and (R.DependencyCount=2),'learned-resource/projection structure');
      Check((R.PassAt(1).Visibility=wppvPrivate) and (R.PassAt(3).Mode=gpmTransform),
        'private source and public alias retained');
      if Kind='long-sequence' then Check((L.Cells.X=64) and (Width=32),'view32 does not reduce64-cell composition');
      Check(SameWfcLatticeLayout(U.PassLayoutAt(1),U.PassLayoutAt(2)) and
        SameWfcLatticeLayout(U.PassLayoutAt(2),U.PassLayoutAt(3)),'exact linked layouts');
    end
    else Check((R.RequirementCount=3) and (R.ResourceCount=3),'three real landscape resources/clauses');
    if not Solved then Check((U.DomainCount=1) and (Length(U.DomainAt(0).AllowedTokens)=0),
      'explicit empty domain is the actual unsolved input');
    J:=DecodeWfcPipelineWorkspaceJournalText(JournalText,P.Journal);
    Check(J.Verification=wpwvUnverifiedClaims,'decoded claims are not trusted execution');
    Check((J.ActionCount=2) and (J.ActionAt(0).Kind=wpwakBeginEpoch) and
      (J.ActionAt(1).Kind=wpwakInitial),'begin and actual initial both retained');
    Check(J.RecipeTextAt(0)=RecipeText,'journal recipe exact full bytes');
    Check(J.RunTextAt(J.ActionAt(1).RunIndex).Text=RunText,'journal applied run exact full bytes');
    Check((J.ActionAt(0).EvidenceText='') and (J.ActionAt(1).EvidenceText<>''),'actual complete outcome claim present');
    E:=ReplayWfcPipelineWorkspace(J,P.Journal,P.Replacement,P.Outcome,P.Evidence,P.Replay);
    Check((E.VerifiedActions=2) and (E.EpochCount=1),'full actual replay verified both actions');
    Check(E.CopyCanonicalJournal=JournalText,'full journal replay byte equality');
    Check((E.HasCurrentOutput=Solved) and (E.HasSuccessfulBaseline=Solved),'actual replay output/baseline flags');
    Replayed:=E.CopyAppliedRun;Check(EncodeWfcPipelineRunText(Replayed)=RunText,'actual replay applied input equality');
    State:=E.CopyPublicState;Found:=False;
    for I:=0 to State.LayerCount-1 do
    begin Layer:=State.LayerAt(I);if Layer.PassIndex=PassIndex then begin Found:=True;Break;end;end;
    Check(Found,'selected public pass exists in actual replay');
    Check(SameWfcLatticeLayout(L,Layer.Layout),'replay geometry matches exported run');
    if Solved then ExpectedStatus:='current' else ExpectedStatus:='not-current';
    Attribute(Svg,'data-workspace-view','1');Attribute(Svg,'data-status',ExpectedStatus);
    Attribute(Svg,'data-pass',IntToStr(PassIndex));Attribute(Svg,'data-cells',Vec(L.Cells));
    Attribute(Svg,'data-origin',Vec(L.Origin));Attribute(Svg,'data-pitch',Vec(L.Pitch));
    Attribute(Svg,'data-local-window','0,0,0,'+IntToStr(Width)+','+IntToStr(Height));
    Check(Occurrences(Svg,'<g data-cell-index=')=Width*Height,'exact viewport cell inventory');
    for Y:=0 to Height-1 do for X:=0 to Width-1 do
    begin
      Index:=Y*L.Cells.X+X;Cell:=Layer.Cells[Index];
      Start:=Pos('<g data-cell-index="'+IntToStr(Index)+'" ',Svg);Check(Start>0,'exact exported cell ordinal');
      Stop:=Pos('</g>',Copy(Svg,Start,Length(Svg)));Check(Stop>0,'complete SVG cell group');
      Group:=Copy(Svg,Start,Stop+3);
      Attribute(Group,'data-cell',IntToStr(X)+','+IntToStr(Y)+',0');
      Attribute(Group,'data-token',WfcTextEncodeToken(Cell.Token,'independent fixture'));
      Attribute(Group,'data-empty',Bit(Cell.Empty));Attribute(Group,'data-generated',Bit(Cell.Generated));
      Minimum:=MakeWfcLatticeVector(L.Origin.X+X*L.Pitch.X,L.Origin.Y+Y*L.Pitch.Y,L.Origin.Z);
      Maximum:=MakeWfcLatticeVector(Minimum.X+L.Pitch.X,Minimum.Y+L.Pitch.Y,Minimum.Z+L.Pitch.Z);
      Attribute(Group,'data-world-min',Vec(Minimum));Attribute(Group,'data-world-max',Vec(Maximum));
    end;
    Check(Length(Svg)>0,'nonempty SVG');Check(Svg[Length(Svg)]=#10,'exact final SVG LF');
  finally Replayed.Free;State.Free;E.Free;J.Free;U.Free;R.Free;end;
end;
procedure MakeUnsolved(const SourceDirectory,NewDirectory:String);
var R:TWfcPipelineModel;U,V:TWfcPipelineRun;D:TWfcPipelineCellDomains;Text:String;
begin
  Check(not DirectoryExists(NewDirectory) and not FileExists(NewDirectory),'new unsolved fixture directory');
  Check(DirectoryExists(ExtractFileDir(ExpandFileName(NewDirectory))),'fixture parent exists');
  R:=nil;U:=nil;V:=nil;
  try
    Text:=ReadText(PathAt(SourceDirectory,'recipe.wfc'));R:=DecodeWfcPipelineModelText(Text);
    U:=DecodeWfcPipelineRunText(ReadText(PathAt(SourceDirectory,'run.wfc')),R);
    SetLength(D,1);D[0]:=MakeWfcPipelineCellDomain(2,0,0,0,nil);
    V:=TWfcPipelineRun.Create(R,U.CopyPassExtents,U.Seed,U.Strategy,U.MaxBacktracks,
      U.MaxPassBacktracks,U.CaptureTrace,nil,D);
    Check(V.DomainCount=1,'actual explicit empty-domain fixture');
    Check(CreateDir(NewDirectory),'create exact new fixture directory');
    WriteNew(PathAt(NewDirectory,'recipe.wfc'),Text);
    WriteNew(PathAt(NewDirectory,'run.wfc'),EncodeWfcPipelineRunText(V));
  finally V.Free;U.Free;R.Free;end;
end;
begin
  if ParamCount<>3 then raise Exception.Create('fixture --verify DIRECTORY CASE | --make-unsolved SOURCE_DIRECTORY NEW_DIRECTORY');
  if ParamStr(1)='--verify' then Verify(ParamStr(2),ParamStr(3))
  else if ParamStr(1)='--make-unsolved' then MakeUnsolved(ParamStr(2),ParamStr(3))
  else raise Exception.Create('unknown fixture command');
  WriteLn('Native workspace fixture checks: ',Checks);
end.
