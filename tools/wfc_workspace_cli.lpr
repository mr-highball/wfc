{ SPDX-License-Identifier: MIT
  Copyright (c) 2021 mr-highball
  Native host for graph-free inspection and explicit workspace actions. }
program wfc_workspace_cli;
{$mode delphi}{$H+}
{$IFDEF PAS2JS}{$FATAL This host requires native FPC; the workspace itself is portable.}{$ENDIF}
uses Classes,SysUtils,wfc,wfc_pipeline_session,wfc_pipeline_workspace_journal,
  wfc_pipeline_workspace_journal_text,wfc_pipeline_workspace_replay,wfc_text_codec,
  wfc_atomic_new_file;
type EUsage=class(Exception);
const EXIT_USAGE=2; EXIT_INVALID=3; EXIT_REFUSED=4; EXIT_IO=5;
  EXIT_UNSOLVED_RECORDED=10;
var Policy: TWfcPipelineWorkspacePolicy; Command,InputPath,RecipePath,RunPath,
  OutputPath,RootText: String; Seen: TStringList; ExitStatus: Integer;
  RequestedRoots: TGraphPassIndices;
function ParseRoots: TGraphPassIndices; forward;

procedure Help;
begin
  WriteLn('WFC workspace: native FPC, complete explicit histories, no external runtime.');
  WriteLn('Commands: inspect, replay, begin, edit, initial, preview, repair');
  WriteLn('  inspect/replay: --input JOURNAL');
  WriteLn('  begin: [--input OLD_JOURNAL] --recipe RECIPE --run RUN --output NEW_JOURNAL');
  WriteLn('  edit: --input JOURNAL --run RUN --output NEW_JOURNAL');
  WriteLn('  initial: --input JOURNAL --output NEW_JOURNAL');
  WriteLn('  preview: --input JOURNAL --run RUN --roots 0,2');
  WriteLn('  repair: --input JOURNAL --run RUN --roots 0,2 --output NEW_JOURNAL');
  WriteLn('Roots are explicit ascending unique zero-based recipe pass indices.');
  WriteLn('Files only; embedded provenance never triggers path or URL access.');
  WriteLn('inspect does not execute; all other input-journal commands explicitly replay.');
  WriteLn('Outputs must be NEW files; existing files are never replaced.');
  WriteLn('--limit NAME=N sets positive logical policy limits; defaults are host policy,');
  WriteLn('not a composition-length, peak-memory or wall-clock guarantee. Names:');
  WriteLn('recipes,runs,context-bytes,actions,roots,evidence-bytes,journal-bytes,');
  WriteLn('input-cells,input-values,input-visits,public-cells,token-bytes,report-passes,');
  WriteLn('trace-events,excluded-values,outcome-bytes,outcome-lines,epochs,solves,');
  WriteLn('instantiated-cells,replay-evidence-bytes.');
  WriteLn('Exit0 accepted/verified;2 usage;3 invalid;4 refused;5 I/O;');
  WriteLn('10 normal unsolved attempt successfully recorded in the new output.');
end;

procedure DefaultPolicy;
begin
  Policy:=Default(TWfcPipelineWorkspacePolicy); Policy.Version:=1;
  with Policy.Journal do begin Version:=1; MaxRecipes:=1024; MaxRuns:=16384;
    MaxContextTextBytes:=67108864; MaxActions:=16384; MaxRootReferences:=65536;
    MaxEvidenceTextBytes:=67108864; MaxEncodedTextBytes:=268435456; end;
  with Policy.Replacement do begin Version:=1; MaxRetainedCellRecords:=1048576;
    MaxRetainedValueItems:=4194304; MaxCandidateVisits:=64000000; end;
  with Policy.Outcome do begin Version:=1; MaxPublicCellRecords:=1048576;
    MaxEncodedTokenBytes:=16777216; MaxReportPassRecords:=65536;
    MaxTraceEvents:=1048576; MaxExcludedAssignmentItems:=4194304; end;
  with Policy.Evidence do begin Version:=1; MaxTextBytes:=33554432; MaxLines:=1048576; end;
  with Policy.Replay do begin Version:=1; MaxEpochs:=1024; MaxSolveActions:=16384;
    MaxInstantiatedCellRecords:=16777216; MaxEvidenceTextBytes:=67108864; end;
end;

procedure SetLimit(const Text: String);
var P,N: Integer; Name: String;
begin
  P:=Pos('=',Text); if P<2 then raise EUsage.Create('--limit requires NAME=N');
  Name:=Copy(Text,1,P-1);
  if Seen.IndexOf('limit:'+Name)>=0 then raise EUsage.Create('duplicate limit '+Name);
  Seen.Add('limit:'+Name);
  try N:=WfcTextParseCanonicalInteger(Copy(Text,P+1,Length(Text)),'limit','workspace CLI');
  except on E:Exception do raise EUsage.Create(E.Message); end;
  if N<1 then raise EUsage.Create('limits must be positive');
  if Name='recipes' then Policy.Journal.MaxRecipes:=N
  else if Name='runs' then Policy.Journal.MaxRuns:=N
  else if Name='context-bytes' then Policy.Journal.MaxContextTextBytes:=N
  else if Name='actions' then Policy.Journal.MaxActions:=N
  else if Name='roots' then Policy.Journal.MaxRootReferences:=N
  else if Name='evidence-bytes' then Policy.Journal.MaxEvidenceTextBytes:=N
  else if Name='journal-bytes' then Policy.Journal.MaxEncodedTextBytes:=N
  else if Name='input-cells' then Policy.Replacement.MaxRetainedCellRecords:=N
  else if Name='input-values' then Policy.Replacement.MaxRetainedValueItems:=N
  else if Name='input-visits' then Policy.Replacement.MaxCandidateVisits:=N
  else if Name='public-cells' then Policy.Outcome.MaxPublicCellRecords:=N
  else if Name='token-bytes' then Policy.Outcome.MaxEncodedTokenBytes:=N
  else if Name='report-passes' then Policy.Outcome.MaxReportPassRecords:=N
  else if Name='trace-events' then Policy.Outcome.MaxTraceEvents:=N
  else if Name='excluded-values' then Policy.Outcome.MaxExcludedAssignmentItems:=N
  else if Name='outcome-bytes' then Policy.Evidence.MaxTextBytes:=N
  else if Name='outcome-lines' then Policy.Evidence.MaxLines:=N
  else if Name='epochs' then Policy.Replay.MaxEpochs:=N
  else if Name='solves' then Policy.Replay.MaxSolveActions:=N
  else if Name='instantiated-cells' then Policy.Replay.MaxInstantiatedCellRecords:=N
  else if Name='replay-evidence-bytes' then Policy.Replay.MaxEvidenceTextBytes:=N
  else raise EUsage.Create('unknown limit '+Name);
end;

procedure Arguments;
var I: Integer; Key,Value: String; Mutation: Boolean;
begin
  if ParamCount=0 then begin Command:='help'; Exit; end;
  Command:=ParamStr(1); if (Command='--help') or (Command='help') then
  begin if ParamCount<>1 then raise EUsage.Create('help accepts no options'); Command:='help'; Exit; end;
  if (Command<>'inspect') and (Command<>'replay') and (Command<>'begin') and
    (Command<>'edit') and (Command<>'initial') and (Command<>'preview') and
    (Command<>'repair') then raise EUsage.Create('unknown command');
  I:=2;
  while I<=ParamCount do
  begin
    Key:=ParamStr(I); Inc(I); if I>ParamCount then raise EUsage.Create('missing value for '+Key);
    Value:=ParamStr(I); Inc(I); if Value='' then raise EUsage.Create('empty value for '+Key);
    if Key='--limit' then begin SetLimit(Value); Continue; end;
    if Seen.IndexOf(Key)>=0 then raise EUsage.Create('duplicate option '+Key); Seen.Add(Key);
    if Key='--input' then InputPath:=Value
    else if Key='--recipe' then RecipePath:=Value
    else if Key='--run' then RunPath:=Value
    else if Key='--output' then OutputPath:=Value
    else if Key='--roots' then RootText:=Value
    else raise EUsage.Create('unknown option '+Key);
  end;
  if (Command<>'begin') and (InputPath='') then raise EUsage.Create('--input required');
  if (RecipePath<>'')<>(Command='begin') then raise EUsage.Create('--recipe is required only for begin');
  if (RunPath<>'')<>((Command='begin') or (Command='edit') or (Command='preview') or (Command='repair')) then
    raise EUsage.Create('--run is required only for begin/edit/preview/repair');
  if (RootText<>'')<>((Command='preview') or (Command='repair')) then raise EUsage.Create('--roots is required only for preview/repair');
  Mutation:=(Command='begin') or (Command='edit') or (Command='initial') or (Command='repair');
  if (OutputPath<>'')<>Mutation then raise EUsage.Create('--output is required only for authoring commands');
  { Parse syntax only after all policy overrides, before files or solver work.
    Recipe-bound upper bounds are checked later by the workspace owner. }
  if RootText<>'' then RequestedRoots:=ParseRoots;
end;

function ReadFileBounded(const Path: String; const Maximum: Integer): String;
var F: TFileStream; Count,Used,Amount: Integer; Extra: Byte;
begin
  F:=TFileStream.Create(Path,fmOpenRead or fmShareDenyWrite);
  try
    if (F.Size<0) or (F.Size>Maximum) then raise EConvertError.Create('input file exceeds selected byte policy');
    Count:=Integer(F.Size); SetLength(Result,Count); Used:=0;
    while Used<Count do
    begin
      Amount:=Count-Used; if Amount>65536 then Amount:=65536;
      Amount:=F.Read(Result[Used+1],Amount); if Amount<=0 then raise EReadError.Create('input changed or read made no progress');
      Inc(Used,Amount);
    end;
    if F.Read(Extra,1)<>0 then raise EReadError.Create('input grew while reading');
  finally F.Free; end;
end;

procedure PublishNew(const Text,Path: String);
var F: TWfcAtomicNewFile; Buffer: array of Byte; I,Used,Count: Integer;
begin
  F:=TWfcAtomicNewFile.Create(Path);
  try
    Used:=0;
    while Used<Length(Text) do
    begin
      Count:=Length(Text)-Used; if Count>65536 then Count:=65536;
      SetLength(Buffer,Count); for I:=0 to Count-1 do Buffer[I]:=Ord(Text[Used+I+1]);
      F.WriteBytes(Buffer); Inc(Used,Count);
    end;
    F.Publish;
    if F.CleanupError<>'' then WriteLn(StdErr,'workspace: ',F.CleanupError);
  finally F.Free; end;
end;

function ParseRoots: TGraphPassIndices;
var I,Start,N,Count: Integer;
begin
  Result:=nil; Start:=1;
  for I:=1 to Length(RootText)+1 do if (I>Length(RootText)) or (RootText[I]=',') then
  begin
    try N:=WfcTextParseCanonicalInteger(Copy(RootText,Start,I-Start),'root','workspace CLI');
    except on E:Exception do raise EUsage.Create(E.Message); end;
    Count:=Length(Result);
    if Count>=Policy.Journal.MaxRootReferences then raise EUsage.Create('root input exceeds selected root budget');
    if (Count>0) and (N<=Result[Count-1]) then raise EUsage.Create('roots must be ascending and unique');
    SetLength(Result,Count+1); Result[Count]:=N; Start:=I+1;
  end;
end;

procedure RunCommand;
var Journal: TWfcPipelineWorkspaceJournal; Slot: TWfcPipelineWorkspaceSlot;
  Receipt: TWfcPipelineWorkspaceReceipt; Preview: TWfcPipelineWorkspaceRepairPreview;
  RecipeText,RunText: String; Scope: TWfcPipelineSessionScope; I: Integer;
begin
  Journal:=nil; Slot:=nil; Receipt:=nil; Preview:=nil;
  try
    if InputPath<>'' then Journal:=DecodeWfcPipelineWorkspaceJournalText(
      ReadFileBounded(InputPath,Policy.Journal.MaxEncodedTextBytes),Policy.Journal);
    if Command='inspect' then
    begin
      WriteLn('verification=unverified-claims'); WriteLn('recipes=',Journal.RecipeCount);
      WriteLn('runs=',Journal.RunCount); WriteLn('actions=',Journal.ActionCount);
      WriteLn('journal-bytes=',Journal.EncodedTextBytes); WriteLn('solver-executed=0'); Exit;
    end;
    if RecipePath<>'' then RecipeText:=ReadFileBounded(RecipePath,Policy.Journal.MaxContextTextBytes);
    if RunPath<>'' then RunText:=ReadFileBounded(RunPath,Policy.Journal.MaxContextTextBytes);
    Slot:=TWfcPipelineWorkspaceSlot.Create;
    if Journal<>nil then Slot.Restore(Journal,Policy.Journal,Policy.Replacement,Policy.Outcome,Policy.Evidence,Policy.Replay,0);
    if Command='begin' then Receipt:=Slot.BeginEpoch(RecipeText,RunText,Policy,Slot.PublicationRevision)
    else if Command='edit' then Receipt:=Slot.ApplyInputs(RunText,Policy,Slot.PublicationRevision)
    else if Command='initial' then Receipt:=Slot.ExecuteInitial(Policy,Slot.PublicationRevision)
    else if Command='repair' then Receipt:=Slot.ExecuteRepair(RunText,RequestedRoots,Policy,Slot.PublicationRevision)
    else if Command='preview' then
    begin
      Preview:=Slot.PreviewRepair(RunText,RequestedRoots,Policy,Slot.PublicationRevision);
      WriteLn('can-execute=',Ord(Preview.CanExecute)); WriteLn('missing-baseline=',Ord(Preview.MissingBaseline));
      Scope:=Preview.CopyScope; Write('missing-passes=');
      for I:=0 to High(Scope.MissingPassIndices) do begin if I>0 then Write(','); Write(Scope.MissingPassIndices[I]); end;
      WriteLn; if not Preview.CanExecute then ExitStatus:=EXIT_REFUSED;
    end;
    if Receipt<>nil then
    begin
      PublishNew(Slot.CopyCanonicalJournal,OutputPath);
      WriteLn('recorded-action=',Receipt.ActionIndex);
      if (Receipt.BorrowSolveOutcome<>nil) and not Receipt.BorrowSolveOutcome.Solved then ExitStatus:=EXIT_UNSOLVED_RECORDED;
    end;
    WriteLn('execution=verified-retained'); WriteLn('current-output=',Ord(Slot.HasCurrentOutput));
    WriteLn('successful-baseline=',Ord(Slot.HasSuccessfulBaseline)); WriteLn('session-revision=',Slot.SessionRevision);
  finally Preview.Free; Receipt.Free; Slot.Free; Journal.Free; end;
end;

function OneLine(const Text: String): String;
var I: Integer;
begin Result:=Text; for I:=1 to Length(Result) do if Ord(Result[I])<32 then Result[I]:=' '; end;
begin
  Seen:=TStringList.Create; ExitStatus:=0;
  try
    try DefaultPolicy; Arguments; if Command='help' then Help else RunCommand;
    except
      on E:EUsage do begin WriteLn(StdErr,'workspace: ',OneLine(E.Message)); ExitStatus:=EXIT_USAGE; end;
      on E:EWfcPipelineWorkspaceReplay do begin WriteLn(StdErr,'workspace: ',OneLine(E.Message)); ExitStatus:=EXIT_REFUSED; end;
      on E:EInOutError do begin WriteLn(StdErr,'workspace: ',OneLine(E.Message)); ExitStatus:=EXIT_IO; end;
      on E:EStreamError do begin WriteLn(StdErr,'workspace: ',OneLine(E.Message)); ExitStatus:=EXIT_IO; end;
      on E:Exception do begin WriteLn(StdErr,'workspace: ',OneLine(E.Message)); ExitStatus:=EXIT_INVALID; end;
    end;
  finally Seen.Free; end;
  Halt(ExitStatus);
end.
