{ SPDX-License-Identifier: MIT
  Copyright (c) 2021 mr-highball
  Native initial-workspace demo host with canonical artifacts and SVG output. }
program PipelineWorkspace;
{$mode delphi}{$H+}
{$IFDEF PAS2JS}{$FATAL This file is the native CLI; its workbench/presets/view are portable.}{$ENDIF}
uses Classes, SysUtils, wfc, wfc_lattice, wfc_sequence, wfc_model,
  wfc_pipeline_model, wfc_pipeline_layout, wfc_pipeline_run,
  wfc_pipeline_workspace_context, wfc_pipeline_workspace_replay,
  wfc_pipeline_session, wfc_pipeline_text, wfc_pipeline_run_text,
  wfc_text_codec, wfc_atomic_new_file,
  pipeline_workspace_workbench, pipeline_workspace_presets, pipeline_workspace_view;

type
  EUsage = class(Exception);
  TFields = array of String;
const
  EXIT_USAGE=2; EXIT_INVALID=3; EXIT_REFUSED=4; EXIT_IO=5;
  EXIT_UNSOLVED_RECORDED=10;
var
  Policy: TWfcPipelineWorkspacePolicy;
  View: TWfcPipelineWorkspaceSlice;
  RunOptions: TWfcWorkspacePresetRunOptions;
  Weights: TWfcWorkspaceLandscapeWeights;
  SequenceOptions: TWfcWorkspaceSequenceOptions;
  Seen,PassOptions: TStringList;
  PresetName,RecipePath,RunPath,OutputDirectory: String;
  HasExplicitView,DirectoryCreated: Boolean;
  FilesPublished,ExitStatus: Integer;

procedure Usage;
begin
  WriteLn('Pipeline Workspace: native FPC initial generation and exact workspace export.');
  WriteLn('  --preset landscape|sequence --output-dir NEW_DIRECTORY');
  WriteLn('  --recipe RECIPE_FILE --run RUN_FILE --output-dir NEW_DIRECTORY');
  WriteLn('Preset-only options (never silently override imported documents):');
  WriteLn('  --pass INDEX,RANK,CX,CY,CZ,OX,OY,OZ,PX,PY,PZ,WRAP');
  WriteLn('    repeat for distinct pass indices; WRAP=0|1, signed origin, positive pitch/cells.');
  WriteLn('  --seed N --strategy one-way|negotiated --local-backtracks N --pass-backtracks N --trace 0|1');
  WriteLn('  landscape: --weights LAND,WATER,CLEAR,TREE,HOUSE,VACANT');
  WriteLn('  sequence: --order N --boundary open|wrap --extent whole|prefix|suffix|fragment|wrap');
  WriteLn('Presentation only (composition extent is set by --pass or the imported run):');
  WriteLn('  --view PASS,X,Y,Z,WIDTH,HEIGHT --cell-pixels N');
  WriteLn('  --limit view-cells=N --limit svg-bytes=N');
  WriteLn('Default view: first public pass, origin/Z0, up to8x6 cells; explicit windows are never clipped.');
  WriteLn('--limit NAME=N: adjustable positive version1 logical limits, not peak heap. Names:');
  WriteLn('  recipes,runs,context-bytes,actions,roots,evidence-bytes,journal-bytes,');
  WriteLn('  input-cells,input-values,input-visits,public-cells,token-bytes,report-passes,');
  WriteLn('  trace-events,excluded-values,outcome-bytes,outcome-lines,epochs,solves,');
  WriteLn('  instantiated-cells,replay-evidence-bytes,view-cells,svg-bytes.');
  WriteLn('Outputs: recipe.wfc, run.wfc, journal.wfc, selected-pass.svg in one NEW directory.');
  WriteLn('Parent must exist. Each file is no-replace published; the four-file set is NOT atomic.');
  WriteLn('No server, seed search, regeneration fallback, physical safety claim or file overwrite.');
  WriteLn('Continue the saved journal with the included wfc_workspace inspect/replay/edit/preview/repair commands.');
  WriteLn('Exit0 solved and exported;2 usage;3 invalid;4 refused;5 I/O;10 normal unsolved exported.');
end;

procedure Defaults;
begin
  RunOptions:=DefaultWorkspacePresetRunOptions; Weights:=DefaultWorkspaceLandscapeWeights;
  SequenceOptions:=DefaultWorkspaceSequenceOptions;
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
  View:=Default(TWfcPipelineWorkspaceSlice); View.Version:=1;
  View.CellPixels:=32; View.MaxRenderedCells:=4096; View.MaxSvgBytes:=8388608;
  View.UseBaseline:=False;
end;

function Number(const Text,Name: String; const Minimum: Integer=0): Integer;
begin
  try Result:=WfcTextParseCanonicalInteger(Text,Name,'Pipeline Workspace CLI');
  except on E:EConvertError do raise EUsage.Create(E.Message); end;
  if Result<Minimum then raise EUsage.Create(Name+' is below its minimum');
end;
function SignedNumber(const Text,Name: String): Integer;
begin
  try Result:=WfcTextParseCanonicalSignedInteger(Text,Name,'Pipeline Workspace CLI');
  except on E:EConvertError do raise EUsage.Create(E.Message); end;
end;
function Bit(const Text,Name: String): Boolean;
begin
  if Text='0' then Exit(False);
  if Text='1' then Exit(True);
  raise EUsage.Create(Name+' must be0 or1');
end;
procedure Unique(const Name: String);
begin
  if Seen.IndexOf(Name)>=0 then raise EUsage.Create('duplicate option '+Name);
  Seen.Add(Name);
end;
function Fields(const Text: String; const Expected: Integer): TFields;
var I,Start,Count: Integer;
begin
  SetLength(Result,Expected); Count:=0; Start:=1;
  for I:=1 to Length(Text)+1 do
    if (I>Length(Text)) or (Text[I]=',') then
    begin
      if Count>=Expected then raise EUsage.Create('too many comma-separated fields');
      if I=Start then raise EUsage.Create('empty comma-separated field');
      Result[Count]:=Copy(Text,Start,I-Start); Inc(Count); Start:=I+1;
    end;
  if Count<>Expected then raise EUsage.Create('wrong comma-separated field count');
end;

procedure SetLimit(const Text: String);
var P,N: Integer; Name: String;
begin
  P:=Pos('=',Text); if P<2 then raise EUsage.Create('--limit requires NAME=N');
  Name:=Copy(Text,1,P-1); Unique('limit:'+Name);
  N:=Number(Copy(Text,P+1,Length(Text)),Name,1);
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
  else if Name='view-cells' then View.MaxRenderedCells:=N
  else if Name='svg-bytes' then View.MaxSvgBytes:=N
  else raise EUsage.Create('unknown limit '+Name);
end;

procedure ParseArguments;
var I,N: Integer; Key,Value: String; V: TFields;
begin
  I:=1;
  while I<=ParamCount do
  begin
    Key:=ParamStr(I); Inc(I);
    if I>ParamCount then raise EUsage.Create('missing value for '+Key);
    Value:=ParamStr(I); Inc(I);
    if (Value='') or (Pos(#0,Value)>0) then raise EUsage.Create('empty/NUL value for '+Key);
    if Key='--limit' then begin SetLimit(Value); Continue; end;
    if Key='--pass' then
    begin
      V:=Fields(Value,12); N:=Number(V[0],'pass index');
      Unique('pass:'+IntToStr(N)); PassOptions.Add(Value); Continue;
    end;
    Unique(Key);
    if Key='--preset' then PresetName:=Value
    else if Key='--recipe' then RecipePath:=Value
    else if Key='--run' then RunPath:=Value
    else if Key='--output-dir' then OutputDirectory:=Value
    else if Key='--seed' then
    begin
      try RunOptions.Seed:=WfcTextParseCanonicalCardinal(Value,'seed','Pipeline Workspace CLI');
      except on E:EConvertError do raise EUsage.Create(E.Message); end;
    end
    else if Key='--strategy' then
    begin
      if Value='one-way' then RunOptions.Strategy:=wpssOneWay
      else if Value='negotiated' then RunOptions.Strategy:=wpssNegotiated
      else raise EUsage.Create('strategy must be one-way or negotiated');
    end
    else if Key='--local-backtracks' then RunOptions.MaxBacktracks:=Number(Value,'local backtracks')
    else if Key='--pass-backtracks' then RunOptions.MaxPassBacktracks:=Number(Value,'pass backtracks')
    else if Key='--trace' then RunOptions.CaptureTrace:=Bit(Value,'trace')
    else if Key='--weights' then
    begin
      V:=Fields(Value,6); Weights.Land:=Number(V[0],'land weight',1);
      Weights.Water:=Number(V[1],'water weight',1); Weights.Clear:=Number(V[2],'clear weight',1);
      Weights.Tree:=Number(V[3],'tree weight',1); Weights.House:=Number(V[4],'house weight',1);
      Weights.Vacant:=Number(V[5],'vacant weight',1);
    end
    else if Key='--order' then SequenceOptions.Order:=Number(Value,'sequence order',1)
    else if Key='--boundary' then
    begin
      if Value='open' then SequenceOptions.Boundary:=wmbOpen
      else if Value='wrap' then SequenceOptions.Boundary:=wmbWrap
      else raise EUsage.Create('source boundary must be open or wrap');
    end
    else if Key='--extent' then
    begin
      if Value='whole' then SequenceOptions.Extent:=wseWhole
      else if Value='prefix' then SequenceOptions.Extent:=wsePrefix
      else if Value='suffix' then SequenceOptions.Extent:=wseSuffix
      else if Value='fragment' then SequenceOptions.Extent:=wseFragment
      else if Value='wrap' then SequenceOptions.Extent:=wseWrap
      else raise EUsage.Create('unknown sequence extent');
    end
    else if Key='--view' then
    begin
      V:=Fields(Value,6); HasExplicitView:=True;
      View.PassIndex:=Number(V[0],'view pass'); View.StartX:=Number(V[1],'view X');
      View.StartY:=Number(V[2],'view Y'); View.SliceZ:=Number(V[3],'view Z');
      View.Width:=Number(V[4],'view width',1); View.Height:=Number(V[5],'view height',1);
    end
    else if Key='--cell-pixels' then View.CellPixels:=Number(Value,'cell pixels',1)
    else raise EUsage.Create('unknown option '+Key);
  end;
  if OutputDirectory='' then raise EUsage.Create('--output-dir NEW_DIRECTORY is required');
  if PresetName<>'' then
  begin
    if (PresetName<>'landscape') and (PresetName<>'sequence') then raise EUsage.Create('unknown preset');
    if (RecipePath<>'') or (RunPath<>'') then raise EUsage.Create('preset and imported definition modes are exclusive');
    if (PresetName='sequence') and (Seen.IndexOf('--weights')>=0) then raise EUsage.Create('--weights is landscape-only');
    if (PresetName='landscape') and ((Seen.IndexOf('--order')>=0) or (Seen.IndexOf('--boundary')>=0) or
      (Seen.IndexOf('--extent')>=0)) then raise EUsage.Create('sequence options require --preset sequence');
  end
  else
  begin
    if (RecipePath='') or (RunPath='') then raise EUsage.Create('choose --preset or both --recipe and --run');
    for I:=0 to Seen.Count-1 do
    begin
      Key:=Seen[I];
      if (Pos('pass:',Key)=1) or (Key='--seed') or (Key='--strategy') or
        (Key='--local-backtracks') or (Key='--pass-backtracks') or (Key='--trace') or
        (Key='--weights') or (Key='--order') or (Key='--boundary') or (Key='--extent') then
        raise EUsage.Create('preset-only option cannot silently rewrite imported documents: '+Key);
    end;
  end;
  OutputDirectory:=ExcludeTrailingPathDelimiter(ExpandFileName(OutputDirectory));
  if FileExists(OutputDirectory) or DirectoryExists(OutputDirectory) then
    raise EUsage.Create('output directory must be NEW');
  if not DirectoryExists(ExtractFileDir(OutputDirectory)) then
    raise EUsage.Create('output directory parent must already exist');
end;

function ReadBounded(const Path: String; const Maximum: Integer): String;
var F: TFileStream; Count,Used,N: Integer; Extra: Byte;
begin
  F:=TFileStream.Create(Path,fmOpenRead or fmShareDenyWrite);
  try
    if (F.Size<0) or (F.Size>Maximum) then raise EConvertError.Create('input exceeds remaining context byte limit');
    Count:=Integer(F.Size); SetLength(Result,Count); Used:=0;
    while Used<Count do
    begin
      N:=Count-Used; if N>65536 then N:=65536;
      N:=F.Read(Result[Used+1],N);
      if N<=0 then raise EReadError.Create('input changed or read made no progress');
      Inc(Used,N);
    end;
    if F.Read(Extra,1)<>0 then raise EReadError.Create('input grew during reading');
  finally F.Free; end;
end;

procedure ApplyGeometry(var T: TWfcPipelinePassTopologies; var E: TWfcPipelinePassExtents);
var I,P,Rank: Integer; V: TFields;
begin
  for I:=0 to PassOptions.Count-1 do
  begin
    V:=Fields(PassOptions[I],12); P:=Number(V[0],'pass index');
    if P>=Length(T) then raise EUsage.Create('pass index is outside selected preset');
    Rank:=Number(V[1],'rank',1);
    E[P]:=MakeWfcLatticeVector(Number(V[2],'cells X',1),Number(V[3],'cells Y',1),Number(V[4],'cells Z',1));
    T[P]:=MakeWfcPipelinePassTopology(Rank,
      MakeWfcLatticeVector(SignedNumber(V[5],'origin X'),SignedNumber(V[6],'origin Y'),SignedNumber(V[7],'origin Z')),
      MakeWfcLatticeVector(Number(V[8],'pitch X',1),Number(V[9],'pitch Y',1),Number(V[10],'pitch Z',1)),
      Bit(V[11],'wrap'));
  end;
end;

function Definition: TWfcPipelineWorkspaceContexts;
var T: TWfcPipelinePassTopologies; E: TWfcPipelinePassExtents;
  L: TWfcPipelineWorkspaceContextLimits; RecipeText,RunText: String;
begin
  L.Version:=1; L.MaxRecipes:=Policy.Journal.MaxRecipes; L.MaxRuns:=Policy.Journal.MaxRuns;
  L.MaxTextBytes:=Policy.Journal.MaxContextTextBytes;
  if PresetName='' then
  begin
    RecipeText:=ReadBounded(RecipePath,L.MaxTextBytes);
    RunText:=ReadBounded(RunPath,L.MaxTextBytes-Length(RecipeText));
    Exit(TWfcPipelineWorkspaceWorkbench.InspectDefinition(RecipeText,RunText,L));
  end;
  if PresetName='landscape' then DefaultMappedLandscapeGeometry(T,E)
  else DefaultLearnedSequenceGeometry(T,E);
  ApplyGeometry(T,E);
  if PresetName='landscape' then
    Result:=BuildMappedLandscapePreset(T,E,RunOptions,Weights,nil,nil,L)
  else Result:=BuildLearnedSequencePreset(T,E,RunOptions,SequenceOptions,nil,nil,L);
end;

procedure SelectView(const C: TWfcPipelineWorkspaceContexts);
var P: Integer; Layout: TWfcLatticeLayout; Found: Boolean;
begin
  if not HasExplicitView then
  begin
    Found:=False;
    for P:=0 to C.BorrowRecipe(0).PassCount-1 do
      if C.BorrowRecipe(0).PassAt(P).Visibility=wppvPublic then
      begin View.PassIndex:=P; Found:=True; Break; end;
    if not Found then raise EUsage.Create('this visual demo requires at least one public pass');
    Layout:=C.BorrowRun(0).PassLayoutAt(View.PassIndex);
    View.Width:=Layout.Cells.X; if View.Width>8 then View.Width:=8;
    View.Height:=Layout.Cells.Y; if View.Height>6 then View.Height:=6;
  end;
  if View.PassIndex>=C.BorrowRecipe(0).PassCount then raise EUsage.Create('view pass is out of range');
  if C.BorrowRecipe(0).PassAt(View.PassIndex).Visibility<>wppvPublic then
    raise EUsage.Create('view pass must be public; private state is not exposed as a public grid');
  Layout:=C.BorrowRun(0).PassLayoutAt(View.PassIndex);
  if (View.StartX>=Layout.Cells.X) or (View.Width>Layout.Cells.X-View.StartX) or
    (View.StartY>=Layout.Cells.Y) or (View.Height>Layout.Cells.Y-View.StartY) or
    (View.SliceZ>=Layout.Cells.Z) then raise EUsage.Create('exact view window is outside selected pass');
  if View.Width>View.MaxRenderedCells div View.Height then raise EUsage.Create('view cell allowance exceeded');
end;

procedure PublishNew(const FileName,Text: String);
var F: TWfcAtomicNewFile; Bytes: array of Byte; Count,Used,I: Integer; Path: String;
begin
  Path:=IncludeTrailingPathDelimiter(OutputDirectory)+FileName;
  F:=TWfcAtomicNewFile.Create(Path);
  try
    Used:=0;
    while Used<Length(Text) do
    begin
      Count:=Length(Text)-Used; if Count>65536 then Count:=65536;
      SetLength(Bytes,Count);
      for I:=0 to Count-1 do Bytes[I]:=Ord(Text[Used+I+1]);
      F.WriteBytes(Bytes); Inc(Used,Count);
    end;
    F.Publish; Inc(FilesPublished);
    WriteLn('published=',Path);
    if F.CleanupError<>'' then WriteLn(StdErr,'Pipeline Workspace: ',F.CleanupError);
  finally F.Free; end;
end;

function Digit(const Value: Boolean): String;
begin if Value then Result:='1' else Result:='0'; end;

procedure Generate;
var C: TWfcPipelineWorkspaceContexts; W: TWfcPipelineWorkspaceWorkbench;
  Receipt: TWfcPipelineWorkspaceReceipt; R: TWfcPipelineModel; U: TWfcPipelineRun;
  RecipeText,RunText,JournalText,Svg: String; Solved: Boolean;
begin
  C:=nil; W:=nil; Receipt:=nil; R:=nil; U:=nil;
  try
    C:=Definition; SelectView(C);
    W:=TWfcPipelineWorkspaceWorkbench.Create;
    Receipt:=W.BeginEpoch(C.RecipeTextAt(0),C.RunTextAt(0).Text,Policy,0);
    FreeAndNil(Receipt);
    Receipt:=W.ExecuteInitial(Policy,W.PublicationRevision);
    Solved:=Receipt.BorrowSolveOutcome.Solved;
    { The same controller supplies every exported definition/state/history.
      An unsolved receipt is normal history; rendering marks it NOT CURRENT. }
    R:=W.CopyCurrentRecipe; U:=W.CopyAppliedRun;
    RecipeText:=EncodeWfcPipelineModelText(R); RunText:=EncodeWfcPipelineRunText(U);
    JournalText:=W.CopyCanonicalJournal; Svg:=PipelineWorkspaceSliceSvg(W,View);
    WriteLn('solved=',Digit(Solved),' current=',Digit(W.HasCurrentOutput),
      ' baseline=',Digit(W.HasSuccessfulBaseline),' publication-revision=',W.PublicationRevision,
      ' session-revision=',W.SessionRevision);
    WriteLn('passes=',R.PassCount,' cells=',U.TotalCellCount,' seed=',U.Seed,
      ' local-backtracks=',U.MaxBacktracks,' pass-backtracks=',U.MaxPassBacktracks);
    WriteLn('view=',View.PassIndex,',',View.StartX,',',View.StartY,',',View.SliceZ,',',View.Width,',',View.Height);
    { No file or directory is created until complete generation, canonical
      serialization and exact-window rendering have all returned normally. }
    if not CreateDir(OutputDirectory) then raise EWriteError.Create('cannot exclusively create new output directory');
    DirectoryCreated:=True;
    PublishNew('recipe.wfc',RecipeText); PublishNew('run.wfc',RunText);
    PublishNew('journal.wfc',JournalText); PublishNew('selected-pass.svg',Svg);
    if Solved then ExitStatus:=0 else ExitStatus:=EXIT_UNSOLVED_RECORDED;
  finally U.Free; R.Free; Receipt.Free; W.Free; C.Free; end;
end;

begin
  ExitStatus:=0; Seen:=TStringList.Create; PassOptions:=TStringList.Create;
  Seen.CaseSensitive:=True;
  try
    try
      if (ParamCount=0) or ((ParamCount=1) and (ParamStr(1)='--help')) then Usage
      else begin Defaults; ParseArguments; Generate; end;
    except
      on E:EUsage do begin WriteLn(StdErr,'Pipeline Workspace: ',E.Message); ExitStatus:=EXIT_USAGE; end;
      on E:EWfcPipelineWorkspaceReplay do
      begin WriteLn(StdErr,'Pipeline Workspace refused: ',E.Message); ExitStatus:=EXIT_REFUSED; end;
      on E:EStreamError do begin WriteLn(StdErr,'Pipeline Workspace I/O: ',E.Message); ExitStatus:=EXIT_IO; end;
      on E:EOSError do begin WriteLn(StdErr,'Pipeline Workspace I/O: ',E.Message); ExitStatus:=EXIT_IO; end;
      on E:EInOutError do begin WriteLn(StdErr,'Pipeline Workspace I/O: ',E.Message); ExitStatus:=EXIT_IO; end;
      on E:Exception do begin WriteLn(StdErr,'Pipeline Workspace invalid: ',E.Message); ExitStatus:=EXIT_INVALID; end;
    end;
    if DirectoryCreated and (ExitStatus<>0) and (ExitStatus<>EXIT_UNSOLVED_RECORDED) then
      WriteLn(StdErr,'Output directory retained; ',FilesPublished,
        ' file(s) were published before failure. Multi-file export is not a directory transaction: ',OutputDirectory);
  finally PassOptions.Free; Seen.Free; end;
  Halt(ExitStatus);
end.
