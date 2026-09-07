{ SPDX-License-Identifier: MIT
  Copyright (c) 2021 mr-highball
  Shared workspace controller; no graph or shadow history escapes. }
unit pipeline_workspace_workbench;
{$mode delphi}{$H+}
interface
uses SysUtils, wfc, wfc_model, wfc_pipeline_layout, wfc_pipeline_model,
  wfc_pipeline_run, wfc_pipeline_session, wfc_pipeline_workspace_context,
  wfc_pipeline_workspace_journal, wfc_pipeline_workspace_replay;
type
  EWfcPipelineWorkspaceWorkbench = class(Exception);
  TWfcPipelineWorkspaceWorkbench = class
  strict private
    FSlot: TWfcPipelineWorkspaceSlot;
    procedure RequireExpected(const ExpectedRevision: Integer);
    function GetHasExecution: Boolean;
    function GetHasCurrentOutput: Boolean;
    function GetHasSuccessfulBaseline: Boolean;
    function GetPublicationRevision: Integer;
    function GetSessionRevision: Integer;
    function GetCurrentRecipeIndex: Integer;
    function GetCurrentRunIndex: Integer;
    function EditCell(const Operation, PassIndex, X, Y, Z: Integer;
      const Token: TWfcModelToken; const AllowedTokens: TWfcModelTokens;
      const Policy: TWfcPipelineWorkspacePolicy;
      const ExpectedRevision: Integer): TWfcPipelineWorkspaceReceipt;
  public
    constructor Create;
    destructor Destroy; override;
    { Graph-free inspectors do not consult or mutate a live controller. }
    class function InspectDefinition(const RecipeText, RunText: String;
      const Limits: TWfcPipelineWorkspaceContextLimits): TWfcPipelineWorkspaceContexts; static;
    class function InspectJournal(const Text: String;
      const Limits: TWfcPipelineWorkspaceJournalLimits): TWfcPipelineWorkspaceJournal; static;
    function BeginEpoch(const RecipeText, RunText: String;
      const Policy: TWfcPipelineWorkspacePolicy;
      const ExpectedRevision: Integer): TWfcPipelineWorkspaceReceipt;
    procedure RestoreJournal(const Text: String;
      const Policy: TWfcPipelineWorkspacePolicy; const ExpectedRevision: Integer);
    function ApplyRun(const Text: String; const Policy: TWfcPipelineWorkspacePolicy;
      const ExpectedRevision: Integer): TWfcPipelineWorkspaceReceipt;
    function SetCellLock(const PassIndex, X, Y, Z: Integer;
      const Token: TWfcModelToken; const Policy: TWfcPipelineWorkspacePolicy;
      const ExpectedRevision: Integer): TWfcPipelineWorkspaceReceipt;
    function ClearCellLock(const PassIndex, X, Y, Z: Integer;
      const Policy: TWfcPipelineWorkspacePolicy;
      const ExpectedRevision: Integer): TWfcPipelineWorkspaceReceipt;
    function SetCellDomain(const PassIndex, X, Y, Z: Integer;
      const AllowedTokens: TWfcModelTokens; const Policy: TWfcPipelineWorkspacePolicy;
      const ExpectedRevision: Integer): TWfcPipelineWorkspaceReceipt;
    function ClearCellDomain(const PassIndex, X, Y, Z: Integer;
      const Policy: TWfcPipelineWorkspacePolicy;
      const ExpectedRevision: Integer): TWfcPipelineWorkspaceReceipt;
    function ExecuteInitial(const Policy: TWfcPipelineWorkspacePolicy;
      const ExpectedRevision: Integer): TWfcPipelineWorkspaceReceipt;
    function PreviewRepair(const RunText: String; const Roots: TGraphPassIndices;
      const Policy: TWfcPipelineWorkspacePolicy;
      const ExpectedRevision: Integer): TWfcPipelineWorkspaceRepairPreview;
    function ExecuteRepair(const RunText: String; const Roots: TGraphPassIndices;
      const Policy: TWfcPipelineWorkspacePolicy;
      const ExpectedRevision: Integer): TWfcPipelineWorkspaceReceipt;
    function BeginMappedEpoch(const Topologies: TWfcPipelinePassTopologies;
      const Extents: TWfcPipelinePassExtents; const Seed: TGraphSeed;
      const Policy: TWfcPipelineWorkspacePolicy;
      const ExpectedRevision: Integer): TWfcPipelineWorkspaceReceipt;
    function CopyCurrentRecipe: TWfcPipelineModel;
    function CopyAppliedRun: TWfcPipelineRun;
    function CopyCanonicalJournal: String;
    function CopyPublicState: TWfcPipelineSessionPublicState;
    function CopyLastSuccessfulState: TWfcPipelineSessionPublicState;
    property HasExecution: Boolean read GetHasExecution;
    property HasCurrentOutput: Boolean read GetHasCurrentOutput;
    property HasSuccessfulBaseline: Boolean read GetHasSuccessfulBaseline;
    property PublicationRevision: Integer read GetPublicationRevision;
    property SessionRevision: Integer read GetSessionRevision;
    property CurrentRecipeIndex: Integer read GetCurrentRecipeIndex;
    property CurrentRunIndex: Integer read GetCurrentRunIndex;
  end;
implementation
uses wfc_lattice, wfc_pipeline_text, wfc_pipeline_run_text,
  wfc_pipeline_workspace_journal_text;

procedure WorkbenchError(const Detail: String);
begin
  raise EWfcPipelineWorkspaceWorkbench.Create('pipeline workspace workbench: '+Detail);
end;

procedure RequireInteger(const Value, Minimum, Maximum: Integer;
  const Name: String);
{$IFDEF PAS2JS}var Valid: Boolean;{$ENDIF}
begin
  {$IFDEF PAS2JS}
  asm Valid=typeof Value==='number' && Number.isInteger(Value); end;
  if not Valid then WorkbenchError(Name+' must be an exact finite Integer');
  {$ENDIF}
  if (Value<Minimum) or (Value>Maximum) then WorkbenchError(Name+' is out of range');
end;

procedure RequireSeed(const Seed: TGraphSeed);
{$IFDEF PAS2JS}var Valid: Boolean;{$ENDIF}
begin
  {$IFDEF PAS2JS}
  asm Valid=typeof Seed==='number' && Number.isInteger(Seed) && Seed>=0 && Seed<=4294967295; end;
  if not Valid then WorkbenchError('seed must be an exact Cardinal');
  {$ENDIF}
end;

procedure RequirePrimitiveText(const Text: String);
{$IFDEF PAS2JS}var Valid: Boolean;{$ENDIF}
begin
  {$IFDEF PAS2JS}
  asm Valid=typeof Text==='string'; end;
  if not Valid then WorkbenchError('document must be a primitive string');
  {$ENDIF}
end;

procedure RequirePolicy(const P: TWfcPipelineWorkspacePolicy);
{$IFDEF PAS2JS}var Valid: Boolean;{$ENDIF}
begin
  {$IFDEF PAS2JS}
  { Check the whole raw tree before passing any record member to a Pascal
    routine. In particular Restore takes nested limits, not the outer policy. }
  asm
    function data(o,k) {
      if(o===null || typeof o!=='object' || Array.isArray(o)) return undefined;
      let d;
      while(o!==null && !(d=Object.getOwnPropertyDescriptor(o,k))) o=Object.getPrototypeOf(o);
      return d && Object.prototype.hasOwnProperty.call(d,'value') ? d : undefined;
    }
    function positive(o,names) {
      for(const name of names) {
        const d=data(o,name);
        if(!d || typeof d.value!=='number' || !Number.isInteger(d.value) ||
          d.value<1 || d.value>2147483647 || (name==='Version' && d.value!==1)) return false;
      }
      return true;
    }
    const version=data(P,'Version');
    Valid=!!version && version.value===1;
    const groups=[
      ['Journal',['Version','MaxRecipes','MaxRuns','MaxContextTextBytes','MaxActions','MaxRootReferences','MaxEvidenceTextBytes','MaxEncodedTextBytes']],
      ['Replacement',['Version','MaxRetainedCellRecords','MaxRetainedValueItems','MaxCandidateVisits']],
      ['Outcome',['Version','MaxPublicCellRecords','MaxEncodedTokenBytes','MaxReportPassRecords','MaxTraceEvents','MaxExcludedAssignmentItems']],
      ['Evidence',['Version','MaxTextBytes','MaxLines']],
      ['Replay',['Version','MaxEpochs','MaxSolveActions','MaxInstantiatedCellRecords','MaxEvidenceTextBytes']]
    ];
    if(Valid) for(const group of groups) {
      const d=data(P,group[0]);
      if(!d || !positive(d.value,group[1])) {Valid=false;break;}
    }
  end;
  if not Valid then WorkbenchError('complete passive positive version1 policy required');
  {$ENDIF}
  if (P.Version<>1) or (P.Journal.Version<>1) or
    (P.Journal.MaxRecipes<1) or (P.Journal.MaxRuns<1) or
    (P.Journal.MaxContextTextBytes<1) or (P.Journal.MaxActions<1) or
    (P.Journal.MaxRootReferences<1) or (P.Journal.MaxEvidenceTextBytes<1) or
    (P.Journal.MaxEncodedTextBytes<1) or
    (P.Replacement.Version<>1) or (P.Replacement.MaxRetainedCellRecords<1) or
    (P.Replacement.MaxRetainedValueItems<1) or (P.Replacement.MaxCandidateVisits<1) or
    (P.Outcome.Version<>1) or (P.Outcome.MaxPublicCellRecords<1) or
    (P.Outcome.MaxEncodedTokenBytes<1) or (P.Outcome.MaxReportPassRecords<1) or
    (P.Outcome.MaxTraceEvents<1) or (P.Outcome.MaxExcludedAssignmentItems<1) or
    (P.Evidence.Version<>1) or (P.Evidence.MaxTextBytes<1) or (P.Evidence.MaxLines<1) or
    (P.Replay.Version<>1) or (P.Replay.MaxEpochs<1) or (P.Replay.MaxSolveActions<1) or
    (P.Replay.MaxInstantiatedCellRecords<1) or (P.Replay.MaxEvidenceTextBytes<1) then
      WorkbenchError('unsupported or nonpositive workspace policy');
end;

procedure RequireToken(const Token: TWfcModelToken);
{$IFDEF PAS2JS}var Valid: Boolean;{$ENDIF}
begin
  {$IFDEF PAS2JS}
  asm Valid=typeof Token==='string'; end;
  if not Valid then WorkbenchError('cell token must be a primitive string');
  {$ENDIF}
  if (Length(Token)>WFC_PIPELINE_MAX_ENCODED_TOKEN_LENGTH) or
    not WfcModelTokenIsValid(Token) then WorkbenchError('invalid cell token');
end;

function OwnedDomain(const Values, Vocabulary: TWfcModelTokens): TWfcModelTokens;
var I, Cursor: Integer;
{$IFDEF PAS2JS}Valid: Boolean;{$ENDIF}
begin
  Result:=nil;
  {$IFDEF PAS2JS}
  asm
    Valid=Array.isArray(Values) && Values.length<=pas.wfc_pipeline_run.WFC_PIPELINE_RUN_MAX_DOMAIN_TOKEN_COUNT;
    if(Valid) for(let i=0;i<Values.length;i++) {
      const d=Object.getOwnPropertyDescriptor(Values,String(i));
      if(!d || !Object.prototype.hasOwnProperty.call(d,'value') || typeof d.value!=='string') {Valid=false;break;}
    }
  end;
  if not Valid then WorkbenchError('domain requires bounded dense passive token entries');
  {$ENDIF}
  if Length(Values)>WFC_PIPELINE_RUN_MAX_DOMAIN_TOKEN_COUNT then
    WorkbenchError('domain token count exceeds the run envelope');
  Cursor:=0;
  for I:=0 to High(Values) do
  begin
    RequireToken(Values[I]);
    while (Cursor<Length(Vocabulary)) and (Vocabulary[Cursor]<>Values[I]) do Inc(Cursor);
    if Cursor=Length(Vocabulary) then
      WorkbenchError('domain tokens must be known and in strict public vocabulary order');
    Inc(Cursor);
  end;
  { Never call Copy/slice or arrayRef on the caller's array. Own slice overrides
    are irrelevant data; dense validated indices alone determine the result. }
  SetLength(Result,Length(Values));
  for I:=0 to High(Values) do Result[I]:=Values[I];
end;

function CompareCell(const AP, AX, AY, AZ, BP, BX, BY, BZ: Integer): Integer;
begin
  Result:=0;
  if AP<BP then Exit(-1); if AP>BP then Exit(1);
  if AZ<BZ then Exit(-1); if AZ>BZ then Exit(1);
  if AY<BY then Exit(-1); if AY>BY then Exit(1);
  if AX<BX then Exit(-1); if AX>BX then Exit(1);
end;

function RunWithInputs(const Recipe: TWfcPipelineModel; const Original: TWfcPipelineRun;
  const Locks: TWfcPipelineCellLocks; const Domains: TWfcPipelineCellDomains): TWfcPipelineRun;
begin
  { Preserve even a legacy recipe explicitly bound to run2; do not choose the
    output format merely from HasPassMapping. }
  if Original.FormatVersion=WFC_PIPELINE_RUN_VERSION then
    Result:=TWfcPipelineRun.Create(Recipe,Original.Width,Original.Height,Original.Depth,
      Original.Seed,Original.Strategy,Original.MaxBacktracks,Original.MaxPassBacktracks,
      Original.CaptureTrace,Locks,Domains)
  else if Original.FormatVersion=WFC_PIPELINE_RUN_MAPPED_VERSION then
    Result:=TWfcPipelineRun.Create(Recipe,Original.CopyPassExtents,Original.Seed,
      Original.Strategy,Original.MaxBacktracks,Original.MaxPassBacktracks,
      Original.CaptureTrace,Locks,Domains)
  else begin WorkbenchError('unsupported applied run format'); Result:=nil; end;
end;

procedure RequireGeometryCounts(const Topologies: TWfcPipelinePassTopologies;
  const Extents: TWfcPipelinePassExtents; const PassCount: Integer);
{$IFDEF PAS2JS}var Valid: Boolean;{$ENDIF}
begin
  {$IFDEF PAS2JS}
  asm
    Valid=Array.isArray(Topologies) && Array.isArray(Extents) &&
      Topologies.length===PassCount && Extents.length===PassCount;
  end;
  if not Valid then raise EWfcPipelineLayoutTable.Create('one topology and extent per existing pass required');
  {$ENDIF}
  if (Length(Topologies)<>PassCount) or (Length(Extents)<>PassCount) then
    raise EWfcPipelineLayoutTable.Create('one topology and extent per existing pass required');
end;

constructor TWfcPipelineWorkspaceWorkbench.Create;
begin inherited Create; FSlot:=TWfcPipelineWorkspaceSlot.Create; end;

destructor TWfcPipelineWorkspaceWorkbench.Destroy;
begin FSlot.Free; inherited Destroy; end;

procedure TWfcPipelineWorkspaceWorkbench.RequireExpected(const ExpectedRevision: Integer);
{$IFDEF PAS2JS}var Valid: Boolean;{$ENDIF}
begin
  {$IFDEF PAS2JS}
  asm Valid=typeof ExpectedRevision==='number' && Number.isInteger(ExpectedRevision); end;
  if not Valid then raise EWfcPipelineWorkspaceReplay.CreateFailure(
    wpwrfStalePublication,-1,0,'publication revision must be an Integer');
  {$ENDIF}
  if (ExpectedRevision<0) or (ExpectedRevision<>FSlot.PublicationRevision) then
    raise EWfcPipelineWorkspaceReplay.CreateFailure(wpwrfStalePublication,-1,0,
      'stale publication revision');
end;

class function TWfcPipelineWorkspaceWorkbench.InspectDefinition(const RecipeText, RunText: String;
  const Limits: TWfcPipelineWorkspaceContextLimits): TWfcPipelineWorkspaceContexts;
var Recipes: TWfcPipelineWorkspaceRecipeTexts; Runs: TWfcPipelineWorkspaceRunTexts;
begin
  RequirePrimitiveText(RecipeText); RequirePrimitiveText(RunText);
  SetLength(Recipes,1); Recipes[0]:=RecipeText;
  SetLength(Runs,1); Runs[0].RecipeIndex:=0; Runs[0].Text:=RunText;
  Result:=TWfcPipelineWorkspaceContexts.Create(Recipes,Runs,Limits);
end;

class function TWfcPipelineWorkspaceWorkbench.InspectJournal(const Text: String;
  const Limits: TWfcPipelineWorkspaceJournalLimits): TWfcPipelineWorkspaceJournal;
begin
  RequirePrimitiveText(Text);
  Result:=DecodeWfcPipelineWorkspaceJournalText(Text,Limits);
end;

function TWfcPipelineWorkspaceWorkbench.BeginEpoch(const RecipeText, RunText: String;
  const Policy: TWfcPipelineWorkspacePolicy; const ExpectedRevision: Integer): TWfcPipelineWorkspaceReceipt;
begin
  RequireExpected(ExpectedRevision); RequirePolicy(Policy);
  Result:=FSlot.BeginEpoch(RecipeText,RunText,Policy,ExpectedRevision);
end;

procedure TWfcPipelineWorkspaceWorkbench.RestoreJournal(const Text: String;
  const Policy: TWfcPipelineWorkspacePolicy; const ExpectedRevision: Integer);
var Journal: TWfcPipelineWorkspaceJournal;
begin
  RequireExpected(ExpectedRevision); RequirePolicy(Policy);
  Journal:=InspectJournal(Text,Policy.Journal);
  try
    FSlot.Restore(Journal,Policy.Journal,Policy.Replacement,Policy.Outcome,
      Policy.Evidence,Policy.Replay,ExpectedRevision);
  finally Journal.Free; end;
end;

function TWfcPipelineWorkspaceWorkbench.ApplyRun(const Text: String;
  const Policy: TWfcPipelineWorkspacePolicy; const ExpectedRevision: Integer): TWfcPipelineWorkspaceReceipt;
begin
  RequireExpected(ExpectedRevision); RequirePolicy(Policy);
  Result:=FSlot.ApplyInputs(Text,Policy,ExpectedRevision);
end;

function TWfcPipelineWorkspaceWorkbench.EditCell(const Operation, PassIndex, X, Y, Z: Integer;
  const Token: TWfcModelToken; const AllowedTokens: TWfcModelTokens;
  const Policy: TWfcPipelineWorkspacePolicy; const ExpectedRevision: Integer): TWfcPipelineWorkspaceReceipt;
var Recipe: TWfcPipelineModel; Original, Desired: TWfcPipelineRun;
  Locks: TWfcPipelineCellLocks; Domains: TWfcPipelineCellDomains;
  Vocabulary, Domain: TWfcModelTokens; Layout: TWfcLatticeLayout;
  I, Position, Count: Integer; Exists, Found: Boolean;
begin
  Result:=nil; RequireExpected(ExpectedRevision); RequirePolicy(Policy);
  RequireInteger(PassIndex,0,WFC_PIPELINE_MAX_PASS_COUNT-1,'pass index');
  RequireInteger(X,0,High(Integer),'cell X'); RequireInteger(Y,0,High(Integer),'cell Y');
  RequireInteger(Z,0,High(Integer),'cell Z');
  Recipe:=nil; Original:=nil; Desired:=nil;
  try
    Recipe:=FSlot.CopyCurrentRecipe;
    if PassIndex>=Recipe.PassCount then WorkbenchError('pass index is outside current recipe');
    if Recipe.PassAt(PassIndex).Visibility<>wppvPublic then WorkbenchError('cell inputs require a public pass');
    Original:=FSlot.CopyAppliedRun; Layout:=Original.PassLayoutAt(PassIndex);
    if (X>=Layout.Cells.X) or (Y>=Layout.Cells.Y) or (Z>=Layout.Cells.Z) then
      WorkbenchError('cell is outside its local pass extent');
    { Validate the target even for clear-absent. No graph is involved. }
    if (Operation=0) or (Operation=2) then
    begin
      Vocabulary:=Recipe.CopyPublicVocabulary(PassIndex);
      if Operation=0 then
      begin
        RequireToken(Token); Found:=False;
        for I:=0 to High(Vocabulary) do if Vocabulary[I]=Token then begin Found:=True; Break; end;
        if not Found then WorkbenchError('lock token is outside public vocabulary');
      end
      else Domain:=OwnedDomain(AllowedTokens,Vocabulary);
    end;
    Locks:=Original.CopyLocks; Domains:=Original.CopyDomains;
    if (Operation=0) or (Operation=1) then
    begin
      Position:=0; Count:=Length(Locks);
      while (Position<Count) and (CompareCell(Locks[Position].PassIndex,
        Locks[Position].X,Locks[Position].Y,Locks[Position].Z,PassIndex,X,Y,Z)<0) do Inc(Position);
      Exists:=(Position<Count) and (CompareCell(Locks[Position].PassIndex,
        Locks[Position].X,Locks[Position].Y,Locks[Position].Z,PassIndex,X,Y,Z)=0);
      if Operation=0 then
      begin
        if not Exists then
        begin
          if Count>=WFC_PIPELINE_RUN_MAX_LOCK_COUNT then WorkbenchError('run lock count envelope exceeded');
          SetLength(Locks,Count+1);
          for I:=Count downto Position+1 do Locks[I]:=Locks[I-1];
        end;
        Locks[Position]:=MakeWfcPipelineCellLock(PassIndex,X,Y,Z,Token);
      end
      else if Exists then
      begin
        for I:=Position to Count-2 do Locks[I]:=Locks[I+1];
        SetLength(Locks,Count-1);
      end;
    end
    else
    begin
      Position:=0; Count:=Length(Domains);
      while (Position<Count) and (CompareCell(Domains[Position].PassIndex,
        Domains[Position].X,Domains[Position].Y,Domains[Position].Z,PassIndex,X,Y,Z)<0) do Inc(Position);
      Exists:=(Position<Count) and (CompareCell(Domains[Position].PassIndex,
        Domains[Position].X,Domains[Position].Y,Domains[Position].Z,PassIndex,X,Y,Z)=0);
      if Operation=2 then
      begin
        if not Exists then
        begin
          if Count>=WFC_PIPELINE_RUN_MAX_DOMAIN_COUNT then WorkbenchError('run domain count envelope exceeded');
          SetLength(Domains,Count+1);
          for I:=Count downto Position+1 do Domains[I]:=Domains[I-1];
        end;
        Domains[Position]:=MakeWfcPipelineCellDomain(PassIndex,X,Y,Z,Domain);
      end
      else if Exists then
      begin
        for I:=Position to Count-2 do Domains[I]:=Domains[I+1];
        SetLength(Domains,Count-1);
      end;
    end;
    Desired:=RunWithInputs(Recipe,Original,Locks,Domains);
    Result:=FSlot.ApplyInputs(EncodeWfcPipelineRunText(Desired),Policy,ExpectedRevision);
  finally Desired.Free; Original.Free; Recipe.Free; end;
end;

function TWfcPipelineWorkspaceWorkbench.SetCellLock(const PassIndex, X, Y, Z: Integer;
  const Token: TWfcModelToken; const Policy: TWfcPipelineWorkspacePolicy;
  const ExpectedRevision: Integer): TWfcPipelineWorkspaceReceipt;
begin Result:=EditCell(0,PassIndex,X,Y,Z,Token,nil,Policy,ExpectedRevision); end;

function TWfcPipelineWorkspaceWorkbench.ClearCellLock(const PassIndex, X, Y, Z: Integer;
  const Policy: TWfcPipelineWorkspacePolicy; const ExpectedRevision: Integer): TWfcPipelineWorkspaceReceipt;
begin Result:=EditCell(1,PassIndex,X,Y,Z,'',nil,Policy,ExpectedRevision); end;

function TWfcPipelineWorkspaceWorkbench.SetCellDomain(const PassIndex, X, Y, Z: Integer;
  const AllowedTokens: TWfcModelTokens; const Policy: TWfcPipelineWorkspacePolicy;
  const ExpectedRevision: Integer): TWfcPipelineWorkspaceReceipt;
begin Result:=EditCell(2,PassIndex,X,Y,Z,'',AllowedTokens,Policy,ExpectedRevision); end;

function TWfcPipelineWorkspaceWorkbench.ClearCellDomain(const PassIndex, X, Y, Z: Integer;
  const Policy: TWfcPipelineWorkspacePolicy; const ExpectedRevision: Integer): TWfcPipelineWorkspaceReceipt;
begin Result:=EditCell(3,PassIndex,X,Y,Z,'',nil,Policy,ExpectedRevision); end;

function TWfcPipelineWorkspaceWorkbench.ExecuteInitial(const Policy: TWfcPipelineWorkspacePolicy;
  const ExpectedRevision: Integer): TWfcPipelineWorkspaceReceipt;
begin
  RequireExpected(ExpectedRevision); RequirePolicy(Policy);
  Result:=FSlot.ExecuteInitial(Policy,ExpectedRevision);
end;

function TWfcPipelineWorkspaceWorkbench.PreviewRepair(const RunText: String;
  const Roots: TGraphPassIndices; const Policy: TWfcPipelineWorkspacePolicy;
  const ExpectedRevision: Integer): TWfcPipelineWorkspaceRepairPreview;
begin
  RequireExpected(ExpectedRevision); RequirePolicy(Policy);
  Result:=FSlot.PreviewRepair(RunText,Roots,Policy,ExpectedRevision);
end;

function TWfcPipelineWorkspaceWorkbench.ExecuteRepair(const RunText: String;
  const Roots: TGraphPassIndices; const Policy: TWfcPipelineWorkspacePolicy;
  const ExpectedRevision: Integer): TWfcPipelineWorkspaceReceipt;
begin
  RequireExpected(ExpectedRevision); RequirePolicy(Policy);
  Result:=FSlot.ExecuteRepair(RunText,Roots,Policy,ExpectedRevision);
end;

function TWfcPipelineWorkspaceWorkbench.BeginMappedEpoch(const Topologies: TWfcPipelinePassTopologies;
  const Extents: TWfcPipelinePassExtents; const Seed: TGraphSeed;
  const Policy: TWfcPipelineWorkspacePolicy; const ExpectedRevision: Integer): TWfcPipelineWorkspaceReceipt;
var OriginalRecipe, Recipe: TWfcPipelineModel; OriginalRun, Run: TWfcPipelineRun;
  Table: TWfcPipelineLayoutTable; RootTopology: TWfcPipelinePassTopology;
begin
  Result:=nil; RequireExpected(ExpectedRevision); RequirePolicy(Policy); RequireSeed(Seed);
  OriginalRecipe:=nil; Recipe:=nil; OriginalRun:=nil; Run:=nil; Table:=nil;
  try
    OriginalRecipe:=FSlot.CopyCurrentRecipe;
    if not OriginalRecipe.HasPassMapping then WorkbenchError('BeginMappedEpoch requires an existing explicitly mapped recipe');
    RequireGeometryCounts(Topologies,Extents,OriginalRecipe.PassCount);
    { The pure layout table validates raw nested descriptors and every signed
      endpoint/count before cloning. Its copies are safe Pascal-owned records. }
    Table:=TWfcPipelineLayoutTable.Create(Topologies,Extents);
    RootTopology:=Table.PassTopologyAt(0);
    Recipe:=TWfcPipelineModel.Create(OriginalRecipe.CopyMetadata,
      OriginalRecipe.CopyVersions,RootTopology.Rank,RootTopology.Wrap,OriginalRecipe.RunMode,
      OriginalRecipe.CopyResources,OriginalRecipe.CopyPasses,OriginalRecipe.CopyDependencies,
      OriginalRecipe.CopyBridges,OriginalRecipe.CopyRequirements,OriginalRecipe.CopyValueQuotas,
      OriginalRecipe.CopyConnectivities,OriginalRecipe.PassMappingVersion,Table.CopyTopologies);
    OriginalRun:=FSlot.CopyAppliedRun;
    Run:=TWfcPipelineRun.Create(Recipe,Table.CopyExtents,Seed,OriginalRun.Strategy,
      OriginalRun.MaxBacktracks,OriginalRun.MaxPassBacktracks,OriginalRun.CaptureTrace,
      OriginalRun.CopyLocks,OriginalRun.CopyDomains);
    { This typed run validates all coupled layouts and every retained input.
      Invalid shrink/link changes reject; no clipping, resampling or dropping. }
    Result:=FSlot.BeginEpoch(EncodeWfcPipelineModelText(Recipe),
      EncodeWfcPipelineRunText(Run),Policy,ExpectedRevision);
  finally Run.Free; OriginalRun.Free; Recipe.Free; Table.Free; OriginalRecipe.Free; end;
end;

function TWfcPipelineWorkspaceWorkbench.CopyCurrentRecipe: TWfcPipelineModel;
begin Result:=FSlot.CopyCurrentRecipe; end;
function TWfcPipelineWorkspaceWorkbench.CopyAppliedRun: TWfcPipelineRun;
begin Result:=FSlot.CopyAppliedRun; end;
function TWfcPipelineWorkspaceWorkbench.CopyCanonicalJournal: String;
begin Result:=FSlot.CopyCanonicalJournal; end;
function TWfcPipelineWorkspaceWorkbench.CopyPublicState: TWfcPipelineSessionPublicState;
begin Result:=FSlot.CopyPublicState; end;
function TWfcPipelineWorkspaceWorkbench.CopyLastSuccessfulState: TWfcPipelineSessionPublicState;
begin Result:=FSlot.CopyLastSuccessfulState; end;
function TWfcPipelineWorkspaceWorkbench.GetHasExecution: Boolean;
begin Result:=FSlot.HasExecution; end;
function TWfcPipelineWorkspaceWorkbench.GetHasCurrentOutput: Boolean;
begin Result:=FSlot.HasCurrentOutput; end;
function TWfcPipelineWorkspaceWorkbench.GetHasSuccessfulBaseline: Boolean;
begin Result:=FSlot.HasSuccessfulBaseline; end;
function TWfcPipelineWorkspaceWorkbench.GetPublicationRevision: Integer;
begin Result:=FSlot.PublicationRevision; end;
function TWfcPipelineWorkspaceWorkbench.GetSessionRevision: Integer;
begin Result:=FSlot.SessionRevision; end;
function TWfcPipelineWorkspaceWorkbench.GetCurrentRecipeIndex: Integer;
begin Result:=FSlot.CurrentRecipeIndex; end;
function TWfcPipelineWorkspaceWorkbench.GetCurrentRunIndex: Integer;
begin Result:=FSlot.CurrentRunIndex; end;
end.
