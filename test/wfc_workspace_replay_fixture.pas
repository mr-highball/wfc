{ SPDX-License-Identifier: MIT
  Copyright (c) 2021 mr-highball
  Independent test historian: calls actual prepared sessions directly, never
  the workspace replay implementation, and records complete captured evidence. }
unit wfc_workspace_replay_fixture;
{$mode delphi}{$H+}
interface
uses SysUtils,wfc,wfc_model,wfc_pipeline_model,wfc_pipeline_run,
  wfc_pipeline_prepare,wfc_pipeline_session,wfc_pipeline_session_evidence,
  wfc_pipeline_workspace_context,wfc_pipeline_workspace_journal;
var ReplayChecks: Integer;
  ExportReplayDocuments: Boolean;
  ReplayDocumentCount: Integer;
procedure Check(const Condition: Boolean; const Detail: String);
procedure ExportReplayJournal(const Text: String);
function ReplacementLimits: TWfcPipelineReplacementLimits;
function OutcomeLimits: TWfcPipelineSessionOutcomeLimits;
function EvidenceLimits: TWfcPipelineSessionEvidenceLimits;
function JournalLimits: TWfcPipelineWorkspaceJournalLimits;
procedure EqualPublicState(const A,B: TWfcPipelineSessionPublicState);
function FixtureJournal(const EndWithFailure: Boolean;
  out ExpectedState,ExpectedBaseline: TWfcPipelineSessionPublicState;
  out ExpectedRevision: Integer): TWfcPipelineWorkspaceJournal;
function CopyWithActions(const Original: TWfcPipelineWorkspaceJournal;
  const Actions: TWfcPipelineWorkspaceActions): TWfcPipelineWorkspaceJournal;
implementation
uses wfc_lattice,wfc_rule_model,wfc_rule_text,wfc_sequence,wfc_sequence_text,
  wfc_sequence_learn,wfc_pipeline_layout,wfc_pipeline_text,wfc_pipeline_run_text,
  wfc_text_codec {$IFDEF PAS2JS},Web{$ENDIF};
procedure Check(const Condition: Boolean; const Detail: String);
begin Inc(ReplayChecks); if not Condition then raise Exception.Create('workspace replay fixture: '+Detail); end;
procedure ExportReplayJournal(const Text: String);
{$IFDEF PAS2JS}var Node: TJSElement;{$ENDIF}
begin
  if not ExportReplayDocuments then Exit;
  Inc(ReplayDocumentCount);
  {$IFDEF PAS2JS}
  { The maintained test-output panel is deliberately bounded. Complete journal
    parity uses dedicated nodes and never relies on its clipped display. }
  Node:=document.createElement('pre'); Node.id:='journal-parity-'+IntToStr(ReplayDocumentCount);
  Node.setAttribute('class','complete-workspace-journal'); Node.textContent:=Text;
  document.body.appendChild(Node);
  {$ELSE}
  WriteLn('journal-parity-',ReplayDocumentCount,'=',
    WfcTextEncodeToken(TWfcModelToken(Text),'private replay parity'));
  {$ENDIF}
end;
function ReplacementLimits: TWfcPipelineReplacementLimits;
begin Result.Version:=1; Result.MaxRetainedCellRecords:=100000; Result.MaxRetainedValueItems:=100000; Result.MaxCandidateVisits:=16000000; end;
function OutcomeLimits: TWfcPipelineSessionOutcomeLimits;
begin
  Result.Version:=1; Result.MaxPublicCellRecords:=10000; Result.MaxEncodedTokenBytes:=1000000;
  Result.MaxReportPassRecords:=10000; Result.MaxTraceEvents:=100000; Result.MaxExcludedAssignmentItems:=10000;
end;
function EvidenceLimits: TWfcPipelineSessionEvidenceLimits;
begin Result.Version:=1; Result.MaxTextBytes:=4000000; Result.MaxLines:=100000; end;
function JournalLimits: TWfcPipelineWorkspaceJournalLimits;
begin
  Result.Version:=1; Result.MaxRecipes:=20; Result.MaxRuns:=100; Result.MaxContextTextBytes:=4000000;
  Result.MaxActions:=100; Result.MaxRootReferences:=400; Result.MaxEvidenceTextBytes:=8000000; Result.MaxEncodedTextBytes:=32000000;
end;
function Tokens(const Values: array of TWfcModelToken): TWfcModelTokens;
var I: Integer;
begin Result:=nil; SetLength(Result,Length(Values)); for I:=0 to High(Values) do Result[I]:=Values[I]; end;
function Recipe(const Name: String; const UnrelatedCells: Integer;
  out Extents: TWfcPipelinePassExtents): TWfcPipelineModel;
var Resources: TWfcPipelineResources; Passes: TWfcPipelinePasses;
  Dependencies: TWfcPipelineDependencies; Bridges: TWfcPipelineBridges;
  Topologies: TWfcPipelinePassTopologies; Versions: TWfcPipelineVersions;
  Rules: TWfcRuleModel; Sequence: TWfcSequenceModel; Samples: TWfcSequenceSamples;
  Weights: TWfcModelIntegerArray; I: Integer;
begin
  SetLength(Resources,2); SetLength(Weights,2); Weights[0]:=1; Weights[1]:=1;
  Rules:=TWfcRuleModel.Create(1,Tokens(['U','V']),Weights,nil);
  try Resources[0]:=MakeWfcPipelineResource('unrelated',wprkRules,EncodeWfcRuleText(Rules),
    'project-authored U/V vocabulary','MIT','literal-uv'); finally Rules.Free; end;
  SetLength(Samples,3); Samples[0]:=MakeWfcSequenceSample(Tokens(['B','A','B']));
  Samples[1]:=MakeWfcSequenceSample(Tokens(['B','C','B'])); Samples[2]:=MakeWfcSequenceSample(Tokens(['A','B','A']));
  Sequence:=LearnSequenceModelCorpus(Samples,1);
  try Resources[1]:=MakeWfcPipelineResource('sequence',wprkSequence,EncodeWfcSequenceText(Sequence),
    'three literal sequence samples','MIT','bab-bcb-aba'); finally Sequence.Free; end;
  SetLength(Passes,4); SetLength(Topologies,4); SetLength(Extents,4);
  Passes[0]:=MakeWfcPipelinePass('unrelated',wppvPublic,gpmOverlay,-1,wpakRules,0,False,wseWhole);
  Passes[1]:=MakeWfcPipelinePass('latent',wppvPrivate,gpmOverlay,-1,wpakSequence,1,True,wseWhole);
  Passes[2]:=MakeWfcPipelinePass('projection',wppvPublic,gpmOverlay,-1,wpakEmpty,-1,False,wseWhole);
  Passes[3]:=MakeWfcPipelinePass('alias',wppvPublic,gpmTransform,2,wpakEmpty,-1,False,wseWhole);
  Topologies[0]:=LegacyWfcPipelinePassTopology(1,False);
  Topologies[1]:=MakeWfcPipelinePassTopology(1,MakeWfcLatticeVector(-9,7,4),MakeWfcLatticeVector(3,5,7),False);
  Extents[0]:=MakeWfcLatticeVector(UnrelatedCells,1,1); Extents[1]:=MakeWfcLatticeVector(3,1,1);
  for I:=2 to 3 do begin Topologies[I]:=Topologies[1]; Extents[I]:=Extents[1]; end;
  SetLength(Dependencies,2); Dependencies[0]:=MakeWfcPipelineDependency(2,1); Dependencies[1]:=MakeWfcPipelineDependency(3,2);
  SetLength(Bridges,1); Bridges[0]:=MakeWfcPipelineBridge(wpbkSequenceProjection,1,2);
  Versions:=CurrentWfcPipelineVersions; Versions.SequenceBridgeVersion:=2;
  Result:=TWfcPipelineModel.Create(MakeWfcPipelineMetadata(Name,'MIT','retained fixture provenance','replay-fixture'),
    Versions,1,False,rmBottomUp,Resources,Passes,Dependencies,Bridges,nil,nil,nil,WFC_PIPELINE_PASS_MAPPING_VERSION,Topologies);
end;
procedure EqualPublicState(const A,B: TWfcPipelineSessionPublicState);
var I,J: Integer; X,Y: TWfcPipelineSessionLayer;
begin
  Check((A=nil)=(B=nil),'state nil distinction'); if A=nil then Exit;
  Check(A.LayerCount=B.LayerCount,'public layer count');
  for I:=0 to A.LayerCount-1 do
  begin
    X:=A.LayerAt(I); Y:=B.LayerAt(I);
    Check((X.PassIndex=Y.PassIndex) and (X.Rank=Y.Rank) and (X.LabelName=Y.LabelName),'layer identity/rank');
    Check((X.Layout.Cells.X=Y.Layout.Cells.X) and (X.Layout.Cells.Y=Y.Layout.Cells.Y) and (X.Layout.Cells.Z=Y.Layout.Cells.Z),'layer extents');
    Check((X.Layout.Origin.X=Y.Layout.Origin.X) and (X.Layout.Origin.Y=Y.Layout.Origin.Y) and (X.Layout.Origin.Z=Y.Layout.Origin.Z),'layer origin');
    Check((X.Layout.Pitch.X=Y.Layout.Pitch.X) and (X.Layout.Pitch.Y=Y.Layout.Pitch.Y) and (X.Layout.Pitch.Z=Y.Layout.Pitch.Z) and (X.Layout.Wrap=Y.Layout.Wrap),'layer pitch/wrap');
    Check(Length(X.Cells)=Length(Y.Cells),'cell count');
    for J:=0 to High(X.Cells) do
    begin
      Check(X.Cells[J].Token=Y.Cells[J].Token,'actual cell token');
      Check(X.Cells[J].Empty=Y.Cells[J].Empty,'actual empty ownership');
      Check(X.Cells[J].Generated=Y.Cells[J].Generated,'actual generated ownership');
    end;
  end;
end;
function CopyWithActions(const Original: TWfcPipelineWorkspaceJournal;
  const Actions: TWfcPipelineWorkspaceActions): TWfcPipelineWorkspaceJournal;
var Recipes: TWfcPipelineWorkspaceRecipeTexts; Runs: TWfcPipelineWorkspaceRunTexts; I: Integer;
begin
  SetLength(Recipes,Original.RecipeCount); SetLength(Runs,Original.RunCount);
  for I:=0 to High(Recipes) do Recipes[I]:=Original.RecipeTextAt(I);
  for I:=0 to High(Runs) do Runs[I]:=Original.RunTextAt(I);
  Result:=TWfcPipelineWorkspaceJournal.Create(Recipes,Runs,Actions,JournalLimits);
end;
function FixtureJournal(const EndWithFailure: Boolean;
  out ExpectedState,ExpectedBaseline: TWfcPipelineSessionPublicState;
  out ExpectedRevision: Integer): TWfcPipelineWorkspaceJournal;
var Models: array of TWfcPipelineModel; Runs: array of TWfcPipelineRun;
  RecipeTexts: TWfcPipelineWorkspaceRecipeTexts; RunTexts: TWfcPipelineWorkspaceRunTexts;
  Actions: TWfcPipelineWorkspaceActions; Session: TWfcPipelinePreparedSession;
  Extents: TWfcPipelinePassExtents; CurrentRecipe,CurrentRun,BaseRun,I,J: Integer;
  Locks: TWfcPipelineCellLocks; Domains: TWfcPipelineCellDomains;
  State: TWfcPipelineSessionPublicState; Layer: TWfcPipelineSessionLayer;
  Seed: Cardinal; Strategy: TWfcPipelineSolveStrategy;
  procedure AddAction(const Kind: TWfcPipelineWorkspaceActionKind; const Evidence: String;
    const Roots: TGraphPassIndices);
  var Index,K: Integer;
  begin
    Index:=Length(Actions); SetLength(Actions,Index+1); Actions[Index].Kind:=Kind;
    Actions[Index].RunIndex:=CurrentRun; Actions[Index].EvidenceText:=Evidence;
    SetLength(Actions[Index].RequestedRootIndices,Length(Roots));
    for K:=0 to High(Roots) do Actions[Index].RequestedRootIndices[K]:=Roots[K];
  end;
  function NewRun(const L: TWfcPipelineCellLocks; const D: TWfcPipelineCellDomains): Integer;
  var PassBudget: Integer;
  begin
    Result:=Length(Runs); SetLength(Runs,Result+1); SetLength(RunTexts,Result+1);
    PassBudget:=0; if Strategy=wpssNegotiated then PassBudget:=8;
    Runs[Result]:=TWfcPipelineRun.Create(Models[CurrentRecipe],Extents,Seed,Strategy,256,PassBudget,True,L,D);
    RunTexts[Result].RecipeIndex:=CurrentRecipe; RunTexts[Result].Text:=EncodeWfcPipelineRunText(Runs[Result]);
  end;
  procedure BeginEpoch(const Name: String; const Width: Integer; const ASeed: Cardinal);
  begin
    FreeAndNil(Session); CurrentRecipe:=Length(Models); SetLength(Models,CurrentRecipe+1);
    SetLength(RecipeTexts,CurrentRecipe+1); Models[CurrentRecipe]:=Recipe(Name,Width,Extents);
    RecipeTexts[CurrentRecipe]:=EncodeWfcPipelineModelText(Models[CurrentRecipe]); Seed:=ASeed;
    CurrentRun:=NewRun(nil,nil); BaseRun:=CurrentRun;
    Session:=TWfcPipelinePreparedSession.Create(Models[CurrentRecipe],Runs[CurrentRun],ReplacementLimits,OutcomeLimits);
    AddAction(wpwakBeginEpoch,'',nil);
  end;
  procedure Initial;
  var O: TWfcPipelineSessionOutcome;
  begin
    O:=Session.ExecuteInitial;
    try Check(O.Solved,'direct fixture initial solves'); AddAction(wpwakInitial,EncodeWfcPipelineSessionOutcomeEvidence(O,EvidenceLimits),nil);
    finally O.Free; end;
  end;
  procedure Edit(const RunIndex: Integer);
  var O: TWfcPipelineSessionEditOutcome;
  begin
    O:=Session.ApplyInputs(Runs[RunIndex]); CurrentRun:=RunIndex;
    try AddAction(wpwakEdit,EncodeWfcPipelineSessionEditEvidence(O,EvidenceLimits),nil); finally O.Free; end;
  end;
  procedure Repair(const Root: Integer; const ExpectedSolved: Boolean);
  var Labels: TGraphPassLabels; Plan: TWfcPipelineSessionRepairPlan; O: TWfcPipelineSessionOutcome;
  begin
    SetLength(Labels,1); Labels[0]:=String(Models[CurrentRecipe].PassAt(Root).LabelName);
    Plan:=Session.PlanRepair(Runs[CurrentRun],Labels); O:=nil;
    try
      Check(Plan.CanExecute,'direct fixture explicit repair authorized'); O:=Session.ExecuteRepair(Plan);
      Check(O.Solved=ExpectedSolved,'direct fixture expected repair result');
      AddAction(wpwakRepair,EncodeWfcPipelineSessionOutcomeEvidence(O,EvidenceLimits),Plan.CopyScope.RequestedRootIndices);
    finally O.Free; Plan.Free; end;
  end;
begin
  Result:=nil; Session:=nil; ExpectedState:=nil; ExpectedBaseline:=nil; ExpectedRevision:=0;
  Strategy:=wpssOneWay;
  try
    BeginEpoch('learned history first epoch',2,71); Initial;
    State:=Session.CopyPublicState;
    try
      J:=-1; for I:=0 to State.LayerCount-1 do if State.LayerAt(I).PassIndex=3 then J:=I;
      Check(J>=0,'fixture has public alias'); Layer:=State.LayerAt(J);
    finally State.Free; end;
    SetLength(Locks,1); Locks[0]:=MakeWfcPipelineCellLock(3,0,0,0,Layer.Cells[0].Token);
    Edit(NewRun(Locks,nil)); Edit(BaseRun); Repair(1,True);
    SetLength(Domains,1); Domains[0]:=MakeWfcPipelineCellDomain(0,1,0,0,nil);
    Edit(NewRun(nil,Domains)); Repair(0,False);
    if not EndWithFailure then
    begin
      Edit(BaseRun); Repair(0,True);
      Strategy:=wpssNegotiated; BeginEpoch('learned history second epoch',4,72);
      Domains[0]:=MakeWfcPipelineCellDomain(0,0,0,0,Tokens(['U']));
      Edit(NewRun(nil,Domains)); Initial; Repair(1,True);
    end;
    Check(Session.HasCurrentOutput=not EndWithFailure,'direct final currentness');
    Check(Session.HasSuccessfulBaseline,'direct final baseline');
    ExpectedState:=Session.CopyPublicState; ExpectedBaseline:=Session.CopyLastSuccessfulState;
    ExpectedRevision:=Session.Revision;
    Result:=TWfcPipelineWorkspaceJournal.Create(RecipeTexts,RunTexts,Actions,JournalLimits);
  finally
    Session.Free;
    for I:=0 to High(Runs) do Runs[I].Free;
    for I:=0 to High(Models) do Models[I].Free;
    if Result=nil then begin FreeAndNil(ExpectedState); FreeAndNil(ExpectedBaseline); end;
  end;
end;
end.
