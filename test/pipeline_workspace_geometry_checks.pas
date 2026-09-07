{ SPDX-License-Identifier: MIT
  Copyright (c) 2021 mr-highball
  Private, independent geometry/epoch checks. No renderer or fixed-world preset
  is used. Expected artifacts are reconstructed with public model/run owners. }
unit pipeline_workspace_geometry_checks;
{$mode delphi}{$H+}
interface
procedure TestPipelineWorkspaceGeometry;
implementation
uses SysUtils,wfc,wfc_model,wfc_lattice,wfc_rule_model,wfc_rule_text,
  wfc_sequence,wfc_sequence_learn,wfc_sequence_text,wfc_pipeline_layout,
  wfc_pipeline_model,wfc_pipeline_text,wfc_pipeline_run,wfc_pipeline_run_text,
  wfc_pipeline_session,wfc_pipeline_workspace_context,
  wfc_pipeline_workspace_journal,wfc_pipeline_workspace_journal_text,
  wfc_pipeline_workspace_replay,wfc_workspace_replay_fixture,
  pipeline_workspace_workbench;

type
  TGeometryFailure = (gfLayout,gfModel,gfRun,gfWorkbench,gfStale);

function GeometryPolicy: TWfcPipelineWorkspacePolicy;
begin
  Result.Version:=1;
  Result.Journal:=JournalLimits;
  Result.Replacement:=ReplacementLimits;
  Result.Outcome:=OutcomeLimits;
  Result.Evidence:=EvidenceLimits;
  Result.Replay.Version:=1;
  Result.Replay.MaxEpochs:=20;
  Result.Replay.MaxSolveActions:=100;
  Result.Replay.MaxInstantiatedCellRecords:=100000;
  Result.Replay.MaxEvidenceTextBytes:=8000000;
end;

function OneToken(const Token: TWfcModelToken): TWfcModelTokens;
begin SetLength(Result,1); Result[0]:=Token; end;

function Position(const X,Y,Z: Integer): TGraphPosition;
begin Result.X:=X; Result.Y:=Y; Result.Z:=Z; end;

function RulesResource(const Id: String; const Rank: Integer;
  const Token: TWfcModelToken): TWfcPipelineResource;
var Rules: TWfcRuleModel; Weights: TWfcModelIntegerArray;
begin
  SetLength(Weights,1); Weights[0]:=7;
  Rules:=TWfcRuleModel.Create(Rank,OneToken(Token),Weights,nil);
  try Result:=MakeWfcPipelineResource(TWfcModelToken(Id),wprkRules,
    EncodeWfcRuleText(Rules),'literal rule provenance','MIT',
    TWfcModelToken('fingerprint-'+Id));
  finally Rules.Free; end;
end;

function GeometryRecipe(out Extents: TWfcPipelinePassExtents): TWfcPipelineModel;
var Resources: TWfcPipelineResources; Passes: TWfcPipelinePasses;
  Dependencies: TWfcPipelineDependencies; Bridges: TWfcPipelineBridges;
  Requirements: TWfcPipelineRequirements; Quotas: TWfcPipelineValueQuotas;
  Connectivity: TWfcPipelineConnectivities; Values: TWfcPipelineConnectivityValues;
  Required: TGraphPositions; Query: TWfcPipelineMappedQuery;
  Topologies: TWfcPipelinePassTopologies; Versions: TWfcPipelineVersions;
  Sequence: TWfcSequenceModel; Samples: TWfcModelTokens; I: Integer;
begin
  SetLength(Resources,3);
  Resources[0]:=RulesResource('terrain-resource',2,'ground');
  Resources[1]:=RulesResource('volume-resource',3,'home');
  SetLength(Samples,3); for I:=0 to 2 do Samples[I]:='A';
  Sequence:=LearnSequenceModel(Samples,1);
  try Resources[2]:=MakeWfcPipelineResource('learned-sequence',wprkSequence,
    EncodeWfcSequenceText(Sequence),'literal AAA training source','MIT','AAA-order1');
  finally Sequence.Free; end;
  SetLength(Passes,5); SetLength(Topologies,5); SetLength(Extents,5);
  Passes[0]:=MakeWfcPipelinePass('terrain',wppvPublic,gpmOverlay,-1,wpakRules,0,False,wseWhole);
  Passes[1]:=MakeWfcPipelinePass('volume',wppvPublic,gpmOverlay,-1,wpakRules,1,False,wseWhole);
  Passes[2]:=MakeWfcPipelinePass('latent-notes',wppvPrivate,gpmOverlay,-1,wpakSequence,2,True,wseWhole);
  Passes[3]:=MakeWfcPipelinePass('public-notes',wppvPublic,gpmOverlay,-1,wpakEmpty,-1,False,wseWhole);
  Passes[4]:=MakeWfcPipelinePass('notes-alias',wppvPublic,gpmTransform,3,wpakEmpty,-1,False,wseWhole);
  Topologies[0]:=MakeWfcPipelinePassTopology(2,MakeWfcLatticeVector(-6,4,2),MakeWfcLatticeVector(2,3,5),True);
  Topologies[1]:=MakeWfcPipelinePassTopology(3,MakeWfcLatticeVector(-4,5,3),MakeWfcLatticeVector(1,2,1),False);
  Topologies[2]:=MakeWfcPipelinePassTopology(1,MakeWfcLatticeVector(31,-5,9),MakeWfcLatticeVector(7,2,4),False);
  Extents[0]:=MakeWfcLatticeVector(6,4,1); Extents[1]:=MakeWfcLatticeVector(2,2,2);
  Extents[2]:=MakeWfcLatticeVector(3,1,1);
  for I:=3 to 4 do begin Topologies[I]:=Topologies[2]; Extents[I]:=Extents[2]; end;
  SetLength(Dependencies,3);
  Dependencies[0]:=MakeWfcPipelineDependency(1,0);
  Dependencies[1]:=MakeWfcPipelineDependency(3,2);
  Dependencies[2]:=MakeWfcPipelineDependency(4,3);
  SetLength(Bridges,1); Bridges[0]:=MakeWfcPipelineBridge(wpbkSequenceProjection,2,3);
  Query.Kind:=gpmkPoint; Query.Match:=gpmmAll;
  Query.MinimumOffset:=MakeGraphOffset(0,0,0); Query.MaximumOffset:=MakeGraphOffset(0,0,0);
  Query.AllowedProviderTokens:=OneToken('ground'); Query.MinimumMatches:=0; Query.MaximumMatches:=0;
  SetLength(Requirements,1); Requirements[0]:=MakeWfcPipelineMappedRequirement(1,'home',0,Query);
  SetLength(Quotas,2);
  Quotas[0]:=MakeWfcPipelineValueQuota(0,'ground-count',OneToken('ground'),1,1000);
  Quotas[1]:=MakeWfcPipelineValueQuota(4,'alias-count',OneToken('A'),1,1000);
  SetLength(Connectivity,1); SetLength(Values,1); SetLength(Required,1);
  Values[0]:=MakeWfcPipelineConnectivityValue('ground',[gdNorth,gdEast,gdSouth,gdWest],False);
  Required[0]:=Position(1,1,0);
  Connectivity[0]:=MakeWfcPipelineConnectivity(0,'connected-ground',Position(0,0,0),Required,Values,True);
  Versions:=CurrentWfcPipelineVersions; Versions.SequenceBridgeVersion:=2;
  Result:=TWfcPipelineModel.Create(MakeWfcPipelineMetadata('geometry document','MIT',
    'independent mixed-rank authoring fixture','geometry-provenance-v1'),Versions,
    Topologies[0].Rank,Topologies[0].Wrap,rmBottomUp,Resources,Passes,Dependencies,
    Bridges,Requirements,Quotas,Connectivity,WFC_PIPELINE_PASS_MAPPING_VERSION,Topologies);
end;

function GeometryRun(const Recipe: TWfcPipelineModel;
  const Extents: TWfcPipelinePassExtents): TWfcPipelineRun;
var Locks: TWfcPipelineCellLocks; Domains: TWfcPipelineCellDomains;
begin
  SetLength(Locks,3); SetLength(Domains,3);
  Locks[0]:=MakeWfcPipelineCellLock(0,3,2,0,'ground');
  Locks[1]:=MakeWfcPipelineCellLock(1,1,1,1,'home');
  Locks[2]:=MakeWfcPipelineCellLock(4,2,0,0,'A');
  Domains[0]:=MakeWfcPipelineCellDomain(0,5,3,0,OneToken('ground'));
  Domains[1]:=MakeWfcPipelineCellDomain(1,0,1,0,OneToken('home'));
  Domains[2]:=MakeWfcPipelineCellDomain(3,1,0,0,OneToken('A'));
  Result:=TWfcPipelineRun.Create(Recipe,Extents,4000000001,wpssNegotiated,1234,9,True,Locks,Domains);
end;

procedure OpenGeometry(const W: TWfcPipelineWorkspaceWorkbench;
  const Solve: Boolean);
var M: TWfcPipelineModel; R: TWfcPipelineRun; E: TWfcPipelinePassExtents;
  Receipt: TWfcPipelineWorkspaceReceipt; P: TWfcPipelineWorkspacePolicy;
begin
  P:=GeometryPolicy; M:=GeometryRecipe(E); R:=nil; Receipt:=nil;
  try
    R:=GeometryRun(M,E);
    Receipt:=W.BeginEpoch(EncodeWfcPipelineModelText(M),EncodeWfcPipelineRunText(R),P,W.PublicationRevision);
    Check((Receipt.Kind=wpwakBeginEpoch) and not Receipt.HasSuccessfulBaseline,
      'geometry fixture begins a real unsolved epoch');
  finally Receipt.Free; R.Free; M.Free; end;
  if Solve then
  begin
    Receipt:=W.ExecuteInitial(P,W.PublicationRevision);
    try Check(Receipt.BorrowSolveOutcome.Solved and W.HasCurrentOutput and W.HasSuccessfulBaseline,
      'mixed-rank mapped/learned fixture establishes a real baseline');
    finally Receipt.Free; end;
  end;
end;

function ReconstructRecipe(const Original: TWfcPipelineModel;
  const Topologies: TWfcPipelinePassTopologies): TWfcPipelineModel;
begin
  { This oracle uses only public immutable constructors, not BeginMappedEpoch
    or any geometry-replacement helper from the workbench implementation. }
  Result:=TWfcPipelineModel.Create(Original.CopyMetadata,Original.CopyVersions,
    Topologies[0].Rank,Topologies[0].Wrap,Original.RunMode,Original.CopyResources,
    Original.CopyPasses,Original.CopyDependencies,Original.CopyBridges,
    Original.CopyRequirements,Original.CopyValueQuotas,Original.CopyConnectivities,
    Original.PassMappingVersion,Topologies);
end;

function ReconstructRun(const Recipe: TWfcPipelineModel; const Original: TWfcPipelineRun;
  const Extents: TWfcPipelinePassExtents; const Seed: TGraphSeed): TWfcPipelineRun;
begin
  Result:=TWfcPipelineRun.Create(Recipe,Extents,Seed,Original.Strategy,
    Original.MaxBacktracks,Original.MaxPassBacktracks,Original.CaptureTrace,
    Original.CopyLocks,Original.CopyDomains);
end;

procedure PreserveHistory(const Before,After: TWfcPipelineWorkspaceJournal);
var I,J: Integer; A,B: TWfcPipelineWorkspaceAction;
begin
  Check(After.ActionCount=Before.ActionCount+1,'geometry change appends exactly one explicit begin action');
  Check(After.RecipeCount=Before.RecipeCount+1,'geometry change appends its own recipe row without deduplication');
  Check(After.RunCount=Before.RunCount+1,'geometry change appends its own complete run row');
  for I:=0 to Before.RecipeCount-1 do
    Check(After.RecipeTextAt(I)=Before.RecipeTextAt(I),'older recipe bytes retained');
  for I:=0 to Before.RunCount-1 do
  begin
    Check(After.RunTextAt(I).RecipeIndex=Before.RunTextAt(I).RecipeIndex,'older run context index retained');
    Check(After.RunTextAt(I).Text=Before.RunTextAt(I).Text,'older complete run bytes retained');
  end;
  for I:=0 to Before.ActionCount-1 do
  begin
    A:=Before.ActionAt(I); B:=After.ActionAt(I);
    Check((A.Kind=B.Kind) and (A.RunIndex=B.RunIndex),'older action identity retained');
    Check(A.EvidenceText=B.EvidenceText,'older complete actual evidence retained');
    Check(Length(A.RequestedRootIndices)=Length(B.RequestedRootIndices),'older explicit root count retained');
    for J:=0 to High(A.RequestedRootIndices) do
      Check(A.RequestedRootIndices[J]=B.RequestedRootIndices[J],'older explicit root index retained');
  end;
  A:=After.ActionAt(Before.ActionCount);
  Check((A.Kind=wpwakBeginEpoch) and (A.RunIndex=Before.RunCount) and
    (Length(A.RequestedRootIndices)=0) and (A.EvidenceText=''),'new geometry action is begin, never hidden solve/repair');
  Check(After.RunTextAt(Before.RunCount).RecipeIndex=Before.RecipeCount,'new run references exact new recipe context');
end;

procedure AssertReconstructed(const W: TWfcPipelineWorkspaceWorkbench;
  const ExpectedModel: TWfcPipelineModel; const ExpectedRun: TWfcPipelineRun);
var ActualModel: TWfcPipelineModel; ActualRun: TWfcPipelineRun; I: Integer;
begin
  ActualModel:=W.CopyCurrentRecipe; ActualRun:=nil;
  try
    ActualRun:=W.CopyAppliedRun;
    Check(EncodeWfcPipelineModelText(ActualModel)=EncodeWfcPipelineModelText(ExpectedModel),
      'complete model bytes equal independent reconstruction of every section/version/provenance');
    Check(EncodeWfcPipelineRunText(ActualRun)=EncodeWfcPipelineRunText(ExpectedRun),
      'complete run bytes equal independent reconstruction including every authored input/option');
    Check((ActualModel.ResourceCount=3) and (ActualModel.PassCount=5) and
      (ActualModel.DependencyCount=3) and (ActualModel.BridgeCount=1) and
      (ActualModel.RequirementCount=1) and (ActualModel.ValueQuotaCount=2) and
      (ActualModel.ConnectivityCount=1),'nonempty complete model sections remain present');
    Check(ActualModel.PassAt(2).Visibility=wppvPrivate,'learned latent pass remains private');
    Check((ActualModel.PassAt(3).Visibility=wppvPublic) and
      (ActualModel.PassAt(4).Mode=gpmTransform),'projection/public alias semantics retained');
    Check((ActualModel.CopyVersions.SequenceBridgeVersion=2) and
      (ActualModel.PassMappingVersion=1) and (ActualRun.FormatVersion=2),
      'inverse bridge, mapping and invocation versions retained');
    Check(Pos('wfcpipeline=5'#10,EncodeWfcPipelineModelText(ActualModel))=1,
      'spatial recipe remains explicit version5');
    Check((ActualRun.LockCount=3) and (ActualRun.DomainCount=3),
      'all three authored locks and all three authored domains retained');
    Check((ActualRun.Strategy=wpssNegotiated) and (ActualRun.MaxBacktracks=1234) and
      (ActualRun.MaxPassBacktracks=9) and ActualRun.CaptureTrace,'strategy, both budgets and trace option retained');
    for I:=0 to 4 do
    begin
      Check(SameWfcLatticeLayout(ActualRun.PassLayoutAt(I),ExpectedRun.PassLayoutAt(I)),
        'each pass has independent exact extents/origin/pitch/wrap');
      Check(ActualModel.PassTopologyAt(I).Rank=ExpectedModel.PassTopologyAt(I).Rank,
        'each pass preserves the independently requested rank');
    end;
  finally ActualRun.Free; ActualModel.Free; end;
end;

procedure RejectGeometry(const W: TWfcPipelineWorkspaceWorkbench;
  const Topologies: TWfcPipelinePassTopologies; const Extents: TWfcPipelinePassExtents;
  const ExpectedRevision: Integer; const Failure: TGeometryFailure; const Detail: String);
var BeforeText,BeforeRecipe,BeforeRun: String; Revision,SessionRevision,RecipeIndex,RunIndex: Integer;
  Current,Baseline: Boolean; BeforeState,BeforeBaseline,AfterState: TWfcPipelineSessionPublicState;
  M: TWfcPipelineModel; R: TWfcPipelineRun; Receipt: TWfcPipelineWorkspaceReceipt;
  P: TWfcPipelineWorkspacePolicy; Raised: Boolean;
begin
  P:=GeometryPolicy; BeforeText:=W.CopyCanonicalJournal; Revision:=W.PublicationRevision;
  SessionRevision:=W.SessionRevision; RecipeIndex:=W.CurrentRecipeIndex; RunIndex:=W.CurrentRunIndex;
  Current:=W.HasCurrentOutput; Baseline:=W.HasSuccessfulBaseline;
  M:=W.CopyCurrentRecipe; try BeforeRecipe:=EncodeWfcPipelineModelText(M); finally M.Free; end;
  R:=W.CopyAppliedRun; try BeforeRun:=EncodeWfcPipelineRunText(R); finally R.Free; end;
  BeforeState:=W.CopyPublicState; BeforeBaseline:=W.CopyLastSuccessfulState;
  Receipt:=nil; AfterState:=nil; Raised:=False;
  try
    try Receipt:=W.BeginMappedEpoch(Topologies,Extents,73,P,ExpectedRevision);
    except
      on E: EWfcPipelineWorkspaceReplay do
      begin Raised:=True; Check((Failure=gfStale) and (E.Kind=wpwrfStalePublication) and (E.Message<>''),Detail+' exact stale error'); end;
      on E: EWfcPipelineWorkspaceWorkbench do
      begin Raised:=True; Check((Failure=gfWorkbench) and (E.Message<>''),Detail+' exact workbench refusal'); end;
      on E: EWfcLattice do
      begin Raised:=True; Check((Failure=gfLayout) and (E.Message<>''),Detail+' typed layout refusal'); end;
      on E: EWfcPipelineModel do
      begin Raised:=True; Check((Failure=gfModel) and (E.Message<>''),Detail+' typed model/refinement refusal'); end;
      on E: EWfcPipelineRun do
      begin Raised:=True; Check((Failure=gfRun) and (E.Message<>''),Detail+' typed run/input refusal'); end;
    end;
    Check(Raised and (Receipt=nil),Detail+' returns no publication receipt');
    Check((W.PublicationRevision=Revision) and (W.SessionRevision=SessionRevision),Detail+' revisions unchanged');
    Check((W.CurrentRecipeIndex=RecipeIndex) and (W.CurrentRunIndex=RunIndex),Detail+' context indices unchanged');
    Check((W.HasCurrentOutput=Current) and (W.HasSuccessfulBaseline=Baseline),Detail+' current/baseline flags unchanged');
    Check(W.CopyCanonicalJournal=BeforeText,Detail+' full accepted journal unchanged');
    M:=W.CopyCurrentRecipe; try Check(EncodeWfcPipelineModelText(M)=BeforeRecipe,Detail+' complete recipe unchanged'); finally M.Free; end;
    R:=W.CopyAppliedRun; try Check(EncodeWfcPipelineRunText(R)=BeforeRun,Detail+' no clipping/dropping/rewriting authored inputs'); finally R.Free; end;
    AfterState:=W.CopyPublicState; EqualPublicState(BeforeState,AfterState); FreeAndNil(AfterState);
    AfterState:=W.CopyLastSuccessfulState; EqualPublicState(BeforeBaseline,AfterState);
  finally AfterState.Free; Receipt.Free; BeforeState.Free; BeforeBaseline.Free; end;
end;

procedure TestChangedGeometryAndHistory;
var W,Restored: TWfcPipelineWorkspaceWorkbench; P: TWfcPipelineWorkspacePolicy;
  Original,ExpectedModel: TWfcPipelineModel; OriginalRun,ExpectedRun: TWfcPipelineRun;
  T: TWfcPipelinePassTopologies; E: TWfcPipelinePassExtents;
  Before,After: TWfcPipelineWorkspaceJournal; Receipt,Solved: TWfcPipelineWorkspaceReceipt;
  OldState,State: TWfcPipelineSessionPublicState; I,Mode,Revision: Integer;
  Roots: TGraphPassIndices;
begin
  W:=TWfcPipelineWorkspaceWorkbench.Create; Restored:=nil; P:=GeometryPolicy;
  Original:=nil; OriginalRun:=nil; ExpectedModel:=nil; ExpectedRun:=nil;
  Before:=nil; After:=nil; Receipt:=nil; Solved:=nil; OldState:=nil; State:=nil;
  try
    OpenGeometry(W,True); OldState:=W.CopyLastSuccessfulState;
    { Preserve a real selected-scope record, not merely initial generations. }
    OriginalRun:=W.CopyAppliedRun; SetLength(Roots,1); Roots[0]:=0;
    Receipt:=W.ExecuteRepair(EncodeWfcPipelineRunText(OriginalRun),Roots,P,W.PublicationRevision);
    Check(Receipt.BorrowSolveOutcome.Solved,'pre-geometry selected repair produces real retained history');
    FreeAndNil(Receipt); FreeAndNil(OriginalRun);
    for Mode:=0 to 2 do
    begin
      Original:=W.CopyCurrentRecipe; OriginalRun:=W.CopyAppliedRun;
      T:=Original.CopyPassTopologies; E:=OriginalRun.CopyPassExtents;
      case Mode of
        0: begin
          T[0]:=MakeWfcPipelinePassTopology(2,MakeWfcLatticeVector(-20,10,-8),MakeWfcLatticeVector(5,4,9),False);
          T[1]:=MakeWfcPipelinePassTopology(3,MakeWfcLatticeVector(-15,14,-6),MakeWfcLatticeVector(2,3,2),True);
          T[2]:=MakeWfcPipelinePassTopology(1,MakeWfcLatticeVector(9,-11,2),MakeWfcLatticeVector(4,7,3),False);
          E[0]:=MakeWfcLatticeVector(8,6,1); E[1]:=MakeWfcLatticeVector(3,4,2); E[2]:=MakeWfcLatticeVector(5,1,1);
          for I:=3 to 4 do begin T[I]:=T[2]; E[I]:=E[2]; end;
        end;
        1: ; { Seed-only epoch, deliberately identical recipe bytes. }
        2: E[0].X:=9; { Extent-only epoch retains unlike other passes. }
      end;
      ExpectedModel:=ReconstructRecipe(Original,T);
      ExpectedRun:=ReconstructRun(ExpectedModel,OriginalRun,E,4000000002+Cardinal(Mode));
      Before:=DecodeWfcPipelineWorkspaceJournalText(W.CopyCanonicalJournal,P.Journal);
      Revision:=W.PublicationRevision;
      Receipt:=W.BeginMappedEpoch(T,E,ExpectedRun.Seed,P,Revision);
      Check((Receipt.PublicationRevision=Revision+1) and (Receipt.RecipeIndex=Before.RecipeCount) and
        (Receipt.RunIndex=Before.RunCount) and (Receipt.ActionIndex=Before.ActionCount),
        'geometry publication has exact next revision/action/context indices');
      Check(not Receipt.HasCurrentOutput and not Receipt.HasSuccessfulBaseline and
        not W.HasCurrentOutput and not W.HasSuccessfulBaseline,'every new geometry/seed/extent epoch resets active baseline/current output');
      State:=W.CopyLastSuccessfulState; Check(State=nil,'new epoch has no stale prior geometry baseline'); FreeAndNil(State);
      Check((OldState.LayerAt(0).Layout.Cells.X=6) and
        (OldState.LayerAt(0).Layout.Cells.Y=4),
        'previously detached successful state retains its original geometry');
      AssertReconstructed(W,ExpectedModel,ExpectedRun);
      After:=DecodeWfcPipelineWorkspaceJournalText(W.CopyCanonicalJournal,P.Journal);
      PreserveHistory(Before,After);
      if Mode=1 then Check(After.RecipeTextAt(After.RecipeCount-1)=Before.RecipeTextAt(Before.RecipeCount-1),
        'seed-only epoch retains an explicit duplicate recipe context, never silently deduplicated');
      ExportReplayJournal(W.CopyCanonicalJournal);
      FreeAndNil(Receipt); FreeAndNil(Before); FreeAndNil(After);
      FreeAndNil(ExpectedRun); FreeAndNil(ExpectedModel); FreeAndNil(OriginalRun); FreeAndNil(Original);
      Solved:=W.ExecuteInitial(P,W.PublicationRevision);
      Check(Solved.BorrowSolveOutcome.Solved and W.HasCurrentOutput and W.HasSuccessfulBaseline,
        'changed arbitrary pass geometry solves only after explicit initial action');
      FreeAndNil(Solved);
    end;
    Restored:=TWfcPipelineWorkspaceWorkbench.Create;
    Restored.RestoreJournal(W.CopyCanonicalJournal,P,0);
    Check(Restored.CopyCanonicalJournal=W.CopyCanonicalJournal,'all changed geometry epochs replay exact complete history');
    State:=W.CopyPublicState;
    OldState.Free; OldState:=Restored.CopyPublicState; EqualPublicState(OldState,State);
    ExportReplayJournal(W.CopyCanonicalJournal);
  finally State.Free; OldState.Free; Solved.Free; Receipt.Free; Before.Free; After.Free;
    ExpectedRun.Free; OriginalRun.Free; ExpectedModel.Free; Original.Free; Restored.Free; W.Free; end;
end;

procedure TestRejectedGeometry;
var W: TWfcPipelineWorkspaceWorkbench; M: TWfcPipelineModel; R: TWfcPipelineRun;
  T: TWfcPipelinePassTopologies; E: TWfcPipelinePassExtents; I,K: Integer;
begin
  W:=TWfcPipelineWorkspaceWorkbench.Create; M:=nil; R:=nil;
  try
    OpenGeometry(W,True); M:=W.CopyCurrentRecipe; R:=W.CopyAppliedRun;
    for K:=0 to 12 do
    begin
      T:=M.CopyPassTopologies; E:=R.CopyPassExtents;
      case K of
        0: begin E[0].X:=3; RejectGeometry(W,T,E,W.PublicationRevision,gfRun,'lock outside shrunk ground'); end;
        1: begin E[0].X:=5; RejectGeometry(W,T,E,W.PublicationRevision,gfRun,'domain alone outside shrunk ground'); end;
        2: begin E[3].X:=4; RejectGeometry(W,T,E,W.PublicationRevision,gfRun,'projection extent differs from private source'); end;
        3: begin E[4].X:=4; RejectGeometry(W,T,E,W.PublicationRevision,gfRun,'alias extent differs from source'); end;
        4: begin T[4].Origin.X:=32; RejectGeometry(W,T,E,W.PublicationRevision,gfRun,'alias same dimensions different origin'); end;
        5: begin T[3].Pitch.X:=8; T[4]:=T[3]; RejectGeometry(W,T,E,W.PublicationRevision,gfRun,'projection same dimensions different pitch'); end;
        6: begin E[2].Y:=2; RejectGeometry(W,T,E,W.PublicationRevision,gfLayout,'rank1 requires unit Y'); end;
        7: begin E[0].Z:=2; RejectGeometry(W,T,E,W.PublicationRevision,gfLayout,'rank2 requires unit Z'); end;
        8: begin E[1].Z:=0; RejectGeometry(W,T,E,W.PublicationRevision,gfLayout,'rank3 requires positive depth'); end;
        9: begin T[1].Rank:=2; E[1].Z:=1; RejectGeometry(W,T,E,W.PublicationRevision,gfModel,'resource rank cannot be silently changed'); end;
        10: begin for I:=2 to 4 do T[I].Wrap:=True; RejectGeometry(W,T,E,W.PublicationRevision,gfModel,'wrapped sequence cannot keep whole extent'); end;
        11: begin T[0].Origin.X:=High(Integer); RejectGeometry(W,T,E,W.PublicationRevision,gfLayout,'world endpoint overflow refused'); end;
        12: begin T[1].Pitch.Z:=0; RejectGeometry(W,T,E,W.PublicationRevision,gfLayout,'zero cell pitch refused'); end;
      end;
    end;
    T:=M.CopyPassTopologies; E:=R.CopyPassExtents; T[0].Rank:=0;
    RejectGeometry(W,T,E,W.PublicationRevision-1,gfStale,'stale revision checked before invalid numeric geometry');
  finally R.Free; M.Free; W.Free; end;
end;

procedure TestLegacyAndEmptyDomain;
var W: TWfcPipelineWorkspaceWorkbench; P: TWfcPipelineWorkspacePolicy;
  M,Expected: TWfcPipelineModel; R,ExpectedRun: TWfcPipelineRun;
  T: TWfcPipelinePassTopologies; E: TWfcPipelinePassExtents;
  Resources: TWfcPipelineResources; Passes: TWfcPipelinePasses;
  Receipt: TWfcPipelineWorkspaceReceipt;
  Before,After: TWfcPipelineWorkspaceJournal;
begin
  W:=TWfcPipelineWorkspaceWorkbench.Create; P:=GeometryPolicy;
  M:=nil; Expected:=nil; R:=nil; ExpectedRun:=nil; Receipt:=nil; Before:=nil; After:=nil;
  try
    SetLength(Resources,1); Resources[0]:=RulesResource('legacy-rule',1,'plain');
    SetLength(Passes,1); Passes[0]:=MakeWfcPipelinePass('legacy',wppvPublic,gpmOverlay,-1,wpakRules,0,False,wseWhole);
    M:=TWfcPipelineModel.Create(MakeWfcPipelineMetadata('legacy','MIT','legacy provenance','legacy-fp'),
      1,False,rmBottomUp,Resources,Passes,nil,nil,nil);
    R:=TWfcPipelineRun.Create(M,2,1,1,55,wpssOneWay,19,0,False,nil,nil);
    Receipt:=W.BeginEpoch(EncodeWfcPipelineModelText(M),EncodeWfcPipelineRunText(R),P,0); FreeAndNil(Receipt);
    T:=M.CopyPassTopologies; E:=R.CopyPassExtents; E[0].X:=3;
    RejectGeometry(W,T,E,W.PublicationRevision,gfWorkbench,'legacy recipe may not be silently upgraded to spatial');
    FreeAndNil(R); FreeAndNil(M); FreeAndNil(W);
    W:=TWfcPipelineWorkspaceWorkbench.Create; OpenGeometry(W,True);
    Receipt:=W.SetCellDomain(1,0,1,0,nil,P,W.PublicationRevision); FreeAndNil(Receipt);
    M:=W.CopyCurrentRecipe; R:=W.CopyAppliedRun; T:=M.CopyPassTopologies; E:=R.CopyPassExtents;
    Expected:=ReconstructRecipe(M,T); ExpectedRun:=ReconstructRun(Expected,R,E,88);
    Before:=DecodeWfcPipelineWorkspaceJournalText(W.CopyCanonicalJournal,P.Journal);
    Receipt:=W.BeginMappedEpoch(T,E,88,P,W.PublicationRevision); FreeAndNil(Receipt);
    After:=DecodeWfcPipelineWorkspaceJournalText(W.CopyCanonicalJournal,P.Journal);
    PreserveHistory(Before,After);
    AssertReconstructed(W,Expected,ExpectedRun);
    FreeAndNil(R); R:=W.CopyAppliedRun;
    Check(Length(R.DomainAt(1).AllowedTokens)=0,'explicit empty domain survives a new epoch, never interpreted as clear');
    Receipt:=W.ExecuteInitial(P,W.PublicationRevision);
    Check(not Receipt.BorrowSolveOutcome.Solved and not W.HasCurrentOutput and not W.HasSuccessfulBaseline,
      'preserved contradictory domain produces a normal unsolved initial outcome in the new epoch');
    ExportReplayJournal(W.CopyCanonicalJournal);
  finally After.Free; Before.Free; Receipt.Free; ExpectedRun.Free; R.Free; Expected.Free; M.Free; W.Free; end;
end;

{$IFDEF PAS2JS}
procedure TestRawGeometryCase(const W: TWfcPipelineWorkspaceWorkbench; const CaseIndex: Integer);
var M: TWfcPipelineModel; R: TWfcPipelineRun; T: TWfcPipelinePassTopologies;
  E: TWfcPipelinePassExtents; Hits,Expected: Integer; Failure: TGeometryFailure;
begin
  { Every call starts fresh. No malformed raw record is later reused as the
    destination of a Pascal record assignment / generated $assign. }
  M:=W.CopyCurrentRecipe; R:=W.CopyAppliedRun;
  try T:=M.CopyPassTopologies; E:=R.CopyPassExtents; finally R.Free; M.Free; end;
  Hits:=0; Expected:=W.PublicationRevision; Failure:=gfLayout;
  case CaseIndex of
    0: asm T=null; end;
    1: asm E={length:5}; end;
    2: asm Object.defineProperty(T,'0',{get:function(){Hits++; throw new Error('topology slot invoked');}}); end;
    3: asm Object.defineProperty(E,'0',{get:function(){Hits++; throw new Error('extent slot invoked');}}); end;
    4: asm Object.defineProperty(T[0],'Origin',{get:function(){Hits++; throw new Error('origin getter invoked');}}); end;
    5: asm Object.defineProperty(T[0].Pitch,'X',{get:function(){Hits++; throw new Error('pitch getter invoked');}}); end;
    6: asm Object.defineProperty(E[0],'X',{get:function(){Hits++; throw new Error('extent component invoked');}}); end;
    7: asm T[0].Rank=NaN; end;
    8: asm T[0].Wrap=1; end;
    9: asm E[0].X='6'; end;
    10: asm T[1].Origin=null; end;
    11: asm T[0].Pitch.X={valueOf:function(){Hits++;return 2;}}; end;
    12: asm E[0].X=1.5; end;
    13: asm { const saved=T[0]; delete T[0]; const parent=Object.create(Array.prototype); parent[0]=saved; Object.setPrototypeOf(T,parent); } end;
    14: asm { const saved=E[0]; delete E[0]; const parent=Object.create(Array.prototype); parent[0]=saved; Object.setPrototypeOf(E,parent); } end;
    15: begin
      Expected:=W.PublicationRevision-1; Failure:=gfStale;
      asm Object.defineProperty(T,'0',{get:function(){Hits++;throw new Error('stale topology slot invoked');}}); E=null; end;
    end;
  end;
  RejectGeometry(W,T,E,Expected,Failure,'raw geometry case '+IntToStr(CaseIndex));
  Check(Hits=0,'raw geometry accessors/coercions are never invoked');
end;
{$ENDIF}

procedure TestPassiveGeometry(const W: TWfcPipelineWorkspaceWorkbench; const Kind: Integer);
var M,ExpectedModel: TWfcPipelineModel; R,ExpectedRun,ActualRun: TWfcPipelineRun;
  T: TWfcPipelinePassTopologies; E: TWfcPipelinePassExtents; Hits: Integer;
  Receipt: TWfcPipelineWorkspaceReceipt; P: TWfcPipelineWorkspacePolicy;
begin
  M:=W.CopyCurrentRecipe; R:=W.CopyAppliedRun; ExpectedModel:=nil;
  ExpectedRun:=nil; ActualRun:=nil; Receipt:=nil; P:=GeometryPolicy; Hits:=0;
  try
    T:=M.CopyPassTopologies; E:=R.CopyPassExtents;
    ExpectedModel:=ReconstructRecipe(M,T); ExpectedRun:=ReconstructRun(ExpectedModel,R,E,901+Cardinal(Kind));
    { Both backends author these exact successful histories. Only hostile/raw
      JavaScript presentation of otherwise equivalent data is conditional. }
    {$IFDEF PAS2JS}
    case Kind of
      0: asm
        for(let i=0;i<T.length;i++) {
          T[i]={Rank:T[i].Rank,Origin:{X:T[i].Origin.X,Y:T[i].Origin.Y,Z:T[i].Origin.Z},
            Pitch:{X:T[i].Pitch.X,Y:T[i].Pitch.Y,Z:T[i].Pitch.Z},Wrap:T[i].Wrap};
          E[i]={X:E[i].X,Y:E[i].Y,Z:E[i].Z};
        }
        Object.defineProperty(T,'slice',{get:function(){Hits++;throw new Error('slice getter invoked');}});
        Object.defineProperty(E,'slice',{get:function(){Hits++;throw new Error('extent slice getter invoked');}});
      end;
      1: asm
        for(let i=0;i<T.length;i++) {
          const old=T[i];
          T[i]=Object.create({Rank:old.Rank,Origin:Object.create({X:old.Origin.X,Y:old.Origin.Y,Z:old.Origin.Z}),
            Pitch:Object.create({X:old.Pitch.X,Y:old.Pitch.Y,Z:old.Pitch.Z}),Wrap:old.Wrap});
          E[i]=Object.create({X:E[i].X,Y:E[i].Y,Z:E[i].Z});
        }
        T.slice=function(){Hits++;return this;}; E.slice=function(){Hits++;return this;};
      end;
      2: asm
        T.slice=null; E.slice=null;
        for(let i=0;i<T.length;i++) {
          Object.freeze(T[i].Origin);Object.freeze(T[i].Pitch);Object.freeze(T[i]);Object.freeze(E[i]);
        }
        Object.freeze(T);Object.freeze(E);
      end;
    end;
    {$ENDIF}
    Receipt:=W.BeginMappedEpoch(T,E,ExpectedRun.Seed,P,W.PublicationRevision);
    Check(Hits=0,'passive ordinary/frozen arrays never dispatch caller slice');
    AssertReconstructed(W,ExpectedModel,ExpectedRun);
    if Kind<>2 then
    begin
      T[0].Origin.X:=888; E[0].X:=99;
      AssertReconstructed(W,ExpectedModel,ExpectedRun);
    end;
    ActualRun:=W.CopyAppliedRun;
    E:=ActualRun.CopyPassExtents; E[0].X:=100;
    AssertReconstructed(W,ExpectedModel,ExpectedRun);
    ExportReplayJournal(W.CopyCanonicalJournal);
  finally Receipt.Free; ActualRun.Free; ExpectedRun.Free; R.Free; ExpectedModel.Free; M.Free; end;
end;

procedure TestGeometryInputBoundaries;
var W: TWfcPipelineWorkspaceWorkbench; I: Integer;
begin
  W:=TWfcPipelineWorkspaceWorkbench.Create;
  try
    OpenGeometry(W,True);
    {$IFDEF PAS2JS}for I:=0 to 15 do TestRawGeometryCase(W,I);{$ENDIF}
    for I:=0 to 2 do TestPassiveGeometry(W,I);
  finally W.Free; end;
end;

procedure TestPipelineWorkspaceGeometry;
begin
  TestChangedGeometryAndHistory;
  TestRejectedGeometry;
  TestLegacyAndEmptyDomain;
  TestGeometryInputBoundaries;
end;
end.
