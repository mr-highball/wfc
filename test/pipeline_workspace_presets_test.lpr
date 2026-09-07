{ SPDX-License-Identifier: MIT
  Copyright (c) 2021 mr-highball
  Independent preset construction and generated-output contracts. }
program pipeline_workspace_presets_test;
{$mode delphi}{$H+}
uses
  {$IFDEF PAS2JS}wfc_browser_test_host,{$ENDIF}
  SysUtils, wfc, wfc_model, wfc_lattice, wfc_rule_model,
  wfc_sequence, wfc_sequence_learn, wfc_sequence_text,
  wfc_pipeline_model, wfc_pipeline_layout, wfc_pipeline_run,
  wfc_pipeline_text, wfc_pipeline_run_text, wfc_pipeline_workspace_context,
  wfc_pipeline_prepare, wfc_pipeline_session, pipeline_workspace_presets;

var Checks: Integer;
procedure Check(const Condition: Boolean; const Detail: String);
begin
  Inc(Checks);
  if not Condition then raise Exception.Create('preset check '+IntToStr(Checks)+': '+Detail);
end;
function Tokens1(const A: String): TWfcModelTokens;
begin SetLength(Result,1); Result[0]:=A; end;
function Tokens2(const A,B: String): TWfcModelTokens;
begin SetLength(Result,2); Result[0]:=A; Result[1]:=B; end;
function Limits: TWfcPipelineWorkspaceContextLimits;
begin Result.Version:=1; Result.MaxRecipes:=1; Result.MaxRuns:=1; Result.MaxTextBytes:=1048576; end;
function ReplacementLimits: TWfcPipelineReplacementLimits;
begin
  Result.Version:=1; Result.MaxRetainedCellRecords:=4096;
  Result.MaxRetainedValueItems:=65536; Result.MaxCandidateVisits:=1048576;
end;
function OutcomeLimits: TWfcPipelineSessionOutcomeLimits;
begin
  Result.Version:=1; Result.MaxPublicCellRecords:=4096; Result.MaxEncodedTokenBytes:=1048576;
  Result.MaxReportPassRecords:=65536; Result.MaxTraceEvents:=65536;
  Result.MaxExcludedAssignmentItems:=65536;
end;
function Root(const LabelName: String): TGraphPassLabels;
begin SetLength(Result,1); Result[0]:=LabelName; end;
procedure SmallLandscape(out T: TWfcPipelinePassTopologies; out E: TWfcPipelinePassExtents);
begin
  DefaultMappedLandscapeGeometry(T,E);
  E[0]:=MakeWfcLatticeVector(2,2,1); E[1]:=MakeWfcLatticeVector(8,8,1);
  E[2]:=MakeWfcLatticeVector(1,1,1);
  T[2]:=MakeWfcPipelinePassTopology(2,MakeWfcLatticeVector(2,2,0),MakeWfcLatticeVector(4,4,1),False);
end;

{ This oracle deliberately does not call the mapped evaluator, coverage query,
  cell-box helper or a solver validator. For these small bounded fixtures it
  computes literal half-open boxes and scans every provider cell independently. }
type TBox = record X0,Y0,Z0,X1,Y1,Z1: Integer; end;
function BoxAt(const L: TWfcLatticeLayout; const Index: Integer): TBox;
var X,Y,Z: Integer;
begin
  X:=Index mod L.Cells.X; Y:=(Index div L.Cells.X) mod L.Cells.Y;
  Z:=Index div (L.Cells.X*L.Cells.Y);
  Result.X0:=L.Origin.X+X*L.Pitch.X; Result.X1:=Result.X0+L.Pitch.X;
  Result.Y0:=L.Origin.Y+Y*L.Pitch.Y; Result.Y1:=Result.Y0+L.Pitch.Y;
  Result.Z0:=L.Origin.Z+Z*L.Pitch.Z; Result.Z1:=Result.Z0+L.Pitch.Z;
end;
function Intersects(const A,B: TBox): Boolean;
begin
  Result:=(A.X0<B.X1) and (B.X0<A.X1) and (A.Y0<B.Y1) and
    (B.Y0<A.Y1) and (A.Z0<B.Z1) and (B.Z0<A.Z1);
end;
function FullyCoveredBy(const Consumer: TBox; const Provider: TWfcPipelineSessionLayer;
  const RequiredToken: String): Boolean;
var I,Visited: Integer; B: TBox;
begin
  Check(not Provider.Layout.Wrap,'literal oracle deliberately bounded');
  Result:=(Consumer.X0>=Provider.Layout.Origin.X) and
    (Consumer.Y0>=Provider.Layout.Origin.Y) and (Consumer.Z0>=Provider.Layout.Origin.Z) and
    (Consumer.X1<=Provider.Layout.Origin.X+Provider.Layout.Cells.X*Provider.Layout.Pitch.X) and
    (Consumer.Y1<=Provider.Layout.Origin.Y+Provider.Layout.Cells.Y*Provider.Layout.Pitch.Y) and
    (Consumer.Z1<=Provider.Layout.Origin.Z+Provider.Layout.Cells.Z*Provider.Layout.Pitch.Z);
  Visited:=0;
  for I:=0 to High(Provider.Cells) do
  begin
    B:=BoxAt(Provider.Layout,I);
    if Intersects(Consumer,B) then
    begin
      Inc(Visited);
      if Provider.Cells[I].Empty or not Provider.Cells[I].Generated or
        (Provider.Cells[I].Token<>RequiredToken) then Result:=False;
    end;
  end;
  Result:=Result and (Visited>0);
end;
procedure ValidateLandscape(const State: TWfcPipelineSessionPublicState);
var Terrain,Foliage,Housing: TWfcPipelineSessionLayer; I: Integer;
begin
  Check(State.LayerCount=3,'three public generated layers');
  Terrain:=State.LayerAt(0); Foliage:=State.LayerAt(1); Housing:=State.LayerAt(2);
  Check((Terrain.PassIndex=0) and (Foliage.PassIndex=1) and (Housing.PassIndex=2),'public pass identities');
  for I:=0 to High(Terrain.Cells) do
    Check(not Terrain.Cells[I].Empty and Terrain.Cells[I].Generated and
      ((Terrain.Cells[I].Token='land') or (Terrain.Cells[I].Token='water')),'actual terrain vocabulary');
  for I:=0 to High(Foliage.Cells) do
  begin
    Check(not Foliage.Cells[I].Empty and Foliage.Cells[I].Generated and
      ((Foliage.Cells[I].Token='clear') or (Foliage.Cells[I].Token='tree')),'actual foliage vocabulary');
    if Foliage.Cells[I].Token='tree' then
      Check(FullyCoveredBy(BoxAt(Foliage.Layout,I),Terrain,'land'),'entire tree footprint is land');
  end;
  for I:=0 to High(Housing.Cells) do
  begin
    Check(not Housing.Cells[I].Empty and Housing.Cells[I].Generated and
      ((Housing.Cells[I].Token='house') or (Housing.Cells[I].Token='vacant')),'actual housing vocabulary');
    if Housing.Cells[I].Token='house' then
    begin
      Check(FullyCoveredBy(BoxAt(Housing.Layout,I),Terrain,'land'),'entire house footprint is land');
      Check(FullyCoveredBy(BoxAt(Housing.Layout,I),Foliage,'clear'),'every intersected foliage cell clear');
    end;
  end;
end;

procedure TestDefinitionAndOwnership;
var T: TWfcPipelinePassTopologies; E: TWfcPipelinePassExtents;
  O: TWfcWorkspacePresetRunOptions; W: TWfcWorkspaceLandscapeWeights;
  C,D: TWfcPipelineWorkspaceContexts; R: TWfcPipelineModel; U: TWfcPipelineRun;
  L: TWfcPipelineWorkspaceContextLimits; Q: TWfcPipelineRequirement; I: Integer;
  RecipeText,RunText: String; Rejected: Boolean;
begin
  SmallLandscape(T,E); O:=DefaultWorkspacePresetRunOptions; W:=DefaultWorkspaceLandscapeWeights;
  O.Seed:=High(Cardinal); O.MaxBacktracks:=0; O.MaxPassBacktracks:=0; O.CaptureTrace:=True;
  C:=BuildMappedLandscapePreset(T,E,O,W,nil,nil,Limits); D:=nil; R:=nil; U:=nil;
  try
    D:=BuildMappedLandscapePreset(T,E,O,W,nil,nil,Limits);
    Check((C.RecipeCount=1) and (C.RunCount=1) and (C.RunTextAt(0).RecipeIndex=0),'complete bound contexts');
    RecipeText:=C.RecipeTextAt(0); RunText:=C.RunTextAt(0).Text;
    Check((RecipeText=D.RecipeTextAt(0)) and (RunText=D.RunTextAt(0).Text),'full canonical determinism');
    R:=C.CopyRecipe(0); U:=C.CopyRun(0);
    Check(R.HasPassMapping and (R.PassCount=3) and (R.ResourceCount=3),'three independent resources');
    Check((R.RequirementCount=3) and (R.DependencyCount=3),'complete explicit coverage DAG');
    Check((U.FormatVersion=2) and (U.TotalCellCount=69),'actual unlike cell count');
    Check((U.Seed=High(Cardinal)) and (U.MaxBacktracks=0) and (U.MaxPassBacktracks=0) and U.CaptureTrace,
      'zero budgets and explicit run options retained');
    Check((U.LockCount=0) and (U.DomainCount=0),'builder inserted no zoning or demand');
    for I:=0 to 2 do
    begin
      Q:=R.RequirementAt(I);
      Check((Q.Kind=wprqMapped) and (Q.MappedQuery.Kind=gpmkCellCoverage) and
        (Q.MappedQuery.Match=gpmmAll),'not point or count approximation');
      Check((R.ResourceAt(I).SourceLicenseIdentifier='MIT') and
        (R.ResourceAt(I).SourceDescription<>''),'resource provenance');
      Check(R.PassAt(I).Visibility=wppvPublic,'editable public rules pass');
    end;
    Check((R.BorrowRuleResource(0).WeightAt(0)=W.Land) and
      (R.BorrowRuleResource(1).WeightAt(0)=W.Clear) and
      (R.BorrowRuleResource(2).WeightAt(0)=W.House),'caller weights retained');
    L:=Limits; L.MaxTextBytes:=Length(RecipeText)+Length(RunText);
    FreeAndNil(D); D:=BuildMappedLandscapePreset(T,E,O,W,nil,nil,L);
    Check(D.TextBytes=L.MaxTextBytes,'exact aggregate context text boundary');
    FreeAndNil(D); Dec(L.MaxTextBytes); Rejected:=False;
    try D:=BuildMappedLandscapePreset(T,E,O,W,nil,nil,L);
    except on EWfcPipelineWorkspaceContext do Rejected:=True; end;
    Check(Rejected and (D=nil),'one-less context budget rejects with typed context error');
    T[0].Origin.X:=-9; E[1].X:=7; W.Land:=1;
    Check((C.RecipeTextAt(0)=RecipeText) and (C.RunTextAt(0).Text=RunText),'caller geometry mutation isolated');
    FreeAndNil(C);
    Check((EncodeWfcPipelineModelText(R)=RecipeText) and (EncodeWfcPipelineRunText(U)=RunText),
      'detached models survive context disposal');
  finally U.Free; R.Free; D.Free; C.Free; end;
end;

procedure TestGeneratedInteriorBlocker;
var T: TWfcPipelinePassTopologies; E: TWfcPipelinePassExtents;
  O: TWfcWorkspacePresetRunOptions; W: TWfcWorkspaceLandscapeWeights;
  Domains: TWfcPipelineCellDomains; C: TWfcPipelineWorkspaceContexts;
  S: TWfcPipelinePreparedSession; U: TWfcPipelineRun; Edit: TWfcPipelineSessionEditOutcome;
  Plan: TWfcPipelineSessionRepairPlan; Outcome: TWfcPipelineSessionOutcome;
  State: TWfcPipelineSessionPublicState; F,H: TWfcPipelineSessionLayer;
  I,X,Y: Integer;
begin
  SmallLandscape(T,E); O:=DefaultWorkspacePresetRunOptions; W:=DefaultWorkspaceLandscapeWeights;
  { Explicit tutorial inputs. This is disclosed domain zoning, not hidden seed
    selection or a prebuilt output. Every inspected cell comes from WFC. }
  SetLength(Domains,69);
  for Y:=0 to 1 do for X:=0 to 1 do
    Domains[Y*2+X]:=MakeWfcPipelineCellDomain(0,X,Y,0,Tokens1('land'));
  for Y:=0 to 7 do for X:=0 to 7 do
  begin
    I:=4+Y*8+X;
    if (X=3) and (Y=3) then Domains[I]:=MakeWfcPipelineCellDomain(1,X,Y,0,Tokens1('tree'))
    else Domains[I]:=MakeWfcPipelineCellDomain(1,X,Y,0,Tokens1('clear'));
  end;
  Domains[68]:=MakeWfcPipelineCellDomain(2,0,0,0,Tokens1('vacant'));
  C:=BuildMappedLandscapePreset(T,E,O,W,nil,Domains,Limits);
  S:=nil; U:=nil; Edit:=nil; Plan:=nil; Outcome:=nil; State:=nil;
  try
    S:=TWfcPipelinePreparedSession.Create(C.BorrowRecipe(0),C.BorrowRun(0),ReplacementLimits,OutcomeLimits);
    Outcome:=S.ExecuteInitial;
    Check(Outcome.Solved and Outcome.HasCurrentOutput,'zoned baseline is actually generated');
    State:=Outcome.CopyPublicState; ValidateLandscape(State);
    F:=State.LayerAt(1); H:=State.LayerAt(2);
    Check(F.Cells[2+2*8].Token='clear','house lower corner is clear');
    Check((F.Cells[3+3*8].Token='tree') and F.Cells[3+3*8].Generated,'strict interior blocker is generated');
    Check(not FullyCoveredBy(BoxAt(H.Layout,0),F,'clear'),'independent full-footprint counterexample');
    FreeAndNil(State); FreeAndNil(Outcome);
    Domains[68]:=MakeWfcPipelineCellDomain(2,0,0,0,Tokens1('house'));
    U:=TWfcPipelineRun.Create(C.BorrowRecipe(0),E,O.Seed,O.Strategy,
      O.MaxBacktracks,O.MaxPassBacktracks,O.CaptureTrace,nil,Domains);
    Edit:=S.ApplyInputs(U); Check(not Edit.HasCurrentOutput,'demand invalidates current publication');
    FreeAndNil(Edit); Plan:=S.PlanRepair(U,Root('housing'));
    Check(Plan.CanExecute,'housing-only request is authorized for a housing edit');
    Outcome:=S.ExecuteRepair(Plan);
    Check(not Outcome.Solved and not Outcome.HasCurrentOutput and Outcome.HasSuccessfulBaseline,
      'interior blocker causes real housing-only failure with retained baseline');
    FreeAndNil(Outcome); FreeAndNil(Plan); FreeAndNil(U);
    Domains[4+3*8+3]:=MakeWfcPipelineCellDomain(1,3,3,0,Tokens1('clear'));
    U:=TWfcPipelineRun.Create(C.BorrowRecipe(0),E,O.Seed,O.Strategy,
      O.MaxBacktracks,O.MaxPassBacktracks,O.CaptureTrace,nil,Domains);
    Edit:=S.ApplyInputs(U); FreeAndNil(Edit);
    Plan:=S.PlanRepair(U,Root('housing'));
    Check(not Plan.CanExecute,'foliage edit cannot silently widen a housing-only permission');
    Check(Length(Plan.CopyScope.MissingPassIndices)>0,'missing upstream scope is reported');
    FreeAndNil(Plan); Plan:=S.PlanRepair(U,Root('foliage'));
    Check(Plan.CanExecute,'explicit foliage root includes downstream housing');
    Outcome:=S.ExecuteRepair(Plan);
    Check(Outcome.Solved and Outcome.HasCurrentOutput,'bounded authorized upstream repair generates demand');
    State:=Outcome.CopyPublicState; ValidateLandscape(State); H:=State.LayerAt(2);
    Check(H.Cells[0].Token='house','actual demanded house, not a fabricated output');
  finally State.Free; Outcome.Free; Plan.Free; Edit.Free; U.Free; S.Free; C.Free; end;
end;

function IndependentCorpus: TWfcSequenceSamples;
var V: TWfcModelTokens;
begin
  { Literal independent transcription of the documented MIT corpus. Do not
    call the shipping corpus getter as the oracle for its own resource bytes. }
  SetLength(Result,3); SetLength(V,5);
  V[0]:='step'; V[1]:='step'; V[2]:='turn'; V[3]:='step'; V[4]:='rest';
  Result[0]:=MakeWfcSequenceSample(V);
  SetLength(V,4); V[0]:='step'; V[1]:='turn'; V[2]:='step'; V[3]:='rest';
  Result[1]:=MakeWfcSequenceSample(V);
  SetLength(V,5); V[0]:='rest'; V[1]:='step'; V[2]:='step'; V[3]:='turn'; V[4]:='rest';
  Result[2]:=MakeWfcSequenceSample(V);
end;

procedure TestLearnedSequence(const Wrapped: Boolean);
var T: TWfcPipelinePassTopologies; E: TWfcPipelinePassExtents;
  O: TWfcWorkspacePresetRunOptions; Q: TWfcWorkspaceSequenceOptions;
  C,D: TWfcPipelineWorkspaceContexts; Expected: TWfcSequenceModel;
  S: TWfcPipelinePreparedSession; Outcome: TWfcPipelineSessionOutcome;
  State: TWfcPipelineSessionPublicState; PublicLayer,AliasLayer: TWfcPipelineSessionLayer;
  Domains: TWfcPipelineCellDomains; Locks: TWfcPipelineCellLocks;
  Copies: TWfcSequenceSamples; Values: TWfcModelTokens; I: Integer;
  Versions: TWfcPipelineVersions; RecipeText: String;
begin
  DefaultLearnedSequenceGeometry(T,E); O:=DefaultWorkspacePresetRunOptions;
  Q:=DefaultWorkspaceSequenceOptions;
  T[0]:=MakeWfcPipelinePassTopology(3,MakeWfcLatticeVector(-7,6,2),MakeWfcLatticeVector(2,1,4),False);
  E[0]:=MakeWfcLatticeVector(2,1,2);
  if Wrapped then begin Q.Order:=1; Q.Boundary:=wmbWrap; Q.Extent:=wseWrap; end;
  for I:=1 to 3 do begin E[I]:=MakeWfcLatticeVector(4,1,1); T[I].Wrap:=Wrapped; end;
  SetLength(Values,4); Values[0]:='step'; Values[1]:='turn'; Values[2]:='step'; Values[3]:='rest';
  SetLength(Domains,4);
  for I:=0 to 3 do Domains[I]:=MakeWfcPipelineCellDomain(3,I,0,0,Tokens1(Values[I]));
  SetLength(Locks,1); Locks[0]:=MakeWfcPipelineCellLock(0,1,0,1,'closed');
  C:=BuildLearnedSequencePreset(T,E,O,Q,Locks,Domains,Limits);
  D:=nil; Expected:=nil; S:=nil; Outcome:=nil; State:=nil;
  try
    Expected:=LearnSequenceModelCorpus(IndependentCorpus,Q.Order,Q.Boundary);
    Check(C.BorrowRecipe(0).ResourceAt(1).Document=EncodeWfcSequenceText(Expected),'exact real learned resource');
    Versions:=C.BorrowRecipe(0).CopyVersions;
    Check(Versions.SequenceBridgeVersion=2,'explicit inverse-capable sequence projection version');
    Check((C.BorrowRecipe(0).PassAt(1).Visibility=wppvPrivate) and
      (C.BorrowRecipe(0).PassAt(1).SequenceExtent=Q.Extent),'explicit private state extent');
    Check((C.BorrowRecipe(0).PassAt(3).Mode=gpmTransform) and
      (C.BorrowRecipe(0).PassAt(3).TransformSourceIndex=2),'real public exact-copy alias');
    Check((C.BorrowRun(0).TotalCellCount=16) and
      (C.BorrowRun(0).LockCount=1) and (C.BorrowRun(0).DomainCount=4),'all pass grids and inputs retained');
    Check((C.BorrowRecipe(0).DependencyCount=2) and (C.BorrowRecipe(0).BridgeCount=1),
      'no hidden cross-link from independent grid');
    Copies:=WorkspaceInlineSequenceCorpus; Copies[0].Tokens[0]:='edited';
    D:=BuildLearnedSequencePreset(T,E,O,Q,Locks,Domains,Limits);
    Check(C.RecipeTextAt(0)=D.RecipeTextAt(0),'corpus getter edits do not mutate future builds');
    RecipeText:=C.RecipeTextAt(0); FreeAndNil(D);
    S:=TWfcPipelinePreparedSession.Create(C.BorrowRecipe(0),C.BorrowRun(0),ReplacementLimits,OutcomeLimits);
    Outcome:=S.ExecuteInitial;
    Check(Outcome.Solved and Outcome.HasCurrentOutput,'actual learned projection with caller alias domains');
    State:=Outcome.CopyPublicState;
    Check(State.LayerCount=3,'latent state pass is not a public layer');
    PublicLayer:=State.LayerAt(1); AliasLayer:=State.LayerAt(2);
    Check((PublicLayer.PassIndex=2) and (AliasLayer.PassIndex=3),'public projection and alias identities');
    for I:=0 to 3 do
    begin
      Check(PublicLayer.Cells[I].Generated and not PublicLayer.Cells[I].Empty and
        (PublicLayer.Cells[I].Token=Values[I]),'projected known corpus path');
      Check(not AliasLayer.Cells[I].Empty and (AliasLayer.Cells[I].Token=PublicLayer.Cells[I].Token),
        'alias equals actual public projection');
    end;
    Check(C.RecipeTextAt(0)=RecipeText,'solving does not rewrite immutable corpus/context');
  finally State.Free; Outcome.Free; S.Free; Expected.Free; D.Free; C.Free; end;
end;

procedure TestTypedRejections;
var T: TWfcPipelinePassTopologies; E: TWfcPipelinePassExtents;
  O: TWfcWorkspacePresetRunOptions; W: TWfcWorkspaceLandscapeWeights;
  Q: TWfcWorkspaceSequenceOptions; L: TWfcPipelineWorkspaceContextLimits;
  C: TWfcPipelineWorkspaceContexts; Domains: TWfcPipelineCellDomains;
  Failed: Boolean; I: Integer;
begin
  for I:=0 to 8 do
  begin
    C:=nil; Domains:=nil; SmallLandscape(T,E); O:=DefaultWorkspacePresetRunOptions;
    W:=DefaultWorkspaceLandscapeWeights; Q:=DefaultWorkspaceSequenceOptions; L:=Limits;
    case I of
      0: W.House:=0;
      1: O.MaxBacktracks:=-1;
      2: L.Version:=2;
      3: E[2].X:=0;
      4: begin SetLength(Domains,1); Domains[0]:=MakeWfcPipelineCellDomain(2,1,0,0,Tokens1('house')); end;
      5: begin SetLength(Domains,1); Domains[0]:=MakeWfcPipelineCellDomain(1,0,0,0,Tokens2('tree','clear')); end;
      6: begin SetLength(Domains,1); Domains[0]:=MakeWfcPipelineCellDomain(1,0,0,0,Tokens2('tree','tree')); end;
      7: begin DefaultLearnedSequenceGeometry(T,E); E[3].X:=11; end;
      8: begin DefaultLearnedSequenceGeometry(T,E); T[2].Origin.X:=12; end;
    end;
    Failed:=False;
    try
      if I<7 then C:=BuildMappedLandscapePreset(T,E,O,W,nil,Domains,L)
      else C:=BuildLearnedSequencePreset(T,E,O,Q,nil,nil,L);
    except
      on Ex: EWfcWorkspacePreset do begin Check(I<=2,'preset error category'); Failed:=True; end;
      on Ex: EWfcLattice do begin Check(I=3,'geometry error category'); Failed:=True; end;
      on Ex: EWfcPipelineRun do begin Check(I>=4,'run/input/link error category'); Failed:=True; end;
    end;
    C.Free; Check(Failed,'typed rejection '+IntToStr(I));
  end;
  SmallLandscape(T,E); SetLength(Domains,1);
  Domains[0]:=MakeWfcPipelineCellDomain(2,0,0,0,nil);
  C:=BuildMappedLandscapePreset(T,E,DefaultWorkspacePresetRunOptions,
    DefaultWorkspaceLandscapeWeights,nil,Domains,Limits);
  try Check((C.BorrowRun(0).DomainCount=1) and
    (Length(C.BorrowRun(0).DomainAt(0).AllowedTokens)=0),'explicit empty domain remains present');
  finally C.Free; end;
end;

procedure TestCallerGeometry;
var T: TWfcPipelinePassTopologies; E: TWfcPipelinePassExtents;
  C: TWfcPipelineWorkspaceContexts; I: Integer;
begin
  { A graph-free width larger than any example default. Rank-one local unused
    axes still have meaningful signed world origins and nonunit world pitches. }
  SetLength(T,3); SetLength(E,3);
  T[0]:=MakeWfcPipelinePassTopology(1,MakeWfcLatticeVector(-100,-3,7),MakeWfcLatticeVector(2,3,5),False);
  T[1]:=MakeWfcPipelinePassTopology(1,MakeWfcLatticeVector(-100,-3,7),MakeWfcLatticeVector(1,3,5),False);
  T[2]:=MakeWfcPipelinePassTopology(1,MakeWfcLatticeVector(-95,-3,7),MakeWfcLatticeVector(10,3,5),False);
  E[0]:=MakeWfcLatticeVector(5000,1,1); E[1]:=MakeWfcLatticeVector(10000,1,1);
  E[2]:=MakeWfcLatticeVector(100,1,1);
  C:=BuildMappedLandscapePreset(T,E,DefaultWorkspacePresetRunOptions,
    DefaultWorkspaceLandscapeWeights,nil,nil,Limits);
  try
    Check(C.BorrowRun(0).TotalCellCount=15100,'caller geometry is not clamped to illustration dimensions');
    for I:=0 to 2 do
    begin
      Check(C.BorrowRecipe(0).BorrowRuleResource(I).Rank=1,'rule rank follows corresponding caller topology');
      Check((C.BorrowRun(0).PassLayoutAt(I).Origin.Z=7) and
        (C.BorrowRun(0).PassLayoutAt(I).Pitch.Z=5),'inactive local axis preserves world geometry');
    end;
  finally C.Free; end;
end;

{$IFDEF PAS2JS}
procedure TestRawCase(const Mode: Integer);
var T: TWfcPipelinePassTopologies; E: TWfcPipelinePassExtents;
  O: TWfcWorkspacePresetRunOptions; W: TWfcWorkspaceLandscapeWeights;
  Q: TWfcWorkspaceSequenceOptions; L: TWfcPipelineWorkspaceContextLimits;
  C: TWfcPipelineWorkspaceContexts; Reads: Integer; Failed: Boolean;
begin
  SmallLandscape(T,E); O:=DefaultWorkspacePresetRunOptions; W:=DefaultWorkspaceLandscapeWeights;
  Q:=DefaultWorkspaceSequenceOptions; L:=Limits; Reads:=0; C:=nil;
  if Mode=8 then DefaultLearnedSequenceGeometry(T,E);
  { A fresh typed local per case: deliberately malformed raw records must never
    be reused through the compiler's record $assign operation on a later case. }
  asm
    switch(Mode) {
      case 0: O=null; break;
      case 1: Object.defineProperty(O,'Seed',{get:function(){Reads++;return 7;}}); break;
      case 2: O.CaptureTrace=0; break;
      case 3: W.Clear=NaN; break;
      case 4: Object.defineProperty(W,'House',{get:function(){Reads++;return 8;}}); break;
      case 5: L.Version=2; break;
      case 6: Object.defineProperty(L,'MaxTextBytes',{get:function(){Reads++;return 1048576;}}); break;
      case 7: Object.defineProperty(T[0].Pitch,'X',{get:function(){Reads++;return 4;}}); break;
      case 8: Object.defineProperty(Q,'Order',{get:function(){Reads++;return 2;}}); break;
      case 9: delete E[1]; break;
      case 10: T=null; break;
      case 11: O.MaxPassBacktracks=0.5; break;
      case 12:
        Object.defineProperty(T,'slice',{get:function(){Reads++;return null;}});
        Object.defineProperty(E,'slice',{get:function(){Reads++;return null;}});
        Object.freeze(T); Object.freeze(E); Object.freeze(O); Object.freeze(W); Object.freeze(L);
        break;
      case 13:
        T.slice=function(){Reads++;return this;}; E.slice=null;
        break;
    }
  end;
  Failed:=False;
  try
    if Mode=8 then C:=BuildLearnedSequencePreset(T,E,O,Q,nil,nil,L)
    else C:=BuildMappedLandscapePreset(T,E,O,W,nil,nil,L);
  except
    on EWfcWorkspacePreset do Failed:=True;
    on EWfcLattice do Failed:=True;
  end;
  try
    Check(Failed=(Mode<12),'raw typed rejection / passive dense success '+IntToStr(Mode));
    Check(Reads=0,'no caller getter or array method dispatch '+IntToStr(Mode));
    if Mode>=12 then Check((C.BorrowRun(0).TotalCellCount=69),'positive hostile-method geometry copied normally');
  finally C.Free; end;
end;
{$ENDIF}

var I: Integer;
begin
  TestDefinitionAndOwnership;
  TestGeneratedInteriorBlocker;
  TestLearnedSequence(False); TestLearnedSequence(True);
  TestTypedRejections; TestCallerGeometry;
  {$IFDEF PAS2JS}for I:=0 to 13 do TestRawCase(I);{$ENDIF}
  WriteLn('Independent workspace preset checks: ',Checks);
end.
