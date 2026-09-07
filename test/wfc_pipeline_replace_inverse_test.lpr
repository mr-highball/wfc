{ SPDX-License-Identifier: MIT
  Copyright (c) 2021 mr-highball
  Real learned inverse domains across editable input replacement. }
program wfc_pipeline_replace_inverse_test;
{$mode delphi}{$H+}
uses {$IFDEF PAS2JS}wfc_browser_test_host,{$ENDIF}
  SysUtils, wfc, wfc_lattice, wfc_model, wfc_rule_model, wfc_rule_text,
  wfc_sequence, wfc_sequence_learn, wfc_sequence_text,
  wfc_pattern2d, wfc_pattern2d_learn, wfc_pattern2d_text,
  wfc_pattern3d, wfc_pattern3d_learn, wfc_pattern3d_text,
  wfc_pipeline_model, wfc_pipeline_layout, wfc_pipeline_run,
  wfc_pipeline_compile, wfc_pipeline_prepare;

type
  TCellState = record
    Empty, Generated: Boolean;
    Value: TGraphValue;
  end;
  TCellStates = array of TCellState;
var Checks: Integer;

procedure Check(const OK: Boolean; const Detail: String);
begin Inc(Checks); if not OK then raise Exception.Create(Detail); end;

function Tokens(const Values: array of TWfcModelToken): TWfcModelTokens;
var I: Integer;
begin
  Result:=nil; SetLength(Result,Length(Values));
  for I:=0 to High(Values) do Result[I]:=Values[I];
end;

function MakeRecipe(const Kind, BridgeVersion: Integer; const ThinWrap: Boolean;
  out Extents: TWfcPipelinePassExtents): TWfcPipelineModel;
var Resources: TWfcPipelineResources; Passes: TWfcPipelinePasses;
  Dependencies: TWfcPipelineDependencies; Bridges: TWfcPipelineBridges;
  Topologies: TWfcPipelinePassTopologies; Versions: TWfcPipelineVersions;
  Rules: TWfcRuleModel; Weights: TWfcModelIntegerArray;
  Sequence: TWfcSequenceModel; Samples: TWfcSequenceSamples;
  Pattern2: TWfcOverlappingModel2D; Pattern3: TWfcOverlappingModel3D;
  Document: String; ResourceKind: TWfcPipelineResourceKind;
  Adapter: TWfcPipelineAdapterKind; Bridge: TWfcPipelineBridgeKind;
  Rank,I: Integer;
begin
  SetLength(Resources,2); SetLength(Weights,2); Weights[0]:=1; Weights[1]:=1;
  Rules:=TWfcRuleModel.Create(1,Tokens(['A','B']),Weights,nil);
  try Resources[0]:=MakeWfcPipelineResource('unrelated',wprkRules,
    EncodeWfcRuleText(Rules),'project-authored unrelated branch','MIT','');
  finally Rules.Free; end;
  if Kind=0 then
  begin
    SetLength(Samples,3);
    Samples[0]:=MakeWfcSequenceSample(Tokens(['B','A','B']));
    Samples[1]:=MakeWfcSequenceSample(Tokens(['B','C','B']));
    Samples[2]:=MakeWfcSequenceSample(Tokens(['A','B','A']));
    Sequence:=LearnSequenceModelCorpus(Samples,1);
    try Document:=EncodeWfcSequenceText(Sequence); finally Sequence.Free; end;
    ResourceKind:=wprkSequence; Adapter:=wpakSequence;
    Bridge:=wpbkSequenceProjection; Rank:=1;
  end
  else if Kind=1 then
  begin
    Pattern2:=LearnOverlappingModel2D(Tokens(['A','B','B','A']),2,2,2,2,wmbWrap,wmsNone);
    try Document:=EncodeWfcPattern2DText(Pattern2); finally Pattern2.Free; end;
    ResourceKind:=wprkPattern2D; Adapter:=wpakPattern2D;
    Bridge:=wpbkPattern2DProjection; Rank:=2;
  end
  else
  begin
    Pattern3:=LearnOverlappingModel3D(Tokens(['A','B','B','A','B','A','A','B']),
      2,2,2,2,2,2,wmbWrap,wmsNone);
    try Document:=EncodeWfcPattern3DText(Pattern3); finally Pattern3.Free; end;
    ResourceKind:=wprkPattern3D; Adapter:=wpakPattern3D;
    Bridge:=wpbkPattern3DProjection; Rank:=3;
  end;
  Resources[1]:=MakeWfcPipelineResource('learned',ResourceKind,Document,
    'project-authored learned replacement fixture','MIT','');
  SetLength(Passes,4); SetLength(Topologies,4); SetLength(Extents,4);
  Passes[0]:=MakeWfcPipelinePass('unrelated',wppvPublic,gpmOverlay,-1,wpakRules,0,False,wseWhole);
  Passes[1]:=MakeWfcPipelinePass('latent',wppvPrivate,gpmOverlay,-1,Adapter,1,Kind=0,wseWhole);
  Passes[2]:=MakeWfcPipelinePass('projection',wppvPublic,gpmOverlay,-1,wpakEmpty,-1,False,wseWhole);
  Passes[3]:=MakeWfcPipelinePass('alias',wppvPublic,gpmTransform,2,wpakEmpty,-1,False,wseWhole);
  Topologies[0]:=LegacyWfcPipelinePassTopology(1,False);
  Topologies[1]:=MakeWfcPipelinePassTopology(Rank,MakeWfcLatticeVector(-9,7,4),
    MakeWfcLatticeVector(3,5,7),Kind<>0);
  Extents[0]:=MakeWfcLatticeVector(2,1,1);
  case Kind of
    0: Extents[1]:=MakeWfcLatticeVector(3,1,1);
    1: Extents[1]:=MakeWfcLatticeVector(4,2,1);
    2: Extents[1]:=MakeWfcLatticeVector(4,2,2);
  end;
  if ThinWrap then Extents[1]:=MakeWfcLatticeVector(1,1,1);
  for I:=2 to 3 do begin Topologies[I]:=Topologies[1]; Extents[I]:=Extents[1]; end;
  SetLength(Dependencies,2);
  Dependencies[0]:=MakeWfcPipelineDependency(2,1);
  Dependencies[1]:=MakeWfcPipelineDependency(3,2);
  SetLength(Bridges,1); Bridges[0]:=MakeWfcPipelineBridge(Bridge,1,2);
  Versions:=CurrentWfcPipelineVersions;
  Versions.Pattern2DBridgeVersion:=BridgeVersion;
  Versions.SequenceBridgeVersion:=BridgeVersion;
  Result:=TWfcPipelineModel.Create(MakeWfcPipelineMetadata(
    'inverse replacement','MIT','real learned sources',''),Versions,1,False,
    rmBottomUp,Resources,Passes,Dependencies,Bridges,nil,nil,nil,
    WFC_PIPELINE_PASS_MAPPING_VERSION,Topologies);
end;

function NewRun(const Recipe: TWfcPipelineModel;
  const Extents: TWfcPipelinePassExtents; const Locks: TWfcPipelineCellLocks;
  const Domains: TWfcPipelineCellDomains): TWfcPipelineRun;
begin
  Result:=TWfcPipelineRun.Create(Recipe,Extents,71,wpssOneWay,256,0,False,Locks,Domains);
end;

function Snapshot(const Graph: TGraph; const Run: TWfcPipelineRun): TCellStates;
var Saved,P,X,Y,Z,N: Integer; G: TGraph; L: TWfcLatticeLayout; E: TGraphEntry;
begin
  Result:=nil; SetLength(Result,Run.TotalCellCount); N:=0;
  Saved:=Graph.CurrentPassIndex;
  try
    Graph.SwitchToPass(0);
    for P:=0 to Run.PassCount-1 do
    begin
      G:=Graph.PassGraph[P]; L:=Run.PassLayoutAt(P);
      for Z:=0 to L.Cells.Z-1 do for Y:=0 to L.Cells.Y-1 do for X:=0 to L.Cells.X-1 do
      begin
        E:=G.Entry[X,Y,Z]; Result[N].Empty:=E.Empty;
        Result[N].Generated:=E.Generated; Result[N].Value:=E.Value; Inc(N);
      end;
    end;
  finally Graph.SwitchToPass(Saved); end;
end;

procedure SameStates(const A,B: TCellStates; const Detail: String);
var I: Integer;
begin
  Check(Length(A)=Length(B),Detail+' state count');
  for I:=0 to High(A) do
    Check((A[I].Empty=B[I].Empty) and (A[I].Generated=B[I].Generated) and
      (A[I].Value=B[I].Value),Detail+' value/ownership '+IntToStr(I));
end;

procedure SameDomains(const Left,Right: TGraph; const Run: TWfcPipelineRun;
  const Detail: String; const LastPass: Integer=-1);
var SavedLeft,SavedRight,P,X,Y,Z,I,Stop: Integer;
  A,B: TGraph; L: TWfcLatticeLayout; AV,BV: TGraphValues;
begin
  SavedLeft:=Left.CurrentPassIndex; SavedRight:=Right.CurrentPassIndex;
  Stop:=LastPass; if Stop<0 then Stop:=Run.PassCount-1;
  try
    Left.SwitchToPass(0); Right.SwitchToPass(0);
    for P:=0 to Stop do
    begin
      A:=Left.PassGraph[P]; B:=Right.PassGraph[P]; L:=Run.PassLayoutAt(P);
      for Z:=0 to L.Cells.Z-1 do for Y:=0 to L.Cells.Y-1 do for X:=0 to L.Cells.X-1 do
      begin
        Check(A.HasAllowedValues(X,Y,Z)=B.HasAllowedValues(X,Y,Z),Detail+' domain presence');
        AV:=A.CopyAllowedValues(X,Y,Z); BV:=B.CopyAllowedValues(X,Y,Z);
        Check(Length(AV)=Length(BV),Detail+' domain count');
        for I:=0 to High(AV) do Check(AV[I]=BV[I],Detail+' complete canonical domain value');
      end;
    end;
  finally Left.SwitchToPass(SavedLeft); Right.SwitchToPass(SavedRight); end;
end;

procedure FixtureSanity(const Kind: Integer; const ThinWrap: Boolean);
var Recipe: TWfcPipelineModel; Extents: TWfcPipelinePassExtents;
  Run,LockedRun: TWfcPipelineRun; P: TWfcPipelinePreparation;
  Plan,LockedPlan: TWfcPipelineInputPlan; Base,Locked: TWfcPipelineInputBinding;
  Locks: TWfcPipelineCellLocks; G: TGraph;
begin
  Recipe:=MakeRecipe(Kind,2,ThinWrap,Extents); Run:=nil; LockedRun:=nil;
  P:=nil; Plan:=nil; LockedPlan:=nil; Base:=nil; Locked:=nil;
  try
    Run:=NewRun(Recipe,Extents,nil,nil); P:=TWfcPipelinePreparation.Create(Recipe,Run);
    Plan:=P.PrepareInputs(Run); Base:=TWfcPipelineInputBinding.Create(P,Plan);
    SetLength(Locks,1); Locks[0]:=MakeWfcPipelineCellLock(2,0,0,0,'A');
    LockedRun:=NewRun(Recipe,Extents,Locks,nil); LockedPlan:=P.PrepareInputs(LockedRun);
    Locked:=TWfcPipelineInputBinding.Create(P,LockedPlan);
    G:=Base.BorrowCompiled.Graph.PassGraph[1];
    if Kind=0 then
    begin
      Check(Length(G.CopyRegisteredValues)=3,'sequence has three real learned states');
      Check(G.HasAllowedValues(0,0,0) and (Length(G.CopyAllowedValues(0,0,0))=2),
        'sequence start has a proper two-of-three compiler base');
      Check(G.HasAllowedValues(2,0,0) and (Length(G.CopyAllowedValues(2,0,0))=2),
        'sequence end has a proper two-of-three compiler base');
    end
    else Check(not G.HasAllowedValues(0,0,0),'wrapped pattern starts without explicit base domain');
    G:=Locked.BorrowCompiled.Graph.PassGraph[1];
    Check(G.HasAllowedValues(0,0,0),'real public lock creates inverse domain');
    if ThinWrap then Check(Length(G.CopyAllowedValues(0,0,0))=0,
      'all repeated checker offsets constrain one anchor to explicit empty')
    else Check(Length(G.CopyAllowedValues(0,0,0))=1,
      'real public lock narrows the initial anchor');
  finally
    Locked.Free; Base.Free; LockedPlan.Free; Plan.Free; P.Free;
    LockedRun.Free; Run.Free; Recipe.Free;
  end;
end;

{$IFNDEF REPLACEMENT_FIXTURE_BASELINE}
function Limits: TWfcPipelineReplacementLimits;
begin
  Result:=Default(TWfcPipelineReplacementLimits); Result.Version:=1;
  Result.MaxRetainedCellRecords:=1000000;
  Result.MaxRetainedValueItems:=1000000;
  Result.MaxCandidateVisits:=16000000;
end;

procedure ReplaceAndCompare(const Recipe: TWfcPipelineModel;
  const Extents: TWfcPipelinePassExtents; const Preparation: TWfcPipelinePreparation;
  const Binding: TWfcPipelineInputBinding; const Locks: TWfcPipelineCellLocks;
  const Domains: TWfcPipelineCellDomains; const PreserveValues,IsNoOp: Boolean;
  const Detail: String);
var Run: TWfcPipelineRun; Plan: TWfcPipelineInputPlan; Fresh: TWfcPipelineInputBinding;
  Before: TCellStates; Impact: TWfcPipelineInputImpact; G: TGraph;
begin
  Run:=NewRun(Recipe,Extents,Locks,Domains); Plan:=nil; Fresh:=nil;
  try
    Plan:=Preparation.PrepareInputs(Run);
    Fresh:=TWfcPipelineInputBinding.Create(Preparation,Plan);
    G:=Binding.BorrowCompiled.Graph; G.SwitchToPass(3); Before:=Snapshot(G,Run);
    Impact:=Binding.ReplaceInputs(Plan);
    Check(G.CurrentPassIndex=3,Detail+' preserves actual selected alias pass');
    Check(Binding.Usable,Detail+' binding remains usable');
    SameDomains(G,Fresh.BorrowCompiled.Graph,Run,Detail);
    if PreserveValues then SameStates(Before,Snapshot(G,Run),Detail);
    if IsNoOp then
      Check((not Impact.AuthoredInputsChanged) and (not Impact.GraphInputsChanged) and
        (Length(Impact.AuthoredPassIndices)=0) and (Length(Impact.ChangedPassIndices)=0),
        Detail+' no-op reports no input impact');
  finally Fresh.Free; Plan.Free; Run.Free; end;
end;

procedure TestReplacement(const Kind,BridgeVersion: Integer;
  const InitiallyLocked,ThinWrap: Boolean);
var Recipe: TWfcPipelineModel; Extents: TWfcPipelinePassExtents;
  Run,BaseRun: TWfcPipelineRun; P: TWfcPipelinePreparation;
  Plan,BasePlan: TWfcPipelineInputPlan; B,Pristine: TWfcPipelineInputBinding;
  Locks: TWfcPipelineCellLocks; Domains: TWfcPipelineCellDomains;
  AllTokens: TWfcModelTokens; Options: TGraphSolveOptions; Report: TGraphSolveReport;
  LabelText: String; FarCell: TWfcLatticeVector; G: TGraph;
begin
  Recipe:=MakeRecipe(Kind,BridgeVersion,ThinWrap,Extents);
  Run:=nil; BaseRun:=nil; P:=nil; Plan:=nil; BasePlan:=nil; B:=nil; Pristine:=nil;
  try
    LabelText:='kind'+IntToStr(Kind)+'/bridge'+IntToStr(BridgeVersion);
    Locks:=nil; Domains:=nil;
    if InitiallyLocked then
    begin SetLength(Locks,1); Locks[0]:=MakeWfcPipelineCellLock(2,0,0,0,'A'); end;
    Run:=NewRun(Recipe,Extents,Locks,Domains); P:=TWfcPipelinePreparation.Create(Recipe,Run);
    Plan:=P.PrepareInputs(Run); B:=TWfcPipelineInputBinding.CreateEditable(P,Plan,Limits);
    BaseRun:=NewRun(Recipe,Extents,nil,nil); BasePlan:=P.PrepareInputs(BaseRun);
    Pristine:=TWfcPipelineInputBinding.Create(P,BasePlan);
    if (not ThinWrap) and (BridgeVersion=2) then
    begin
      Options:=DefaultGraphSolveOptions; Options.MaxBacktracks:=256;
      Check(B.BorrowCompiled.Graph.TrySolve(Options,Report),LabelText+' actual initial learned solve');
    end;
    SetLength(Locks,1); Locks[0]:=MakeWfcPipelineCellLock(2,0,0,0,'A');
    ReplaceAndCompare(Recipe,Extents,P,B,Locks,nil,InitiallyLocked,InitiallyLocked,LabelText+' lock origin');
    if BridgeVersion=1 then
      SameDomains(B.BorrowCompiled.Graph,Pristine.BorrowCompiled.Graph,Run,
        LabelText+' forward-only bridge does not create inverse domains',1);
    AllTokens:=Recipe.CopyPublicVocabulary(2);
    FarCell:=Extents[2]; Dec(FarCell.X); Dec(FarCell.Y); Dec(FarCell.Z);
    SetLength(Domains,2);
    Domains[0]:=MakeWfcPipelineCellDomain(2,FarCell.X,FarCell.Y,FarCell.Z,Tokens(['A']));
    Domains[1]:=MakeWfcPipelineCellDomain(3,0,0,0,AllTokens);
    ReplaceAndCompare(Recipe,Extents,P,B,Locks,Domains,True,False,LabelText+' two contributors');
    Domains[0]:=MakeWfcPipelineCellDomain(2,FarCell.X,FarCell.Y,FarCell.Z,AllTokens);
    Domains[1]:=MakeWfcPipelineCellDomain(3,0,0,0,Tokens(['A']));
    ReplaceAndCompare(Recipe,Extents,P,B,Locks,Domains,True,False,LabelText+' broaden far and reveal alias');
    Locks:=nil;
    ReplaceAndCompare(Recipe,Extents,P,B,Locks,Domains,False,False,LabelText+' unlock reveals domain');
    G:=B.BorrowCompiled.Graph.PassGraph[2];
    Check(G.Entry[0,0,0].Empty and (not G.Entry[0,0,0].Generated),
      LabelText+' removing caller lock really empties public owner');
    Domains[1]:=MakeWfcPipelineCellDomain(3,0,0,0,AllTokens);
    ReplaceAndCompare(Recipe,Extents,P,B,Locks,Domains,True,False,LabelText+' explicit full domains');
    Domains[1]:=MakeWfcPipelineCellDomain(3,0,0,0,nil);
    ReplaceAndCompare(Recipe,Extents,P,B,Locks,Domains,True,False,LabelText+' explicit empty alias');
    Domains:=nil;
    ReplaceAndCompare(Recipe,Extents,P,B,Locks,Domains,True,False,LabelText+' absent restores bases');
    SameDomains(B.BorrowCompiled.Graph,Pristine.BorrowCompiled.Graph,Run,LabelText+' exact pristine domains');
    SetLength(Locks,1); Locks[0]:=MakeWfcPipelineCellLock(2,FarCell.X,FarCell.Y,FarCell.Z,'A');
    ReplaceAndCompare(Recipe,Extents,P,B,Locks,nil,False,False,LabelText+' later far lock');
    Locks:=nil;
    ReplaceAndCompare(Recipe,Extents,P,B,Locks,nil,False,False,LabelText+' remove later far lock');
    SameDomains(B.BorrowCompiled.Graph,Pristine.BorrowCompiled.Graph,Run,LabelText+' later base restored');
    ReplaceAndCompare(Recipe,Extents,P,B,nil,nil,True,True,LabelText+' exact repeat');
  finally
    Pristine.Free; B.Free; BasePlan.Free; Plan.Free; P.Free;
    BaseRun.Free; Run.Free; Recipe.Free;
  end;
end;
{$ENDIF}

var Kind: Integer;
begin
  for Kind:=0 to 2 do FixtureSanity(Kind,False);
  FixtureSanity(1,True); FixtureSanity(2,True);
  {$IFNDEF REPLACEMENT_FIXTURE_BASELINE}
  for Kind:=0 to 2 do
  begin TestReplacement(Kind,2,False,False); TestReplacement(Kind,2,True,False); end;
  TestReplacement(0,1,False,False); TestReplacement(1,1,False,False);
  TestReplacement(1,2,True,True); TestReplacement(2,2,True,True);
  {$ENDIF}
  WriteLn('Private inverse replacement: ',Checks,' checks passed.');
end.
