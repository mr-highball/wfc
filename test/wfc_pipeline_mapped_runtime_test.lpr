{ SPDX-License-Identifier: MIT
  Copyright (c) 2021 mr-highball }
program wfc_pipeline_mapped_runtime_test;
{$mode delphi}{$H+}
uses
  {$IFDEF PAS2JS}wfc_browser_test_host,{$ENDIF}
  SysUtils, wfc, wfc_lattice, wfc_model, wfc_rule_model, wfc_rule_text,
  wfc_sequence, wfc_sequence_learn, wfc_sequence_text,
  wfc_pattern2d, wfc_pattern2d_learn, wfc_pattern2d_text,
  wfc_pattern3d, wfc_pattern3d_learn, wfc_pattern3d_text,
  wfc_pipeline_layout, wfc_pipeline_model, wfc_pipeline_mapping,
  wfc_pipeline_compile, wfc_pipeline_connectivity, wfc_pipeline_run,
  wfc_pipeline_result, wfc_pipeline_runtime;

type
  TFixture = record
    Resources: TWfcPipelineResources;
    Passes: TWfcPipelinePasses;
    Dependencies: TWfcPipelineDependencies;
    Bridges: TWfcPipelineBridges;
    Requirements: TWfcPipelineRequirements;
    Quotas: TWfcPipelineValueQuotas;
    Connectivities: TWfcPipelineConnectivities;
    Topologies: TWfcPipelinePassTopologies;
    Extents: TWfcPipelinePassExtents;
  end;
var Checks: Integer;

procedure Check(const Condition: Boolean; const MessageText: String);
begin
  Inc(Checks);
  if not Condition then raise Exception.Create(MessageText);
end;

function Tokens(const Values: array of TWfcModelToken): TWfcModelTokens;
var I: Integer;
begin
  Result := nil; SetLength(Result, Length(Values));
  for I := 0 to High(Values) do Result[I] := Values[I];
end;

function RulesResource(const Rank: Integer): TWfcPipelineResource;
var R: TWfcRuleModel; Weights: TWfcModelIntegerArray;
begin
  SetLength(Weights, 2); Weights[0] := 1; Weights[1] := 1;
  R := TWfcRuleModel.Create(Rank, Tokens(['A','B']), Weights, nil);
  try
    Result := MakeWfcPipelineResource(TWfcModelToken('rules-' + IntToStr(Rank)),
      wprkRules, EncodeWfcRuleText(R), 'project-authored', 'MIT', '');
  finally R.Free; end;
end;

function Fixture: TFixture;
var I: Integer;
begin
  Result := Default(TFixture);
  SetLength(Result.Resources, 1); Result.Resources[0] := RulesResource(1);
  SetLength(Result.Passes, 3); SetLength(Result.Topologies, 3);
  SetLength(Result.Extents, 3);
  for I := 0 to 2 do
  begin
    Result.Passes[I] := MakeWfcPipelinePass(TWfcModelToken('pass-' + IntToStr(I)),
      wppvPublic, gpmOverlay, -1, wpakRules, 0, False, wseWhole);
    Result.Topologies[I] := LegacyWfcPipelinePassTopology(1, False);
  end;
  Result.Extents[0] := MakeWfcLatticeVector(2,1,1);
  Result.Extents[1] := MakeWfcLatticeVector(5,1,1);
  Result.Extents[2] := MakeWfcLatticeVector(3,1,1);
end;

function Recipe(const F: TFixture): TWfcPipelineModel;
var V: TWfcPipelineVersions;
begin
  V := CurrentWfcPipelineVersions;
  V.Pattern2DBridgeVersion := 2; V.SequenceBridgeVersion := 2;
  Result := TWfcPipelineModel.Create(MakeWfcPipelineMetadata(
    'Mapped runtime fixture', 'MIT', 'project-authored', ''), V,
    F.Topologies[0].Rank, F.Topologies[0].Wrap, rmBottomUp,
    F.Resources, F.Passes, F.Dependencies, F.Bridges, F.Requirements,
    F.Quotas, F.Connectivities, WFC_PASS_MAPPING_VERSION, F.Topologies);
end;

procedure ForceToken(const G: TGraph; const Index: Integer; const Value: TGraphValue);
var L: TWfcLatticeLayout; X,Y,Z: Integer;
begin
  L := G.PassLayout;
  X := Index mod L.Cells.X; Y := (Index div L.Cells.X) mod L.Cells.Y;
  Z := Index div (L.Cells.X * L.Cells.Y);
  G.SetAllowedValues(X,Y,Z,Value);
end;

procedure ForceAll(const G: TGraph; const Value: TGraphValue);
var I: Integer;
begin for I := 0 to WfcLatticeCellCount(G.PassLayout)-1 do ForceToken(G,I,Value); end;

procedure ReplaceGroup(const G: TGraph; const Value: TGraphValue);
var Replacement: TGraphRuleGroup;
begin
  G.RuleGroups.Remove(Value);
  Replacement := TGraphRuleGroup.Create(Value);
  try G.RuleGroups.Add(Value,Replacement); Replacement := nil;
  finally Replacement.Free; end;
end;

procedure TestPrefixKeysAndAliases;
var F: TFixture; R: TWfcPipelineModel; Run: TWfcPipelineRun;
  Output: TWfcPipelineResult; Locks: TWfcPipelineCellLocks;
  Domains: TWfcPipelineCellDomains; Mode, AliasMode: Integer;
begin
  for AliasMode := 0 to 1 do
  begin
    F := Fixture;
    if AliasMode = 1 then
    begin
      F.Passes[2] := MakeWfcPipelinePass('alias',wppvPublic,gpmTransform,1,
        wpakEmpty,-1,False,wseWhole);
      SetLength(F.Dependencies,1); F.Dependencies[0] := MakeWfcPipelineDependency(2,1);
      F.Extents[2] := F.Extents[1];
    end;
    R := Recipe(F);
    try
      SetLength(Locks,2); SetLength(Domains,2);
      Locks[0] := MakeWfcPipelineCellLock(1,4,0,0,'A');
      Domains[0] := MakeWfcPipelineCellDomain(1,4,0,0,Tokens(['A']));
      Locks[1] := MakeWfcPipelineCellLock(2,2,0,0,'B');
      Domains[1] := MakeWfcPipelineCellDomain(2,2,0,0,Tokens(['B']));
      for Mode := 0 to 1 do
      begin
        Run := TWfcPipelineRun.Create(R,F.Extents,3,TWfcPipelineSolveStrategy(Mode),
          64,16*Mode,True,Locks,Domains);
        try
          Check((Run.PassOffsetAt(1)=2) and (Run.PassOffsetAt(2)=7),
            'prefixes include every local layout and alias storage');
          Check((Run.PassOffsetAt(1)+4=6) and (Run.PassOffsetAt(2)+2=9),
            'formerly colliding keys 6 and 9 stay distinct');
          Output := ExecuteWfcPipeline(R,Run);
          try
            Check(Output.Status=wprsSolved,'mixed-length public lock/domain runtime solves');
            Check(Length(Output.LayerAt(0).Tokens)=2,'tiny root extent retained');
            Check(Length(Output.LayerAt(1).Tokens)=5,'late larger layer captured');
            Check(Length(Output.LayerAt(2).Tokens)=F.Extents[2].X,'third layer has own extent');
            Check(Output.LayerAt(1).Tokens[4]='A','late source lock retained');
            Check(Output.LayerAt(2).Tokens[2]='B','distinct key lock retained');
            if AliasMode=1 then
              Check(Output.LayerAt(2).Tokens[4]='A','alias copies complete larger source');
          finally Output.Free; end;
        finally Run.Free; end;
      end;
    finally R.Free; end;
  end;
end;

procedure TestMappedCommit;
var F: TFixture; R: TWfcPipelineModel; C: TWfcCompiledPipeline;
  Q: TWfcPipelineMappedQuery; Options: TGraphSolveOptions; Report: TGraphSolveReport;
  Run: TWfcPipelineRun; Output: TWfcPipelineResult; I,Mode: Integer;
begin
  F := Fixture;
  F.Extents[1] := MakeWfcLatticeVector(6,1,1);
  F.Topologies[1] := MakeWfcPipelinePassTopology(1,MakeWfcLatticeVector(-9,7,4),
    MakeWfcLatticeVector(1,3,2),False);
  F.Topologies[2] := MakeWfcPipelinePassTopology(1,MakeWfcLatticeVector(-9,7,4),
    MakeWfcLatticeVector(2,3,2),False);
  SetLength(F.Dependencies,1); F.Dependencies[0] := MakeWfcPipelineDependency(2,1);
  Q := Default(TWfcPipelineMappedQuery); Q.Kind := gpmkCellCoverage; Q.Match := gpmmAll;
  Q.AllowedProviderTokens := Tokens(['A']);
  SetLength(F.Requirements,1); F.Requirements[0] := MakeWfcPipelineMappedRequirement(2,'A',1,Q);
  R := Recipe(F);
  try
    for Mode := 0 to 1 do
    begin
      Run := TWfcPipelineRun.Create(R,F.Extents,7,TWfcPipelineSolveStrategy(Mode),64,16*Mode,False,nil,nil);
      try
        Output := ExecuteWfcPipeline(R,Run);
        try
          Check(Output.Status=wprsSolved,'mapped runtime succeeds with variable topology');
          for I := 0 to 2 do
            if Output.LayerAt(2).Tokens[I]='A' then
              Check((Output.LayerAt(1).Tokens[I*2]='A') and
                (Output.LayerAt(1).Tokens[I*2+1]='A'),
                'literal two-cell public footprint agrees with solved output');
        finally Output.Free; end;
      finally Run.Free; end;
    end;
    C := CompileWfcPipeline(R,F.Extents);
    try
      ReplaceGroup(C.Graph.PassGraph[2],'A');
      ForceAll(C.Graph.PassGraph[1],'A'); ForceToken(C.Graph.PassGraph[1],5,'B');
      ForceAll(C.Graph.PassGraph[2],'A'); C.Graph.Seed := 9;
      Options := DefaultGraphSolveOptions; Options.MaxBacktracks := 32;
      Check(not C.Graph.TrySolve(Options,Report),'independent mapped commit rejects removed graph clause');
      Check((Report.Contradiction.Kind=gckFinalValidation) and
        (Report.FailedPassIndex=2) and (Report.Contradiction.EntryIndex=2),
        'far-edge consumer failure is local index two despite tiny root');
      Check((C.LastValidation.Kind=wpcvkRequirement) and (C.LastValidation.RequirementIndex=0),
        'immutable mapped descriptor owns rejection');
      Check(C.Graph.PassGraph[2].Entry[2,0,0].Empty,'mapped rejection rolls back late consumer');
      Check(C.Graph.PassGraph[1].Entry[5,0,0].Empty,'mapped rejection rolls back provider');
      Check(not C.Graph.TrySolve(Options,Report),'same seeded invalid replay still rejected');
    finally C.Free; end;
  finally R.Free; end;
end;

procedure TestLegacyDefinitionAndPreflight;
var F: TFixture; R: TWfcPipelineModel; C: TWfcCompiledPipeline;
  Options: TGraphSolveOptions; Report: TGraphSolveReport; Rejected: Boolean;
  Terms: TWfcPipelineRequirementTerms;
begin
  F := Fixture; F.Passes[1].Mode := gpmLegacy;
  SetLength(F.Dependencies,1); F.Dependencies[0] := MakeWfcPipelineDependency(1,0);
  R := Recipe(F);
  try
    C := CompileWfcPipeline(R,F.Extents);
    try
      Check(C.Graph.PassGraph[1].PassMode=gpmLegacy,'unlike legacy definition installs before real mode');
      Options := DefaultGraphSolveOptions;
      Check(C.Graph.TrySolve(Options,Report),'unlike legacy defined pass solves');
    finally C.Free; end;
  finally R.Free; end;
  F := Fixture;
  F.Passes[2] := MakeWfcPipelinePass('alias',wppvPublic,gpmTransform,1,wpakEmpty,-1,False,wseWhole);
  SetLength(F.Dependencies,1); F.Dependencies[0] := MakeWfcPipelineDependency(2,1);
  R := Recipe(F);
  try
    C := nil; Rejected := False;
    try C := CompileWfcPipeline(R,F.Extents);
    except on E: EWfcPipelineCompile do Rejected := E.Stage=wpcsPreflight; end;
    C.Free; Check(Rejected,'unlike transform extents rejected before graph allocation');
  finally R.Free; end;
  F := Fixture;
  SetLength(F.Dependencies,1); F.Dependencies[0] := MakeWfcPipelineDependency(2,1);
  SetLength(Terms,1); Terms[0] := MakeWfcPipelineRequirementTerm(0,0,0,Tokens(['A']));
  SetLength(F.Requirements,1); F.Requirements[0] := MakeWfcPipelineRequirement(2,'A',1,wprqExact,Terms);
  R := Recipe(F);
  try
    C := nil; Rejected := False;
    try C := CompileWfcPipeline(R,F.Extents);
    except on E: EWfcPipelineCompile do Rejected := E.Stage=wpcsPreflight; end;
    C.Free; Check(Rejected,'legacy index requirement rejects unlike extents during preflight');
  finally R.Free; end;
end;

procedure TestQuotaConnectivityAndLayoutCommit;
var F: TFixture; R: TWfcPipelineModel; C: TWfcCompiledPipeline;
  Options: TGraphSolveOptions; Report: TGraphSolveReport; Profiles: TWfcPipelineConnectivityValues;
  Root: TGraphPosition; Required: TGraphPositions; L: TWfcLatticeLayout;
  Failed: Integer; Layouts: TWfcLatticeLayouts;
begin
  Options := DefaultGraphSolveOptions; Options.MaxBacktracks := 16;
  F := Fixture; SetLength(F.Quotas,1);
  F.Quotas[0] := MakeWfcPipelineValueQuota(1,'late count',Tokens(['A']),0,4);
  R := Recipe(F);
  try
    C := CompileWfcPipeline(R,F.Extents);
    try
      C.Graph.PassGraph[1].ClearValueQuotas; ForceAll(C.Graph.PassGraph[1],'A');
      Check(not C.Graph.TrySolve(Options,Report),'quota recount includes all five cells after mutable quota removal');
      Check((C.LastValidation.Kind=wpcvkValueQuota) and (Report.FailedPassIndex=1),
        'quota rejection belongs to larger public owner');
    finally C.Free; end;
  finally R.Free; end;
  F := Fixture; F.Topologies[1].Wrap := True;
  Root := Default(TGraphPosition); SetLength(Required,1); Required[0] := Root; Required[0].X := 4;
  SetLength(Profiles,1); Profiles[0] := MakeWfcPipelineConnectivityValue('A',[gdEast,gdWest]);
  SetLength(F.Connectivities,1); F.Connectivities[0] := MakeWfcPipelineConnectivity(1,
    'late seam',Root,Required,Profiles,True);
  R := Recipe(F);
  try
    C := CompileWfcPipeline(R,F.Extents);
    try
      L := C.Graph.PassGraph[1].PassLayout;
      Check(ValidateWfcPipelineConnectivity(R,0,L,Tokens(['A','B','B','B','A']),Failed),
        'connectivity uses owner wrap rather than open root');
      ForceAll(C.Graph.PassGraph[1],'B'); ForceToken(C.Graph.PassGraph[1],0,'A');
      ForceToken(C.Graph.PassGraph[1],4,'A');
      Check(C.Graph.TrySolve(Options,Report),'compiled owner-specific seam connects');
    finally C.Free; end;
  finally R.Free; end;
  F.Topologies[1].Wrap := False; R := Recipe(F);
  try
    C := CompileWfcPipeline(R,F.Extents);
    try
      C.Graph.PassGraph[1].ClearConnectivity; ForceAll(C.Graph.PassGraph[1],'B');
      ForceToken(C.Graph.PassGraph[1],0,'A'); ForceToken(C.Graph.PassGraph[1],4,'A');
      Check(not C.Graph.TrySolve(Options,Report),'independent connectivity catches unreachable late cell');
      Check((C.LastValidation.Kind=wpcvkConnectivity) and (Report.Contradiction.EntryIndex=4),
        'connectivity failure index follows larger owner');
    finally C.Free; end;
  finally R.Free; end;
  F := Fixture; R := Recipe(F);
  try
    C := CompileWfcPipeline(R,F.Extents);
    try
      SetLength(Layouts,3); Layouts[0] := C.Graph.PassGraph[0].PassLayout;
      Layouts[1] := C.Graph.PassGraph[1].PassLayout; Layouts[2] := C.Graph.PassGraph[2].PassLayout;
      Inc(Layouts[1].Origin.X); C.Graph.ConfigurePassLayouts(Layouts);
      Check(not C.Graph.TrySolve(Options,Report),'borrowed layout mutation rejected inside commit');
      Check((C.LastValidation.Kind=wpcvkLayout) and (Report.FailedPassIndex=1),
        'layout mutation has explicit local owner diagnostics');
    finally C.Free; end;
  finally R.Free; end;
end;

procedure TestInversePairs;
var F: TFixture; R: TWfcPipelineModel; Run: TWfcPipelineRun; Output: TWfcPipelineResult;
  Model2: TWfcOverlappingModel2D; Model3: TWfcOverlappingModel3D; Seq: TWfcSequenceModel;
  Locks: TWfcPipelineCellLocks; Domains: TWfcPipelineCellDomains; Kind,Mode,I: Integer;
  DocumentText: String; ResourceKind: TWfcPipelineResourceKind;
  Adapter: TWfcPipelineAdapterKind; BridgeKind: TWfcPipelineBridgeKind;
begin
  for Kind := 0 to 2 do
  begin
    F := Fixture; SetLength(F.Resources,2);
    if Kind=0 then
    begin
      Seq := LearnSequenceModel(Tokens(['A','B','A']),2);
      try DocumentText := EncodeWfcSequenceText(Seq); finally Seq.Free; end;
      ResourceKind := wprkSequence; Adapter := wpakSequence; BridgeKind := wpbkSequenceProjection;
      F.Extents[1] := MakeWfcLatticeVector(3,1,1);
      F.Topologies[1] := MakeWfcPipelinePassTopology(1,MakeWfcLatticeVector(-9,7,4),
        MakeWfcLatticeVector(3,5,7),False);
    end
    else if Kind=1 then
    begin
      Model2 := LearnOverlappingModel2D(Tokens(['A','B','B','A']),2,2,2,2,wmbWrap,wmsNone);
      try DocumentText := EncodeWfcPattern2DText(Model2); finally Model2.Free; end;
      ResourceKind := wprkPattern2D; Adapter := wpakPattern2D; BridgeKind := wpbkPattern2DProjection;
      F.Extents[1] := MakeWfcLatticeVector(4,2,1);
      F.Topologies[1] := MakeWfcPipelinePassTopology(2,MakeWfcLatticeVector(-9,7,4),
        MakeWfcLatticeVector(3,5,7),True);
    end
    else
    begin
      Model3 := LearnOverlappingModel3D(Tokens(['A','B','B','A','B','A','A','B']),
        2,2,2,2,2,2,wmbWrap,wmsNone);
      try DocumentText := EncodeWfcPattern3DText(Model3); finally Model3.Free; end;
      ResourceKind := wprkPattern3D; Adapter := wpakPattern3D; BridgeKind := wpbkPattern3DProjection;
      F.Extents[1] := MakeWfcLatticeVector(4,2,2);
      F.Topologies[1] := MakeWfcPipelinePassTopology(3,MakeWfcLatticeVector(-9,7,4),
        MakeWfcLatticeVector(3,5,7),True);
    end;
    F.Topologies[2] := F.Topologies[1]; F.Extents[2] := F.Extents[1];
    F.Resources[1] := MakeWfcPipelineResource('latent',ResourceKind,DocumentText,'project-authored','MIT','');
    F.Passes[1] := MakeWfcPipelinePass('latent',wppvPrivate,gpmOverlay,-1,Adapter,1,Kind=0,wseWhole);
    F.Passes[2] := MakeWfcPipelinePass('public',wppvPublic,gpmOverlay,-1,wpakEmpty,-1,False,wseWhole);
    SetLength(F.Dependencies,1); F.Dependencies[0] := MakeWfcPipelineDependency(2,1);
    SetLength(F.Bridges,1); F.Bridges[0] := MakeWfcPipelineBridge(BridgeKind,1,2);
    R := Recipe(F);
    try
      SetLength(Locks,1); SetLength(Domains,1);
      Locks[0] := MakeWfcPipelineCellLock(2,F.Extents[2].X-1,F.Extents[2].Y-1,F.Extents[2].Z-1,'A');
      Domains[0] := MakeWfcPipelineCellDomain(2,0,0,0,Tokens(['A','B']));
      for Mode := 0 to 1 do
      begin
        Run := TWfcPipelineRun.Create(R,F.Extents,4,TWfcPipelineSolveStrategy(Mode),64,16*Mode,False,Locks,Domains);
        try
          Output := ExecuteWfcPipeline(R,Run);
          try
            Check(Output.Status=wprsSolved,'matching latent pair behind unrelated root solves with inverse input');
            Check(Output.LayerCount=2,'private states omitted while root and public pair remain');
            Check(Length(Output.LayerAt(1).Tokens)=Run.PassCellCount(2),'complete pair-local projection captured');
            Check(Output.LayerAt(1).Tokens[Run.PassCellCount(2)-1]='A','far-edge inverse lock holds');
            if Kind=0 then
              Check((Output.LayerAt(1).Tokens[0]='A') and (Output.LayerAt(1).Tokens[1]='B'),
                'sequence whole extent preserves public sample')
            else
              for I := 1 to F.Extents[2].X-1 do
                Check(Output.LayerAt(1).Tokens[I]<>Output.LayerAt(1).Tokens[I-1],
                  'literal checker adjacency preserved independently of private states');
          finally Output.Free; end;
        finally Run.Free; end;
      end;
    finally R.Free; end;
  end;
end;

procedure TestLateTransformAndLegacyRequirement;
var F: TFixture; R: TWfcPipelineModel; C: TWfcCompiledPipeline;
  Options: TGraphSolveOptions; Report: TGraphSolveReport;
  Terms: TWfcPipelineRequirementTerms; Wrapped: Integer;
begin
  Options := DefaultGraphSolveOptions; Options.MaxBacktracks := 16;
  F := Fixture; F.Extents[2] := F.Extents[1];
  F.Passes[2] := MakeWfcPipelinePass('copy',wppvPublic,gpmTransform,1,wpakEmpty,-1,False,wseWhole);
  SetLength(F.Dependencies,1); F.Dependencies[0] := MakeWfcPipelineDependency(2,1);
  R := Recipe(F);
  try
    C := CompileWfcPipeline(R,F.Extents);
    try
      ForceAll(C.Graph.PassGraph[1],'A');
      C.Graph.PassGraph[2].Entry[4,0,0].Value := 'B';
      Check(not C.Graph.TrySolve(Options,Report),'late conflicting transform lock rejected');
      Check((C.LastValidation.Kind=wpcvkTransform) and (Report.Contradiction.EntryIndex=4),
        'transform comparison reaches beyond root extent');
      Check(C.Graph.PassGraph[2].Entry[4,0,0].Value='B','transform rejection preserves preexisting lock');
      Check(C.Graph.PassGraph[1].Entry[4,0,0].Empty,'transform rejection restores generated late source');
    finally C.Free; end;
  finally R.Free; end;
  for Wrapped := 0 to 1 do
  begin
    F := Fixture; F.Extents[2] := F.Extents[1];
    F.Topologies[1].Wrap := Wrapped=1; F.Topologies[2] := F.Topologies[1];
    SetLength(F.Dependencies,1); F.Dependencies[0] := MakeWfcPipelineDependency(2,1);
    SetLength(Terms,2);
    Terms[0] := MakeWfcPipelineRequirementTerm(1,0,0,Tokens(['A']));
    Terms[1] := MakeWfcPipelineRequirementTerm(6,0,0,Tokens(['A']));
    SetLength(F.Requirements,1);
    F.Requirements[0] := MakeWfcPipelineCountRequirement(2,'A',1,Terms,1,1,gpcmDistinctCells);
    R := Recipe(F);
    try
      C := CompileWfcPipeline(R,F.Extents);
      try
        ReplaceGroup(C.Graph.PassGraph[2],'A'); ForceAll(C.Graph.PassGraph[1],'A');
        ForceAll(C.Graph.PassGraph[2],'A');
        if Wrapped=1 then
          Check(C.Graph.TrySolve(Options,Report),'late legacy count uses owner wrap and unique alias cells')
        else
        begin
          Check(not C.Graph.TrySolve(Options,Report),'open late legacy count fails at final cell');
          Check((C.LastValidation.Kind=wpcvkRequirement) and (Report.Contradiction.EntryIndex=4),
            'legacy requirement loops complete owner shape');
        end;
      finally C.Free; end;
    finally R.Free; end;
  end;
end;

procedure TestConnectivityTopologyBinding;
var F: TFixture; R: TWfcPipelineModel; Table: TWfcPipelineLayoutTable;
  Topologies: TWfcPipelinePassTopologies; L: TWfcLatticeLayout;
  Profiles: TWfcPipelineConnectivityValues; Root: TGraphPosition;
  Required: TGraphPositions; Failed, Field: Integer; Rejected: Boolean;

  procedure AlterTopology(var T: TWfcPipelinePassTopology; const Index: Integer);
  begin
    case Index of
      0: Inc(T.Origin.X); 1: Inc(T.Origin.Y); 2: Inc(T.Origin.Z);
      3: Inc(T.Pitch.X); 4: Inc(T.Pitch.Y); 5: Inc(T.Pitch.Z);
      6: T.Wrap := not T.Wrap;
    end;
  end;

  procedure RequireTableRejected(const Detail: String);
  begin
    Table := TWfcPipelineLayoutTable.Create(Topologies,F.Extents);
    try
      Rejected := False; Failed := 123;
      try PreflightWfcPipelineConnectivity(R,Table,Failed);
      except on E: EWfcPipelineConnectivity do Rejected := True; end;
      Check(Rejected and (Failed=-1),Detail+' rejects as invocation-wide topology error');
    finally Table.Free; end;
  end;

begin
  F := Fixture;
  Root := Default(TGraphPosition); SetLength(Required,1); Required[0] := Root; Required[0].X := 4;
  SetLength(Profiles,1); Profiles[0] := MakeWfcPipelineConnectivityValue('A',[gdEast,gdWest]);
  SetLength(F.Connectivities,1);
  F.Connectivities[0] := MakeWfcPipelineConnectivity(1,'bounded endpoints',Root,Required,Profiles,True);
  R := Recipe(F);
  try
    Table := ResolveWfcPipelineLayoutTable(R,F.Extents);
    try
      L := Table.PassLayoutAt(1);
      Check(not ValidateWfcPipelineConnectivity(R,0,L,Tokens(['A','B','B','B','A']),Failed)
        and (Failed=4),'bound layout cannot connect endpoints through the boundary');
    finally Table.Free; end;
    for Field := 0 to 6 do
    begin
      Topologies := R.CopyPassTopologies; AlterTopology(Topologies[1],Field);
      L := MakeWfcLatticeLayout(5,1,1,Topologies[1].Origin,Topologies[1].Pitch,Topologies[1].Wrap);
      Rejected := False; Failed := 123;
      try ValidateWfcPipelineConnectivity(R,0,L,Tokens(['A','B','B','B','A']),Failed);
      except on E: EWfcPipelineConnectivity do Rejected := True; end;
      Check(Rejected and (Failed=-1),'direct owner topology field '+IntToStr(Field)+' is recipe-bound');
      RequireTableRejected('table owner topology field '+IntToStr(Field));
    end;
    Topologies := R.CopyPassTopologies; Topologies[1].Rank := 2;
    RequireTableRejected('degenerate owner rank mismatch');
    Topologies := R.CopyPassTopologies; Inc(Topologies[2].Origin.Z);
    RequireTableRejected('unrelated pass topology mismatch');
    Topologies := R.CopyPassTopologies; Topologies[0].Rank := 2;
    RequireTableRejected('unrelated degenerate rank mismatch');

    F.Extents[0] := MakeWfcLatticeVector(4,1,1);
    F.Extents[1] := MakeWfcLatticeVector(6,1,1);
    F.Extents[2] := MakeWfcLatticeVector(2,1,1);
    Table := TWfcPipelineLayoutTable.Create(R.CopyPassTopologies,F.Extents);
    try
      PreflightWfcPipelineConnectivity(R,Table,Failed);
      Check(Failed=-1,'bound topologies permit independent changed extents on every pass');
      Check(ValidateWfcPipelineConnectivity(R,0,Table.PassLayoutAt(1),Tokens(['A','A','A','A','A','A']),Failed),
        'direct bound owner permits a different valid cell extent');
    finally Table.Free; end;
  finally R.Free; end;

  F := Fixture;
  R := TWfcPipelineModel.Create(MakeWfcPipelineMetadata('legacy empty connectivity','MIT','',''),
    CurrentWfcPipelineVersions,1,False,rmBottomUp,F.Resources,F.Passes,nil,nil,nil,nil,nil);
  try
    PreflightWfcPipelineConnectivity(R,0,0,0,Failed);
    Check(Failed=-1,'legacy numeric no-connectivity shortcut still skips shape work');
  finally R.Free; end;
end;

procedure TestAggregateBudgets;
var F: TFixture; R: TWfcPipelineModel; Run: TWfcPipelineRun;
  Runtime: TWfcPipelineRuntime; I, CaseIndex: Integer; Rejected: Boolean;
  Rules: TWfcRuleModel; Weights: TWfcModelIntegerArray;
begin
  for CaseIndex := 0 to 2 do
  begin
    F := Fixture;
    if CaseIndex=0 then
    begin
      { A tiny root must not hide a sum beyond the public output envelope. }
      F.Extents[1] := MakeWfcLatticeVector(WFC_PIPELINE_RUN_MAX_CELL_COUNT,1,1);
      F.Extents[2] := MakeWfcLatticeVector(1,1,1);
    end
    else if CaseIndex=1 then
    begin
      SetLength(F.Passes,6); SetLength(F.Extents,6); SetLength(F.Topologies,6);
      for I := 1 to 5 do
      begin
        F.Passes[I] := MakeWfcPipelinePass(TWfcModelToken('private-'+IntToStr(I)),
          wppvPrivate,gpmOverlay,-1,wpakRules,0,False,wseWhole);
        F.Topologies[I] := F.Topologies[0];
        F.Extents[I] := MakeWfcLatticeVector(WFC_PIPELINE_RUN_MAX_CELL_COUNT,1,1);
      end;
    end
    else
    begin
      SetLength(Weights,1); Weights[0] := 1;
      Rules := TWfcRuleModel.Create(1,Tokens([TWfcModelToken(StringOfChar('x',100))]),Weights,nil);
      try F.Resources[0] := MakeWfcPipelineResource('long',wprkRules,
        EncodeWfcRuleText(Rules),'project-authored','MIT','');
      finally Rules.Free; end;
      F.Extents[1] := MakeWfcLatticeVector(1000000,1,1);
    end;
    R := Recipe(F);
    try
      Run := TWfcPipelineRun.Create(R,F.Extents,0,wpssOneWay,0,0,False,nil,nil);
      try
        Runtime := nil; Rejected := False;
        try Runtime := TWfcPipelineRuntime.Create(R,Run);
        except on E: EWfcPipelineRuntime do
          case CaseIndex of
            0: Rejected := Pos('result cell limit',E.Message)>0;
            1: Rejected := Pos('runtime pass-cell limit',E.Message)>0;
            2: Rejected := Pos('encoded-token budget',E.Message)>0;
          end;
        end;
        Runtime.Free;
        Check(Rejected,'actual per-pass aggregate budget rejects before graph allocation');
      finally Run.Free; end;
    finally R.Free; end;
  end;
end;

begin
  TestPrefixKeysAndAliases;
  TestMappedCommit;
  TestLegacyDefinitionAndPreflight;
  TestQuotaConnectivityAndLayoutCommit;
  TestInversePairs;
  TestLateTransformAndLegacyRequirement;
  TestConnectivityTopologyBinding;
  TestAggregateBudgets;
  WriteLn('Mapped pipeline runtime checks: ',Checks);
end.
