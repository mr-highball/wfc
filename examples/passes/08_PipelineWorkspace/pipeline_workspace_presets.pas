{ SPDX-License-Identifier: MIT
  Copyright (c) 2021 mr-highball
  Project-authored graph-free definitions for a generic pipeline workbench. }
unit pipeline_workspace_presets;
{$mode delphi}{$H+}
interface

uses SysUtils, wfc, wfc_model, wfc_lattice, wfc_sequence,
  wfc_sequence_learn, wfc_pipeline_layout, wfc_pipeline_model,
  wfc_pipeline_run, wfc_pipeline_workspace_context;

type
  EWfcWorkspacePreset = class(Exception);
  TWfcWorkspacePresetRunOptions = record
    Seed: TGraphSeed;
    Strategy: TWfcPipelineSolveStrategy;
    MaxBacktracks, MaxPassBacktracks: Integer;
    CaptureTrace: Boolean;
  end;
  TWfcWorkspaceLandscapeWeights = record
    Land, Water, Clear, Tree, House, Vacant: Integer;
  end;
  TWfcWorkspaceSequenceOptions = record
    Order: Integer;
    Boundary: TWfcModelBoundary;
    Extent: TWfcSequenceExtent;
  end;

{ Each result owns one complete canonical recipe and one run, bound at index0.
  Construction primes models but never allocates or solves a graph. The caller
  owns the result. Borrowed contexts must not outlive it; copies are independent. }
function BuildMappedLandscapePreset(const Topologies: TWfcPipelinePassTopologies;
  const Extents: TWfcPipelinePassExtents;
  const RunOptions: TWfcWorkspacePresetRunOptions;
  const Weights: TWfcWorkspaceLandscapeWeights;
  const Locks: TWfcPipelineCellLocks; const Domains: TWfcPipelineCellDomains;
  const ContextLimits: TWfcPipelineWorkspaceContextLimits): TWfcPipelineWorkspaceContexts;
function BuildLearnedSequencePreset(const Topologies: TWfcPipelinePassTopologies;
  const Extents: TWfcPipelinePassExtents;
  const RunOptions: TWfcWorkspacePresetRunOptions;
  const SequenceOptions: TWfcWorkspaceSequenceOptions;
  const Locks: TWfcPipelineCellLocks; const Domains: TWfcPipelineCellDomains;
  const ContextLimits: TWfcPipelineWorkspaceContextLimits): TWfcPipelineWorkspaceContexts;

{ Illustrations, not implicit limits or feasibility guarantees. Builders do not
  call these helpers, change caller geometry, search seeds, or add input rows. }
function DefaultWorkspacePresetRunOptions: TWfcWorkspacePresetRunOptions;
function DefaultWorkspaceLandscapeWeights: TWfcWorkspaceLandscapeWeights;
function DefaultWorkspaceSequenceOptions: TWfcWorkspaceSequenceOptions;
procedure DefaultMappedLandscapeGeometry(out Topologies: TWfcPipelinePassTopologies;
  out Extents: TWfcPipelinePassExtents);
procedure DefaultLearnedSequenceGeometry(out Topologies: TWfcPipelinePassTopologies;
  out Extents: TWfcPipelinePassExtents);
function WorkspaceInlineSequenceCorpus: TWfcSequenceSamples;

implementation
uses wfc_rule_model, wfc_rule_text, wfc_sequence_text,
  wfc_pipeline_text, wfc_pipeline_run_text;

procedure Fail(const Detail: String);
begin raise EWfcWorkspacePreset.Create('workspace preset: '+Detail); end;

procedure GuardCommon(const Options: TWfcWorkspacePresetRunOptions;
  const Limits: TWfcPipelineWorkspaceContextLimits);
{$IFDEF PAS2JS}var Valid: Boolean;{$ENDIF}
begin
  {$IFDEF PAS2JS}
  asm
    function field(o,k) {
      if(o===null || typeof o!=='object' || Array.isArray(o)) return undefined;
      let d;
      while(o!==null && !(d=Object.getOwnPropertyDescriptor(o,k))) o=Object.getPrototypeOf(o);
      return d && ('value' in d) ? d : undefined;
    }
    function integer(o,k,lo,hi) {
      const d=field(o,k);
      return !!d && Number.isInteger(d.value) && d.value>=lo && d.value<=hi;
    }
    const trace=field(Options,'CaptureTrace');
    Valid=integer(Options,'Seed',0,4294967295) && integer(Options,'Strategy',0,1) &&
      integer(Options,'MaxBacktracks',0,1000000) && integer(Options,'MaxPassBacktracks',0,65536) &&
      !!trace && typeof trace.value==='boolean' && integer(Limits,'Version',1,1) &&
      integer(Limits,'MaxRecipes',1,2147483647) && integer(Limits,'MaxRuns',1,2147483647) &&
      integer(Limits,'MaxTextBytes',1,2147483647);
  end;
  if not Valid then Fail('passive typed run options and positive version1 context limits required');
  {$ENDIF}
  if (Ord(Options.Strategy)<Ord(Low(TWfcPipelineSolveStrategy))) or
    (Ord(Options.Strategy)>Ord(High(TWfcPipelineSolveStrategy))) or
    (Options.MaxBacktracks<0) or (Options.MaxBacktracks>WFC_PIPELINE_RUN_MAX_BACKTRACKS) or
    (Options.MaxPassBacktracks<0) or (Options.MaxPassBacktracks>WFC_PIPELINE_RUN_MAX_PASS_BACKTRACKS) then
    Fail('run strategy or search budget is out of range');
  if (Limits.Version<>1) or (Limits.MaxRecipes<1) or (Limits.MaxRuns<1) or
    (Limits.MaxTextBytes<1) then Fail('positive version1 context limits required');
end;

procedure GuardWeights(const Weights: TWfcWorkspaceLandscapeWeights);
{$IFDEF PAS2JS}var Valid: Boolean;{$ENDIF}
begin
  {$IFDEF PAS2JS}
  asm
    function positive(o,k) {
      if(o===null || typeof o!=='object' || Array.isArray(o)) return false;
      let d;
      while(o!==null && !(d=Object.getOwnPropertyDescriptor(o,k))) o=Object.getPrototypeOf(o);
      return !!d && ('value' in d) && Number.isInteger(d.value) && d.value>0 && d.value<=2147483647;
    }
    Valid=['Land','Water','Clear','Tree','House','Vacant'].every(k=>positive(Weights,k));
  end;
  if not Valid then Fail('six passive positive Integer weights required');
  {$ENDIF}
  if (Weights.Land<1) or (Weights.Water<1) or (Weights.Clear<1) or
    (Weights.Tree<1) or (Weights.House<1) or (Weights.Vacant<1) then
    Fail('six positive weights required');
end;

procedure GuardSequence(const Options: TWfcWorkspaceSequenceOptions);
{$IFDEF PAS2JS}var Valid: Boolean;{$ENDIF}
begin
  {$IFDEF PAS2JS}
  asm
    function integer(o,k,lo,hi) {
      if(o===null || typeof o!=='object' || Array.isArray(o)) return false;
      let d;
      while(o!==null && !(d=Object.getOwnPropertyDescriptor(o,k))) o=Object.getPrototypeOf(o);
      return !!d && ('value' in d) && Number.isInteger(d.value) && d.value>=lo && d.value<=hi;
    }
    Valid=integer(Options,'Order',1,1024) && integer(Options,'Boundary',0,1) &&
      integer(Options,'Extent',0,4);
  end;
  if not Valid then Fail('passive typed sequence options required');
  {$ENDIF}
  if (Options.Order<1) or (Options.Order>WFC_SEQUENCE_MAX_ORDER) or
    (Ord(Options.Boundary)<Ord(Low(TWfcModelBoundary))) or
    (Ord(Options.Boundary)>Ord(High(TWfcModelBoundary))) or
    (Ord(Options.Extent)<Ord(Low(TWfcSequenceExtent))) or
    (Ord(Options.Extent)>Ord(High(TWfcSequenceExtent))) then
    Fail('sequence order, source boundary or projection extent is out of range');
end;

function Geometry(const Topologies: TWfcPipelinePassTopologies;
  const Extents: TWfcPipelinePassExtents; const ExpectedCount: Integer): TWfcPipelineLayoutTable;
{$IFDEF PAS2JS}var Valid: Boolean;{$ENDIF}
begin
  {$IFDEF PAS2JS}
  asm
    Valid=Array.isArray(Topologies) && Topologies.length===ExpectedCount &&
      Array.isArray(Extents) && Extents.length===ExpectedCount;
  end;
  if not Valid then Fail('complete preset topology and extent arrays required');
  {$ENDIF}
  if (Length(Topologies)<>ExpectedCount) or (Length(Extents)<>ExpectedCount) then
    Fail('preset pass count does not match geometry');
  { The core table validates all passive nested geometry before taking copies.
    No caller array Copy/slice, record assignment or element read precedes it. }
  Result:=TWfcPipelineLayoutTable.Create(Topologies,Extents);
end;

function RuleResource(const Id, First, Second: String; const Rank, FirstWeight,
  SecondWeight: Integer; const Description: String): TWfcPipelineResource;
var Tokens: TWfcModelTokens; Weights: TWfcModelIntegerArray; Model: TWfcRuleModel;
begin
  SetLength(Tokens,2); Tokens[0]:=First; Tokens[1]:=Second;
  SetLength(Weights,2); Weights[0]:=FirstWeight; Weights[1]:=SecondWeight;
  Model:=TWfcRuleModel.Create(Rank,Tokens,Weights,nil);
  try
    Result:=MakeWfcPipelineResource(Id,wprkRules,EncodeWfcRuleText(Model),
      Description,'MIT',Id+'-v1');
  finally Model.Free; end;
end;

function Coverage(const Token: String): TWfcPipelineMappedQuery;
begin
  Result:=Default(TWfcPipelineMappedQuery);
  Result.Kind:=gpmkCellCoverage; Result.Match:=gpmmAll;
  SetLength(Result.AllowedProviderTokens,1); Result.AllowedProviderTokens[0]:=Token;
end;

function OwnContexts(const Recipe: TWfcPipelineModel; const Table: TWfcPipelineLayoutTable;
  const Options: TWfcWorkspacePresetRunOptions; const Locks: TWfcPipelineCellLocks;
  const Domains: TWfcPipelineCellDomains;
  const Limits: TWfcPipelineWorkspaceContextLimits): TWfcPipelineWorkspaceContexts;
var Run: TWfcPipelineRun; Recipes: TWfcPipelineWorkspaceRecipeTexts;
  Runs: TWfcPipelineWorkspaceRunTexts;
begin
  { The run guards caller-owned locks/domains before reading/copying them. }
  Run:=TWfcPipelineRun.Create(Recipe,Table.CopyExtents,Options.Seed,Options.Strategy,
    Options.MaxBacktracks,Options.MaxPassBacktracks,Options.CaptureTrace,Locks,Domains);
  try
    SetLength(Recipes,1); SetLength(Runs,1);
    Recipes[0]:=EncodeWfcPipelineModelText(Recipe);
    Runs[0].RecipeIndex:=0; Runs[0].Text:=EncodeWfcPipelineRunText(Run);
    Result:=TWfcPipelineWorkspaceContexts.Create(Recipes,Runs,Limits);
  finally Run.Free; end;
end;

function BuildMappedLandscapePreset(const Topologies: TWfcPipelinePassTopologies;
  const Extents: TWfcPipelinePassExtents; const RunOptions: TWfcWorkspacePresetRunOptions;
  const Weights: TWfcWorkspaceLandscapeWeights; const Locks: TWfcPipelineCellLocks;
  const Domains: TWfcPipelineCellDomains;
  const ContextLimits: TWfcPipelineWorkspaceContextLimits): TWfcPipelineWorkspaceContexts;
var Table: TWfcPipelineLayoutTable; Recipe: TWfcPipelineModel;
  Resources: TWfcPipelineResources; Passes: TWfcPipelinePasses;
  Dependencies: TWfcPipelineDependencies; Requirements: TWfcPipelineRequirements;
  T: TWfcPipelinePassTopology;
begin
  GuardCommon(RunOptions,ContextLimits); GuardWeights(Weights);
  Table:=Geometry(Topologies,Extents,3); Recipe:=nil;
  try
    SetLength(Resources,3);
    Resources[0]:=RuleResource('terrain-rules','land','water',Table.PassTopologyAt(0).Rank,
      Weights.Land,Weights.Water,'Project-authored land/water weights; no local adjacency exclusions.');
    Resources[1]:=RuleResource('foliage-rules','clear','tree',Table.PassTopologyAt(1).Rank,
      Weights.Clear,Weights.Tree,'Project-authored clear/tree weights; trees require full-cell land coverage.');
    Resources[2]:=RuleResource('housing-rules','house','vacant',Table.PassTopologyAt(2).Rank,
      Weights.House,Weights.Vacant,'Project-authored house/vacant weights; houses require full-cell land and clear coverage.');
    SetLength(Passes,3);
    Passes[0]:=MakeWfcPipelinePass('terrain',wppvPublic,gpmOverlay,-1,wpakRules,0,False,wseWhole);
    Passes[1]:=MakeWfcPipelinePass('foliage',wppvPublic,gpmOverlay,-1,wpakRules,1,False,wseWhole);
    Passes[2]:=MakeWfcPipelinePass('housing',wppvPublic,gpmOverlay,-1,wpakRules,2,False,wseWhole);
    SetLength(Dependencies,3);
    Dependencies[0]:=MakeWfcPipelineDependency(1,0);
    Dependencies[1]:=MakeWfcPipelineDependency(2,0);
    Dependencies[2]:=MakeWfcPipelineDependency(2,1);
    SetLength(Requirements,3);
    Requirements[0]:=MakeWfcPipelineMappedRequirement(1,'tree',0,Coverage('land'));
    Requirements[1]:=MakeWfcPipelineMappedRequirement(2,'house',0,Coverage('land'));
    Requirements[2]:=MakeWfcPipelineMappedRequirement(2,'house',1,Coverage('clear'));
    T:=Table.PassTopologyAt(0);
    Recipe:=TWfcPipelineModel.Create(MakeWfcPipelineMetadata('Mapped landscape',
      'MIT','Project-authored terrain, foliage and housing predicate preset.',
      'mapped-landscape-v1'),CurrentWfcPipelineVersions,T.Rank,T.Wrap,rmBottomUp,
      Resources,Passes,Dependencies,nil,Requirements,nil,nil,
      WFC_PIPELINE_PASS_MAPPING_VERSION,Table.CopyTopologies);
    Result:=OwnContexts(Recipe,Table,RunOptions,Locks,Domains,ContextLimits);
  finally Recipe.Free; Table.Free; end;
end;

function WorkspaceInlineSequenceCorpus: TWfcSequenceSamples;
var Tokens: TWfcModelTokens;
begin
  { New MIT sample corpus, not imported test-oracle data. Fresh rows every call. }
  SetLength(Result,3);
  SetLength(Tokens,5); Tokens[0]:='step'; Tokens[1]:='step'; Tokens[2]:='turn';
  Tokens[3]:='step'; Tokens[4]:='rest'; Result[0]:=MakeWfcSequenceSample(Tokens);
  SetLength(Tokens,4); Tokens[0]:='step'; Tokens[1]:='turn'; Tokens[2]:='step';
  Tokens[3]:='rest'; Result[1]:=MakeWfcSequenceSample(Tokens);
  SetLength(Tokens,5); Tokens[0]:='rest'; Tokens[1]:='step'; Tokens[2]:='step';
  Tokens[3]:='turn'; Tokens[4]:='rest'; Result[2]:=MakeWfcSequenceSample(Tokens);
end;

function BuildLearnedSequencePreset(const Topologies: TWfcPipelinePassTopologies;
  const Extents: TWfcPipelinePassExtents; const RunOptions: TWfcWorkspacePresetRunOptions;
  const SequenceOptions: TWfcWorkspaceSequenceOptions; const Locks: TWfcPipelineCellLocks;
  const Domains: TWfcPipelineCellDomains;
  const ContextLimits: TWfcPipelineWorkspaceContextLimits): TWfcPipelineWorkspaceContexts;
var Table: TWfcPipelineLayoutTable; Recipe: TWfcPipelineModel; Sequence: TWfcSequenceModel;
  Resources: TWfcPipelineResources; Passes: TWfcPipelinePasses;
  Dependencies: TWfcPipelineDependencies; Bridges: TWfcPipelineBridges;
  Versions: TWfcPipelineVersions; T: TWfcPipelinePassTopology; BoundaryName: String;
begin
  GuardCommon(RunOptions,ContextLimits); GuardSequence(SequenceOptions);
  Table:=Geometry(Topologies,Extents,4); Recipe:=nil; Sequence:=nil;
  try
    Sequence:=LearnSequenceModelCorpus(WorkspaceInlineSequenceCorpus,
      SequenceOptions.Order,SequenceOptions.Boundary);
    if SequenceOptions.Boundary=wmbOpen then BoundaryName:='open' else BoundaryName:='wrap';
    SetLength(Resources,2);
    Resources[0]:=RuleResource('independent-rules','open','closed',Table.PassTopologyAt(0).Rank,
      3,1,'Project-authored independent open/closed grid; no sequence dependency.');
    Resources[1]:=MakeWfcPipelineResource('inline-sequence',wprkSequence,
      EncodeWfcSequenceText(Sequence),
      'MIT inline corpus v1: step step turn step rest | step turn step rest | rest step step turn rest',
      'MIT','inline-step-turn-rest-v1/order='+IntToStr(SequenceOptions.Order)+'/boundary='+BoundaryName);
    SetLength(Passes,4);
    Passes[0]:=MakeWfcPipelinePass('independent',wppvPublic,gpmOverlay,-1,wpakRules,0,False,wseWhole);
    Passes[1]:=MakeWfcPipelinePass('sequence-states',wppvPrivate,gpmOverlay,-1,wpakSequence,1,
      True,SequenceOptions.Extent);
    Passes[2]:=MakeWfcPipelinePass('sequence',wppvPublic,gpmOverlay,-1,wpakEmpty,-1,False,wseWhole);
    Passes[3]:=MakeWfcPipelinePass('sequence-alias',wppvPublic,gpmTransform,2,wpakEmpty,-1,False,wseWhole);
    SetLength(Dependencies,2);
    Dependencies[0]:=MakeWfcPipelineDependency(2,1);
    Dependencies[1]:=MakeWfcPipelineDependency(3,2);
    SetLength(Bridges,1); Bridges[0]:=MakeWfcPipelineBridge(wpbkSequenceProjection,1,2);
    Versions:=CurrentWfcPipelineVersions; Versions.SequenceBridgeVersion:=2;
    T:=Table.PassTopologyAt(0);
    Recipe:=TWfcPipelineModel.Create(MakeWfcPipelineMetadata('Learned sequence workspace',
      'MIT','Real corpus priming, private states, public projection, exact public alias and independent grid.',
      'learned-sequence-workspace-v1'),Versions,T.Rank,T.Wrap,rmBottomUp,
      Resources,Passes,Dependencies,Bridges,nil,nil,nil,
      WFC_PIPELINE_PASS_MAPPING_VERSION,Table.CopyTopologies);
    Result:=OwnContexts(Recipe,Table,RunOptions,Locks,Domains,ContextLimits);
  finally Sequence.Free; Recipe.Free; Table.Free; end;
end;

function DefaultWorkspacePresetRunOptions: TWfcWorkspacePresetRunOptions;
begin
  Result.Seed:=7; Result.Strategy:=wpssNegotiated;
  Result.MaxBacktracks:=128; Result.MaxPassBacktracks:=16; Result.CaptureTrace:=False;
end;
function DefaultWorkspaceLandscapeWeights: TWfcWorkspaceLandscapeWeights;
begin
  Result.Land:=32; Result.Water:=1; Result.Clear:=32;
  Result.Tree:=1; Result.House:=8; Result.Vacant:=1;
end;
function DefaultWorkspaceSequenceOptions: TWfcWorkspaceSequenceOptions;
begin Result.Order:=2; Result.Boundary:=wmbOpen; Result.Extent:=wseWhole; end;
procedure DefaultMappedLandscapeGeometry(out Topologies: TWfcPipelinePassTopologies;
  out Extents: TWfcPipelinePassExtents);
begin
  SetLength(Topologies,3); SetLength(Extents,3);
  Topologies[0]:=MakeWfcPipelinePassTopology(2,MakeWfcLatticeVector(0,0,0),MakeWfcLatticeVector(4,4,1),False);
  Topologies[1]:=MakeWfcPipelinePassTopology(2,MakeWfcLatticeVector(0,0,0),MakeWfcLatticeVector(1,1,1),False);
  Topologies[2]:=MakeWfcPipelinePassTopology(2,MakeWfcLatticeVector(4,4,0),MakeWfcLatticeVector(8,8,1),False);
  Extents[0]:=MakeWfcLatticeVector(8,6,1); Extents[1]:=MakeWfcLatticeVector(32,24,1);
  Extents[2]:=MakeWfcLatticeVector(3,2,1);
end;
procedure DefaultLearnedSequenceGeometry(out Topologies: TWfcPipelinePassTopologies;
  out Extents: TWfcPipelinePassExtents);
var I: Integer;
begin
  SetLength(Topologies,4); SetLength(Extents,4);
  Topologies[0]:=MakeWfcPipelinePassTopology(2,MakeWfcLatticeVector(-6,4,0),MakeWfcLatticeVector(2,3,1),False);
  Extents[0]:=MakeWfcLatticeVector(5,3,1);
  for I:=1 to 3 do
  begin
    Topologies[I]:=MakeWfcPipelinePassTopology(1,MakeWfcLatticeVector(11,-3,5),MakeWfcLatticeVector(3,2,1),False);
    Extents[I]:=MakeWfcLatticeVector(12,1,1);
  end;
end;
end.
