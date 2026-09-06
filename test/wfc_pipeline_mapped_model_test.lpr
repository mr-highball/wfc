{ SPDX-License-Identifier: MIT
  Copyright (c) 2021 mr-highball }
program wfc_pipeline_mapped_model_test;
{$mode delphi}{$H+}
uses {$IFDEF PAS2JS}wfc_browser_test_host,{$ENDIF} SysUtils,wfc,wfc_lattice,
  wfc_model,wfc_rule_model,wfc_rule_text,wfc_sequence,
  wfc_pipeline_layout,wfc_pipeline_model;
var Checks:Integer;
  Resources:TWfcPipelineResources; Passes:TWfcPipelinePasses;
  Edges:TWfcPipelineDependencies; Requirements:TWfcPipelineRequirements;
  Topologies:TWfcPipelinePassTopologies; Query:TWfcPipelineMappedQuery;

procedure Check(const OK:Boolean; const Name:String);
begin Inc(Checks); if not OK then raise Exception.Create(Name); end;
function Tokens(const A:array of TWfcModelToken):TWfcModelTokens;
var I:Integer;
begin Result:=nil; SetLength(Result,Length(A)); for I:=0 to High(A) do Result[I]:=A[I]; end;
function RuleDocument(const Rank:Integer):String;
var M:TWfcRuleModel; Weights:TWfcModelIntegerArray;
begin
  SetLength(Weights,2); Weights[0]:=1; Weights[1]:=2;
  M:=TWfcRuleModel.Create(Rank,Tokens(['clear','tree']),Weights,nil);
  try Result:=EncodeWfcRuleText(M); finally M.Free; end;
end;
procedure ResetInputs;
var I:Integer;
begin
  {$IFDEF PAS2JS}
  { Hostile interop cases deliberately destroy nested record shape. Replace
    the fixture object, rather than assigning through its damaged children. }
  asm pas.program.Query=pas.wfc_pipeline_model.TWfcPipelineMappedQuery.$new(); end;
  {$ENDIF}
  Resources:=nil; Passes:=nil; Edges:=nil; Requirements:=nil; Topologies:=nil;
  SetLength(Resources,2); SetLength(Passes,2); SetLength(Edges,1); SetLength(Topologies,2);
  for I:=0 to 1 do begin
    Resources[I]:=MakeWfcPipelineResource(TWfcModelToken('r'+IntToStr(I)),wprkRules,
      RuleDocument(1+I*2),'authored','MIT','');
    Passes[I]:=MakeWfcPipelinePass(TWfcModelToken('p'+IntToStr(I)),wppvPublic,
      gpmOverlay,-1,wpakRules,I,False,wseWhole);
  end;
  Edges[0]:=MakeWfcPipelineDependency(1,0);
  Topologies[0]:=MakeWfcPipelinePassTopology(1,MakeWfcLatticeVector(-12,3,-5),MakeWfcLatticeVector(4,8,6),False);
  Topologies[1]:=MakeWfcPipelinePassTopology(3,MakeWfcLatticeVector(-7,4,-2),MakeWfcLatticeVector(1,2,3),True);
  Query:=Default(TWfcPipelineMappedQuery); Query.Kind:=gpmkCellCoverage; Query.Match:=gpmmAll;
  Query.AllowedProviderTokens:=Tokens(['clear']);
  SetLength(Requirements,1); Requirements[0]:=MakeWfcPipelineMappedRequirement(1,'tree',0,Query);
end;
function NewRecipe(const Spatial:Boolean=True; const Version:Integer=1):TWfcPipelineModel;
begin
  if Spatial then Result:=TWfcPipelineModel.Create(MakeWfcPipelineMetadata('mapped','MIT','',''),
    CurrentWfcPipelineVersions,1,False,rmBottomUp,Resources,Passes,Edges,nil,Requirements,nil,nil,Version,Topologies)
  else Result:=TWfcPipelineModel.Create(MakeWfcPipelineMetadata('mapped','MIT','',''),
    CurrentWfcPipelineVersions,1,False,rmBottomUp,Resources,Passes,Edges,nil,Requirements,nil,nil);
end;
function Rejected(const Spatial:Boolean=True; const Version:Integer=1):Boolean;
var M:TWfcPipelineModel;
begin
  Result:=False; M:=nil;
  try try M:=NewRecipe(Spatial,Version); except on E:Exception do Result:=True; end; finally M.Free; end;
end;
function QueryRejected(const Q:TWfcPipelineMappedQuery):Boolean;
var G:TGraphPassMapQuery;
begin
  Result:=False;
  try G:=WfcPipelineMappedQueryGeometry(Q); if Length(G.Values)<>1 then raise Exception.Create('bad placeholder');
  except on E:Exception do Result:=True; end;
end;
{$IFDEF PAS2JS}
function TypedRejected(const Fragment:String):Boolean;
var M:TWfcPipelineModel;
begin
  Result:=False; M:=nil;
  try try M:=NewRecipe;
    except on E:EWfcPipelineModel do Result:=(E.Message<>'') and (Pos(Fragment,E.Message)>0); end;
  finally M.Free; end;
end;
{$ENDIF}
procedure ModelCases;
var M,N:TWfcPipelineModel; T:TWfcPipelinePassTopology; R:TWfcPipelineRequirement;
  CopyTopologies:TWfcPipelinePassTopologies; Signature:TWfcPipelineSignature;
  I:Integer; G:TGraphPassMapQuery;
  Terms:TWfcPipelineRequirementTerms;
begin
  ResetInputs; M:=NewRecipe;
  try
    Check(M.HasPassMapping and (M.PassMappingVersion=1),'explicit capability');
    Check((M.Rank=1) and (M.PassTopologyAt(1).Rank=3),'per-pass resource ranks');
    T:=M.PassTopologyAt(0); Check((T.Pitch.Y=8) and (T.Origin.Z=-5),'inactive local axes retain world geometry');
    Signature:=M.Signature;
    Topologies[1].Origin.X:=55; Requirements[0].MappedQuery.AllowedProviderTokens[0]:='tree';
    Check((M.PassTopologyAt(1).Origin.X=-7) and (M.RequirementAt(0).MappedQuery.AllowedProviderTokens[0]='clear'),'constructor detaches topology and query');
    CopyTopologies:=M.CopyPassTopologies; CopyTopologies[0].Pitch.Y:=99;
    R:=M.RequirementAt(0); R.MappedQuery.AllowedProviderTokens[0]:='tree';
    Check((M.PassTopologyAt(0).Pitch.Y=8) and (M.RequirementAt(0).MappedQuery.AllowedProviderTokens[0]='clear'),'copy accessors detach nested records');
    ResetInputs; N:=NewRecipe; try Check(N.Signature=Signature,'same semantic signature'); finally N.Free; end;
    for I:=0 to 7 do begin
      ResetInputs;
      case I of
        0:Inc(Topologies[1].Origin.X); 1:Inc(Topologies[1].Origin.Y); 2:Inc(Topologies[1].Origin.Z);
        3:Inc(Topologies[1].Pitch.X); 4:Inc(Topologies[1].Pitch.Y); 5:Inc(Topologies[1].Pitch.Z);
        6:Topologies[1].Wrap:=False; 7:Requirements[0].MappedQuery.MinimumOffset.DeltaZ:=1;
      end;
      N:=NewRecipe; try Check(N.Signature<>Signature,'each topology/query field contributes '+IntToStr(I)); finally N.Free; end;
    end;
  finally M.Free; end;
  ResetInputs; Check(Rejected(False),'legacy constructor rejects mapped tag');
  Check(Rejected(True,0),'explicit capability zero rejected'); Check(Rejected(True,2),'future capability rejected');
  SetLength(Topologies,1); Check(Rejected,'missing topology rejected');
  ResetInputs; Topologies[0].Wrap:=True; Check(Rejected,'legacy pass-zero wrap mismatch rejected');
  ResetInputs; Topologies[0].Rank:=2; Check(Rejected,'legacy pass-zero rank mismatch rejected');
  ResetInputs; Topologies[1].Pitch.X:=0; Check(Rejected,'nonpositive pitch rejected');
  ResetInputs; Topologies[1].Origin.X:=High(Integer); Check(Rejected,'one-cell world endpoint overflow rejected');
  ResetInputs; Edges:=nil; Check(Rejected,'explicit mapped dependency required');
  ResetInputs; Passes[0].Visibility:=wppvPrivate; Check(Rejected,'provider must be public');
  ResetInputs; Passes[1].Visibility:=wppvPrivate; Check(Rejected,'consumer must be public');
  ResetInputs; Requirements[0].ConsumerToken:='missing'; Check(Rejected,'consumer vocabulary guarded');
  ResetInputs; Requirements[0].MappedQuery.AllowedProviderTokens:=Tokens(['missing']); Check(Rejected,'provider vocabulary guarded');
  ResetInputs; Requirements[0].MappedQuery.AllowedProviderTokens:=Tokens(['tree','clear']); Check(Rejected,'provider ordering guarded');
  ResetInputs; Requirements[0].MappedQuery.AllowedProviderTokens:=Tokens(['clear','clear']); Check(Rejected,'duplicate tokens rejected');
  ResetInputs; Requirements[0].MappedQuery.AllowedProviderTokens:=nil; Check(Rejected,'empty token set rejected');
  ResetInputs; Requirements[0].MappedQuery.Match:=gpmmCount; Requirements[0].MappedQuery.MinimumMatches:=High(Integer);
  Requirements[0].MappedQuery.MaximumMatches:=High(Integer); Check(not Rejected,'coverage bounds independent of legacy term count');
  ResetInputs; SetLength(Requirements[0].Terms,1); Requirements[0].Terms[0].OffsetY:=High(Integer);
  Requirements[0].MaximumCount:=-99; M:=NewRecipe;
  try R:=M.RequirementAt(0); Check((Length(R.Terms)=0) and (R.MaximumCount=0),'mapped tag never interprets legacy payload'); finally M.Free; end;
  ResetInputs; Requirements:=nil; Passes:=Copy(Passes,0,1); Resources:=Copy(Resources,0,1); Edges:=nil;
  M:=NewRecipe(False);
  try
    Check(not M.HasPassMapping and (M.PassMappingVersion=0),'legacy capability remains absent');
    T:=M.PassTopologyAt(0); Check((T.Origin.X=0) and (T.Pitch.Y=1) and (T.Rank=1),'legacy topology synthesized');
    Signature:=M.Signature; SetLength(Topologies,1); Topologies[0]:=LegacyWfcPipelinePassTopology(1,False);
    N:=NewRecipe; try Check(N.Signature<>Signature,'explicit uniform topology retains new identity'); finally N.Free; end;
  finally M.Free; end;
  ResetInputs;
  for I:=0 to 5 do begin
    Query.Kind:=TGraphPassMapKind(I div 2); Query.Match:=TGraphPassMapMatch(I mod 2);
    Query.MinimumOffset:=MakeGraphOffset(-2,3,-4); Query.MaximumOffset:=MakeGraphOffset(0,0,0);
    if Query.Kind=gpmkRegionCoverage then Query.MaximumOffset:=MakeGraphOffset(4,5,1);
    Query.MinimumMatches:=0; Query.MaximumMatches:=0; if Query.Match=gpmmCount then Query.MaximumMatches:=1;
    G:=WfcPipelineMappedQueryGeometry(Query);
    Check((G.Kind=Query.Kind) and (G.MinimumOffset.DeltaZ=-4) and (Length(G.Values)=1),'shared geometry variant '+IntToStr(I));
  end;
  ResetInputs; Query.MaximumOffset.DeltaZ:=1; Check(QueryRejected(Query),'dirty unused maximum offset');
  ResetInputs; Query.MaximumMatches:=1; Check(QueryRejected(Query),'dirty all-match count');
  ResetInputs; Query.Kind:=gpmkPoint; Query.Match:=gpmmCount; Query.MaximumMatches:=2; Check(QueryRejected(Query),'point cannot count two cells');
  ResetInputs; Query.Kind:=gpmkRegionCoverage; Check(QueryRejected(Query),'region must be positive on all axes');
  ResetInputs; Query.Match:=gpmmCount; Query.MinimumMatches:=2; Query.MaximumMatches:=1; Check(QueryRejected(Query),'reversed count bounds');
  ResetInputs; Query.MinimumOffset:=MakeGraphOffset(Low(Integer),High(Integer),Low(Integer));
  Check(not QueryRejected(Query),'signed declaration extrema retained for extent-aware preflight');
  ResetInputs; Resources[1].Document:=RuleDocument(1);
  SetLength(Terms,1); Terms[0]:=MakeWfcPipelineRequirementTerm(0,0,0,Tokens(['clear']));
  Requirements[0]:=MakeWfcPipelineRequirement(1,'tree',0,wprqExact,Terms);
  Requirements[0].MappedQuery.MaximumMatches:=-99;
  Requirements[0].MappedQuery.AllowedProviderTokens:=Tokens(['not a provider token']);
  {$IFDEF PAS2JS}
  asm Object.defineProperty(pas.program.Requirements[0],'MappedQuery',{get:function(){throw new Error('legacy payload was read');}}); end;
  {$ENDIF}
  M:=NewRecipe(False);
  try R:=M.RequirementAt(0); Check((R.Kind=wprqExact) and (Length(R.Terms)=1),'old tags never read appended mapped payload');
  finally M.Free; end;
  {$IFDEF PAS2JS}
  ResetInputs; asm pas.program.Topologies={length:2}; end;
  M:=nil;
  try
    try M:=NewRecipe; Check(False,'JS topology shape must fail');
    except on E:EWfcPipelineModel do Check(E.Message='mapped topology count must equal pass count','JS topology error retains exact typed message'); end;
  finally M.Free; end;
  ResetInputs; asm pas.program.Query.MinimumOffset.DeltaX=0.5; end; Check(QueryRejected(Query),'JS fractional query rejected');
  ResetInputs; asm pas.program.Query.MaximumMatches=NaN; end; Check(QueryRejected(Query),'JS NaN rejected');
  ResetInputs; asm pas.program.Query.Kind='0'; end; Check(QueryRejected(Query),'JS numeric string rejected');
  ResetInputs; asm pas.program.Query.MinimumOffset=null; end; Check(QueryRejected(Query),'JS null offset rejected before copy');
  ResetInputs; asm pas.program.Query.AllowedProviderTokens={length:1,0:'clear'}; end; Check(QueryRejected(Query),'JS fake token array rejected');
  ResetInputs; asm delete pas.program.Query.AllowedProviderTokens[0]; end; Check(QueryRejected(Query),'JS sparse tokens rejected');
  ResetInputs; asm Object.defineProperty(pas.program.Query,'Kind',{get:function(){throw new Error('getter invoked');}}); end;
  Check(QueryRejected(Query),'JS query accessor rejected without invoking');
  ResetInputs; asm pas.program.Topologies[1].Wrap=1; end; Check(Rejected,'JS non-Boolean topology wrap');
  ResetInputs; asm pas.program.Topologies[1].Origin.X='5'; end; Check(Rejected,'JS string topology coordinate');
  ResetInputs; asm delete pas.program.Topologies[1]; end; Check(Rejected,'JS sparse topology registry');
  ResetInputs; asm pas.program.Passes[0].HasSequenceExtent=0; end;
  Check(TypedRejected('must be Boolean'),'JS explicit spatial sequence flag strictly Boolean');
  ResetInputs; asm pas.program.Passes[0].Mode=NaN; end;
  Check(TypedRejected('mode is unknown'),'JS explicit spatial pass mode finite enum');
  ResetInputs; asm pas.program.Passes[0].Visibility='1'; end;
  Check(TypedRejected('visibility is unknown'),'JS explicit spatial visibility typed enum');
  ResetInputs; Requirements[0]:=MakeWfcPipelineCountRequirement(1,'tree',0,Terms,0,1,gpcmMatchingTerms);
  asm pas.program.Requirements[0].MinimumCount=NaN; end;
  Check(TypedRejected('requirement minimum'),'JS spatial legacy count rejects NaN minimum');
  ResetInputs; Requirements[0]:=MakeWfcPipelineCountRequirement(1,'tree',0,Terms,0,1,gpcmMatchingTerms);
  asm pas.program.Requirements[0].MaximumCount=NaN; end;
  Check(TypedRejected('requirement maximum'),'JS spatial legacy count rejects NaN maximum');
  ResetInputs; Requirements[0]:=MakeWfcPipelineRequirement(1,'tree',0,wprqExact,Terms);
  asm pas.program.Requirements[0].Terms[0].OffsetX=0.5; end;
  Check(TypedRejected('requirement offset X'),'JS spatial legacy clause rejects fractional offset');
  ResetInputs; Requirements[0]:=MakeWfcPipelineRequirement(1,'tree',0,wprqExact,Terms);
  asm pas.program.Requirements[0].Terms[0].OffsetZ='0'; end;
  Check(TypedRejected('requirement offset Z'),'JS spatial legacy clause rejects string offset');
  {$ENDIF}
end;
begin
  ModelCases; WriteLn('Mapped pipeline model: ',Checks,' checks passed');
end.
