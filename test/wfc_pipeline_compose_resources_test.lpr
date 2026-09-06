{ SPDX-License-Identifier: MIT
  Copyright (c) 2021 mr-highball
  Real learned fragment composition integration conformance. }
program wfc_pipeline_compose_resources_test;
{$mode delphi}{$H+}
uses
  SysUtils, {$IFDEF PAS2JS}wfc_browser_test_host,{$ENDIF}
  wfc, wfc_lattice, wfc_model, wfc_sequence, wfc_rule_model, wfc_rule_text,
  wfc_training, wfc_pipeline_layout, wfc_pipeline_model, wfc_pipeline_text,
  wfc_pipeline_compose, wfc_pipeline_run, wfc_pipeline_result,
  wfc_pipeline_result_text, wfc_pipeline_runtime;

const FragmentCount = 4;
type TRecipes = array[0..FragmentCount-1] of TWfcPipelineModel;
var Checks: Integer;

procedure Check(const Condition: Boolean; const MessageText: String);
begin
  Inc(Checks);
  if not Condition then raise Exception.Create(MessageText);
end;

function Tokens(const Values: array of TWfcModelToken): TWfcModelTokens;
var I: Integer;
begin
  Result:=nil; SetLength(Result,Length(Values));
  for I:=0 to High(Values) do Result[I]:=Values[I];
end;

function Position(const X,Y,Z: Integer): TGraphPosition;
begin Result.X:=X; Result.Y:=Y; Result.Z:=Z; end;

function FragmentToken(const Index: Integer): TWfcModelToken;
begin
  case Index of
    0: Result:='land';
    1: Result:='path';
    2: Result:='stone';
    3: Result:='note';
  else raise Exception.Create('bad fixture index'); end;
end;

function FixtureId(const Index: Integer): TWfcModelToken;
begin Result:=TWfcModelToken('fragment-'+IntToStr(Index)); end;

function LearnedFragment(const Index: Integer): TWfcPipelineModel;
var Samples: TWfcTrainingSamples; Options: TWfcTrainingOptions;
  Policy: TWfcTrainingValueQuotas; Connections: TWfcTrainingConnectivities;
  Profiles: TWfcTrainingConnectivityValues; Terminals: TGraphPositions;
  Document: TWfcTrainingDocument; Value,LicenseText: TWfcModelToken;
begin
  Value:=FragmentToken(Index); SetLength(Samples,1);
  case Index of
    0: begin
      Samples[0]:=MakeWfcTrainingSample('authored square',2,2,
        Tokens([Value,Value,Value,Value]));
      Options:=MakeWfcTrainingOptions(wtkPattern2D,wmbWrap,wmsNone,2,2,0);
      LicenseText:='MIT';
    end;
    1,3: begin
      Samples[0]:=MakeWfcTrainingSample('authored phrase',3,1,
        Tokens([Value,Value,Value]));
      Options:=MakeWfcTrainingOptions(wtkSequence,wmbOpen,wmsNone,0,0,2);
      if Index=3 then Options.Boundary:=wmbWrap;
      LicenseText:='CC0-1.0';
    end;
    2: begin
      Samples[0]:=MakeWfcTrainingSample('authored volume',2,2,2,
        Tokens([Value,Value,Value,Value,Value,Value,Value,Value]));
      Options:=MakeWfcTrainingOptions(wtkPattern3D,wmbWrap,wmsNone,2,2,2,0);
      LicenseText:='LicenseRef-project-fixture';
    end;
  else raise Exception.Create('bad learned fixture index'); end;
  SetLength(Policy,1);
  Policy[0]:=MakeWfcTrainingValueQuota('retained occupancy',Tokens([Value]),1,128);
  SetLength(Profiles,1);
  Profiles[0]:=MakeWfcTrainingConnectivityValue(Value,
    [gdNorth,gdEast,gdSouth,gdWest,gdUp,gdDown],True);
  SetLength(Terminals,1); Terminals[0]:=Position(1,0,0);
  SetLength(Connections,1);
  Connections[0]:=MakeWfcTrainingConnectivity('retained route',Position(0,0,0),
    Terminals,Profiles,True);
  Document:=TWfcTrainingDocument.Create(MakeWfcTrainingMetadata(
    'authored resource',LicenseText,TWfcModelToken('local corpus '+IntToStr(Index))),
    Options,Samples,Policy,Connections);
  try
    Result:=LearnWfcTrainingRecipe(Document);
    Check((Result.PassCount=2) and (Result.BridgeCount=1),
      'real learner exports private/public bridge pair');
    Check((Result.ValueQuotaAt(0).PassIndex=1) and
      (Result.ConnectivityAt(0).PassIndex=1),'learner attaches policies to public output');
    Check((Result.ResourceAt(0).SourceLicenseIdentifier=LicenseText) and
      (Pos('wfclearn-v',String(Result.ResourceAt(0).SourceFingerprint))=1),
      'real learner supplies declared license and source fingerprint');
  finally Document.Free; end;
end;

function DecorateFragment(const Learned: TWfcPipelineModel;
  const Index: Integer): TWfcPipelineModel;
var Resources: TWfcPipelineResources; Passes: TWfcPipelinePasses;
  Dependencies: TWfcPipelineDependencies; Requirements: TWfcPipelineRequirements;
  Quotas: TWfcPipelineValueQuotas; Connections: TWfcPipelineConnectivities;
  Topologies: TWfcPipelinePassTopologies; Query: TWfcPipelineMappedQuery;
  Rules: TWfcRuleModel; Weights: TWfcModelIntegerArray;
  Origin,Pitch: TWfcLatticeVector; I: Integer;
begin
  Resources:=Learned.CopyResources; SetLength(Resources,2);
  SetLength(Weights,1); Weights[0]:=1;
  Rules:=TWfcRuleModel.Create(1,Tokens(['marker']),Weights,nil);
  try Resources[1]:=MakeWfcPipelineResource('marker rules',wprkRules,
    EncodeWfcRuleText(Rules),'authored mapping consumer','MIT','rules-fixture-v1');
  finally Rules.Free; end;
  Passes:=Learned.CopyPasses; SetLength(Passes,4);
  Passes[2]:=MakeWfcPipelinePass('public alias',wppvPublic,gpmTransform,1,
    wpakEmpty,-1,False,wseWhole);
  Passes[3]:=MakeWfcPipelinePass('mapped consumer',wppvPublic,gpmOverlay,-1,
    wpakRules,1,False,wseWhole);
  Dependencies:=Learned.CopyDependencies; SetLength(Dependencies,3);
  Dependencies[1]:=MakeWfcPipelineDependency(2,1);
  Dependencies[2]:=MakeWfcPipelineDependency(3,2);
  Origin:=MakeWfcLatticeVector(-12+Index*50,-6+Index*7,Index*9);
  Pitch:=MakeWfcLatticeVector(2+Index,3+Index,4+Index);
  SetLength(Topologies,4);
  for I:=0 to 2 do Topologies[I]:=MakeWfcPipelinePassTopology(
    Learned.Rank,Origin,Pitch,Learned.WrapNeighbors);
  Inc(Origin.X,Pitch.X);
  Topologies[3]:=MakeWfcPipelinePassTopology(1,Origin,
    MakeWfcLatticeVector(Pitch.X*2,Pitch.Y,Pitch.Z),False);
  Query:=Default(TWfcPipelineMappedQuery);
  Query.Kind:=gpmkPoint; Query.Match:=gpmmAll;
  Query.MinimumOffset:=MakeGraphOffset(-Pitch.X,0,0);
  Query.AllowedProviderTokens:=Tokens([FragmentToken(Index)]);
  SetLength(Requirements,1);
  Requirements[0]:=MakeWfcPipelineMappedRequirement(3,'marker',2,Query);
  Quotas:=Learned.CopyValueQuotas; SetLength(Quotas,2);
  Quotas[1]:=Learned.ValueQuotaAt(0); Quotas[1].PassIndex:=2;
  Connections:=Learned.CopyConnectivities; SetLength(Connections,2);
  Connections[1]:=Learned.ConnectivityAt(0); Connections[1].PassIndex:=2;
  Result:=TWfcPipelineModel.Create(Learned.CopyMetadata,Learned.CopyVersions,
    Learned.Rank,Learned.WrapNeighbors,Learned.RunMode,Resources,Passes,
    Dependencies,Learned.CopyBridges,Requirements,Quotas,Connections,
    WFC_PIPELINE_PASS_MAPPING_VERSION,Topologies);
end;

procedure CheckTokens(const Actual,Expected: TWfcModelTokens; const Context: String);
var I: Integer;
begin
  Check(Length(Actual)=Length(Expected),Context+' token count');
  for I:=0 to High(Expected) do Check(Actual[I]=Expected[I],Context+' token order');
end;

procedure InspectComposition(const Composition: TWfcPipelineComposition;
  const Sources: TRecipes; const Inputs: TWfcPipelineFragmentInputs);
var R: TWfcPipelineModel; I,J,PassBase,ResourceBase: Integer;
  Original,Remapped: TWfcPipelineResource; P,SourceP: TWfcPipelinePass;
  B,SourceB: TWfcPipelineBridge; D,SourceD: TWfcPipelineDependency;
  Q,SourceQ: TWfcPipelineRequirement; V,SourceV: TWfcPipelineValueQuota;
  C,SourceC: TWfcPipelineConnectivity;
  T,SourceT: TWfcPipelinePassTopology;
begin
  R:=Composition.BorrowRecipe;
  Check((R.PassCount=16) and (R.ResourceCount=8) and (R.BridgeCount=4),
    'four learned fragments preserve all resource and projection rows');
  Check((R.RequirementCount=4) and (R.ValueQuotaCount=8) and
    (R.ConnectivityCount=8),'mapped and learned/alias policies are retained');
  Check(R.CopyMetadata.LicenseIdentifier='MIT','explicit output license is caller supplied');
  for I:=0 to FragmentCount-1 do
  begin
    PassBase:=I*4; ResourceBase:=I*2;
    Check(Composition.FragmentAt(I).RecipeText=Inputs[I].RecipeText,
      'exact source recipe envelope retained');
    Check(Composition.ResolvePass(FixtureId(I),'public alias')=PassBase+2,
      'original alias resolves to remapped public pass');
    Check(Composition.ResolveResource(FixtureId(I),'learned')=ResourceBase,
      'repeated original learned resource ID resolves within fragment');
    for J:=0 to Sources[I].ResourceCount-1 do
    begin
      Original:=Sources[I].ResourceAt(J); Remapped:=R.ResourceAt(ResourceBase+J);
      Check(Composition.MapIndex(FixtureId(I),wpcosResource,J)=ResourceBase+J,
        'resource prefix map');
      Check((Remapped.Id=Inputs[I].ResourceIds[J]) and (Remapped.Kind=Original.Kind),
        'only explicit resource ID is renamed');
      Check(Remapped.Document=Original.Document,'nested resource canonical bytes unchanged');
      Check((Remapped.SourceDescription=Original.SourceDescription) and
        (Remapped.SourceLicenseIdentifier=Original.SourceLicenseIdentifier) and
        (Remapped.SourceFingerprint=Original.SourceFingerprint),
        'resource source/license/fingerprint provenance unchanged');
    end;
    for J:=0 to 3 do
    begin
      P:=R.PassAt(PassBase+J); SourceP:=Sources[I].PassAt(J);
      Check(Composition.MapIndex(FixtureId(I),wpcosPass,J)=PassBase+J,'pass prefix map');
      Check((P.LabelName=Inputs[I].PassLabels[J]) and
        (P.Visibility=SourceP.Visibility) and (P.AdapterKind=SourceP.AdapterKind) and
        (P.Mode=SourceP.Mode),'private/public adapter and pass semantics retained');
      if SourceP.ResourceIndex=-1 then Check(P.ResourceIndex=-1,'no-resource sentinel retained')
      else Check(P.ResourceIndex=ResourceBase+SourceP.ResourceIndex,'resource owner remapped');
      Check((P.HasSequenceExtent=SourceP.HasSequenceExtent) and
        (P.SequenceExtent=SourceP.SequenceExtent),'sequence whole/wrap extent retained');
      T:=R.PassTopologyAt(PassBase+J); SourceT:=Sources[I].PassTopologyAt(J);
      Check((T.Rank=SourceT.Rank) and (T.Origin.X=SourceT.Origin.X) and
        (T.Origin.Y=SourceT.Origin.Y) and (T.Origin.Z=SourceT.Origin.Z) and
        (T.Pitch.X=SourceT.Pitch.X) and (T.Pitch.Y=SourceT.Pitch.Y) and
        (T.Pitch.Z=SourceT.Pitch.Z) and (T.Wrap=SourceT.Wrap),
        'rank and all world topology fields retained');
      if J>0 then CheckTokens(R.CopyPublicVocabulary(PassBase+J),
        Sources[I].CopyPublicVocabulary(J),'public vocabulary is not renamed');
    end;
    Check(R.PassAt(PassBase+2).TransformSourceIndex=PassBase+1,
      'alias targets its own remapped public projection, not private keys');
    SourceB:=Sources[I].BridgeAt(0); B:=R.BridgeAt(I);
    Check(Composition.MapIndex(FixtureId(I),wpcosBridge,0)=I,'bridge prefix map');
    Check((B.Kind=SourceB.Kind) and (B.SourcePassIndex=PassBase) and
      (B.TargetPassIndex=PassBase+1),'private projection endpoints remapped');
    for J:=0 to 2 do
    begin
      D:=R.DependencyAt(I*3+J); SourceD:=Sources[I].DependencyAt(J);
      Check(Composition.MapIndex(FixtureId(I),wpcosDependency,J)=I*3+J,'dependency prefix map');
      Check((D.ConsumerPassIndex=PassBase+SourceD.ConsumerPassIndex) and
        (D.ProviderPassIndex=PassBase+SourceD.ProviderPassIndex),
        'bridge, alias and mapped consumer dependency rows stay internal');
    end;
    Q:=R.RequirementAt(I); SourceQ:=Sources[I].RequirementAt(0);
    Check(Composition.MapIndex(FixtureId(I),wpcosRequirement,0)=I,'mapped row prefix');
    Check((Q.Kind=wprqMapped) and (Q.ConsumerPassIndex=PassBase+3) and
      (Q.ProviderPassIndex=PassBase+2) and (Q.ConsumerToken='marker') and
      (Q.MappedQuery.Kind=SourceQ.MappedQuery.Kind) and
      (Q.MappedQuery.Match=SourceQ.MappedQuery.Match) and
      (Q.MappedQuery.MinimumOffset.DeltaX=SourceQ.MappedQuery.MinimumOffset.DeltaX),
      'mapped query owner/alias remapped but signed world offset not translated');
    CheckTokens(Q.MappedQuery.AllowedProviderTokens,SourceQ.MappedQuery.AllowedProviderTokens,
      'mapped query public allowed values');
    for J:=0 to 1 do
    begin
      V:=R.ValueQuotaAt(I*2+J); SourceV:=Sources[I].ValueQuotaAt(J);
      Check(Composition.MapIndex(FixtureId(I),wpcosValueQuota,J)=I*2+J,'quota prefix map');
      Check((V.PassIndex=PassBase+SourceV.PassIndex) and (V.LabelText=SourceV.LabelText) and
        (V.MinimumCount=SourceV.MinimumCount) and (V.MaximumCount=SourceV.MaximumCount),
        'learner and alias quota owners/bounds unchanged');
      CheckTokens(V.Values,SourceV.Values,'quota public values');
      C:=R.ConnectivityAt(I*2+J); SourceC:=Sources[I].ConnectivityAt(J);
      Check(Composition.MapIndex(FixtureId(I),wpcosConnectivity,J)=I*2+J,'connectivity prefix map');
      Check((C.PassIndex=PassBase+SourceC.PassIndex) and (C.LabelText=SourceC.LabelText) and
        (C.Root.X=0) and (C.Root.Y=0) and (C.Root.Z=0) and
        (Length(C.RequiredPositions)=1) and (C.RequiredPositions[0].X=1) and
        (C.RequiredPositions[0].Y=0) and (C.RequiredPositions[0].Z=0) and
        C.RequireAllParticipants,'connectivity coordinates are local and public owner retained');
      Check((Length(C.Values)=1) and (C.Values[0].Value=SourceC.Values[0].Value) and
        (C.Values[0].Openings=SourceC.Values[0].Openings) and C.Values[0].RequiredByValue,
        'all six connectivity ports and required-by-value flag retained');
    end;
  end;
end;

function Extents(const Second: Boolean): TWfcPipelinePassExtents;
var I,J: Integer; Cells: TWfcLatticeVector;
begin
  Result:=nil; SetLength(Result,16);
  for I:=0 to FragmentCount-1 do
  begin
    case I of
      0: if Second then Cells:=MakeWfcLatticeVector(4,2,1)
         else Cells:=MakeWfcLatticeVector(2,2,1);
      1: if Second then Cells:=MakeWfcLatticeVector(5,1,1)
         else Cells:=MakeWfcLatticeVector(3,1,1);
      2: if Second then Cells:=MakeWfcLatticeVector(2,2,3)
         else Cells:=MakeWfcLatticeVector(2,2,2);
      3: if Second then Cells:=MakeWfcLatticeVector(6,1,1)
         else Cells:=MakeWfcLatticeVector(4,1,1);
    end;
    for J:=0 to 2 do Result[I*4+J]:=Cells;
    Result[I*4+3]:=MakeWfcLatticeVector(1+Ord(Second),1,1);
  end;
end;

procedure ExecuteComposition(const Composition: TWfcPipelineComposition;
  const Second: Boolean);
var R: TWfcPipelineModel; Run: TWfcPipelineRun;
  Output,Replay,Decoded: TWfcPipelineResult; E: TWfcPipelinePassExtents;
  Locks: TWfcPipelineCellLocks; Domains: TWfcPipelineCellDomains;
  Layer: TWfcPipelineResultLayer; I,J,ExpectedCount: Integer;
  ExpectedToken: TWfcModelToken; Saved: String;
begin
  R:=Composition.BorrowRecipe; E:=Extents(Second);
  SetLength(Locks,4); SetLength(Domains,4);
  for I:=0 to FragmentCount-1 do
  begin
    Locks[I]:=MakeWfcPipelineCellLock(I*4+2,1,0,0,FragmentToken(I));
    Domains[I]:=MakeWfcPipelineCellDomain(I*4+1,0,0,0,Tokens([FragmentToken(I)]));
  end;
  Run:=TWfcPipelineRun.Create(R,E,17,wpssOneWay,65536,0,True,Locks,Domains);
  try
    Check((Run.PassCellCount(0)<>Run.PassCellCount(4)) and
      (Run.PassCellCount(4)<>Run.PassCellCount(8)),
      'independent learned grids have unlike invocation cell counts');
    Output:=ExecuteWfcPipeline(R,Run);
    try
      Check(Output.Status=wprsSolved,'real mixed-resource runtime solved');
      Check(Output.LayerCount=12,'only public outputs, aliases and consumers are exported');
      for I:=0 to Output.LayerCount-1 do
      begin
        Layer:=Output.LayerAt(I);
        Check((Layer.PassIndex mod 4)<>0,'no private learned keys leak into output layers');
        Check(Layer.LabelName=R.PassAt(Layer.PassIndex).LabelName,'export uses remapped public label');
        ExpectedCount:=E[Layer.PassIndex].X*E[Layer.PassIndex].Y*E[Layer.PassIndex].Z;
        Check(Length(Layer.Tokens)=ExpectedCount,'each output uses its own invocation extent');
        if (Layer.PassIndex mod 4)=3 then ExpectedToken:='marker'
        else ExpectedToken:=FragmentToken(Layer.PassIndex div 4);
        for J:=0 to High(Layer.Tokens) do Check(Layer.Tokens[J]=ExpectedToken,
          'real projection, inverse public input, alias and mapped consumer values');
      end;
      Saved:=EncodeWfcPipelineResultText(Output);
      Decoded:=DecodeWfcPipelineResultText(Saved,R,Run);
      try Check(EncodeWfcPipelineResultText(Decoded)=Saved,'full result canonical roundtrip validates composed policies');
      finally Decoded.Free; end;
      Replay:=ExecuteWfcPipeline(R,Run);
      try Check(EncodeWfcPipelineResultText(Replay)=Saved,'same composed invocation replays byte identically');
      finally Replay.Free; end;
    finally Output.Free; end;
  finally Run.Free; end;
end;

procedure RejectBridgeExtentMismatches(const Composition: TWfcPipelineComposition);
var E: TWfcPipelinePassExtents; Run: TWfcPipelineRun; I: Integer;
  Rejected: Boolean; BeforeText: String;
begin
  BeforeText:=Composition.RecipeText;
  for I:=0 to FragmentCount-1 do
  begin
    E:=Extents(False);
    { Change the private source only: alias/output still agree, so this is a
      projection-bridge failure rather than an earlier transform mismatch. }
    Inc(E[I*4].X); Run:=nil; Rejected:=False;
    try
      try Run:=TWfcPipelineRun.Create(Composition.BorrowRecipe,E,17,wpssOneWay,
        65536,0,False,nil,nil);
      except on Error: EWfcPipelineRun do
        Rejected:=(Pos('projection bridge',Error.Message)>0) and (Length(Error.Message)>0);
      end;
      Check(Rejected and (Run=nil),'each learned projection rejects unlike source/target extents');
      Check(Composition.RecipeText=BeforeText,'invalid invocation cannot change composed recipe');
    finally Run.Free; end;
  end;
end;

procedure TestRealLearnedResources;
var Learned: TWfcPipelineModel; Sources: TRecipes;
  Inputs: TWfcPipelineFragmentInputs; Composition: TWfcPipelineComposition;
  Decoded: TWfcPipelineModel; I: Integer; Text: String;
begin
  Sources:=Default(TRecipes); Composition:=nil; SetLength(Inputs,FragmentCount);
  try
    for I:=0 to FragmentCount-1 do
    begin
      Learned:=LearnedFragment(I);
      try Sources[I]:=DecorateFragment(Learned,I);
      finally Learned.Free; end;
      Inputs[I].FragmentId:=FixtureId(I);
      Inputs[I].RecipeText:=EncodeWfcPipelineModelText(Sources[I]);
      Inputs[I].ResourceIds:=Tokens([TWfcModelToken('resource-'+IntToStr(I)),
        TWfcModelToken('rules-'+IntToStr(I))]);
      Inputs[I].PassLabels:=Tokens([TWfcModelToken('private-'+IntToStr(I)),
        TWfcModelToken('public-'+IntToStr(I)),TWfcModelToken('alias-'+IntToStr(I)),
        TWfcModelToken('consumer-'+IntToStr(I))]);
    end;
    Composition:=TWfcPipelineComposition.Create(MakeWfcPipelineMetadata(
      'mixed learned composition','MIT','explicit assembly of local corpora','composition-fixture-v1'),Inputs);
    InspectComposition(Composition,Sources,Inputs);
    Text:=Composition.RecipeText;
    Check(Pos('wfcpipeline=5'#10,Text)=1,'composition owns explicit spatial canonical recipe5');
    Decoded:=DecodeWfcPipelineModelText(Text);
    try Check(EncodeWfcPipelineModelText(Decoded)=Text,'complete owned canonical recipe roundtrip');
    finally Decoded.Free; end;
    ExecuteComposition(Composition,False);
    ExecuteComposition(Composition,True);
    RejectBridgeExtentMismatches(Composition);
  finally
    Composition.Free;
    for I:=0 to FragmentCount-1 do Sources[I].Free;
  end;
end;

begin
  try
    TestRealLearnedResources;
    WriteLn('Pipeline compose learned-resource checks: ',Checks);
  except on E: Exception do begin
    WriteLn('FAIL: ',E.ClassName,': ',E.Message); Halt(1);
  end; end;
end.
