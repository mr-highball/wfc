{ SPDX-License-Identifier: MIT
  Copyright (c) 2021 mr-highball
  Immutable pipeline composition conformance. }
program wfc_pipeline_compose_test;
{$mode delphi}{$H+}
uses SysUtils,{$IFDEF PAS2JS}wfc_browser_test_host,{$ENDIF}
  wfc,wfc_model,wfc_rule_model,wfc_rule_text,wfc_lattice,wfc_sequence,
  wfc_pipeline_layout,wfc_pipeline_model,wfc_pipeline_text,wfc_pipeline_compose;

var Checks: Integer;

procedure Check(const OK: Boolean; const Detail: String);
begin Inc(Checks); if not OK then raise Exception.Create(Detail); end;

function Tokens(const Values: array of TWfcModelToken): TWfcModelTokens;
var I: Integer;
begin Result:=nil; SetLength(Result,Length(Values)); for I:=0 to High(Values) do Result[I]:=Values[I]; end;

function Meta: TWfcPipelineMetadata;
begin Result:=MakeWfcPipelineMetadata('composed','MIT','explicit composition','author-v1'); end;

function RuleResource(const Id: TWfcModelToken): TWfcPipelineResource;
var Rules: TWfcRuleModel; Weights: TWfcModelIntegerArray;
begin
  SetLength(Weights,2); Weights[0]:=3; Weights[1]:=1;
  Rules:=TWfcRuleModel.Create(1,Tokens(['A','B']),Weights,nil);
  try Result:=MakeWfcPipelineResource(Id,wprkRules,EncodeWfcRuleText(Rules),'exact source','MIT','opaque:source/fingerprint');
  finally Rules.Free; end;
end;

function Simple(const Mode: TGraphPassMode; const Empty: Boolean=False;
  const Traversal: TGraphRunMode=rmBottomUp; const PatternVersion: Integer=2;
  const SequenceVersion: Integer=2): TWfcPipelineModel;
var R: TWfcPipelineResources; P: TWfcPipelinePasses; V: TWfcPipelineVersions;
begin
  R:=nil; SetLength(P,1);
  if Empty then P[0]:=MakeWfcPipelinePass('root',wppvPrivate,Mode,-1,wpakEmpty,-1,False,wseWhole)
  else begin SetLength(R,1); R[0]:=RuleResource('rules');
    P[0]:=MakeWfcPipelinePass('root',wppvPublic,Mode,-1,wpakRules,0,False,wseWhole); end;
  V:=CurrentWfcPipelineVersions; V.Pattern2DBridgeVersion:=PatternVersion; V.SequenceBridgeVersion:=SequenceVersion;
  Result:=TWfcPipelineModel.Create(MakeWfcPipelineMetadata('source','MIT','source metadata','source-id'),
    V,1,False,Traversal,R,P,nil,nil,nil,nil,nil);
end;

function Rich: TWfcPipelineModel;
var R: TWfcPipelineResources; P: TWfcPipelinePasses; D: TWfcPipelineDependencies;
  Q: TWfcPipelineRequirements; Terms: TWfcPipelineRequirementTerms;
  Mapped: TWfcPipelineMappedQuery; Quotas: TWfcPipelineValueQuotas;
  C: TWfcPipelineConnectivities; Profiles: TWfcPipelineConnectivityValues;
  Root: TGraphPosition; Required: TGraphPositions; T: TWfcPipelinePassTopologies; I: Integer;
begin
  SetLength(R,2); R[0]:=RuleResource('rules'); R[1]:=RuleResource('extra');
  SetLength(P,5);
  P[0]:=MakeWfcPipelinePass('provider',wppvPublic,gpmOverlay,-1,wpakRules,0,False,wseWhole);
  P[1]:=MakeWfcPipelinePass('forward-alias',wppvPublic,gpmTransform,4,wpakEmpty,-1,False,wseWhole);
  P[2]:=MakeWfcPipelinePass('consumer',wppvPublic,gpmOverlay,-1,wpakRules,0,False,wseWhole);
  P[3]:=MakeWfcPipelinePass('private-copy',wppvPrivate,gpmLegacy,-1,wpakEmpty,-1,False,wseWhole);
  P[4]:=MakeWfcPipelinePass('later-source',wppvPublic,gpmOverlay,-1,wpakRules,1,False,wseWhole);
  SetLength(D,3); D[0]:=MakeWfcPipelineDependency(1,4);
  D[1]:=MakeWfcPipelineDependency(2,0); D[2]:=MakeWfcPipelineDependency(3,2);
  SetLength(Q,4); SetLength(Terms,1);
  Terms[0]:=MakeWfcPipelineRequirementTerm(0,0,0,Tokens(['A','B']));
  Q[0]:=MakeWfcPipelineRequirement(2,'A',0,wprqExact,Terms);
  SetLength(Terms,2);
  Terms[0]:=MakeWfcPipelineRequirementTerm(-1,0,0,Tokens(['A']));
  Terms[1]:=MakeWfcPipelineRequirementTerm(1,0,0,Tokens(['B']));
  Q[1]:=MakeWfcPipelineRequirement(2,'A',0,wprqAny,Terms);
  Q[2]:=MakeWfcPipelineCountRequirement(2,'B',0,Terms,0,2,gpcmDistinctCells);
  Mapped:=Default(TWfcPipelineMappedQuery); Mapped.Kind:=gpmkRegionCoverage;
  Mapped.Match:=gpmmCount; Mapped.MinimumOffset:=MakeGraphOffset(-1,0,0);
  Mapped.MaximumOffset:=MakeGraphOffset(3,1,1); Mapped.MaximumMatches:=2;
  Mapped.AllowedProviderTokens:=Tokens(['A','B']); Q[3]:=MakeWfcPipelineMappedRequirement(2,'B',0,Mapped);
  SetLength(Quotas,2); Quotas[0]:=MakeWfcPipelineValueQuota(1,'same-label',Tokens(['A']),0,100);
  Quotas[1]:=MakeWfcPipelineValueQuota(2,'same-label',Tokens(['A','B']),0,200);
  Root:=Default(TGraphPosition); SetLength(Required,1); Required[0]:=Root; Required[0].X:=1;
  SetLength(Profiles,2); Profiles[0]:=MakeWfcPipelineConnectivityValue('A',[gdEast,gdWest,gdUp],True);
  Profiles[1]:=MakeWfcPipelineConnectivityValue('B',[gdEast,gdWest],False);
  SetLength(C,1); C[0]:=MakeWfcPipelineConnectivity(1,'connected-alias',Root,Required,Profiles,False);
  SetLength(T,5);
  for I:=0 to 4 do T[I]:=MakeWfcPipelinePassTopology(1,MakeWfcLatticeVector(-6,2,3),MakeWfcLatticeVector(2,3,4),False);
  Result:=TWfcPipelineModel.Create(MakeWfcPipelineMetadata('rich source','MIT','all policy tags','rich-v1'),
    CurrentWfcPipelineVersions,1,False,rmBottomUp,R,P,D,nil,Q,Quotas,C,1,T);
end;

function ForwardRoot: TWfcPipelineModel;
var R: TWfcPipelineResources; P: TWfcPipelinePasses; D: TWfcPipelineDependencies;
begin
  SetLength(R,1); R[0]:=RuleResource('rules'); SetLength(P,2); SetLength(D,1);
  P[0]:=MakeWfcPipelinePass('root',wppvPublic,gpmTransform,1,wpakEmpty,-1,False,wseWhole);
  P[1]:=MakeWfcPipelinePass('later',wppvPublic,gpmOverlay,-1,wpakRules,0,False,wseWhole);
  D[0]:=MakeWfcPipelineDependency(0,1);
  Result:=TWfcPipelineModel.Create(Meta,1,False,rmBottomUp,R,P,D,nil,nil);
end;

function Input(const Id: TWfcModelToken; const Model: TWfcPipelineModel): TWfcPipelineFragmentInput;
var I: Integer;
begin
  Result.FragmentId:=Id; Result.RecipeText:=EncodeWfcPipelineModelText(Model);
  SetLength(Result.ResourceIds,Model.ResourceCount); SetLength(Result.PassLabels,Model.PassCount);
  for I:=0 to Model.ResourceCount-1 do Result.ResourceIds[I]:=Id+'.resource'+TWfcModelToken(IntToStr(I));
  for I:=0 to Model.PassCount-1 do Result.PassLabels[I]:=Id+'.pass'+TWfcModelToken(IntToStr(I));
end;

function CopyInputs(const Values: TWfcPipelineFragmentInputs): TWfcPipelineFragmentInputs;
var I,J: Integer;
begin
  Result:=nil; SetLength(Result,Length(Values));
  for I:=0 to High(Values) do begin
    Result[I].FragmentId:=Values[I].FragmentId; Result[I].RecipeText:=Values[I].RecipeText;
    SetLength(Result[I].ResourceIds,Length(Values[I].ResourceIds));
    for J:=0 to High(Values[I].ResourceIds) do Result[I].ResourceIds[J]:=Values[I].ResourceIds[J];
    SetLength(Result[I].PassLabels,Length(Values[I].PassLabels));
    for J:=0 to High(Values[I].PassLabels) do Result[I].PassLabels[J]:=Values[I].PassLabels[J];
  end;
end;

procedure Rejected(const F: TWfcPipelineFragmentInputs; const Fragment: String);
var C: TWfcPipelineComposition; Saw: Boolean;
begin
  C:=nil; Saw:=False;
  try
    try C:=TWfcPipelineComposition.Create(Meta,F);
    except on E: EWfcPipelineCompose do Saw:=(E.Message<>'') and (Pos(Fragment,E.Message)>0); end;
    Check(Saw,'specific compose rejection: '+Fragment); Check(C=nil,'no partial owner escapes rejection');
  finally C.Free; end;
end;

procedure TestOptionalMetadata;
var A,Decoded:TWfcPipelineModel; F:TWfcPipelineFragmentInputs;
  M,Actual:TWfcPipelineMetadata; C:TWfcPipelineComposition;
  I:Integer; Bad:TWfcModelToken;

  procedure RejectMetadata(const Value:TWfcPipelineMetadata;
    const Expected:String);
  var Candidate:TWfcPipelineComposition; Saw:Boolean;
  begin
    Candidate:=nil; Saw:=False;
    try
      try Candidate:=TWfcPipelineComposition.Create(Value,F);
      except on E:EWfcPipelineCompose do Saw:=Pos(Expected,E.Message)>0; end;
      Check(Saw,'metadata retains typed boundary: '+Expected);
      Check(Candidate=nil,'invalid metadata cannot publish a partial owner');
    finally Candidate.Free; end;
  end;

begin
  A:=Simple(gpmOverlay);
  try
    SetLength(F,1); F[0]:=Input('metadata',A);
    for I:=0 to 3 do
    begin
      M:=Meta;
      if (I and 1)=0 then M.SourceDescription:='';
      if (I and 2)=0 then M.SourceFingerprint:='';
      C:=TWfcPipelineComposition.Create(M,F);
      try
        Actual:=C.BorrowRecipe.CopyMetadata;
        Check(Actual.SourceDescription=M.SourceDescription,
          'optional source description is preserved for combination '+IntToStr(I));
        Check(Actual.SourceFingerprint=M.SourceFingerprint,
          'optional source fingerprint is preserved for combination '+IntToStr(I));
        Decoded:=DecodeWfcPipelineModelText(C.RecipeText);
        try Check(EncodeWfcPipelineModelText(Decoded)=C.RecipeText,
          'optional metadata participates in exact canonical recipe bytes');
        finally Decoded.Free; end;
      finally C.Free; end;
    end;
    M:=Meta; M.Name:=''; RejectMetadata(M,'metadata name cannot be empty');
    M:=Meta; M.LicenseIdentifier:=''; RejectMetadata(M,'metadata license cannot be empty');
    {$IFDEF PAS2JS}Bad:=#$D800;{$ELSE}
    SetLength(Bad,2); Bad[1]:=AnsiChar($C0); Bad[2]:=AnsiChar($AF);
    {$ENDIF}
    for I:=0 to 3 do
    begin
      M:=Meta;
      case I of
        0:M.Name:=Bad;
        1:M.LicenseIdentifier:=Bad;
        2:M.SourceDescription:=Bad;
        3:M.SourceFingerprint:=Bad;
      end;
      RejectMetadata(M,'contains invalid Unicode');
    end;
  finally A.Free; end;
end;

procedure TestRemappingAndOwnership;
var F,Saved: TWfcPipelineFragmentInputs; A,B: TWfcPipelineModel;
  C,Again: TWfcPipelineComposition; R: TWfcPipelineModel;
  Original: String; I,J: Integer; Q: TWfcPipelineRequirement;
  CopyF: TWfcPipelineFragmentInput; Profiles: TWfcPipelineConnectivity;
begin
  A:=Simple(gpmOverlay); B:=Rich; C:=nil; Again:=nil;
  try
    SetLength(F,3); F[0]:=Input('first',A); F[1]:=Input('rich',B); F[2]:=Input('last',A);
    Saved:=CopyInputs(F); C:=TWfcPipelineComposition.Create(Meta,F); R:=C.BorrowRecipe;
    Check((C.FragmentCount=3) and (R.PassCount=7) and (R.ResourceCount=4),'unequal contiguous fragment extents');
    Check(R.HasPassMapping and (WfcPipelineModelTextVersion(R)=5),'explicit spatial composition even with legacy fragments');
    Check((R.DependencyCount=3) and (R.BridgeCount=0) and (R.RequirementCount=4) and
      (R.ValueQuotaCount=2) and (R.ConnectivityCount=1),'all row counts preserved without cross-links');
    Check((C.ResolvePass('rich','provider')=1) and (C.ResolvePass('rich','forward-alias')=2), 'original labels resolve stable map');
    Check((C.ResolveResource('rich','rules')=1) and (C.ResolveResource('rich','extra')=2),'resource references map separately');
    Check((C.MapIndex('last',wpcosResource,0)=3) and (C.MapIndex('last',wpcosPass,0)=6),'independent prefix families');
    for I:=0 to 3 do Check(C.MapIndex('rich',wpcosRequirement,I)=I,'requirement ordinal map');
    for I:=0 to 2 do Check(C.MapIndex('rich',wpcosDependency,I)=I,'dependency ordinal map');
    for I:=0 to 1 do Check(C.MapIndex('rich',wpcosValueQuota,I)=I,'quota ordinal map');
    Check(C.MapIndex('rich',wpcosConnectivity,0)=0,'connectivity ordinal map');
    Check((R.PassAt(2).Mode=gpmTransform) and (R.PassAt(2).TransformSourceIndex=5) and (R.PassAt(2).ResourceIndex=-1),'forward alias and sentinel survive');
    Check((R.PassAt(5).ResourceIndex=2) and (R.PassAt(3).ResourceIndex=1),'materialized resource references remap');
    Check((R.PassAt(4).Mode=gpmLegacy) and (R.PassAt(4).Visibility=wppvPrivate) and
      (R.PassAt(4).TransformSourceIndex=-1),'internal legacy remains private and definitionless');
    Check((R.DependencyAt(0).ConsumerPassIndex=2) and (R.DependencyAt(0).ProviderPassIndex=5),'forward dependency remains forward');
    Check((R.DependencyAt(2).ConsumerPassIndex=4) and (R.DependencyAt(2).ProviderPassIndex=3),'internal legacy predecessor remains within fragment');
    for I:=0 to 3 do begin Q:=R.RequirementAt(I);
      Check((Q.ConsumerPassIndex=3) and (Q.ProviderPassIndex=1),'both policy endpoints remapped');
      Check(Q.Kind=B.RequirementAt(I).Kind,'requirement tag retained');
    end;
    Q:=R.RequirementAt(0); Check((Length(Q.Terms)=1) and (Q.Terms[0].AllowedProviderTokens[1]='B'),'exact token order');
    Q:=R.RequirementAt(1); Check((Q.Terms[0].OffsetX=-1) and (Q.Terms[1].OffsetX=1),'signed ordered terms unchanged');
    Q:=R.RequirementAt(2); Check((Q.CountMode=gpcmDistinctCells) and (Q.MinimumCount=0) and (Q.MaximumCount=2),'count semantics unchanged');
    Q:=R.RequirementAt(3); Check((Q.MappedQuery.Kind=gpmkRegionCoverage) and (Q.MappedQuery.Match=gpmmCount) and
      (Q.MappedQuery.MinimumOffset.DeltaX=-1) and (Q.MappedQuery.MaximumOffset.DeltaX=3) and
      (Q.MappedQuery.MaximumMatches=2) and (Q.MappedQuery.AllowedProviderTokens[1]='B'),'mapped payload is not index-remapped');
    Check((R.ValueQuotaAt(0).PassIndex=2) and (R.ValueQuotaAt(1).PassIndex=3),'quota public owners not lowered');
    Profiles:=R.ConnectivityAt(0);
    Check((Profiles.PassIndex=2) and (Profiles.RequiredPositions[0].X=1) and (gdUp in Profiles.Values[0].Openings) and
      Profiles.Values[0].RequiredByValue and not Profiles.RequireAllParticipants,'connectivity owner/coordinates/all-direction profiles preserved');
    Check((R.PassTopologyAt(1).Origin.X=-6) and (R.PassTopologyAt(1).Origin.Y=2) and
      (R.PassTopologyAt(1).Pitch.Z=4) and (R.PassTopologyAt(0).Pitch.X=1),'legacy topology synthesis and explicit world axes coexist');
    for I:=0 to B.ResourceCount-1 do begin
      Check(R.ResourceAt(1+I).Document=B.ResourceAt(I).Document,'exact resource document');
      Check(R.ResourceAt(1+I).SourceDescription=B.ResourceAt(I).SourceDescription,'resource description');
      Check(R.ResourceAt(1+I).SourceLicenseIdentifier=B.ResourceAt(I).SourceLicenseIdentifier,'resource license');
      Check(R.ResourceAt(1+I).SourceFingerprint=B.ResourceAt(I).SourceFingerprint,'resource fingerprint');
    end;
    Check((R.ResourceAt(1).Document=R.ResourceAt(2).Document) and (R.ResourceAt(1).Id<>R.ResourceAt(2).Id),'identical payloads never deduplicated');
    Check(C.FragmentAt(1).RecipeText=Saved[1].RecipeText,'full source envelope and metadata retained');
    Check(R.CopyMetadata.Name='composed','output metadata explicit');
    Original:=C.RecipeText;
    F[1].ResourceIds[0]:='mutated caller resource'; F[1].PassLabels[0]:='mutated caller pass'; F[1].RecipeText:='invalid';
    CopyF:=C.FragmentAt(1); CopyF.ResourceIds[0]:='mutated returned resource'; CopyF.PassLabels[0]:='mutated returned pass';
    Q:=R.RequirementAt(3); Q.MappedQuery.AllowedProviderTokens[0]:='mutated returned token';
    Check(C.RecipeText=Original,'caller and returned nested array mutation cannot alter output');
    Check(C.FragmentAt(1).PassLabels[0]=Saved[1].PassLabels[0],'returned arrays are detached');
    FreeAndNil(A); FreeAndNil(B); F:=nil;
    Check(C.RecipeText=Original,'input model lifetimes independent');
    Again:=TWfcPipelineComposition.Create(Meta,Saved);
    Check(Again.RecipeText=Original,'same explicit ordered inputs give complete identical bytes');
    for I:=0 to 2 do begin CopyF:=Again.FragmentAt(I);
      for J:=0 to High(CopyF.PassLabels) do Check(CopyF.PassLabels[J]=Saved[I].PassLabels[J],'stable manifest mapping');
    end;
    WriteLn('compose-recipe-bytes=',Length(Original),' signature=',WfcPipelineSignatureHex(R.Signature));
  finally Again.Free; C.Free; B.Free; A.Free; end;
end;

procedure TestRejectionsAndLegacy;
var A,B: TWfcPipelineModel; Good,F: TWfcPipelineFragmentInputs;
  C: TWfcPipelineComposition; I,J: Integer; Saw: Boolean; Saved: String;
begin
  A:=Simple(gpmOverlay); B:=nil; C:=nil;
  try
    SetLength(Good,2); Good[0]:=Input('one',A); Good[1]:=Input('two',A);
    C:=TWfcPipelineComposition.Create(Meta,Good); Saved:=C.RecipeText;
    F:=CopyInputs(Good); F[1].FragmentId:=F[0].FragmentId; Rejected(F,'duplicate fragment ID');
    F:=CopyInputs(Good); F[1].ResourceIds[0]:=F[0].ResourceIds[0]; Rejected(F,'duplicate final resource ID');
    F:=CopyInputs(Good); F[1].PassLabels[0]:=F[0].PassLabels[0]; Rejected(F,'duplicate final pass label');
    F:=CopyInputs(Good); F[1].PassLabels[0]:=''; Rejected(F,'cannot be empty');
    F:=CopyInputs(Good); F[1].ResourceIds:=nil; Rejected(F,'name-vector lengths');
    F:=CopyInputs(Good); SetLength(F[1].PassLabels,2); F[1].PassLabels[1]:='extra'; Rejected(F,'name-vector lengths');
    F:=CopyInputs(Good); F[1].RecipeText:='wfcpipeline=99'+#10; Rejected(F,'version');
    F:=CopyInputs(Good); F[1].RecipeText:=F[1].RecipeText+#10; Rejected(F,'blank lines');
    F:=nil; Rejected(F,'fragment');
    SetLength(F,257); Rejected(F,'fragment');
    F:=CopyInputs(Good); F[1].PassLabels[0]:=TWfcModelToken(StringOfChar('x',WFC_PIPELINE_MAX_ENCODED_TOKEN_LENGTH+1)); Rejected(F,'raw token');
    F:=CopyInputs(Good); F[1].PassLabels[0]:=TWfcModelToken(StringOfChar('%',400000)); Rejected(F,'encoded token');
    F:=CopyInputs(Good);
    for I:=0 to 1 do begin F[I].RecipeText:='not a recipe'; SetLength(F[I].ResourceIds,33);
      for J:=0 to 32 do F[I].ResourceIds[J]:=TWfcModelToken(IntToStr(I)+'r'+IntToStr(J)); end;
    Rejected(F,'aggregate resource name count'); //before invoking either malformed nested decoder
    F:=CopyInputs(Good); F[0].PassLabels[0]:='A'; F[1].PassLabels[0]:='a';
    with TWfcPipelineComposition.Create(Meta,F) do try Check(BorrowRecipe.PassCount=2,'case-sensitive explicit names'); finally Free; end;
    B:=Simple(gpmLegacy); F:=CopyInputs(Good); F[1]:=Input('legacy',B); Rejected(F,'legacy root');
    FreeAndNil(B); B:=Simple(gpmLegacy,True); F[1]:=Input('empty-legacy',B); Rejected(F,'legacy root');
    F[0]:=Input('first-legacy',B); F[1]:=Input('overlay',A);
    with TWfcPipelineComposition.Create(Meta,F) do try
      Check((BorrowRecipe.PassAt(0).Mode=gpmLegacy) and (BorrowRecipe.PassAt(0).AdapterKind=wpakEmpty),'first empty legacy root preserved');
      Check(BorrowRecipe.DependencyCount=0,'no implicit cross-fragment dependency');
    finally Free; end;
    FreeAndNil(B); B:=ForwardRoot; F:=CopyInputs(Good); F[1]:=Input('forward-root',B);
    with TWfcPipelineComposition.Create(Meta,F) do try
      Check((BorrowRecipe.PassAt(1).Mode=gpmTransform) and (BorrowRecipe.PassAt(1).TransformSourceIndex=2),
        'appended explicit transform root preserves later local source');
      Check((BorrowRecipe.DependencyCount=1) and (BorrowRecipe.DependencyAt(0).ConsumerPassIndex=1) and
        (BorrowRecipe.DependencyAt(0).ProviderPassIndex=2),'no dependency on the preceding unrelated fragment');
    finally Free; end;
    FreeAndNil(B); B:=Simple(gpmOverlay,False,rmTopDown); F:=CopyInputs(Good); F[1]:=Input('top-down',B); Rejected(F,'run mode mismatch');
    FreeAndNil(B); B:=Simple(gpmOverlay,False,rmBottomUp,1,2); F[1]:=Input('bridge-v1',B); Rejected(F,'Pattern2DBridgeVersion');
    F[0]:=Input('same-v1',B);
    with TWfcPipelineComposition.Create(Meta,F) do try Check(BorrowRecipe.CopyVersions.Pattern2DBridgeVersion=1,'equal old bridge pins not upgraded'); finally Free; end;
    FreeAndNil(B); B:=Simple(gpmOverlay,False,rmBottomUp,2,1); F:=CopyInputs(Good); F[1]:=Input('sequence-v1',B); Rejected(F,'SequenceBridgeVersion');
    Check(C.RecipeText=Saved,'failed new owners never change existing composition');
    Saw:=False; try C.MapIndex('one',wpcosResource,-1); except on E: EWfcPipelineCompose do Saw:=True; end; Check(Saw,'sentinel is not a public row index');
    Saw:=False; try C.MapIndex('one',wpcosBridge,0); except on E: EWfcPipelineCompose do Saw:=True; end; Check(Saw,'empty row family lookup rejected');
    Saw:=False; try C.FragmentAt(2); except on E: EWfcPipelineCompose do Saw:=True; end; Check(Saw,'bad fragment index rejected');
    Saw:=False; try C.ResolvePass('one','not-present'); except on E: EWfcPipelineCompose do Saw:=True; end; Check(Saw,'unknown original label rejected');
    Saw:=False; try C.ResolveResource('missing','rules'); except on E: EWfcPipelineCompose do Saw:=True; end; Check(Saw,'unknown fragment rejected');
  finally C.Free; B.Free; A.Free; end;
end;

{$IFDEF PAS2JS}
procedure TestHostileJavaScript;
var A: TWfcPipelineModel; Good,F: TWfcPipelineFragmentInputs;
  M: TWfcPipelineMetadata; C,Existing: TWfcPipelineComposition;
  I,J,Reads,BadIndex: Integer; BadToken: TWfcModelToken;
  Saw: Boolean; Saved: String;

  procedure ResetRawInputs;
  begin
    F:=CopyInputs(Good);
    { The preceding hostile case may replace the record with null/plain data.
      Recreate the test's typed record before pas2js emits its $assign call. }
    asm M=pas.wfc_pipeline_model.TWfcPipelineMetadata.$new(); end;
    M:=Meta; Reads:=0;
  end;

  procedure RejectRaw(const Detail: String);
  begin
    C:=nil; Saw:=False;
    try
      try C:=TWfcPipelineComposition.Create(M,F);
      except on E: EWfcPipelineCompose do
        Saw:=(Pos('pipeline composition: ',E.Message)=1) and (Length(E.Message)>22); end;
      Check(Saw,'typed nonempty JavaScript input rejection: '+Detail);
      Check(C=nil,'no partial JavaScript owner: '+Detail);
      Check(Reads=0,'no getter/coercion executes: '+Detail);
    finally C.Free; end;
  end;

begin
  A:=Simple(gpmOverlay); Existing:=nil;
  try
    SetLength(Good,1); Good[0]:=Input('one',A);
    Existing:=TWfcPipelineComposition.Create(Meta,Good); Saved:=Existing.RecipeText;
    for I:=0 to 60 do
    begin
      ResetRawInputs;
      case I of
        0:asm M=null; end;
        1:asm M=[]; end;
        2:asm M={}; end;
        3:asm M.Name=12; end;
        4:asm M.LicenseIdentifier=new String('MIT'); end;
        5:asm M.SourceDescription=undefined; end;
        6:asm M.SourceFingerprint=null; end;
        7:asm Object.defineProperty(M,'Name',{get:function(){Reads++;return 'composed';}}); end;
        8:asm
          const p={LicenseIdentifier:'MIT',SourceDescription:'',SourceFingerprint:''};
          Object.defineProperty(p,'Name',{get:function(){Reads++;return 'composed';}});
          M=Object.create(p);
        end;
        9:asm F=null; end;
        10:asm F={length:1,0:F[0]}; end;
        11:asm F='not an array'; end;
        12:asm delete F[0]; end;
        13:asm Object.defineProperty(F,'0',{get:function(){Reads++;throw new Error('fragment slot getter');}}); end;
        14:asm const p=Object.create(Array.prototype);p[0]=F[0];delete F[0];Object.setPrototypeOf(F,p); end;
        15:asm F[0]=null; end;
        16:asm F[0]=[]; end;
        17:asm F[0]={}; end;
        18:asm F[0].FragmentId=1; end;
        19:asm F[0].FragmentId=new String('one'); end;
        20:asm F[0].RecipeText=null; end;
        21:asm F[0].RecipeText=new String(F[0].RecipeText); end;
        22:asm Object.defineProperty(F[0],'RecipeText',{get:function(){Reads++;throw new Error('recipe getter');}}); end;
        23:asm Object.defineProperty(F[0],'ResourceIds',{get:function(){Reads++;return ['one.resource0'];}}); end;
        24:asm Object.defineProperty(F[0],'PassLabels',{get:function(){Reads++;return ['one.pass0'];}}); end;
        25:asm
          const p={FragmentId:'one',ResourceIds:['one.resource0'],PassLabels:['one.pass0']};
          Object.defineProperty(p,'RecipeText',{get:function(){Reads++;throw new Error('prototype recipe getter');}});
          F[0]=Object.create(p);
        end;
        26:asm F[0].ResourceIds={length:1,0:'one.resource0'}; end;
        27:asm F[0].PassLabels={length:1,0:'one.pass0'}; end;
        28:asm F[0].PassLabels=null; end;
        29:asm delete F[0].ResourceIds[0]; end;
        30:asm delete F[0].PassLabels[0]; end;
        31:asm Object.defineProperty(F[0].ResourceIds,'0',{get:function(){Reads++;return 'one.resource0';}}); end;
        32:asm Object.defineProperty(F[0].PassLabels,'0',{get:function(){Reads++;return 'one.pass0';}}); end;
        33:asm
          const p=Object.create(Array.prototype);p[0]='one.resource0';
          delete F[0].ResourceIds[0];Object.setPrototypeOf(F[0].ResourceIds,p);
        end;
        34:asm
          const p=Object.create(Array.prototype);
          Object.defineProperty(p,'0',{get:function(){Reads++;return 'one.pass0';}});
          delete F[0].PassLabels[0];Object.setPrototypeOf(F[0].PassLabels,p);
        end;
        35:asm F[0].ResourceIds[0]=null; end;
        36:asm F[0].PassLabels[0]=new String('one.pass0'); end;
        37:asm F[0].ResourceIds[0]={toString:function(){Reads++;return 'one.resource0';}}; end;
        38:asm F[0].PassLabels[0]={valueOf:function(){Reads++;return 'one.pass0';}}; end;
        39:asm F[0].RecipeText={toString:function(){Reads++;return 'wfcpipeline=1';}}; end;
        40:asm F=new Array(2147483648); end;
        41:asm F[0].ResourceIds=new Array(2147483648); end;
        42:asm F[0].PassLabels=new Array(2147483648); end;
        43:asm F[0].ResourceIds=new Uint32Array([0]); end;
        44:asm F[0].PassLabels=new Uint32Array([0]); end;
        45:asm F[0].PassLabels[0]=Symbol('one.pass0'); end;
        46:asm F[0].FragmentId=Symbol('one'); end;
        47:asm M.Name=Symbol('composed'); end;
        48:asm F[0].FragmentId='\ud800'; end;
        49:asm F[0].ResourceIds[0]='\udc00'; end;
        50:asm F[0].PassLabels[0]='\ud800x'; end;
        51:asm M.SourceDescription='\ud800'; end;
        52:asm F[0].RecipeText='\u00e9'; end;
        53:asm F[0].RecipeText=NaN; end;
        54:asm F[0].PassLabels[0]=NaN; end;
        55:asm F[0].ResourceIds=undefined; end;
        56:asm F[0].PassLabels=undefined; end;
        57:asm F[0].ResourceIds=[];Object.defineProperty(F[0].ResourceIds,'0',{get:function(){Reads++;return 'one.resource0';}}); end;
        58:asm Object.defineProperty(F[0],'FragmentId',{get:function(){Reads++;return 'one';}}); end;
        59:asm Object.defineProperty(M,'SourceFingerprint',{get:function(){Reads++;return '';}}); end;
        60:asm F=new Uint8Array(1); end;
      end;
      RejectRaw(IntToStr(I));
    end;
    Check(Existing.RecipeText=Saved,'all hostile constructor failures leave existing owner unchanged');
    for I:=0 to 10 do
    begin
      asm BadIndex=[NaN,Infinity,-Infinity,0.5,'0',null,undefined,true,{},[],new Number(0)][I]; end;
      for J:=0 to 2 do
      begin
        Saw:=False;
        try
          case J of
            0:Existing.FragmentAt(BadIndex);
            1:Existing.MapIndex('one',TWfcPipelineComposeSection(BadIndex),0);
            2:Existing.MapIndex('one',wpcosPass,BadIndex);
          end;
        except on E: EWfcPipelineCompose do Saw:=Pos('exact finite Integer',E.Message)>0; end;
        Check(Saw,'raw JavaScript index is typed before use '+IntToStr(I)+'/'+IntToStr(J));
      end;
    end;
    for I:=0 to 9 do
    begin
      Reads:=0;
      asm BadToken=[NaN,1,null,undefined,true,[],{},new String('one'),Symbol('one'),
        {toString:function(){Reads++;return 'one';},valueOf:function(){Reads++;return 'one';}}][I]; end;
      for J:=0 to 4 do
      begin
        Saw:=False;
        try
          case J of
            0:Existing.MapIndex(BadToken,wpcosPass,0);
            1:Existing.ResolvePass(BadToken,'root');
            2:Existing.ResolveResource(BadToken,'rules');
            3:Existing.ResolvePass('one',BadToken);
            4:Existing.ResolveResource('one',BadToken);
          end;
        except on E: EWfcPipelineCompose do Saw:=Pos('must be a string',E.Message)>0; end;
        Check(Saw,'raw JavaScript lookup token typed '+IntToStr(I)+'/'+IntToStr(J));
        Check(Reads=0,'lookup does not coerce token');
      end;
    end;
    ResetRawInputs;
    asm
      M={Name:M.Name,LicenseIdentifier:M.LicenseIdentifier,SourceDescription:M.SourceDescription,SourceFingerprint:M.SourceFingerprint};
      F=[{FragmentId:F[0].FragmentId,RecipeText:F[0].RecipeText,ResourceIds:F[0].ResourceIds,PassLabels:F[0].PassLabels}];
    end;
    C:=TWfcPipelineComposition.Create(M,F);
    try Check(C.RecipeText=Saved,'ordinary passive JavaScript data records supported'); finally C.Free; end;
    ResetRawInputs;
    asm
      M=Object.create({Name:M.Name,LicenseIdentifier:M.LicenseIdentifier,SourceDescription:M.SourceDescription,SourceFingerprint:M.SourceFingerprint});
      F=[Object.create({FragmentId:F[0].FragmentId,RecipeText:F[0].RecipeText,ResourceIds:F[0].ResourceIds,PassLabels:F[0].PassLabels})];
    end;
    C:=TWfcPipelineComposition.Create(M,F);
    try Check(C.RecipeText=Saved,'passive inherited record defaults supported, unlike inherited array slots'); finally C.Free; end;
    Check((Existing.FragmentCount=1) and (Existing.RecipeText=Saved),'hostile lookup failures preserve owner');
  finally Existing.Free; A.Free; end;
end;
{$ENDIF}

begin
  TestRemappingAndOwnership;
  TestRejectionsAndLegacy;
  TestOptionalMetadata;
  {$IFDEF PAS2JS}TestHostileJavaScript;{$ENDIF}
  WriteLn('Private immutable composer: ',Checks,' checks passed.');
end.
