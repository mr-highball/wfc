{ SPDX-License-Identifier: MIT
  Copyright (c) 2021 mr-highball }
program wfc_pipeline_mapped_text_test;
{$mode delphi}{$H+}
uses {$IFDEF PAS2JS}wfc_browser_test_host,{$ENDIF} SysUtils,wfc,wfc_lattice,
  wfc_model,wfc_rule_model,wfc_rule_text,wfc_sequence,wfc_pattern3d,
  wfc_pattern3d_learn,wfc_pattern3d_text,wfc_pipeline_layout,
  wfc_pipeline_model,wfc_pipeline_text;
var Checks:Integer;
procedure Check(const OK:Boolean; const Name:String);
begin Inc(Checks); if not OK then raise Exception.Create(Name); end;
function Tokens(const A:array of TWfcModelToken):TWfcModelTokens;
var I:Integer;
begin Result:=nil; SetLength(Result,Length(A)); for I:=0 to High(A) do Result[I]:=A[I]; end;
function Fixture(const Features:Integer; const Spatial:Boolean=True):TWfcPipelineModel;
var Resources:TWfcPipelineResources; Passes:TWfcPipelinePasses;
  Dependencies:TWfcPipelineDependencies; Bridges:TWfcPipelineBridges;
  Requirements:TWfcPipelineRequirements; Topologies:TWfcPipelinePassTopologies;
  Quotas:TWfcPipelineValueQuotas; Connectivities:TWfcPipelineConnectivities;
  Profiles:TWfcPipelineConnectivityValues; Weights:TWfcModelIntegerArray;
  Rules:TWfcRuleModel; Pattern:TWfcOverlappingModel3D;
  Q:TWfcPipelineMappedQuery; I,Provider,Rank:Integer; HasVolume:Boolean;
begin
  HasVolume:=(Features and 4)<>0; Rank:=2; if HasVolume and not Spatial then Rank:=3;
  SetLength(Resources,2); SetLength(Passes,2); SetLength(Topologies,2);
  SetLength(Weights,2); Weights[0]:=1; Weights[1]:=1;
  Rules:=TWfcRuleModel.Create(Rank,Tokens(['clear','tree']),Weights,nil);
  try Resources[0]:=MakeWfcPipelineResource('rules',wprkRules,EncodeWfcRuleText(Rules),'authored','MIT','');
  finally Rules.Free; end;
  Resources[1]:=Resources[0]; Resources[1].Id:='provider';
  for I:=0 to 1 do begin
    Passes[I]:=MakeWfcPipelinePass(TWfcModelToken('p'+IntToStr(I)),wppvPublic,gpmOverlay,-1,wpakRules,I,False,wseWhole);
    Topologies[I]:=MakeWfcPipelinePassTopology(Rank,MakeWfcLatticeVector(-8,4,-2),MakeWfcLatticeVector(1+I,2+I,1),False);
  end;
  Provider:=1;
  if HasVolume then begin
    Pattern:=LearnOverlappingModel3D(Tokens(['clear','tree']),2,1,1,1,1,1,wmbWrap,wmsNone);
    try Resources[1]:=MakeWfcPipelineResource('volume',wprkPattern3D,EncodeWfcPattern3DText(Pattern),'authored','MIT','');
    finally Pattern.Free; end;
    SetLength(Passes,3); SetLength(Topologies,3); SetLength(Bridges,1);
    Passes[1]:=MakeWfcPipelinePass('latent',wppvPrivate,gpmOverlay,-1,wpakPattern3D,1,False,wseWhole);
    Passes[2]:=MakeWfcPipelinePass('public-volume',wppvPublic,gpmOverlay,-1,wpakEmpty,-1,False,wseWhole);
    Topologies[1]:=MakeWfcPipelinePassTopology(3,MakeWfcLatticeVector(3,-4,9),MakeWfcLatticeVector(2,3,4),True);
    Topologies[2]:=Topologies[1];
    Bridges[0]:=MakeWfcPipelineBridge(wpbkPattern3DProjection,1,2); Provider:=2;
  end;
  SetLength(Dependencies,1+Ord(HasVolume));
  Dependencies[0]:=MakeWfcPipelineDependency(0,Provider);
  if HasVolume then Dependencies[1]:=MakeWfcPipelineDependency(2,1);
  if Spatial then begin
    SetLength(Requirements,6);
    for I:=0 to 5 do begin
      Q:=Default(TWfcPipelineMappedQuery); Q.Kind:=TGraphPassMapKind(I div 2); Q.Match:=TGraphPassMapMatch(I mod 2);
      Q.MinimumOffset:=MakeGraphOffset(-3,1,-2);
      if Q.Kind=gpmkRegionCoverage then Q.MaximumOffset:=MakeGraphOffset(5,9,3);
      if Q.Match=gpmmCount then begin Q.MinimumMatches:=0; Q.MaximumMatches:=1; end;
      Q.AllowedProviderTokens:=Tokens(['clear','tree']);
      Requirements[I]:=MakeWfcPipelineMappedRequirement(0,'tree',Provider,Q);
    end;
  end;
  if (Features and 1)<>0 then begin
    SetLength(Quotas,1); Quotas[0]:=MakeWfcPipelineValueQuota(Provider,'quantity',Tokens(['clear']),0,High(Integer));
  end;
  if (Features and 2)<>0 then begin
    SetLength(Connectivities,1); SetLength(Profiles,1);
    Profiles[0]:=MakeWfcPipelineConnectivityValue('clear',[],False);
    Connectivities[0]:=MakeWfcPipelineConnectivity(Provider,'connected',Default(TGraphPosition),nil,Profiles,False);
  end;
  if Spatial then Result:=TWfcPipelineModel.Create(MakeWfcPipelineMetadata('codec study','MIT','',''),
    CurrentWfcPipelineVersions,Rank,False,rmBottomUp,Resources,Passes,Dependencies,Bridges,Requirements,Quotas,Connectivities,1,Topologies)
  else Result:=TWfcPipelineModel.Create(MakeWfcPipelineMetadata('codec study','MIT','',''),
    CurrentWfcPipelineVersions,Rank,HasVolume,rmBottomUp,Resources,Passes,Dependencies,Bridges,Requirements,Quotas,Connectivities);
end;
function Rejected(const S:String):Boolean;
var M:TWfcPipelineModel;
begin
  Result:=False; M:=nil;
  try
    try M:=DecodeWfcPipelineModelText(S);
    except
      on E:EConvertError do Result:=(Pos('invalid WFC pipeline text: ',E.Message)=1) and (Length(E.Message)>27);
      on E:Exception do raise Exception.Create('wrong codec exception '+E.ClassName+': '+E.Message);
    end;
  finally M.Free; end;
end;
procedure Mutate(const S,OldText,NewText,Name:String);
begin
  Check(Pos(OldText,S)>0,'mutation target exists: '+Name);
  Check(Rejected(StringReplace(S,OldText,NewText,[rfReplaceAll])),'reject '+Name);
end;
procedure RoundTrips;
var I,J:Integer; M,N:TWfcPipelineModel; S:String; Q:TWfcPipelineRequirement;
begin
  for I:=0 to 7 do begin
    M:=Fixture(I);
    try
      S:=EncodeWfcPipelineModelText(M);
      Check(Copy(S,1,14)='wfcpipeline=5'#10,'explicit spatial recipe5 '+IntToStr(I));
      Check((Pos('pass-mapping-version=1'#10,S)>0) and (Pos('graph-pass-mapping-version=1'#10,S)>0),'separate portable/core pins');
      N:=DecodeWfcPipelineModelText(S);
      try
        Check(N.Signature=M.Signature,'semantic identity round trip');
        Check(EncodeWfcPipelineModelText(N)=S,'full canonical byte round trip');
        Check((N.ValueQuotaCount=M.ValueQuotaCount) and (N.ConnectivityCount=M.ConnectivityCount) and (N.HasPattern3D=M.HasPattern3D),'optional feature combination preserved');
        for J:=0 to 5 do begin Q:=N.RequirementAt(J);
          Check((Q.Kind=wprqMapped) and (Ord(Q.MappedQuery.Kind)=J div 2) and (Ord(Q.MappedQuery.Match)=J mod 2)
            and (Q.MappedQuery.MinimumOffset.DeltaZ=-2) and (Length(Q.MappedQuery.AllowedProviderTokens)=2),'mapped variant exact round trip');
        end;
      finally N.Free; end;
    finally M.Free; end;
    M:=Fixture(I,False);
    try
      S:=EncodeWfcPipelineModelText(M); N:=DecodeWfcPipelineModelText(S);
      try Check(not N.HasPassMapping and (EncodeWfcPipelineModelText(N)=S),'legacy format stays legacy');
      finally N.Free; end;
      if I=0 then Check(WfcPipelineModelTextVersion(M)=1,'legacy1 unchanged');
      if I=1 then Check(WfcPipelineModelTextVersion(M)=2,'legacy2 unchanged');
      if I=2 then Check(WfcPipelineModelTextVersion(M)=3,'legacy3 unchanged');
      if I>=4 then Check(WfcPipelineModelTextVersion(M)=4,'legacy4 unchanged');
    finally M.Free; end;
  end;
end;
procedure Malformed;
var M:TWfcPipelineModel; S:String;
begin
  M:=Fixture(0); try S:=EncodeWfcPipelineModelText(M); finally M.Free; end;
  Mutate(S,'wfcpipeline=5','wfcpipeline=6','future format');
  Mutate(S,'pass-mapping-version=1','pass-mapping-version=2','future mapping pin');
  Mutate(S,'graph-pass-mapping-version=1','graph-pass-mapping-version=0','wrong core pin');
  Mutate(S,'pattern3d-present=false','pattern3d-present=true','false volume presence');
  Mutate(S,'pass-topologies=2','pass-topologies=1','missing topology row');
  Mutate(S,'pass-topologies=2','pass-topologies=3','extra topology count');
  Mutate(S,'pass-topology=1,','pass-topology=0,','duplicate topology index');
  Mutate(S,'pass-topology=0,2,-8,4,-2,1,2,1,false','pass-topology=0,2,-08,4,-2,1,2,1,false','noncanonical signed origin');
  Mutate(S,'pass-topology=0,2,-8,4,-2,1,2,1,false','pass-topology=0,2,2147483647,4,-2,1,2,1,false','world endpoint overflow');
  Mutate(S,'pass-topology=0,2,-8,4,-2,1,2,1,false','pass-topology=0,2,-8,4,-2,0,2,1,false','zero pitch');
  Mutate(S,'pass-topology=0,2,-8,4,-2,1,2,1,false','pass-topology=0,2,-8,4,-2,1,2,1,true','pass-zero view mismatch');
  Mutate(S,'mapped-v1,0','mapped-v1,1','mapped legacy term payload');
  Mutate(S,'mapped=0,point,all,-3,1,-2,0,0,0,0,0,2','mapped=0,point,all,-3,1,-2,0,0,0,0,1,2','dirty all count');
  Mutate(S,'mapped=1,point,count,-3,1,-2,0,0,0,0,1,2','mapped=1,point,count,-3,1,-2,0,0,0,0,2,2','point coverage count');
  Mutate(S,'mapped=4,region,all,-3,1,-2,5,9,3,0,0,2','mapped=4,region,all,-3,1,-2,5,1,3,0,0,2','empty region axis');
  Mutate(S,'mapped-token=0,1,tree','mapped-token=0,1,clear','duplicate provider token');
  Mutate(S,'mapped-token=0,1,tree','mapped-token=0,0,tree','duplicate token ordinal');
  Mutate(S,'mapped-token=0,1,tree','mapped-token=1,1,tree','wrong token parent');
  Mutate(S,'mapped-token=0,1,tree','mapped-token=0,1,missing','token outside provider');
  Mutate(S,'mapped=0,point,','mapped=0,nearest,','unsupported resampling');
  Mutate(S,'mapped=0,point,all,-3,','mapped=0,point,all,-0,','negative zero alias');
  Mutate(S,'mapped=0,point,all,-3,','mapped=0,point,all,0.5,','fractional offset');
  Check(Rejected(S+'extra'#10),'trailing document data');
  Check(Rejected(StringReplace(S,#10,#13#10,[rfReplaceAll])),'CRLF noncanonical');
end;
begin
  RoundTrips; Malformed; WriteLn('Mapped pipeline text: ',Checks,' checks passed');
end.
