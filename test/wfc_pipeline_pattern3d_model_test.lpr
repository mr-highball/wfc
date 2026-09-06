{ SPDX-License-Identifier: MIT
  Copyright (c) 2021 mr-highball }
program wfc_pipeline_pattern3d_model_test;
{$mode delphi}{$H+}
uses {$IFDEF PAS2JS}wfc_browser_test_host,{$ENDIF}
  SysUtils, wfc, wfc_model, wfc_model_text, wfc_learn3d, wfc_sequence,
  wfc_pattern3d, wfc_pattern3d_learn, wfc_pattern3d_text, wfc_pipeline_model;

type
  TInputs = record
    Versions: TWfcPipelineVersions;
    Rank: Integer;
    Wrap: Boolean;
    Resources: TWfcPipelineResources;
    Passes: TWfcPipelinePasses;
    Dependencies: TWfcPipelineDependencies;
    Bridges: TWfcPipelineBridges;
    Requirements: TWfcPipelineRequirements;
    Quotas: TWfcPipelineValueQuotas;
    Connectivity: TWfcPipelineConnectivities;
  end;
  TTest = procedure;
var Checks, Failures: Integer;

procedure Check(const OK: Boolean; const MessageText: String);
begin
  Inc(Checks);
  if not OK then begin Inc(Failures); WriteLn('[FAIL] ',MessageText); end;
end;

procedure Run(const Name: String; const Test: TTest);
begin
  WriteLn('[TEST] ',Name);
  try Test; except on E: Exception do
    begin Inc(Failures); WriteLn('[EXCEPTION] ',E.ClassName,': ',E.Message); end; end;
end;

function Tokens(const A: array of TWfcModelToken): TWfcModelTokens;
var I: Integer;
begin
  Result:=nil; SetLength(Result,Length(A));
  for I:=0 to High(A) do Result[I]:=A[I];
end;

function Model(const A: TInputs): TWfcPipelineModel;
begin
  Result:=TWfcPipelineModel.Create(MakeWfcPipelineMetadata('volume','MIT','literal XYZ corpus',''),
    A.Versions,A.Rank,A.Wrap,rmBottomUp,A.Resources,A.Passes,A.Dependencies,
    A.Bridges,A.Requirements,A.Quotas,A.Connectivity);
end;

function Fixture: TInputs;
var P: TWfcOverlappingModel3D; M: TWfcModel; T: TWfcModelTokens;
begin
  Result.Resources:=nil; Result.Passes:=nil; Result.Dependencies:=nil;
  Result.Bridges:=nil; Result.Requirements:=nil; Result.Quotas:=nil; Result.Connectivity:=nil;
  Result.Versions:=CurrentWfcPipelineVersions; Result.Rank:=3; Result.Wrap:=True;
  T:=Tokens(['A','B','C','D','E','F','G','H']);
  P:=LearnOverlappingModel3D(T,2,2,2,2,1,2,wmbOpen,wmsNone);
  M:=nil;
  try
    M:=LearnModel3D(T,2,2,2,wmbWrap,wmsNone);
    SetLength(Result.Resources,2);
    Result.Resources[0]:=MakeWfcPipelineResource('tiles',wprkPattern3D,
      EncodeWfcPattern3DText(P),'literal eight-token volume','MIT','source-1');
    Result.Resources[1]:=MakeWfcPipelineResource('decor',wprkModel,
      EncodeWfcModelText(M),'literal eight-token volume','MIT','source-1');
  finally M.Free; P.Free; end;
  SetLength(Result.Passes,4);
  Result.Passes[0]:=MakeWfcPipelinePass('patterns',wppvPrivate,gpmOverlay,-1,wpakPattern3D,0,False,wseWhole);
  Result.Passes[1]:=MakeWfcPipelinePass('terrain',wppvPublic,gpmOverlay,-1,wpakEmpty,-1,False,wseWhole);
  Result.Passes[2]:=MakeWfcPipelinePass('foliage',wppvPublic,gpmOverlay,-1,wpakModel,1,False,wseWhole);
  Result.Passes[3]:=MakeWfcPipelinePass('copy',wppvPublic,gpmTransform,1,wpakEmpty,-1,False,wseWhole);
  SetLength(Result.Dependencies,3);
  Result.Dependencies[0]:=MakeWfcPipelineDependency(1,0);
  Result.Dependencies[1]:=MakeWfcPipelineDependency(2,1);
  Result.Dependencies[2]:=MakeWfcPipelineDependency(3,1);
  SetLength(Result.Bridges,1);
  Result.Bridges[0]:=MakeWfcPipelineBridge(wpbkPattern3DProjection,0,1);
end;

procedure Reject(const A: TInputs; const Fragment: String);
var M: TWfcPipelineModel; Raised: Boolean;
begin
  M:=nil; Raised:=False;
  try
    try M:=Model(A);
    except on E: EWfcPipelineModel do
      begin Raised:=(Fragment='') or (Pos(Fragment,E.Message)>0);
        if not Raised then WriteLn('[DETAIL] ',E.Message); end;
    end;
    Check(Raised,'reject '+Fragment);
  finally M.Free; end;
end;

procedure TestOwnershipAndPublicSemantics;
var A: TInputs; M,N: TWfcPipelineModel; V: TWfcModelTokens;
  Terms: TWfcPipelineRequirementTerms; Profiles: TWfcPipelineConnectivityValues;
  Positions: TGraphPositions; Root: TGraphPosition; I: Integer; Key: String;
begin
  A:=Fixture; SetLength(Terms,1);
  Terms[0]:=MakeWfcPipelineRequirementTerm(0,-1,1,Tokens(['E']));
  SetLength(A.Requirements,1);
  A.Requirements[0]:=MakeWfcPipelineRequirement(2,'A',1,wprqExact,Terms);
  SetLength(A.Quotas,1);
  A.Quotas[0]:=MakeWfcPipelineValueQuota(3,'stone',Tokens(['A','E']),0,8);
  Root.X:=0; Root.Y:=0; Root.Z:=1; Positions:=nil;
  SetLength(Profiles,2);
  Profiles[0]:=MakeWfcPipelineConnectivityValue('A',[gdUp,gdDown],False);
  Profiles[1]:=MakeWfcPipelineConnectivityValue('E',[gdUp,gdDown],True);
  SetLength(A.Connectivity,1);
  A.Connectivity[0]:=MakeWfcPipelineConnectivity(1,'shaft',Root,Positions,Profiles,False);
  M:=Model(A); N:=nil;
  try
    Check(M.HasPattern3D and (M.Rank=3) and M.WrapNeighbors,'explicit 3D recipe identity');
    Check((Ord(wprkSequence)=3) and (Ord(wpakSequence)=4) and
      (Ord(wpbkSequenceProjection)=1),'legacy enum ordinals unchanged');
    Check((M.CopyVersions.Pattern3DGraphAdapterVersion=1) and
      (M.CopyVersions.Pattern3DBridgeVersion=1),'new adapter and inverse bridge version1');
    Check((M.BorrowPattern3DResource(0).PatternWidth=2) and
      (M.BorrowPattern3DResource(0).PatternHeight=1) and
      (M.BorrowPattern3DResource(0).PatternDepth=2),'XYZ footprint survives typed ownership');
    Check(M.BorrowPattern3DResource(0).SourceBoundary=wmbOpen,'source open boundary independent of output wrap');
    V:=M.CopyPublicVocabulary(1);
    Check(Length(V)=8,'all palette tokens exposed despite incomplete offset coverage');
    for I:=0 to 7 do Check(V[I]=TWfcModelToken(Chr(Ord('A')+I)),'palette order retained');
    Check((M.CopyPublicVocabulary(3)[4]='E') and
      (M.RequirementAt(0).Terms[0].OffsetZ=1),'transform and downstream fullXYZ requirement');
    Check((M.ValueQuotaAt(0).PassIndex=3) and (M.ConnectivityAt(0).Root.Z=1) and
      (M.ConnectivityAt(0).Values[1].Openings=[gdUp,gdDown]),'public alias quotas and vertical connectivity');
    Key:=M.BorrowPattern3DResource(0).PatternKeyAt(0);
    V[0]:='changed'; A.Resources[0].Document:='changed';
    A.Quotas[0].Values[0]:='changed'; A.Connectivity[0].Values[0].Value:='changed';
    Check((M.CopyPublicVocabulary(1)[0]='A') and (M.ValueQuotaAt(0).Values[0]='A') and
      (M.ConnectivityAt(0).Values[0].Value='A') and
      (M.BorrowPattern3DResource(0).PatternKeyAt(0)=Key),'all managed inputs detached');
    WriteLn('[INFO] full-volume recipe signature ',WfcPipelineSignatureHex(M.Signature));
    Check(WfcPipelineSignatureHex(M.Signature)='F85EEF40','full feature-domain signature golden');
    A.Resources:=M.CopyResources; A.Passes:=M.CopyPasses; A.Dependencies:=M.CopyDependencies;
    A.Bridges:=M.CopyBridges; A.Requirements:=M.CopyRequirements; A.Quotas:=M.CopyValueQuotas;
    A.Connectivity:=M.CopyConnectivities;
    N:=Model(A); Check(N.Signature=M.Signature,'detached reconstruction preserves semantic identity');
    N.Free; N:=nil;
    A.Resources[0].Document:=StringReplace(A.Resources[0].Document,'t=5,F','t=5,Q',[]);
    N:=Model(A);
    Check((N.Signature<>M.Signature) and (N.BorrowPattern3DResource(0).PatternKeyAt(0)<>Key),
      'actual token payload changes exact private key and recipe signature');
  finally N.Free; M.Free; end;
end;

procedure TestGuards;
var A: TInputs; P: TWfcOverlappingModel3D; M: TWfcPipelineModel; Raised: Boolean; I: Integer;
begin
  A:=Fixture; A.Rank:=2; Reject(A,'rank 3');
  A:=Fixture; A.Wrap:=False; Reject(A,'wrapped rank-3');
  A:=Fixture; A.Passes[0].Visibility:=wppvPublic; Reject(A,'private');
  A:=Fixture; A.Passes[0].ResourceIndex:=1; Reject(A,'pattern3d resource');
  A:=Fixture; A.Passes[0].HasSequenceExtent:=True; Reject(A,'sequence extent');
  A:=Fixture; A.Passes[0].SequenceExtent:=wseWrap; Reject(A,'sequence extent');
  A:=Fixture; A.Passes[1].AdapterKind:=wpakModel; A.Passes[1].ResourceIndex:=1;
  Reject(A,'empty public overlay');
  A:=Fixture; A.Dependencies[0].ProviderPassIndex:=2; Reject(A,'cycle');
  A:=Fixture; A.Dependencies:=nil; Reject(A,'requires dependency');
  A:=Fixture; A.Bridges[0].SourcePassIndex:=2; Reject(A,'requires dependency');
  A:=Fixture; A.Bridges[0].TargetPassIndex:=0; Reject(A,'source pass');
  A:=Fixture; A.Resources[0].Document:=StringReplace(A.Resources[0].Document,'wfcp=2','wfcp=1',[]);
  Reject(A,'cannot be decoded');
  A:=Fixture; A.Resources[0].Kind:=wprkPattern2D; Reject(A,'cannot be decoded');
  A:=Fixture; A.Resources[1].Kind:=wprkPattern3D; Reject(A,'cannot be decoded');
  for I:=0 to 2 do
  begin
    A:=Fixture; A.Versions.Pattern3DBridgeVersion:=I;
    if I<>1 then Reject(A,'pattern3d bridge version');
    A:=Fixture; A.Versions.Pattern3DGraphAdapterVersion:=I;
    if I<>1 then Reject(A,'pattern3d graph-adapter version');
  end;
  A:=Fixture; P:=LearnOverlappingModel3D(Tokens(['@p3v1;forbidden']),1,1,1,1,1,1,wmbOpen,wmsNone);
  try A.Resources[0].Document:=EncodeWfcPattern3DText(P); finally P.Free; end;
  Reject(A,'reserved latent-key');
  A:=Fixture; M:=Model(A);
  try
    Raised:=False; try M.BorrowPattern3DResource(1); except on E: EWfcPipelineModel do Raised:=True; end;
    Check(Raised,'typed borrow rejects wrong resource kind');
    Raised:=False; try M.BorrowPattern3DResource(-1); except on E: EWfcPipelineModel do Raised:=True; end;
    Check(Raised,'typed borrow rejects invalid index');
  finally M.Free; end;
  A:=Fixture; SetLength(A.Bridges,0); SetLength(A.Dependencies,0); SetLength(A.Passes,1);
  A.Wrap:=False; M:=Model(A);
  try Check(M.HasPattern3D,'private-only open 3D adapter is independent of bridge limitation'); finally M.Free; end;
  A:=Fixture; SetLength(A.Bridges,0); SetLength(A.Dependencies,0); SetLength(A.Passes,1);
  A.Passes[0]:=MakeWfcPipelinePass('unused',wppvPrivate,gpmOverlay,-1,wpakEmpty,-1,False,wseWhole);
  M:=Model(A);
  try Check(M.HasPattern3D,'unused typed3D resource still selects explicit feature identity'); finally M.Free; end;
end;

procedure TestLegacyNormalization;
var A: TInputs; M,N: TWfcPipelineModel;
begin
  A:=Fixture; A.Resources:=nil; A.Dependencies:=nil; A.Bridges:=nil;
  SetLength(A.Passes,1);
  A.Passes[0]:=MakeWfcPipelinePass('empty',wppvPrivate,gpmOverlay,-1,wpakEmpty,-1,False,wseWhole);
  M:=Model(A); N:=nil;
  try
    A.Versions.Pattern3DGraphAdapterVersion:=Low(Integer);
    A.Versions.Pattern3DBridgeVersion:=High(Integer);
    {$IFDEF PAS2JS}
    asm
      Object.defineProperty(A.Versions,'Pattern3DGraphAdapterVersion',
        {get:function(){throw new Error('legacy read of new graph version');}});
      Object.defineProperty(A.Versions,'Pattern3DBridgeVersion',
        {get:function(){throw new Error('legacy read of new bridge version');}});
    end;
    {$ENDIF}
    N:=Model(A);
    Check(not N.HasPattern3D and (N.Signature=M.Signature),'legacy ignores appended caller fields without reading');
    Check((N.CopyVersions.Pattern3DGraphAdapterVersion=1) and
      (N.CopyVersions.Pattern3DBridgeVersion=1),'legacy copies normalize new fields deterministically');
  finally N.Free; M.Free; end;
end;

{$IFDEF PAS2JS}
procedure TestHostileJS;
var A: TInputs; I,J: Integer; Bad: Integer;
begin
  for I:=0 to 9 do
  begin
    asm Bad=[NaN,Infinity,-Infinity,0.5,'1',null,undefined,true,{},[]][I]; end;
    for J:=0 to 7 do
    begin
      A:=Fixture;
      case J of
        0:A.Versions.Pattern3DGraphAdapterVersion:=Bad;
        1:A.Versions.Pattern3DBridgeVersion:=Bad;
        2:A.Rank:=Bad;
        3:A.Passes[0].ResourceIndex:=Bad;
        4:A.Bridges[0].SourcePassIndex:=Bad;
        5:A.Dependencies[0].ProviderPassIndex:=Bad;
        6:asm A.Wrap=Bad; end;
        7:asm A.Passes[0].HasSequenceExtent=Bad; end;
      end;
      if (I=7) and (J=6) then Continue;
      Reject(A,'');
    end;
  end;
end;
{$ENDIF}

begin
  Run('XYZ ownership and public semantic constraints',TestOwnershipAndPublicSemantics);
  Run('typed resource and adapter guards',TestGuards);
  Run('legacy appended-field normalization',TestLegacyNormalization);
  {$IFDEF PAS2JS}Run('typed-JS hostile new fields',TestHostileJS);{$ENDIF}
  WriteLn('Checks: ',Checks,'  Failures: ',Failures);
  if Failures<>0 then begin
    {$IFDEF PAS2JS}raise Exception.Create('pattern3d pipeline model checks failed');{$ELSE}Halt(1);{$ENDIF}
  end;
end.
