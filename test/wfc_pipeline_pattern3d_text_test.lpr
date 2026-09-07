{ SPDX-License-Identifier: MIT
  Copyright (c) 2021 mr-highball }
program wfc_pipeline_pattern3d_text_test;
{$mode delphi}{$H+}
uses {$IFDEF PAS2JS}wfc_browser_test_host,{$ENDIF}
  SysUtils,wfc,wfc_model,wfc_sequence,wfc_pattern3d,wfc_pipeline_model,
  wfc_pipeline_text,wfc_text_codec;
const
  PATTERN_TEXT =
    'wfcp=2'#10+'rank=3'#10+'samples=1'#10+'s=0,1,1,2'#10+
    'footprint=1,1,2'#10+'boundary=wrap'#10+'symmetry=none'#10+
    'directions=N,E,S,W,U,D'#10+'palette=2'#10+'t=0,A'#10+'t=1,B'#10+
    'patterns=2'#10+'p=0,1,0,1'#10+'p=1,1,1,0'#10+'relations=overlap'#10+'end'#10;
  VERSION_LINES = 'pattern3d-graph-adapter-version=1'#10+'pattern3d-bridge-version=1'#10;
  EMPTY_TAIL = 'value-quota-version=0'#10+'value-quotas=0'#10+
    'connectivity-version=0'#10+'connectivities=0'#10;
  GOLDEN_PREFIX =
    'wfcpipeline=4'#10+'name=volume-codec'#10+'license=MIT'#10+'source='#10+'fingerprint='#10+
    'graph-model-version=1'#10+'random-algorithm-version=1'#10+
    'solver-algorithm-version=2'#10+'pipeline-algorithm-version=2'#10+
    'bundle-graph-adapter-version=1'#10+'model-graph-adapter-version=1'#10+
    'rules-graph-adapter-version=1'#10+'pattern2d-graph-adapter-version=1'#10+
    'sequence-graph-adapter-version=1'#10+'pattern2d-bridge-version=2'#10+
    'sequence-bridge-version=2'#10+VERSION_LINES+'rank=3'#10+'wrap=true'#10+
    'traversal=bottom-up'#10+'resources=1'#10+
    'resource=0,volume,pattern3d,wfcp%3D2%0Arank%3D3%0Asamples%3D1%0As%3D0%2C1%2C1%2C2%0A'+
    'footprint%3D1%2C1%2C2%0Aboundary%3Dwrap%0Asymmetry%3Dnone%0A'+
    'directions%3DN%2CE%2CS%2CW%2CU%2CD%0Apalette%3D2%0At%3D0%2CA%0At%3D1%2CB%0A'+
    'patterns%3D2%0Ap%3D0%2C1%2C0%2C1%0Ap%3D1%2C1%2C1%2C0%0Arelations%3Doverlap%0Aend%0A,literal,MIT,'#10+
    'passes=2'#10+'pass=0,patterns,private,overlay,-1,pattern3d,0,false,whole'#10+
    'pass=1,terrain,public,overlay,-1,empty,-1,false,whole'#10+'dependencies=1'#10+'dependency=0,1,0'#10+
    'bridges=1'#10+'bridge=0,pattern3d-projection,0,1'#10+'requirements=0'#10+EMPTY_TAIL;
type TTest=procedure;
var Checks,Failures:Integer;

procedure Check(const OK:Boolean; const Msg:String);
begin Inc(Checks); if not OK then begin Inc(Failures); WriteLn('[FAIL] ',Msg); end; end;
procedure Run(const Name:String; const Test:TTest);
begin
  WriteLn('[TEST] ',Name);
  try Test; except on E:Exception do begin Inc(Failures); WriteLn('[EXCEPTION] ',E.Message); end; end;
end;
function Tokens(const A:array of TWfcModelToken):TWfcModelTokens;
var I:Integer;
begin Result:=nil; SetLength(Result,Length(A)); for I:=0 to High(A) do Result[I]:=A[I]; end;

function Fixture(const Quota,Connectivity:Boolean):TWfcPipelineModel;
var R:TWfcPipelineResources; P:TWfcPipelinePasses; D:TWfcPipelineDependencies;
  B:TWfcPipelineBridges; Q:TWfcPipelineValueQuotas; C:TWfcPipelineConnectivities;
  Root:TGraphPosition; Positions:TGraphPositions; Profiles:TWfcPipelineConnectivityValues;
begin
  SetLength(R,1); R[0]:=MakeWfcPipelineResource('volume',wprkPattern3D,PATTERN_TEXT,'literal','MIT','');
  SetLength(P,2);
  P[0]:=MakeWfcPipelinePass('patterns',wppvPrivate,gpmOverlay,-1,wpakPattern3D,0,False,wseWhole);
  P[1]:=MakeWfcPipelinePass('terrain',wppvPublic,gpmOverlay,-1,wpakEmpty,-1,False,wseWhole);
  SetLength(D,1); D[0]:=MakeWfcPipelineDependency(1,0);
  SetLength(B,1); B[0]:=MakeWfcPipelineBridge(wpbkPattern3DProjection,0,1);
  Q:=nil; C:=nil;
  if Quota then begin SetLength(Q,1); Q[0]:=MakeWfcPipelineValueQuota(1,'amount',Tokens(['A']),1,8); end;
  if Connectivity then
  begin
    Root.X:=0; Root.Y:=0; Root.Z:=1; SetLength(Positions,1);
    Positions[0].X:=0; Positions[0].Y:=0; Positions[0].Z:=3;
    SetLength(Profiles,2);
    Profiles[0]:=MakeWfcPipelineConnectivityValue('A',[gdUp,gdDown],False);
    Profiles[1]:=MakeWfcPipelineConnectivityValue('B',[gdUp,gdDown],True);
    SetLength(C,1); C[0]:=MakeWfcPipelineConnectivity(1,'vertical',Root,Positions,Profiles,True);
  end;
  Result:=TWfcPipelineModel.Create(MakeWfcPipelineMetadata('volume-codec','MIT','',''),
    3,True,rmBottomUp,R,P,D,B,nil,Q,C);
end;

procedure Reject(const S,Fragment:String);
var M:TWfcPipelineModel; Raised:Boolean;
begin
  M:=nil; Raised:=False;
  try
    try M:=DecodeWfcPipelineModelText(S);
    except on E:Exception do begin Raised:=(Fragment='') or(Pos(Fragment,E.Message)>0);
      if not Raised then WriteLn('[DETAIL] ',E.Message); end;
    end;
    Check(Raised,'reject '+Fragment);
  finally M.Free; end;
end;

function Replace(const S,Old,New:String):String;
begin
  if Pos(Old,S)=0 then raise Exception.Create('mutation fixture text missing: '+Old);
  Result:=StringReplace(S,Old,New,[]);
end;

function Nested(const S,Old,New:String):String;
var T:String;
begin
  T:=Replace(PATTERN_TEXT,Old,New);
  Result:=Replace(S,WfcTextEncodeToken(PATTERN_TEXT,'test'),WfcTextEncodeToken(T,'test'));
end;

procedure TestCanonical;
var M,N:TWfcPipelineModel; S,Expected:String; I,J:Integer;
begin
  for I:=0 to 3 do
  begin
    M:=Fixture((I and 1)<>0,(I and 2)<>0); N:=nil;
    try
      S:=EncodeWfcPipelineModelText(M);
      Check(WfcPipelineModelTextVersion(M)=4,'3D feature selects v4 for every registry combination');
      Check(Pos(VERSION_LINES,S)>0,'both explicit new versions in canonical order');
      Check((Pos('pattern3d-projection',S)>0) and(Pos('wfcp%3D2',S)>0),'actual nested wfcp2 and bridge names');
      if I=0 then
      begin
        Expected:=GOLDEN_PREFIX+'signature='+WfcPipelineSignatureHex(M.Signature)+#10+'end'#10;
        Check(S=Expected,'exact full canonical layout independently spelled out');
        Check(Pos(EMPTY_TAIL,S)>0,'empty optional registries encode zero/zero');
        WriteLn('[INFO] minimal-volume recipe signature ',WfcPipelineSignatureHex(M.Signature));
        Check(WfcPipelineSignatureHex(M.Signature)='B65F7944','minimal feature-domain signature golden');
      end;
      N:=DecodeWfcPipelineModelText(S);
      Check((N.Signature=M.Signature) and N.HasPattern3D and
        (EncodeWfcPipelineModelText(N)=S),'byte-exact v4 round trip and semantic identity');
      Check((N.ValueQuotaCount=(I and 1)) and(N.ConnectivityCount=((I shr 1) and 1)),
        'optional registries reconstruct exact counts');
      if I>=2 then Check((N.ConnectivityAt(0).Root.Z=1) and
        (N.ConnectivityAt(0).RequiredPositions[0].Z=3),'connectivity full Z coordinates retained');
      for J:=0 to 1 do Check(N.BorrowPattern3DResource(0).PatternKeyAt(J)=
        M.BorrowPattern3DResource(0).PatternKeyAt(J),'exact token-payload identity survives nesting');
      if I=3 then
      begin
        Reject(Replace(S,'value-quota-version=1','value-quota-version=0'),'zero exactly');
        Reject(Replace(S,'connectivity-version=1','connectivity-version=0'),'zero exactly');
        Reject(Replace(S,'quota-token=0,0,A','quota-token=0,0,unknown'),'public vocabulary');
        Reject(Replace(S,'terminal=0,0,0,0,3','terminal=0,0,0,0,1'),'repeats the root');
      end;
    finally N.Free; M.Free; end;
  end;
end;

procedure TestStrict;
var M:TWfcPipelineModel; S,T:String; I:Integer;
begin
  M:=Fixture(False,False);
  try S:=EncodeWfcPipelineModelText(M); finally M.Free; end;
  Reject(Replace(S,'wfcpipeline=4','wfcpipeline=04'),'version');
  Reject(Replace(S,'wfcpipeline=4','wfcpipeline='+
    IntToStr(WFC_PIPELINE_MAX_SUPPORTED_TEXT_VERSION+1)),'version');
  Reject(Replace(S,'wfcpipeline=4','wfcpipeline=5'),'pattern3d presence');
  Reject(Replace(S,VERSION_LINES,''),'pattern3d graph-adapter');
  Reject(Replace(S,VERSION_LINES,'pattern3d-bridge-version=1'#10+'pattern3d-graph-adapter-version=1'#10),
    'pattern3d graph-adapter');
  for I:=0 to 2 do if I<>1 then
  begin
    Reject(Replace(S,'pattern3d-graph-adapter-version=1','pattern3d-graph-adapter-version='+IntToStr(I)),
      'pattern3d graph-adapter version');
    Reject(Replace(S,'pattern3d-bridge-version=1','pattern3d-bridge-version='+IntToStr(I)),
      'pattern3d bridge version');
  end;
  Reject(Replace(S,'pattern3d-bridge-version=1','pattern3d-bridge-version=01'),'leading zero');
  Reject(Replace(S,'pattern3d-bridge-version=1','pattern3d-bridge-version=2147483648'),'integer range');
  Reject(Replace(S,'value-quota-version=0','value-quota-version=1'),'zero exactly');
  Reject(Replace(S,'connectivity-version=0','connectivity-version=1'),'zero exactly');
  Reject(Replace(S,'connectivity-version=0','connectivity-version=2'),'connectivity version');
  Reject(Replace(S,'connectivities=0','connectivities=4097'),'limit');
  Reject(Replace(S,'rank=3','rank=2'),'rank 3');
  Reject(Replace(S,'wrap=true','wrap=false'),'wrapped rank-3');
  Reject(Replace(S,',pattern3d,',',pattern2d,'),'declared kind');
  Reject(Replace(S,',pattern3d-projection,',',pattern2d-projection,'),'rank-2');
  Reject(Replace(S,'bridge=0,','bridge=1,'),'ordered');
  Reject(Replace(S,'resource=0,','resource=1,'),'ordered');
  Reject(Replace(S,'pass=0,patterns,private','pass=0,patterns,public'),'private');
  Reject(Replace(S,'pass=0,patterns,private,overlay,-1,pattern3d,0,false,whole',
    'pass=0,patterns,private,overlay,-1,pattern3d,0,true,whole'),'sequence extent');
  Reject(Nested(S,'wfcp=2','wfcp=1'),'declared kind');
  Reject(Nested(S,'rank=3','rank=2'),'declared kind');
  Reject(Nested(S,'footprint=1,1,2','footprint=1,1,0'),'declared kind');
  Reject(Nested(S,'s=0,1,1,2','s=0,1,1,3'),'declared kind');
  Reject(Nested(S,'p=0,1,0,1','p=0,1,0'),'declared kind');
  Reject(Nested(S,'p=0,1,0,1','p=0,1,0,2'),'declared kind');
  Reject(Nested(S,'p=1,1,1,0','p=1,1,0,1'),'declared kind');
  Reject(Nested(S,'t=0,A','t=0,%41'),'declared kind');
  Reject(Nested(S,'t=0,A','t=0,C'),'signature');
  Reject(Nested(S,'relations=overlap','relations=24'),'declared kind');
  Reject(Replace(S,'signature=','signature=0'),'eight uppercase');
  Reject(S+'end'#10,'trailing'); Reject(Copy(S,1,Length(S)-1),'LF');
  Reject(StringReplace(S,#10,#13#10,[rfReplaceAll]),'format version');
  { Removing V4-only fields cannot smuggle the feature into older versions. }
  T:=Replace(S,VERSION_LINES,'');
  T:=Replace(T,EMPTY_TAIL,'');
  Reject(Replace(T,'wfcpipeline=4','wfcpipeline=1'),'version 4');
end;

procedure TestFeatureSelection;
var M,N:TWfcPipelineModel; P:TWfcPipelinePasses; R:TWfcPipelineResources;
  S:String;
begin
  SetLength(P,1); P[0]:=MakeWfcPipelinePass('empty',wppvPrivate,gpmOverlay,-1,wpakEmpty,-1,False,wseWhole);
  M:=TWfcPipelineModel.Create(MakeWfcPipelineMetadata('legacy','MIT','',''),3,True,rmBottomUp,nil,P,nil,nil,nil);
  N:=nil;
  try
    S:=EncodeWfcPipelineModelText(M);
    Check((WfcPipelineModelTextVersion(M)=1) and(Pos('pattern3d-',S)=0),
      'rank3 alone does not select new format or version material');
    S:=Replace(S,'wfcpipeline=1','wfcpipeline=4');
    S:=Replace(S,'sequence-bridge-version=2'#10,'sequence-bridge-version=2'#10+VERSION_LINES);
    S:=Replace(S,'signature=',EMPTY_TAIL+'signature=');
    Reject(S,'version 4');
    SetLength(R,1); R[0]:=MakeWfcPipelineResource('unused',wprkPattern3D,PATTERN_TEXT,'literal','MIT','');
    N:=TWfcPipelineModel.Create(M.CopyMetadata,3,True,rmBottomUp,R,P,nil,nil,nil);
    S:=EncodeWfcPipelineModelText(N);
    Check(WfcPipelineModelTextVersion(N)=4,'resource-only feature chooses v4');
    M.Free; M:=DecodeWfcPipelineModelText(S);
    Check(M.HasPattern3D and(EncodeWfcPipelineModelText(M)=S),'unused typed resource round-trips');
  finally N.Free; M.Free; end;
end;

begin
  Run('exact v4 canonical layout and optional registries',TestCanonical);
  Run('strict outer and nested documents',TestStrict);
  Run('feature-selected versions',TestFeatureSelection);
  WriteLn('Checks: ',Checks,'  Failures: ',Failures);
  if Failures<>0 then begin
    {$IFDEF PAS2JS}raise Exception.Create('pattern3d pipeline text checks failed');{$ELSE}Halt(1);{$ENDIF}
  end;
end.
