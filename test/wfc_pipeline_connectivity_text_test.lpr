{ SPDX-License-Identifier: MIT
  Copyright (c) 2026 mr-highball }
program wfc_pipeline_connectivity_text_test;
{$mode delphi}{$H+}

uses
  {$IFDEF PAS2JS}wfc_browser_test_host,{$ENDIF}
  SysUtils, wfc, wfc_model, wfc_sequence, wfc_rule_model, wfc_rule_text,
  wfc_pipeline_model, wfc_pipeline_text, wfc_text_codec;

var Checks, Failures: Integer;

procedure Check(const ACondition: Boolean; const AMessage: String);
begin
  Inc(Checks);
  if not ACondition then begin Inc(Failures); WriteLn('FAIL: ', AMessage); end;
end;

function Position(const X, Y, Z: Integer): TGraphPosition;
begin Result.X := X; Result.Y := Y; Result.Z := Z; end;

function NoteToken: TWfcModelToken;
begin
  {$IFDEF PAS2JS}Result := TWfcModelToken(Chr($266B));
  {$ELSE}Result := TWfcModelToken(UTF8Encode(UnicodeString(WideChar($266B))));{$ENDIF}
end;

function Recipe(const ACount: Integer; const AWithQuota: Boolean = False;
  const AProfileCount: Integer = 2; const ATerminalCount: Integer = 2;
  const AUnicode: Boolean = False; const ARank: Integer = 3): TWfcPipelineModel;
var R: TWfcRuleModel; Resources: TWfcPipelineResources; Passes: TWfcPipelinePasses;
  V: TWfcModelTokens; W: TWfcModelIntegerArray; C: TWfcPipelineConnectivities;
  P: TWfcPipelineConnectivityValues; T: TGraphPositions;
  Q: TWfcPipelineValueQuotas; I: Integer; L: TWfcModelToken;
begin
  SetLength(V,AProfileCount); SetLength(W,AProfileCount); SetLength(P,AProfileCount);
  for I := 0 to AProfileCount-1 do
  begin
    V[I] := 'v'+TWfcModelToken(IntToStr(I));
    if AUnicode then V[I] := V[I]+','+NoteToken;
    W[I] := 1;
    if I = 0 then P[I] := MakeWfcPipelineConnectivityValue(V[I],[gdEast,gdWest])
    else P[I] := MakeWfcPipelineConnectivityValue(V[I],[],True);
  end;
  R := TWfcRuleModel.Create(ARank,V,W,nil);
  try
    SetLength(Resources,1);
    Resources[0] := MakeWfcPipelineResource('rules',wprkRules,
      EncodeWfcRuleText(R),'connectivity codec','MIT','');
  finally R.Free; end;
  SetLength(Passes,2);
  Passes[0] := MakeWfcPipelinePass('first',wppvPublic,gpmOverlay,-1,wpakRules,0,False,wseWhole);
  Passes[1] := MakeWfcPipelinePass('second',wppvPublic,gpmOverlay,-1,wpakRules,0,False,wseWhole);
  SetLength(T,ATerminalCount);
  for I := 0 to High(T) do T[I] := Position(I+1,0,0);
  SetLength(C,ACount);
  for I := 0 to High(C) do
  begin
    L := 'c'+TWfcModelToken(IntToStr(I));
    if AUnicode then L := L+','+#10+NoteToken;
    C[I] := MakeWfcPipelineConnectivity(I mod 2,L,Position(0,0,0),T,P,I mod 2=1);
  end;
  Q := nil;
  if AWithQuota then
  begin
    SetLength(Q,1);
    Q[0] := MakeWfcPipelineValueQuota(0,'quantity',V,1,9);
  end;
  Result := TWfcPipelineModel.Create(MakeWfcPipelineMetadata('connectivity-codec','MIT','',''),
    ARank,False,rmBottomUp,Resources,Passes,nil,nil,nil,Q,C);
end;

function ReplaceOnce(const S, AOld, ANew: String): String;
var P: Integer;
begin
  P := Pos(AOld,S);
  if P = 0 then raise Exception.Create('missing fixture text: '+AOld);
  Result := Copy(S,1,P-1)+ANew+Copy(S,P+Length(AOld),Length(S));
end;

procedure Reject(const S, AMessage: String; const AReason: String = '');
var M: TWfcPipelineModel; Rejected: Boolean;
begin
  M := nil; Rejected := False;
  try
    try M := DecodeWfcPipelineModelText(S);
    except on E: EConvertError do
      begin
        Rejected := (AReason='') or (Pos(AReason,E.Message)>0);
        if not Rejected then WriteLn('Unexpected reason: ',E.Message);
      end;
    end;
  finally M.Free; end;
  Check(Rejected,AMessage);
end;

procedure TestCanonical;
const SECTION =
  'value-quota-version=0'#10'value-quotas=0'#10+
  'connectivity-version=1'#10'connectivities=2'#10+
  'connectivity=0,0,c0,0,0,0,false,2,2'#10+
  'terminal=0,0,1,0,0'#10'terminal=0,1,2,0,0'#10+
  'profile=0,0,v0,10,false'#10'profile=0,1,v1,0,true'#10+
  'connectivity=1,1,c1,0,0,0,true,2,2'#10+
  'terminal=1,0,1,0,0'#10'terminal=1,1,2,0,0'#10+
  'profile=1,0,v0,10,false'#10'profile=1,1,v1,0,true'#10;
var M,D,Base,Q: TWfcPipelineModel; S,Expected: String; C: TWfcPipelineConnectivity;
begin
  M := Recipe(2); Base := Recipe(0); D := nil; Q := nil;
  try
    S := EncodeWfcPipelineModelText(M);
    Expected := ReplaceOnce(EncodeWfcPipelineModelText(Base),'wfcpipeline=1'#10,'wfcpipeline=3'#10);
    Expected := ReplaceOnce(Expected,'signature='+WfcPipelineSignatureHex(Base.Signature),
      SECTION+'signature='+WfcPipelineSignatureHex(M.Signature));
    Check(S=Expected,'entire document has exact version-three section and ordered children');
    Check(WfcPipelineSignatureHex(Base.Signature)='B16949C8','legacy fixture signature pinned');
    Check(WfcPipelineSignatureHex(M.Signature)='597320BF','connectivity fixture signature pinned');
    Check((WfcPipelineModelTextVersion(Base)=1) and (Pos('connectivity',
      Copy(EncodeWfcPipelineModelText(Base),Pos('requirements=',EncodeWfcPipelineModelText(Base)),1000))=0),
      'empty extension preserves original format');
    D := DecodeWfcPipelineModelText(S);
    Check((D.Signature=M.Signature) and (EncodeWfcPipelineModelText(D)=S), 'exact v3 roundtrip');
    C := D.ConnectivityAt(0); C.Values[0].Value := 'changed'; C.RequiredPositions[0].X := 900;
    Check(EncodeWfcPipelineModelText(D)=S,'decoded descriptor children are detached');
    Check((D.ConnectivityVersion=1) and (D.ConnectivityCount=2) and (D.ValueQuotaVersion=0),
      'independent opt-in capability versions');
    WriteLn('Connectivity codec signatures: legacy=',WfcPipelineSignatureHex(Base.Signature),
      ' v3=',WfcPipelineSignatureHex(M.Signature));
    Q := Recipe(2,True);
    Check((Pos('value-quota-version=1'#10,EncodeWfcPipelineModelText(Q))>0) and
      (Q.Signature<>M.Signature),'quota and connectivity coexist in version3');
    D.Free; D := nil; D := DecodeWfcPipelineModelText(EncodeWfcPipelineModelText(Q));
    Check((D.ValueQuotaCount=1) and (D.ConnectivityCount=2),'both extensions survive decoding');
    Q.Free; Q := nil; Q := Recipe(0,True);
    Check(WfcPipelineModelTextVersion(Q)=2,'quota-only recipe remains version2');
  finally Q.Free; D.Free; Base.Free; M.Free; end;
  M := Recipe(1,False,2,0,True); D := nil;
  try
    S := EncodeWfcPipelineModelText(M);
    Check((Pos('c0%2C%0A%E2%99%AB',S)>0) and (Pos('v0%2C%E2%99%AB',S)>0),
      'public Unicode and punctuation identity is canonical UTF8');
    D := DecodeWfcPipelineModelText(S);
    Check(EncodeWfcPipelineModelText(D)=S,'Unicode and empty terminal arrays roundtrip');
  finally D.Free; M.Free; end;
end;

procedure TestHostile;
const BAD: array[0..9] of String = ('-1','+1','01',' 1','1 ','1.0','NaN','Infinity','2147483648','');
var M: TWfcPipelineModel; S,T: String; I: Integer;
begin
  M := Recipe(2); try S := EncodeWfcPipelineModelText(M); finally M.Free; end;
  for I := 0 to High(BAD) do
  begin
    Reject(ReplaceOnce(S,'connectivities=2','connectivities='+BAD[I]),'exact connectivity count');
    Reject(ReplaceOnce(S,'terminal=0,0,1,0,0','terminal=0,0,'+BAD[I]+',0,0'),'exact terminal coordinate');
    Reject(ReplaceOnce(S,'profile=0,0,v0,10,false','profile=0,0,v0,'+BAD[I]+',false'),'exact opening mask');
  end;
  Reject(ReplaceOnce(S,'wfcpipeline=3','wfcpipeline='+
    IntToStr(WFC_PIPELINE_MAX_SUPPORTED_TEXT_VERSION+1)),'unknown envelope rejected');
  Reject(ReplaceOnce(S,'wfcpipeline=3','wfcpipeline=4'),
    'v4 cannot be selected by relabeling a connectivity-only document');
  Reject(ReplaceOnce(S,'wfcpipeline=3','wfcpipeline=2'),'v2 does not carry connectivity');
  Reject(ReplaceOnce(S,'wfcpipeline=3','wfcpipeline=1'),'v1 does not carry extensions');
  Reject(ReplaceOnce(S,'connectivity-version=1','connectivity-version=0'),'nonempty capability required');
  Reject(ReplaceOnce(S,'connectivity-version=1','connectivity-version=2'),'unknown connectivity version');
  Reject(ReplaceOnce(S,'value-quota-version=0','value-quota-version=1'),'empty quota version must be0');
  Reject(ReplaceOnce(S,'connectivities=2','connectivities=0'),'v3 registry cannot be empty');
  Reject(ReplaceOnce(S,'connectivity=0,0,c0','connectivity=1,0,c0'),'ordered descriptor indices');
  Reject(ReplaceOnce(S,'connectivity=0,0,c0','connectivity=0,2,c0'),'valid public owner');
  Reject(ReplaceOnce(S,'connectivity=1,1,c1','connectivity=1,0,c0'),'unique pass-label keys');
  Reject(ReplaceOnce(S,'connectivity=0,0,c0','connectivity=0,0,'),'nonempty label');
  Reject(ReplaceOnce(S,'false,2,2','False,2,2'),'strict all-participants Boolean');
  Reject(ReplaceOnce(S,'false,2,2','0,2,2'),'numeric Boolean rejected');
  Reject(ReplaceOnce(S,'false,2,2','false,2,2,0'),'extra descriptor field');
  Reject(ReplaceOnce(S,'false,2,2','false,2'),'missing descriptor field');
  Reject(ReplaceOnce(S,'terminal=0,0,1,0,0','terminal=1,0,1,0,0'),'terminal parent indices');
  Reject(ReplaceOnce(S,'terminal=0,0,1,0,0','terminal=0,1,1,0,0'),'terminal ordered indices');
  Reject(ReplaceOnce(S,'terminal=0,1,2,0,0','terminal=0,1,1,0,0'),'duplicate terminal rejected');
  Reject(ReplaceOnce(S,'terminal=0,1,2,0,0','terminal=0,1,0,0,0'),'redundant root terminal rejected');
  Reject(ReplaceOnce(S,'terminal=0,0,1,0,0','terminal=0,0,3,0,0'),'terminal coordinate canonical order');
  Reject(ReplaceOnce(S,'profile=0,0,v0','profile=1,0,v0'),'profile parent indices');
  Reject(ReplaceOnce(S,'profile=0,1,v1','profile=0,0,v1'),'profile ordered indices');
  Reject(ReplaceOnce(S,'profile=0,1,v1','profile=0,1,v0'),'duplicate profile token');
  Reject(ReplaceOnce(S,'profile=0,0,v0','profile=0,0,unknown'),'unknown profile token');
  Reject(ReplaceOnce(S,'profile=0,0,v0','profile=0,0,'),'nonempty profile token');
  Reject(ReplaceOnce(S,'profile=0,0,v0,10,false','profile=0,0,v0,64,false'),'six-bit opening mask');
  Reject(ReplaceOnce(S,'profile=0,0,v0,10,false','profile=0,0,v0,10,TRUE'),'strict required Boolean');
  Reject(ReplaceOnce(S,'profile=0,0,v0','profile=0,0,%76%30'),'unnecessary escapes rejected');
  Reject(ReplaceOnce(S,'profile=0,0,v0','profile=0,0,%e2%99%ab'),'lowercase escapes rejected');
  Reject(ReplaceOnce(S,'profile=0,0,v0','profile=0,0,%E2%28%A1'),'invalid UTF8 rejected');
  Reject(ReplaceOnce(S,'profile=0,0,v0','profile=0,0,%'),'truncated escape rejected');
  Reject(ReplaceOnce(S,'connectivity-version=1'#10,'connectivity-version=1'#13#10),'CRLF rejected');
  Reject(S+'end'#10,'trailing data rejected');
  Reject(ReplaceOnce(S,'profile=0,0,v0,10,false','profile=0,0,v0,63,false'),
    'valid semantic edits require a new signature','signature does not match');
  M := Recipe(1,True); try T := EncodeWfcPipelineModelText(M); finally M.Free; end;
  Reject(ReplaceOnce(T,'value-quota-version=1','value-quota-version=0'),'nonempty quota requires version1');
  { Every byte truncation must fail, not leak partially parsed ownership. }
  for I := 0 to Length(S)-1 do Reject(Copy(S,1,I),'truncated document rejected');
end;

function ReplaceSection(const S, ASection: String): String;
var A,B: Integer;
begin
  A := Pos('connectivity-version=',S); B := Pos(#10'signature=',S)+1;
  if (A=0) or (B<=A) then raise Exception.Create('missing connectivity section');
  Result := Copy(S,1,A-1)+ASection+Copy(S,B,Length(S));
end;

procedure TestCoordinatesAndPorts;
var Base,M,D: TWfcPipelineModel; C: TWfcPipelineConnectivities; S: String;
begin
  Base:=Recipe(1,False,2,2,False,1); M:=nil; D:=nil;
  try
    S:=EncodeWfcPipelineModelText(Base);
    Reject(ReplaceOnce(S,'c0,0,0,0,false','c0,0,1,0,false'),'rank1 root Y rejected early','outside the recipe rank');
    Reject(ReplaceOnce(S,'terminal=0,0,1,0,0','terminal=0,0,1,0,1'),'rank1 terminal Z rejected early','outside the recipe rank');
    C:=Base.CopyConnectivities;
    C[0].Values[0].Openings:=[gdNorth,gdEast,gdSouth,gdWest,gdUp,gdDown];
    C[0].Root.X:=High(Integer); C[0].RequiredPositions:=nil;
    M:=TWfcPipelineModel.Create(Base.CopyMetadata,Base.CopyVersions,1,True,Base.RunMode,
      Base.CopyResources,Base.CopyPasses,nil,nil,nil,nil,C);
    S:=EncodeWfcPipelineModelText(M); D:=DecodeWfcPipelineModelText(S);
    Check((D.ConnectivityAt(0).Root.X=High(Integer)) and (Pos('v0,63,false',S)>0),
      'shape-independent maximum coordinate and all6ports are canonical even at rank1');
    Check(EncodeWfcPipelineModelText(D)=S,'all opening bits roundtrip');
  finally D.Free; M.Free; Base.Free; end;
  Base:=Recipe(1,False,1,0,False,2);
  try
    S:=EncodeWfcPipelineModelText(Base);
    Reject(ReplaceOnce(S,'c0,0,0,0,false','c0,0,0,1,false'),'rank2 root Z rejected early','outside the recipe rank');
  finally Base.Free; end;
end;

procedure TestCaps;
var M,D: TWfcPipelineModel; S,T,BigLabel: String; L: TWfcTextLines; I,J,N: Integer;
begin
  M := Recipe(2); try S := EncodeWfcPipelineModelText(M); finally M.Free; end;
  Reject(ReplaceOnce(S,'connectivities=2','connectivities=4097'),'registry count cap','connectivity count');
  Reject(ReplaceOnce(S,'connectivities=2','connectivities=4096'),'registry actual rows preflight','records are incomplete');
  Reject(ReplaceOnce(S,'false,2,2','false,65537,2'),'terminal array cap','terminal count');
  Reject(ReplaceOnce(S,'false,2,2','false,2,1025'),'profile array cap','profile count');
  Reject(ReplaceOnce(S,'false,2,2','false,65536,2'),'terminal records preflight','records are incomplete');
  Reject(ReplaceOnce(S,'false,2,2','false,2,1024'),'profile records preflight','records are incomplete');
  Reject(ReplaceOnce(S,'false,2,2','false,2,0'),'profiles must be nonempty');
  Reject('wfcpipeline=1'#10+StringOfChar(#10,WFC_PIPELINE_MAX_TEXT_LINE_COUNT),'legacy line budget','line-count limit');
  Reject('wfcpipeline=2'#10+StringOfChar(#10,WFC_PIPELINE_VALUE_QUOTA_MAX_TEXT_LINE_COUNT),'quota line budget','line-count limit');
  Reject('wfcpipeline=3'#10+StringOfChar(#10,WFC_PIPELINE_CONNECTIVITY_MAX_TEXT_LINE_COUNT),'connectivity line budget','line-count limit');
  T := StringOfChar('x',WFC_PIPELINE_MAX_ENCODED_TOKEN_LENGTH+129);
  Reject(ReplaceOnce(S,'profile=0,0,v0','profile=0,0,'+T),'row cap before splitting','row-length limit');
  T := StringOfChar('x',WFC_PIPELINE_MAX_ENCODED_TOKEN_LENGTH+1);
  Reject(ReplaceOnce(S,'profile=0,0,v0','profile=0,0,'+T),'shared token cap','encoded token-length limit');
  SetLength(L,2+65*1025); L[0]:='connectivity-version=1'; L[1]:='connectivities=65'; N:=2;
  for I:=0 to 64 do
  begin
    L[N]:='connectivity='+IntToStr(I)+',0,c'+IntToStr(I)+',0,0,0,false,0,1024'; Inc(N);
    for J:=0 to 1023 do begin L[N]:='profile='+IntToStr(I)+','+IntToStr(J)+',v0,0,false'; Inc(N); end;
  end;
  T:=WfcTextJoinCanonicalLines(L,'test'); L:=nil;
  Reject(ReplaceSection(S,T),'aggregate profiles before final allocation','aggregate connectivity profile count'); T:='';
  SetLength(L,2+2*(32769+2)); L[0]:='connectivity-version=1'; L[1]:='connectivities=2'; N:=2;
  for I:=0 to 1 do
  begin
    L[N]:='connectivity='+IntToStr(I)+',0,c'+IntToStr(I)+',0,0,0,false,32769,1'; Inc(N);
    for J:=0 to 32768 do begin L[N]:='terminal='+IntToStr(I)+','+IntToStr(J)+','+IntToStr(J+1)+',0,0'; Inc(N); end;
    L[N]:='profile='+IntToStr(I)+',0,v0,0,false'; Inc(N);
  end;
  T:=WfcTextJoinCanonicalLines(L,'test'); L:=nil;
  Reject(ReplaceSection(S,T),'aggregate terminals before final allocation','aggregate connectivity terminal count'); T:='';
  BigLabel:=StringOfChar('x',WFC_PIPELINE_MAX_ENCODED_TOKEN_LENGTH-4);
  SetLength(L,2+17*2); L[0]:='connectivity-version=1'; L[1]:='connectivities=17'; N:=2;
  for I:=0 to 16 do
  begin
    L[N]:='connectivity='+IntToStr(I)+',0,'+BigLabel+IntToStr(I)+',0,0,0,false,0,1'; Inc(N);
    L[N]:='profile='+IntToStr(I)+',0,v0,0,false'; Inc(N);
  end;
  T:=WfcTextJoinCanonicalLines(L,'test'); L:=nil; BigLabel:='';
  Reject(ReplaceSection(S,T),'shared outer byte budget','aggregate outer-token encoding'); T:='';
  M:=Recipe(WFC_PIPELINE_MAX_CONNECTIVITY_COUNT,False,1,0); D:=nil;
  try
    T:=EncodeWfcPipelineModelText(M); D:=DecodeWfcPipelineModelText(T);
    Check((D.ConnectivityCount=4096) and (EncodeWfcPipelineModelText(D)=T),'exact descriptor ceiling roundtrip');
  finally D.Free; M.Free; end;
  M:=Recipe(1,False,1,WFC_PIPELINE_MAX_CONNECTIVITY_REQUIRED_POSITION_COUNT); D:=nil;
  try
    T:=EncodeWfcPipelineModelText(M); D:=DecodeWfcPipelineModelText(T);
    Check((Length(D.ConnectivityAt(0).RequiredPositions)=65536) and (EncodeWfcPipelineModelText(D)=T),
      'exact terminal ceiling roundtrip');
  finally D.Free; M.Free; end;
  M:=Recipe(1,False,WFC_PIPELINE_MAX_CONNECTIVITY_VALUE_COUNT,0); D:=nil;
  try
    T:=EncodeWfcPipelineModelText(M); D:=DecodeWfcPipelineModelText(T);
    Check((Length(D.ConnectivityAt(0).Values)=1024) and (EncodeWfcPipelineModelText(D)=T),
      'exact profile ceiling roundtrip');
  finally D.Free; M.Free; end;
end;

begin
  try TestCanonical; TestHostile; TestCoordinatesAndPorts; TestCaps;
  except on E:Exception do begin Inc(Failures); WriteLn('EXCEPTION: ',E.ClassName,': ',E.Message); end; end;
  WriteLn('Pipeline connectivity codec checks: ',Checks-Failures,'/',Checks);
  if Failures<>0 then Halt(1);
end.
