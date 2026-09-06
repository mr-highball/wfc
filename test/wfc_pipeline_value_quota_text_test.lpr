(*
MIT License

Copyright (c) 2021 mr-highball

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
*)
program wfc_pipeline_value_quota_text_test;
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

function Tokens(const AValues: array of TWfcModelToken): TWfcModelTokens;
var I: Integer;
begin
  Result := nil; SetLength(Result,Length(AValues));
  for I := 0 to High(AValues) do Result[I] := AValues[I];
end;

function NoteToken: TWfcModelToken;
begin
  {$IFDEF PAS2JS}Result := TWfcModelToken(Chr($266B));
  {$ELSE}Result := TWfcModelToken(UTF8Encode(UnicodeString(WideChar($266B))));{$ENDIF}
end;

function Recipe(const AValues: TWfcModelTokens; const AQuotaCount: Integer;
  const ALabelPrefix: TWfcModelToken = 'q'): TWfcPipelineModel;
var R: TWfcRuleModel; Resources: TWfcPipelineResources; Passes: TWfcPipelinePasses;
  Q: TWfcPipelineValueQuotas; Weights: TWfcModelIntegerArray; I: Integer;
begin
  SetLength(Weights,Length(AValues));
  for I := 0 to High(Weights) do Weights[I] := 1;
  R := TWfcRuleModel.Create(1,AValues,Weights,nil);
  try
    SetLength(Resources,1);
    Resources[0] := MakeWfcPipelineResource('rules',wprkRules,
      EncodeWfcRuleText(R),'quota codec test','MIT','');
  finally R.Free; end;
  SetLength(Passes,2);
  Passes[0] := MakeWfcPipelinePass('first',wppvPublic,gpmOverlay,-1,wpakRules,0,False,wseWhole);
  Passes[1] := MakeWfcPipelinePass('second',wppvPublic,gpmOverlay,-1,wpakRules,0,False,wseWhole);
  SetLength(Q,AQuotaCount);
  for I := 0 to High(Q) do Q[I] := MakeWfcPipelineValueQuota(I mod 2,
    ALabelPrefix + TWfcModelToken(IntToStr(I)),AValues,0,2);
  Result := TWfcPipelineModel.Create(MakeWfcPipelineMetadata('quota-codec','MIT','',''),
    1,False,rmBottomUp,Resources,Passes,nil,nil,nil,Q);
end;

function ReplaceOnce(const S, OldText, NewText: String): String;
var P: Integer;
begin
  P := Pos(OldText,S);
  if P = 0 then raise Exception.Create('missing test replacement: ' + OldText);
  Result := Copy(S,1,P-1) + NewText + Copy(S,P+Length(OldText),Length(S));
end;

procedure Reject(const S, AMessage: String; const AReason: String = '');
var M: TWfcPipelineModel; Rejected: Boolean;
begin
  M := nil; Rejected := False;
  try
    try M := DecodeWfcPipelineModelText(S);
    except on E: EConvertError do
      Rejected := (AReason = '') or (Pos(AReason,E.Message) > 0); end;
  finally M.Free; end;
  Check(Rejected,AMessage);
end;

function WithoutQuotas(const M: TWfcPipelineModel): TWfcPipelineModel;
begin
  Result := TWfcPipelineModel.Create(M.CopyMetadata,M.CopyVersions,M.Rank,
    M.WrapNeighbors,M.RunMode,M.CopyResources,M.CopyPasses,M.CopyDependencies,
    M.CopyBridges,M.CopyRequirements);
end;

procedure TestCanonical;
const SECTION =
  'value-quota-version=1'#10'value-quotas=2'#10 +
  'value-quota=0,0,q0,0,2,2'#10 +
  'quota-token=0,0,A'#10'quota-token=0,1,B'#10 +
  'value-quota=1,1,q1,0,2,2'#10 +
  'quota-token=1,0,A'#10'quota-token=1,1,B'#10;
var M, D, Legacy: TWfcPipelineModel; S, L, Expected: String; Q: TWfcPipelineValueQuota;
begin
  Check((WFC_PIPELINE_TEXT_VERSION=1) and (WFC_PIPELINE_MAX_SUPPORTED_TEXT_VERSION=3),
    'legacy version and supported capability are separate');
  Check((WFC_PIPELINE_MAX_TEXT_LINE_COUNT=86618) and
    (WFC_PIPELINE_VALUE_QUOTA_MAX_TEXT_LINE_COUNT=156252), 'version-specific line caps');
  Check(WFC_PIPELINE_MAX_ENCODED_TEXT_LENGTH=268435456, 'encoded byte envelope unchanged');
  M := Recipe(Tokens(['A','B']),2); D := nil; Legacy := nil;
  try
    S := EncodeWfcPipelineModelText(M);
    Check(WfcPipelineModelTextVersion(M)=2, 'quotas select text version two');
    Check(M.ValueQuotaVersion=1, 'explicit quota semantic capability');
    Legacy := WithoutQuotas(M); L := EncodeWfcPipelineModelText(Legacy);
    Check((WfcPipelineModelTextVersion(Legacy)=1) and (Pos('value-quota',L)=0),
      'quota-free recipes have no added fields or header bump');
    Expected := ReplaceOnce(L,'wfcpipeline=1'#10,'wfcpipeline=2'#10);
    Expected := ReplaceOnce(Expected,'signature='+WfcPipelineSignatureHex(Legacy.Signature),
      SECTION+'signature='+WfcPipelineSignatureHex(M.Signature));
    Check(S=Expected, 'entire version-two document uses exact ordered quota section');
    Check(WfcPipelineSignatureHex(Legacy.Signature)='CF6F306E', 'quota-free fixture identity pinned');
    Check(WfcPipelineSignatureHex(M.Signature)='F3F9C14C', 'version-two fixture identity pinned');
    WriteLn('Quota codec signatures: legacy=',WfcPipelineSignatureHex(Legacy.Signature),
      ' v2=',WfcPipelineSignatureHex(M.Signature));
    D := DecodeWfcPipelineModelText(S);
    Check((D.Signature=M.Signature) and (EncodeWfcPipelineModelText(D)=S), 'v2 exact round trip');
    Q := D.ValueQuotaAt(1);
    Check((Q.PassIndex=1) and (Q.LabelText='q1') and (Length(Q.Values)=2), 'quota descriptor decoded');
    Q.Values[0] := 'changed';
    Check(EncodeWfcPipelineModelText(D)=S, 'decoded quota token arrays are detached');
    Reject(ReplaceOnce(S,'value-quota-version=1'#10,'value-quota-version=2'#10),
      'unknown quota semantic version rejected');
    Reject(ReplaceOnce(S,'wfcpipeline=2'#10,'wfcpipeline=4'#10), 'unknown outer version rejected');
    Reject(ReplaceOnce(S,'wfcpipeline=2'#10,'wfcpipeline=1'#10), 'v1 never admits quota sections');
    Reject(ReplaceOnce(L,'wfcpipeline=1'#10,'wfcpipeline=2'#10), 'v2 missing quota section rejected');
    Expected := ReplaceOnce(Expected,SECTION,'value-quota-version=1'#10'value-quotas=0'#10);
    Reject(Expected,'v2 empty extension is noncanonical');
    Reject(ReplaceOnce(S,'signature='+WfcPipelineSignatureHex(M.Signature),'signature=00000000'),
      'changed semantic signature rejected');
    Reject(ReplaceOnce(S,'value-quota=0,0,q0,0,2,2','value-quota=0,0,q0,1,2,2'),
      'changing valid bounds without resigning rejected');
  finally D.Free; Legacy.Free; M.Free; end;
  M := Recipe(Tokens(['A,',NoteToken]),2,'line,'#10+NoteToken); D := nil;
  try
    S := EncodeWfcPipelineModelText(M);
    Check((Pos('quota-token=0,0,A%2C'#10,S)>0) and
      (Pos('quota-token=0,1,%E2%99%AB'#10,S)>0), 'UTF8 and delimiter tokens are escaped');
    Check(Pos('line%2C%0A%E2%99%AB0',S)>0, 'quota label uses canonical UTF8 escaping');
    D := DecodeWfcPipelineModelText(S);
    Check(EncodeWfcPipelineModelText(D)=S, 'Unicode/delimiter quotas round trip');
  finally D.Free; M.Free; end;
end;

procedure TestNestedRequirements;
var Base, M, D: TWfcPipelineModel; Q: TWfcPipelineValueQuotas;
  Dependencies: TWfcPipelineDependencies; Requirements: TWfcPipelineRequirements;
  Terms: TWfcPipelineRequirementTerms; S: String; Rejected: Boolean;
begin
  Rejected := False;
  try WfcPipelineModelTextVersion(nil);
  except on E: EArgumentNilException do Rejected := True; end;
  Check(Rejected, 'text-version inspection rejects nil owners');
  Base := Recipe(Tokens(['A','B']),2); M := nil; D := nil;
  try
    Q := Base.CopyValueQuotas;
    Q[1].LabelText := Q[0].LabelText;
    Q[1].MinimumCount := High(Integer); Q[1].MaximumCount := High(Integer);
    SetLength(Dependencies,1);
    Dependencies[0] := MakeWfcPipelineDependency(1,0);
    SetLength(Terms,1);
    Terms[0] := MakeWfcPipelineRequirementTerm(0,0,0,Tokens(['A','B']));
    SetLength(Requirements,1);
    Requirements[0] := MakeWfcPipelineCountRequirement(1,'A',0,Terms,0,1,gpcmMatchingTerms);
    M := TWfcPipelineModel.Create(Base.CopyMetadata,Base.CopyVersions,Base.Rank,
      Base.WrapNeighbors,Base.RunMode,Base.CopyResources,Base.CopyPasses,
      Dependencies,Base.CopyBridges,Requirements,Q);
    S := EncodeWfcPipelineModelText(M);
    Check(Pos('value-quota=1,1,q0,2147483647,2147483647,2'#10,S)>0,
      'same label on another pass and full integer bounds are canonical');
    Check((Pos('count=0,0,1'#10,S)>0) and (Pos('allowed=0,0,1,B'#10,S)>0),
      'existing nested count requirements coexist with quotas');
    D := DecodeWfcPipelineModelText(S);
    Check((D.RequirementCount=1) and (D.ValueQuotaCount=2) and
      (EncodeWfcPipelineModelText(D)=S), 'all nested sections round trip with quota tail');
  finally D.Free; M.Free; Base.Free; end;
end;

procedure TestHostileRows;
const BAD_NUMBERS: array[0..9] of String =
  ('-1','+1','01',' 1','1 ','1.0','NaN','Infinity','2147483648','');
var M: TWfcPipelineModel; S, T: String; I: Integer;
begin
  M := Recipe(Tokens(['A','B']),2);
  try S := EncodeWfcPipelineModelText(M); finally M.Free; end;
  for I := 0 to High(BAD_NUMBERS) do
  begin
    Reject(ReplaceOnce(S,'value-quota=0,0,q0,0,2,2',
      'value-quota=0,0,q0,'+BAD_NUMBERS[I]+',2,2'), 'noncanonical quota bound rejected');
    Reject(ReplaceOnce(S,'value-quotas=2','value-quotas='+BAD_NUMBERS[I]),
      'noncanonical quota count rejected');
    Reject(ReplaceOnce(S,'quota-token=0,0,A','quota-token=0,'+BAD_NUMBERS[I]+',A'),
      'noncanonical token ordinal rejected');
  end;
  Reject(ReplaceOnce(S,'value-quota=0,0,q0,0,2,2','value-quota=1,0,q0,0,2,2'), 'quota ordinal starts at zero');
  Reject(ReplaceOnce(S,'value-quota=1,1,q1,0,2,2','value-quota=0,1,q1,0,2,2'), 'duplicate quota ordinal rejected');
  Reject(ReplaceOnce(S,'quota-token=0,0,A','quota-token=1,0,A'), 'wrong token parent rejected');
  Reject(ReplaceOnce(S,'quota-token=0,1,B','quota-token=0,0,B'), 'duplicate token ordinal rejected');
  Reject(ReplaceOnce(S,'quota-token=0,1,B','quota-token=0,2,B'), 'skipped token ordinal rejected');
  Reject(ReplaceOnce(S,'quota-token=0,0,A'#10'quota-token=0,1,B',
    'quota-token=0,0,B'#10'quota-token=0,1,A'), 'set tokens require public-vocabulary order');
  Reject(ReplaceOnce(S,'quota-token=0,1,B','quota-token=0,1,A'), 'duplicate set member rejected');
  Reject(ReplaceOnce(S,'quota-token=0,1,B','quota-token=0,1,unknown'), 'unknown public member rejected');
  Reject(ReplaceOnce(S,'value-quota=1,1,q1,0,2,2','value-quota=1,0,q0,0,2,2'), 'duplicate pass-label rejected');
  Reject(ReplaceOnce(S,'value-quota=0,0,q0,0,2,2','value-quota=0,2,q0,0,2,2'), 'out-of-range owner rejected before tokens');
  Reject(ReplaceOnce(S,'value-quota=0,0,q0,0,2,2','value-quota=0,0,q0,3,2,2'), 'reversed bounds rejected before tokens');
  Reject(ReplaceOnce(S,'value-quota=0,0,q0,0,2,2','value-quota=0,0,,0,2,2'), 'empty quota label rejected');
  Reject(ReplaceOnce(S,'value-quota=0,0,q0,0,2,2','value-quota=0,0,q,0,0,2,2'), 'extra descriptor field rejected');
  Reject(ReplaceOnce(S,'value-quota=0,0,q0,0,2,2','value-quota=0,0,q0,0,2'), 'missing descriptor field rejected');
  Reject(ReplaceOnce(S,'quota-token=0,0,A','quota-token=0,0,'), 'empty token rejected');
  Reject(ReplaceOnce(S,'quota-token=0,0,A','quota-token=0,0,A,B'), 'extra token field rejected');
  Reject(ReplaceOnce(S,'quota-token=0,0,A','quota-token=0,A'), 'missing token field rejected');
  Reject(ReplaceOnce(S,'quota-token=0,0,A','quota-token=0,0,%41'), 'unnecessary token escape rejected');
  Reject(ReplaceOnce(S,'quota-token=0,0,A','quota-token=0,0,%2c'), 'lowercase token escape rejected');
  Reject(ReplaceOnce(S,'quota-token=0,0,A','quota-token=0,0,%'), 'truncated escape rejected');
  Reject(ReplaceOnce(S,'quota-token=0,0,A','quota-token=0,0,%E2%28%A1'), 'invalid UTF8 rejected');
  Reject(ReplaceOnce(S,'value-quota-version=1'#10,'value-quota-version=1'#13#10), 'CRLF rejected');
  Reject(ReplaceOnce(S,'value-quota-version=1'#10,'value-quota-version=1'#10#10), 'blank quota line rejected');
  Reject(S+'end'#10,'trailing data rejected');
  T := Copy(S,1,Length(S)-1); Reject(T,'missing terminal LF rejected');
end;

function ReplaceSection(const S, ASection: String): String;
var First, Last: Integer;
begin
  First := Pos('value-quota-version=',S);
  Last := Pos(#10'signature=',S)+1;
  if (First=0) or (Last<=First) then raise Exception.Create('missing quota section');
  Result := Copy(S,1,First-1)+ASection+Copy(S,Last,Length(S));
end;

procedure TestCaps;
var M, D: TWfcPipelineModel; S, T, BigLabel: String; V: TWfcModelTokens;
  Lines: TWfcTextLines; I,J,N: Integer;
begin
  M := Recipe(Tokens(['A','B']),2);
  try S := EncodeWfcPipelineModelText(M); finally M.Free; end;
  Reject(ReplaceOnce(S,'value-quotas=2','value-quotas=4097'), 'quota registry cap before allocation','value-quota count');
  Reject(ReplaceOnce(S,'value-quotas=2','value-quotas=4096'), 'declared registry requires enough rows','records are incomplete');
  Reject(ReplaceOnce(S,'value-quota=0,0,q0,0,2,2','value-quota=0,0,q0,0,2,1025'), 'per-quota token cap before allocation','token count');
  Reject(ReplaceOnce(S,'value-quota=0,0,q0,0,2,2','value-quota=0,0,q0,0,2,1024'), 'token array needs enough actual rows','records are incomplete');
  Reject(ReplaceOnce(S,'value-quota=0,0,q0,0,2,2','value-quota=0,0,q0,0,2,0'), 'nonempty token set enforced');
  T := 'wfcpipeline=1'#10 + StringOfChar(#10,WFC_PIPELINE_MAX_TEXT_LINE_COUNT);
  Reject(T,'v1 original line ceiling unchanged','line-count limit');
  T := 'wfcpipeline=2'#10 + StringOfChar(#10,WFC_PIPELINE_VALUE_QUOTA_MAX_TEXT_LINE_COUNT);
  Reject(T,'v2 exact independent line ceiling','line-count limit');
  T := StringOfChar('x',WFC_PIPELINE_MAX_ENCODED_TOKEN_LENGTH+129);
  Reject(ReplaceOnce(S,'quota-token=0,0,A','quota-token=0,0,'+T), 'oversized row rejected before field copies','row-length limit');
  T := StringOfChar('x',WFC_PIPELINE_MAX_ENCODED_TOKEN_LENGTH+1);
  Reject(ReplaceOnce(S,'quota-token=0,0,A','quota-token=0,0,'+T), 'encoded token limit remains shared','encoded token-length limit');
  { A complete bounded document with too many aggregate token records must
    fail before its last token-array allocation, not just on a bad signature. }
  SetLength(Lines,2+65*1025); Lines[0]:='value-quota-version=1'; Lines[1]:='value-quotas=65'; N:=2;
  for I:=0 to 64 do
  begin
    Lines[N]:='value-quota='+IntToStr(I)+',0,q'+IntToStr(I)+',0,2,1024'; Inc(N);
    for J:=0 to 1023 do begin Lines[N]:='quota-token='+IntToStr(I)+','+IntToStr(J)+',A'; Inc(N); end;
  end;
  T := WfcTextJoinCanonicalLines(Lines,'quota test'); Lines:=nil;
  Reject(ReplaceSection(S,T),'aggregate quota tokens preflighted','aggregate value-quota token count'); T:='';
  { All existing outer tokens and quota labels share ONE 16MiB budget. }
  BigLabel := StringOfChar('x',WFC_PIPELINE_MAX_ENCODED_TOKEN_LENGTH-4);
  SetLength(Lines,2+17*2); Lines[0]:='value-quota-version=1'; Lines[1]:='value-quotas=17'; N:=2;
  for I:=0 to 16 do
  begin
    Lines[N]:='value-quota='+IntToStr(I)+',0,'+BigLabel+IntToStr(I)+',0,2,1'; Inc(N);
    Lines[N]:='quota-token='+IntToStr(I)+',0,A'; Inc(N);
  end;
  T := WfcTextJoinCanonicalLines(Lines,'quota test'); Lines:=nil; BigLabel:='';
  Reject(ReplaceSection(S,T),'quota labels cannot get a second outer byte budget','aggregate outer-token encoding'); T:='';
  M := Recipe(Tokens(['A']),WFC_PIPELINE_MAX_VALUE_QUOTA_COUNT); D:=nil;
  try
    T:=EncodeWfcPipelineModelText(M); D:=DecodeWfcPipelineModelText(T);
    Check(D.ValueQuotaCount=4096,'exact quota-count ceiling accepted');
    Check(EncodeWfcPipelineModelText(D)=T,'maximum registry canonical roundtrip');
  finally D.Free; M.Free; end;
  SetLength(V,1024);
  for I:=0 to High(V) do V[I]:='v'+TWfcModelToken(IntToStr(I));
  M:=Recipe(V,1); D:=nil;
  try
    T:=EncodeWfcPipelineModelText(M); D:=DecodeWfcPipelineModelText(T);
    Check(Length(D.ValueQuotaAt(0).Values)=1024,'exact per-quota token ceiling accepted');
    Check(EncodeWfcPipelineModelText(D)=T,'maximum token set canonical roundtrip');
  finally D.Free; M.Free; end;
end;

begin
  try TestCanonical; TestNestedRequirements; TestHostileRows; TestCaps;
  except on E: Exception do begin Inc(Failures); WriteLn('EXCEPTION: ',E.ClassName,': ',E.Message); end; end;
  WriteLn('Pipeline quota codec checks: ',Checks-Failures,'/',Checks);
  if Failures<>0 then Halt(1);
end.
