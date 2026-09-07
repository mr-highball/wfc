{ SPDX-License-Identifier: MIT
  Copyright (c) 2021 mr-highball }
program wfc_training_pattern3d_test;
{$mode delphi}{$H+}

uses
  {$IFDEF PAS2JS}wfc_browser_test_host,{$ENDIF}
  SysUtils, wfc, wfc_model, wfc_learn3d, wfc_pattern3d,
  wfc_pattern3d_learn, wfc_pattern3d_text, wfc_training, wfc_training_text,
  wfc_pipeline_model, wfc_pipeline_text, wfc_pipeline_run,
  wfc_pipeline_run_text, wfc_pipeline_result, wfc_pipeline_result_text,
  wfc_pipeline_runtime, wfc_pipeline_connectivity;

const
  SOURCE_TEXT = 'wfclearn=6'#10'name=volume'#10'license=MIT'#10 +
    'source=authored'#10'kind=pattern3d'#10'boundary=wrap'#10 +
    'symmetry=none'#10'footprint=2,2,2'#10'order=0'#10'samples=1'#10 +
    'sample=0,2,2,2,checker'#10'token=0,0,B'#10'token=0,1,A'#10 +
    'token=0,2,A'#10'token=0,3,B'#10'token=0,4,A'#10'token=0,5,B'#10 +
    'token=0,6,B'#10'token=0,7,A'#10 +
    'value-quota-version=0'#10'value-quotas=0'#10 +
    'connectivity-version=0'#10'connectivities=0'#10'end'#10;
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

function Position(const X, Y, Z: Integer): TGraphPosition;
begin Result.X := X; Result.Y := Y; Result.Z := Z; end;

function Change(const Text, OldText, NewText: String): String;
begin
  Check(Pos(OldText, Text) > 0, 'mutation target exists');
  Result := StringReplace(Text, OldText, NewText, []);
end;

procedure Reject(const Text, MessageText: String);
var D: TWfcTrainingDocument; Failed: Boolean;
begin
  D := nil; Failed := False;
  try
    try D := DecodeWfcTrainingText(Text);
    except on E: EConvertError do Failed := True; end;
    Check(Failed and (D = nil), MessageText);
  finally D.Free; end;
end;

procedure RejectTyped(const O: TWfcTrainingOptions;
  const S: TWfcTrainingSamples; const MessageText: String);
var D: TWfcTrainingDocument; Failed: Boolean;
begin
  D := nil; Failed := False;
  try
    try D := TWfcTrainingDocument.Create(
      MakeWfcTrainingMetadata('volume', 'MIT', 'authored'), O, S);
    except on E: EWfcTraining do Failed := True; end;
    Check(Failed and (D = nil), MessageText);
  finally D.Free; end;
end;

{ Independent FNV-1a byte walk with 16-bit limbs. No production hash helpers
  and no overflowing multiplication, including in JavaScript's number model. }
function LiteralFingerprint: Cardinal;
var Lo, Hi: Cardinal;
  procedure ByteValue(const B: Integer);
  var Product, Carry, PreviousLo: Cardinal;
  begin
    Lo := Lo xor Cardinal(B); PreviousLo := Lo;
    Product := Lo * 403; Carry := Product div 65536;
    Lo := Product mod 65536;
    Hi := (Hi * 403 + PreviousLo * 256 + Carry) mod 65536;
  end;
  procedure Field(const S: String);
  var N, I: Integer;
  begin
    N := Length(S);
    for I := 0 to 3 do begin ByteValue(N mod 256); N := N div 256; end;
    for I := 1 to Length(S) do ByteValue(Ord(S[I]));
  end;
begin
  Lo := $9DC5; Hi := $811C;
  Field('wfclearn-v6'); Field('6'); Field('volume'); Field('MIT'); Field('authored');
  Field('pattern3d'); Field('wrap'); Field('none');
  Field('2'); Field('2'); Field('2'); Field('0'); Field('1');
  Field('checker'); Field('2'); Field('2'); Field('2'); Field('8');
  Field('B'); Field('A'); Field('A'); Field('B');
  Field('A'); Field('B'); Field('B'); Field('A'); Field('pattern3d'); Field('1');
  Result := Hi * 65536 + Lo;
end;

procedure TestExactSourceAndOwnership;
var D, CopyD: TWfcTrainingDocument; O: TWfcTrainingOptions;
  S: TWfcTrainingSamples; M: TWfcOverlappingModel3D; R: TWfcPipelineModel;
begin
  Check((Ord(wtkAdjacency1D) = 0) and (Ord(wtkAdjacency2D) = 1) and
    (Ord(wtkPattern2D) = 2) and (Ord(wtkSequence) = 3) and
    (Ord(wtkAdjacency3D) = 4) and (Ord(wtkPattern3D) = 5), 'append-only training kind');
  Check((WFC_TRAINING_VERSION = 1) and (WFC_TRAINING_PATTERN_3D_VERSION = 1) and
    (WFC_TRAINING_PATTERN_3D_TEXT_VERSION = 6), 'independent explicit source capability');
  D := DecodeWfcTrainingText(SOURCE_TEXT);
  try
    Check(EncodeWfcTrainingText(D) = SOURCE_TEXT, 'exact v6 canonical bytes');
    Check((D.Signature = LiteralFingerprint) and
      (WfcTrainingSignatureHex(D.Signature) = '9B8F3815'), 'independent source fingerprint golden');
    Check((D.TotalTokenCount = 8) and (D.CopyOptions.PatternDepth = 2) and
      (D.SampleAt(0).Depth = 2), 'XYZ shape is retained');
    O := D.CopyOptions; S := D.CopySamples;
    CopyD := TWfcTrainingDocument.Create(D.CopyMetadata, O, S, nil, nil);
    try
      O.PatternDepth := 1; S[0].Depth := 1; S[0].Tokens[0] := 'changed';
      Check((CopyD.Signature = D.Signature) and (EncodeWfcTrainingText(CopyD) = SOURCE_TEXT),
        'source owns detached options and nested sample payload');
    finally CopyD.Free; end;
    S := D.CopySamples; O := D.CopyOptions; O.PatternDepth := 1;
    CopyD := TWfcTrainingDocument.Create(D.CopyMetadata, O, S);
    try Check(CopyD.Signature <> D.Signature, 'footprint Z participates in identity');
    finally CopyD.Free; end;
    M := DecodeWfcPattern3DText(LearnWfcTrainingModelText(D));
    try
      Check((M.PatternCount = 2) and (M.PatternWeightAt(0) = 4) and
        (M.PatternWeightAt(1) = 4) and (M.PatternDepth = 2), 'real XYZ windows and raw weights');
      Check((M.PaletteTokenAt(0) = 'B') and (M.PaletteTokenAt(1) = 'A'),
        'palette uses original sample first appearance');
    finally M.Free; end;
    R := LearnWfcTrainingRecipe(D);
    try
      Check((R.Rank = 3) and R.WrapNeighbors and R.HasPattern3D and
        (R.ResourceAt(0).Kind = wprkPattern3D), 'typed learned volume resource');
      Check((R.PassCount = 2) and (R.PassAt(0).AdapterKind = wpakPattern3D) and
        (R.PassAt(0).Visibility = wppvPrivate) and
        (R.PassAt(1).Visibility = wppvPublic) and
        (R.BridgeAt(0).Kind = wpbkPattern3DProjection), 'private search and public XYZ projection');
      Check(Pos('wfcpipeline=4'#10, EncodeWfcPipelineModelText(R)) = 1,
        'portable recipe selects only the pattern3D extension');
      Check(R.ResourceAt(0).SourceFingerprint = 'wfclearn-v6/9B8F3815',
        'embedded resource binds exact authored source');
      Check(Pos('2x2x2', String(R.ResourceAt(0).SourceDescription)) > 0,
        'resource provenance reports source XYZ');
    finally R.Free; end;
    WriteLn('Pattern3D training source: ', WfcTrainingSignatureHex(D.Signature));
  finally D.Free; end;
end;

procedure TestLearnerDispatchAndBoundaries;
var D, Base: TWfcTrainingDocument; O: TWfcTrainingOptions; S: TWfcTrainingSamples;
  DirectSamples: TWfcLearnVolumeSamples; Direct, Learned: TWfcOverlappingModel3D;
  R: TWfcPipelineModel; B: TWfcModelBoundary; Sym: TWfcModelSymmetry;
  I, J, Total, Transforms, Origins: Integer; Text: String;
begin
  Base := DecodeWfcTrainingText(SOURCE_TEXT);
  try
    SetLength(S, 3); SetLength(DirectSamples, 3);
    S[0] := MakeWfcTrainingSample('cuboid', 3, 2, 2,
      Tokens(['A','B','A','B','A','B','B','A','B','A','B','A']));
    S[1] := MakeWfcTrainingSample('isolated', 2, 2, 2,
      Tokens(['C','C','C','C','C','C','C','C']));
    S[2] := MakeWfcTrainingSample('repeat', 3, 2, 2, S[0].Tokens);
    for I := 0 to 2 do DirectSamples[I] := MakeLearnSample3D(S[I].Tokens,
      S[I].Width, S[I].Height, S[I].Depth);
    for B := Low(TWfcModelBoundary) to High(TWfcModelBoundary) do
      for Sym := Low(TWfcModelSymmetry) to High(TWfcModelSymmetry) do
      begin
        O := MakeWfcTrainingOptions(wtkPattern3D, B, Sym, 2, 2, 2, 0);
        D := TWfcTrainingDocument.Create(Base.CopyMetadata, O, S);
        try
          Text := LearnWfcTrainingModelText(D);
          Direct := LearnOverlappingModel3DCorpus(DirectSamples, 2, 2, 2, B, Sym);
          try Check(Text = EncodeWfcPattern3DText(Direct),
            'training dispatch preserves exact literal corpus extraction'); finally Direct.Free; end;
          Learned := DecodeWfcPattern3DText(Text);
          try
            Total := 0;
            for I := 0 to Learned.PatternCount - 1 do
            begin
              Inc(Total, Learned.PatternWeightAt(I));
              if Learned.PatternPaletteIndexAt(I,0,0,0) = 2 then
                for J := 0 to 7 do Check(Learned.PatternPaletteIndexAt(I,
                  J mod 2,(J div 2) mod 2,J div 4) = 2, 'no extraction seam between independent samples');
            end;
            case Sym of wmsNone: Transforms := 1; wmsD4: Transforms := 8;
              wmsCubeRotations: Transforms := 24; else Transforms := 48; end;
            if B = wmbWrap then Origins := 32 else Origins := 5;
            Check(Total = Origins * Transforms, 'one raw observation per transformed valid origin');
            Check(Learned.SourceBoundary = B, 'source boundary retained independently');
          finally Learned.Free; end;
          R := LearnWfcTrainingRecipe(D);
          try
            Check(R.WrapNeighbors and (R.BorrowPattern3DResource(0).SourceBoundary = B),
              'output recipe wraps even for an open-trained source model');
          finally R.Free; end;
        finally D.Free; end;
      end;
    SetLength(S,1); S[0] := MakeWfcTrainingSample('single',1,1,1,Tokens(['only']));
    O := MakeWfcTrainingOptions(wtkPattern3D,wmbWrap,wmsNone,4,4,4,0);
    D := TWfcTrainingDocument.Create(Base.CopyMetadata,O,S);
    try
      Learned := DecodeWfcPattern3DText(LearnWfcTrainingModelText(D));
      try Check((Learned.PatternCount=1) and (Learned.PatternDepth=4) and
        (Learned.PatternWeightAt(0)=1), 'maximum training footprint aliases a wrapped singleton');
      finally Learned.Free; end;
    finally D.Free; end;
    O.Boundary := wmbOpen;
    RejectTyped(O,S,'open source cannot contain an oversized footprint');
  finally Base.Free; end;
end;

function PolicyDocument(const Quota, Connectivity: Boolean): TWfcTrainingDocument;
var D: TWfcTrainingDocument; Q: TWfcTrainingValueQuotas;
  C: TWfcTrainingConnectivities; V: TWfcTrainingConnectivityValues; P: TGraphPositions;
begin
  Q := nil; C := nil;
  if Quota then
  begin
    SetLength(Q,2);
    Q[0] := MakeWfcTrainingValueQuota('all',Tokens(['A','B']),8,8);
    Q[1] := MakeWfcTrainingValueQuota('half',Tokens(['A']),4,4);
  end;
  if Connectivity then
  begin
    SetLength(C,1); SetLength(V,2); SetLength(P,1);
    V[0] := MakeWfcTrainingConnectivityValue('A',[gdNorth,gdEast,gdSouth,gdWest,gdUp,gdDown]);
    V[1] := MakeWfcTrainingConnectivityValue('B',[gdNorth,gdEast,gdSouth,gdWest,gdUp,gdDown],True);
    P[0] := Position(1,1,1);
    C[0] := MakeWfcTrainingConnectivity('all',Position(0,0,0),P,V,True);
  end;
  D := DecodeWfcTrainingText(SOURCE_TEXT);
  try Result := TWfcTrainingDocument.Create(D.CopyMetadata,D.CopyOptions,D.CopySamples,Q,C);
  finally D.Free; end;
end;

procedure TestPoliciesAndReplay;
var Q,C: Boolean; D,Reload: TWfcTrainingDocument; R,ReloadR: TWfcPipelineModel;
  Run,ReloadRun: TWfcPipelineRun; Output,Replay,Parsed: TWfcPipelineResult;
  Text,ResultText: String; Failed: Boolean; BadCell: Integer;
begin
  for Q := False to True do for C := False to True do
  begin
    D := PolicyDocument(Q,C);
    try
      Text := EncodeWfcTrainingText(D);
      Check(Pos('wfclearn=6'#10,Text)=1,'all four policy-presence combinations remain v6');
      Reload := DecodeWfcTrainingText(Text);
      try Check((Reload.Signature=D.Signature) and (EncodeWfcTrainingText(Reload)=Text),
        'policy-bearing source exact save/load'); finally Reload.Free; end;
      Failed := False;
      try LearnWfcTrainingModelText(D); except on E: EWfcTraining do Failed := True; end;
      Check(Failed=(Q or C),'model-only export refuses either hard policy');
      R := LearnWfcTrainingRecipe(D);
      try
        if Q then Check((R.ValueQuotaAt(0).PassIndex=1) and
          (R.ValueQuotaAt(0).Values[0]='B') and (D.ValueQuotaAt(0).Values[0]='A'),
          'quota lowers authored strings to actual public vocabulary order');
        if C then Check((R.ConnectivityAt(0).PassIndex=1) and
          (R.ConnectivityAt(0).Values[0].Value='B') and
          (D.ConnectivityAt(0).Values[0].Value='A'), 'connectivity lowers without rewriting source order');
        ReloadR := DecodeWfcPipelineModelText(EncodeWfcPipelineModelText(R));
        try
          Run := TWfcPipelineRun.Create(R,2,2,2,17,wpssOneWay,256,0,True,nil,nil);
          try
            Output := ExecuteWfcPipeline(R,Run);
            try
              Check((Output.Status=wprsSolved) and (Output.LayerCount=1), 'XYZ policies solve on public projection');
              if C then Check(ValidateWfcPipelineConnectivity(R,0,2,2,2,
                Output.LayerAt(0).Tokens,BadCell),'independent public six-port traversal');
              ResultText := EncodeWfcPipelineResultText(Output);
              ReloadRun := DecodeWfcPipelineRunText(EncodeWfcPipelineRunText(Run),ReloadR);
              try
                Replay := ExecuteWfcPipeline(ReloadR,ReloadRun);
                try Check(EncodeWfcPipelineResultText(Replay)=ResultText,'complete canonical result replay');
                finally Replay.Free; end;
                Parsed := DecodeWfcPipelineResultText(ResultText,ReloadR,ReloadRun);
                try Check(EncodeWfcPipelineResultText(Parsed)=ResultText,'stored result independently validates');
                finally Parsed.Free; end;
              finally ReloadRun.Free; end;
            finally Output.Free; end;
          finally Run.Free; end;
        finally ReloadR.Free; end;
      finally R.Free; end;
    finally D.Free; end;
  end;
end;

procedure TestLegacyDepthIsNeverRead;
var K: TWfcTrainingKind; O,Normalized: TWfcTrainingOptions;
  S: TWfcTrainingSamples; D,Expected,Reload: TWfcTrainingDocument; Text: String;
begin
  for K := wtkAdjacency1D to wtkAdjacency3D do
  begin
    O := MakeWfcTrainingOptions(K,wmbOpen,wmsNone,0,0,0);
    if K=wtkPattern2D then begin O.Boundary:=wmbWrap; O.PatternWidth:=1; O.PatternHeight:=1; end;
    if K=wtkSequence then begin O.Boundary:=wmbWrap; O.Order:=2; end;
    SetLength(S,1); S[0]:=MakeWfcTrainingSample('legacy',2,1,Tokens(['A','B']));
    Expected:=TWfcTrainingDocument.Create(MakeWfcTrainingMetadata('old','MIT','old'),O,S);
    try
      Text:=EncodeWfcTrainingText(Expected);
      O.PatternDepth:=Low(Integer);
      {$IFDEF PAS2JS}
      asm Object.defineProperty(O, 'PatternDepth', { configurable: true,
        get: function () { throw new Error('legacy depth field was read'); } }); end;
      {$ENDIF}
      D:=TWfcTrainingDocument.Create(Expected.CopyMetadata,O,S);
      try
        Normalized:=D.CopyOptions;
        Check((Normalized.PatternDepth=1) and (D.Signature=Expected.Signature) and
          (EncodeWfcTrainingText(D)=Text), 'legacy record ignores poisoned or throwing depth field');
        Check(LearnWfcTrainingModelText(D)=LearnWfcTrainingModelText(Expected),'legacy learned bytes are unaffected');
        Reload:=DecodeWfcTrainingText(Text);
        try Check(Reload.CopyOptions.PatternDepth=1,'old text defaults the appended depth'); finally Reload.Free; end;
      finally D.Free; end;
      {$IFDEF PAS2JS}asm delete O.PatternDepth; end;{$ENDIF}
    finally Expected.Free; end;
  end;
  O:=MakeWfcTrainingOptions(wtkPattern3D,wmbWrap,wmsNone,2,2,0);
  Check(O.PatternDepth=1,'six-argument factory defaults Z to one');
  O:=MakeWfcTrainingOptions(wtkPattern3D,wmbWrap,wmsNone,2,2,3,0);
  Check((O.PatternDepth=3) and (O.Order=0),'seven-argument factory is PW PH PD Order');
  O:=MakeWfcTrainingOptions(wtkAdjacency1D,wmbOpen,wmsNone,0,0,Low(Integer),0);
  Check(O.PatternDepth=1,'legacy seven-argument call ignores irrelevant Z');
end;

procedure TestStrictPreflight;
var D: TWfcTrainingDocument; O: TWfcTrainingOptions; S: TWfcTrainingSamples;
  I: Integer; Bad,Text: String;
begin
  for I:=1 to 5 do Reject(Change(SOURCE_TEXT,'wfclearn=6','wfclearn='+IntToStr(I)), 'new kind cannot enter old source version');
  Reject(Change(SOURCE_TEXT,'wfclearn=6','wfclearn=7'),'unknown version');
  Reject(Change(SOURCE_TEXT,'kind=pattern3d','kind=adjacency3d'),'v6 requires exact feature kind');
  Reject(Change(SOURCE_TEXT,'footprint=2,2,2','footprint=2,2'),'mandatory footprint Z');
  Reject(Change(SOURCE_TEXT,'footprint=2,2,2','footprint=2,2,2,2'),'exact footprint arity');
  Reject(Change(SOURCE_TEXT,'sample=0,2,2,2,checker','sample=0,2,2,checker'),'mandatory sample Z');
  Reject(Change(SOURCE_TEXT,'sample=0,2,2,2,checker','sample=0,2,2,65536,checker'),'sample volume preflight');
  Reject(Change(SOURCE_TEXT,'footprint=2,2,2','footprint=5,4,4'),'training footprint 64-cell limit');
  Reject(Change(SOURCE_TEXT,'order=0','order=1'),'nonsequence order remains zero');
  Reject(Change(Change(SOURCE_TEXT,'symmetry=none','symmetry=d4'),'footprint=2,2,2','footprint=2,1,2'),'D4 footprint square XY');
  Reject(Change(Change(SOURCE_TEXT,'symmetry=none','symmetry=cube24'),'footprint=2,2,2','footprint=2,2,1'),'cube footprint cubic');
  Reject(Change(SOURCE_TEXT,'samples=1','samples=4097'),'sample count cap');
  Reject(Change(SOURCE_TEXT,'samples=1','samples=4096'),'sample available-record preflight');
  Reject(Change(SOURCE_TEXT,'value-quota-version=0'#10'value-quotas=0'#10,''),'mandatory empty quota section');
  Reject(Change(SOURCE_TEXT,'connectivity-version=0'#10'connectivities=0'#10,''),'mandatory empty connectivity section');
  Reject(Change(SOURCE_TEXT,'value-quota-version=0','value-quota-version=1'),'quota capability agrees with count');
  Reject(Change(SOURCE_TEXT,'connectivity-version=0','connectivity-version=1'),'connectivity capability agrees with count');
  Reject(Change(SOURCE_TEXT,'token=0,0,B','token=0,0,%42'),'canonical token encoding');
  Reject(Change(SOURCE_TEXT,'token=0,0,B','token=0,0,%ED%A0%80'),'invalid Unicode rejected');
  Reject(SOURCE_TEXT+#10,'no trailing record');
  Reject(StringReplace(SOURCE_TEXT,#10,#13#10,[rfReplaceAll]),'LF remains canonical');
  for I:=0 to Length(SOURCE_TEXT)-1 do Reject(Copy(SOURCE_TEXT,1,I),'every proper source prefix rejected');
  for I:=0 to 10 do
  begin
    case I of 0:Bad:='0'; 1:Bad:='-1'; 2:Bad:='+2'; 3:Bad:='02';
      4:Bad:='2.0'; 5:Bad:='2e0'; 6:Bad:='NaN'; 7:Bad:='Infinity';
      8:Bad:='2147483648'; 9:Bad:=' 2'; else Bad:='65'; end;
    Reject(Change(SOURCE_TEXT,'footprint=2,2,2','footprint=2,2,'+Bad),'strict bounded Z text');
  end;
  D:=DecodeWfcTrainingText(SOURCE_TEXT);
  try
    for I:=0 to 8 do
    begin
      O:=D.CopyOptions; S:=D.CopySamples;
      case I of
        0:O.PatternDepth:=0; 1:O.PatternDepth:=High(Integer);
        2:O.PatternWidth:=0; 3:O.PatternHeight:=-1; 4:O.Order:=1;
        5:S[0].Depth:=0; 6:S[0].Depth:=65537;
        7:S[0].Tokens:=Tokens(['short']); 8:S[0].Width:=High(Integer);
      end;
      RejectTyped(O,S,'typed dimensions and counts fail before learning');
    end;
  finally D.Free; end;
  Text:='wfclearn=6'#10+StringOfChar(#10,WFC_TRAINING_PATTERN_3D_MAX_TEXT_LINE_COUNT);
  Reject(Text,'bounded source line envelope');
  Reject('wfclearn=6'#10+StringOfChar('x',WFC_TRAINING_MAX_ENCODED_TEXT_LENGTH),'unchanged source text byte budget');
end;

{$IFDEF PAS2JS}
procedure TestHostileJavaScript;
var D: TWfcTrainingDocument; O: TWfcTrainingOptions; S: TWfcTrainingSamples;
  I,F,Bad: Integer;
begin
  D:=DecodeWfcTrainingText(SOURCE_TEXT);
  try
    for F:=0 to 9 do for I:=0 to 10 do
    begin
      case I of
        0:asm Bad=1.5; end; 1:asm Bad=NaN; end;
        2:asm Bad=Infinity; end; 3:asm Bad=-Infinity; end;
        4:asm Bad='2'; end; 5:asm Bad=null; end;
        6:asm Bad=true; end; 7:asm Bad=undefined; end;
        8:asm Bad=2147483648; end; 9:asm Bad=4294967296; end;
        10:asm Bad=9007199254740992; end;
      end;
      O:=D.CopyOptions; S:=D.CopySamples;
      case F of
        0:O.PatternWidth:=Bad; 1:O.PatternHeight:=Bad; 2:O.PatternDepth:=Bad;
        3:O.Order:=Bad; 4:S[0].Width:=Bad; 5:S[0].Height:=Bad; 6:S[0].Depth:=Bad;
        7:O.Kind:=TWfcTrainingKind(Bad); 8:O.Boundary:=TWfcModelBoundary(Bad);
        9:O.Symmetry:=TWfcModelSymmetry(Bad);
      end;
      RejectTyped(O,S,'hostile JS numeric and enum field rejected');
    end;
  finally D.Free; end;
end;
{$ENDIF}

begin
  try
    TestExactSourceAndOwnership;
    TestLearnerDispatchAndBoundaries;
    TestPoliciesAndReplay;
    TestLegacyDepthIsNeverRead;
    TestStrictPreflight;
    {$IFDEF PAS2JS}TestHostileJavaScript;{$ENDIF}
    WriteLn('Pattern3D training checks: ',Checks);
  except on E:Exception do begin WriteLn('FAIL: ',E.ClassName,': ',E.Message); Halt(1); end; end;
end.
